/*
 * Description: Update an internal view of a buffer's state.
 *
 * Copyright (C) 2022 Danny McClanahan <dmcC2@hypnicjerk.ai>
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

//! Update an internal view of a buffer's state.
//!
//! # TODO
//! 1. maintain a list of all lines of text in the buffer, along with their checksums & covered
//!    regions
//! 1. every time a line is modified, update its checksum + region
//! 1. if a newline is deleted or inserted, calculate new adjacent lines, regions, & checksums
//! 1. when a sync process fails, obtain the minimum sequence of hashed line insertions/deletions to
//!    apply
//! 1. TODO: figure out how to broadcast synced lines. rebroadcast them all periodically? LRU?
//!    MRU?
//!
//! # sync negotiation
//! - A provides a list of "trial lines" with indices and checksums to B.
//! - B provides a response of which checksums weren't found.
//!   - If B found any such lines, then B pauses receiving any new operations for that buffer
//!     momentarily.
//! - A provides the N previous/successive lines from each mismatched line from B.
//! - B reports the *line number intervals* which didn't match, so A can provide more
//!   previous/successive lines necessary for B to obtain the whole extent of non-matching
//!   lines. Note that there needs to be a sentinel value for the final line from A being the end of
//!   the buffer.
//! - [A & B repeat the above process until the non-matching intervals have been closed in on.]
//! - A then sends all of the line contents according to all the checksums B doesn't have.
//! - B applies all the changes to the line contents as a single edit.
//! - B then unpauses receiving operations for the buffer.

use crate::{buffers::BufferId, transforms};

use async_lock::RwLock;
use displaydoc::Display;
use indexmap::IndexMap;
use thiserror::Error;

use std::{collections::hash_map::DefaultHasher, hash::Hasher, num, sync::Arc};

/// <checksum hash: {hash}, length: {length}>
#[derive(Debug, Display, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TextChecksum {
  /// Hash traits such as [`Hasher`] by default will only output an unsigned integer type. If wider
  /// output is desired, we should explicitly use a cryptographic checksum like
  /// [`sha2`](https://docs.rs/sha2/latest/sha2/index.html).
  pub hash: u64,
  pub length: usize,
}

impl TextChecksum {
  pub fn extract(text: &str) -> Self {
    let mut hasher = DefaultHasher::new();
    hasher.write(text.as_bytes());
    Self {
      hash: hasher.finish(),
      length: text.len(),
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TextSection {
  pub contents: String,
  pub num_occurrences: usize,
}

#[derive(Debug, Display, Error, Clone)]
pub enum InternError {
  /// checksum {0} was not found in the intern table
  ChecksumDidNotExist(TextChecksum),
  /// hash collision for checksum {0} between {1} and {2}
  HashCollision(TextChecksum, String, String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InternedTexts {
  pub interned_text_sections: IndexMap<TextChecksum, TextSection>,
}

impl InternedTexts {
  pub fn new() -> Self {
    Self {
      interned_text_sections: IndexMap::new(),
    }
  }

  pub fn get_no_eq_check(&self, checksum: TextChecksum) -> Result<&TextSection, InternError> {
    match self.interned_text_sections.get(&checksum) {
      Some(text_section) => Ok(text_section),
      None => Err(InternError::ChecksumDidNotExist(checksum)),
    }
  }

  fn increment_hashed(&mut self, checksum: TextChecksum, text: &str) -> Result<(), InternError> {
    let text_section = self
      .interned_text_sections
      .entry(checksum)
      .or_insert_with(|| TextSection {
        contents: text.to_string(),
        num_occurrences: 0,
      });
    /* TODO: only do the == check if length is below some number? */
    if text == &text_section.contents {
      text_section.num_occurrences += 1;
      Ok(())
    } else {
      Err(InternError::HashCollision(
        checksum,
        text.to_string(),
        text_section.contents.clone(),
      ))
    }
  }

  pub fn increment(&mut self, text: &str) -> Result<TextChecksum, InternError> {
    let checksum = TextChecksum::extract(text);
    self.increment_hashed(checksum, text)?;
    Ok(checksum)
  }

  pub fn extract_from(&mut self, other: Self) -> Result<(), InternError> {
    for (checksum, text_section) in other.interned_text_sections.into_iter() {
      self.increment_hashed(checksum, &text_section.contents)?;
    }
    Ok(())
  }

  pub fn decrement(&mut self, checksum: TextChecksum) -> Result<(), InternError> {
    let text_section = self
      .interned_text_sections
      .get_mut(&checksum)
      .ok_or_else(|| InternError::ChecksumDidNotExist(checksum))?;
    text_section.num_occurrences -= 1;
    /* If the buffer has no more instances of this string, remove it from the interns list. */
    if text_section.num_occurrences == 0 {
      assert!(self.interned_text_sections.remove(&checksum).is_some());
    }
    Ok(())
  }
}

/// <section index @ {0}>
#[derive(Debug, Display, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SectionIndex(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SectionRange {
  pub start: SectionIndex,
  pub end: SectionIndex,
}

impl SectionRange {
  pub fn new(start: SectionIndex, end: SectionIndex) -> Self {
    assert!(start <= end);
    Self { start, end }
  }

  pub fn single(si: SectionIndex) -> Self { Self::new(si, si) }
}

/// <insertion index @ {0}>
#[derive(Debug, Display, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct InsertionIndex(pub usize);

impl TryFrom<transforms::Point> for InsertionIndex {
  type Error = num::TryFromIntError;

  fn try_from(value: transforms::Point) -> Result<Self, Self::Error> {
    let transforms::Point { code_point_index } = value;
    Ok(Self(code_point_index.try_into()?))
  }
}

impl TryFrom<InsertionIndex> for transforms::Point {
  type Error = num::TryFromIntError;

  fn try_from(value: InsertionIndex) -> Result<Self, Self::Error> {
    let InsertionIndex(index) = value;
    Ok(Self {
      code_point_index: index.try_into()?,
    })
  }
}

impl InsertionIndex {
  pub fn delete_range(&self, distance: usize) -> DeletionRange {
    DeletionRange {
      beg: self.clone(),
      end: InsertionIndex(self.0 + distance),
    }
  }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct WithinSectionIndex(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FoundSection {
  pub section: SectionIndex,
  pub within: WithinSectionIndex,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct DeletionRange {
  pub beg: InsertionIndex,
  pub end: InsertionIndex,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct DeletionResult {
  pub beg: FoundSection,
  pub end: FoundSection,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TokenIndex {
  pub location: usize,
  pub length: usize,
}

pub trait Tokenizer {
  fn tokenize(&self, input: &str) -> Vec<TokenIndex>;
}

/// Tokenize a string into newline separators: `'\n'`.
///
///```
/// use emdocs_protocol::state::{Tokenizer, NewlineTokenizer};
///
/// let tokens: Vec<usize> = NewlineTokenizer.tokenize("a\nb\nc\n\nd").into_iter()
///   .map(|ti| ti.location)
///   .collect();
/// assert_eq!(vec![1, 3, 5, 6], tokens);
///
/// let tokens: Vec<usize> = NewlineTokenizer.tokenize("ab\n").into_iter()
///   .map(|ti| ti.location)
///   .collect();
/// assert_eq!(vec![2], tokens);
///
/// let tokens: Vec<usize> = NewlineTokenizer.tokenize("ab\n\n").into_iter()
///   .map(|ti| ti.location)
///   .collect();
/// assert_eq!(vec![2, 3], tokens);
///```
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NewlineTokenizer;

impl Tokenizer for NewlineTokenizer {
  fn tokenize(&self, mut input: &str) -> Vec<TokenIndex> {
    let mut tokens = Vec::new();
    let mut last_token_index: usize = 0;
    while let Some(newline_index) = input.find('\n') {
      last_token_index += newline_index;
      tokens.push(TokenIndex {
        location: last_token_index,
        /* 1 is the length of '\n', so we advance by that much so as not to hit it again. */
        length: 1,
      });
      /* 1 is the length of '\n', so we advance by that much so as not to hit it again. */
      last_token_index += 1;
      /* 1 is the length of '\n', so we advance by that much so as not to hit it again. */
      input = &input[newline_index + 1..];
    }
    tokens
  }
}

#[derive(Debug, Display, Error)]
pub enum BufferError {
  /// error managing interned string: {0}
  Intern(#[from] InternError),
  /// section index {0} was past the end of the section list at {1}
  SectionOutOfBounds(SectionIndex, SectionIndex),
  /// insertion index {0} was past the end of the buffer at {1}
  EditOutOfBounds(InsertionIndex, InsertionIndex),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Buffer {
  pub interns: InternedTexts,
  pub lines: Vec<TextChecksum>,
}

impl Buffer {
  pub fn new() -> Self {
    Self {
      interns: InternedTexts::new(),
      lines: Vec::new(),
    }
  }

  /* pub fn dump(self) -> String { */
  /*   let Self { interns, lines } = self; */
  /*   let mut ret = String::new(); */
  /*   let n = lines.len(); */
  /*   for (ind, checksum) in lines.into_iter().enumerate() { */
  /*     let text_section = interns.get_no_eq_check(&checksum); */
  /*     ret.push_str(&text_section.contents); */
  /*     if ind < n - 1 { */
  /*       /\* 1 is the length of '\n', so we advance by that much so as not to hit it again. *\/ */
  /*       ret.push_str("\n"); */
  /*     } */
  /*   } */
  /*   ret */
  /* } */

  /// Tokenize a string into a buffer.
  ///
  ///```
  /// # fn main() -> Result<(), emdocs_protocol::state::BufferError> {
  /// use emdocs_protocol::state::*;
  ///
  /// let buffer = Buffer::tokenize("ab\nc\n\nef\ng")?;
  /// assert_eq!(
  ///   buffer,
  ///   Buffer {
  ///     interns: InternedTexts {
  ///       interned_text_sections: [
  ///         (TextChecksum { hash: 6148830537548944441, length: 2 },
  ///          TextSection { contents: "ab".to_string(), num_occurrences: 1 }),
  ///         (TextChecksum { hash: 15797338846215409778, length: 1 },
  ///          TextSection { contents: "c".to_string(), num_occurrences: 1 }),
  ///         (TextChecksum { hash: 15130871412783076140, length: 0 },
  ///          TextSection { contents: "".to_string(), num_occurrences: 1 }),
  ///         (TextChecksum { hash: 4980332201698043396, length: 2 },
  ///          TextSection { contents: "ef".to_string(), num_occurrences: 1 }),
  ///         (TextChecksum { hash: 4158092142439706792, length: 1 },
  ///          TextSection { contents: "g".to_string(), num_occurrences: 1 }),
  ///       ].into_iter().collect(),
  ///     },
  ///     lines: vec![
  ///       TextChecksum { hash: 6148830537548944441, length: 2 },
  ///       TextChecksum { hash: 15797338846215409778, length: 1 },
  ///       TextChecksum { hash: 15130871412783076140, length: 0 },
  ///       TextChecksum { hash: 4980332201698043396, length: 2 },
  ///       TextChecksum { hash: 4158092142439706792, length: 1 }],
  ///   },
  /// );
  ///
  /// let buffer = Buffer::tokenize("ab\n")?;
  /// assert_eq!(
  ///   buffer,
  ///   Buffer {
  ///     interns: InternedTexts {
  ///       interned_text_sections: [
  ///         (TextChecksum { hash: 6148830537548944441, length: 2 },
  ///          TextSection { contents: "ab".to_string(), num_occurrences: 1 }),
  ///         (TextChecksum { hash: 15130871412783076140, length: 0 },
  ///          TextSection { contents: "".to_string(), num_occurrences: 1 }),
  ///       ].into_iter().collect(),
  ///     },
  ///     lines: vec![
  ///       TextChecksum { hash: 6148830537548944441, length: 2 },
  ///       TextChecksum { hash: 15130871412783076140, length: 0 },
  ///     ],
  ///   },
  /// );
  ///
  /// let buffer = Buffer::tokenize("ab\n\n")?;
  /// assert_eq!(
  ///   buffer,
  ///   Buffer {
  ///     interns: InternedTexts {
  ///       interned_text_sections: [
  ///         (TextChecksum { hash: 6148830537548944441, length: 2 },
  ///          TextSection { contents: "ab".to_string(), num_occurrences: 1 }),
  ///         (TextChecksum { hash: 15130871412783076140, length: 0 },
  ///          TextSection { contents: "".to_string(), num_occurrences: 2 }),
  ///       ].into_iter().collect(),
  ///     },
  ///     lines: vec![
  ///       TextChecksum { hash: 6148830537548944441, length: 2 },
  ///       TextChecksum { hash: 15130871412783076140, length: 0 },
  ///       TextChecksum { hash: 15130871412783076140, length: 0 },
  ///     ],
  ///   },
  /// );
  /// # Ok(())
  /// # }
  ///```
  pub fn tokenize(input: &str) -> Result<Self, BufferError> {
    let mut interns = InternedTexts::new();
    let mut lines = Vec::new();
    let mut last_token_index: usize = 0;
    for TokenIndex { location, length } in NewlineTokenizer.tokenize(input).into_iter() {
      let cur_line = &input[last_token_index..location];
      let checksum = interns.increment(cur_line)?;
      lines.push(checksum);
      last_token_index = location + length;
    }
    /* If we ended on a token, then the last line is empty. If there were no tokens, then this is
     * the whole string. */
    let cur_line = &input[last_token_index..];
    let checksum = interns.increment(cur_line)?;
    lines.push(checksum);
    Ok(Self { interns, lines })
  }

  /// ???
  ///
  ///```
  /// # fn main() -> Result<(), emdocs_protocol::state::BufferError> {
  /// use emdocs_protocol::state::*;
  ///
  /// let mut abc = Buffer::tokenize("ab\nc")?;
  /// let def = Buffer::tokenize("de\nf")?;
  /// let mut abc2 = abc.clone();
  /// let def2 = def.clone();
  /// let mut abc3 = abc.clone();
  /// let def3 = def.clone();
  ///
  /// abc.merge_into_at(def, SectionRange::single(SectionIndex(1)))?;
  /// let ab_de_f = Buffer::tokenize("ab\nde\nf")?;
  /// assert_eq!(abc, ab_de_f);
  ///
  /// abc2.merge_into_at(def2, SectionRange::single(SectionIndex(0)))?;
  /// let de_f_c = Buffer::tokenize("de\nf\nc")?;
  /// assert_eq!(abc2, de_f_c);
  ///
  /// abc3.merge_into_at(def3, SectionRange::new(SectionIndex(0), SectionIndex(1)))?;
  /// let de_f = Buffer::tokenize("de\nf")?;
  /// assert_eq!(abc3, de_f);
  ///
  /// let mut ab_c_def_g = Buffer::tokenize("ab\nc\ndef\ng")?;
  /// let f = Buffer::tokenize("f")?;
  /// ab_c_def_g.merge_into_at(f, SectionRange::new(SectionIndex(1), SectionIndex(2)))?;
  /// let ab_f_g = Buffer::tokenize("ab\nf\ng")?;
  /// assert_eq!(ab_c_def_g, ab_f_g);
  /// # Ok(())
  /// # }
  ///```
  pub fn merge_into_at(&mut self, other: Self, at: SectionRange) -> Result<(), BufferError> {
    /* TODO: make this faster! */
    let Self { interns, lines } = other;
    self.interns.extract_from(interns)?;
    let (prefix, occluded, suffix) = Self::split_lines_by_range(&self.lines, at)?;
    for checksum in occluded.iter() {
      self.interns.decrement(*checksum)?;
    }
    self.lines = prefix
      .iter()
      .cloned()
      .chain(lines.into_iter())
      .chain(suffix.iter().cloned())
      .collect();
    Ok(())
  }

  fn split_lines_by_range(
    lines: &[TextChecksum],
    range: SectionRange,
  ) -> Result<(&[TextChecksum], &[TextChecksum], &[TextChecksum]), BufferError> {
    let SectionRange { start, end } = range;
    assert!(!lines.is_empty());
    let final_section = SectionIndex(lines.len() - 1);
    if start > final_section {
      return Err(BufferError::SectionOutOfBounds(start, final_section));
    }
    /* Do *NOT* include the upper bound. */
    let lhs = &lines[..start.0];
    if end > final_section {
      return Err(BufferError::SectionOutOfBounds(end, final_section));
    }
    /* Have to add + 1 to convert into noninclusive range, sigh. */
    let mid = &lines[start.0..end.0 + 1];
    let rhs = &lines[end.0 + 1..];
    Ok((lhs, mid, rhs))
  }

  fn get_section(&self, si: SectionIndex) -> Result<&TextChecksum, BufferError> {
    self
      .lines
      .get(si.0)
      .ok_or_else(|| BufferError::SectionOutOfBounds(si, self.final_section()))
  }

  fn final_section(&self) -> SectionIndex {
    assert!(!self.lines.is_empty());
    SectionIndex(self.lines.len() - 1)
  }

  fn locate_within_section(&self, at: InsertionIndex) -> Result<FoundSection, BufferError> {
    /* TODO: make this faster! */
    let mut cur_location: usize = 0;
    let mut found_section: Option<FoundSection> = None;
    for (section_index, checksum) in self.lines.iter().enumerate() {
      /* 1 is the length of '\n', so we advance by that much so as not to hit it again. */
      if cur_location + checksum.length + 1 > at.0 {
        let within_section = at.0 - cur_location;
        found_section = Some(FoundSection {
          section: SectionIndex(section_index),
          within: WithinSectionIndex(within_section),
        });
        break;
      } else {
        /* 1 is the length of '\n', so we advance by that much so as not to hit it again. */
        cur_location += checksum.length + 1;
      }
    }
    match found_section {
      Some(fs) => Ok(fs),
      None => {
        if at.0 > cur_location {
          Err(BufferError::EditOutOfBounds(
            at,
            InsertionIndex(cur_location),
          ))
        } else {
          let final_section = self.final_section();
          Ok(FoundSection {
            section: final_section,
            within: WithinSectionIndex(self.get_section(final_section)?.length),
          })
        }
      },
    }
  }

  fn locate_deletion_range(&self, range: DeletionRange) -> Result<DeletionResult, BufferError> {
    /* TODO: make this faster! */
    let DeletionRange { beg, end } = range;
    let beg = self.locate_within_section(beg)?;
    let end = self.locate_within_section(end)?;
    Ok(DeletionResult { beg, end })
  }

  /// ???
  ///
  ///```
  /// # fn main() -> Result<(), emdocs_protocol::state::BufferError> {
  /// use emdocs_protocol::state::*;
  ///
  /// let mut abc = Buffer::tokenize("ab\nc")?;
  /// let mut abc2 = abc.clone();
  /// let mut abc3 = abc.clone();
  ///
  /// abc.insert_at(InsertionIndex(2), "de\nf")?;
  /// let abde_f_c = Buffer::tokenize("abde\nf\nc")?;
  /// assert_eq!(abc, abde_f_c);
  ///
  /// abc2.insert_at(InsertionIndex(0), "de\nf")?;
  /// let de_fab_c = Buffer::tokenize("de\nfab\nc")?;
  /// assert_eq!(abc2, de_fab_c);
  ///
  /// abc3.insert_at(InsertionIndex(3), "de\nf")?;
  /// let ab_de_fc = Buffer::tokenize("ab\nde\nfc")?;
  /// assert_eq!(abc3, ab_de_fc);
  /// # Ok(())
  /// # }
  ///```
  pub fn insert_at(&mut self, at: InsertionIndex, s: &str) -> Result<(), BufferError> {
    /* TODO: make this faster! */
    let FoundSection { section, within } = self.locate_within_section(at)?;
    let current_line = &self
      .interns
      .get_no_eq_check(*self.get_section(section)?)?
      .contents;
    let new_line_string = [&current_line[..within.0], s, &current_line[within.0..]].concat();
    self.merge_into_at(
      Self::tokenize(&new_line_string)?,
      SectionRange::single(section),
    )?;
    Ok(())
  }

  /// ???
  ///
  ///```
  /// # fn main() -> Result<(), emdocs_protocol::state::BufferError> {
  /// use emdocs_protocol::state::*;
  ///
  /// let mut ab_c = Buffer::tokenize("ab\nc")?;
  /// ab_c.replace("d\nef")?;
  /// let d_ef = Buffer::tokenize("d\nef")?;
  /// assert_eq!(ab_c, d_ef);
  /// # Ok(())
  /// # }
  ///```
  pub fn replace(&mut self, s: &str) -> Result<(), BufferError> {
    let Self { interns, lines } = Self::tokenize(s)?;
    self.interns = interns;
    self.lines = lines;
    Ok(())
  }

  /// ???
  ///
  ///```
  /// # fn main() -> Result<(), emdocs_protocol::state::BufferError> {
  /// use emdocs_protocol::state::*;
  ///
  /// let mut abc = Buffer::tokenize("ab\nc")?;
  /// abc.delete_at(DeletionRange { beg: InsertionIndex(0), end: InsertionIndex(0) })?;
  /// let b_c = Buffer::tokenize("b\nc")?;
  /// assert_eq!(abc, b_c);
  ///
  /// let mut ab_c_def_g = Buffer::tokenize("ab\nc\ndef\ng")?;
  /// ab_c_def_g.delete_at(DeletionRange { beg: InsertionIndex(3), end: InsertionIndex(6) })?;
  /// let ab_f_g = Buffer::tokenize("ab\nf\ng")?;
  /// assert_eq!(ab_c_def_g, ab_f_g);
  /// # Ok(())
  /// # }
  ///```
  pub fn delete_at(&mut self, range: DeletionRange) -> Result<(), BufferError> {
    let DeletionResult { beg, end } = self.locate_deletion_range(range)?;
    let prefix = &self
      .interns
      .get_no_eq_check(*self.get_section(beg.section)?)?
      .contents[..beg.within.0];
    let suffix = &self
      .interns
      .get_no_eq_check(*self.get_section(end.section)?)?
      .contents[end.within.0 + 1..];
    let new_line_string = [prefix, suffix].concat();
    self.merge_into_at(
      Self::tokenize(&new_line_string)?,
      SectionRange::new(beg.section, end.section),
    )?;
    Ok(())
  }
}

#[derive(Debug, Display, Error)]
pub enum BufferMappingError {
  /// error handling a buffer: {0}
  Buf(#[from] BufferError),
  /// failed to find a buffer with id {0}
  BufNotFound(BufferId),
  /// numerical conversion error: {0}
  Num(#[from] num::TryFromIntError),
}

#[derive(Debug, Clone)]
pub struct BufferMapping {
  pub live_buffers: Arc<RwLock<IndexMap<BufferId, Arc<RwLock<Buffer>>>>>,
}

impl BufferMapping {
  pub fn new() -> Self {
    Self {
      live_buffers: Arc::new(RwLock::new(IndexMap::new())),
    }
  }

  pub async fn initialize_buffer(
    &self,
    id: BufferId,
    contents: &str,
  ) -> Result<(), BufferMappingError> {
    let buffer = self
      .live_buffers
      .write()
      .await
      .entry(id)
      .or_insert_with(|| Arc::new(RwLock::new(Buffer::new())))
      .clone();
    buffer.write().await.replace(contents)?;
    Ok(())
  }

  pub async fn apply_transform(
    &self,
    id: BufferId,
    transform: transforms::Transform,
  ) -> Result<(), BufferMappingError> {
    let transforms::Transform { r#type } = transform;
    let buffer = self
      .live_buffers
      .read()
      .await
      .get(&id)
      .ok_or_else(|| BufferMappingError::BufNotFound(id))?
      .clone();
    let mut buffer = buffer.write().await;
    match r#type {
      transforms::TransformType::edit(transforms::Edit { point, payload }) => {
        let insertion_index: InsertionIndex = point.try_into()?;
        match payload {
          transforms::EditPayload::insert(transforms::Insert { contents }) => {
            buffer.insert_at(insertion_index, &contents)?;
          },
          transforms::EditPayload::delete(transforms::Delete { distance }) => {
            let deletion_range = insertion_index.delete_range(distance.try_into()?);
            buffer.delete_at(deletion_range)?;
          },
        }
      },
      transforms::TransformType::sync(sync) => todo!("sync: {:?}", sync),
    }
    Ok(())
  }
}
