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

use std::{
  collections::{hash_map::DefaultHasher, BTreeMap},
  hash::Hasher,
  num,
  ops::Bound,
  sync::Arc,
};

/// <checksum hash: {hash}, length: {length}>
#[derive(Debug, Display, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TextChecksum {
  /// Hash traits such as [`Hasher`] by default will only output an unsigned integer type. If wider
  /// output is desired, we should explicitly use a cryptographic checksum like
  /// [`sha2`](https://docs.rs/sha2/latest/sha2/index.html).
  pub hash: u64,
  /// Contains the number of **bytes** in the string.
  pub length: usize,
  /// Contains the number of **[unicode code points]** in the string.
  ///
  /// [unicode code points]: https://manishearth.github.io/blog/2017/01/14/stop-ascribing-meaning-to-unicode-code-points/
  pub code_points: usize,
}

impl TextChecksum {
  pub fn extract(text: &str) -> Self {
    let mut hasher = DefaultHasher::new();
    hasher.write(text.as_bytes());
    Self {
      hash: hasher.finish(),
      length: text.len(),
      code_points: text.chars().fold(0, |l, _| l + 1),
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

/// <code point section index @ {0}>
#[derive(Debug, Display, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct CodePointSectionIndex(pub usize);

impl CodePointSectionIndex {
  pub fn end_index(&self, length: usize) -> CodePointInsertionIndex {
    CodePointInsertionIndex(self.0 + length)
  }
}

/// <code point insertion index @ {0}>
#[derive(Debug, Display, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct CodePointInsertionIndex(pub usize);

impl CodePointInsertionIndex {
  pub fn section_bounds(&self) -> (Bound<CodePointSectionIndex>, Bound<CodePointSectionIndex>) {
    (
      Bound::Unbounded,
      Bound::Included(CodePointSectionIndex(self.0)),
    )
  }

  pub fn from_point(point: transforms::Point) -> Self {
    let transforms::Point { code_point_index } = point;
    Self(code_point_index as usize)
  }
}

/// <byte section index @ {0}>
#[derive(Debug, Display, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ByteSectionIndex(pub usize);

impl ByteSectionIndex {
  pub fn end_index(&self, length: usize) -> InsertionIndex { InsertionIndex(self.0 + length) }

  pub fn within_index(&self, at: InsertionIndex) -> WithinSectionIndex {
    WithinSectionIndex(at.0 - self.0)
  }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SectionRange {
  pub start: ByteSectionIndex,
  pub end: ByteSectionIndex,
}

impl SectionRange {
  pub fn new(start: ByteSectionIndex, end: ByteSectionIndex) -> Self {
    assert!(start <= end);
    Self { start, end }
  }

  pub fn single(si: ByteSectionIndex) -> Self { Self::new(si, si) }
}

/// <insertion index @ {0}>
#[derive(Debug, Display, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct InsertionIndex(pub usize);

impl InsertionIndex {
  pub fn delete_range(&self, distance: usize) -> DeletionRange {
    DeletionRange {
      beg: self.clone(),
      end: InsertionIndex(self.0 + distance),
    }
  }

  pub fn section_bounds(&self) -> (Bound<ByteSectionIndex>, Bound<ByteSectionIndex>) {
    (Bound::Unbounded, Bound::Included(ByteSectionIndex(self.0)))
  }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct WithinSectionIndex(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FoundSection {
  pub section: ByteSectionIndex,
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
  SectionOutOfBounds(ByteSectionIndex, ByteSectionIndex),
  /// insertion index {0} was past the end of the buffer at {1}
  EditOutOfBounds(InsertionIndex, InsertionIndex),
  /// code point {0} was past the end of the buffer at {1}
  CodePointOutOfBounds(CodePointInsertionIndex, CodePointInsertionIndex),
}

/// <checksum {checksum} with byte index {byte_index}>
#[derive(Debug, Display, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ChecksumWithByteSection {
  pub checksum: TextChecksum,
  pub byte_index: ByteSectionIndex,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Buffer {
  pub interns: InternedTexts,
  pub lines_by_bytes: BTreeMap<ByteSectionIndex, TextChecksum>,
  pub lines_by_code_points: BTreeMap<CodePointSectionIndex, ChecksumWithByteSection>,
}

impl Buffer {
  pub fn new() -> Self {
    Self {
      interns: InternedTexts::new(),
      lines_by_bytes: BTreeMap::new(),
      lines_by_code_points: BTreeMap::new(),
    }
  }

  pub fn dump(&self) -> String {
    let Self {
      interns,
      lines_by_bytes,
      ..
    } = self;
    let mut ret = String::new();
    let n = lines_by_bytes.len();
    for (ind, checksum) in lines_by_bytes.values().enumerate() {
      let text_section = interns
        .get_no_eq_check(*checksum)
        .expect("checksum should exist");
      ret.push_str(&text_section.contents);
      if ind < n - 1 {
        /* 1 is the length of '\n', so we advance by that much so as not to hit it again. */
        ret.push_str("\n");
      }
    }
    ret
  }

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
  ///         (TextChecksum { hash: 6148830537548944441, length: 2, code_points: 2 },
  ///          TextSection { contents: "ab".to_string(), num_occurrences: 1 }),
  ///         (TextChecksum { hash: 15797338846215409778, length: 1, code_points: 1 },
  ///          TextSection { contents: "c".to_string(), num_occurrences: 1 }),
  ///         (TextChecksum { hash: 15130871412783076140, length: 0, code_points: 0 },
  ///          TextSection { contents: "".to_string(), num_occurrences: 1 }),
  ///         (TextChecksum { hash: 4980332201698043396, length: 2, code_points: 2 },
  ///          TextSection { contents: "ef".to_string(), num_occurrences: 1 }),
  ///         (TextChecksum { hash: 4158092142439706792, length: 1, code_points: 1 },
  ///          TextSection { contents: "g".to_string(), num_occurrences: 1 }),
  ///       ].into_iter().collect(),
  ///     },
  ///     lines_by_bytes: [
  ///       (ByteSectionIndex(0), TextChecksum { hash: 6148830537548944441, length: 2, code_points: 2 }),
  ///       (ByteSectionIndex(3), TextChecksum { hash: 15797338846215409778, length: 1, code_points: 1 }),
  ///       (ByteSectionIndex(5), TextChecksum { hash: 15130871412783076140, length: 0, code_points: 0 }),
  ///       (ByteSectionIndex(6), TextChecksum { hash: 4980332201698043396, length: 2, code_points: 2 }),
  ///       (ByteSectionIndex(9), TextChecksum { hash: 4158092142439706792, length: 1, code_points: 1 }),
  ///     ].into(),
  ///     lines_by_code_points: [
  ///       (CodePointSectionIndex(0),
  ///        ChecksumWithByteSection {
  ///          checksum: TextChecksum { hash: 6148830537548944441, length: 2, code_points: 2 },
  ///          byte_index: ByteSectionIndex(0),
  ///        }),
  ///       (CodePointSectionIndex(3),
  ///        ChecksumWithByteSection {
  ///          checksum: TextChecksum { hash: 15797338846215409778, length: 1, code_points: 1 },
  ///          byte_index: ByteSectionIndex(3),
  ///        }),
  ///       (CodePointSectionIndex(5),
  ///        ChecksumWithByteSection {
  ///          checksum: TextChecksum { hash: 15130871412783076140, length: 0, code_points: 0 },
  ///          byte_index: ByteSectionIndex(5),
  ///        }),
  ///       (CodePointSectionIndex(6),
  ///        ChecksumWithByteSection {
  ///          checksum: TextChecksum { hash: 4980332201698043396, length: 2, code_points: 2 },
  ///          byte_index: ByteSectionIndex(6),
  ///        }),
  ///       (CodePointSectionIndex(9),
  ///        ChecksumWithByteSection {
  ///          checksum: TextChecksum { hash: 4158092142439706792, length: 1, code_points: 1 },
  ///          byte_index: ByteSectionIndex(9),
  ///        }),
  ///     ].into(),
  ///   },
  /// );
  ///
  /// let buffer = Buffer::tokenize("ab\n")?;
  /// assert_eq!(
  ///   buffer,
  ///   Buffer {
  ///     interns: InternedTexts {
  ///       interned_text_sections: [
  ///         (TextChecksum { hash: 6148830537548944441, length: 2, code_points: 2 },
  ///          TextSection { contents: "ab".to_string(), num_occurrences: 1 }),
  ///         (TextChecksum { hash: 15130871412783076140, length: 0, code_points: 0 },
  ///          TextSection { contents: "".to_string(), num_occurrences: 1 }),
  ///       ].into_iter().collect(),
  ///     },
  ///     lines_by_bytes: [
  ///       (ByteSectionIndex(0), TextChecksum { hash: 6148830537548944441, length: 2, code_points: 2 }),
  ///       (ByteSectionIndex(3), TextChecksum { hash: 15130871412783076140, length: 0, code_points: 0 }),
  ///     ].into(),
  ///     lines_by_code_points: [
  ///       (CodePointSectionIndex(0),
  ///        ChecksumWithByteSection {
  ///          checksum: TextChecksum { hash: 6148830537548944441, length: 2, code_points: 2 },
  ///          byte_index: ByteSectionIndex(0),
  ///        }),
  ///       (CodePointSectionIndex(3),
  ///        ChecksumWithByteSection {
  ///          checksum: TextChecksum { hash: 15130871412783076140, length: 0, code_points: 0 },
  ///          byte_index: ByteSectionIndex(3),
  ///        }),
  ///     ].into(),
  ///   },
  /// );
  ///
  /// let buffer = Buffer::tokenize("ab\n\n")?;
  /// assert_eq!(
  ///   buffer,
  ///   Buffer {
  ///     interns: InternedTexts {
  ///       interned_text_sections: [
  ///         (TextChecksum { hash: 6148830537548944441, length: 2, code_points: 2 },
  ///          TextSection { contents: "ab".to_string(), num_occurrences: 1 }),
  ///         (TextChecksum { hash: 15130871412783076140, length: 0, code_points: 0 },
  ///          TextSection { contents: "".to_string(), num_occurrences: 2 }),
  ///       ].into_iter().collect(),
  ///     },
  ///     lines_by_bytes: [
  ///       (ByteSectionIndex(0), TextChecksum { hash: 6148830537548944441, length: 2, code_points: 2 }),
  ///       (ByteSectionIndex(3), TextChecksum { hash: 15130871412783076140, length: 0, code_points: 0 }),
  ///       (ByteSectionIndex(4), TextChecksum { hash: 15130871412783076140, length: 0, code_points: 0 }),
  ///     ].into(),
  ///     lines_by_code_points: [
  ///       (CodePointSectionIndex(0),
  ///        ChecksumWithByteSection {
  ///          checksum: TextChecksum { hash: 6148830537548944441, length: 2, code_points: 2 },
  ///          byte_index: ByteSectionIndex(0),
  ///        }),
  ///       (CodePointSectionIndex(3),
  ///        ChecksumWithByteSection {
  ///          checksum: TextChecksum { hash: 15130871412783076140, length: 0, code_points: 0 },
  ///          byte_index: ByteSectionIndex(3),
  ///        }),
  ///       (CodePointSectionIndex(4),
  ///        ChecksumWithByteSection {
  ///          checksum: TextChecksum { hash: 15130871412783076140, length: 0, code_points: 0 },
  ///          byte_index: ByteSectionIndex(4),
  ///        }),
  ///     ].into(),
  ///   },
  /// );
  /// # Ok(())
  /// # }
  ///```
  pub fn tokenize(input: &str) -> Result<Self, BufferError> {
    let mut ret = Self::new();
    let mut checksums = Vec::new();
    let mut last_token_index: usize = 0;
    for TokenIndex { location, length } in NewlineTokenizer.tokenize(input).into_iter() {
      let cur_line = &input[last_token_index..location];
      checksums.push(ret.interns.increment(cur_line)?);
      last_token_index = location + length;
    }
    /* If we ended on a token, then the last line is empty. If there were no tokens, then this is
     * the whole string. */
    let cur_line = &input[last_token_index..];
    checksums.push(ret.interns.increment(cur_line)?);
    ret.process_checksums(checksums);
    assert_eq!(ret.lines_by_bytes.keys().next(), Some(&ByteSectionIndex(0)));
    assert_eq!(
      ret.lines_by_code_points.keys().next(),
      Some(&CodePointSectionIndex(0))
    );
    assert_eq!(ret.lines_by_bytes.len(), ret.lines_by_code_points.len());
    Ok(ret)
  }

  fn process_checksums(&mut self, checksums: Vec<TextChecksum>) {
    let InsertionIndex(mut last_token_index) = self
      .byte_extent()
      /* 1 is the length of '\n', so we advance by that much so as not to hit it again. */
      .map(|InsertionIndex(i)| InsertionIndex(i + 1))
      .unwrap_or_else(|| InsertionIndex(0));
    let CodePointInsertionIndex(mut last_code_point_index) = self
      .code_point_extent()
      /* 1 is the length of '\n', so we advance by that much so as not to hit it again. */
      .map(|CodePointInsertionIndex(i)| CodePointInsertionIndex(i + 1))
      .unwrap_or_else(|| CodePointInsertionIndex(0));
    for checksum in checksums.into_iter() {
      /* Add section to byte sections. */
      let byte_index = ByteSectionIndex(last_token_index);
      self.lines_by_bytes.insert(byte_index, checksum);
      /* Add section to code point sections. */
      self.lines_by_code_points.insert(
        CodePointSectionIndex(last_code_point_index),
        ChecksumWithByteSection {
          checksum,
          byte_index,
        },
      );
      /* 1 is the length of '\n', so we advance by that much so as not to hit it again. */
      last_token_index += checksum.length + 1;
      /* 1 is the length of '\n', so we advance by that much so as not to hit it again. */
      last_code_point_index += checksum.code_points + 1;
    }
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
  /// abc.merge_into_at(def, SectionRange::single(ByteSectionIndex(3)))?;
  /// let ab_de_f = Buffer::tokenize("ab\nde\nf")?;
  /// assert_eq!(abc, ab_de_f);
  ///
  /// abc2.merge_into_at(def2, SectionRange::single(ByteSectionIndex(0)))?;
  /// let de_f_c = Buffer::tokenize("de\nf\nc")?;
  /// assert_eq!(abc2, de_f_c);
  ///
  /// abc3.merge_into_at(def3, SectionRange::new(ByteSectionIndex(0), ByteSectionIndex(3)))?;
  /// let de_f = Buffer::tokenize("de\nf")?;
  /// assert_eq!(abc3, de_f);
  ///
  /// let mut ab_c_def_g = Buffer::tokenize("ab\nc\ndef\ng")?;
  /// let f = Buffer::tokenize("f")?;
  /// ab_c_def_g.merge_into_at(f, SectionRange::new(ByteSectionIndex(3), ByteSectionIndex(5)))?;
  /// let ab_f_g = Buffer::tokenize("ab\nf\ng")?;
  /// assert_eq!(ab_c_def_g, ab_f_g);
  /// # Ok(())
  /// # }
  ///```
  pub fn merge_into_at(&mut self, other: Self, at: SectionRange) -> Result<(), BufferError> {
    /* TODO: make this faster! */
    let Self {
      interns: other_interns,
      lines_by_bytes: other_lines_by_bytes,
      ..
    } = other;
    /* Intern all the new strings from `other`. */
    self.interns.extract_from(other_interns)?;

    let SectionRange { start, end } = at;
    /* Split out the sections of the map that need to be removed or updated. */
    let (occluded, suffix) = {
      let mut occluded_and_suffix = self.lines_by_bytes.split_off(&start);
      let mut suffix = occluded_and_suffix.split_off(&end);
      /* Extract the `end` value from `suffix` and insert into `occluded_and_suffix`. */
      assert_eq!(
        None,
        occluded_and_suffix.insert(end, suffix.remove(&end).unwrap())
      );
      (occluded_and_suffix, suffix)
    };

    /* Remove all code point sections after the prefix, which should be the exact same length as
     * `self.lines_by_bytes()` (the prefix of byte sections). */
    self.lines_by_code_points = self
      .lines_by_code_points
      .iter()
      .take(self.lines_by_bytes.len())
      .map(|(index, checksum)| (*index, *checksum))
      .collect();

    /* De-intern all the strings from `self` that we're no longer using. */
    for (_, checksum) in occluded.into_iter() {
      self.interns.decrement(checksum)?;
    }

    /* Insert lines from `other`, bumping up the token index each time. */
    let other_checksums: Vec<_> = other_lines_by_bytes
      .into_iter()
      .map(|(_, checksum)| checksum)
      .collect();
    self.process_checksums(other_checksums);
    /* Re-insert lines from `suffix`, bumping up the token index each time. */
    let suffix_checksums: Vec<_> = suffix.into_iter().map(|(_, checksum)| checksum).collect();
    self.process_checksums(suffix_checksums);

    Ok(())
  }

  fn get_section(&self, si: ByteSectionIndex) -> Result<&TextChecksum, BufferError> {
    self
      .lines_by_bytes
      .get(&si)
      .ok_or_else(|| BufferError::SectionOutOfBounds(si, self.final_section()))
  }

  fn final_section(&self) -> ByteSectionIndex {
    *self
      .lines_by_bytes
      .keys()
      .last()
      .expect("should have at least one section")
  }

  /// Retrieve the byte index corresponding to the code point index.
  ///
  ///```
  /// # fn main() -> Result<(), emdocs_protocol::state::BufferError> {
  /// use emdocs_protocol::{transforms::*, state::*};
  ///
  /// let ab_c = Buffer::tokenize("ab\nc")?;
  /// let index = ab_c.locate_code_point(Point { code_point_index: 1 })?;
  /// assert_eq!(index, InsertionIndex(1));
  /// let index = ab_c.locate_code_point(Point { code_point_index: 2 })?;
  /// assert_eq!(index, InsertionIndex(2));
  ///
  /// let ae_o = Buffer::tokenize("a̐é\nö̲g")?;
  /// let index = ae_o.locate_code_point(Point { code_point_index: 2 })?;
  /// assert_eq!(index, InsertionIndex(3));
  /// let index = ae_o.locate_code_point(Point { code_point_index: 4 })?;
  /// assert_eq!(index, InsertionIndex(6));
  /// # Ok(())
  /// # }
  ///```
  pub fn locate_code_point(&self, point: transforms::Point) -> Result<InsertionIndex, BufferError> {
    let at = CodePointInsertionIndex::from_point(point);
    let final_index = self
      .code_point_extent()
      .expect("should have been non-empty");
    if at > final_index {
      return Err(BufferError::CodePointOutOfBounds(at, final_index));
    }
    let (section, checksum_with_index) = self
      .lines_by_code_points
      .range(at.section_bounds())
      .last()
      .expect("first element should have been at 0, so this should always produce something");
    assert!(at <= section.end_index(checksum_with_index.checksum.code_points));
    let within_bytes = if at == section.end_index(checksum_with_index.checksum.code_points) {
      checksum_with_index.checksum.length
    } else {
      self
      .interns
      .get_no_eq_check(checksum_with_index.checksum)?
      .contents
      .char_indices()
      /* We want to iterate through as many code points as remain after subtracting the header for
       * this section. If the code point lies on the exact start of the section (so `at` equals
       * `section`), then we want the first code point (which will always start at 0). Therefore, we
       * add one to the number we take. */
      .take(at.0 - section.0 + 1)
        .last()
        .map(|(within_bytes, _)| within_bytes)
      .expect("should have had enough code points in this text section!")
    };
    Ok(InsertionIndex(
      checksum_with_index.byte_index.0 + within_bytes,
    ))
  }

  fn byte_extent(&self) -> Option<InsertionIndex> {
    self
      .lines_by_bytes
      .iter()
      .last()
      .map(|(section, checksum)| section.end_index(checksum.length))
  }

  fn code_point_extent(&self) -> Option<CodePointInsertionIndex> {
    self
      .lines_by_code_points
      .iter()
      .last()
      .map(|(section, checksum_with_index)| {
        section.end_index(checksum_with_index.checksum.code_points)
      })
  }

  fn locate_within_section(&self, at: InsertionIndex) -> Result<FoundSection, BufferError> {
    /* TODO: make this faster (B-tree?)! */
    let final_index = self.byte_extent().expect("should have been non-empty");
    if at > final_index {
      return Err(BufferError::EditOutOfBounds(at, final_index));
    }
    let (section, checksum) = self
      .lines_by_bytes
      .range(at.section_bounds())
      .last()
      .expect("first element should have been at 0, so this should always produce something");
    assert!(at <= section.end_index(checksum.length));
    Ok(FoundSection {
      section: *section,
      within: section.within_index(at),
    })
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
    let Self {
      interns,
      lines_by_bytes,
      lines_by_code_points,
    } = Self::tokenize(s)?;
    self.interns = interns;
    self.lines_by_bytes = lines_by_bytes;
    self.lines_by_code_points = lines_by_code_points;
    Ok(())
  }

  /// ???
  ///
  ///```
  /// # fn main() -> Result<(), emdocs_protocol::state::BufferError> {
  /// use emdocs_protocol::state::*;
  ///
  /// let mut ab_c = Buffer::tokenize("ab\nc")?;
  /// ab_c.delete_at(DeletionRange { beg: InsertionIndex(0), end: InsertionIndex(1) })?;
  /// let b_c = Buffer::tokenize("b\nc")?;
  /// assert_eq!(ab_c, b_c);
  ///
  /// let mut ab_c = Buffer::tokenize("ab\nc")?;
  /// ab_c.delete_at(DeletionRange { beg: InsertionIndex(1), end: InsertionIndex(1) })?;
  /// let ab_c2 = Buffer::tokenize("ab\nc")?;
  /// assert_eq!(ab_c, ab_c2);
  ///
  /// let mut ab_c_def_g = Buffer::tokenize("ab\nc\ndef\ng")?;
  /// ab_c_def_g.delete_at(DeletionRange { beg: InsertionIndex(3), end: InsertionIndex(7) })?;
  /// let ab_f_g = Buffer::tokenize("ab\nf\ng")?;
  /// assert_eq!(ab_c_def_g, ab_f_g);
  ///
  /// let mut ae_o = Buffer::tokenize("a̐é\nö̲g")?;
  /// ae_o.delete_at(DeletionRange { beg: InsertionIndex(3), end: InsertionIndex(12) })?;
  /// let ag = Buffer::tokenize("a̐g")?;
  /// assert_eq!(ae_o, ag);
  ///
  /// let mut ab_c = Buffer::tokenize("ab\nc")?;
  /// ab_c.delete_at(DeletionRange { beg: InsertionIndex(2), end: InsertionIndex(3) })?;
  /// let abc = Buffer::tokenize("abc")?;
  /// assert_eq!(ab_c, abc);
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
      .contents[end.within.0..];
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
        let insertion_index: InsertionIndex = buffer.locate_code_point(point)?;
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

#[cfg(test)]
pub mod proptest_strategies {
  use super::*;

  use proptest::prelude::*;

  /* TODO: generate an arbitrary tokenization string for ConstantTokenizer and for the target
   * string, and then make sure to *use* ConstantTokenizer to extract the positions of existing
   * token indices before adding more in delimiter_indices()! Figure out whether to just remove the
   * automatically generated token indices first (to insert our own) or whether to leave them in
   * and add our own on top! */
  prop_compose! {
    pub fn non_delimited_string(str_len: usize)
      /* TODO: assuming \n is delimiter!!! */
      (in_between in prop::string::string_regex(&format!(r"[^\n]{{0,{}}}", str_len)).unwrap())
      (in_between in Just(in_between))
       -> String {
        in_between
      }
  }
  prop_compose! {
    pub fn delimiter_indices(s: String, newline_factor: f64)
      (mut indices in prop::collection::vec(
        0..s.len(),
        1..((s.len() as f64 / newline_factor).floor() as i32) as usize))
       -> Vec<usize> {
        indices.sort_unstable();
        let mut prior_non_char_boundary: Option<usize> = None;
        let mut ret = Vec::new();
        for ind in indices.into_iter() {
          /* Try to find a char boundary in between the prior and the next value. */
          if let Some(prior_non_char_boundary) = prior_non_char_boundary {
            for new_index in (prior_non_char_boundary + 1)..ind {
              if s.is_char_boundary(new_index) {
                ret.push(new_index);
                break;
              }
            }
            /* If we can't find any before the next proposed delimiter, just forget about that
             * one. */
          }
          /* Save the current proposed delimiter if it can't immediately be used. */
          prior_non_char_boundary = if s.is_char_boundary(ind) {
            ret.push(ind);
            None
          } else {
            Some(ind)
          }
        }
        /* After looping over all the proposed delimiters, ensure that the final one is placed if
         * it was not directly on a char boundary (if possible). */
        if let Some(prior_non_char_boundary) = prior_non_char_boundary {
          for new_index in (prior_non_char_boundary + 1)..s.len() {
            if s.is_char_boundary(new_index) {
              ret.push(new_index);
            }
          }
        }
        ret
    }
  }
  pub fn delimited_input(s: &str, indices: &[usize]) -> String {
    let mut buf = String::new();
    let mut last_start: usize = 0;
    for ind in indices.iter() {
      assert!(*ind <= s.len());
      buf.push_str(&s[last_start..*ind]);
      /* TODO: assuming \n is delimiter!!! */
      buf.push('\n');
      last_start = *ind;
    }
    buf.push_str(&s[last_start..]);
    buf
  }
  prop_compose! {
    pub fn string_with_indices(str_len: usize, newline_factor: f64)
      (in_between in non_delimited_string(str_len))
      (indices in delimiter_indices(in_between.clone(), newline_factor),
       in_between in Just(in_between))
       -> (String, Vec<usize>) {
        (in_between, indices)
      }
  }
  prop_compose! {
    pub fn delimited_string(str_len: usize, newline_factor: f64)
      ((s, indices) in string_with_indices(str_len, newline_factor)) -> String {
        delimited_input(&s, &indices)
      }
  }
  prop_compose! {
    pub fn insertion_index(str_len: usize, newline_factor: f64)
      (s in delimited_string(str_len, newline_factor))
      (mut index in 0..s.len(), s in Just(s))
       -> (String, InsertionIndex) {
        while !s.is_char_boundary(index) {
          index -= 1;
        }
        (s, InsertionIndex(index))
    }
  }
  prop_compose! {
    pub fn deletion_range(str_len: usize, newline_factor: f64)
      (s in delimited_string(str_len, newline_factor))
      (index1 in 0..s.len(), index2 in 0..s.len(), s in Just(s))
       -> (String, DeletionRange) {
        let (mut index1, mut index2) = if index1 <= index2 {
          (index1, index2)
        } else {
          (index2, index1)
        };
        while !s.is_char_boundary(index1) {
          index1 -= 1;
        }
        while !s.is_char_boundary(index2) {
          index2 -= 1;
        }
        (s, DeletionRange {
          beg: InsertionIndex(index1),
          end: InsertionIndex(index2),
        })
      }
  }
  prop_compose! {
    pub fn code_point_index(str_len: usize, newline_factor: f64)
      (s in delimited_string(str_len, newline_factor))
      (code_point in 0..(s.chars().fold(0, |l, _| l + 1) as i32), s in Just(s))
       -> (String, transforms::Point, InsertionIndex) {
        let (byte_index, _) = s.char_indices().skip(code_point as usize).next()
          .expect("code point should have been within range");
        (
          s,
          transforms::Point { code_point_index: code_point as u64 },
          InsertionIndex(byte_index),
        )
      }
  }
}

#[cfg(test)]
mod test {
  use super::{proptest_strategies::*, *};

  use proptest::prelude::*;

  proptest! {
    #[test]
    fn test_tokenize_newlines((s, indices) in string_with_indices(5000, 5.0)) {
      let input = delimited_input(&s, &indices);
      let expected: Vec<TokenIndex> = indices.into_iter().enumerate().map(|(ind, loc)| {
        TokenIndex {
          /* NB: Have to add +ind because with each token-delimited (newline) we push the location
           * of the inserted token one further to the right. It would be 2*ind if the delimiter
           * was 2 chars long. */
          location: loc + ind,
          /* TODO: assuming \n is delimiter!!! */
          length: 1,

        }
      }).collect();
      prop_assert_eq!(NewlineTokenizer.tokenize(&input), expected);
    }
  }
  proptest! {
    #[test]
    fn test_buffer_tokenize_lines((s, indices) in string_with_indices(5000, 5.0)) {
      let input = delimited_input(&s, &indices);
      let buf = Buffer::tokenize(&input).unwrap();
      prop_assert_eq!(buf.lines_by_bytes.len(), indices.len() + 1);
    }
  }
  proptest! {
    #[test]
    fn test_buffer_insert((base, at) in insertion_index(5000, 5.0), s in any::<String>()) {
      let mut buf = Buffer::tokenize(&base).unwrap();
      buf.insert_at(at, &s).unwrap();
      let merged_str = [&base[..at.0], &s, &base[at.0..]].concat();
      let buf2 = Buffer::tokenize(&merged_str).unwrap();
      prop_assert_eq!(buf, buf2);
    }
  }
  proptest! {
    #[test]
    fn test_buffer_delete_none((base, at) in insertion_index(5000, 5.0)) {
      let mut buf = Buffer::tokenize(&base).unwrap();
      let buf2 = buf.clone();
      let range = DeletionRange { beg: at, end: at };
      buf.delete_at(range).unwrap();
      prop_assert_eq!(buf, buf2);
    }
  }
  proptest! {
    #[test]
    fn test_buffer_delete((base, range) in deletion_range(5000, 5.0)) {
      let mut buf = Buffer::tokenize(&base).unwrap();
      buf.delete_at(range).unwrap();
      let spliced_str = [&base[..range.beg.0], &base[range.end.0..]].concat();
      let buf2 = Buffer::tokenize(&spliced_str).unwrap();
      prop_assert_eq!(buf, buf2);
    }
  }
  proptest! {
    #[test]
    fn test_locate_code_point((s, point, expected_index) in code_point_index(5000, 5.0)) {
      let buf = Buffer::tokenize(&s).unwrap();
      let found_index = buf.locate_code_point(point).unwrap();
      prop_assert_eq!(expected_index, found_index);
    }
  }
}
