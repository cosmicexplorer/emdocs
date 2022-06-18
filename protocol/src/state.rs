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

use indexmap::IndexMap;

use std::{
  collections::{hash_map::DefaultHasher, BTreeMap},
  hash::Hasher,
};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
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

  pub fn get_no_eq_check(&self, checksum: &TextChecksum) -> &TextSection {
    self
      .interned_text_sections
      .get(checksum)
      .expect("checksum must exist")
  }

  pub fn increment(&mut self, text: &str) -> TextChecksum {
    let checksum = TextChecksum::extract(text);
    self
      .interned_text_sections
      .entry(checksum)
      .and_modify(|text_section| {
        assert_eq!(
          text, &text_section.contents,
          "we never expect a hash collision!!!!???"
        );
        text_section.num_occurrences += 1;
      })
      .or_insert_with(|| TextSection {
        contents: text.to_string(),
        num_occurrences: 1,
      });
    checksum
  }

  pub fn decrement(&mut self, checksum: &TextChecksum) {
    let num_occurrences = self
      .interned_text_sections
      .get_mut(checksum)
      .map(|text_section| {
        text_section.num_occurrences -= 1;
        text_section.num_occurrences
      })
      .expect("expected checksum to exist to be decremented");
    if num_occurrences == 0 {
      assert!(self.interned_text_sections.remove(checksum).is_some());
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SectionIndex(pub usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Buffer {
  pub interns: InternedTexts,
  pub lines: Vec<TextChecksum>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct InsertionIndex(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
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

impl Buffer {
  /// Tokenize a string into a buffer.
  ///
  ///```
  /// use emdocs_protocol::state::*;
  ///
  /// let buffer = Buffer::tokenize("ab\nc\n\nef\ng");
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
  /// let buffer = Buffer::tokenize("ab\n");
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
  /// let buffer = Buffer::tokenize("ab\n\n");
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
  ///```
  pub fn tokenize(input: &str) -> Self {
    let mut interns = InternedTexts::new();
    let mut lines = Vec::new();
    let mut last_token_index: usize = 0;
    for TokenIndex { location, length } in NewlineTokenizer.tokenize(input).into_iter() {
      let cur_line = &input[last_token_index..location];
      let checksum = interns.increment(cur_line);
      lines.push(checksum);
      last_token_index = location + length;
    }
    /* If we ended on a token, then the last line is empty. */
    let cur_line = if last_token_index == input.len() {
      ""
    } else {
      /* If there were no tokens, then this is the whole string. */
      &input[last_token_index..]
    };
    let checksum = interns.increment(cur_line);
    lines.push(checksum);
    Self { interns, lines }
  }

  /// ???
  ///
  ///```
  /// use emdocs_protocol::state::*;
  ///
  /// let mut abc = Buffer::tokenize("ab\nc");
  /// let def = Buffer::tokenize("de\nf");
  /// let mut abc2 = abc.clone();
  /// let def2 = def.clone();
  ///
  /// abc.merge_into_at(def, SectionIndex(1));
  /// let abcdef = Buffer::tokenize("ab\nde\nf");
  /// assert_eq!(abc, abcdef);
  ///
  /// abc2.merge_into_at(def2, SectionIndex(0));
  /// let defabc = Buffer::tokenize("de\nf\nc");
  /// assert_eq!(abc2, defabc);
  ///```
  pub fn merge_into_at(&mut self, other: Self, at: SectionIndex) {
    /* TODO: make this faster! */
    let Self { interns, lines } = other;
    for (checksum, text_section) in interns.interned_text_sections.into_iter() {
      assert_eq!(checksum, self.interns.increment(&text_section.contents));
    }
    let SectionIndex(at) = at;
    assert!(at < self.lines.len(), "cannot merge past end of lines!");
    self.interns.decrement(&self.lines[at]);
    self.lines = self.lines[..at]
      .iter()
      .cloned()
      .chain(lines.into_iter())
      .chain(self.lines[at + 1..].iter().cloned())
      .collect();
  }

  /// ???
  ///
  ///```
  /// use emdocs_protocol::state::*;
  ///
  /// let mut abc = Buffer::tokenize("ab\nc");
  /// let mut abc2 = abc.clone();
  /// let mut abc3 = abc.clone();
  ///
  /// abc.insert_at(InsertionIndex(2), "de\nf");
  /// let abde_f_c = Buffer::tokenize("abde\nf\nc");
  /// assert_eq!(abc, abde_f_c);
  ///
  /// abc2.insert_at(InsertionIndex(0), "de\nf");
  /// let de_fab_c = Buffer::tokenize("de\nfab\nc");
  /// assert_eq!(abc2, de_fab_c);
  ///
  /// abc3.insert_at(InsertionIndex(3), "de\nf");
  /// let ab_de_fc = Buffer::tokenize("ab\nde\nfc");
  /// assert_eq!(abc3, ab_de_fc);
  ///```
  pub fn insert_at(&mut self, at: InsertionIndex, s: &str) {
    /* TODO: make this faster! */
    let InsertionIndex(at) = at;
    dbg!(at);
    let mut cur_location: usize = 0;
    let mut found_section: Option<(SectionIndex, usize)> = None;
    for (section_index, checksum) in self.lines.iter().enumerate() {
      dbg!(cur_location);
      assert!(cur_location <= at);
      /* 1 is the length of '\n', so we advance by that much so as not to hit it again. */
      if cur_location + checksum.length + 1 > at {
        let within_section = at - cur_location;
        found_section = Some((SectionIndex(section_index), within_section));
        break;
      } else {
        /* 1 is the length of '\n', so we advance by that much so as not to hit it again. */
        cur_location += checksum.length + 1;
      }
    }
    let (section_index, within_section) = found_section.unwrap_or_else(|| {
      if at > cur_location {
        unreachable!(
          "tried to insert at {}, when max position in buffer was {}",
          at, cur_location
        );
      }
      assert_eq!(at, cur_location);
      assert!(!self.lines.is_empty());
      let final_section = self.lines.len() - 1;
      (
        SectionIndex(final_section),
        self.lines[final_section].length,
      )
    });
    let current_line = &self
      .interns
      .get_no_eq_check(&self.lines[section_index.0])
      .contents;
    let new_line_string = [
      &current_line[..within_section],
      s,
      &current_line[within_section..],
    ]
    .concat();
    self.merge_into_at(Self::tokenize(&new_line_string), section_index);
  }
}
