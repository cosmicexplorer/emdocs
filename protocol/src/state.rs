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

use unicode_buffer::{Buffer, BufferError, CodePointInsertionIndex, InsertionIndex};

use async_lock::RwLock;
use displaydoc::Display;
use indexmap::IndexMap;
use thiserror::Error;

use std::{num, sync::Arc};

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
      .ok_or(BufferMappingError::BufNotFound(id))?
      .clone();
    let mut buffer = buffer.write().await;
    match r#type {
      transforms::TransformType::edit(transforms::Edit { point, payload }) => {
        let point = CodePointInsertionIndex(point.code_point_index as usize);
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
