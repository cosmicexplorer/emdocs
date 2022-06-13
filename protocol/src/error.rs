/*
 * Description: Define top-level error types.
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

//! Define top-level [enum@Error] types.

use serde_mux;

use displaydoc::Display;
use thiserror::Error;

/// Parent error type for this crate.
#[derive(Debug, Display, Error)]
pub enum Error {
  /// an error {0} occurred when en/decoding a protobuf
  Proto(#[from] serde_mux::ProtobufCodingFailure),
}

impl From<prost::DecodeError> for Error {
  fn from(value: prost::DecodeError) -> Self { Self::Proto(value.into()) }
}

impl From<prost::EncodeError> for Error {
  fn from(value: prost::EncodeError) -> Self { Self::Proto(value.into()) }
}
