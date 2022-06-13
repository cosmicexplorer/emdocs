/*
 * Description: Buffer identity and metadata.
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

//! Buffer identity and metadata.

/// [`prost`] structs for serializing transforms.
pub mod proto {
  #[doc(inline)]
  pub use proto::*;
  mod proto {
    #![allow(missing_docs)]
    include!(concat!(env!("OUT_DIR"), "/emdocs.proto.buffers.rs"));
  }
}

use uuid::Uuid;

/// A serializable identifier for a buffer sharable across time and space.
///
/// Validate that it can be round-tripped through protobuf:
///```
/// # fn main() -> Result<(), emdocs_protocol::Error> {
/// use serde_mux::{traits::*, Protobuf};
/// use emdocs_protocol::buffers::*;
///
/// let buf = BufferId::default();
/// let buf_proto = Protobuf::<BufferId, proto::BufferId>::new(buf.clone());
/// let bytes = buf_proto.serialize();
/// let buf_serde = Protobuf::<BufferId, proto::BufferId>::deserialize(&bytes)?;
/// assert!(buf == buf_serde);
/// # Ok(())
/// # }
///```
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BufferId {
  pub uuid: Uuid,
}

impl Default for BufferId {
  fn default() -> Self {
    Self {
      uuid: Uuid::new_v4(),
    }
  }
}

mod serde_impl {
  use super::*;
  use crate::error::Error;

  use serde_mux;

  use std::convert::{TryFrom, TryInto};

  mod buffer_id {
    use super::*;

    impl serde_mux::Schema for proto::BufferId {
      type Source = BufferId;
    }

    impl TryFrom<proto::BufferId> for BufferId {
      type Error = Error;

      fn try_from(proto_message: proto::BufferId) -> Result<Self, Error> {
        let proto::BufferId { uuid } = proto_message.clone();
        let uuid: [u8; 16] = uuid
          .clone()
          .ok_or_else(|| {
            Error::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
              "uuid",
              format!("{:?}", proto_message),
            ))
          })?
          .try_into()
          .map_err(|e| serde_mux::ProtobufCodingFailure::SliceLength(16, format!("{:?}", e)))?;
        Ok(Self {
          uuid: Uuid::from_bytes(uuid),
        })
      }
    }

    impl From<BufferId> for proto::BufferId {
      fn from(value: BufferId) -> Self {
        let BufferId { uuid } = value;
        proto::BufferId {
          uuid: Some(uuid.as_bytes().to_vec()),
        }
      }
    }
  }
}
