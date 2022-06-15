/*
 * Description: Wrapper messages for p2p propagation.
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

//! Wrapper messages for p2p propagation.

/// [`prost`] structs for serializing p2p messages.
pub mod proto {
  #[doc(inline)]
  pub use proto::*;
  mod proto {
    #![allow(missing_docs)]
    tonic::include_proto!("emdocs.proto.p2p");
  }
}

use displaydoc::Display;
use thiserror::Error;
use uuid::Uuid;

#[derive(Debug, Display, Error)]
pub enum P2pError {
  /// protobuf error {0}
  Proto(#[from] serde_mux::ProtobufCodingFailure),
}

impl From<prost::DecodeError> for P2pError {
  fn from(value: prost::DecodeError) -> Self { Self::Proto(value.into()) }
}

impl From<prost::EncodeError> for P2pError {
  fn from(value: prost::EncodeError) -> Self { Self::Proto(value.into()) }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct P2pMessageID {
  pub uuid: Uuid,
}

impl Default for P2pMessageID {
  fn default() -> Self {
    Self {
      uuid: Uuid::new_v4(),
    }
  }
}

#[cfg(test)]
pub mod proptest_strategies {
  use super::*;
  use crate::proptest_strategies::*;

  use proptest::prelude::*;

  prop_compose! {
    pub fn new_p2p_id()(uuid in new_uuid()) -> P2pMessageID {
      P2pMessageID { uuid }
    }
  }
}

mod serde_impl {
  #[cfg(test)]
  use super::proptest_strategies::*;
  use super::*;

  use serde_mux;

  use std::convert::{TryFrom, TryInto};

  mod p2p_message_id {
    use super::*;

    use serde::{
      de::{Deserialize, Deserializer, MapAccess, Visitor},
      ser::{Serialize, SerializeStruct, Serializer},
    };

    use std::fmt;

    impl serde_mux::Schema for proto::P2pMessageId {
      type Source = P2pMessageID;
    }

    impl TryFrom<proto::P2pMessageId> for P2pMessageID {
      type Error = P2pError;

      fn try_from(proto_message: proto::P2pMessageId) -> Result<Self, P2pError> {
        let proto::P2pMessageId { uuid } = proto_message.clone();
        let uuid: [u8; 16] = uuid
          .clone()
          .ok_or_else(|| {
            P2pError::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
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

    impl From<P2pMessageID> for proto::P2pMessageId {
      fn from(value: P2pMessageID) -> Self {
        let P2pMessageID { uuid } = value;
        proto::P2pMessageId {
          uuid: Some(uuid.as_bytes().to_vec()),
        }
      }
    }


    impl Serialize for P2pMessageID {
      fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
      where S: Serializer {
        let mut buffer_id = serializer.serialize_struct("P2pMessageID", 1)?;
        buffer_id.serialize_field("uuid", &self.uuid.as_bytes())?;
        buffer_id.end()
      }
    }

    struct P2pMessageIDVisitor;

    impl<'de> Visitor<'de> for P2pMessageIDVisitor {
      type Value = P2pMessageID;

      fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "A p2p message id")
      }

      fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
      where A: MapAccess<'de> {
        let (k, v): (String, Vec<u8>) = map.next_entry()?.expect("uuid key must exist");
        assert_eq!(k, "uuid");
        Ok(P2pMessageID {
          uuid: Uuid::from_bytes(v.try_into().expect("uuid bytes wrong length")),
        })
      }
    }

    impl<'de> Deserialize<'de> for P2pMessageID {
      fn deserialize<D>(deserializer: D) -> Result<P2pMessageID, D::Error>
      where D: Deserializer<'de> {
        deserializer.deserialize_struct("P2pMessageID", &["uuid"], P2pMessageIDVisitor)
      }
    }

    #[cfg(test)]
    mod test {
      use super::*;

      use serde_mux::{Deserializer, Serializer};

      use proptest::prelude::*;

      proptest! {
        #[test]
        fn test_serde_p2p_id(p2p_id in new_p2p_id()) {
          let protobuf =
            serde_mux::Protobuf::<P2pMessageID, proto::P2pMessageId>::new(p2p_id.clone());
          let buf: Box<[u8]> = protobuf.serialize();
          let resurrected =
            serde_mux::Protobuf::<P2pMessageID, proto::P2pMessageId>::deserialize(&buf).unwrap();
          prop_assert_eq!(p2p_id, resurrected);
        }
      }
    }
  }
}
