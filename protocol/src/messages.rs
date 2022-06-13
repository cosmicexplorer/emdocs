/*
 * Description: Messages sent between clients.
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

//! Messages sent between clients.
//!
//!```
//! # fn main() -> Result<(), emdocs_protocol::Error> {
//! use serde_mux::{traits::*, Protobuf};
//! use emdocs_protocol::{
//!   buffers::BufferId,
//!   messages::*,
//!   transforms::{Edit, EditPayload, Insert, Point},
//! };
//!
//! let insert = Insert { contents: "hey".to_string() };
//! let edit = Edit { point: Point::default(), payload: EditPayload::Insert(insert) };
//! let transform = Transform {
//!   source: BufferId::default(),
//!   r#type: TransformType::Edit(edit),
//! };
//! let msg = Message::Transform(transform);
//! let msg_proto = Protobuf::<Message, proto::Message>::new(msg.clone());
//! let buf = msg_proto.serialize();
//! let msg_serde = Protobuf::<Message, proto::Message>::deserialize(&buf)?;
//! assert!(msg == msg_serde);
//! # Ok(())
//! # }
//!```

/// [`prost`] structs for serializing messages.
pub mod proto {
  pub use crate::{buffers::proto as buffers, transforms::proto as transforms};
  #[doc(inline)]
  pub use proto::*;
  mod proto {
    #![allow(missing_docs)]
    include!(concat!(env!("OUT_DIR"), "/emdocs.proto.messages.rs"));
  }
}

use crate::{buffers::BufferId, transforms::Edit};

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
#[allow(non_camel_case_types)]
pub enum TransformType {
  edit(Edit),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Transform {
  pub source: BufferId,
  pub r#type: TransformType,
}

/// The interface all clients need to implement!
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
#[allow(non_camel_case_types)]
pub enum Message {
  transform(Transform),
}

#[cfg(test)]
pub mod proptest_strategies {
  use super::*;
  use crate::{buffers::proptest_strategies::*, transforms::proptest_strategies::*};

  use proptest::{prelude::*, strategy::Strategy};

  pub fn new_transform_type() -> impl Strategy<Value=TransformType> {
    prop_oneof![new_edit().prop_map(TransformType::edit),]
  }
  prop_compose! {
    pub fn new_transform()(source in new_buffer_id(), r#type in new_transform_type()) -> Transform {
      Transform { source, r#type }
    }
  }
  pub fn new_message() -> impl Strategy<Value=Message> {
    prop_oneof![new_transform().prop_map(Message::Transform),]
  }
}

mod serde_impl {
  #[cfg(test)]
  use super::proptest_strategies::*;
  use super::*;
  use crate::error::Error;

  use serde_mux;

  use std::convert::{TryFrom, TryInto};

  mod transform {
    use super::*;

    impl serde_mux::Schema for proto::Transform {
      type Source = Transform;
    }

    impl TryFrom<proto::Transform> for Transform {
      type Error = Error;

      fn try_from(proto_message: proto::Transform) -> Result<Self, Error> {
        let proto::Transform { source, r#type } = proto_message.clone();
        let source: BufferId = source
          .ok_or_else(|| {
            Error::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
              "source",
              format!("{:?}", proto_message),
            ))
          })?
          .try_into()?;
        let r#type = r#type.ok_or_else(|| {
          Error::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
            "type",
            format!("{:?}", proto_message),
          ))
        })?;
        let r#type = match r#type {
          proto::transform::Type::Edit(edit) => TransformType::edit(edit.try_into()?),
        };
        Ok(Self { source, r#type })
      }
    }

    impl From<Transform> for proto::Transform {
      fn from(value: Transform) -> Self {
        let Transform { source, r#type } = value;
        let r#type = match r#type {
          TransformType::edit(edit) => proto::transform::Type::Edit(edit.into()),
        };
        Self {
          source: Some(source.into()),
          r#type: Some(r#type),
        }
      }
    }
  }

  mod message {
    use super::*;

    impl serde_mux::Schema for proto::Message {
      type Source = Message;
    }

    impl TryFrom<proto::Message> for Message {
      type Error = Error;

      fn try_from(proto_message: proto::Message) -> Result<Self, Error> {
        let proto::Message { r#type } = proto_message.clone();
        let r#type = r#type.ok_or_else(|| {
          Error::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
            "type",
            format!("{:?}", proto_message),
          ))
        })?;
        Ok(match r#type {
          proto::message::Type::Transform(transform) => Message::transform(transform.try_into()?),
        })
      }
    }

    impl From<Message> for proto::Message {
      fn from(value: Message) -> Self {
        let r#type = match value {
          Message::transform(transform) => proto::message::Type::Transform(transform.into()),
        };
        Self {
          r#type: Some(r#type),
        }
      }
    }

    #[cfg(test)]
    mod test {
      use super::*;

      use serde_mux::traits::*;

      use proptest::prelude::*;

      proptest! {
        #[test]
        fn test_serde_message(message in new_message()) {
          let protobuf = serde_mux::Protobuf::<Message, proto::Message>::new(message.clone());
          let buf: Box<[u8]> = protobuf.serialize();
          let resurrected =
            serde_mux::Protobuf::<Message, proto::Message>::deserialize(&buf).unwrap();
          prop_assert_eq!(message, resurrected);
        }
      }
    }
  }
}
