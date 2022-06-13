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
//!   transforms::{Edit, EditPayload, Insert, Point, Transform, TransformType},
//! };
//!
//! let insert = Insert { contents: "hey".to_string() };
//! let edit = Edit { point: Point::default(), payload: EditPayload::insert(insert) };
//! let op = Operation {
//!   source: BufferId::default(),
//!   transform: Transform { r#type: TransformType::edit(edit) },
//! };
//! let op_proto = Protobuf::<Operation, proto::Operation>::new(op.clone());
//! let buf = op_proto.serialize();
//! let op_serde = Protobuf::<Operation, proto::Operation>::deserialize(&buf)?;
//! assert!(op == op_serde);
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

use crate::{buffers::BufferId, transforms::Transform};

use serde::{Deserialize, Serialize};

/// The interface all clients need to implement!
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Operation {
  pub source: BufferId,
  pub transform: Transform,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
#[allow(non_camel_case_types)]
pub enum IDEMessage {
  op(Operation),
  link(BufferAssociation),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct RemoteClient {
  pub ip_address: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct BufferAssociation {
  pub buffer_id: BufferId,
  pub remote: RemoteClient,
}

/// The JSON interface from this executable to IDEs.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
#[allow(non_camel_case_types)]
pub enum ClientMessage {
  ok,
}

#[cfg(test)]
pub mod proptest_strategies {
  use super::*;
  use crate::{buffers::proptest_strategies::*, transforms::proptest_strategies::*};

  use proptest::{prelude::*, strategy::Strategy};

  prop_compose! {
    pub fn operation_given_source(source: BufferId)(transform in new_transform()) -> Operation {
      Operation {
        source,
        transform,
      }
    }
  }
  prop_compose! {
    pub fn new_operation()(source in new_buffer_id())(op in operation_given_source(source)) -> Operation {
      op
    }
  }
  prop_compose! {
    pub fn new_remote_client()(ip_address in any::<String>()) -> RemoteClient {
      RemoteClient { ip_address }
    }
  }
  prop_compose! {
    pub fn new_buffer_association()(buffer_id in new_buffer_id(), remote in new_remote_client()) -> BufferAssociation {
      BufferAssociation { buffer_id, remote }
    }
  }
  pub fn new_ide_msg() -> impl Strategy<Value=IDEMessage> {
    prop_oneof![
      new_operation().prop_map(IDEMessage::op),
      new_buffer_association().prop_map(IDEMessage::link),
    ]
  }
}

mod serde_impl {
  #[cfg(test)]
  use super::proptest_strategies::*;
  use super::*;
  use crate::error::Error;

  use serde_mux;

  use std::convert::{TryFrom, TryInto};

  mod operation {
    use super::*;

    impl serde_mux::Schema for proto::Operation {
      type Source = Operation;
    }

    impl TryFrom<proto::Operation> for Operation {
      type Error = Error;

      fn try_from(proto_message: proto::Operation) -> Result<Self, Error> {
        let proto::Operation { source, transform } = proto_message.clone();
        let source = source.ok_or_else(|| {
          Error::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
            "source",
            format!("{:?}", proto_message),
          ))
        })?;
        let transform = transform.ok_or_else(|| {
          Error::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
            "transform",
            format!("{:?}", proto_message),
          ))
        })?;
        Ok(Self {
          source: source.try_into()?,
          transform: transform.try_into()?,
        })
      }
    }

    impl From<Operation> for proto::Operation {
      fn from(value: Operation) -> Self {
        let Operation { source, transform } = value;
        Self {
          source: Some(source.into()),
          transform: Some(transform.into()),
        }
      }
    }
  }

  mod remote_client {
    use super::*;

    impl serde_mux::Schema for proto::RemoteClient {
      type Source = RemoteClient;
    }

    impl TryFrom<proto::RemoteClient> for RemoteClient {
      type Error = Error;

      fn try_from(proto_message: proto::RemoteClient) -> Result<Self, Error> {
        let proto::RemoteClient { ip_address } = proto_message.clone();
        let ip_address = ip_address.ok_or_else(|| {
          Error::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
            "ip_address",
            format!("{:?}", proto_message),
          ))
        })?;
        Ok(Self { ip_address })
      }
    }

    impl From<RemoteClient> for proto::RemoteClient {
      fn from(value: RemoteClient) -> Self {
        let RemoteClient { ip_address } = value;
        Self {
          ip_address: Some(ip_address.into()),
        }
      }
    }
  }

  mod buffer_association {
    use super::*;

    impl serde_mux::Schema for proto::BufferAssociation {
      type Source = BufferAssociation;
    }

    impl TryFrom<proto::BufferAssociation> for BufferAssociation {
      type Error = Error;

      fn try_from(proto_message: proto::BufferAssociation) -> Result<Self, Error> {
        let proto::BufferAssociation { buffer_id, remote } = proto_message.clone();
        let buffer_id = buffer_id.ok_or_else(|| {
          Error::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
            "buffer_id",
            format!("{:?}", proto_message),
          ))
        })?;
        let remote = remote.ok_or_else(|| {
          Error::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
            "remote",
            format!("{:?}", proto_message),
          ))
        })?;
        Ok(Self {
          buffer_id: buffer_id.try_into()?,
          remote: remote.try_into()?,
        })
      }
    }

    impl From<BufferAssociation> for proto::BufferAssociation {
      fn from(value: BufferAssociation) -> Self {
        let BufferAssociation { buffer_id, remote } = value;
        Self {
          buffer_id: Some(buffer_id.into()),
          remote: Some(remote.into()),
        }
      }
    }
  }

  mod ide_msg {
    use super::*;

    impl serde_mux::Schema for proto::IdeMessage {
      type Source = IDEMessage;
    }

    impl TryFrom<proto::IdeMessage> for IDEMessage {
      type Error = Error;

      fn try_from(proto_message: proto::IdeMessage) -> Result<Self, Error> {
        let proto::IdeMessage { r#type } = proto_message.clone();
        let r#type = r#type.ok_or_else(|| {
          Error::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
            "type",
            format!("{:?}", proto_message),
          ))
        })?;
        Ok(match r#type {
          proto::ide_message::Type::Op(op) => IDEMessage::op(op.try_into()?),
          proto::ide_message::Type::Link(link) => IDEMessage::link(link.try_into()?),
        })
      }
    }

    impl From<IDEMessage> for proto::IdeMessage {
      fn from(value: IDEMessage) -> Self {
        Self {
          r#type: Some(match value {
            IDEMessage::op(op) => proto::ide_message::Type::Op(op.into()),
            IDEMessage::link(link) => proto::ide_message::Type::Link(link.into()),
          }),
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
        fn test_serde_ide_msg(ide_msg in new_ide_msg()) {
          let protobuf = serde_mux::Protobuf::<IDEMessage, proto::IdeMessage>::new(ide_msg.clone());
          let buf: Box<[u8]> = protobuf.serialize();
          let resurrected =
            serde_mux::Protobuf::<IDEMessage, proto::IdeMessage>::deserialize(&buf).unwrap();
          prop_assert_eq!(ide_msg, resurrected);
        }
      }
    }
  }
}
