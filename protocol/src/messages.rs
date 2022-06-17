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
    tonic::include_proto!("emdocs.proto.messages");
  }
}

use crate::{
  buffers::{BufferError, BufferId},
  transforms::{Transform, TransformError},
};

use displaydoc::Display;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use std::io;

#[derive(Debug, Display, Error)]
pub enum ProtocolError {
  /// tonic error: {0}
  Tonic(#[from] tonic::Status),
  /// buffer error {0}
  Buf(#[from] BufferError),
  /// transform error {0}
  Transform(#[from] TransformError),
  /// an error {0} occurred when en/decoding a protobuf
  Proto(#[from] serde_mux::ProtobufCodingFailure),
  /// io error {0}
  Io(#[from] io::Error),
  /// json error {0}
  Json(#[from] serde_json::Error),
}

impl From<prost::DecodeError> for ProtocolError {
  fn from(value: prost::DecodeError) -> Self { Self::Proto(value.into()) }
}

impl From<prost::EncodeError> for ProtocolError {
  fn from(value: prost::EncodeError) -> Self { Self::Proto(value.into()) }
}

/// The interface all clients need to implement!
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Operation {
  pub source: BufferId,
  pub transform: Transform,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
#[allow(non_camel_case_types)]
pub enum OperationResult {
  ok,
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
  op(Operation),
}

/* A universe that receives effects. */
#[tonic::async_trait]
pub trait OperationService {
  async fn process_operation(&self, request: Operation) -> Result<OperationResult, ProtocolError>;
}

/* A conversation between the client and an IDE. */
#[tonic::async_trait]
pub trait IDEService {
  async fn process_ide_message(&self, request: IDEMessage) -> Result<ClientMessage, ProtocolError>;
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
    pub fn new_remote_client()(ip_address in r"[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+") -> RemoteClient {
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

  use serde_mux;

  use std::convert::{TryFrom, TryInto};

  mod operation_result {
    use super::*;

    impl serde_mux::Schema for proto::OperationResult {
      type Source = OperationResult;
    }

    impl TryFrom<proto::OperationResult> for OperationResult {
      type Error = ProtocolError;

      fn try_from(proto_message: proto::OperationResult) -> Result<Self, ProtocolError> {
        let proto::OperationResult { r#type } = proto_message.clone();
        let r#type = r#type.ok_or_else(|| {
          ProtocolError::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
            "type",
            format!("{:?}", proto_message),
          ))
        })?;
        match r#type {
          proto::operation_result::Type::Ok(_) => Ok(Self::ok),
        }
      }
    }

    impl From<OperationResult> for proto::OperationResult {
      fn from(value: OperationResult) -> Self {
        match value {
          OperationResult::ok => proto::OperationResult {
            r#type: Some(proto::operation_result::Type::Ok(proto::OkResult {})),
          },
        }
      }
    }
  }

  mod operation {
    use super::*;

    impl serde_mux::Schema for proto::Operation {
      type Source = Operation;
    }

    impl TryFrom<proto::Operation> for Operation {
      type Error = ProtocolError;

      fn try_from(proto_message: proto::Operation) -> Result<Self, ProtocolError> {
        let proto::Operation { source, transform } = proto_message.clone();
        let source = source.ok_or_else(|| {
          ProtocolError::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
            "source",
            format!("{:?}", proto_message),
          ))
        })?;
        let transform = transform.ok_or_else(|| {
          ProtocolError::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
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
      type Error = ProtocolError;

      fn try_from(proto_message: proto::RemoteClient) -> Result<Self, ProtocolError> {
        let proto::RemoteClient { ip_address } = proto_message.clone();
        let ip_address = ip_address.ok_or_else(|| {
          ProtocolError::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
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
      type Error = ProtocolError;

      fn try_from(proto_message: proto::BufferAssociation) -> Result<Self, ProtocolError> {
        let proto::BufferAssociation { buffer_id, remote } = proto_message.clone();
        let buffer_id = buffer_id.ok_or_else(|| {
          ProtocolError::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
            "buffer_id",
            format!("{:?}", proto_message),
          ))
        })?;
        let remote = remote.ok_or_else(|| {
          ProtocolError::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
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
      type Error = ProtocolError;

      fn try_from(proto_message: proto::IdeMessage) -> Result<Self, ProtocolError> {
        let proto::IdeMessage { r#type } = proto_message.clone();
        let r#type = r#type.ok_or_else(|| {
          ProtocolError::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
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

  mod client_message {
    use super::*;

    impl serde_mux::Schema for proto::ClientMessage {
      type Source = ClientMessage;
    }

    impl TryFrom<proto::ClientMessage> for ClientMessage {
      type Error = ProtocolError;

      fn try_from(proto_message: proto::ClientMessage) -> Result<Self, ProtocolError> {
        let proto::ClientMessage { r#type } = proto_message.clone();
        let r#type = r#type.ok_or_else(|| {
          ProtocolError::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
            "type",
            format!("{:?}", proto_message),
          ))
        })?;
        match r#type {
          proto::client_message::Type::Ok(_) => Ok(Self::ok),
          proto::client_message::Type::Op(op) => Ok(Self::op(op.try_into()?)),
        }
      }
    }

    impl From<ClientMessage> for proto::ClientMessage {
      fn from(value: ClientMessage) -> Self {
        match value {
          ClientMessage::ok => Self {
            r#type: Some(proto::client_message::Type::Ok(proto::OkResult {})),
          },
          ClientMessage::op(op) => Self {
            r#type: Some(proto::client_message::Type::Op(op.into())),
          },
        }
      }
    }
  }
}
