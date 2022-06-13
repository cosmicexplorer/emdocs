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
  doc(Operation),
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

  use proptest::prelude::*;

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

    #[cfg(test)]
    mod test {
      use super::*;

      use serde_mux::traits::*;

      use proptest::prelude::*;

      proptest! {
        #[test]
        fn test_serde_operation(operation in new_operation()) {
          let protobuf = serde_mux::Protobuf::<Operation, proto::Operation>::new(operation.clone());
          let buf: Box<[u8]> = protobuf.serialize();
          let resurrected =
            serde_mux::Protobuf::<Operation, proto::Operation>::deserialize(&buf).unwrap();
          prop_assert_eq!(operation, resurrected);
        }
      }
    }
  }
}
