/*
 * Description: Buffer transform messages.
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

//! Buffer transform messages.

/// [`prost`] structs for serializing transforms.
pub mod proto {
  #[doc(inline)]
  pub use proto::*;
  mod proto {
    #![allow(missing_docs)]
    include!(concat!(env!("OUT_DIR"), "/emdocs.proto.transforms.rs"));
  }
}

use displaydoc::Display;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Insert {
  pub contents: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Delete {
  pub distance: u32,
}

/// <point @ {code_point_index}>
#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Point {
  pub code_point_index: u32,
}

impl Default for Point {
  fn default() -> Self {
    Self {
      code_point_index: 0,
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum EditPayload {
  Insert(Insert),
  Delete(Delete),
}

/// An operational transform editing a buffer.
///
/// Validate that it can be round-tripped through protobuf:
///```
/// # fn main() -> Result<(), emdocs_protocol::Error> {
/// use serde_mux::{traits::*, Protobuf};
/// use emdocs_protocol::transforms::*;
///
/// let insert = Insert { contents: "hey".to_string() };
/// let edit = Edit { point: Point::default(), payload: EditPayload::Insert(insert) };
/// let edit_proto = Protobuf::<Edit, proto::Edit>::new(edit.clone());
/// let buf = edit_proto.serialize();
/// let edit_serde = Protobuf::<Edit, proto::Edit>::deserialize(&buf)?;
/// assert!(edit == edit_serde);
/// # Ok(())
/// # }
///```
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Edit {
  pub point: Point,
  pub payload: EditPayload,
}

#[cfg(test)]
pub mod proptest_strategies {
  use super::*;

  use proptest::{prelude::*, strategy::Strategy};

  prop_compose! {
    pub fn new_insert()(contents in any::<String>()) -> Insert {
      Insert { contents }
    }
  }
  prop_compose! {
    pub fn new_delete()(distance in any::<u32>()) -> Delete {
      Delete { distance }
    }
  }
  prop_compose! {
    pub fn new_point()(code_point_index in any::<u32>()) -> Point {
      Point { code_point_index }
    }
  }
  pub fn new_edit_payload() -> impl Strategy<Value=EditPayload> {
    prop_oneof![
      new_insert().prop_map(EditPayload::Insert),
      new_delete().prop_map(EditPayload::Delete),
    ]
  }
  prop_compose! {
    pub fn new_edit()(point in new_point(), payload in new_edit_payload()) -> Edit {
      Edit { point, payload }
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

  mod insert {
    use super::*;

    impl serde_mux::Schema for proto::Insert {
      type Source = Insert;
    }

    impl TryFrom<proto::Insert> for Insert {
      type Error = Error;

      fn try_from(proto_message: proto::Insert) -> Result<Self, Error> {
        let proto::Insert { contents } = proto_message.clone();
        let contents = contents.ok_or_else(|| {
          Error::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
            "contents",
            format!("{:?}", proto_message),
          ))
        })?;
        Ok(Self { contents })
      }
    }

    impl From<Insert> for proto::Insert {
      fn from(value: Insert) -> Self {
        let Insert { contents } = value;
        proto::Insert {
          contents: Some(contents),
        }
      }
    }
  }

  mod delete {
    use super::*;

    impl serde_mux::Schema for proto::Delete {
      type Source = Delete;
    }

    impl TryFrom<proto::Delete> for Delete {
      type Error = Error;

      fn try_from(proto_message: proto::Delete) -> Result<Self, Error> {
        let proto::Delete { distance } = proto_message.clone();
        let distance = distance.ok_or_else(|| {
          Error::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
            "distance",
            format!("{:?}", proto_message),
          ))
        })?;
        Ok(Self { distance })
      }
    }

    impl From<Delete> for proto::Delete {
      fn from(value: Delete) -> Self {
        let Delete { distance } = value;
        proto::Delete {
          distance: Some(distance),
        }
      }
    }
  }

  mod point {
    use super::*;

    impl serde_mux::Schema for proto::Point {
      type Source = Point;
    }

    impl TryFrom<proto::Point> for Point {
      type Error = Error;

      fn try_from(proto_message: proto::Point) -> Result<Self, Error> {
        let proto::Point { code_point_index } = proto_message.clone();
        let code_point_index = code_point_index.ok_or_else(|| {
          Error::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
            "code_point_index",
            format!("{:?}", proto_message),
          ))
        })?;
        Ok(Self { code_point_index })
      }
    }

    impl From<Point> for proto::Point {
      fn from(value: Point) -> Self {
        let Point { code_point_index } = value;
        proto::Point {
          code_point_index: Some(code_point_index),
        }
      }
    }
  }

  mod edit {
    use super::*;

    impl serde_mux::Schema for proto::Edit {
      type Source = Edit;
    }

    impl TryFrom<proto::Edit> for Edit {
      type Error = Error;

      fn try_from(proto_message: proto::Edit) -> Result<Self, Error> {
        let proto::Edit { point, payload } = proto_message.clone();
        let point: Point = point
          .ok_or_else(|| {
            Error::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
              "point",
              format!("{:?}", proto_message),
            ))
          })?
          .try_into()?;
        let payload = payload.ok_or_else(|| {
          Error::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
            "payload",
            format!("{:?}", proto_message),
          ))
        })?;
        let payload = match payload {
          proto::edit::Payload::Insert(insert) => EditPayload::Insert(insert.try_into()?),
          proto::edit::Payload::Delete(delete) => EditPayload::Delete(delete.try_into()?),
        };
        Ok(Self { point, payload })
      }
    }

    impl From<Edit> for proto::Edit {
      fn from(value: Edit) -> Self {
        let Edit { point, payload } = value;
        let payload = match payload {
          EditPayload::Insert(insert) => proto::edit::Payload::Insert(insert.into()),
          EditPayload::Delete(delete) => proto::edit::Payload::Delete(delete.into()),
        };
        Self {
          point: Some(point.into()),
          payload: Some(payload),
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
        fn test_serde_edit(edit in new_edit()) {
          let protobuf = serde_mux::Protobuf::<Edit, proto::Edit>::new(edit.clone());
          let buf: Box<[u8]> = protobuf.serialize();
          let resurrected =
            serde_mux::Protobuf::<Edit, proto::Edit>::deserialize(&buf).unwrap();
          prop_assert_eq!(edit, resurrected);
        }
      }
    }
  }
}
