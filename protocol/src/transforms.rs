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
//!
//!```
//! # fn main() -> Result<(), emdocs_protocol::Error> {
//! use serde_mux::{traits::*, Protobuf};
//! use emdocs_protocol::transforms::*;
//!
//! let insert = Insert { contents: "hey".to_string() };
//! let edit = Edit { point: Point::default(), payload: EditPayload::insert(insert) };
//! let edit_proto = Protobuf::<Edit, proto::Edit>::new(edit.clone());
//! let buf = edit_proto.serialize();
//! let edit_serde = Protobuf::<Edit, proto::Edit>::deserialize(&buf)?;
//! assert!(edit == edit_serde);
//! # Ok(())
//! # }
//!```

/// [`prost`] structs for serializing transforms.
pub mod proto {
  pub use crate::buffers::proto as buffers;
  #[doc(inline)]
  pub use proto::*;
  mod proto {
    #![allow(missing_docs)]
    tonic::include_proto!("emdocs.proto.transforms");
  }
}

use displaydoc::Display;
use serde::{Deserialize, Serialize};
use thiserror::Error;

#[derive(Debug, Display, Error)]
pub enum TransformError {
  /// an error {0} occurred when en/decoding a protobuf
  Proto(#[from] serde_mux::ProtobufCodingFailure),
}

impl From<prost::DecodeError> for TransformError {
  fn from(value: prost::DecodeError) -> Self { Self::Proto(value.into()) }
}

impl From<prost::EncodeError> for TransformError {
  fn from(value: prost::EncodeError) -> Self { Self::Proto(value.into()) }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Insert {
  pub contents: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Delete {
  pub distance: u64,
}

/// <point @ {code_point_index}>
#[derive(
  Debug, Display, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize,
)]
pub struct Point {
  pub code_point_index: u64,
}

#[allow(clippy::derivable_impls)]
impl Default for Point {
  fn default() -> Self {
    Self {
      code_point_index: 0,
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
#[allow(non_camel_case_types)]
pub enum EditPayload {
  insert(Insert),
  delete(Delete),
}

/// An operational transform editing a buffer.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Edit {
  pub point: Point,
  pub payload: EditPayload,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Region {
  pub start: Point,
  pub end: Point,
}

pub const CHECKSUM_LENGTH: usize = 32;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct SyncChecksum {
  pub checksum: [u8; CHECKSUM_LENGTH],
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct SyncContents {
  pub contents: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
#[allow(non_camel_case_types)]
pub enum SyncRegionType {
  checksum(SyncChecksum),
  contents(SyncContents),
}

/* FIXME: take a look at this discussion on differential synchronization, an alternative method
 * which may be less complex (and is reminiscent of the signal protocol's handling of out-of-order
 * messages!!)
 *
 * - tweet: https://twitter.com/toddewilliams/status/1549433609005711360?s=20&t=-eLMr1iPQvR24oFgRpwJwA
 * - paper: https://static.googleusercontent.com/media/research.google.com/en//pubs/archive/35605.pdf
 * - archive: https://web.archive.org/web/20200930235331/https://static.googleusercontent.com/media/research.google.com/en//pubs/archive/35605.pdf
 */
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct SyncRegion {
  pub region: Region,
  pub r#type: SyncRegionType,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
#[allow(non_camel_case_types)]
pub enum TransformType {
  edit(Edit),
  sync(SyncRegion),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Transform {
  pub r#type: TransformType,
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
    pub fn new_delete()(distance in any::<u64>()) -> Delete {
      Delete { distance }
    }
  }
  prop_compose! {
    pub fn new_point()(code_point_index in any::<u64>()) -> Point {
      Point { code_point_index }
    }
  }
  pub fn new_edit_payload() -> impl Strategy<Value=EditPayload> {
    prop_oneof![
      new_insert().prop_map(EditPayload::insert),
      new_delete().prop_map(EditPayload::delete),
    ]
  }
  prop_compose! {
    pub fn new_edit()(point in new_point(), payload in new_edit_payload()) -> Edit {
      Edit { point, payload }
    }
  }
  pub fn new_transform_type() -> impl Strategy<Value=TransformType> {
    prop_oneof![new_edit().prop_map(TransformType::edit),]
  }
  prop_compose! {
    pub fn new_transform()(r#type in new_transform_type()) -> Transform {
      Transform { r#type }
    }
  }
}

mod serde_impl {
  #[cfg(test)]
  use super::proptest_strategies::*;
  use super::*;

  use serde_mux;

  use std::convert::{TryFrom, TryInto};

  mod insert {
    use super::*;

    impl serde_mux::Schema for proto::Insert {
      type Source = Insert;
    }

    impl TryFrom<proto::Insert> for Insert {
      type Error = TransformError;

      fn try_from(proto_message: proto::Insert) -> Result<Self, TransformError> {
        let proto::Insert { contents } = proto_message.clone();
        let contents = contents.ok_or_else(|| {
          TransformError::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
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
      type Error = TransformError;

      fn try_from(proto_message: proto::Delete) -> Result<Self, TransformError> {
        let proto::Delete { distance } = proto_message;
        let distance = distance.ok_or_else(|| {
          TransformError::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
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
      type Error = TransformError;

      fn try_from(proto_message: proto::Point) -> Result<Self, TransformError> {
        let proto::Point { code_point_index } = proto_message;
        let code_point_index = code_point_index.ok_or_else(|| {
          TransformError::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
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
      type Error = TransformError;

      fn try_from(proto_message: proto::Edit) -> Result<Self, TransformError> {
        let proto::Edit { point, payload } = proto_message.clone();
        let point: Point = point
          .ok_or_else(|| {
            TransformError::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
              "point",
              format!("{:?}", proto_message),
            ))
          })?
          .try_into()?;
        let payload = payload.ok_or_else(|| {
          TransformError::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
            "payload",
            format!("{:?}", proto_message),
          ))
        })?;
        let payload = match payload {
          proto::edit::Payload::Insert(insert) => EditPayload::insert(insert.try_into()?),
          proto::edit::Payload::Delete(delete) => EditPayload::delete(delete.try_into()?),
        };
        Ok(Self { point, payload })
      }
    }

    impl From<Edit> for proto::Edit {
      fn from(value: Edit) -> Self {
        let Edit { point, payload } = value;
        let payload = match payload {
          EditPayload::insert(insert) => proto::edit::Payload::Insert(insert.into()),
          EditPayload::delete(delete) => proto::edit::Payload::Delete(delete.into()),
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

  mod region {
    use super::*;

    impl serde_mux::Schema for proto::Region {
      type Source = Region;
    }

    impl TryFrom<proto::Region> for Region {
      type Error = TransformError;

      fn try_from(proto_message: proto::Region) -> Result<Self, TransformError> {
        let proto::Region { start, end } = proto_message.clone();
        let start: Point = start
          .ok_or_else(|| {
            TransformError::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
              "start",
              format!("{:?}", proto_message),
            ))
          })?
          .try_into()?;
        let end: Point = end
          .ok_or_else(|| {
            TransformError::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
              "end",
              format!("{:?}", proto_message),
            ))
          })?
          .try_into()?;
        Ok(Self { start, end })
      }
    }

    impl From<Region> for proto::Region {
      fn from(value: Region) -> Self {
        let Region { start, end } = value;
        Self {
          start: Some(start.into()),
          end: Some(end.into()),
        }
      }
    }
  }

  mod sync_checksum {
    use super::*;

    impl serde_mux::Schema for proto::SyncChecksum {
      type Source = SyncChecksum;
    }

    impl TryFrom<proto::SyncChecksum> for SyncChecksum {
      type Error = TransformError;

      fn try_from(proto_message: proto::SyncChecksum) -> Result<Self, TransformError> {
        let proto::SyncChecksum { checksum } = proto_message.clone();
        let checksum = checksum
          .ok_or_else(|| {
            TransformError::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
              "checksum",
              format!("{:?}", proto_message),
            ))
          })?
          .try_into()
          .map_err(|e| {
            serde_mux::ProtobufCodingFailure::SliceLength(CHECKSUM_LENGTH, format!("{:?}", e))
          })?;
        Ok(Self { checksum })
      }
    }

    impl From<SyncChecksum> for proto::SyncChecksum {
      fn from(value: SyncChecksum) -> Self {
        let SyncChecksum { checksum } = value;
        Self {
          checksum: Some(checksum.into()),
        }
      }
    }
  }

  mod sync_contents {
    use super::*;

    impl serde_mux::Schema for proto::SyncContents {
      type Source = SyncContents;
    }

    impl TryFrom<proto::SyncContents> for SyncContents {
      type Error = TransformError;

      fn try_from(proto_message: proto::SyncContents) -> Result<Self, TransformError> {
        let proto::SyncContents { contents } = proto_message.clone();
        let contents = contents.ok_or_else(|| {
          TransformError::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
            "contents",
            format!("{:?}", proto_message),
          ))
        })?;
        Ok(Self { contents })
      }
    }

    impl From<SyncContents> for proto::SyncContents {
      fn from(value: SyncContents) -> Self {
        let SyncContents { contents } = value;
        Self {
          contents: Some(contents),
        }
      }
    }
  }

  mod sync_region {
    use super::*;

    impl serde_mux::Schema for proto::SyncRegion {
      type Source = SyncRegion;
    }

    impl TryFrom<proto::SyncRegion> for SyncRegion {
      type Error = TransformError;

      fn try_from(proto_message: proto::SyncRegion) -> Result<Self, TransformError> {
        let proto::SyncRegion { region, r#type } = proto_message.clone();
        let region = region
          .ok_or_else(|| {
            TransformError::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
              "region",
              format!("{:?}", proto_message),
            ))
          })?
          .try_into()?;
        let r#type = r#type.ok_or_else(|| {
          TransformError::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
            "type",
            format!("{:?}", proto_message),
          ))
        })?;
        let r#type = match r#type {
          proto::sync_region::Type::Checksum(sync_checksum) => {
            SyncRegionType::checksum(sync_checksum.try_into()?)
          },
          proto::sync_region::Type::Contents(sync_contents) => {
            SyncRegionType::contents(sync_contents.try_into()?)
          },
        };
        Ok(Self { region, r#type })
      }
    }

    impl From<SyncRegion> for proto::SyncRegion {
      fn from(value: SyncRegion) -> Self {
        let SyncRegion { region, r#type } = value;
        let r#type = match r#type {
          SyncRegionType::checksum(sync_checksum) => {
            proto::sync_region::Type::Checksum(sync_checksum.into())
          },
          SyncRegionType::contents(sync_contents) => {
            proto::sync_region::Type::Contents(sync_contents.into())
          },
        };
        Self {
          region: Some(region.into()),
          r#type: Some(r#type),
        }
      }
    }
  }

  mod transform {
    use super::*;

    impl serde_mux::Schema for proto::Transform {
      type Source = Transform;
    }

    impl TryFrom<proto::Transform> for Transform {
      type Error = TransformError;

      fn try_from(proto_message: proto::Transform) -> Result<Self, TransformError> {
        let proto::Transform { r#type } = proto_message.clone();
        let r#type = r#type.ok_or_else(|| {
          TransformError::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
            "type",
            format!("{:?}", proto_message),
          ))
        })?;
        let r#type = match r#type {
          proto::transform::Type::Edit(edit) => TransformType::edit(edit.try_into()?),
          proto::transform::Type::Sync(sync) => TransformType::sync(sync.try_into()?),
        };
        Ok(Self { r#type })
      }
    }

    impl From<Transform> for proto::Transform {
      fn from(value: Transform) -> Self {
        let Transform { r#type } = value;
        let r#type = match r#type {
          TransformType::edit(edit) => proto::transform::Type::Edit(edit.into()),
          TransformType::sync(sync) => proto::transform::Type::Sync(sync.into()),
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
        fn test_serde_transform(transform in new_transform()) {
          let protobuf = serde_mux::Protobuf::<Transform, proto::Transform>::new(transform.clone());
          let buf: Box<[u8]> = protobuf.serialize();
          let resurrected =
            serde_mux::Protobuf::<Transform, proto::Transform>::deserialize(&buf).unwrap();
          prop_assert_eq!(transform, resurrected);
        }
      }
    }
  }
}
