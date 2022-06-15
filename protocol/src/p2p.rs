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
  pub use crate::messages::proto as messages;
  #[doc(inline)]
  pub use proto::*;
  mod proto {
    #![allow(missing_docs)]
    tonic::include_proto!("emdocs.proto.p2p");
  }
}

use crate::messages;

use async_lock::{Mutex, RwLock};
use bloomfilter::Bloom;
use displaydoc::Display;
use serde::{Deserialize, Serialize};
use thiserror::Error;
use uuid::Uuid;

use std::sync::Arc;

#[derive(Debug, Display, Error)]
pub enum P2pError {
  /// protobuf error {0}
  Proto(#[from] serde_mux::ProtobufCodingFailure),
  /// protocol error {0}
  Protocol(#[from] messages::ProtocolError),
  /// tonic error: {0}
  Tonic(#[from] tonic::Status),
}

impl From<prost::DecodeError> for P2pError {
  fn from(value: prost::DecodeError) -> Self { Self::Proto(value.into()) }
}

impl From<prost::EncodeError> for P2pError {
  fn from(value: prost::EncodeError) -> Self { Self::Proto(value.into()) }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct P2pMessageId {
  pub uuid: Uuid,
}

impl Default for P2pMessageId {
  fn default() -> Self {
    Self {
      uuid: Uuid::new_v4(),
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct P2pMessage {
  pub id: P2pMessageId,
  pub op: messages::Operation,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct P2pSendResult {}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct P2pReceiveParams {}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct P2pReceiveResult {
  pub messages: Vec<P2pMessage>,
}

#[tonic::async_trait]
pub trait P2p {
  async fn propagate(&self, msg: P2pMessage) -> Result<P2pSendResult, P2pError>;
  async fn receive(&self, params: P2pReceiveParams) -> Result<P2pReceiveResult, P2pError>;
}

#[derive(Clone)]
pub struct P2pService {
  seen: Arc<RwLock<Bloom<P2pMessageId>>>,
}

impl P2pService {
  fn from_bloom(bloom: Bloom<P2pMessageId>) -> Self {
    Self {
      seen: Arc::new(RwLock::new(bloom)),
    }
  }

  pub fn new() -> Self {
    /* FIXME: make these less arbitrary (along with the use of a bloom filter in itself)! */
    let estimated_max_items_count = 10000;
    let estimated_false_positive_rate = 0.001;
    let bloom = Bloom::<P2pMessageId>::new_for_fp_rate(
      estimated_max_items_count,
      estimated_false_positive_rate,
    );
    Self::from_bloom(bloom)
  }
}

#[tonic::async_trait]
impl proto::p2p_server::P2p for P2pService {
  async fn propagate(
    &self,
    request: tonic::Request<proto::P2pMessage>,
  ) -> Result<tonic::Response<proto::P2pSendResult>, tonic::Status> {
    dbg!(request.metadata());
    let request: P2pMessage = request
      .into_inner()
      .try_into()
      .expect("failed to convert p2p proto request");
    dbg!(&request);
    /* If we've seen the id before, don't propagate it! */
    if self.seen.write().await.check_and_set(&request.id) {
      let response = P2pSendResult {};
      let response: proto::P2pSendResult = response.into();
      Ok(tonic::Response::new(response))
    } else {
      todo!("idk");
    }
  }

  async fn receive(
    &self,
    request: tonic::Request<proto::P2pReceiveParams>,
  ) -> Result<tonic::Response<proto::P2pReceiveResult>, tonic::Status> {
    let request: P2pReceiveParams = request
      .into_inner()
      .try_into()
      .expect("failed to convert p2p receive params proto request");
    dbg!(&request);
    todo!("idk2")
    /* let response: P2pMessage = todo!("idk2"); */
    /* let response: proto::P2pMessage = response.into(); */
    /* Ok(tonic::Response::new(response)) */
  }
}

#[derive(Clone)]
pub struct P2pClient {
  client: Arc<Mutex<proto::p2p_client::P2pClient<tonic::transport::Channel>>>,
}

impl P2pClient {
  fn from_client(client: proto::p2p_client::P2pClient<tonic::transport::Channel>) -> Self {
    Self {
      client: Arc::new(Mutex::new(client)),
    }
  }

  pub async fn connect(address: String) -> Result<Self, tonic::transport::Error> {
    let client = proto::p2p_client::P2pClient::connect(address).await?;
    Ok(Self::from_client(client))
  }
}

#[tonic::async_trait]
impl P2p for P2pClient {
  async fn propagate(&self, msg: P2pMessage) -> Result<P2pSendResult, P2pError> {
    dbg!(&msg);
    let msg: proto::P2pMessage = msg.into();
    let mut request = tonic::Request::new(msg);
    let metadata = request.metadata_mut();
    let result: P2pSendResult = self
      .client
      .lock()
      .await
      /* TODO: Annoying that this is generated as an constrained impl method over the generic client
       * type, not related to the generated server trait (makes it hard to use in generic code). */
      .propagate(request)
      .await?
      .into_inner()
      .try_into()?;
    Ok(result)
  }

  async fn receive(&self, params: P2pReceiveParams) -> Result<P2pReceiveResult, P2pError> {
    dbg!(&params);
    let params: proto::P2pReceiveParams = params.into();
    let result: P2pReceiveResult = self
      .client
      .lock()
      .await
      .receive(params)
      .await?
      .into_inner()
      .try_into()?;
    Ok(result)
  }
}

#[cfg(test)]
pub mod proptest_strategies {
  use super::*;
  use crate::{messages::proptest_strategies::*, proptest_strategies::*};

  use proptest::prelude::*;

  prop_compose! {
    pub fn new_p2p_id()(uuid in new_uuid()) -> P2pMessageId {
      P2pMessageId { uuid }
    }
  }
  prop_compose! {
    pub fn new_p2p_message()(id in new_p2p_id(), op in new_operation()) -> P2pMessage {
      P2pMessage { id, op }
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
      type Source = P2pMessageId;
    }

    impl TryFrom<proto::P2pMessageId> for P2pMessageId {
      type Error = P2pError;

      fn try_from(proto_message: proto::P2pMessageId) -> Result<Self, P2pError> {
        let proto::P2pMessageId { uuid } = proto_message.clone();
        let uuid: [u8; 16] = uuid
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

    impl From<P2pMessageId> for proto::P2pMessageId {
      fn from(value: P2pMessageId) -> Self {
        let P2pMessageId { uuid } = value;
        proto::P2pMessageId {
          uuid: Some(uuid.as_bytes().to_vec()),
        }
      }
    }


    impl Serialize for P2pMessageId {
      fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
      where S: Serializer {
        let mut buffer_id = serializer.serialize_struct("P2pMessageId", 1)?;
        buffer_id.serialize_field("uuid", &self.uuid.as_bytes())?;
        buffer_id.end()
      }
    }

    struct P2pMessageIDVisitor;

    impl<'de> Visitor<'de> for P2pMessageIDVisitor {
      type Value = P2pMessageId;

      fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "A p2p message id")
      }

      fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
      where A: MapAccess<'de> {
        let (k, v): (String, Vec<u8>) = map.next_entry()?.expect("uuid key must exist");
        assert_eq!(k, "uuid");
        Ok(P2pMessageId {
          uuid: Uuid::from_bytes(v.try_into().expect("uuid bytes wrong length")),
        })
      }
    }

    impl<'de> Deserialize<'de> for P2pMessageId {
      fn deserialize<D>(deserializer: D) -> Result<P2pMessageId, D::Error>
      where D: Deserializer<'de> {
        deserializer.deserialize_struct("P2pMessageId", &["uuid"], P2pMessageIDVisitor)
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
            serde_mux::Protobuf::<P2pMessageId, proto::P2pMessageId>::new(p2p_id.clone());
          let buf: Box<[u8]> = protobuf.serialize();
          let resurrected =
            serde_mux::Protobuf::<P2pMessageId, proto::P2pMessageId>::deserialize(&buf).unwrap();
          prop_assert_eq!(p2p_id, resurrected);
        }
      }
    }
  }

  mod p2p_message {
    use super::*;

    impl serde_mux::Schema for proto::P2pMessage {
      type Source = P2pMessage;
    }

    impl TryFrom<proto::P2pMessage> for P2pMessage {
      type Error = P2pError;

      fn try_from(proto_message: proto::P2pMessage) -> Result<Self, P2pError> {
        let proto::P2pMessage { id, op } = proto_message.clone();
        let id: P2pMessageId = id
          .ok_or_else(|| {
            P2pError::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
              "id",
              format!("{:?}", proto_message),
            ))
          })?
          .try_into()?;
        let op: messages::Operation = op
          .ok_or_else(|| {
            P2pError::Proto(serde_mux::ProtobufCodingFailure::OptionalFieldAbsent(
              "op",
              format!("{:?}", proto_message),
            ))
          })?
          .try_into()?;
        Ok(Self { id, op })
      }
    }

    impl From<P2pMessage> for proto::P2pMessage {
      fn from(value: P2pMessage) -> Self {
        let P2pMessage { id, op } = value;
        Self {
          id: Some(id.into()),
          op: Some(op.into()),
        }
      }
    }
  }

  mod p2p_send_result {
    use super::*;

    impl serde_mux::Schema for proto::P2pSendResult {
      type Source = P2pSendResult;
    }

    impl TryFrom<proto::P2pSendResult> for P2pSendResult {
      type Error = P2pError;

      fn try_from(proto_message: proto::P2pSendResult) -> Result<Self, P2pError> { Ok(Self {}) }
    }

    impl From<P2pSendResult> for proto::P2pSendResult {
      fn from(value: P2pSendResult) -> Self { Self {} }
    }
  }

  mod p2p_receive_params {
    use super::*;

    impl serde_mux::Schema for proto::P2pReceiveParams {
      type Source = P2pReceiveParams;
    }

    impl TryFrom<proto::P2pReceiveParams> for P2pReceiveParams {
      type Error = P2pError;

      fn try_from(proto_message: proto::P2pReceiveParams) -> Result<Self, P2pError> { Ok(Self {}) }
    }

    impl From<P2pReceiveParams> for proto::P2pReceiveParams {
      fn from(value: P2pReceiveParams) -> Self { Self {} }
    }
  }

  mod p2p_receive_result {
    use super::*;

    impl serde_mux::Schema for proto::P2pReceiveResult {
      type Source = P2pReceiveResult;
    }

    impl TryFrom<proto::P2pReceiveResult> for P2pReceiveResult {
      type Error = P2pError;

      fn try_from(proto_message: proto::P2pReceiveResult) -> Result<Self, P2pError> {
        let proto::P2pReceiveResult { messages } = proto_message.clone();
        let messages: Vec<P2pMessage> = messages
          .into_iter()
          .map(|x| x.try_into())
          .collect::<Result<Vec<_>, _>>()?;
        Ok(Self { messages })
      }
    }

    impl From<P2pReceiveResult> for proto::P2pReceiveResult {
      fn from(value: P2pReceiveResult) -> Self {
        let P2pReceiveResult { messages } = value;
        let messages: Vec<proto::P2pMessage> = messages.into_iter().map(|x| x.into()).collect();
        Self { messages }
      }
    }
  }
}
