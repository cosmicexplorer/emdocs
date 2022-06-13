/*
 * Description: The JSON interface to the emdocs p2p client.
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

//! The JSON interface to the emdocs p2p client.

use emdocs_protocol::{buffers::BufferId, messages};

pub mod connections {
  use super::*;

  use futures::future;
  use indexmap::{IndexMap, IndexSet};
  use parking_lot::RwLock;
  use reqwest;
  use serde::ser::Serialize;

  use std::sync::Arc;

  #[derive(Debug, Clone)]
  pub struct Client {
    target: messages::RemoteClient,
    client: reqwest::Client,
  }

  impl Client {
    pub fn new(target: messages::RemoteClient, client: reqwest::Client) -> Self {
      Self { target, client }
    }

    pub async fn send<T: Serialize>(&self, x: T) -> Result<(), reqwest::Error> {
      self
        .client
        .get(&self.target.ip_address)
        .json(&x)
        .send()
        .await?;
      Ok(())
    }
  }

  #[derive(Debug, Clone)]
  pub struct BufferTopic {
    pub clients: Arc<RwLock<IndexSet<messages::RemoteClient>>>,
  }

  impl BufferTopic {
    pub fn new() -> Self {
      Self {
        clients: Arc::new(RwLock::new(IndexSet::new())),
      }
    }

    pub fn add(&self, remote: messages::RemoteClient) { self.clients.write().insert(remote); }
  }

  #[derive(Debug, Clone)]
  pub struct Connections {
    associations: Arc<RwLock<IndexMap<BufferId, BufferTopic>>>,
    remote_clients: Arc<RwLock<IndexMap<messages::RemoteClient, Client>>>,
  }

  impl Connections {
    pub fn new() -> Self {
      Self {
        associations: Arc::new(RwLock::new(IndexMap::new())),
        remote_clients: Arc::new(RwLock::new(IndexMap::new())),
      }
    }

    pub fn topic_for_buffer(&self, buffer_id: BufferId) -> BufferTopic {
      self
        .associations
        .write()
        .entry(buffer_id)
        .or_insert_with(BufferTopic::new)
        .clone()
    }

    pub fn record_buffer_client(&self, association: messages::BufferAssociation) {
      let messages::BufferAssociation { buffer_id, remote } = association;
      self.topic_for_buffer(buffer_id).add(remote);
    }

    pub fn get_client(&self, remote: messages::RemoteClient) -> Client {
      self
        .remote_clients
        .write()
        .entry(remote.clone())
        .or_insert_with(|| Client::new(remote, reqwest::Client::new()))
        .clone()
    }

    pub async fn broadcast<T: Serialize+Clone>(
      &self,
      buffer_id: BufferId,
      x: T,
    ) -> Result<(), reqwest::Error> {
      let clients: Vec<_> = self
        .topic_for_buffer(buffer_id)
        .clients
        .read()
        .iter()
        .cloned()
        .collect();
      future::join_all(clients.into_iter().map(|remote| async {
        let client = self.get_client(remote);
        client.send(x.clone()).await?;
        Ok(())
      }))
      .await
      .into_iter()
      .collect::<Result<Vec<()>, reqwest::Error>>()?;
      Ok(())
    }
  }
}
