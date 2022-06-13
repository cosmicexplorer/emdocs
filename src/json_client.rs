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

use emdocs_protocol::{buffers::BufferId, messages::Message};

use serde::{Deserialize, Serialize};

/// The JSON interface from IDEs to this executable.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
#[allow(non_camel_case_types)]
pub enum IDEMessage {
  doc(Message),
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
