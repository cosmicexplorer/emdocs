/*
 * Description: A client for the emdocs protocol.
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

//! A client for the emdocs protocol.

#![deny(rustdoc::missing_crate_level_docs)]
/* Make all doctests fail if they produce any warnings. */
#![doc(test(attr(deny(warnings))))]
#![deny(clippy::all)]

mod json_client;
use json_client::{connections, protocol};

use emdocs_protocol::messages::{Message, Transform};

use clap::{Parser, Subcommand};
use serde_json;

use std::io::{self, BufRead, Write};

#[derive(Debug, Parser)]
#[clap(author, version, about, long_about = None)]
struct Opts {
  #[clap(subcommand)]
  action: Action,
}

#[derive(Debug, Subcommand)]
enum Action {
  Serve,
}

/* echo '{"doc": {"transform": {"source": {"uuid":[34,246,198,16,207,151,73,193,141,135,206,60,34,174,195,229]}, "type": {"edit": {"point": {"code_point_index": 0}, "payload": {"insert": {"contents": "aaa"}}}}}}}' | cargo run -- serve | jq */
/* echo '{"link": {"buffer_id": {"uuid":[34,246,198,16,207,151,73,193,141,135,206,60,34,174,195,229]}, "remote": {"ip_address": "asdf"}}}' | cargo run -- serve | jq */
/* -- */
/* echo '{"link": {"buffer_id": {"uuid":[34,246,198,16,207,151,73,193,141,135,206,60,34,174,195,229]}, "remote": {"ip_address": "https://0.0.0.0:3600"}}}\n{"doc": {"transform": {"source": {"uuid":[34,246,198,16,207,151,73,193,141,135,206,60,34,174,195,229]}, "type": {"edit": {"point": {"code_point_index": 0}, "payload": {"insert": {"contents": "aaa"}}}}}}}' | cargo run -- serve */
#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
  let Opts { action } = Opts::parse();
  let connections = connections::Connections::new();
  match action {
    Action::Serve => {
      for line in io::stdin().lock().lines() {
        let line = line.expect("io error reading stdin line");
        let ide_msg: protocol::IDEMessage =
          serde_json::from_str(&line).expect("IDE message decoding failed");
        dbg!(&ide_msg);
        match ide_msg {
          protocol::IDEMessage::link(association) => {
            connections.record_buffer_client(association);
          },
          protocol::IDEMessage::doc(msg) => match msg {
            Message::transform(Transform { source, r#type }) => {
              connections.broadcast(source, &r#type).await?;
            },
          },
        }
        let client_msg: Vec<u8> =
          serde_json::to_vec(&protocol::ClientMessage::ok).expect("client message encoding failed");
        io::stdout()
          .write(&client_msg)
          .expect("io write should succeed");
      }
    },
  }
  Ok(())
}
