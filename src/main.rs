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

use emdocs_protocol::{
  buffers,
  messages::{self, IDEService, OperationService},
  p2p::{self, P2p},
  transforms,
};

use clap::{Parser, Subcommand};
use serde_json;

use std::io::{self, BufRead, Write};

#[derive(Debug, Parser)]
#[clap(author, version, about, long_about = None)]
struct Opts {
  #[clap(subcommand)]
  action: Action,

  /// Port to receive p2p connections at.
  /* TODO: what should this default port be? */
  #[clap(short, long, default_value_t = 37263)]
  port: usize,
}

#[derive(Debug, Subcommand)]
enum Action {
  /// Communicate via lines of JSON over stdio.
  Interact,
  /// Listen for HTTP connections at the remote port and propagate p2p messages.
  Serve,
}

/* echo '{"link": {"buffer_id": {"uuid":[34,246,198,16,207,151,73,193,141,135,206,60,34,174,195,229]}, "remote": {"ip_address": "https://0.0.0.0:3600"}}}\n{"op": {"source": {"uuid":[34,246,198,16,207,151,73,193,141,135,206,60,34,174,195,229]}, "transform": {"type": {"edit": {"point": {"code_point_index": 0}, "payload": {"insert": {"contents": "aaa"}}}}}}}' | cargo run -- interact | jq | sponge */
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
  let Opts { action, port } = Opts::parse();

  match action {
    Action::Interact => {
      let p2p_client = p2p::P2pClient::connect(format!("http://[::1]:{}", port)).await?;
      let op_client = messages::OperationServiceClient { p2p_client };

      /* TODO: The IDE's VFS is an OperationService!!! */

      /* Hook up stdio to an instance of an IDEService by JSON encoding lines. */
      for line in io::stdin().lock().lines() {
        let ide_msg: messages::IDEMessage = serde_json::from_str(&line?)?;
        let client_msg = match ide_msg {
          messages::IDEMessage::op(op) => {
            let result = op_client.process_operation(op).await?;
            dbg!(result);
            messages::ClientMessage::ok
          },
          messages::IDEMessage::link(link) => {
            eprintln!("do nothing with link {:?}", link);
            messages::ClientMessage::ok
          },
        };
        let mut client_msg: Vec<u8> = serde_json::to_vec(&client_msg)?;
        /* Ensure we have clear lines between entries in stdout. */
        client_msg.push(b'\n');
        io::stdout().write(&client_msg)?;
      }
    },
    Action::Serve => {
      let addr = format!("[::1]:{}", port).parse()?;
      tonic::transport::Server::builder()
        .add_service(p2p::proto::p2p_server::P2pServer::new(p2p::P2pService))
        .serve(addr)
        .await?;
    },
  }

  Ok(())
}
