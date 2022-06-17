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
  messages::{self, OperationService},
  p2p::{self, P2p},
};

use clap::{Parser, Subcommand};
use serde_json;

use std::{
  io::{self, BufRead, Write},
  thread, time,
};

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
  /// Listen for HTTP connections at the remote port and propagate p2p messages.
  Serve,
  /// Communicate via lines of JSON over stdio.
  Interact,
}

/* echo '{"link": {"buffer_id": {"uuid": "bfbb5e5f7e474018a42b0664e6fdb6c9"}, "remote": {"ip_address": "https://0.0.0.0:3600"}}}\n{"op": {"source": {"uuid":"bfbb5e5f7e474018a42b0664e6fdb6c9"}, "transform": {"type": {"edit": {"point": {"code_point_index": 0}, "payload": {"insert": {"contents": "aaa"}}}}}}}' | cargo run -- interact | jq | sponge */
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
  let Opts { action, port } = Opts::parse();

  match action {
    Action::Serve => {
      let addr = format!("[::1]:{}", port).parse()?;
      tonic::transport::Server::builder()
        .add_service(p2p::proto::p2p_server::P2pServer::new(
          p2p::P2pService::new(),
        ))
        .serve(addr)
        .await?;
    },
    Action::Interact => {
      let p2p_client = p2p::P2pClient::connect(format!("http://[::1]:{}", port)).await?;

      struct OS;
      #[tonic::async_trait]
      impl messages::OperationService for OS {
        async fn process_operation(
          &self,
          request: messages::Operation,
        ) -> Result<messages::OperationResult, messages::ProtocolError> {
          dbg!(&request);
          let client_msg = messages::ClientMessage::op(request);
          let mut client_msg: Vec<u8> = serde_json::to_vec(&client_msg)?;
          /* Ensure we have clear lines between entries in stdout. */
          client_msg.push(b'\n');
          io::stdout().write(&client_msg)?;
          Ok(messages::OperationResult::ok)
        }
      }

      /* Write every operation *received from p2p* to stdout as a JSON encoded ClientMessage. */
      let p2p2 = p2p_client.clone();
      /* Process p2p events in a background thread. */
      tokio::spawn(async move {
        while let Ok(p2p::P2pReceiveResult { messages }) = p2p2.receive(p2p2.receive_params()).await
        {
          for p2p::P2pMessage {
            op,
            msg_id,
            user_id,
          } in messages.into_iter()
          {
            dbg!(msg_id);
            dbg!(user_id);
            assert_eq!(
              messages::OperationResult::ok,
              OS.process_operation(op)
                .await
                .expect("processing op failed")
            );
          }
        }
      });

      /* Hook up stdin to an instance of an IDEService by JSON decoding lines. */
      for line in io::stdin().lock().lines() {
        let ide_msg: messages::IDEMessage = serde_json::from_str(&line?)?;
        let client_msg = match ide_msg {
          messages::IDEMessage::op(op) => {
            dbg!(&op);
            /* Propagate the message to the rest of the swarm. */
            assert_eq!(
              p2p::P2pSendResult {},
              p2p_client
                .propagate(p2p::P2pMessage {
                  msg_id: p2p::P2pMessageId::default(),
                  user_id: p2p_client.receive_params().user_id,
                  op: op.clone(),
                })
                .await?,
            );
            messages::ClientMessage::ok
          },
          messages::IDEMessage::link(link) => {
            eprintln!("do nothing with link {:?}", link);
            messages::ClientMessage::ok
          },
        };
        assert_eq!(messages::ClientMessage::ok, client_msg);
      }

      thread::sleep(time::Duration::from_millis(2000));
    },
  }

  Ok(())
}
