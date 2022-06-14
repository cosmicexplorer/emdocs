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

use emdocs_protocol::messages::{self, IDEService};

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
  /// Communicate via lines of JSON over stdio.
  Serve {
    /* TODO: what should this default port be? */
    #[clap(short, long, default_value_t = 37263)]
    port: usize,
  },
}

/* echo '{"link": {"buffer_id": {"uuid":[34,246,198,16,207,151,73,193,141,135,206,60,34,174,195,229]}, "remote": {"ip_address": "https://0.0.0.0:3600"}}}\n{"op": {"source": {"uuid":[34,246,198,16,207,151,73,193,141,135,206,60,34,174,195,229]}, "transform": {"type": {"edit": {"point": {"code_point_index": 0}, "payload": {"insert": {"contents": "aaa"}}}}}}}' | cargo run -- serve */
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
  let Opts { action } = Opts::parse();

  match action {
    Action::Serve { port } => {
      /* let addr = format!("[::1]:{}", port).parse()?; */
      /* let operation_service = */
      /*   messages::OperationServiceClient::connect(format!("http://[::1]:{}", port)).await?; */
      struct OS;
      #[tonic::async_trait]
      impl messages::OperationService for OS {
        async fn process_operation(
          &self,
          request: messages::Operation,
        ) -> Result<messages::OperationResult, messages::ProtocolError> {
          eprintln!("do nothing with operation {:?}", request);
          Ok(messages::OperationResult::ok)
        }
      }

      let ide_service = messages::IDEServiceClient::from_client(OS);
      /* let op_server = tonic::transport::Server::builder() */
      /*   .add_service( */
      /*     messages::proto::operation_service_server::OperationServiceServer::new( */
      /*       operation_service.clone(), */
      /*     ), */
      /*   ) */
      /*   .serve(addr); */

      /* TODO: The IDE's VFS is an OperationService!!! */

      /* Hook up stdio to an instance of an IDEService by JSON en/decoding lines. */
      for line in io::stdin().lock().lines() {
        let ide_msg: messages::IDEMessage = serde_json::from_str(&line?)?;
        let client_msg = ide_service.process_ide_message(ide_msg).await?;
        let mut client_msg: Vec<u8> = serde_json::to_vec(&client_msg)?;
        /* Ensure we have clear lines between entries in stdout. */
        client_msg.push(b'\n');
        io::stdout().write(&client_msg)?;
      }
    },
  }

  /* op_server.await?; */

  Ok(())
}
