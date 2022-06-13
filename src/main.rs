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

use clap::{Parser, Subcommand};
use serde_json;

use std::io::{self, Write};

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
fn main() {
  let Opts { action } = Opts::parse();
  match action {
    Action::Serve => {
      let mut buf = String::new();
      while io::stdin()
        .read_line(&mut buf)
        .expect("io read should succeed")
        != 0
      {
        let ide_msg: json_client::IDEMessage =
          serde_json::from_str(&buf).expect("IDE message decoding failed");
        dbg!(&ide_msg);
        let client_msg: Vec<u8> = serde_json::to_vec(&json_client::ClientMessage::ok)
          .expect("client message encoding failed");
        io::stdout()
          .write(&client_msg)
          .expect("io write should succeed");
      }
    },
  }
}
