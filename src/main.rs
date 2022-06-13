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

/* #![warn(missing_docs)] */
#![deny(rustdoc::missing_crate_level_docs)]
/* Make all doctests fail if they produce any warnings. */
#![doc(test(attr(deny(warnings))))]
#![deny(clippy::all)]

mod connections;

use emdocs_protocol::buffers::BufferId;

use clap::{Parser, Subcommand};
use serde_json;

use std::io;

#[derive(Debug, Parser)]
#[clap(author, version, about, long_about = None)]
struct Opts {
  #[clap(subcommand)]
  action: Action,
}

#[derive(Debug, Subcommand)]
enum Action {
  NewBuffer,
  ReadBuffer,
}

fn main() {
  let Opts { action } = Opts::parse();
  match action {
    Action::NewBuffer => {
      let buf = BufferId::default();
      let j = serde_json::to_string(&buf).expect("expected json encoding to succeed");
      println!("{}", j);
    },
    Action::ReadBuffer => {
      let buf: BufferId =
        serde_json::from_reader(io::stdin()).expect("expected json decoding to succeed");
      println!("{:?}", buf);
    },
  }
}
