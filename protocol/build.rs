/*
 * Description: Build script for protocol buffers.
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

fn main() {
  let protos = [
    "src/buffers.proto",
    "src/transforms.proto",
    "src/messages.proto",
    "src/p2p.proto",
  ];
  for proto in &protos {
    println!("cargo:rerun-if-changed={}", proto);
    tonic_build::compile_protos(&proto).expect("protobufs were somehow invalid for tonic?");
  }
}
