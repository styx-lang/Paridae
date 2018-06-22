/*
 * Copyright (c) 2018 Lasse Dissing
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

mod lexer;
mod tokens;

fn main() {
    let source = "hello := \"world\";".to_string();

    let tokens = lexer::lex(source);
    println!("{:?}", tokens);
}
