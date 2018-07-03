#![feature(box_syntax, box_patterns)]
/*
 * Copyright (c) 2018 Lasse Dissing
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use std::fs::File;
use std::io::prelude::*;

mod lexer;
mod tokens;
mod ast;
mod parser;

fn main() {
    let mut file = File::open("examples/floating_arithmetic.par").unwrap();
    let mut source = String::new();
    let _ = file.read_to_string(&mut source);

    let tokens = lexer::lex(source);
    for token in tokens.clone() {
        println!("{:?}", token);
    }
    let items = parser::parse(tokens);

    println!("{:?}", items);

    //ast::dump_parse_tree(expr, "parse_tree.gv");;
}
