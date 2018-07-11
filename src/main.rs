#![feature(box_syntax, box_patterns)]
/*
 * Copyright (c) 2018 Lasse Dissing
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

extern crate llvm_sys as llvm;

use std::fs::File;
use std::io::Write;
use std::io::prelude::*;

mod lexer;
mod tokens;
mod ast;
mod ast_visualization;
mod parser;
mod codegen;

fn main() {
    let mut file = File::open("examples/factorial.par").unwrap();
    let mut source = String::new();
    let _ = file.read_to_string(&mut source);

    let tokens = lexer::lex(source);
    let items = parser::parse(tokens);

    /*for item in &items {
        println!("{:?}", item);
    }*/


    ast_visualization::dump_parse_tree(items[0].clone(), "parse_tree.gv");

    let llir = unsafe {
        codegen::generate(items)
    };

    let mut output_file = File::create("main.ll").unwrap();
    let _ = output_file.write(llir.as_bytes());
}
