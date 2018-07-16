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
use std::env;
use std::path::Path;

mod lexer;
mod tokens;
mod ast;
mod parser;
mod codegen;
mod type_checking;

fn parse_source_file(filename: &str) -> Vec<ast::Item> {
    let mut file = File::open(filename).unwrap();
    let mut source = String::new();
    let _ = file.read_to_string(&mut source);

    let tokens = lexer::lex(source);
    parser::parse(tokens)
}

fn execute_include_directives(items: Vec<ast::Item>) -> Vec<ast::Item> {

    let mut resulting_items = Vec::new();
    resulting_items.reserve(items.len());

    for item in items {

        if let ast::ItemKind::Directive(ast::DirectiveKind::Include(ref filename)) = item.node {
            let mut additional_nodes = parse_source_file(filename.as_ref());
            resulting_items.append(&mut additional_nodes);
        } else {
            resulting_items.push(item);
        }
    }

    resulting_items
}

fn main() {

    let _ = env::set_current_dir(&Path::new(".."));

    let mut items = parse_source_file("examples/struct.par");

    items = execute_include_directives(items);

    let typed_items = type_checking::check(items);

    let llir = unsafe {
        codegen::generate(typed_items)
    };

    let mut output_file = File::create("main.ll").unwrap();
    let _ = output_file.write(llir.as_bytes());
}
