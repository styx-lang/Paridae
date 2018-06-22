/*
 * Copyright (c) 2018 Lasse Dissing
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use tokens::*;
use ast::*;

struct ParsingContext {
    current: usize,
    tokens: Vec<Token>,
    ast: Vec<Decl>
}

fn accept(ctx: &mut ParsingContext, token: TokenType) -> bool {
    if ctx.tokens[ctx.current].token_type == token {
        ctx.current += 1;
        true
    } else {
        false
    }
}

fn expect(ctx: &mut ParsingContext, token: TokenType) -> bool {
    if accept(ctx, token.clone()) {
        true
    } else {
        panic!("Expected {:?} but got {:?}", token, ctx.tokens[ctx.current]);
    }
}


pub fn parse(tokens: Vec<Token>) -> Vec<Decl> {
    let mut ctx = ParsingContext {current: 0, tokens, ast: Vec::new() };
    ctx.ast
}