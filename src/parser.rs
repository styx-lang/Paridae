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
}

fn is_done(ctx: &mut ParsingContext) -> bool {
    ctx.current == ctx.tokens.len()
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

fn look_ahead(ctx: &mut ParsingContext, offset: usize) -> Token {
    if ctx.current == ctx.tokens.len() { Token { token_type: TokenType::EOF, lexeme: None, line: 0} }
    else { ctx.tokens[ctx.current + offset].clone() }
}

fn consume(ctx: &mut ParsingContext) -> Token {
    ctx.current += 1;
    ctx.tokens[ctx.current-1].clone()
}

fn get_precedence(operator: BinaryOperatorKind) -> u32 {
    use ast::BinaryOperatorKind::*;

    match operator {
        Product => 5,
        Division => 5,
        Addition => 4,
        Subtraction => 4,
        Less => 3,
        LessEq => 3,
        Greater => 3,
        GreaterEq => 3,
        Equality => 3,
        NotEq => 3,
    }
}

fn parse_integer_literal(ctx: &mut ParsingContext, token: Token) -> Expr {
    let n = token.lexeme.unwrap().parse::<i64>().unwrap();
    Expr {node: ExprKind::Literal(Box::new(LitKind::Int(n))), t: None }
}

fn parse_identifier(ctx: &mut ParsingContext, token: Token) -> Expr {
    Expr{ node: ExprKind::Identifier(token.lexeme.unwrap()), t: None }
}

fn parse_prefix_operator(ctx: &mut ParsingContext, token: Token) -> Expr {
    use tokens::TokenType::*;

    let operation = match token.token_type {
        Minus => UnaryOperatorKind::Negation,
        Bang => UnaryOperatorKind::Complement,
        _ => panic!("{:?} is not a valid prefix operator!", token.token_type),
    };

    let operand = parse_expression(ctx, 6);
    Expr{ node: ExprKind::Unary(operation, Box::new(operand)), t: None }
}

fn convert_token_to_binary_operator(token: TokenType) -> Option<BinaryOperatorKind> {
    use tokens::TokenType::*;

    match token {
        Plus => Some(BinaryOperatorKind::Addition),
        Minus => Some(BinaryOperatorKind::Subtraction),
        Star => Some(BinaryOperatorKind::Product),
        Slash => Some(BinaryOperatorKind::Division),
        Less => Some(BinaryOperatorKind::Less),
        LessEqual => Some(BinaryOperatorKind::LessEq),
        Greater => Some(BinaryOperatorKind::Greater),
        GreaterEqual => Some(BinaryOperatorKind::GreaterEq),
        EqualEqual => Some(BinaryOperatorKind::Equality),
        BangEqual => Some(BinaryOperatorKind::NotEq),
        _ => None,
    }
}

fn parse_binary_operator(ctx: &mut ParsingContext, left: Expr, token: Token) -> Expr {

    let operation = convert_token_to_binary_operator(token.token_type).expect("Given token must be a binary operator");

    let precedence = get_precedence(operation);
    let right = parse_expression(ctx, precedence);

    Expr{ node: ExprKind::Binary(operation, Box::new(left), Box::new(right)), t: None }
}

fn get_current_precedence(ctx: &mut ParsingContext) -> u32 {
    if ctx.tokens.len() <= ctx.current {
       0
    } else if let Some(op) = convert_token_to_binary_operator(ctx.tokens[ctx.current].token_type) {
        get_precedence(op)
    } else {
        0
    }
}


fn parse_expression(ctx: &mut ParsingContext, precedence: u32) -> Expr {
    use tokens::TokenType::*;

    let token = consume(ctx);

    let mut left = match token.token_type {
        Identifier => parse_identifier(ctx, token.clone()),
        Integer => parse_integer_literal(ctx, token.clone()),
        Minus | Bang => parse_prefix_operator(ctx, token.clone()),
        _ => panic!("{:?} is not a valid expression prefix", token.token_type),
    };

    while precedence < get_current_precedence(ctx) {
        let token = consume(ctx);

        left = parse_binary_operator(ctx, left, token);
    }

    left

}

pub fn parse(tokens: Vec<Token>) -> Expr {
    let mut ctx = ParsingContext {current: 0, tokens};
    let expr = parse_expression(&mut ctx, 0);
    expr
}