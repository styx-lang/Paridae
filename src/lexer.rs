/*
 * Copyright (c) 2018 Lasse Dissing
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use tokens::*;

struct LexingContext {
    start: usize,
    current: usize,
    line: usize,
    tokens: Vec<Token>,
    source: Vec<char>,
}

impl LexingContext {
    fn new(source: String) -> LexingContext {
        LexingContext {
            start: 0,
            current: 0,
            line: 1,
            tokens: Vec::new(),
            source: source.chars().collect(),
        }
    }
}

fn peek_offset(ctx: &mut LexingContext, offset: usize) -> char {
    if is_done(ctx) {
        '\0'
    } else {
        *ctx.source.get(ctx.current + offset).unwrap()
    }
}

fn peek(ctx: &mut LexingContext) -> char {
    peek_offset(ctx, 0)
}

fn advance(ctx: &mut LexingContext) -> char {
    ctx.current += 1;
    *ctx.source.get(ctx.current - 1).unwrap()
}

fn add_simple_token(ctx: &mut LexingContext, t: TokenType) {
    ctx.tokens.push(Token {
        token_type: t,
        lexeme: None,
        line: ctx.line,
    });
}

fn add_lookahead_conditional_token(
    ctx: &mut LexingContext,
    expect: char,
    first: TokenType,
    second: TokenType,
) {
    let t = if peek(ctx) == expect {
        advance(ctx);
        first
    } else {
        second
    };
    ctx.tokens.push(Token {
        token_type: t,
        lexeme: None,
        line: ctx.line,
    });
}

fn get_lexeme(ctx: &mut LexingContext) -> String {
    ctx.source[ctx.start..ctx.current].iter().collect()
}

fn add_lexeme_token(ctx: &mut LexingContext, lexeme: String, token_type: TokenType) {
    ctx.tokens.push(Token {
        token_type,
        lexeme: Some(lexeme),
        line: ctx.line,
    });
}

fn single_line_comment(ctx: &mut LexingContext) {
    while peek(ctx) != '\n' && !is_done(ctx) {
        advance(ctx);
    }
}

fn string(ctx: &mut LexingContext) {
    while peek(ctx) != '"' && !is_done(ctx) {
        if peek(ctx) == '\n' {
            ctx.line += 1;
        }
        advance(ctx);
    }

    if is_done(ctx) {
        panic!("Unterminated string");
    }

    //Consume closing "
    advance(ctx);
    let lexeme = get_lexeme(ctx);
    let string_content = lexeme[1..(lexeme.len()-1)].to_string();
    add_lexeme_token(ctx, string_content, TokenType::String);
}

fn number(ctx: &mut LexingContext) {

    while peek(ctx).is_numeric() {
        advance(ctx);
    }

    let mut dot_encountered = false;
    if peek(ctx) == '.' && peek_offset(ctx, 1).is_numeric() {
        dot_encountered = true;
        advance(ctx);
        while peek(ctx).is_numeric() {
            advance(ctx);
        }
    }

    let lexeme = get_lexeme(ctx);
    let t = if dot_encountered {
        TokenType::Float
    } else {
        TokenType::Integer
    };

    add_lexeme_token(ctx, lexeme, t);
}

fn identifier(ctx: &mut LexingContext) {
    while peek(ctx).is_alphanumeric() || peek(ctx) == '_' {
        advance(ctx);
    }
    let lexeme = get_lexeme(ctx);
    if let Some(t) = is_keyword(lexeme.as_str()) {
        add_simple_token(ctx, t);
    } else {
        add_lexeme_token(ctx, lexeme, TokenType::Identifier);
    }
}

fn scan_token(ctx: &mut LexingContext) {
    use tokens::TokenType::*;

    let c = advance(ctx);

    match c {
        '(' => add_simple_token(ctx, LeftParen),
        ')' => add_simple_token(ctx, RightParen),
        '[' => add_simple_token(ctx, LeftBracket),
        ']' => add_simple_token(ctx, RightBracket),
        '{' => add_simple_token(ctx, LeftCurly),
        '}' => add_simple_token(ctx, RightCurly),
        '+' => add_simple_token(ctx, Plus),
        '-' => add_simple_token(ctx, Minus),
        '*' => add_simple_token(ctx, Star),
        '^' => add_simple_token(ctx, Hat),
        ';' => add_simple_token(ctx, Semicolon),
        '.' => add_simple_token(ctx, Dot),
        ',' => add_simple_token(ctx, Comma),
        ':' => add_lookahead_conditional_token(ctx, ':', ColonColon, Colon),
        '=' => add_lookahead_conditional_token(ctx, '=', EqualEqual, Equal),
        '!' => add_lookahead_conditional_token(ctx, '=', BangEqual, Bang),
        '<' => add_lookahead_conditional_token(ctx, '=', LessEqual, Less),
        '>' => add_lookahead_conditional_token(ctx, '=', GreaterEqual, Greater),
        '&' => add_lookahead_conditional_token(ctx, '&', AndAnd, And),
        '|' => add_lookahead_conditional_token(ctx, '|', OrOr, Or),
        '/' => {
            let next = peek(ctx);
            if next == '/' {
                single_line_comment(ctx);
            } else {
                add_simple_token(ctx, Slash);
            }
        }
        ' ' => {}
        '\t' => {}
        '\n' => ctx.line += 1,
        '"' => string(ctx),
        _ => {
            if c.is_numeric() {
                number(ctx);
            } else if c.is_alphabetic() || c == '_' {
                identifier(ctx);
            } else {
                panic!("Unexpected character {}", c);
            }
        }
    }
}

fn is_done(ctx: &LexingContext) -> bool {
    ctx.current >= ctx.source.len()
}

pub fn lex(source: String) -> Vec<Token> {
    let mut ctx = LexingContext::new(source);

    while !is_done(&ctx) {
        ctx.start = ctx.current;
        scan_token(&mut ctx);
    }
    ctx.tokens
}
