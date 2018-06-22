/*
 * Copyright (c) 2018 Lasse Dissing
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    Identifier,
    //Keywords
    Break,
    Const,
    Continue,
    Defer,
    Else,
    For,
    If,
    Return,
    Struct,
    //Operators and punctuation
    Plus,
    Minus,
    Star,
    Slash,
    And,
    AndAnd,
    Or,
    OrOr,
    Hat,
    Equal,
    Bang,
    BangEqual,
    EqualEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftCurly,
    RightCurly,
    Comma,
    Dot,
    Colon,
    ColonColon,
    Semicolon,
    //Literals
    Integer,
    Float,
    String,
    True,
    False
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: Option<String>,
    pub line: usize,
}


pub fn is_keyword(s: &str) -> Option<TokenType> {
    use self::TokenType::*;

    // Apparently Rust does not yet support static const HashMaps?
    match s {
        "break" => Some(Break),
        "const" => Some(Const),
        "continue" => Some(Continue),
        "defer" => Some(Defer),
        "else" => Some(Else),
        "false" => Some(False),
        "for" => Some(For),
        "if" => Some(If),
        "return" => Some(Return),
        "struct" => Some(Struct),
        "true" => Some(True),
        _ => None
    }
}

