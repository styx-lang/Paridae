/*
 * Copyright (c) 2018 Lasse Dissing
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

#[derive(Debug)]
pub enum TokenType {
    Identifier,
    //Keywords
    Break,
    Const,
    Continue,
    Defer,
    //Operators and punctuation
    Plus,
    Minus,
    Star,
    Slash,
    And,
    Or,
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
    Semicolon,
    //Literals
    Integer,
    Float,
    String,
    Boolean,
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: Option<String>,
    pub line: usize,
}
