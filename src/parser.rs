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
    if !is_done(ctx) && ctx.tokens[ctx.current].token_type == token {
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

fn parse_if(ctx: &mut ParsingContext) -> Expr {
    let condition = parse_expression(ctx, 0);
    let then = parse_block(ctx);
    let otherwise = if accept(ctx, TokenType::Else) {
        if accept(ctx, TokenType::If) {
            let nested = parse_if(ctx);
            let block = Block { stmts: vec![ Stmt { node: StmtKind::Expr(box nested) }] };
            Some(box block)
        } else {
            Some(box parse_block(ctx))
        }
    } else {
        None
    };

    Expr { node: ExprKind::If(box condition, box then, otherwise), t: None }
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

fn parse_float_literal(ctx: &mut ParsingContext, token: Token) -> Expr {
    let f = token.lexeme.unwrap().parse::<f64>().unwrap();
    Expr {node: ExprKind::Literal(Box::new(LitKind::Float(f))), t: None }
}

fn parse_string_literal(ctx: &mut ParsingContext, token: Token) -> Expr {
    let s = token.lexeme.unwrap();
    Expr {node: ExprKind::Literal(Box::new(LitKind::Str(s))), t: None }
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

fn parse_binary_operator(ctx: &mut ParsingContext, left: Expr, operator: BinaryOperatorKind) -> Expr {

    let precedence = get_precedence(operator);
    let right = parse_expression(ctx, precedence);

    Expr{ node: ExprKind::Binary(operator, Box::new(left), Box::new(right)), t: None }
}

fn parse_infix_operator(ctx: &mut ParsingContext, left: Expr, token: Token) -> Expr {
    use self::TokenType::*;

    if token.token_type == LeftParen {
        parse_call(ctx, left)
    } else if let Some(operator) = convert_token_to_binary_operator(token.token_type) {
        parse_binary_operator(ctx, left, operator)
    } else {
        panic!("Unsupported infix operator: {:?} on line {}", token.token_type, token.line);
    }
}

fn parse_call(ctx: &mut ParsingContext, left: Expr) -> Expr {
    use self::TokenType::*;

    let mut args = Vec::new();

    if !accept(ctx, RightParen) {
        loop {
            let expr = parse_expression(ctx, 0);
            args.push(box expr);
            if !accept(ctx, Comma) { break; }
        }
        expect(ctx, RightParen);
    }

    Expr {node: ExprKind::Call(box left,  args), t: None }
}

fn get_current_precedence(ctx: &mut ParsingContext) -> u32 {
    use self::TokenType::*;

    if ctx.tokens.len() <= ctx.current {
       0
    } else {
        let token = ctx.tokens[ctx.current].token_type;
        if let Some(op) = convert_token_to_binary_operator(token) {
            get_precedence(op)
        } else if token == LeftParen {
            8
        } else {
            0
        }
    }
}


fn parse_expression(ctx: &mut ParsingContext, precedence: u32) -> Expr {
    use tokens::TokenType::*;

    let token = consume(ctx);

    let mut left = match token.token_type {
        Identifier => parse_identifier(ctx, token.clone()),
        Integer => parse_integer_literal(ctx, token.clone()),
        TokenType::String => parse_string_literal(ctx, token.clone()),
        Float => parse_float_literal(ctx, token.clone()),
        Minus | Bang => parse_prefix_operator(ctx, token.clone()),
        LeftParen => {
            let inner = parse_expression(ctx, 0);
            expect(ctx, RightParen);
            inner
        }
        If => parse_if(ctx),
        _ => panic!("{:?} is not a valid expression prefix", token.token_type),
    };

    while precedence < get_current_precedence(ctx) {
        let token = consume(ctx);
        left = parse_infix_operator(ctx, left, token);
    }

    left

}

fn parse_type(ctx: &mut ParsingContext) -> Type {
    let token = consume(ctx);
    if token.token_type == TokenType::Identifier {
        Type {name: token.lexeme.unwrap() }
    } else {
        panic!("Expected type but got {:?} on line {:?}", token.token_type, token.line);
    }
}

fn parse_variable_decl(ctx: &mut ParsingContext) -> Item {
    let identifier = consume(ctx);
    expect(ctx, TokenType::Colon);
    let _type = if accept(ctx, TokenType::Equal) {
        None
    } else {
        let t = parse_type(ctx);
        expect(ctx, TokenType::Equal);
        Some(box t)
    };
    let expr = parse_expression(ctx, 0);
    let node = ItemKind::VariableDecl(_type, box expr);
    Item {name: identifier.lexeme.unwrap(), node, line: identifier.line }
}

fn parse_const_decl(ctx: &mut ParsingContext) -> Item {
    panic!("Not yet implemented");
}

fn parse_assignment(ctx: &mut ParsingContext) -> Stmt {
    let place = parse_expression(ctx, 0);
    expect(ctx, TokenType::Equal);
    let value = parse_expression(ctx, 0);
    Stmt { node: StmtKind::Assignment(box place, box value) }
}

fn parse_stmt(ctx: &mut ParsingContext) -> Stmt {
    use self::TokenType::*;

    let result = if accept(ctx, Break) {
        Stmt { node: StmtKind::Break }
    } else if accept(ctx, Continue) {
        Stmt { node: StmtKind::Continue }
    } else if accept(ctx, Return) {
        let expr = parse_expression(ctx, 0);
        Stmt { node: StmtKind::Return(box expr) }
    } else if accept(ctx, Defer) {
        let expr = parse_expression(ctx, 0);
        Stmt { node: StmtKind::Defer(box expr) }
    } else if accept(ctx, While) {
        let expr = parse_expression(ctx, 0);
        let block = parse_block(ctx);
        Stmt { node: StmtKind::While(box expr, box block) }
    } else if accept(ctx, Semicolon) {
        Stmt { node: StmtKind::Empty }
    } else {

        if look_ahead(ctx, 0).token_type == Identifier && look_ahead(ctx, 1).token_type == Colon {
            Stmt { node: StmtKind::Item(box parse_variable_decl(ctx)) }
        } else {
            let left = parse_expression(ctx, 0);
            let next = look_ahead(ctx, 0);
            if next.token_type == Equal {
                parse_assignment(ctx)
            } else if next.token_type == Semicolon || next.token_type == RightCurly {
                Stmt { node: StmtKind::Expr(box left) }
            } else {
                panic!("Unexpected token {:?} on line {} ", next, next.line);
            }
        }
    };
    if look_ahead(ctx, 0).token_type != RightCurly {
        expect(ctx, Semicolon);
    }
    result
}

fn parse_block(ctx: &mut ParsingContext) -> Block {
    use self::TokenType::*;

    let mut stmts = Vec::new();

    expect(ctx, LeftCurly);
    while !accept(ctx, RightCurly) {
        let stmt = parse_stmt(ctx);
        stmts.push(stmt);
    }

    Block { stmts }
}

fn parse_signature(ctx: &mut ParsingContext) -> Signature {
    use self::TokenType::*;

    let mut inputs = Vec::new();

    expect(ctx, LeftParen);
    if !accept(ctx, RightParen) {
        loop {
            let arg_name = consume(ctx);
            if arg_name.token_type != Identifier {
                panic!("Unexpected {:?} token in function signature on line {}", arg_name.token_type, arg_name.line);
            }
            expect(ctx, Colon);
            let arg_type = parse_type(ctx);
            inputs.push((box arg_type, arg_name.lexeme.unwrap()));
            if !accept(ctx, Comma) { break; }
        }
        expect(ctx, RightParen);
    }

    let output = if accept(ctx, Arrow) {
        Some(box parse_type(ctx))
    } else {
        None
    };

    Signature { inputs, output }
}

fn parse_function_decl(ctx: &mut ParsingContext) -> Item {
    use self::TokenType::*;

    let identifier = consume(ctx);
    expect(ctx, ColonColon);
    let signature = parse_signature(ctx);
    let block = if look_ahead(ctx, 0).token_type == LeftCurly {
        Some(box parse_block(ctx))
    } else {
        None
    };

    let node = ItemKind::FunctionDecl(box signature, block);

    Item {name: identifier.lexeme.unwrap(), node, line: identifier.line }
}

fn parse_item(ctx: &mut ParsingContext) -> Item {
    use self::TokenType::*;
    let token = look_ahead(ctx, 0);
    if token.token_type != Identifier {
        panic!("Tried to parse a item starting with a {:?} on line {:?}", token.token_type, token.line);
    }
    let result = match look_ahead(ctx, 1).token_type {
        Colon => parse_variable_decl(ctx),
        ColonColon => match look_ahead(ctx, 2).token_type {
            Identifier | Equal => parse_const_decl(ctx),
            LeftParen => parse_function_decl(ctx),
            _ => panic!("Unexpected token {:?} in item on line {:?}", token.token_type, token.line)
        }
        _ => panic!("Unexpected token {:?} in item on line {:?}", token.token_type, token.line)
    };
    accept(ctx, Semicolon);
    result
}

pub fn parse(tokens: Vec<Token>) -> Vec<Item> {
    let mut ctx = ParsingContext {current: 0, tokens};

    let mut ast = Vec::new();
    while !is_done(&mut ctx) {
       ast.push(parse_item(&mut ctx));
    }
    ast
}