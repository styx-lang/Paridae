/*
 * Copyright (c) 2018 Lasse Dissing
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use tokens::*;
use ast::*;

use std::collections::HashMap;

struct ParsingContext {
    current: usize,
    tokens: Vec<Token>,
    types: HashMap<String, Type>,
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

    Expr { node: ExprKind::If(box condition, box then, otherwise), t: Type::Infer }
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
    Expr {node: ExprKind::Literal(Box::new(LitKind::Int(n))), t: Type::Signed(IntegerSize::Unspecified) }
}

fn parse_float_literal(ctx: &mut ParsingContext, token: Token) -> Expr {
    let f = token.lexeme.unwrap().parse::<f64>().unwrap();
    Expr {node: ExprKind::Literal(Box::new(LitKind::Float(f))), t: Type::Float(FloatingSize::Unspecified) }
}

fn parse_string_literal(ctx: &mut ParsingContext, token: Token) -> Expr {
    let s = token.lexeme.unwrap();
    Expr {node: ExprKind::Literal(Box::new(LitKind::Str(s))), t: Type::Infer }
}

fn parse_identifier(ctx: &mut ParsingContext, token: Token) -> Expr {
    Expr{ node: ExprKind::Identifier(token.lexeme.unwrap()), t: Type::Infer }
}

fn parse_prefix_operator(ctx: &mut ParsingContext, token: Token) -> Expr {
    use tokens::TokenType::*;

    let operation = match token.token_type {
        Minus => UnaryOperatorKind::Negation,
        Bang => UnaryOperatorKind::Complement,
        And => UnaryOperatorKind::Refer,
        Star => UnaryOperatorKind::Deref,
        _ => panic!("{:?} is not a valid prefix operator!", token.token_type),
    };

    let operand = parse_expression(ctx, 6);
    Expr{ node: ExprKind::Unary(operation, Box::new(operand)), t: Type::Infer }
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

    Expr{ node: ExprKind::Binary(operator, Box::new(left), Box::new(right)), t: Type::Infer }
}


fn parse_member_access(ctx: &mut ParsingContext, left: Expr) -> Expr {
    let field_token = consume(ctx);
    if field_token.token_type != TokenType::Identifier {
        panic!("Token {:?} is not a valid struct field", field_token);
    }
    let field_name = field_token.lexeme.unwrap();

    Expr { node: ExprKind::Member(box left, field_name), t: Type::Infer }
}

fn parse_indexing(ctx: &mut ParsingContext, left: Expr) -> Expr {
    let index = parse_expression(ctx, 0);
    expect(ctx, TokenType::RightBracket);

    Expr { node: ExprKind::Index(box left, box index), t: Type::Infer }
}

fn parse_infix_operator(ctx: &mut ParsingContext, left: Expr, token: Token) -> Expr {
    use self::TokenType::*;

    if token.token_type == LeftParen {
        parse_call(ctx, left)
    } else if token.token_type == Dot {
        parse_member_access(ctx, left)
    } else if token.token_type == LeftBracket {
        parse_indexing(ctx, left)
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

    Expr {node: ExprKind::Call(box left,  args), t: Type::Infer }
}

fn get_current_precedence(ctx: &mut ParsingContext) -> u32 {
    use self::TokenType::*;

    if ctx.tokens.len() <= ctx.current {
       0
    } else {
        let token = ctx.tokens[ctx.current].token_type;
        if let Some(op) = convert_token_to_binary_operator(token) {
            get_precedence(op)
        } else if token == LeftParen || token == Dot || token == LeftBracket {
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
        Minus | Bang | And | Star => parse_prefix_operator(ctx, token.clone()),
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

fn primitive_type_by_name(name: &String, ctx: &ParsingContext) -> Type {
    use self::Type::*;

    match name.as_ref() {
        "int" => Signed(IntegerSize::Unspecified),
        "float" => Float(FloatingSize::Unspecified),
        "s8" => Signed(IntegerSize::I8),
        "s16" => Signed(IntegerSize::I16),
        "s32" => Signed(IntegerSize::I32),
        "s64" => Signed(IntegerSize::I64),
        "u8" => Unsigned(IntegerSize::I8),
        "u16" => Unsigned(IntegerSize::I16),
        "u32" => Unsigned(IntegerSize::I32),
        "u64" => Unsigned(IntegerSize::I64),
        "bool" => Bool,
        "char" => Char,
        "void" => Void,
        _ => {
                panic!("{} is not a primitive type", name)
        },
    }
}

fn parse_type(ctx: &mut ParsingContext) -> Type {
    let token = consume(ctx);
    if token.token_type == TokenType::Identifier {
        let type_name = token.lexeme.unwrap();
        if let Some(adt) = ctx.types.get(&type_name) {
            adt.clone()
        } else {
            primitive_type_by_name(&type_name, ctx)
        }
    } else if token.token_type == TokenType::Star {
        let inner = parse_type(ctx);
        Type::Ptr(box inner)
    } else if token.token_type == TokenType::LeftBracket {
        expect(ctx, TokenType::RightBracket);
        let inner = parse_type(ctx);
        Type::Slice(box inner)
    } else {
        panic!("Expected type but got {:?} on line {:?}", token.token_type, token.line);
    }
}

fn parse_variable_decl(ctx: &mut ParsingContext) -> Item {
    let identifier = consume(ctx);
    expect(ctx, TokenType::Colon);
    let _type = if accept(ctx, TokenType::Equal) {
        Type::Infer
    } else {
        let t = parse_type(ctx);
        t
    };
    let expr = if accept(ctx, TokenType::Equal) {
        Some(box parse_expression(ctx, 0))
    } else {
        None
    };
    let node = ItemKind::VariableDecl(_type, expr);
    Item {name: identifier.lexeme.unwrap(), node, line: identifier.line }
}

fn parse_const_decl(ctx: &mut ParsingContext) -> Item {
    panic!("Not yet implemented");
}

fn parse_assignment(place: Expr, ctx: &mut ParsingContext) -> Stmt {
    expect(ctx, TokenType::Equal);
    let value = parse_expression(ctx, 0);
    Stmt { node: StmtKind::Assignment(box place, box value) }
}

fn parse_stmt(ctx: &mut ParsingContext) -> Stmt {
    use self::TokenType::*;

    let mut semicolon_exception = false;

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
        semicolon_exception = true;
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
                parse_assignment(left, ctx)
            } else if next.token_type == Semicolon || next.token_type == RightCurly {
                Stmt { node: StmtKind::Expr(box left) }
            } else {
                panic!("Unexpected token {:?} on line {} ", next, next.line);
            }
        }
    };
    if !semicolon_exception && look_ahead(ctx, 0).token_type != RightCurly {
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
            inputs.push((arg_type, arg_name.lexeme.unwrap()));
            if !accept(ctx, Comma) { break; }
        }
        expect(ctx, RightParen);
    }

    let output = if accept(ctx, Arrow) {
        parse_type(ctx)
    } else {
        Type::Void
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

fn parse_directive(ctx: &mut ParsingContext) -> Item {

    let token = consume(ctx);
    let line = token.lexeme.unwrap();
    let parts: Vec<&str> = line.split(" ").collect();
    match parts[0] {
        "#include" => Item { name: parts[1].to_string(), node: ItemKind::Directive(DirectiveKind::Include(parts[1].to_string())), line: token.line },
        _ => panic!("Unknown directive {}", parts[0]),
    }
}

fn parse_struct_decl(ctx: &mut ParsingContext) -> Item {
    let identifier = consume(ctx);
    expect(ctx, TokenType::ColonColon);
    expect(ctx, TokenType::Struct);
    expect(ctx, TokenType::LeftCurly);

    let mut fields = Vec::new();

    while !accept(ctx, TokenType::RightCurly) {
        let name_token = consume(ctx);
        if name_token.token_type != TokenType::Identifier {
            panic!("Expected field identifier byt got {:?}", name_token);
        }
        let field_name = name_token.lexeme.unwrap();
        expect(ctx, TokenType::Colon);
        let field_type = parse_type(ctx);
        fields.push((field_name, field_type));
        expect(ctx, TokenType::Semicolon);
    }

    let type_name = identifier.lexeme.unwrap();
    let type_def = Type::Struct(type_name.clone(), fields);
    if ctx.types.contains_key(&type_name) {
        panic!("Type {} defined multiple times!", type_name);
    }
    ctx.types.insert(type_name.clone(), type_def.clone());

    Item {name: type_name, node: ItemKind::StructDecl(type_def), line: identifier.line }
}

fn parse_item(ctx: &mut ParsingContext) -> Item {
    use self::TokenType::*;
    let token = look_ahead(ctx, 0);
    if token.token_type == Directive {
        return parse_directive(ctx);
    }
    if token.token_type != Identifier {
        panic!("Tried to parse a item starting with a {:?} on line {:?}", token.token_type, token.line);
    }
    let result = match look_ahead(ctx, 1).token_type {
        Colon => parse_variable_decl(ctx),
        ColonColon => match look_ahead(ctx, 2).token_type {
            Struct => parse_struct_decl(ctx),
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
    let mut ctx = ParsingContext {current: 0, tokens, types: HashMap::new()};

    let mut ast = Vec::new();
    while !is_done(&mut ctx) {
       ast.push(parse_item(&mut ctx));
    }
    ast
}