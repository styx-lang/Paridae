/*
 * Copyright (c) 2018 Lasse Dissing
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use std::collections::HashMap;
use ast::*;

struct TypeScope {
    symbols: HashMap<String, Type>,
    parent: usize,
    current_return_type: Type
}

struct TypeContext {
    current: usize,
    scope_arena: Vec<TypeScope>,
}

fn initialize_scope(return_type: Type, ctx: &mut TypeContext) {
    let new_scope = TypeScope { symbols: HashMap::new(), parent: ctx.current, current_return_type: return_type  } ;
    ctx.scope_arena.push(new_scope);
    ctx.current = ctx.scope_arena.len()-1;
}

fn finalize_scope(ctx: &mut TypeContext) {
    ctx.current = ctx.scope_arena[ctx.current].parent;
}

fn declare_symbol(name: &String, t: &Type, ctx: &mut TypeContext) {
    let mut current_scope = &mut ctx.scope_arena[ctx.current].symbols;
    if current_scope.contains_key(name) {
        panic!("Redeclaring symbol {}", name);
    }
    current_scope.insert(name.clone(), t.clone());
}

fn lookup_symbol(name: &String, ctx: &mut TypeContext) -> Type {
    let mut reached_top = false;

    let mut current = ctx.current;
    while !reached_top {
        if current == 0 {
            reached_top = true;
        }
        let current_scope = &ctx.scope_arena[current];
        if let Some(t) = current_scope.symbols.get(name) {
            return t.clone();
        } else {
            current = current_scope.parent;
        }
    }
    panic!("Failed to find symbol {}", name);
}

fn get_current_return_type(ctx: &mut TypeContext) -> Type {
    ctx.scope_arena[ctx.current].current_return_type.clone()
}

fn primitive_type_by_name(name: &String) -> Type {
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
        _ => panic!("{} is not a primitive type", name),
    }
}

fn type_by_name(name: &String) -> Type {
    primitive_type_by_name(name)
}

fn check_type(t: Type) -> Type {
    if let Type::Unchecked(s) = t {
        type_by_name(&s)
    } else if t == Type::Void {
        Type::Void
    } else {
        panic!("ICE: Missing type information for {:?}", t);
    }
}

fn check_unary_operator(op: UnaryOperatorKind, inner: Expr, ctx: &mut TypeContext) -> Expr {

    let checked_inner = check_expr(inner, ctx);

    let _type = checked_inner.t.clone();

    match op {
        UnaryOperatorKind::Negation => {
            match _type {
                Type::Float(_) | Type::Signed(_) | Type::Unsigned(_) =>
                    Expr { node: ExprKind::Unary(op, box checked_inner), t: _type},
                other => panic!("Unary negation can only be done on numeric types and not on {:?}", other),
            }
        }
        UnaryOperatorKind::Complement => {
            match _type {
                Type::Bool => Expr { node: ExprKind::Unary(op, box checked_inner), t: Type::Bool},
                other => panic!("Unary complement can only be done on boolean types and not on {:?}", other),
            }
        }
    }
}

fn try_implicit_cast(expr: Expr, target: Type, ctx: &mut TypeContext) -> Option<Expr> {
    if expr.t == Type::Signed(IntegerSize::Unspecified) {
        if let ExprKind::Literal(l) = expr.node {
            Some(Expr { node: ExprKind::Literal(l), t: target })
        } else {
            None
        }
    } else {
        None
    }
}

fn check_binary_operator(op: BinaryOperatorKind, lhs: Expr, rhs: Expr, ctx: &mut TypeContext) -> Expr {
    use self::BinaryOperatorKind::*;

    let mut checked_lhs = check_expr(lhs, ctx);
    let mut checked_rhs = check_expr(rhs, ctx);

    if checked_lhs.t != checked_rhs.t {

        if let Some(expr) = try_implicit_cast(checked_lhs.clone(), checked_rhs.t.clone(), ctx) {
            checked_lhs = expr;
        } else if let Some(expr) = try_implicit_cast(checked_rhs.clone(), checked_lhs.t.clone(), ctx) {
            checked_rhs = expr;
        } else {
            panic!("Binary operations call only be done on equal types: {:?} vs {:?}", checked_lhs, checked_rhs);
        }
    }

    let _type = checked_lhs.t.clone();

    match op {
        Addition | Subtraction | Product | Division => {
            match _type {
                Type::Float(_) | Type::Signed(_) | Type::Unsigned(_) =>
                    Expr { node: ExprKind::Binary(op, box checked_lhs, box checked_rhs), t: _type},
                other => panic!("{:?} can only be done on numeric types and not on {:?}",op , other),
            }
        },
        Less | LessEq | Greater | GreaterEq | Equality | NotEq => {
            Expr { node: ExprKind::Binary(op, box checked_lhs, box checked_rhs), t: Type::Bool }
        }
    }
}

fn check_identifier(ident: String, ctx: &mut TypeContext) -> Expr {

    let t = lookup_symbol(&ident, ctx);
    Expr { node: ExprKind::Identifier(ident), t }
}

fn check_call(fun: Expr, args: Vec<Box<Expr>>, ctx: &mut TypeContext) -> Expr {
    let fun_expr = check_expr(fun, ctx);

    let (inputs, output) = if let Type::Function(inputs, box output) = fun_expr.t.clone() {
        (inputs, output)
    } else {
        panic!("Trying to call not function type {:?}", fun_expr.t)
    };

    if args.len() != inputs.len() {
        panic!("Tried to call function {:?} with {} arguments but expected {}",fun_expr, args.len(), inputs.len())
    }

    let mut checked_args = Vec::new();

    for (input, box arg) in inputs.into_iter().zip(args.into_iter()) {
        let mut checked = check_expr(arg, ctx);
        if checked.t != input {
            if let Some(e) = try_implicit_cast(checked.clone(), input.clone(), ctx) {
                checked = e;
            } else {
                panic!("Tried to call {:?} with arguments of type {:?} but expected {:?}",fun_expr, checked.t, input);
            }
        }
        checked_args.push(box checked);
    }

    Expr { node: ExprKind::Call(box fun_expr, checked_args), t: output }
}

fn check_condition(condition: Expr, then: Block, otherwise: Option<Box<Block>>, ctx: &mut TypeContext) -> Expr {

    let checked_condition = check_expr(condition, ctx);
    if checked_condition.t != Type::Bool {
        panic!("Condition to an if expression must return a boolean value")     ;
    }

    let checked_then = check_block(then, ctx);
    let checked_otherwise = if let Some(box o) = otherwise {
        Some(box check_block(o, ctx))
    } else {
        None
    };

    Expr { node: ExprKind::If(box checked_condition, box checked_then, checked_otherwise), t: Type::Void }
}

fn check_expr(expr: Expr, ctx: &mut TypeContext) -> Expr {
    use self::ExprKind::*;

    match expr.node {
        Unary(op, box inner) => check_unary_operator(op, inner, ctx),
        Binary(op, box lhs, box rhs) => check_binary_operator(op, lhs, rhs, ctx),
        Literal(box lit) => Expr { node: ExprKind::Literal(box lit), t: expr.t },
        Identifier(s) => check_identifier(s, ctx),
        Call(box fun, args) => check_call(fun, args, ctx),
        If(box condition, box then, otherwise) => check_condition(condition, then, otherwise, ctx),
        _ => panic!("Other expressions kinds not yet supported {:?}", expr.node),
    }
}


fn check_return(expr: Expr, ctx: &mut TypeContext) -> StmtKind {
    let mut checked_expr = check_expr(expr, ctx);

    let expected_type = get_current_return_type(ctx);
    if checked_expr.t != expected_type {
        if let Some(e) = try_implicit_cast(checked_expr.clone(), expected_type.clone(), ctx) {
            checked_expr = e;
        } else {
            panic!("Trying to return type {:?} but expected {:?}", checked_expr.t, expected_type);
        }
    }

    StmtKind::Return(box checked_expr)
}

fn check_stmt(stmt: Stmt, ctx: &mut TypeContext) -> Stmt {
    use self::StmtKind::*;

    let kind = match stmt.node {
        Return(box e) => check_return(e, ctx),
        Item(box i) => {
            let checked_item = check_item(i, ctx);
            Item(box checked_item)
        },
        Expr(box e) => {
            let checked_expr = check_expr(e, ctx);
            Expr(box checked_expr)
        }
        _ => panic!("Other statement kinds not yet supported {:?}")
    };

    Stmt { node: kind }
}

fn check_block(block: Block, ctx: &mut TypeContext) -> Block {
    let mut res_stmts = Vec::new();

    for stmt in block.stmts {
        res_stmts.push(check_stmt(stmt, ctx));
    }

    Block { stmts: res_stmts }
}

fn check_function_decl(name: String, sig: Signature, block: Option<Box<Block>>, ctx: &mut TypeContext) -> ItemKind {

    let mut checked_inputs = Vec::new();
    let mut argument_types = Vec::new();

    for (t, n) in sig.inputs {
        let res_t = check_type(t);
        if res_t == Type::Void {
            panic!("Parameter {} in signature of function {} cannot be void", n, name);
        }
        argument_types.push(res_t.clone());
        checked_inputs.push((res_t, n));
    }
    let checked_output = check_type(sig.output);

    declare_symbol(&name, &Type::Function(argument_types, box checked_output.clone()), ctx);

    let checked_block = if let Some(box b) = block {

        initialize_scope( checked_output.clone(), ctx);
        for (param_type, param_name) in &checked_inputs {
            declare_symbol(param_name, param_type, ctx);
        }
        let res_block = check_block(b, ctx);
        finalize_scope(ctx);
        Some(box res_block)
    } else {
        None
    };

    ItemKind::FunctionDecl(box Signature {inputs: checked_inputs, output: checked_output}, checked_block)
}

fn check_variable_decl(name: String, t: Type, expr: Expr, ctx: &mut TypeContext) -> ItemKind {
    use self::Type::*;

    let res_expr = check_expr(expr, ctx);

    let res_t = if let Unchecked(s) = t {
        primitive_type_by_name(&s)
    } else if t == Infer {
        res_expr.t.clone()
    } else {
        panic!("Declaration of variable {} did not type check", name);
    };

    declare_symbol(&name, &res_t, ctx);

    ItemKind::VariableDecl(res_t, box res_expr)
}

fn check_const_decl(name: String, t: Type, expr: Expr, ctx: &mut TypeContext) -> ItemKind {
    panic!("Not yet supported");
}

fn check_item(item: Item, ctx: &mut TypeContext) -> Item {
    use self::ItemKind::*;

    let name = item.name.clone();
    let kind = match item.node {
        FunctionDecl(box sig, block) => check_function_decl(name, sig, block, ctx),
        VariableDecl(t, box expr) => check_variable_decl(name, t, expr, ctx),
        ConstDecl(t, box expr) => check_const_decl(name, t, expr, ctx),
        _ => panic!("Other item kinds does not yet type check: {:?}", item.node),
    };

    Item {name: item.name, node: kind, line: item.line }
}

pub fn check(ast: Vec<Item>) -> Vec<Item> {

    let global_scope = TypeScope { symbols: HashMap::new(), parent: 0, current_return_type: Type::Void };

    let mut ctx = TypeContext { current: 0, scope_arena: vec![global_scope] };

    let mut resulting_ast = Vec::new();

    for item in ast {
        resulting_ast.push(check_item(item, &mut ctx));
    }

    resulting_ast
}