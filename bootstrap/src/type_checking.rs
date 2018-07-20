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
    let current_scope = &mut ctx.scope_arena[ctx.current].symbols;
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
        },
        UnaryOperatorKind::Complement => {
            match _type {
                Type::Bool => Expr { node: ExprKind::Unary(op, box checked_inner), t: Type::Bool},
                other => panic!("Unary complement can only be done on boolean types and not on {:?}", other),
            }
        },
        UnaryOperatorKind::Deref => {
            match _type {
                Type::Ptr(box inner_type) => Expr { node: ExprKind::Unary(op, box checked_inner), t: inner_type },
                other => panic!("Cannot dereference non-pointer type {:?}", other),
            }
        },
        UnaryOperatorKind::Refer => {
            Expr { node: ExprKind::Unary(op, box checked_inner), t: Type::Ptr(box _type) }
        }
    }
}

fn try_implicit_cast(expr: Expr, target: Type, ctx: &mut TypeContext) -> Option<Expr> {
    use self::ExprKind::*;

    if expr.t == Type::Signed(IntegerSize::Unspecified) {

        match expr.node {
            Literal(l) => Some(Expr { node: Literal(l), t: target }),
            Unary(op, box inner) => {
                if let Some(i) = try_implicit_cast(inner, target.clone(), ctx) {
                    Some(Expr { node: Unary(op, box i), t: target})
                } else {
                    None
                }
            },
            Binary(op, box lhs, box rhs) => {
                match (try_implicit_cast(lhs, target.clone(), ctx), try_implicit_cast(rhs, target.clone(), ctx)) {
                    (Some(l), Some(r)) => Some(Expr { node: Binary(op, box l, box r), t: target}),
                    _ => None
                }
            },
            _ => None
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
            panic!("Binary operations can only be done on equal types: {:?} vs {:?}", checked_lhs, checked_rhs);
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
        if checked.t != input && input == Type::Void {
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

fn check_member_access(owner: Expr, field_name: String, ctx: &mut TypeContext) -> Expr {

    let checked_owner = check_expr(owner, ctx);
    let field_type = {

        let extract_field_type = |struct_name: String, fields: Vec<(String, Type)>| -> Type {
            for (n, t) in fields {
                if n == field_name {
                    return t;
                }
            }
            panic!("Struct {} does not have a field named {}", struct_name, field_name);
        };

        if let Type::Struct(struct_name, fields) = checked_owner.t.clone() {
            extract_field_type(struct_name, fields)
        } else if let Type::Ptr(box Type::Struct(struct_name, fields)) = checked_owner.t.clone() {
            extract_field_type(struct_name, fields)
        } else {
            panic!("Tried to access field {} on non-struct {:?}", field_name, checked_owner);
        }
    };

    Expr { node: ExprKind::Member(box checked_owner, field_name), t: field_type }
}

fn check_array_indexing(array: Expr, index: Expr, ctx: &mut TypeContext) -> Expr {

    let checked_array = check_expr(array, ctx);
    let checked_index = check_expr(index, ctx);

    match checked_index.t.clone() {
        Type::Signed(_) => {},
        other => panic!("Cannot access an array using a {:?} as index", other),
    }

    if let Type::Slice(box array_type) = checked_array.t.clone() {
        Expr { node: ExprKind::Index(box checked_array, box checked_index), t: array_type }
    } else {
        panic!("Tried to index into non-slice type {:?}", checked_array);
    }
}

fn check_literal(lit: LitKind) -> Type {
    use self::LitKind::*;

    match lit {
        Bool(_) => Type::Bool,
        Float(_) => Type::Float(FloatingSize::F64),
        Int(_) => Type::Signed(IntegerSize::I32),
        Str(_) => Type::Slice(box Type::Signed(IntegerSize::I8))
    }
}

fn check_casting(target: Type, inner: Expr, ctx: &mut TypeContext) -> Expr {
    let checked_inner = check_expr(inner, ctx);
    Expr { node: ExprKind::Cast(target.clone(), box checked_inner), t: target }
}

fn check_expr(expr: Expr, ctx: &mut TypeContext) -> Expr {
    use self::ExprKind::*;

    match expr.node {
        Cast(target, box inner) => check_casting(target, inner, ctx),
        Unary(op, box inner) => check_unary_operator(op, inner, ctx),
        Binary(op, box lhs, box rhs) => check_binary_operator(op, lhs, rhs, ctx),
        Literal(box lit) => Expr { node: ExprKind::Literal(box lit.clone()), t: check_literal(lit) },
        Identifier(s) => check_identifier(s, ctx),
        Call(box fun, args) => check_call(fun, args, ctx),
        If(box condition, box then, otherwise) => check_condition(condition, then, otherwise, ctx),
        Member(box owner, field_name) => check_member_access(owner, field_name, ctx),
        Index(box array, box index) => check_array_indexing(array, index, ctx),
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

fn check_while(condition: Expr, block: Block, ctx: &mut TypeContext) -> StmtKind {
    let checked_condition = check_expr(condition, ctx);

    if checked_condition.t != Type::Bool {
        panic!("Condition in while loop must be of boolean type");
    }

    let checked_block = check_block(block, ctx);

    StmtKind::While(box checked_condition, box checked_block)
}

fn check_assignment(place: Expr, value: Expr, ctx: &mut TypeContext) -> StmtKind {
    let checked_place = check_expr(place, ctx);
    let mut checked_value = check_expr(value, ctx);

    if checked_place.t != checked_value.t {
        if let Some(expr) = try_implicit_cast(checked_value.clone(), checked_place.t.clone(), ctx) {
            checked_value = expr;
        } else {
            panic!("Trying to assign value {:?} to place {:?}", checked_value, checked_place);
        }
    }

    StmtKind::Assignment(box checked_place, box checked_value)
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
        },
        While(box condition, box block) => check_while(condition, block, ctx),
        Assignment(box place, box value) => check_assignment(place, value, ctx),
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
        if t == Type::Void {
            panic!("Parameter {} in signature of function {} cannot be void", n, name);
        }
        argument_types.push(t.clone());
        checked_inputs.push((t, n));
    }

    declare_symbol(&name, &Type::Function(argument_types, box sig.output.clone()), ctx);

    let checked_block = if let Some(box b) = block {

        initialize_scope( sig.output.clone(), ctx);
        for (param_type, param_name) in &checked_inputs {
            declare_symbol(param_name, param_type, ctx);
        }
        let res_block = check_block(b, ctx);
        finalize_scope(ctx);
        Some(box res_block)
    } else {
        None
    };

    ItemKind::FunctionDecl(box Signature {inputs: checked_inputs, output: sig.output}, checked_block)
}

fn check_variable_decl(name: String, t: Type, expr: Option<Box<Expr>>, ctx: &mut TypeContext) -> ItemKind {
    use self::Type::*;

    let res_expr = if let Some(box e) = expr {
        let mut res_expr = check_expr(e, ctx);

        if t == Infer {
            panic!("Type inference not yet operational. See variable {} ", name);
        }

        if t != res_expr.t {
            res_expr = if let Some(e) = try_implicit_cast(res_expr.clone(), t.clone(), ctx) {
                e
            } else {
                panic!("Declared type {:?} of variable {} does not match right hand {:?}", t, name, res_expr);
            };
        }
        Some(box res_expr)
    } else {
        None
    };

    declare_symbol(&name, &t, ctx);

    ItemKind::VariableDecl(t, res_expr)
}

fn check_const_decl(name: String, t: Type, expr: Expr, ctx: &mut TypeContext) -> ItemKind {
    panic!("Not yet supported");
}

fn check_item(item: Item, ctx: &mut TypeContext) -> Item {
    use self::ItemKind::*;

    let name = item.name.clone();
    let kind = match item.node {
        FunctionDecl(box sig, block) => check_function_decl(name, sig, block, ctx),
        VariableDecl(t, expr) => check_variable_decl(name, t, expr, ctx),
        ConstDecl(t, box expr) => check_const_decl(name, t, expr, ctx),
        StructDecl(t) => StructDecl(t),
        _ => panic!("Other item kinds does not yet type check: {:?}", item.node),
    };

    Item {name: item.name, node: kind, line: item.line }
}

fn add_builtin(ctx: &mut TypeContext) {

    declare_symbol(&String::from("len"), &Type::Function(vec![Type::Slice(box Type::Void)], box Type::Signed(IntegerSize::I32)), ctx);
}

pub fn check(ast: Vec<Item>) -> Vec<Item> {

    let global_scope = TypeScope { symbols: HashMap::new(), parent: 0, current_return_type: Type::Void };

    let mut ctx = TypeContext { current: 0, scope_arena: vec![global_scope] };

    add_builtin(&mut ctx);

    let mut resulting_ast = Vec::new();

    for item in ast {
        resulting_ast.push(check_item(item, &mut ctx));
    }

    resulting_ast
}