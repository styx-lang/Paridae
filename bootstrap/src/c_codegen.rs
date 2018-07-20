/*
 * Copyright (c) 2018 Lasse Dissing
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use ast::*;


struct CodeGenContext {
    builder: Vec<String>,
}

fn generate_literal(lit: LitKind, t: Type) -> String {
    use self::LitKind::*;

    match lit {
        Int(i) =>format!("{}", i),
        Str(s) => format!("{{(u8*)\"{}\", {}}}", s, s.len()+1),
        _ => panic!("Literal type not yet supported!")
    }
}

fn get_int_size_postfix(size: IntegerSize) -> String {
    use self::IntegerSize::*;

    match size {
        I8 => "8",
        I16 => "16",
        I32 => "32",
        I64 => "64",
        Arch => "64",
        Unspecified => "32",
    }.to_string()
}

fn get_type(t: Type, ctx: &CodeGenContext) -> String {
    match t {
        Type::Void => "void".to_string(),
        Type::Signed(size) => format!("s{}", get_int_size_postfix(size)),
        Type::Unsigned(size) => format!("u{}", get_int_size_postfix(size)),
        Type::Bool => "int".to_string(),
        Type::Ptr(box inner) => format!("{}*", get_type(inner, ctx)),
        Type::Slice(box inner) => "_paridae_slice".to_string(),
        Type::Struct(name, _) => name,
        other => panic!("Type {:?} is not yet implemented", other),
    }
}

fn generate_unary_operator(op: UnaryOperatorKind, inner: Expr, ctx: &mut CodeGenContext) -> String {
    let inner_val = generate_expr(inner.clone(), ctx);
    match op {
        UnaryOperatorKind::Deref => {
            format!("*{}", inner_val)
        },
        UnaryOperatorKind::Refer => {
            format!("&{}", inner_val)
        },
        _ => panic!("Unary operator {:?} not yet implemented", op),
    }
}

fn generate_binary_operator(bin_op: BinaryOperatorKind, left: Expr, right: Expr, ctx: &mut CodeGenContext) -> String {
    use self::BinaryOperatorKind::*;

    let lhs = generate_expr(left, ctx);
    let rhs = generate_expr(right, ctx);

    match bin_op {
        Addition => format!("({} + {})", lhs, rhs),
        Subtraction => format!("({} - {})", lhs, rhs),
        Product => format!("({} * {})", lhs, rhs),
        Division => format!("({} / {})", lhs, rhs),
        Equality => format!("({} == {})", lhs, rhs),
        NotEq => format!("({} != {})", lhs, rhs),
        Less => format!("({} < {})", lhs, rhs),
        LessEq => format!("({} <= {})", lhs, rhs),
        Greater => format!("({} > {})", lhs, rhs),
        GreaterEq => format!("({} >= {})", lhs, rhs),
    }
}

fn generate_call(fun: Expr, args: Vec<Box<Expr>>, ctx: &mut CodeGenContext) -> String {

    let fun_expr = generate_expr(fun, ctx);

    let mut arg_values = Vec::new();

    for box arg in args {
        arg_values.push(generate_expr(arg, ctx));
    }

    format!("{}({})", fun_expr, arg_values.join(","))
}

fn generate_condition(condition: Expr, then: Block, otherwise: Option<Box<Block>>, ctx: &mut CodeGenContext) -> String {
    let condition_value = generate_expr(condition, ctx);

    ctx.builder.push(format!("if ({})", condition_value));
    generate_block(then, ctx);
    if let Some(box o) = otherwise {
        generate_block(o, ctx);
    }
    String::from("Conditions are not real expressions yet!")
}


fn generate_field_access(owner: Expr, field_name: String, ctx: &mut CodeGenContext) -> String {

    let owner_expr = generate_expr(owner, ctx);

    format!("{}.{}", owner_expr, field_name)
}


fn generate_array_index(array: Expr, index: Expr, ctx: &mut CodeGenContext) -> String {
    let array_type = get_type(array.t.clone(), ctx);
    let ptr = generate_expr(array, ctx);
    let offset = generate_expr(index, ctx);

    format!("{}.ptr[{}*sizeof({})]", ptr, offset, array_type)
}

fn generate_cast(target: Type, inner: Expr, ctx: &mut CodeGenContext) -> String {
    format!("({})({})", get_type(target, ctx), generate_expr(inner, ctx))
}

fn generate_expr(expr: Expr, ctx: &mut CodeGenContext) -> String {
    use self::ExprKind::*;

    match expr.node {
        Cast(target, box inner) => generate_cast(target, inner, ctx),
        Unary(op, box inner) => generate_unary_operator(op, inner, ctx),
        Binary(op, box lhs, box rhs) => generate_binary_operator(op, lhs, rhs, ctx),
        Literal(box lit) => generate_literal(lit, expr.t),
        Identifier(s) => s,
        Call(box fun, args) => generate_call(fun, args, ctx),
        If(box condition, box then, otherwise) => generate_condition(condition, then, otherwise, ctx),
        Member(box owner, field_name) => generate_field_access(owner, field_name, ctx),
        Index(box array, box index) => generate_array_index(array, index, ctx),
    }
}

fn generate_return(e: Expr, ctx: &mut CodeGenContext) {
    let val = generate_expr(e, ctx);
    ctx.builder.push(format!("return {};", val));
}

fn generate_while(condition: Expr, block: Block, ctx: &mut CodeGenContext) {

    let condition_value = generate_expr(condition, ctx);

    ctx.builder.push(format!("while ({})", condition_value));
    generate_block(block, ctx);
}

fn generate_assignment(place: Expr, value: Expr, ctx: &mut CodeGenContext) {
    let res_val = generate_expr(value, ctx);

    //TODO Simplify

    let result = match place.node {
        ExprKind::Identifier(ident) => {
            format!("{} = {};", ident, res_val)
        },
        ExprKind::Unary(UnaryOperatorKind::Deref, box inner) => {
            if let ExprKind::Identifier(ident) = inner.node {
                format!("*{} = {};", ident, res_val)
            } else {
                panic!("Can only assign to simple pointers for now {:?}", inner);
            }
        },
        ExprKind::Member(box owner, field_name) => {
            let owner_expr = generate_expr(owner, ctx);
            format!("{}.{} = {}", owner_expr, field_name, res_val)
        }
        _ => panic!("Currently does not supports assigning to place {:?}", place),
    };
    ctx.builder.push(result);
}

fn generate_stmt(stmt: Stmt, ctx: &mut CodeGenContext) {
    use self::StmtKind::*;

    match stmt.node {
        Return(box e) => generate_return(e, ctx),
        Item(box i) => generate_item(i, ctx),
        Expr(box e) => {
            let result = generate_expr(e, ctx);
            ctx.builder.push(result + ";");
        },
        While(box c, box b) => generate_while(c, b, ctx),
        Assignment(box place, box value) => generate_assignment(place, value, ctx),
        _ => panic!("Other statement kinds not yet supported {:?}", stmt.node),
    }
}

fn generate_block(block: Block, ctx: &mut CodeGenContext) {
    ctx.builder.push("{".to_string());
    for stmt in block.stmts {
        generate_stmt(stmt, ctx);
    }
    ctx.builder.push("}".to_string());
}

fn generate_function_decl(name: String, sig: Signature, block: Option<Box<Block>>, ctx: &mut CodeGenContext) {
    let ret_type = get_type(sig.output, ctx);

    let mut args = Vec::new();
    for (t, n) in &sig.inputs {
        let arg_type = get_type(t.clone(), ctx);
        args.push(format!("{} {}", arg_type, n));
    }
    let signature = format!("{} {}({})", ret_type, name, args.join(","));


    if let Some(b) = block {
        ctx.builder.push(signature);
        generate_block(*b, ctx);
    } else {
        ctx.builder.push(signature + ";");
    }
}

fn generate_variable_decl(name: String, _type: Type, expr: Option<Box<Expr>>, ctx: &mut CodeGenContext) {

    let var_type = get_type(_type, ctx);

    let decl = if let Some(box e) = expr {
        let initial_value = generate_expr(e, ctx);
        format!("{} {} = {};", var_type, name, initial_value)
    } else {
        format!("{} {};", var_type, name)
    };

    ctx.builder.push(decl);

}

fn generate_const_decl(name: String, _type: Type, expr: Expr, ctx: &CodeGenContext) {
    panic!("Not yet implemented");
}

fn generate_struct_decl(name: String, _type: Type, ctx: &mut CodeGenContext) {
    ctx.builder.push(format!("struct {} {{", name));
    if let Type::Struct(_, fields) = _type {
        for (field_name, field_type) in &fields {
            let type_string = get_type(field_type.clone(), ctx);
            ctx.builder.push(format!("{} {};", type_string, field_name));
        }
   } else {
        panic!("ICE: Passed non-struct type decl to 'generate_struct_decl'");
    }
    ctx.builder.push(format!("}}"));
}

fn generate_item(item: Item, ctx: &mut CodeGenContext) {
    use self::ItemKind::*;

    let name = item.name.clone();
    match item.node {
        FunctionDecl(box sig, block) => generate_function_decl(name, sig, block, ctx),
        VariableDecl(t, expr) => generate_variable_decl(name, t, expr, ctx),
        ConstDecl(t, box expr) => generate_const_decl(name, t, expr, ctx),
        StructDecl(t) => generate_struct_decl(name, t, ctx),
        Directive(_) => panic!("ICE: Directive item should have been processed before codegen"),
    }
}

fn generate_prelude(ctx: &mut CodeGenContext) {
    let prelude = vec![
        "typedef unsigned char u8;",
        "typedef unsigned short u16;",
        "typedef unsigned int u32;",
        "typedef unsigned long u64;",
        "typedef signed char s8;",
        "typedef short s16;",
        "typedef int s32;",
        "typedef long s64;",
        "typedef struct {",
        "u8* ptr;",
        "s32 len;",
        "} _paridae_slice;",
        "s32 len(_paridae_slice slice) {",
        "return slice.len;",
        "}",
    ];
    ctx.builder.push(prelude.join("\n"));
}

pub fn generate(ast: Vec<Item>) -> String {
    let mut ctx = CodeGenContext { builder: Vec::new() };

    generate_prelude(&mut ctx);

    for item in ast {
        generate_item(item, &mut ctx);
    }
    ctx.builder.join("\n")
}