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
        Str(s) => format!("\"{}\"", s), // TODO Stick with c strings for now
        Bool(b) => if b {String::from("1")} else {String::from("0")},
        Float(f) => format!("{}", f),
        _ => panic!("Literal type {:?} not yet supported!", t)
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

fn get_float_size_postfix(size: FloatingSize) -> String {
    use self::FloatingSize::*;

    match size {
        F32 => "32",
        F64 => "64",
        Unspecified => "64",
    }.to_string()
}

fn get_type(t: Type, ctx: &CodeGenContext) -> String {
    match t {
        Type::Void => "void".to_string(),
        Type::Signed(size) => format!("s{}", get_int_size_postfix(size)),
        Type::Float(size) => format!("f{}", get_float_size_postfix(size)),
        Type::Unsigned(size) => format!("u{}", get_int_size_postfix(size)),
        Type::Bool => "int".to_string(),
        Type::Ptr(box inner) => format!("{}*", get_type(inner, ctx)),
        Type::Slice(box inner) => "struct _paridae_slice".to_string(),
        Type::Struct(name, _) => format!("struct {}", name),
        Type::Union(name, _) => format!("union {}", name),
        Type::Enum(name, _) => format!("enum {}", name),
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
        UnaryOperatorKind::Complement => {
            format!("!{}", inner_val)
        },
        UnaryOperatorKind::Negation => {
            format!("-{}", inner_val)
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
        Modulus => format!("({} % {})", lhs, rhs),
        LeftShift => format!("({} << {})", lhs, rhs),
        RightShift => format!("({} >> {})", lhs, rhs),
        Xor => format!("({} ^ {})", lhs, rhs),
        Equality => format!("({} == {})", lhs, rhs),
        NotEq => format!("({} != {})", lhs, rhs),
        Less => format!("({} < {})", lhs, rhs),
        LessEq => format!("({} <= {})", lhs, rhs),
        Greater => format!("({} > {})", lhs, rhs),
        GreaterEq => format!("({} >= {})", lhs, rhs),
        And => format!("({} && {})", lhs, rhs),
        Or => format!("({} || {})", lhs, rhs),
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
        ctx.builder.push(String::from("else"));
        generate_block(o, ctx);
    }
    String::from("")
}


fn generate_field_access(owner: Expr, field_name: String, ctx: &mut CodeGenContext) -> String {

    let owner_type = owner.t.clone();
    let owner_expr = generate_expr(owner, ctx);

    if let Type::Ptr(_) = owner_type {
        format!("{}->{}", owner_expr, field_name)
    } else {
        format!("{}.{}", owner_expr, field_name)
    }
}


fn generate_array_index(array: Expr, index: Expr, ctx: &mut CodeGenContext) -> String {
    let array_type = array.t.clone();
    let ptr = generate_expr(array, ctx);
    let offset = generate_expr(index, ctx);

    if let Type::Ptr(_) = array_type {
        format!("{}[{}]", ptr, offset)
    } else if let Type::Slice(box inner) = array_type {
        let inner_str = get_type(inner, ctx);
        format!("*(({}*) &({}.ptr[{}*sizeof({})]))", inner_str , ptr, offset, inner_str)
    } else {
        panic!("Indexing into {:?} not implemented", array_type);
    }

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

    let place_result = generate_expr(place, ctx);

    ctx.builder.push(format!("{} = {};", place_result, res_val));
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
        Break => ctx.builder.push(String::from("break;")),
        Continue => ctx.builder.push(String::from("continue;")),
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
    ctx.builder.push(format!("struct {}", name));
    if let Type::Struct(_, fields) = _type {
        if !fields.is_empty() {
            ctx.builder.push(String::from("{"));
            for (field_name, field_type) in &fields {
                let type_string = get_type(field_type.clone(), ctx);
                ctx.builder.push(format!("{} {};", type_string, field_name));
            }
            ctx.builder.push(String::from("};"));
        } else {
            ctx.builder.push(String::from(";"));
        }

   } else {
        panic!("ICE: Passed non-struct type decl to 'generate_struct_decl'");
    }
}

fn generate_union_decl(name: String, _type: Type, ctx: &mut CodeGenContext) {
    ctx.builder.push(format!("union {} {{", name));
    if let Type::Union(_, fields) = _type {
        for (field_name, field_type) in &fields {
            let type_string = get_type(field_type.clone(), ctx);
            ctx.builder.push(format!("{} {};", type_string, field_name));
        }
   } else {
        panic!("ICE: Passed non-union type decl to 'generate_union_decl'");
    }
    ctx.builder.push(String::from("};"));
}

fn generate_enum_decl(name: String, _type: Type, ctx: &mut CodeGenContext) {
    ctx.builder.push(format!("enum {} {{", name));
    if let Type::Enum(_, variants) = _type {
        for variant_name in &variants {
            ctx.builder.push(format!("{},", variant_name));
        }
   } else {
        panic!("ICE: Passed non-enum type decl to 'generate_enum_decl'");
    }
    ctx.builder.push(String::from("};"));
}

fn generate_item(item: Item, ctx: &mut CodeGenContext) {
    use self::ItemKind::*;

    let name = item.name.clone();
    match item.node {
        FunctionDecl(box sig, block) => generate_function_decl(name, sig, block, ctx),
        VariableDecl(t, expr) => generate_variable_decl(name, t, expr, ctx),
        ConstDecl(t, box expr) => generate_const_decl(name, t, expr, ctx),
        StructDecl(t) => generate_struct_decl(name, t, ctx),
        UnionDecl(t) => generate_union_decl(name, t, ctx),
        EnumDecl(t) => generate_enum_decl(name, t, ctx),
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
        "typedef float f32;",
        "typedef double f64;",
        "int printf(const char* format, ...);",
        "int fprintf(void* fp, const char* format, ...);",
        "int sprintf(char* buffer, const char* format, ...);",
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
