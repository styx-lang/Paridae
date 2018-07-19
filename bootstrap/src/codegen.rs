/*
 * Copyright (c) 2018 Lasse Dissing
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use ast::*;
use llvm::*;
use llvm::core::*;
use llvm::prelude::*;
use std::ffi::{CStr, CString};
use std::collections::HashMap;

struct Scope {
    symbols: HashMap<String, LLVMValueRef>,
    parent: usize
}

struct AdtDef {
    fields: Vec<(String, Type)>,
    type_ref: LLVMTypeRef
}

struct CodeGenContext {
    llvm_context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    current_scope: usize,
    scope_arena: Vec<Scope>,
    types: HashMap<String, AdtDef>
}

fn initialize_scope(ctx: &mut CodeGenContext) {
    let new_scope = Scope { symbols: HashMap::new(), parent: ctx.current_scope } ;
    ctx.scope_arena.push(new_scope);
    ctx.current_scope = ctx.scope_arena.len()-1;
}

fn finalize_scope(ctx: &mut CodeGenContext) {
    ctx.current_scope = ctx.scope_arena[ctx.current_scope].parent;
}

fn declare_symbol(name: &String, val: LLVMValueRef, ctx: &mut CodeGenContext) {
    let mut current_scope = &mut ctx.scope_arena[ctx.current_scope].symbols;
    if current_scope.contains_key(name) {
        panic!("Redeclaring symbol {}", name);
    }
    current_scope.insert(name.clone(), val);
}

fn lookup_symbol(name: &String, ctx: &CodeGenContext) -> LLVMValueRef {
    let mut reached_top = false;

    let mut current = ctx.current_scope;
    while !reached_top {
        if current == 0 {
            reached_top = true;
        }
        let current_scope = &ctx.scope_arena[current];
        if let Some(val) = current_scope.symbols.get(name) {
            return *val;
        } else {
            current = current_scope.parent;
        }
    }
    panic!("Failed to find symbol {}", name);
}

unsafe fn get_slice_type(element_type: Type, ctx: &CodeGenContext) -> LLVMTypeRef {

    let element_ref = LLVMInt8TypeInContext(ctx.llvm_context);// get_type(element_type, ctx);
    let array_ref = LLVMArrayType(element_ref, 0);

    let mut llvm_fields = Vec::new();
    llvm_fields.push(array_ref);
    llvm_fields.push(LLVMInt32TypeInContext(ctx.llvm_context));

    LLVMStructType(llvm_fields.as_mut_ptr(), llvm_fields.len() as u32, 0)
}

unsafe fn get_type(t: Type, ctx: &CodeGenContext) -> LLVMTypeRef {
    use self::Type::*;
    use self::IntegerSize::*;
    use self::FloatingSize::*;

    match t {
        Signed(I8) | Unsigned(I8) => LLVMInt8TypeInContext(ctx.llvm_context),
        Signed(I16) | Unsigned(I16) => LLVMInt16TypeInContext(ctx.llvm_context),
        Signed(I32) | Unsigned(I32) => LLVMInt32TypeInContext(ctx.llvm_context),
        Signed(I64) | Unsigned(I64) => LLVMInt64TypeInContext(ctx.llvm_context),
        Signed(Arch) | Unsigned(Arch) => LLVMInt64TypeInContext(ctx.llvm_context),
        Float(F32) => LLVMFloatTypeInContext(ctx.llvm_context),
        Float(F64) => LLVMDoubleTypeInContext(ctx.llvm_context),
        Ptr(box inner) => {
            let llvm_inner = get_type(inner, ctx);
            LLVMPointerType(llvm_inner, 0)
        }
        Struct(name, _) => {
            if let Some(adt) = ctx.types.get(&name) {
                adt.type_ref
            } else {
                panic!("Unknown struct type {:?}", name)
            }
        },
        Slice(box inner) => get_slice_type(inner, ctx),
        other => panic!("CodeGen does not support {:?} yet", other)
   }
}

unsafe fn generate_int_literal(val: i64, t: Type, ctx: &CodeGenContext) -> LLVMValueRef {
    LLVMConstInt(get_type(t, ctx), val as u64, 0)
}

unsafe fn generate_literal(lit: LitKind, t: Type, ctx: &CodeGenContext) -> LLVMValueRef {
    use self::LitKind::*;

    match lit {
        Int(i) => generate_int_literal(i, t, ctx),
        _ => panic!("Literal type not yet supported!")
    }
}

unsafe fn generate_identifier(ident: String, ctx: &CodeGenContext) -> LLVMValueRef {
    lookup_symbol(&ident, ctx)
}

unsafe fn generate_unary_operator(op: UnaryOperatorKind, inner: Expr, ctx: &mut CodeGenContext) -> LLVMValueRef {
    let inner_val = generate_expr(inner.clone(), ctx);
    match op {
        UnaryOperatorKind::Deref => {
            LLVMBuildLoad(ctx.builder, inner_val, b"ptr_deref_tmp\0".as_ptr() as *const _)
        },
        UnaryOperatorKind::Refer => {
            inner_val
        },
        _ => panic!("Unary operator {:?} not yet implemented", op),
    }
}

unsafe fn generate_binary_operator(bin_op: BinaryOperatorKind, left: Expr, right: Expr, ctx: &mut CodeGenContext) -> LLVMValueRef {
    use self::BinaryOperatorKind::*;

    let mut lhs = generate_expr(left, ctx);
    let mut rhs = generate_expr(right, ctx);

    if LLVMGetTypeKind(LLVMTypeOf(lhs)) == LLVMTypeKind::LLVMPointerTypeKind {
        lhs = LLVMBuildLoad(ctx.builder, lhs, b"ptr_tmp\0".as_ptr() as *const _);
    }

    if LLVMGetTypeKind(LLVMTypeOf(rhs)) == LLVMTypeKind::LLVMPointerTypeKind {
        rhs = LLVMBuildLoad(ctx.builder, rhs, b"ptr_tmp\0".as_ptr() as *const _);
    }


    match bin_op {
        Addition => LLVMBuildAdd(ctx.builder, lhs, rhs, b"add_tmp\0".as_ptr() as *const _),
        Subtraction => LLVMBuildSub(ctx.builder, lhs, rhs, b"sub_tmp\0".as_ptr() as *const _),
        Product => LLVMBuildMul(ctx.builder, lhs, rhs, b"mul_tmp\0".as_ptr() as *const _),
        Division => LLVMBuildSDiv(ctx.builder, lhs, rhs, b"div_tmp\0".as_ptr() as *const _),
        Equality => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntEQ, lhs, rhs, b"cmp_tmp\0".as_ptr() as *const _),
        NotEq => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntNE, lhs, rhs, b"cmp_tmp\0".as_ptr() as *const _),
        Less => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSLT, lhs, rhs, b"cmp_tmp\0".as_ptr() as *const _),
        LessEq => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSLE, lhs, rhs, b"cmp_tmp\0".as_ptr() as *const _),
        Greater => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSGT, lhs, rhs, b"cmp_tmp\0".as_ptr() as *const _),
        GreaterEq => LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntSGE, lhs, rhs, b"cmp_tmp\0".as_ptr() as *const _),
    }
}

unsafe fn generate_call(fun: Expr, args: Vec<Box<Expr>>, ctx: &mut CodeGenContext) -> LLVMValueRef {
    let name = if let ExprKind::Identifier(s) = fun.node {
        s
    } else {
        panic!("Higher order functions not yet supported!");
    };
    let function = LLVMGetNamedFunction(ctx.module, name.as_bytes().as_ptr() as *const _); //name.as_bytes()
    if function as usize == 0x0 {
        panic!("Failed to find function {}", name);
    }
    let expected_count = LLVMCountParams(function) as usize;
    if args.len() != expected_count {
        panic!("Expected {} arguments in function call to {} but got {}", expected_count, name, args.len());
    }
    let mut arg_values = Vec::new();

    for box arg in args {
        let is_pointer = if let Type::Ptr(_) = arg.t.clone() {
            true
        } else {
            false
        };
        let mut val = generate_expr(arg, ctx);
        if !is_pointer && LLVMGetTypeKind(LLVMTypeOf(val)) == LLVMTypeKind::LLVMPointerTypeKind {
            val = LLVMBuildLoad(ctx.builder, val, b"ptr_tmp\0".as_ptr() as *const _);
        }
        arg_values.push(val);
    }
    if name != "len" && LLVMGetTypeKind(LLVMGetReturnType(LLVMGetReturnType(LLVMTypeOf(function)))) == LLVMTypeKind::LLVMVoidTypeKind{
        LLVMBuildCall(ctx.builder, function, arg_values.as_mut_ptr(), arg_values.len() as u32, b"\0".as_ptr() as *const _)
    } else {
        LLVMBuildCall(ctx.builder, function, arg_values.as_mut_ptr(), arg_values.len() as u32, b"call_tmp\0".as_ptr() as *const _)
    }
}

unsafe fn generate_condition(condition: Expr, then: Block, otherwise: Option<Box<Block>>, ctx: &mut CodeGenContext) -> LLVMValueRef {
    let condition_value = generate_expr(condition, ctx);

    let bot = LLVMConstInt(LLVMInt1TypeInContext(ctx.llvm_context), 0, 0);
    let branch_flag = LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntNE, condition_value, bot, b"ifcond\0".as_ptr() as *const _);

    let parent_function = LLVMGetBasicBlockParent(LLVMGetInsertBlock(ctx.builder));

    let mut then_bb = LLVMAppendBasicBlockInContext(ctx.llvm_context, parent_function, b"then\0".as_ptr() as *const _);

    if let Some(box o) = otherwise {
        let mut else_bb = LLVMAppendBasicBlockInContext(ctx.llvm_context, parent_function, b"else\0".as_ptr() as *const _);
        let merge_bb = LLVMAppendBasicBlockInContext(ctx.llvm_context, parent_function, b"ifcont\0".as_ptr() as *const _);
        LLVMBuildCondBr(ctx.builder, branch_flag, then_bb, else_bb);
        LLVMPositionBuilderAtEnd(ctx.builder, then_bb);
        initialize_scope(ctx);
        let then_val = generate_block(then, ctx);
        LLVMBuildBr(ctx.builder, merge_bb);
        then_bb = LLVMGetInsertBlock(ctx.builder);
        finalize_scope(ctx);
        initialize_scope(ctx);
        LLVMPositionBuilderAtEnd(ctx.builder, else_bb);
        let otherwise_val = generate_block(o, ctx);
        LLVMBuildBr(ctx.builder, merge_bb);
        else_bb = LLVMGetInsertBlock(ctx.builder);
        finalize_scope(ctx);
        LLVMPositionBuilderAtEnd(ctx.builder, merge_bb);

        LLVMBuildIntCast(ctx.builder, condition_value, LLVMInt32TypeInContext(ctx.llvm_context), b"ret_cast\0".as_ptr() as *const _)
    } else {
        panic!("Stand alone ifs not yet supported");
    }
}

unsafe fn determine_field_index(field_name: &String, struct_type: Type) -> u32 {
    if let Type::Struct(struct_name, fields) = struct_type {
        for (idx, (n, _)) in fields.iter().enumerate() {
            if n == field_name {
                return idx as u32;
            }
        }
        panic!("ICE: Failed to find field {} in type {:?}", field_name, struct_name);
    } else {
        panic!("ICE: Tried to generate field access on type {:?}", struct_type);
    }
}


unsafe fn generate_gep(owner: Expr, field_name: String, ctx: &mut CodeGenContext) -> LLVMValueRef {
    let owner_type = owner.t.clone();

    let mut base_ptr = generate_expr(owner, ctx);

    let idx = if let Type::Ptr(box inner) = owner_type {
        base_ptr = LLVMBuildLoad(ctx.builder, base_ptr, b"struct_ptr_deref\0".as_ptr() as *const _);
        determine_field_index(&field_name, inner)
    } else {
        determine_field_index(&field_name, owner_type)
    };

    LLVMBuildStructGEP(ctx.builder, base_ptr, idx, field_name.as_bytes().as_ptr() as *const _)
}

unsafe fn generate_field_access(owner: Expr, field_name: String, ctx: &mut CodeGenContext) -> LLVMValueRef {

    let ptr = generate_gep(owner, field_name.clone(), ctx);

    LLVMBuildLoad(ctx.builder, ptr, field_name.as_bytes().as_ptr() as *const _)
}


unsafe fn generate_array_index(array: Expr, index: Expr, ctx: &mut CodeGenContext) -> LLVMValueRef {
    let ptr = generate_expr(array, ctx);
    let offset = generate_expr(index, ctx);

    unimplemented!();
}

unsafe fn generate_expr(expr: Expr, ctx: &mut CodeGenContext) -> LLVMValueRef {
    use self::ExprKind::*;
    
    match expr.node {
        Unary(op, box inner) => generate_unary_operator(op, inner, ctx),
        Binary(op, box lhs, box rhs) => generate_binary_operator(op, lhs, rhs, ctx),
        Literal(box lit) => generate_literal(lit, expr.t, ctx),
        Identifier(s) => generate_identifier(s, ctx),
        Call(box fun, args) => generate_call(fun, args, ctx),
        If(box condition, box then, otherwise) => generate_condition(condition, then, otherwise, ctx),
        Member(box owner, field_name) => generate_field_access(owner, field_name, ctx),
        Index(box array, box index) => generate_array_index(array, index, ctx),
    }
}

unsafe fn generate_return(e: Expr, ctx: &mut CodeGenContext) {
    let mut val = generate_expr(e, ctx);
    if LLVMGetTypeKind(LLVMTypeOf(val)) == LLVMTypeKind::LLVMPointerTypeKind {
            val = LLVMBuildLoad(ctx.builder, val, b"ptr_tmp\0".as_ptr() as *const _);
        }
    LLVMBuildRet(ctx.builder, val);
}

unsafe fn generate_while(condition: Expr, block: Block, ctx: &mut CodeGenContext) {
    let parent_function = LLVMGetBasicBlockParent(LLVMGetInsertBlock(ctx.builder));
    let mut condition_bb = LLVMAppendBasicBlockInContext(ctx.llvm_context, parent_function, b"loop_condition\0".as_ptr() as *const _);
    let mut action_bb = LLVMAppendBasicBlockInContext(ctx.llvm_context, parent_function, b"loop_action\0".as_ptr() as *const _);
    let mut done_bb = LLVMAppendBasicBlockInContext(ctx.llvm_context, parent_function, b"loop_done\0".as_ptr() as *const _);
    LLVMBuildBr(ctx.builder, condition_bb);
    LLVMPositionBuilderAtEnd(ctx.builder, condition_bb);
    let condition_value = generate_expr(condition, ctx);
    let bot = LLVMConstInt(LLVMInt1TypeInContext(ctx.llvm_context), 0, 0);
    let branch_flag = LLVMBuildICmp(ctx.builder, LLVMIntPredicate::LLVMIntNE, condition_value, bot,b"whilecond\0".as_ptr() as *const _);
    LLVMBuildCondBr(ctx.builder, branch_flag, action_bb, done_bb);
    LLVMPositionBuilderAtEnd(ctx.builder, action_bb);
    initialize_scope(ctx);
    generate_block(block, ctx);
    finalize_scope(ctx);
    LLVMBuildBr(ctx.builder, condition_bb);
    LLVMPositionBuilderAtEnd(ctx.builder, done_bb);


}

unsafe fn generate_assignment(place: Expr, value: Expr, ctx: &mut CodeGenContext) {
    let res_val = generate_expr(value, ctx);

    match place.node {
        ExprKind::Identifier(ident) => {
            let place_ptr = lookup_symbol(&ident, ctx);
            LLVMBuildStore(ctx.builder, res_val, place_ptr);
        },
        ExprKind::Unary(UnaryOperatorKind::Deref, box inner) => {
            if let ExprKind::Identifier(ident) = inner.node {
                let place_ptr = lookup_symbol(&ident, ctx);
                let place_tmp = LLVMBuildLoad(ctx.builder, place_ptr, b"ptr_tmp\0".as_ptr() as *const _);
                LLVMBuildStore(ctx.builder, res_val, place_tmp);
            } else {
                panic!("Can only assign to simple pointers for now {:?}", inner);
            }
        },
        ExprKind::Member(box owner, field_name) => {

            let ptr = generate_gep(owner, field_name.clone(), ctx);
            LLVMBuildStore(ctx.builder, res_val, ptr);
        }
        _ => panic!("Currently does not supports assigning to place {:?}", place),
    }
}

unsafe fn generate_stmt(stmt: Stmt, ctx: &mut CodeGenContext) {
    use self::StmtKind::*;

    match stmt.node {
        Return(box e) => generate_return(e, ctx),
        Item(box i) => generate_item(i, ctx),
        Expr(box e) => {
            let _ = generate_expr(e, ctx);
        },
        While(box c, box b) => generate_while(c, b, ctx),
        Assignment(box place, box value) => generate_assignment(place, value, ctx),
        _ => panic!("Other statement kinds not yet supported {:?}", stmt.node),
    }
}

unsafe fn generate_block(block: Block, ctx: &mut CodeGenContext) {
    for stmt in block.stmts {
        generate_stmt(stmt, ctx);
    }
}

unsafe fn generate_function_decl(name: String, sig: Signature, block: Option<Box<Block>>, ctx: &mut CodeGenContext) {
    let ret_type = if sig.output != Type::Void {
        get_type(sig.output, ctx)
    } else {
        LLVMVoidTypeInContext(ctx.llvm_context)
    };
    let mut args = Vec::new();
    for (t, n) in &sig.inputs {
        let arg_type = get_type(t.clone(), ctx);
        args.push(arg_type);
    }

    let function_type =LLVMFunctionType(ret_type, args.as_mut_ptr(), args.len() as u32, 0);
    let function_name = CString::new(name.as_bytes()).unwrap();
    let function = LLVMAddFunction(ctx.module, function_name.as_ptr() as *const _,
                                                   function_type);
    if let Some(b) = block {
        initialize_scope(ctx);
        let bb = LLVMAppendBasicBlockInContext(ctx.llvm_context, function, b"entry\0".as_ptr() as *const _);
        LLVMPositionBuilderAtEnd(ctx.builder, bb);
        for (idx, (t, n)) in sig.inputs.iter().enumerate() {
            let arg_type = get_type(t.clone(), ctx);
            let allocation = LLVMBuildAlloca(ctx.builder, arg_type, n.as_bytes().as_ptr() as *const _);
            LLVMBuildStore(ctx.builder, LLVMGetParam(function, idx as u32), allocation);
            declare_symbol(&n, allocation, ctx);
        }
        generate_block(*b, ctx);
        finalize_scope(ctx);
        LLVMBuildUnreachable(ctx.builder);
    }
}

unsafe fn generate_variable_decl(name: String, _type: Type, expr: Option<Box<Expr>>, ctx: &mut CodeGenContext) {
    let variable_name = CString::new(name.as_bytes()).unwrap();
    let allocation = LLVMBuildAlloca(ctx.builder, get_type(_type.clone(), ctx), variable_name.as_ptr() as *const _);

    if let Some(box e) = expr {
        let mut initial_value = generate_expr(e, ctx);
        match _type {
            Type::Ptr(_) => {},
            _ => {
                if LLVMGetTypeKind(LLVMTypeOf(initial_value)) == LLVMTypeKind::LLVMPointerTypeKind {
                    initial_value = LLVMBuildLoad(ctx.builder, initial_value, b"ptr_tmp\0".as_ptr() as *const _);
                }
            }
        }

        let store = LLVMBuildStore(ctx.builder, initial_value, allocation);
    }

    declare_symbol(&name, allocation, ctx);

}

unsafe fn generate_const_decl(name: String, _type: Type, expr: Expr, ctx: &CodeGenContext) {
    panic!("Not yet implemented");
}

unsafe fn generate_struct_decl(name: String, _type: Type, ctx: &mut CodeGenContext) {
    let type_ref = LLVMStructCreateNamed(ctx.llvm_context, name.as_bytes().as_ptr() as *const _);
    let mut llvm_fields = Vec::new();
    if let Type::Struct(_, fields) = _type {
        for (_, field_type) in &fields {
            llvm_fields.push(get_type(field_type.clone(), ctx));
        }
        LLVMStructSetBody(type_ref, llvm_fields.as_mut_ptr(), llvm_fields.len() as u32, 0);
        ctx.types.insert(name, AdtDef { fields, type_ref});
    } else {
        panic!("ICE: Passed non-struct type decl to 'generate_struct_decl'");
    }
}

unsafe fn generate_item(item: Item, ctx: &mut CodeGenContext) {
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

unsafe fn add_builtin(ctx: &mut CodeGenContext) {
    let slice_type = LLVMPointerType(get_slice_type(Type::Void, ctx), 0);
    let function_type = LLVMFunctionType(LLVMInt32TypeInContext(ctx.llvm_context), vec![slice_type].as_mut_ptr(), 1, 0);
    let function = LLVMAddFunction(ctx.module, b"len\0".as_ptr() as *const _, function_type);
    let bb = LLVMAppendBasicBlockInContext(ctx.llvm_context, function, b"entry\0".as_ptr() as *const _);
    LLVMPositionBuilderAtEnd(ctx.builder, bb);

    let param = LLVMGetParam(function, 0);

    let zero = LLVMConstInt(LLVMInt32TypeInContext(ctx.llvm_context), 0, 0);
    let len_ptr = LLVMBuildStructGEP(ctx.builder, param, 1, b"len_deref\0".as_ptr() as *const _);
    let len = LLVMBuildLoad(ctx.builder, len_ptr, b"len_ptr\0".as_ptr() as *const _);
    LLVMBuildRet(ctx.builder, len);
}

pub unsafe fn generate(ast: Vec<Item>) -> String {
    let llvm_context = LLVMContextCreate();
    let module = LLVMModuleCreateWithName(b"paridae\0".as_ptr() as *const _);
    let builder = LLVMCreateBuilderInContext(llvm_context);

    let mut global_scope = Scope { symbols: HashMap::new(), parent: 0 };
    let mut ctx = CodeGenContext { llvm_context, module, builder, scope_arena: vec![global_scope], current_scope: 0 , types: HashMap::new()};

    add_builtin(&mut ctx);

    for item in ast {
        generate_item(item, &mut ctx);
    }

    let res_buf = CStr::from_ptr(LLVMPrintModuleToString(module));
    let result = res_buf.to_str().unwrap();

    LLVMDisposeBuilder(builder);
    LLVMDisposeModule(module);
    LLVMContextDispose(llvm_context);

    result.to_owned()
}