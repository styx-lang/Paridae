use ast::*;
use llvm::core::*;
use llvm::prelude::*;
use std::ffi::CStr;
use std::ptr;
use std::fs::File;

struct CodeGenContext {
    llvm_context: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
}

fn get_type(t: Type, ctx: CodeGenContext) -> LLVMTypeRef {
   match t.name.as_ref() {
       "int" => LLVMInt32TypeInContext(ctx.llvm_context),
       "i8" | "u8" => LLVMInt8TypeInContext(ctx.llvm_context),
       "i16" | "u16" => LLVMInt16TypeInContext(ctx.llvm_context),
       "i32" | "u32" => LLVMInt32TypeInContext(ctx.llvm_context),
       "i64" | "u32"=> LLVMInt64TypeInContext(ctx.llvm_context),
       "f32" => LLVMFloatTypeInContext(ctx.llvm_context),
       "f64" => LLVMDoubleTypeInContext(ctx.llvm_context),
   }
}

fn generate_function_decl(name: String, sig: Signature, block: Option<Box<Block>>, ctx: CodeGenContext) {
    let ret_type = if let Some(t) = sig.output {
        get_type(*t, ctx);
    } else {
        LLVMVoidTypeInContext(ctx.llvm_context);
    };
    let mut args = Vec::new();
    for (t, n) in sig.inputs {
        let arg_type = get_type(*t, ctx);
        args.push(arg_type);
    }

    let function_type =LLVMFunctionType(int_type, ptr::null_mut(), 0, 0);
    let function = LLVMAddFunction(module, b"_paridae_entry\0".as_ptr() as *const _,
                                                   function_type);
}

fn generate_variable_decl(name: String, sig: Signature, block: Option<Box<Block>>, ctx: CodeGenContext) {
    panic!("Not yet implemented");
}

fn generate_const_decl(name: String, sig: Signature, block: Option<Box<Block>>, ctx: CodeGenContext) {
    panic!("Not yet implemented");
}

fn generate_type_decl(name: String, sig: Signature, block: Option<Box<Block>>, ctx: CodeGenContext) {
    panic!("Not yet implemented");
}

fn generate_item(item: Item, builder: LLVMBuilderRef, context: LLVMContextRef) {
    use self::ItemKind::*;

    match item.node {
        FunctionDecl(box sig, block) => visit_function_decl(name, sig, block, edges, nodes),
        VariableDecl(t, box expr) => visit_variable_decl(name, t, expr, edges, nodes),
        ConstDecl(t, box expr) => visit_variable_decl(name, t, expr, edges, nodes),
        TypeDecl(box t) => visit_type_decl(name, t, edges, nodes),
    }
}

pub fn generate(ast: Vec<Item>) -> String {
    unsafe {
        // Set up a context, module and builder in that context.
        let context = LLVMContextCreate();
        let module = LLVMModuleCreateWithName(b"paridae\0".as_ptr() as *const _);
        let builder = LLVMCreateBuilderInContext(context);

        let int_type = LLVMInt32TypeInContext(context);
        let function_type =LLVMFunctionType(int_type, ptr::null_mut(), 0, 0);
        let function = LLVMAddFunction(module, b"_paridae_entry\0".as_ptr() as *const _,
                                                   function_type);
        // Create a basic block in the function and set our builder to generate
        // code in it.
        let bb = LLVMAppendBasicBlockInContext(context, function,
                                                           b"entry\0".as_ptr() as *const _);
        LLVMPositionBuilderAtEnd(builder, bb);

        // Emit a `ret 0` into the function
        let zero_value = LLVMConstInt(int_type, 12, 0);
        LLVMBuildRet(builder, zero_value);

        // Dump the module as IR to stdout.
        let res_buf = CStr::from_ptr(LLVMPrintModuleToString(module));
        let result = res_buf.to_str().unwrap();

        // Clean up. Values created in the context mostly get cleaned up there.
        LLVMDisposeBuilder(builder);
        LLVMDisposeModule(module);
        LLVMContextDispose(context);

        result.to_owned()
    }
}