/*
 * Copyright (c) 2018 Lasse Dissing
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */


#[derive(Debug, Clone)]
pub struct Item {
    pub name: String,
    pub node: ItemKind,
    pub line: usize,
}

#[derive(Debug, Clone)]
pub enum ItemKind {
    ConstDecl(Type, Box<Expr>),
    FunctionDecl(Box<Signature>, Option<Box<Block>>),
    VariableDecl(Type, Option<Box<Expr>>),
    Directive(DirectiveKind),
    StructDecl(Type)
}

#[derive(Debug, Clone)]
pub enum DirectiveKind {
    Include(String),
}

#[derive(Debug, Clone)]
pub struct Signature {
    pub inputs: Vec<(Type, String)>,
    pub output: Type
}

impl ToString for Signature {
    fn to_string(&self) -> String {
        let mut parts = Vec::new();
        parts.push(String::from("("));
        for (_, name) in &self.inputs {
            parts.push(format!("{}", name));
            parts.push(String::from(", "));
        }
        let len = parts.len();
        if len > 1 {
            parts[len - 1] = String::from(")");
        } else {
            parts.push(String::from(")"));
        }

        parts.iter().map(|x| x.clone()).collect()
    }
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub node: StmtKind
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    //Assignment the result of the second expr to the result of the first expr
    //p[12] = 42 * 2;
    Assignment(Box<Expr>, Box<Expr>),
    Item(Box<Item>),
    Expr(Box<Expr>),
    Return(Box<Expr>),
    Break,
    Continue,
    Defer(Box<Expr>),
    While(Box<Expr>, Box<Block>),
    Empty,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IntegerSize {
    I8,
    I16,
    I32,
    I64,
    Arch,
    Unspecified
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FloatingSize {
    F32,
    F64,
    Unspecified
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Bool,
    Signed(IntegerSize),
    Unsigned(IntegerSize),
    Float(FloatingSize),
    Char,
    Array(Box<Type>, usize),
    Slice(Box<Type>),
    Ptr(Box<Type>),
    Void,
    Function(Vec<Type>, Box<Type>),
    Struct(String, Vec<(String,Type)>),
    Infer,
}

impl ToString for Type {
    fn to_string(&self) -> String {
        format!("{:?}", self)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperatorKind {
    // The "*" operator for dereferencing pointers
    Deref,
    // The "&" operator for getting the address of a place
    Refer,
    //The numerical negation "-"
    Negation,
    //The logical not operation "!"
    Complement,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperatorKind {
    Addition,
    Subtraction,
    Product,
    Division,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Equality,
    NotEq,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub node: ExprKind,
    pub t: Type
}

#[derive(Debug, Clone)]
pub enum LitKind {
    Str(String),
    Int(i64),
    Float(f64),
    Bool(bool)
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    //Binary operator expression such as 12 * 42
    Binary(BinaryOperatorKind, Box<Expr>, Box<Expr>),
    //Function call where the first expression resolves to the function
    //and the vector of expression resolves to each argument
    Call(Box<Expr>, Vec<Box<Expr>>),
    Identifier(String),
    //If condition with optional else clause
    // if expr block1 [else block2]
    If(Box<Expr>, Box<Block>, Option<Box<Block>>),
    //Literal such as 12 or "hello"
    Literal(Box<LitKind>),
    //Field access in struct "a.b"
    Member(Box<Expr>, String),
    //Unary operators such as negation or pointer dereferencing
    Unary(UnaryOperatorKind, Box<Expr>),
}

