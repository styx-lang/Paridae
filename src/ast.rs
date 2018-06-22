/*
 * Copyright (c) 2018 Lasse Dissing
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

// Use a frozen immutable wrapper around box as used by the Rust compiler itself
#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct P<T: ?Sized> {
    ptr: Box<T>
}

pub fn P<T: 'static>(val: T) -> P<T> {
    P { ptr: Box::new(val) }
}

#[derive(Debug)]
pub struct Decl {
    pub name: String,
    pub node: DeclKind
}

#[derive(Debug)]
pub enum DeclKind {
    Extern(P<Prototype>),
    Const(P<Type>, P<Expr>),
    Function(P<Prototype>, P<Block>),
    Variable(P<Type>, P<Expr>),
}

#[derive(Debug)]
pub struct Prototype {
    pub inputs: Vec<(P<Type>, String)>,
    pub output: P<Type>
}

#[derive(Debug)]
pub struct Stmt {
    pub node: StmtKind
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>
}

#[derive(Debug)]
pub enum StmtKind {
    Decl(P<Decl>),
    Expr(P<Expr>),
    Semi(P<Expr>)
}

#[derive(Debug)]
pub struct Type {

}

#[derive(Debug)]
pub enum UnaryOperatorKind {
    Negation,
    Complement,
}

#[derive(Debug)]
pub enum BinaryOperatorKind {
    Plus,
    Minus,
    Multiply,
    Divide,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Equality,
}

#[derive(Debug)]
pub struct Expr {
    pub node: ExprKind,
    pub t: Type
}

#[derive(Debug)]
pub enum ExprKind {
    //Array ex: [1,2,3]
    Array(Vec<P<Expr>>),
    //Assignment the result of the second expr to the result of the first expr
    //p[12] = 42 * 2;
    Assign(P<Expr>, P<Expr>),
    //Binary operator expression such as 12 * 42
    Binary(BinaryOperatorKind, P<Expr>, P<Expr>),
    Block(P<Block>),
    Break,
    //Function call where the first expression resolves to the function
    //and the vector of expression resolves to each argument
    Call(P<Expr>, Vec<P<Expr>>),
    Continue,
    //If condition with optional else clause
    // if expr1 expr2 else expr3
    If(P<Expr>, P<Expr>, Option<P<Expr>>),
    //Literal such as 12 or "hello"
    Literal(P<Expr>),
    //'return' an optional value from a block
    Return(Option<P<Expr>>),
    //Unary operators such as negation or pointer dereferencing
    Unary(UnaryOperatorKind, P<Expr>),
    // while expr1 expr2
    While(P<Expr>, P<Expr>),
}
