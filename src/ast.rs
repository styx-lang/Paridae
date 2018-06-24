/*
 * Copyright (c) 2018 Lasse Dissing
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use std::fs::File;
use std::io::prelude::*;


#[derive(Debug)]
pub struct Item {
    pub name: String,
    pub node: ItemKind
}

#[derive(Debug)]
pub enum ItemKind {
    Const(Box<Type>, Box<Expr>),
    Function(Box<Prototype>, Box<Block>),
    Variable(Box<Type>, Box<Expr>),
}

#[derive(Debug)]
pub struct Prototype {
    pub inputs: Vec<(Box<Type>, String)>,
    pub output: Box<Type>
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
    Item(Box<Item>),
    Expr(Box<Expr>),
    Semi(Box<Expr>)
}

#[derive(Debug)]
pub struct Type {

}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperatorKind {
    Negation,
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

#[derive(Debug)]
pub struct Expr {
    pub node: ExprKind,
    pub t: Option<Type>
}

#[derive(Debug)]
pub enum LitKind {
    Str(String),
    Int(i64),
    Float(f64),
    Bool(bool)
}

#[derive(Debug)]
pub enum ExprKind {
    //Array ex: [1,2,3]
    Array(Vec<Box<Expr>>),
    //Assignment the result of the second expr to the result of the first expr
    //p[12] = 42 * 2;
    Assign(Box<Expr>, Box<Expr>),
    //Binary operator expression such as 12 * 42
    Binary(BinaryOperatorKind, Box<Expr>, Box<Expr>),
    Block(Box<Block>),
    Break,
    //Function call where the first expression resolves to the function
    //and the vector of expression resolves to each argument
    Call(Box<Expr>, Vec<Box<Expr>>),
    Continue,
    Identifier(String),
    //If condition with optional else clause
    // if expr1 expr2 else expr3
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    //Literal such as 12 or "hello"
    Literal(Box<LitKind>),
    //'return' an optional value from a block
    Return(Option<Box<Expr>>),
    //Unary operators such as negation or pointer dereferencing
    Unary(UnaryOperatorKind, Box<Expr>),
    // while expr1 expr2
    While(Box<Expr>, Box<Expr>),
}

fn visit_literal(lit: LitKind,  nodes: &mut Vec<String>) -> usize {
    use self::LitKind::*;

    let id = nodes.len();
    let literal = match lit {
        Int(i) => format!("{}", i),
        Float(f) => format!("{}", f),
        Str(s) => s.clone(),
        Bool(b) => format!("{}", b),
    };
    nodes.push(format!("n{} [label=\"{}\"];", id, literal));
    id
}

fn visit_unary_operator(bin_op: UnaryOperatorKind, inner: Expr, edges: &mut Vec<String>,  nodes: &mut Vec<String>) -> usize {
    let id = nodes.len();
    nodes.push(format!("n{} [label=\"{:?}\"];\n", id, bin_op));
    let inner_id = visit_expr(inner, edges, nodes);
    edges.push(format!("n{} -> n{};", id, inner_id));
    id
}

fn visit_binary_operator(bin_op: BinaryOperatorKind, left: Expr, right: Expr, edges: &mut Vec<String>,  nodes: &mut Vec<String>) -> usize {
    let id = nodes.len();
    nodes.push(format!("n{} [label=\"{:?}\"];\n", id, bin_op));
    let left_id = visit_expr(left, edges, nodes);
    let right_id = visit_expr(right, edges, nodes);
    edges.push(format!("n{} -> n{};", id, left_id));
    edges.push(format!("n{} -> n{};", id, right_id));
    id
}

fn visit_expr(expr: Expr, edges: &mut Vec<String>,  nodes: &mut Vec<String>) -> usize {
    use self::ExprKind::*;

    match expr.node {
        Unary(op, box inner) => visit_unary_operator(op, inner, edges, nodes),
        Binary(op, box left, box right) => visit_binary_operator(op, left, right, edges, nodes),
        Literal(box l) => visit_literal(l, nodes),
        _ => panic!("Other expression types not yet supported!"),
    }
}

pub fn dump_parse_tree(ast: Expr, filename: &str) {

    let mut edges = Vec::new();
    let mut nodes = Vec::new();

    visit_expr(ast, &mut edges, &mut nodes);

    let mut lines = Vec::new();
    let header = r#"
digraph parse_tree {
    node [shape=none, fontsize=12, fontname="Courier", height=.1];
    ranksep=.3;
    edge [arrowsize=.5]
    "#;

    let footer = "}";

    lines.push(header.to_string());

    for edge in edges {
        lines.push(edge);
    }

    for node in nodes {
        lines.push(node);
    }

    lines.push(footer.to_string());

    let content : String = lines.iter().map(|x| x.clone()).collect();
    let mut file = File::create(filename).expect(&format!("Unable to create file `{}`", filename));
    let _ = file.write_all(content.as_bytes()).expect("Unable to write parse tree file");
}