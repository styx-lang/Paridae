/*
 * Copyright (c) 2018 Lasse Dissing
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use std::fs::File;
use std::io::prelude::*;

use ast::*;

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
    edges.push(format!("n{} -> n{};\n", id, inner_id));
    id
}

fn visit_binary_operator(bin_op: BinaryOperatorKind, left: Expr, right: Expr, edges: &mut Vec<String>,  nodes: &mut Vec<String>) -> usize {
    let id = nodes.len();
    nodes.push(format!("n{} [label=\"{:?}\"];\n", id, bin_op));
    let left_id = visit_expr(left, edges, nodes);
    let right_id = visit_expr(right, edges, nodes);
    edges.push(format!("n{} -> n{};\n", id, left_id));
    edges.push(format!("n{} -> n{};\n", id, right_id));
    id
}

fn visit_identifier(identifier: String, nodes:&mut Vec<String>) -> usize {
    let id = nodes.len();
    nodes.push(format!("n{} [label=\"{}\"];\n", id, identifier));
    id
}

fn visit_conditional(condition: Expr, b1: Block, b2: Option<Box<Block>>, edges: &mut Vec<String>,  nodes: &mut Vec<String> ) -> usize {
    let id = nodes.len();
    nodes.push(format!("n{} [label=\"if\"];\n", id));
    let condition_id = visit_expr(condition, edges, nodes);
    edges.push(format!("n{} -> n{};\n", id, condition_id));
    let then_id = nodes.len();
    nodes.push(format!("n{} [label=\"then\"];\n", then_id));
    let _ = visit_block(b1, edges, nodes, then_id);
    edges.push(format!("n{} -> n{};\n", id, then_id));
    if let Some(box inner) = b2 {
        let otherwise_id = nodes.len();
        nodes.push(format!("n{} [label=\"otherwise\"];\n", otherwise_id));
        let _ = visit_block(inner, edges, nodes, otherwise_id);
        edges.push(format!("n{} -> n{};\n", id, otherwise_id));
    }
    id
}

fn visit_call(fn_expr: Expr, args: Vec<Box<Expr>>, edges: &mut Vec<String>,  nodes: &mut Vec<String> ) -> usize {
    let id = nodes.len();
    match fn_expr.node.clone() {
        ExprKind::Identifier(s) => nodes.push(format!("n{} [label=\"{}\"];\n", id, s)),
        e => {
            nodes.push(format!("n{} [label=\"func\"];\n", id));
            let child_id = visit_expr(fn_expr, edges, nodes);
            edges.push(format!("n{} -> n{};\n", id, child_id));
        },
    }
    for arg in &args {
        let child_id = visit_expr(*(arg.clone()), edges, nodes);
        edges.push(format!("n{} -> n{};\n", id, child_id));
    }
    id
}

fn visit_expr(expr: Expr, edges: &mut Vec<String>,  nodes: &mut Vec<String>) -> usize {
    use self::ExprKind::*;

    match expr.node {
        Unary(op, box inner) => visit_unary_operator(op, inner, edges, nodes),
        Binary(op, box left, box right) => visit_binary_operator(op, left, right, edges, nodes),
        Literal(box l) => visit_literal(l, nodes),
        Identifier(s) => visit_identifier(s, nodes),
        If(box e, box b1, b2) => visit_conditional(e, b1, b2, edges, nodes),
        Call(box e, args) => visit_call(e, args, edges, nodes),
        _ => panic!("Other expression types not yet supported! {:?}", expr.node),
    }

}

fn visit_assignment(left: Expr, right: Expr, edges: &mut Vec<String>,  nodes: &mut Vec<String>) -> usize {
    panic!("Not yet implemented");
}

fn visit_keyword(name: &str, edges: &mut Vec<String>,  nodes: &mut Vec<String>) -> usize {
    panic!("Not yet implemented");
}

fn visit_keyword_expr(name: &str, expr: Expr, edges: &mut Vec<String>,  nodes: &mut Vec<String>) -> usize {
    panic!("Not yet implemented");
}

fn visit_while(condition: Expr, block: Block, edges: &mut Vec<String>,  nodes: &mut Vec<String>) -> usize {
    panic!("Not yet implemented");
}

fn visit_stmt(stmt: Stmt, edges: &mut Vec<String>,  nodes: &mut Vec<String>, root: usize) -> usize {
    use self::StmtKind::*;

    let leaf_id = match stmt.node {
        Assignment(box lhs, box rhs) => visit_assignment(lhs, rhs, edges, nodes),
        Expr(box e) => visit_expr(e, edges, nodes),
        Item(box i) => visit_item(i, edges, nodes),
        Return(box e) => visit_keyword_expr("Return", e, edges, nodes),
        Defer(box e) => visit_keyword_expr("Defer", e, edges, nodes),
        Break => visit_keyword("Break", edges, nodes),
        Empty => root,
        Continue => visit_keyword("Continue", edges, nodes),
        While(box condition, box block) => visit_while(condition, block, edges, nodes),
        _ => panic!("Other statement types not yet supported! {:?}", stmt.node),
    };

    edges.push(format!("n{} -> n{};\n", root, leaf_id));

    root
}

fn visit_block(block: Block, edges: &mut Vec<String>,  nodes: &mut Vec<String>, root: usize) -> usize {
    for stmt in block.stmts {
        visit_stmt(stmt, edges, nodes, root);
    }
    root
}

fn visit_function_decl(name: String, sig: Signature, block: Option<Box<Block>>, edges: &mut Vec<String>,  nodes: &mut Vec<String>) -> usize {
    let id = nodes.len();
    nodes.push(format!("n{} [label=\"{} {}\"];\n", id, name, sig.to_string()));
    if let Some(box b) = block {
        let inner_id = visit_block(b, edges, nodes, id);
    }
    id
}

fn visit_variable_decl(name: String, _type: Option<Box<Type>>, expr: Expr, edges: &mut Vec<String>,  nodes: &mut Vec<String>) -> usize {
    let id = nodes.len();
    let type_name = if let Some(box t) = _type {
        t.to_string()
    } else {
        String::new()
    };
    nodes.push(format!("n{} [label=\"{}: {}\"];\n", id, name, type_name));
    let inner_id = visit_expr(expr, edges, nodes);
    edges.push(format!("n{} -> n{};\n", id, inner_id));
    id
}

fn visit_const_decl(name: String, _type: Type, expr: Expr, edges: &mut Vec<String>,  nodes: &mut Vec<String>) -> usize {
    panic!("Not yet supported!");
}

fn visit_type_decl(name: String, _type: Type, edges: &mut Vec<String>,  nodes: &mut Vec<String>) -> usize {
    panic!("Not yet supported!");
}

fn visit_item(item: Item, edges: &mut Vec<String>,  nodes: &mut Vec<String>) -> usize {
    use self::ItemKind::*;

    let name = item.name.clone();
    match item.node {
        FunctionDecl(box sig, block) => visit_function_decl(name, sig, block, edges, nodes),
        VariableDecl(t, box expr) => visit_variable_decl(name, t, expr, edges, nodes),
        ConstDecl(t, box expr) => visit_variable_decl(name, t, expr, edges, nodes),
        TypeDecl(box t) => visit_type_decl(name, t, edges, nodes),
    }
}

pub fn dump_parse_tree(item: Item, filename: &str) {

    let mut edges = Vec::new();
    let mut nodes = Vec::new();

    visit_item(item, &mut edges, &mut nodes);

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
