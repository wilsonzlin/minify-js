use serde_json::{json, Value};

use crate::ast::{
    ArrayElement, ClassOrObjectMemberKey, ClassOrObjectMemberValue, ForInOfStmtHeaderLhs,
    ForStmtHeader, ForThreeInit, NodeId, NodeMap, ObjectMemberType, Syntax,
};

fn visit_node(m: &NodeMap, n: NodeId) -> Value {
    match m[n].stx() {
        Syntax::IdentifierPattern { name } => json!({
            "$t": "IdentifierPattern",
            "name": name.as_str().to_string(),
        }),
        Syntax::ArrayPattern { elements, rest } => json!({
            "$t": "ArrayPattern",
            "elements": elements.iter().map(|e| e.as_ref().map(|e| json!({
                "target": visit_node(m, e.target),
                "default_value": e.default_value.map(|n| visit_node(m, n)),
            }))).collect::<Vec<_>>(),
            "rest": rest.map(|n| visit_node(m, n)),
        }),
        Syntax::ObjectPattern { properties, rest } => json!({
            "$t": "ObjectPattern",
            "properties": properties.iter().map(|n| visit_node(m, *n)).collect::<Vec<_>>(),
            "rest": rest.map(|n| visit_node(m, n)),
        }),
        Syntax::FunctionName { name } => json!({
            "$t": "FunctionName",
            "name": name.as_str().to_string(),
        }),
        Syntax::FunctionSignature { parameters } => json!({
            "$t": "FunctionSignature",
            "parameters": parameters.iter().map(|n| visit_node(m, *n)).collect::<Vec<_>>(),
        }),
        Syntax::ClassDecl {
            name,
            super_class,
            members,
        } => todo!(),
        Syntax::FunctionDecl {
            name,
            signature,
            body,
        } => json!({
            "$t": "FunctionDecl",
            "name": visit_node(m, *name),
            "signature": visit_node(m, *signature),
            "body": visit_node(m, *body),
        }),
        Syntax::ParamDecl {
            rest,
            pattern,
            default_value,
        } => json!({
            "$t": "ParamDecl",
            "rest": rest,
            "pattern": visit_node(m, *pattern),
            "default_value": default_value.map(|n| visit_node(m, n)),
        }),
        Syntax::VarDecl { mode, declarators } => json!({
            "$t": "VarDecl",
            "mode": mode,
            "declarators": declarators.iter().map(|d| json!({
                "pattern": visit_node(m, d.pattern),
                "initializer": d.initializer.map(|n| visit_node(m, n)),
            })).collect::<Vec<_>>(),
        }),
        Syntax::ArrowFunctionExpr { signature, body } => json!({
            "$t": "ArrowFunctionExpr",
            "signature": visit_node(m, *signature),
            "body": visit_node(m, *body),
        }),
        Syntax::BinaryExpr {
            parenthesised,
            operator,
            left,
            right,
        } => json!({
            "$t": "BinaryExpr",
            "parenthesised": parenthesised,
            "operator": operator,
            "left": visit_node(m, *left),
            "right": visit_node(m, *right),
        }),
        Syntax::CallExpr {
            parenthesised,
            callee,
            arguments,
        } => json!({
            "$t": "CallExpr",
            "parenthesised": parenthesised,
            "callee": visit_node(m, *callee),
            "arguments": arguments.iter().map(|n| visit_node(m, *n)).collect::<Vec<_>>(),
        }),
        Syntax::ConditionalExpr {
            parenthesised,
            test,
            consequent,
            alternate,
        } => json!({
            "$t": "ConditionalExpr",
            "parenthesised": parenthesised,
            "test": visit_node(m, *test),
            "consequent": visit_node(m, *consequent),
            "alternate": visit_node(m, *alternate),
        }),
        Syntax::ComputedMemberExpr { object, member } => json!({
            "$t": "ComputedMemberExpr",
            "object": visit_node(m, *object),
            "member": visit_node(m, *member),
        }),
        Syntax::FunctionExpr {
            parenthesised,
            name,
            signature,
            body,
        } => json!({
            "$t": "FunctionExpr",
            "parenthesised": parenthesised,
            "name": name.as_ref().map(|n| visit_node(m, *n)),
            "signature": visit_node(m, *signature),
            "body": visit_node(m, *body),
        }),
        Syntax::IdentifierExpr { name } => json!({
            "$t": "IdentifierExpr",
            "name": name.as_str().to_string(),
        }),
        Syntax::ImportExpr { module } => json!({
            "$t": "ImportExpr",
           "module": module,
        }),
        Syntax::LiteralArrayExpr { elements } => json!({
            "$t": "LiteralArrayExpr",
            "elements": elements.iter().map(|e| match e {
                ArrayElement::Single(e) => json!({
                    "single": visit_node(m, *e),
                }),
                ArrayElement::Rest(e) => json!({
                    "rest": visit_node(m, *e),
                }),
                ArrayElement::Empty => Value::Null,
            }).collect::<Vec<_>>(),
        }),
        Syntax::LiteralBooleanExpr { value } => json!({
            "$t": "LiteralBooleanExpr",
            "value": value,
        }),
        Syntax::LiteralNull {} => json!({
            "$t": "LiteralNull",
        }),
        Syntax::LiteralNumberExpr { value } => json!({
            "$t": "LiteralNumberExpr",
            "value": value.0,
        }),
        Syntax::LiteralObjectExpr { members } => json!({
            "$t": "LiteralObjectExpr",
            "members": members.iter().map(|n| visit_node(m, *n)).collect::<Vec<_>>(),
        }),
        Syntax::LiteralRegexExpr {} => json!({
            "$t": "LiteralRegexExpr",
        }),
        Syntax::LiteralStringExpr { value } => json!({
            "$t": "LiteralStringExpr",
            "value": value.as_str().to_string(),
        }),
        Syntax::LiteralUndefined {} => json!({
            "$t": "LiteralUndefined",
        }),
        Syntax::ThisExpr {} => json!({
            "$t": "ThisExpr",
        }),
        Syntax::UnaryExpr {
            parenthesised,
            operator,
            argument,
        } => json!({
            "$t": "UnaryExpr",
            "parenthesised": parenthesised,
            "operator": operator,
            "argument": visit_node(m, *argument),
        }),
        Syntax::UnaryPostfixExpr {
            parenthesised,
            operator,
            argument,
        } => json!({
            "$t": "UnaryPostfixExpr",
            "parenthesised": parenthesised,
            "operator": operator,
            "argument": visit_node(m, *argument),
        }),
        Syntax::YieldExpr { argument, delegate } => json!({
            "$t": "YieldExpr",
            "argument": visit_node(m, *argument),
            "delegate": delegate,
        }),
        Syntax::BlockStmt { body } => json!({
            "$t": "BlockStmt",
            "body": body.iter().map(|n| visit_node(m, *n)).collect::<Vec<_>>(),
        }),
        Syntax::BreakStmt { label } => json!({
            "$t": "BreakStmt", "label": label.as_ref().map(|n| n.as_str().to_string()) }),
        Syntax::ContinueStmt { label } => json!({
            "$t": "ContinueStmt", "label": label.as_ref().map(|n| n.as_str().to_string()) }),
        Syntax::DebuggerStmt {} => json!({
            "$t": "DebuggerStmt",
        }),
        Syntax::DoWhileStmt { condition, body } => json!({
            "$t": "DoWhileStmt",
            "condition": visit_node(m, *condition),
            "body": visit_node(m, *body)
        }),
        Syntax::EmptyStmt {} => json!({
            "$t": "EmptyStmt",
        }),
        Syntax::ExportDeclStmt { declaration } => json!({
            "$t": "ExportDeclStmt", "declaration": visit_node(m, *declaration) }),
        Syntax::ExportDefaultStmt { expression } => json!({
            "$t": "ExportDefaultStmt", "expression": visit_node(m, *expression) }),
        Syntax::ExportListStmt { names, from } => todo!(),
        Syntax::ExpressionStmt { expression } => json!({
            "$t": "ExpressionStmt",
            "expression": visit_node(m, *expression),
        }),
        Syntax::IfStmt {
            test,
            consequent,
            alternate,
        } => json!({
            "$t": "IfStmt",
            "test": visit_node(m, *test),
            "consequent": visit_node(m, *consequent),
            "alternate": alternate.map(|n| visit_node(m, n)),
        }),
        Syntax::ImportStmt {
            default,
            names,
            module,
        } => todo!(),
        Syntax::ForStmt { header, body } => json!({
            "$t": "ForStmt",
            "header": match header {
                ForStmtHeader::Three { init, condition, post } => json!({
                    "three": json!({
                        "init": match init {
                            ForThreeInit::None => Value::Null,
                            ForThreeInit::Expression(e) => json!({
                                "expression": visit_node(m, *e),
                            }),
                            ForThreeInit::Declaration(e) => json!({
                                "declaration": visit_node(m, *e),
                            }),
                        },
                        "condition": condition.map(|n| visit_node(m, n)),
                        "post": post.map(|n| visit_node(m, n)),
                    }),
                }),
                ForStmtHeader::InOf { of, lhs, rhs } => json!({
                    "inOf": json!({
                        "of": of,
                        "lhs": match lhs {
                            ForInOfStmtHeaderLhs::Declaration(decl) => json!({
                                "declaration": visit_node(m, *decl),
                            }),
                            ForInOfStmtHeaderLhs::Pattern(pat) => json!({
                                "pattern": visit_node(m, *pat),
                            }),
                        },
                        "rhs": visit_node(m, *rhs),
                    }),
                }),
            },
            "body": visit_node(m, *body),
        }),
        Syntax::ReturnStmt { value } => json!({
            "$t": "ReturnStmt", "value": value.map(|n| visit_node(m, n)) }),
        Syntax::SwitchStmt { test, branches } => json!({
            "$t": "SwitchStmt",
            "test": visit_node(m, *test),
            "branches": branches.iter().map(|n| visit_node(m, *n)).collect::<Vec<_>>(),
        }),
        Syntax::ThrowStmt { value } => json!({
            "$t": "ThrowStmt", "value": visit_node(m, *value) }),
        Syntax::TryStmt {
            wrapped,
            catch,
            finally,
        } => json!({
            "$t": "TryStmt",
            "wrapped": visit_node(m, *wrapped),
            "catch": catch.map(|n| visit_node(m, n)),
            "finally": finally.map(|n| visit_node(m, n)),
        }),
        Syntax::VarStmt { declaration } => json!({
            "$t": "VarStmt", "declaration": visit_node(m, *declaration) }),
        Syntax::WhileStmt { condition, body } => json!({
            "$t": "WhileStmt",
            "condition": visit_node(m, *condition),
            "body": visit_node(m, *body),
        }),
        Syntax::TopLevel { body } => json!({
            "$t": "TopLevel", "body": body.iter().map(|n| visit_node(m, *n)).collect::<Vec<_>>() }),
        Syntax::CatchBlock { parameter, body } => json!({
            "$t": "CatchBlock",
            "parameter": parameter.map(|n| visit_node(m, n)),
            "body": visit_node(m, *body),
        }),
        Syntax::SwitchBranch { case, body } => json!({
            "$t": "SwitchBranch",
            "case": case.map(|n| visit_node(m, n)),
            "body": body.iter().map(|n| visit_node(m, *n)).collect::<Vec<_>>(),
        }),
        Syntax::ObjectMember { typ } => json!({
            "$t": "ObjectMember",
            "typ": match typ {
                ObjectMemberType::Rest { value } => json!({
                    "rest": visit_node(m, *value),
                }),
                ObjectMemberType::Shorthand { name } => json!({
                    "shorthand": json!(name.as_str().to_string()),
                }),
                ObjectMemberType::Valued { key, value } => json!({
                    "valued": json!({
                        "key": match key {
                            ClassOrObjectMemberKey::Direct(r) => json!(r.as_str().to_string()),
                            ClassOrObjectMemberKey::Computed(e) => visit_node(m, *e),
                        },
                        "value": match value {
                            ClassOrObjectMemberValue::Getter { body } => json!({
                                "getter": json!({
                                    "body": visit_node(m, *body),
                                }),
                            }),
                            ClassOrObjectMemberValue::Method { signature, body } => json!({
                                "method": json!({
                                    "signature": visit_node(m, *signature),
                                    "body": visit_node(m, *body),
                                }),
                            }),
                            ClassOrObjectMemberValue::Property { initializer } => json!({
                                "property": json!({
                                    "initializer": initializer.map(|n| visit_node(m, n)),
                                }),
                            }),
                            ClassOrObjectMemberValue::Setter { parameter, body } => json!({
                                "setter": json!({
                                    "parameter": visit_node(m, *parameter),
                                    "body": visit_node(m, *body),
                                }),
                            }),
                        },
                    }),
                }),
            },
        }),
        Syntax::ObjectPatternProperty {
            key,
            target,
            default_value,
        } => json!({
            "$t": "ObjectPatternProperty",
            "key": match key {
                ClassOrObjectMemberKey::Direct(r) => json!(r.as_str().to_string()),
                ClassOrObjectMemberKey::Computed(e) => visit_node(m, *e),
            },
            "target": target.map(|n| visit_node(m, n)),
            "default": default_value.map(|n| visit_node(m, n)),
        }),
        Syntax::MemberAccessExpr {
            parenthesised,
            optional_chaining,
            left,
            right,
        } => json!({
            "$t": "MemberAccessExpr",
            "parenthesised": parenthesised,
            "optional_chaining": optional_chaining,
            "left": visit_node(m, *left),
            "right": right.as_str().to_string(),
        }),
    }
}

pub fn serialise_ast(node_map: &NodeMap, top_level_node_id: NodeId) -> Value {
    visit_node(node_map, top_level_node_id)
}
