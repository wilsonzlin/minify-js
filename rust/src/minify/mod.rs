use crate::{
    ast::{
        ArrayElement, ClassOrObjectMemberKey, ClassOrObjectMemberValue, ForInOfStmtHeaderLhs,
        ForStmtHeader, ForThreeInit, NodeData, NodeId, NodeMap, ObjectMemberType, Syntax,
    },
    char::{ID_CONTINUE_CHARSTR, ID_START_CHARSTR},
    source::{Source, SourceRange},
    symbol::ScopeMap,
};

fn generate_minified_name(mut id: usize) -> SourceRange {
    let mut name = vec![ID_START_CHARSTR[id % ID_START_CHARSTR.len()]];
    if id >= ID_START_CHARSTR.len() {
        id /= ID_START_CHARSTR.len();
        while id >= ID_CONTINUE_CHARSTR.len() {
            name.push(ID_CONTINUE_CHARSTR[id % ID_CONTINUE_CHARSTR.len()]);
            id /= ID_CONTINUE_CHARSTR.len();
        }
        name.push(ID_CONTINUE_CHARSTR[id % ID_CONTINUE_CHARSTR.len()]);
    };
    let end = name.len();
    SourceRange {
        source: Source::new(name),
        start: 0,
        end,
    }
}

// Rust won't let us iterate the tree, matching and exploring, while also making the occasional mutation of nodes or subtrees during this iterating. Therefore, we use an update list that we'll apply after the iteration finishes.

struct NodeUpdate {
    // If this is outside the range of NodeMap, it's a new node that needs to be inserted into the NodeMap.
    node_id: NodeId,
    new_data: NodeData,
}

struct NodeUpdates {
    updates: Vec<NodeUpdate>,
    next_new_node_id: usize,
}

fn visit_class_or_object_key(
    s: &ScopeMap,
    m: &NodeMap,
    updates: &mut NodeUpdates,
    key: &ClassOrObjectMemberKey,
) -> () {
    match key {
        ClassOrObjectMemberKey::Direct(e) => {}
        ClassOrObjectMemberKey::Computed(e) => visit_node(s, m, updates, *e),
    };
}

fn visit_class_or_object_value(
    s: &ScopeMap,
    m: &NodeMap,
    updates: &mut NodeUpdates,
    value: &ClassOrObjectMemberValue,
) -> () {
    match value {
        ClassOrObjectMemberValue::Getter { body } => visit_node(s, m, updates, *body),
        ClassOrObjectMemberValue::Method {
            signature, body, ..
        } => {
            visit_node(s, m, updates, *signature);
            visit_node(s, m, updates, *body);
        }
        ClassOrObjectMemberValue::Property { initializer } => {
            if let Some(initializer) = initializer {
                visit_node(s, m, updates, *initializer);
            };
        }
        ClassOrObjectMemberValue::Setter { body, parameter } => {
            visit_node(s, m, updates, *parameter);
            visit_node(s, m, updates, *body);
        }
    }
}

fn visit_node(s: &ScopeMap, m: &NodeMap, updates: &mut NodeUpdates, n: NodeId) -> () {
    let scope_id = m[n].scope();
    let scope = &s[scope_id];
    match m[n].stx() {
        Syntax::FunctionExpr {
            name,
            signature,
            body,
            ..
        } => {
            if let Some(name) = name {
                visit_node(s, m, updates, *name);
            };
            visit_node(s, m, updates, *signature);
            visit_node(s, m, updates, *body);
        }
        stx @ (Syntax::IdentifierPattern { name } | Syntax::ClassOrFunctionName { name }) => {
            let sym = scope.find_symbol(s, name);
            if let Some(sym) = sym {
                let minified = generate_minified_name(sym.minified_name_id());
                updates.updates.push(NodeUpdate {
                    node_id: n,
                    new_data: NodeData::new(
                        scope_id,
                        minified.clone(),
                        match stx {
                            Syntax::IdentifierPattern { .. } => Syntax::IdentifierPattern {
                                name: minified.clone(),
                            },
                            Syntax::ClassOrFunctionName { .. } => Syntax::ClassOrFunctionName {
                                name: minified.clone(),
                            },
                            _ => unreachable!(),
                        },
                    ),
                })
            };
        }
        Syntax::ArrayPattern { elements, rest } => {
            for e in elements {
                if let Some(e) = e {
                    visit_node(s, m, updates, e.target);
                };
            }
            if let Some(r) = rest {
                visit_node(s, m, updates, *r);
            };
        }
        Syntax::ArrowFunctionExpr { signature, body } => {
            visit_node(s, m, updates, *signature);
            visit_node(s, m, updates, *body);
        }
        Syntax::BinaryExpr { left, right, .. } => {
            visit_node(s, m, updates, *left);
            visit_node(s, m, updates, *right);
        }
        Syntax::BlockStmt { body } => {
            for stmt in body {
                visit_node(s, m, updates, *stmt);
            }
        }
        Syntax::BreakStmt { .. } => {}
        Syntax::CallExpr {
            callee, arguments, ..
        } => {
            visit_node(s, m, updates, *callee);
            for arg in arguments {
                visit_node(s, m, updates, *arg);
            }
        }
        Syntax::CatchBlock { parameter, body } => {
            if let Some(p) = parameter {
                visit_node(s, m, updates, *p);
            }
            visit_node(s, m, updates, *body);
        }
        Syntax::ClassDecl {
            name,
            extends,
            members,
        } => {
            visit_node(s, m, updates, *name);
            if let Some(extends) = extends {
                visit_node(s, m, updates, *extends);
            };
            for member in members {
                visit_class_or_object_key(s, m, updates, &member.key);
                visit_class_or_object_value(s, m, updates, &member.value);
            }
        }
        Syntax::ClassExpr {
            name,
            extends,
            members,
            ..
        } => {
            if let Some(name) = name {
                visit_node(s, m, updates, *name);
            }
            if let Some(extends) = extends {
                visit_node(s, m, updates, *extends);
            };
            for member in members {
                visit_class_or_object_key(s, m, updates, &member.key);
                visit_class_or_object_value(s, m, updates, &member.value);
            }
        }
        Syntax::ComputedMemberExpr { object, member } => {
            visit_node(s, m, updates, *object);
            visit_node(s, m, updates, *member);
        }
        Syntax::ConditionalExpr {
            test,
            consequent,
            alternate,
            ..
        } => {
            visit_node(s, m, updates, *test);
            visit_node(s, m, updates, *consequent);
            visit_node(s, m, updates, *alternate);
        }
        Syntax::ContinueStmt { .. } => {}
        Syntax::DebuggerStmt {} => {}
        Syntax::DoWhileStmt { condition, body } => {
            visit_node(s, m, updates, *body);
            visit_node(s, m, updates, *condition);
        }
        Syntax::EmptyStmt {} => {}
        Syntax::ExportDeclStmt { declaration } => todo!(),
        Syntax::ExportDefaultStmt { expression } => todo!(),
        Syntax::ExportListStmt { names, from } => todo!(),
        Syntax::ExpressionStmt { expression } => {
            visit_node(s, m, updates, *expression);
        }
        Syntax::ForStmt { header, body } => {
            match header {
                ForStmtHeader::Three {
                    init,
                    condition,
                    post,
                } => {
                    match init {
                        ForThreeInit::None => {}
                        ForThreeInit::Expression(n) => visit_node(s, m, updates, *n),
                        ForThreeInit::Declaration(n) => visit_node(s, m, updates, *n),
                    };
                    if let Some(condition) = condition {
                        visit_node(s, m, updates, *condition);
                    }
                    if let Some(post) = post {
                        visit_node(s, m, updates, *post);
                    }
                }
                ForStmtHeader::InOf { lhs, rhs, .. } => {
                    match lhs {
                        ForInOfStmtHeaderLhs::Declaration(n) => visit_node(s, m, updates, *n),
                        ForInOfStmtHeaderLhs::Pattern(n) => visit_node(s, m, updates, *n),
                    }
                    visit_node(s, m, updates, *rhs);
                }
            };
            visit_node(s, m, updates, *body);
        }
        Syntax::FunctionDecl {
            name,
            signature,
            body,
            ..
        } => {
            visit_node(s, m, updates, *name);
            visit_node(s, m, updates, *signature);
            visit_node(s, m, updates, *body);
        }
        Syntax::FunctionSignature { parameters } => {
            for p in parameters {
                visit_node(s, m, updates, *p);
            }
        }
        Syntax::IdentifierExpr { name } => {
            let sym = scope.find_symbol(s, name);
            if let Some(sym) = sym {
                let minified = generate_minified_name(sym.minified_name_id());
                updates.updates.push(NodeUpdate {
                    node_id: n,
                    new_data: NodeData::new(
                        scope_id,
                        minified.clone(),
                        Syntax::IdentifierExpr {
                            name: minified.clone(),
                        },
                    ),
                })
            };
        }
        Syntax::IfStmt {
            test,
            consequent,
            alternate,
        } => {
            visit_node(s, m, updates, *test);
            visit_node(s, m, updates, *consequent);
            if let Some(alternate) = alternate {
                visit_node(s, m, updates, *alternate);
            };
        }
        Syntax::ImportExpr { module } => todo!(),
        Syntax::ImportStmt {
            default,
            names,
            module,
        } => todo!(),
        Syntax::LiteralArrayExpr { elements } => {
            for e in elements {
                match e {
                    ArrayElement::Single(e) => visit_node(s, m, updates, *e),
                    ArrayElement::Rest(e) => visit_node(s, m, updates, *e),
                    ArrayElement::Empty => {}
                }
            }
        }
        Syntax::LiteralBooleanExpr { .. } => {}
        Syntax::LiteralNull {} => {}
        Syntax::LiteralNumberExpr { .. } => {}
        Syntax::LiteralObjectExpr { members } => {
            for member in members {
                visit_node(s, m, updates, *member);
            }
        }
        Syntax::LiteralRegexExpr {} => {}
        Syntax::LiteralStringExpr { .. } => {}
        Syntax::LiteralUndefined {} => {}
        Syntax::ObjectPattern { properties, rest } => {
            for p in properties {
                visit_node(s, m, updates, *p);
            }
            if let Some(r) = rest {
                visit_node(s, m, updates, *r);
            }
        }
        Syntax::ObjectPatternProperty {
            key,
            target,
            default_value,
        } => {
            match key {
                ClassOrObjectMemberKey::Direct(name) => {
                    if target.is_none() {
                        let sym = scope.find_symbol(s, name);
                        if let Some(sym) = sym {
                            let minified = generate_minified_name(sym.minified_name_id());
                            let replacement_target_node_id = updates.next_new_node_id;
                            updates.next_new_node_id += 1;
                            let replacement_target_node = NodeData::new(
                                scope_id,
                                minified.clone(),
                                Syntax::IdentifierPattern {
                                    name: minified.clone(),
                                },
                            );
                            updates.updates.push(NodeUpdate {
                                node_id: NodeId::new(replacement_target_node_id),
                                new_data: replacement_target_node,
                            });
                            updates.updates.push(NodeUpdate {
                                node_id: n,
                                new_data: NodeData::new(
                                    scope_id,
                                    minified.clone(),
                                    Syntax::ObjectPatternProperty {
                                        key: key.clone(),
                                        target: Some(NodeId::new(replacement_target_node_id)),
                                        default_value: default_value.clone(),
                                    },
                                ),
                            });
                        };
                    }
                }
                ClassOrObjectMemberKey::Computed(c) => visit_node(s, m, updates, *c),
            };
            if let Some(target) = target {
                visit_node(s, m, updates, *target);
            }
            if let Some(v) = default_value {
                visit_node(s, m, updates, *v);
            }
        }
        Syntax::ParamDecl {
            pattern,
            default_value,
            ..
        } => {
            visit_node(s, m, updates, *pattern);
            if let Some(v) = default_value {
                visit_node(s, m, updates, *v);
            }
        }
        Syntax::ReturnStmt { value } => {
            if let Some(v) = value {
                visit_node(s, m, updates, *v);
            }
        }
        Syntax::SwitchBranch { case, body } => {
            if let Some(v) = case {
                visit_node(s, m, updates, *v);
            }
            for stmt in body {
                visit_node(s, m, updates, *stmt);
            }
        }
        Syntax::SwitchStmt { test, branches } => {
            visit_node(s, m, updates, *test);
            for b in branches {
                visit_node(s, m, updates, *b);
            }
        }
        Syntax::ThisExpr {} => {}
        Syntax::ThrowStmt { value } => {
            visit_node(s, m, updates, *value);
        }
        Syntax::TopLevel { body } => {
            for stmt in body {
                visit_node(s, m, updates, *stmt);
            }
        }
        Syntax::TryStmt {
            wrapped,
            catch,
            finally,
        } => {
            visit_node(s, m, updates, *wrapped);
            if let Some(catch) = catch {
                visit_node(s, m, updates, *catch);
            }
            if let Some(finally) = finally {
                visit_node(s, m, updates, *finally);
            }
        }
        Syntax::UnaryExpr { argument, .. } => {
            visit_node(s, m, updates, *argument);
        }
        Syntax::UnaryPostfixExpr { argument, .. } => {
            visit_node(s, m, updates, *argument);
        }
        Syntax::VarDecl { declarators, .. } => {
            for decl in declarators {
                visit_node(s, m, updates, decl.pattern);
                if let Some(e) = decl.initializer {
                    visit_node(s, m, updates, e);
                }
            }
        }
        Syntax::VarStmt { declaration } => {
            visit_node(s, m, updates, *declaration);
        }
        Syntax::WhileStmt { condition, body } => {
            visit_node(s, m, updates, *condition);
            visit_node(s, m, updates, *body);
        }
        Syntax::YieldExpr { argument, delegate } => todo!(),
        Syntax::ObjectMember { typ } => {
            match typ {
                ObjectMemberType::Valued { key, value } => {
                    visit_class_or_object_key(s, m, updates, key);
                    visit_class_or_object_value(s, m, updates, value);
                }
                ObjectMemberType::Shorthand { name } => {
                    let sym = scope.find_symbol(s, name);
                    if let Some(sym) = sym {
                        let minified = generate_minified_name(sym.minified_name_id());
                        let replacement_initializer_node_id = updates.next_new_node_id;
                        updates.next_new_node_id += 1;
                        let replacement_initializer_node = NodeData::new(
                            scope_id,
                            minified.clone(),
                            Syntax::IdentifierExpr {
                                name: minified.clone(),
                            },
                        );
                        updates.updates.push(NodeUpdate {
                            node_id: NodeId::new(replacement_initializer_node_id),
                            new_data: replacement_initializer_node,
                        });
                        updates.updates.push(NodeUpdate {
                            node_id: n,
                            new_data: NodeData::new(
                                scope_id,
                                minified.clone(),
                                Syntax::ObjectMember {
                                    typ: ObjectMemberType::Valued {
                                        key: ClassOrObjectMemberKey::Direct(name.clone()),
                                        value: ClassOrObjectMemberValue::Property {
                                            initializer: Some(NodeId::new(
                                                replacement_initializer_node_id,
                                            )),
                                        },
                                    },
                                },
                            ),
                        });
                    };
                }
                ObjectMemberType::Rest { value } => todo!(),
            };
        }
        Syntax::MemberAccessExpr { left, .. } => {
            visit_node(s, m, updates, *left);
        }
        Syntax::LabelStmt { name } => {}
    };
}

pub fn minify_js(
    scope_map: &mut ScopeMap,
    node_map: &mut NodeMap,
    top_level_node_id: NodeId,
) -> () {
    // We need these since we cannot look up in scope_map while mutably borrowed by iter_mut().
    let symbol_counts: Vec<usize> = scope_map.iter().map(|s| s.symbol_count()).collect();
    let mut minified_name_starts: Vec<usize> = vec![0; scope_map.len()];
    // Iterating like this assumes that any parent of an element is always present before it in the list.
    for (id, scope) in scope_map.iter_mut().enumerate() {
        // TODO Opportunity for optimisation: not all variables are used by descendant scopes, so some of their names can be reused by descendants.
        let minified_name_start = match scope.parent() {
            Some(p) => symbol_counts[p.id()] + minified_name_starts[p.id()],
            None => 0,
        };
        scope.symbols_update(|no, symbol| {
            symbol.set_minified_name_id(minified_name_start + no);
        });
        minified_name_starts[id] = minified_name_start;
    }

    let mut node_updates = NodeUpdates {
        updates: Vec::new(),
        next_new_node_id: node_map.len(),
    };
    match node_map[top_level_node_id].stx() {
        Syntax::TopLevel { body } => {
            for n in body.iter() {
                visit_node(&scope_map, &node_map, &mut node_updates, *n);
            }
        }
        _ => panic!("not top level"),
    };
    for u in node_updates.updates {
        if u.node_id.id() == node_map.len() {
            node_map.push(u.new_data)
        } else {
            node_map[u.node_id] = u.new_data;
        }
    }
}
