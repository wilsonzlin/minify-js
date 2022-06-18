use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

use crate::source::{Source, SourceRange};

pub struct Identifier {
    name: SourceRange,
}

struct SymbolData {
    loc: SourceRange,
    name: SourceRange,
}

#[derive(Clone)]
pub struct Symbol {
    data: Arc<SymbolData>,
}

#[derive(Clone)]
pub enum ScopeType {
    Module,
    Function,
    Block,
}

struct ScopeData {
    symbols: HashMap<Identifier, Symbol>,
    parent: Option<Scope>,
    typ: ScopeType,
}

#[derive(Clone)]
pub struct Scope {
    data: Rc<ScopeData>,
}

impl Scope {
    pub fn new(parent: Option<Scope>, typ: ScopeType) -> Scope {
        Scope {
            data: Rc::new(ScopeData {
                symbols: HashMap::new(),
                parent,
                typ,
            }),
        }
    }
}

pub struct ExportIdentifier {
    module: Source,
    name: Identifier,
}

pub struct ProgramSymbols {
    globals: HashMap<Identifier, Symbol>,
    exports: HashMap<ExportIdentifier, Symbol>,
}
