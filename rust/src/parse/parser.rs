use std::ops::{Index, IndexMut};

use crate::ast::{NodeData, NodeId, NodeMap, Syntax};
use crate::error::{SyntaxError, SyntaxErrorType, TsResult};
use crate::lex::{lex_next, LexMode, Lexer, LexerCheckpoint};
use crate::source::SourceRange;
use crate::symbol::{ScopeData, ScopeId, ScopeMap, ScopeType};
use crate::token::{Token, TokenType};

#[derive(Debug)]
pub struct MaybeToken {
    range: SourceRange,
    matched: bool,
}

impl MaybeToken {
    pub fn is_match(&self) -> bool {
        self.matched
    }

    pub fn match_loc(&self) -> Option<&SourceRange> {
        if self.matched {
            Some(&self.range)
        } else {
            None
        }
    }

    pub fn match_loc_take(self) -> Option<SourceRange> {
        if self.matched {
            Some(self.range)
        } else {
            None
        }
    }

    pub fn error(&self, err: SyntaxErrorType) -> SyntaxError {
        debug_assert!(!self.matched);
        SyntaxError::from_loc(&self.range, err)
    }

    pub fn and_then<R, F: FnOnce() -> TsResult<R>>(self, f: F) -> TsResult<Option<R>> {
        Ok(if self.matched { Some(f()?) } else { None })
    }

    pub fn or_else<R, F: FnOnce() -> TsResult<R>>(self, f: F) -> TsResult<Option<R>> {
        Ok(if self.matched { None } else { Some(f()?) })
    }
}

pub struct ParserCheckpoint {
    checkpoint: LexerCheckpoint,
}

struct BufferedToken {
    token: Token,
    lex_mode: LexMode,
    after_checkpoint: LexerCheckpoint,
}

pub struct Parser {
    lexer: Lexer,
    buffered: Option<BufferedToken>,
    node_map: NodeMap,
    /*
      We need at least two separate passes; consider:

      ```js
      let a = 1;
      {
        let fn = () => a;
        let a = 2;
        fn();
      }
      ```

      This is why we can't just minify in the same one pass. Therefore, to save time, we also analyse scope declarations while parsing.
    */
    scope_map: ScopeMap,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        Parser {
            lexer,
            buffered: None,
            node_map: NodeMap::new(),
            scope_map: ScopeMap::new(),
        }
    }

    pub fn create_node(&mut self, scope: ScopeId, loc: SourceRange, stx: Syntax) -> NodeId {
        self.node_map.create_node(scope, loc, stx)
    }

    pub fn scope_map_mut(&mut self) -> &mut ScopeMap {
        &mut self.scope_map
    }

    pub fn create_global_scope(&mut self) -> ScopeId {
        self.scope_map.create_scope(None, None, ScopeType::Global)
    }

    pub fn create_child_scope(&mut self, parent: ScopeId, typ: ScopeType) -> ScopeId {
        self.scope_map.create_scope(
            self.scope_map[parent].self_or_ancestor_closure(),
            Some(parent),
            typ,
        )
    }

    pub fn take(self) -> (NodeMap, ScopeMap) {
        (self.node_map, self.scope_map)
    }

    pub fn source_range(&self) -> SourceRange {
        self.lexer.source_range()
    }

    pub fn checkpoint(&self) -> ParserCheckpoint {
        ParserCheckpoint {
            checkpoint: self.lexer.checkpoint(),
        }
    }

    pub fn since_checkpoint(&self, checkpoint: ParserCheckpoint) -> SourceRange {
        self.lexer.since_checkpoint(checkpoint.checkpoint)
    }

    pub fn restore_checkpoint(&mut self, checkpoint: ParserCheckpoint) -> () {
        self.buffered = None;
        self.lexer.apply_checkpoint(checkpoint.checkpoint);
    }

    fn forward<K: FnOnce(&Token) -> bool>(&mut self, mode: LexMode, keep: K) -> TsResult<Token> {
        match self.buffered.as_ref() {
            Some(b) if b.lex_mode == mode => Ok(if keep(&b.token) {
                self.lexer.apply_checkpoint(b.after_checkpoint);
                self.buffered.take().unwrap().token
            } else {
                b.token.clone()
            }),
            _ => {
                // Don't use self.checkpoint as self.backtrack will clear buffer.
                let cp = self.lexer.checkpoint();
                let t = lex_next(&mut self.lexer, mode)?;
                self.buffered = if keep(&t) {
                    None
                } else {
                    let after_checkpoint = self.lexer.checkpoint();
                    self.lexer.apply_checkpoint(cp);
                    Some(BufferedToken {
                        token: t.clone(),
                        lex_mode: mode,
                        after_checkpoint,
                    })
                };
                Ok(t)
            }
        }
    }

    pub fn next_with_mode(&mut self, mode: LexMode) -> TsResult<Token> {
        self.forward(mode, |_| true)
    }

    pub fn next(&mut self) -> TsResult<Token> {
        self.next_with_mode(LexMode::Standard)
    }

    pub fn peek_with_mode(&mut self, mode: LexMode) -> TsResult<Token> {
        self.forward(mode, |_| false)
    }

    pub fn peek(&mut self) -> TsResult<Token> {
        self.peek_with_mode(LexMode::Standard)
    }

    pub fn consume_peeked(&mut self) -> () {
        let b = self.buffered.take().unwrap();
        self.lexer.apply_checkpoint(b.after_checkpoint);
    }

    pub fn maybe_with_mode(&mut self, typ: TokenType, mode: LexMode) -> TsResult<MaybeToken> {
        let t = self.forward(mode, |t| t.typ() == typ)?;
        Ok(MaybeToken {
            matched: t.typ() == typ,
            range: t.loc_take(),
        })
    }

    pub fn consume_if(&mut self, typ: TokenType) -> TsResult<MaybeToken> {
        self.maybe_with_mode(typ, LexMode::Standard)
    }

    pub fn require_with_mode(&mut self, typ: TokenType, mode: LexMode) -> TsResult<Token> {
        let t = self.next_with_mode(mode)?;
        if t.typ() != typ {
            Err(t.error(SyntaxErrorType::RequiredTokenNotFound(typ)))
        } else {
            Ok(t)
        }
    }

    pub fn require_predicate<P: FnOnce(TokenType) -> bool>(
        &mut self,
        pred: P,
        expected: &'static str,
    ) -> TsResult<Token> {
        let t = self.next_with_mode(LexMode::Standard)?;
        if !pred(t.typ()) {
            Err(t.error(SyntaxErrorType::ExpectedSyntax(expected)))
        } else {
            Ok(t)
        }
    }

    pub fn require(&mut self, typ: TokenType) -> TsResult<Token> {
        self.require_with_mode(typ, LexMode::Standard)
    }
}

impl Index<NodeId> for Parser {
    type Output = NodeData;

    fn index(&self, index: NodeId) -> &Self::Output {
        &self.node_map[index]
    }
}

impl IndexMut<NodeId> for Parser {
    fn index_mut(&mut self, index: NodeId) -> &mut Self::Output {
        &mut self.node_map[index]
    }
}

impl Index<ScopeId> for Parser {
    type Output = ScopeData;

    fn index(&self, index: ScopeId) -> &Self::Output {
        &self.scope_map[index]
    }
}

impl IndexMut<ScopeId> for Parser {
    fn index_mut(&mut self, index: ScopeId) -> &mut Self::Output {
        &mut self.scope_map[index]
    }
}
