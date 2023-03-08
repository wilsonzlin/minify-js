use super::ctx::MinifyScope;
use super::ctx::MinifySymbol;
use parse_js::char::ID_CONTINUE_CHARSTR;
use parse_js::char::ID_START_CHARSTR;
use parse_js::lex::KEYWORD_STRS;
use parse_js::session::Session;
use parse_js::session::SessionHashMap;
use parse_js::session::SessionHashSet;
use parse_js::session::SessionVec;
use parse_js::source::SourceRange;
use parse_js::symbol::Identifier;
use parse_js::symbol::Scope;
use parse_js::symbol::Symbol;

// Generator of minified names. Works by generating the next smallest possible name (starting from `a`), and then repeats until it finds one that is not a keyword or would conflict with an inherited variable (a variable that is in scope **and** used by code that we would otherwise shadow).
pub struct MinifiedNameGenerator<'a> {
  session: &'a Session,
  // Index of each character of the last generated name, in reverse order (i.e. first character is last element) for optimised extension.
  state: SessionVec<'a, usize>,
}

impl<'a> MinifiedNameGenerator<'a> {
  pub fn new(session: &'a Session) -> MinifiedNameGenerator<'a> {
    MinifiedNameGenerator {
      session,
      state: session.new_vec(),
    }
  }

  fn transition_to_next_possible_minified_name(&mut self) -> SessionVec<'a, u8> {
    let n = &mut self.state;
    let mut overflow = true;
    for i in 0..n.len() {
      let charset = if i == n.len() - 1 {
        ID_START_CHARSTR
      } else {
        ID_CONTINUE_CHARSTR
      };
      if n[i] == charset.len() - 1 {
        n[i] = 0;
      } else {
        n[i] += 1;
        overflow = false;
        break;
      };
    }
    if overflow {
      n.push(0);
    };

    let mut name = self.session.new_vec(); // TODO Capacity
    for (i, idx) in n.iter().enumerate() {
      let charset = if i == n.len() - 1 {
        ID_START_CHARSTR
      } else {
        ID_CONTINUE_CHARSTR
      };
      name.push(charset[*idx]);
    }
    name.reverse();
    name
  }

  // TODO This needs optimisation, in case inherited_vars has a long sequence of used minified names (likely).
  pub fn generate_next_available_minified_name(
    &mut self,
    inherited_vars: &SessionHashSet<Identifier<'a>>,
  ) -> Identifier<'a> {
    loop {
      let name = self.transition_to_next_possible_minified_name();
      if KEYWORD_STRS.contains_key(name.as_slice()) {
        continue;
      };
      let name = self
        .session
        .get_allocator()
        .alloc_slice_copy(name.as_slice());
      let as_ident = SourceRange::new(name, 0, name.len());
      if inherited_vars.contains(&as_ident) {
        continue;
      };
      return as_ident;
    }
  }
}

// This should be run after Pass2 and before Pass3 visitor runs.
// The Pass1 pass collects all usages of variables to determine inherited variables for each scope, so we can know what minified names can be safely used (see `MinifiedNameGenerator`). This function will then go through each declaration in each scope and generate and update their corresponding `MinifySymbol.minified_name`.
// Some pecularities to note: globals aren't minified (whether declared or not), so when blacklisting minified names, they are directly disallowed. However, all other variables will be minified, so we need to blacklist their minified name, not their original name. This is why this function processes scopes top-down (from the root), as we need to know the minified names of ancestor variables first before we can blacklist them.
pub fn minify_names<'a>(
  session: &'a Session,
  scope: Scope<'a>,
  minify_scopes: &mut SessionHashMap<'a, Scope<'a>, MinifyScope<'a>>,
  minify_symbols: &mut SessionHashMap<'a, Symbol, MinifySymbol<'a>>,
) {
  // It's possible that the entry doesn't exist, if there were no inherited variables during the first pass.
  let minify_scope = minify_scopes
    .entry(scope)
    .or_insert_with(|| MinifyScope::new(session));
  // Our `inherited_vars` contains original names; we need to retrieve their minified names.
  let mut minified_inherited_vars = session.new_hashset();
  for &original_inherited_var in minify_scope.inherited_vars.iter() {
    match scope.find_symbol(original_inherited_var) {
      None => {
        // Global (undeclared or declared).
        minified_inherited_vars.insert(original_inherited_var);
      }
      Some(sym) => {
        let min_sym = minify_symbols.get(&sym).unwrap();
        let min_name = min_sym.minified_name.unwrap();
        minified_inherited_vars.insert(min_name);
      }
    };
  }
  // Yes, we start from the very beginning in case there are possible gaps/opportunities due to inherited variables on ancestors.
  let mut next_min_name = MinifiedNameGenerator::new(session);
  for &sym_name in scope.symbol_names().iter() {
    let sym = scope.get_symbol(sym_name).unwrap();
    let min_sym = minify_symbols
      .entry(sym)
      .or_insert_with(|| MinifySymbol::new(session));
    assert!(min_sym.minified_name.is_none());
    if min_sym.is_used_as_jsx_component {
      // We'll process these in another iteration, as there's fewer characters allowed for the identifier start, and we don't want to skip past valid identifiers for non-JSX-component names.
      continue;
    };
    min_sym.minified_name =
      Some(next_min_name.generate_next_available_minified_name(&minified_inherited_vars));
  }
  for &sym_name in scope.symbol_names().iter() {
    let sym = scope.get_symbol(sym_name).unwrap();
    let min_sym = minify_symbols.get_mut(&sym).unwrap();
    if !min_sym.is_used_as_jsx_component {
      continue;
    };
    // TODO This is very slow and dumb.
    let mut min_name;
    loop {
      min_name = next_min_name.generate_next_available_minified_name(&minified_inherited_vars);
      if !min_name.as_slice()[0].is_ascii_lowercase() {
        break;
      };
    }
    min_sym.minified_name = Some(min_name)
  }
  for &c in scope.children().iter() {
    minify_names(session, c, minify_scopes, minify_symbols);
  }
}
