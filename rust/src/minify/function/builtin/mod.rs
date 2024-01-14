use ahash::AHashMap;
use itertools::Itertools;
use once_cell::sync::Lazy;
use std::iter::once;
use std::ops::Deref;

// These are separate JS files so that any TypeScript-based IDE will provide nice syntax highlighting and quick verification that the types exist.
const DEFS: &str = include_str!("./builtin.js");
const DEFS_TYPEDARRAY: &str = include_str!("./builtin.TypedArray.js");

#[derive(Default)]
pub(crate) struct BuiltinEntries(AHashMap<&'static str, BuiltinEntries>);

impl Deref for BuiltinEntries {
  type Target = AHashMap<&'static str, BuiltinEntries>;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

pub(crate) static BUILTINS: Lazy<BuiltinEntries> = Lazy::new(|| {
  let typedarray_defs = DEFS_TYPEDARRAY
    .split("\n")
    .filter(|l| !l.is_empty() || l.starts_with("//"))
    .map(|def| {
      def
        .strip_prefix("TypedArray.")
        .unwrap()
        .split("\n")
        .collect_vec()
    })
    .collect_vec();

  let mut map: BuiltinEntries = Default::default();

  for def in DEFS.split("\n") {
    if def.is_empty() || def.starts_with("//") {
      continue;
    };
    if let Some((cls, _)) = def.rsplit_once("(TypedArray)") {
      for subpath in typedarray_defs.iter() {
        let mut cur = map.0.entry(cls).or_default();
        for p in subpath {
          cur = cur.0.entry(p).or_default();
        }
      }
    } else {
      let mut cur = &mut map;
      for p in def.split(".") {
        cur = cur.0.entry(p).or_default();
      }
    };
  }

  map
});

pub(crate) fn check_if_builtin_exists(base: &[String], prop: &String) -> bool {
  let mut cur = &*BUILTINS;
  for p in base.iter().chain(once(prop)) {
    let Some(c) = cur.get(p.as_str()) else {
      return false;
    };
    cur = c;
  }
  true
}
