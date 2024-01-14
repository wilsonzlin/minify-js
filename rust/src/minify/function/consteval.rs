use super::inst::BinOp;
use super::inst::BinOp::*;
use super::inst::Const;
use super::inst::Const::*;
use super::inst::UnOp;
use super::inst::UnOp::*;
use parse_js::num::JsNumber as JN;
use std::cmp::Ordering;
use std::f64::consts::E;
use std::f64::consts::PI;
use std::mem::discriminant;
use std::str::FromStr;

/**
 * NOTES ON BUILTINS
 *
 * We often intentionally skip const evaluating builtin values (i.e. at least one arg is a Arg::Builtin). Their values are opaque to us.
 * Yes technically we have the list of all builtins. But we may have forgotten some or new ones may be added in the future and we haven't implemented them yet (and our compiler shouldn't emit incorrect code even then). We don't want to give an incorrect answer.
 * Note that all of these are unsafe:
 * - Checking if they strictly equal. Even if paths are identical, they could point to NaN (e.g. `Number.NaN`); even if paths are unidentical, they could still point to the same object (e.g. `Number.POSITIVE_INFINITY` and `Infinity`). It's incorrect to return either true or false, because there are exceptions in both cases. And these exceptions could change in the future, even if our compiler doesn't, but our compiler still has to be correct then.
 * - Checking if either is null or undefined. A builtin could be null or undefined. Accessing an unknown property on a builtin object results in undefined *today* but may not in the future.
 * - Even `void (Builtin)` is not safe because the builtin path may not exist and we could be suppressing an error.
 */

// TODO Verify num_bigint::BigInt::from_str matches ECMAScript spec.
pub(crate) fn parse_bigint(raw: &str) -> Option<num_bigint::BigInt> {
  num_bigint::BigInt::from_str(raw).ok()
}

// TODO Verify f64::parse matches ECMAScript spec.
pub(crate) fn coerce_bigint_to_num(v: &num_bigint::BigInt) -> f64 {
  v.to_string().parse().unwrap()
}

// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number#number_coercion
pub(crate) fn coerce_str_to_num(raw: &str) -> f64 {
  let raw = raw.trim();
  if raw.is_empty() {
    return 0.0;
  };
  if matches!(raw, "+Infinity" | "Infinity") {
    return f64::INFINITY;
  };
  if raw == "-Infinity" {
    return f64::NEG_INFINITY;
  };
  let raw = raw.strip_prefix("+").unwrap_or(raw);
  // TODO Verify Rust's f64::parse matches ECMAScript exactly.
  match raw.parse::<f64>() {
    Ok(v) => v,
    Err(_) => f64::NAN,
  }
}

// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-tonumber
pub(crate) fn coerce_to_num(v: &Const) -> f64 {
  match v {
    BigInt(_) => panic!("cannot coerce bigint to num according to spec"),
    Bool(false) => 0.0,
    Bool(true) => 1.0,
    Null => 0.0,
    Num(v) => v.0,
    Str(v) => coerce_str_to_num(&v),
    Undefined => f64::NAN,
  }
}

// https://developer.mozilla.org/en-US/docs/Glossary/Falsy
pub(crate) fn coerce_to_bool(v: &Const) -> bool {
  match v {
    BigInt(v) => v == &num_bigint::BigInt::from(0),
    Bool(b) => *b,
    Null => false,
    Num(JN(v)) => !v.is_nan() && *v != 0.0,
    Str(v) => !v.is_empty(),
    Undefined => false,
  }
}

// If return value is None, then all comparison operators between `a` and `b` result in false.
// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-islessthan
pub(crate) fn js_cmp(a: &Const, b: &Const) -> Option<Ordering> {
  match (a, b) {
    (Str(a), Str(b)) => Some(a.cmp(b)),
    (Str(a), BigInt(b)) => parse_bigint(a).map(|a| a.cmp(b)),
    (BigInt(a), Str(b)) => parse_bigint(b).map(|b| a.cmp(&b)),
    (BigInt(a), BigInt(b)) => Some(a.cmp(b)),
    (a, b) => {
      // https://tc39.es/ecma262/multipage/ecmascript-data-types-and-values.html#sec-numeric-types-number-lessThan
      let a = coerce_to_num(a);
      let b = coerce_to_num(b);
      if a.is_nan() || b.is_nan() {
        None
      } else {
        Some(a.partial_cmp(&b).unwrap())
      }
    }
  }
}

pub(crate) fn js_div(a: f64, b: f64) -> f64 {
  #[allow(illegal_floating_point_literal_pattern)]
  match (a, b) {
    (a, 0.0) if a > 0.0 => f64::INFINITY,
    (a, 0.0) if a < 0.0 => f64::NEG_INFINITY,
    (0.0, 0.0) => f64::NAN,
    _ => a / b,
  }
}

pub(crate) fn js_mod(a: f64, b: f64) -> f64 {
  #[allow(illegal_floating_point_literal_pattern)]
  match (a, b) {
    (_, 0.0) => f64::NAN,
    (a, _) if a.is_infinite() => f64::NAN,
    _ => a % b,
  }
}

// https://tc39.es/ecma262/multipage/abstract-operations.html#sec-islooselyequal
pub(crate) fn js_loose_eq(a: &Const, b: &Const) -> bool {
  if discriminant(a) == discriminant(b) {
    return js_strict_eq(a, b);
  };
  match (a, b) {
    (Null, Undefined) => true,
    (Undefined, Null) => true,
    (Num(l), Str(r)) => l.0 == coerce_str_to_num(&r),
    (Str(l), Num(r)) => coerce_str_to_num(&l) == r.0,
    (BigInt(l), Str(r)) => parse_bigint(r).is_some_and(|r| l == &r),
    (Str(l), BigInt(r)) => parse_bigint(l).is_some_and(|l| &l == r),
    (Bool(l), r) => js_loose_eq(&Num(JN(*l as u8 as f64)), r),
    (l, Bool(r)) => js_loose_eq(l, &Num(JN(*r as u8 as f64))),
    (BigInt(l), Num(r)) => r.0.is_finite() && coerce_bigint_to_num(l) == r.0,
    (Num(l), BigInt(r)) => l.0.is_finite() && l.0 == coerce_bigint_to_num(r),
    _ => false,
  }
}

pub(crate) fn js_strict_eq(a: &Const, b: &Const) -> bool {
  match (a, b) {
    (Num(v), _) | (_, Num(v)) if v.0.is_nan() => false,
    (a, b) => a == b,
  }
}

pub(crate) fn maybe_eval_const_bin_expr(op: BinOp, a: &Const, b: &Const) -> Option<Const> {
  #[rustfmt::skip]
  let res = match (op, a, b) {
    (Add, Num(l), Num(r)) => Num(JN(l.0 + r.0)),
    (Add, Num(l), Str(r)) => Str(format!("{l}{r}")),
    (Add, Str(l), Num(r)) => Str(format!("{l}{r}")),
    (Add, Str(l), Str(r)) => Str(format!("{l}{r}")),
    (Div, Num(l), Num(r)) => Num(JN(js_div(l.0, r.0))),
    (Div, Num(l), Str(r)) => Num(JN(js_div(l.0, coerce_str_to_num(r)))),
    (Div, Str(l), Num(r)) => Num(JN(js_div(coerce_str_to_num(l), r.0))),
    (Div, Str(l), Str(r)) => Num(JN(js_div(coerce_str_to_num(l), coerce_str_to_num(r)))),
    (Geq, l, r) => Bool(js_cmp(l, r).is_some_and(|c| c.is_ge())),
    (Gt, l, r) => Bool(js_cmp(l, r).is_some_and(|c| c.is_gt())),
    (Leq, l, r) => Bool(js_cmp(l, r).is_some_and(|c| c.is_le())),
    (LooseEq, l, r) => Bool(js_loose_eq(l, r)),
    (Lt, l, r) => Bool(js_cmp(l, r).is_some_and(|c| c.is_lt())),
    (Mod, Num(l), Num(r)) => Num(JN(js_mod(l.0, r.0))),
    (Mod, Num(l), Str(r)) => Num(JN(js_mod(l.0, coerce_str_to_num(r)))),
    (Mod, Str(l), Num(r)) => Num(JN(js_mod(coerce_str_to_num(l), r.0))),
    (Mod, Str(l), Str(r)) => Num(JN(js_mod(coerce_str_to_num(l), coerce_str_to_num(r)))),
    (Mul, Num(l), Num(r)) => Num(JN(l.0 * r.0)),
    (Mul, Num(l), Str(r)) => Num(JN(l.0 * coerce_str_to_num(r))),
    (Mul, Str(l), Num(r)) => Num(JN(coerce_str_to_num(l) * r.0)),
    (Mul, Str(l), Str(r)) => Num(JN(coerce_str_to_num(l) * coerce_str_to_num(r))),
    (NotLooseEq, l, r) => Bool(!js_loose_eq(l, r)),
    (NotStrictEq, l, r) => Bool(!js_strict_eq(l, r)),
    (StrictEq, l, r) => Bool(js_strict_eq(l, r)),
    (Sub, Num(l), Num(r)) => Num(JN(l.0 - r.0)),
    (Sub, Num(l), Str(r)) => Num(JN(l.0 - coerce_str_to_num(r))),
    (Sub, Str(l), Num(r)) => Num(JN(coerce_str_to_num(l) - r.0)),
    (Sub, Str(l), Str(r)) => Num(JN(coerce_str_to_num(l) - coerce_str_to_num(r))),
    _ => return None,
  };
  Some(res)
}

pub(crate) fn maybe_eval_const_un_expr(op: UnOp, a: &Const) -> Option<Const> {
  #[rustfmt::skip]
  let res = match (op, a) {
    (Neg, Num(l)) => Num(JN(-l.0)),
    (Not, a) => Bool(!coerce_to_bool(a)),
    (Plus, BigInt(_)) => return None,
    (Plus, l) => Num(JN(coerce_to_num(&l))),
    (Typeof, BigInt(_)) => Str("bigint".into()),
    (Typeof, Bool(_)) => Str("boolean".into()),
    (Typeof, Null) => Str("object".into()),
    (Typeof, Num(_)) => Str("number".into()),
    (Typeof, Str(_)) => Str("string".into()),
    (Typeof, Undefined) => Str("undefined".into()),
    (Void, _) => Undefined,
    _ => return None,
  };
  Some(res)
}

pub(crate) fn maybe_eval_const_builtin_call(func: &str, args: &[Const]) -> Option<Const> {
  #[rustfmt::skip]
  let v = match args.len() {
    1 => match (func, &args[0]) {
      ("Math.cos", Num(a)) => Num(JN(a.0.cos())),
      ("Math.log", Num(a)) => Num(JN(a.0.ln())),
      ("Math.log10", Num(a)) => Num(JN(a.0.log10())),
      ("Math.log1p", Num(a)) => Num(JN(a.0.ln_1p())),
      ("Math.log2", Num(a)) => Num(JN(a.0.log2())),
      ("Number", Str(a)) => Num(JN(coerce_str_to_num(&a))),
      _ => return None,
    }
    _ => return None,
  };
  Some(v)
}

pub(crate) fn maybe_eval_const_builtin_val(path: &str) -> Option<Const> {
  #[rustfmt::skip]
  let v = match path {
    "Math.E" => Num(JN(E)),
    "Math.PI" => Num(JN(PI)),
    "NaN" => Num(JN(f64::NAN)),
    "Number.EPSILON" => Num(JN(f64::EPSILON)),
    "Number.MAX_SAFE_INTEGER" => Num(JN((2u64.pow(53) - 1) as f64)),
    "Number.MIN_SAFE_INTEGER" => Num(JN(-(2i64.pow(53) - 1) as f64)),
    "Number.NaN" => Num(JN(f64::NAN)),
    "Number.NEGATIVE_INFINITY" => Num(JN(f64::NEG_INFINITY)),
    "Number.POSITIVE_INFINITY" => Num(JN(f64::INFINITY)),
    _ => return None,
  };
  Some(v)
}
