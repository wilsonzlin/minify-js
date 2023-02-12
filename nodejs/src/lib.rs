use minify_js::Session;
use neon::prelude::*;
use neon::types::buffer::TypedArray;
use std::str::FromStr;

fn minify(mut cx: FunctionContext) -> JsResult<JsBuffer> {
  let top_level_mode_raw = cx.argument::<JsString>(0).map(|v| v.value(&mut cx))?;
  let top_level_mode = match minify_js::TopLevelMode::from_str(&top_level_mode_raw) {
    Ok(m) => m,
    Err(_) => return cx.throw_type_error("invalid top-level mode"),
  };
  let src = cx.argument::<JsBuffer>(1)?;
  let mut out = Vec::new();
  // TODO Allow reuse by creating a JS function that creates a native object.
  let session = Session::new();
  let res = match minify_js::minify(&session, top_level_mode, src.as_slice(&mut cx), &mut out) {
    Ok(()) => Ok(JsBuffer::external(&mut cx, out)),
    // We can't call `cx.throw_error` here as `cx` is already borrowed, so we create the error string and then throw later.
    Err(err) => Err(format!("{:?}", err)),
  };
  match res {
    Ok(res) => Ok(res),
    Err(msg) => cx.throw_error(msg),
  }
}

#[neon::main]
fn main(mut cx: ModuleContext) -> NeonResult<()> {
  cx.export_function("minify", minify)?;
  Ok(())
}
