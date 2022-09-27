use std::str::FromStr;

use neon::prelude::*;
use neon::types::buffer::TypedArray;

fn minify(mut cx: FunctionContext) -> JsResult<JsBuffer> {
    let top_level_mode_raw = cx.argument::<JsString>(0).map(|v| v.value(&mut cx))?;
    let top_level_mode = match minify_js::TopLevelMode::from_str(&top_level_mode_raw) {
        Ok(m) => m,
        Err(_) => return cx.throw_type_error("invalid top-level mode"),
    };
    let src = cx.argument::<JsBuffer>(1)?;
    let mut out = Vec::new();
    match minify_js::minify(top_level_mode, src.as_slice(&mut cx).to_vec(), &mut out) {
        Ok(()) => Ok(JsBuffer::external(&mut cx, out)),
        Err(err) => cx.throw_error(format!("{:?}", err)),
    }
}

#[neon::main]
fn main(mut cx: ModuleContext) -> NeonResult<()> {
    cx.export_function("minify", minify)?;
    Ok(())
}
