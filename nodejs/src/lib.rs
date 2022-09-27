use neon::prelude::*;
use neon::types::buffer::TypedArray;

fn minify(mut cx: FunctionContext) -> JsResult<JsBuffer> {
    let top_level_type_raw = cx
        .argument::<JsString>(0)
        .and_then(|v| v.to_string(&mut cx))?;
    let top_level_type = minify_js::TopLevelType::from_str(top_level_type_raw)
        .map_err(|| cx.type_error("invalid top-level type"))?;
    let src = cx.argument::<JsBuffer>(1)?;
    let mut out = Vec::new();
    match minify_js::minify(top_level_type, src.as_slice(&mut cx).to_vec(), &mut out) {
        Ok(()) => Ok(JsBuffer::external(&mut cx, out)),
        Err(err) => cx.throw_error(format!("{:?}", err)),
    }
}

#[neon::main]
fn main(mut cx: ModuleContext) -> NeonResult<()> {
    cx.export_function("minify", minify)?;
    Ok(())
}
