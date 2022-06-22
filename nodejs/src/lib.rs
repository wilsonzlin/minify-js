use neon::prelude::*;
use neon::types::buffer::TypedArray;

fn minify(mut cx: FunctionContext) -> JsResult<JsBuffer> {
    let src = cx.argument::<JsBuffer>(0)?;
    let mut out = Vec::new();
    match minify_js::minify(src.as_slice(&mut cx).to_vec(), &mut out) {
        Ok(()) => Ok(JsBuffer::external(&mut cx, out)),
        Err(err) => cx.throw_error(format!("{:?}", err)),
    }
}

#[neon::main]
fn main(mut cx: ModuleContext) -> NeonResult<()> {
    cx.export_function("minify", minify)?;
    Ok(())
}
