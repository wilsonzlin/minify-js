use std::io::BufWriter;

use crate::emit::emit_js;
use crate::parse::toplevel::parse_top_level;
use crate::util::test::*;

fn check(src: &str, expected: &str) -> () {
    let mut parser = p(src);
    let node = parse_top_level(&mut parser).unwrap();
    let mut out = BufWriter::new(Vec::new());
    emit_js(&mut out, &node).unwrap();
    assert_eq!(
        unsafe { std::str::from_utf8_unchecked(out.get_ref().as_slice()) },
        expected
    );
}

#[test]
fn test_emit() {
    check(
        r#"
          /* Test code */

          a.b.c
          module.functions

          var a = 1, b, { c, d: [ e, f, , , ...g ], ...h } = i;

          (( {a} = 1, [b] = 2 ) => {
            {
              let x = y;
            }
            a,b
            ;
            return
            1.2.toString()
          })();;;

          const a = ({}) => {}
          const b = (a) => {}, c = (1/7)/(2/7)
        "#,
        "a.b.c;\
        module.functions;\
        var a=1,b,{c,d:[e,f,,,...g],...h}=i;\
        ({a}=1,[b]=2)=>{{let x=y;}a,b;return ;1.2.toString();}();\
        const a=({})=>{};\
        const b=a=>{},c=(1/7)/(2/7);\
        ",
    )
}
