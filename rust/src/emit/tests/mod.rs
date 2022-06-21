use std::io::BufWriter;

use crate::emit::emit_js;
use crate::lex::Lexer;
use crate::minify::minify_js;
use crate::parse::parser::Parser;
use crate::parse::toplevel::parse_top_level;

fn check(src: &str, expected: &str) -> () {
    let mut parser = Parser::new(Lexer::new(src.as_bytes().to_vec()));
    let p = parse_top_level(&mut parser).unwrap();
    let mut out = BufWriter::new(Vec::new());
    let (mut node_map, mut scope_map) = parser.take();
    minify_js(&mut scope_map, &mut node_map, p.top_level_node_id);
    emit_js(&mut out, &mut node_map, p.top_level_node_id).unwrap();
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
          function * gen () {
            yield * "hello world!";
          }
          !() => {
            com.java.names.long
            module.functions

            function this_is_a_function_decl_not_expr() {
              this_is_a_function_decl_not_expr()
            }

            var the = 1, quick, { brown, _: [ fox, jumped, , , ...over ], ...lazy } = i;

            (( {the} = this_is_a_function_decl_not_expr, [quick] = 2 ) => {
              {
                let brown = this_is_a_function_decl_not_expr(fox);
              }
              the,quick,brown,fox
              ;
              return
              1.2.toString()
            })();;;

            const lorem = ({}) => {}
            const ipsum = (a) => {}, dolor = (1/7)/(2/7)
          }()
        "#,
        "\
        function*gen(){yield*\"hello world!\"}\
        !()=>{\
        com.java.names.long;\
        module.functions;\
        function a(){a()}\
        var b=1,c,{brown:d,_:[e,f,,,...g],...h}=i;\
        ({the:l}=a,[m]=2)=>{{let n=a(e)};l,m,d,e;return;1.2.toString()}();\
        const i=({})=>{};\
        const j=l=>{},k=(1/7)/(2/7)\
        }()\
        ",
    )
}
