use syntax::ast::Toplevel;

mod typed_ast;
pub use self::typed_ast::*;
mod type_env;
mod infer;
pub use self::infer::Infer;
mod deref;
pub use self::deref::*;

use Result;

pub fn typecheck(module_name: String, program: Vec<Toplevel>) -> Result<Module> {
    let mut infer = Infer::new();
    let mut module = infer.process(module_name, program)?;
    deref(&mut module)?;
    Ok(module)
}

#[cfg(test)]
mod tests {
    use super::*;
    use basis::sourcemap::SourceMap;
    use syntax;

    fn run_semantic_check(input: &str) -> ::std::result::Result<Module, String> {
        let mut srcmap = SourceMap::new();
        let source = srcmap.add_dummy(input.to_string());
        let nodes = syntax::parse(&srcmap, &*source).unwrap();
        typecheck("test".to_string(), nodes).map_err(|err| err.with_source_map(&srcmap).to_string())
    }

    #[test]
    fn simple_literal() {
        let input = r#"

            def f(): Bool = {
                1
                2.3
                true
                false
            }

        "#;
        let module = run_semantic_check(input).unwrap();
        assert_eq!(module.decls.len(), 1);
        assert!(module.decls.contains_key("f"))
    }

    #[test]
    fn infix_operations() {
        let input = r#"

            def f(): Bool = {
                2 == 1
            }

        "#;
        let module = run_semantic_check(input).unwrap();
        assert_eq!(module.decls.len(), 1);
        assert!(module.decls.contains_key("f"))
    }

    #[test]
    fn arithmetic_infix_operations() {
        let input = r#"

            def f(): Int = {
                2 + 1
            }

        "#;
        let module = run_semantic_check(input).unwrap();
        assert_eq!(module.decls.len(), 1);
        assert!(module.decls.contains_key("f"))
    }

    #[test]
    fn parameters() {
        let input = r#"

            def f(x: Int): Int = {
                x - 2
            }

        "#;
        let module = run_semantic_check(input).unwrap();
        assert_eq!(module.decls.len(), 1);
        assert!(module.decls.contains_key("f"));
    }

    #[test]
    fn funcall() {
        let input = r#"

            def f(x: Int): Int = {
                x - 2
            }

            def g(): Int = f(0)

        "#;
        let module = run_semantic_check(input).unwrap();
        assert_eq!(module.decls.len(), 2);
        assert!(module.decls.contains_key("f"));
        assert!(module.decls.contains_key("g"));
    }

    #[test]
    fn if_expression() {
        let input = r#"

            def f(x: Int): Int = {
                if x == 0 {
                    1
                } else {
                    x
                }
            }

        "#;
        let module = run_semantic_check(input).unwrap();
        assert_eq!(module.decls.len(), 1);
        assert!(module.decls.contains_key("f"));
    }

    #[test]
    fn while_expression() {
        let input = r#"

            def f(x: Int) = {
                while true {
                    x - 2
                }
            }

        "#;
        let module = run_semantic_check(input).unwrap();
        assert_eq!(module.decls.len(), 1);
        assert!(module.decls.contains_key("f"));
    }

    #[test]
    fn ref_deref_expression() {
        let input = r#"

            def f(x: Int): Int = {
                let n = ref(x)
                let y = @n
                n <- y + 1
                @n
            }

        "#;
        let module = run_semantic_check(input).unwrap();
        assert_eq!(module.decls.len(), 1);
        assert!(module.decls.contains_key("f"));
    }

    mod failure_cases {
        use super::*;
        use basis::errors::disable_colorized_error;

        fn run(input: &str, expected_messages: &[&str]) {
            disable_colorized_error();
            let err = run_semantic_check(input).unwrap_err().to_string();
            for e in expected_messages {
                assert!(
                    err.contains(e),
                    "expected error contains {:?}, but got error is {:?}",
                    e,
                    err.to_string()
                );
            }
        }

        #[test]
        fn global_identifier() {
            let input = r#"let x: Int = 0.0"#;
            run(input, &["<dummy>:1:14", "mismatched types"]);
        }

        #[test]
        fn arithmetic_infix_operations() {
            for op in &["+", "-", "/", "*"] {
                let input = format!(r#"def foo() = {{ let x = 1 {} 2.3; () }} "#, op);
                run(&input, &["<dummy>:1:27", "mismatched types"]);
            }
        }

        #[test]
        fn function_return_type() {
            let input = r#"

            def add(x: Int, y: Int): Float = x + y

        "#;
            run(input, &["<dummy>:3:46", "mismatched types"]);
        }

        #[test]
        fn function_parameter_type() {
            let input = r#"

            def add(x: Int, y: Int): Int = x + y

            let x: Int = add(1.0, 2.0)

        "#;
            run(input, &["<dummy>:5:30", "mismatched types"]);
        }

        #[test]
        fn function_parameter_number() {
            let input = r#"

            def add(x: Int, y: Int): Int = x + y

            let x: Int = add(0)

        "#;
            run(
                input,
                &["<dummy>:5:26", "invalid number", "expected 2", "given 1"],
            );
        }

        #[test]
        fn if_else_expression() {
            let input = r#"

            let x: Int = if true {
                1
            } else {
                2.0
            }

        "#;

            run(
                input,
                &[
                    "<dummy>:6:17",
                    "mismatched types",
                    "'then' clause and 'else' clause",
                ],
            );
        }

        #[test]
        fn if_expression() {
            let input = r#"

            let x: Int = if true {
                1
            }

        "#;

            run(input, &["<dummy>:4:17", "mismatched types"]);
        }

        #[test]
        fn name_conflict() {
            let input = r#"

            let foo: Int = 0

            def foo() = ()

        "#;

            run(
                input,
                &[
                    "<dummy>:5:13",
                    "duplicate definition",
                    "<dummy>:3:13: note",
                    "previous definition",
                ],
            );
        }

        #[test]
        fn non_ref_assignment() {
            let input = r#"

            def foo() = {
                let x = 1
                x <- 2
            }

        "#;

            run(input, &["<dummy>:5:17", "mismatched types", "Ref[_]"]);
        }
    }
}
