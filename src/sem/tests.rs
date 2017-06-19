use super::*;

use basis::pos::Source;
use syntax;

fn run_semantic_check(input: &str) -> ::std::result::Result<ir::Program, String> {
    let source = Source::with_dummy(input.to_string());
    // We're testing semantic checking, not syntax parsing.
    let nodes = syntax::parse(&source).unwrap();
    let mut ctx = Context::new();
    ctx.check_and_transform(nodes)
        .map_err(|err| err.with_source(&source).to_string())
}

mod success {
    use super::*;

    #[test]
    fn simple_literal() {
        let input = r#"

            def f(): Bool = {
                1
                true
                false
            }

"#;
        let program = run_semantic_check(input).unwrap();
        assert_eq!(program.entries.len(), 1);
        assert_eq!(program.inits.len(), 0);
    }

    #[test]
    fn arithmetic_infix_operations() {
        let input = r#"

            def f(): Float = {
                1 - 2
                1 + 2
                1 * 2
                1 / 2
                1.0 - 2.0
                1.0 + 2.0
                1.0 * 2.0
                1.0 / 2.0
            }

"#;
        let program = run_semantic_check(input).unwrap();
        assert_eq!(program.entries.len(), 1);
        assert_eq!(program.inits.len(), 0);
    }

    #[test]
    fn global_identifier() {
        let input = r#"

            def f(): Int = {
                let x: Int = 0
                let y: Int = x
                x + y
            }

"#;
        let program = run_semantic_check(input).unwrap();
        assert_eq!(program.entries.len(), 1);
        assert_eq!(program.inits.len(), 0);
    }

    #[test]
    fn function_definition() {
        let input = r#"

            def add(x: Int, y: Int): Int = x + y

            def nop() = { }

            def local_variables(): Int = {
                let x = 0
                x
            }

"#;
        let program = run_semantic_check(input).unwrap();
        assert_eq!(program.entries.len(), 3);
        assert_eq!(program.inits.len(), 0);
    }

    #[test]
    fn function_call() {
        let input = r#"

            def add(x: Int, y: Int): Int = x + y

            let x: Int = 0
            let y: Int = 1
            let z: Int = add(0, 1)

"#;
        let program = run_semantic_check(input).unwrap();
        assert_eq!(program.entries.len(), 4);
        assert_eq!(program.inits.len(), 3);
    }

    #[test]
    fn if_expression() {
        let input = r#"

            let cond: Bool = true
            let x: Int = if cond {
                1
            } else {
                2
            }

"#;
        let program = run_semantic_check(input).unwrap();
        assert_eq!(program.entries.len(), 2);
        assert_eq!(program.inits.len(), 2);
    }

    #[test]
    fn ref_deref_assignment() {
        let input = r#"

            def foo() = {
                let x = ref(0)
                let y = @x
                x <- y + 1
            }

            let x: Ref[Int] = ref(0)
            let y: Int = @x

"#;
        let program = run_semantic_check(input).unwrap();
        assert_eq!(program.entries.len(), 3);
        assert_eq!(program.inits.len(), 2);
    }
}

mod failure {
    use super::*;
    use basis::errors::disable_colorized_error;

    fn run(input: &str, expected_messages: &[&str]) {
        disable_colorized_error();
        let err = run_semantic_check(input).unwrap_err();
        for e in expected_messages {
            assert!(err.contains(e),
                    "expected error contains {:?}, but got error is {:?}",
                    e,
                    err.to_string());
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
        run(input,
            &["<dummy>:5:26", "invalid number", "expected 2", "given 1"]);
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

        run(input,
            &["<dummy>:6:17", "mismatched types", "'if' and 'else'"]);
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

        run(input,
            &["<dummy>:5:13",
              "duplicate definition",
              "<dummy>:3:13: note",
              "previous definition"]);
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
