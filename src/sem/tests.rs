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

        1
        true
        false

"#;
        let program = run_semantic_check(input).unwrap();
        assert!(program.entries.is_empty());
        assert_eq!(program.toplevels.len(), 3);
    }

    #[test]
    fn arithmetic_infix_operations() {
        let input = r#"

            1 - 2
            1 + 2
            1 * 2
            1 / 2
            1.0 - 2.0
            1.0 + 2.0
            1.0 * 2.0
            1.0 / 2.0

"#;
        let program = run_semantic_check(input).unwrap();
        assert!(program.entries.is_empty());
        assert_eq!(program.toplevels.len(), 8);
    }

    #[test]
    fn global_identifier() {
        let input = r#"

            let x: int = 0
            let y: int = x
            x + y

"#;
        let program = run_semantic_check(input).unwrap();
        assert_eq!(program.entries.len(), 2);
        assert_eq!(program.toplevels.len(), 3);
    }

    #[test]
    fn function_definition() {
        let input = r#"

            def add(x: int, y: int): int = x + y

            def nop() = { }

            def local_variables(): int = {
                let x = 0
                x
            }

"#;
        let program = run_semantic_check(input).unwrap();
        assert_eq!(program.entries.len(), 3);
        assert_eq!(program.toplevels.len(), 0);
    }

    #[test]
    fn function_call() {
        let input = r#"

            def add(x: int, y: int): int = x + y

            let x: int = 0
            let y: int = 1
            let z: int = add(0, 1)

"#;
        let program = run_semantic_check(input).unwrap();
        assert_eq!(program.entries.len(), 4);
        assert_eq!(program.toplevels.len(), 3);
    }

    #[test]
    fn if_expression() {
        let input = r#"

            let cond: bool = true
            let x: int = if cond {
                1
            } else {
                2
            }

"#;
        let program = run_semantic_check(input).unwrap();
        assert_eq!(program.entries.len(), 2);
        assert_eq!(program.toplevels.len(), 2);
    }
}
