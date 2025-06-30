use chumsky::Parser;
use php_parser::lexer::Token;
use php_parser::parser::{parse_expr, parse_stmt};

/// Helper: Lex input string into tokens
fn lex(input: &str) -> Vec<Token> {
    let mut lexer = Token::lexer(input);
    let mut tokens = vec![];
    while let Some(token) = lexer.next() {
        if token != Token::Error {
            tokens.push(token);
        }
    }
    tokens
}

/// Helper: Parse expression from string
fn parse_expr_from_str(
    input: &str,
) -> Result<php_parser::ast::Expr, Vec<chumsky::error::Simple<Token>>> {
    let tokens = lex(input);
    parse_expr().parse(tokens)
}

/// Helper: Parse statement from string
fn parse_stmt_from_str(
    input: &str,
) -> Result<php_parser::ast::Stmt, Vec<chumsky::error::Simple<Token>>> {
    let tokens = lex(input);
    parse_stmt().parse(tokens)
}

// --- PHP 7.4 新语法 ---

#[test]
fn test_arrow_function_php74() {
    // PHP 7.4
    // fn($x) => $x + 1
    let code = "fn($x) => $x + 1;";
    let ast = parse_expr_from_str(code).expect("Should parse arrow function");
    match ast {
        php_parser::ast::Expr::ArrowFunction { params, .. } => {
            assert_eq!(params.len(), 1);
            assert_eq!(params[0].name, "x");
        }
        _ => panic!("Expected ArrowFunction"),
    }
}

#[test]
fn test_typed_property_php74() {
    // PHP 7.4
    // public int $age;
    let code = "public int $age;";
    let tokens = lex(code);
    use php_parser::parser::parse_class_member;
    let member = parse_class_member()
        .parse(tokens)
        .expect("Should parse property");
    match member {
        php_parser::ast::ClassMember::Property { ty, name, .. } => {
            assert_eq!(name, "age");
            assert!(matches!(ty, Some(php_parser::ast::Type::Simple(ref s)) if s == "int"));
        }
        _ => panic!("Expected Property"),
    }
}

#[test]
fn test_null_coalesce_assign_php74() {
    // PHP 7.4
    // $a ??= 1;
    let code = "$a ??= 1;";
    let ast = parse_expr_from_str(code).expect("Should parse null coalesce assign");
    match ast {
        php_parser::ast::Expr::NullCoalesceAssign { .. } => {}
        _ => panic!("Expected NullCoalesceAssign"),
    }
}

#[test]
fn test_array_unpack_php74() {
    // PHP 7.4
    // $arr = [1, ...$rest];
    let code = "$arr = [1, ...$rest];";
    let ast = parse_expr_from_str(code).expect("Should parse array unpack");
    match ast {
        php_parser::ast::Expr::Assign { right, .. } => match *right {
            php_parser::ast::Expr::Array(ref items) => {
                assert_eq!(items.len(), 2);
            }
            _ => panic!("Expected Array"),
        },
        _ => panic!("Expected Assign"),
    }
}

#[test]
fn test_typed_function_php74() {
    // PHP 7.4
    // function sum(int $a, int $b): int { return $a + $b; }
    let code = "function sum(int $a, int $b): int { return $a + $b; }";
    let stmt = parse_stmt_from_str(code).expect("Should parse typed function");
    match stmt {
        php_parser::ast::Stmt::FunctionDecl {
            name,
            params,
            return_type,
            ..
        } => {
            assert_eq!(name, "sum");
            assert_eq!(params.len(), 2);
            assert!(
                matches!(return_type, Some(php_parser::ast::Type::Simple(ref s)) if s == "int")
            );
        }
        _ => panic!("Expected FunctionDecl"),
    }
}

#[test]
fn test_class_with_typed_property_and_method_php74() {
    // PHP 7.4
    let code = r#"
        class User {
            public int $id;
            public function getId(): int { return $this->id; }
        }
    "#;
    let stmt = parse_stmt_from_str(code).expect("Should parse class");
    match stmt {
        php_parser::ast::Stmt::Class { name, members } => {
            assert_eq!(name, "User");
            assert_eq!(members.len(), 2);
        }
        _ => panic!("Expected Class"),
    }
}

#[test]
fn test_short_closure_in_array_map_php74() {
    // PHP 7.4
    // array_map(fn($x) => $x * 2, $nums);
    let code = "array_map(fn($x) => $x * 2, $nums);";
    let ast = parse_expr_from_str(code).expect("Should parse arrow function in array_map");
    match ast {
        php_parser::ast::Expr::Call { func, args } => {
            match *func {
                php_parser::ast::Expr::Ident(ref s) if s == "array_map" => {}
                _ => panic!("Expected Ident(array_map)"),
            }
            assert!(matches!(
                args[0],
                php_parser::ast::Expr::ArrowFunction { .. }
            ));
        }
        _ => panic!("Expected Call"),
    }
}

// --- 早期 PHP 语法（PHP 5.x/7.0）---

#[test]
fn test_basic_function_php5() {
    // PHP 5.0+
    // function foo($x) { return $x + 1; }
    let code = "function foo($x) { return $x + 1; }";
    let stmt = parse_stmt_from_str(code).expect("Should parse basic function");
    match stmt {
        php_parser::ast::Stmt::FunctionDecl { name, params, .. } => {
            assert_eq!(name, "foo");
            assert_eq!(params.len(), 1);
            assert_eq!(params[0].name, "x");
        }
        _ => panic!("Expected FunctionDecl"),
    }
}

#[test]
fn test_basic_class_php5() {
    // PHP 5.0+
    let code = r#"
        class A {
            public $x;
            public function foo() { return $this->x; }
        }
    "#;
    let stmt = parse_stmt_from_str(code).expect("Should parse class");
    match stmt {
        php_parser::ast::Stmt::Class { name, members } => {
            assert_eq!(name, "A");
            assert!(members.iter().any(
                |m| matches!(m, php_parser::ast::ClassMember::Property { name, .. } if name == "x")
            ));
            assert!(members.iter().any(
                |m| matches!(m, php_parser::ast::ClassMember::Method { name, .. } if name == "foo")
            ));
        }
        _ => panic!("Expected Class"),
    }
}

#[test]
fn test_closure_php53() {
    // PHP 5.3+
    // $f = function($a) { return $a + 2; };
    let code = "$f = function($a) { return $a + 2; };";
    let ast = parse_expr_from_str(code).expect("Should parse closure");
    match ast {
        php_parser::ast::Expr::Assign { right, .. } => match *right {
            php_parser::ast::Expr::Function { params, .. } => {
                assert_eq!(params.len(), 1);
                assert_eq!(params[0].name, "a");
            }
            _ => panic!("Expected Function"),
        },
        _ => panic!("Expected Assign"),
    }
}

#[test]
fn test_scalar_type_hint_php70() {
    // PHP 7.0+
    // function foo(int $a): int { return $a; }
    let code = "function foo(int $a): int { return $a; }";
    let stmt = parse_stmt_from_str(code).expect("Should parse typed function");
    match stmt {
        php_parser::ast::Stmt::FunctionDecl {
            name,
            params,
            return_type,
            ..
        } => {
            assert_eq!(name, "foo");
            assert_eq!(params.len(), 1);
            assert!(
                matches!(params[0].ty, Some(php_parser::ast::Type::Simple(ref s)) if s == "int")
            );
            assert!(
                matches!(return_type, Some(php_parser::ast::Type::Simple(ref s)) if s == "int")
            );
        }
        _ => panic!("Expected FunctionDecl"),
    }
}

#[test]
fn test_null_coalesce_php70() {
    // PHP 7.0+
    // $x = $a ?? $b;
    let code = "$x = $a ?? $b;";
    let ast = parse_expr_from_str(code).expect("Should parse null coalesce");
    match ast {
        php_parser::ast::Expr::Assign { right, .. } => match *right {
            php_parser::ast::Expr::BinOp {
                op: php_parser::ast::BinOp::NullCoalesce,
                ..
            } => {}
            _ => panic!("Expected NullCoalesce BinOp"),
        },
        _ => panic!("Expected Assign"),
    }
}
