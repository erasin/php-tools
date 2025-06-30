use crate::ast::*;
use crate::lexer::Token;
use chumsky::prelude::*;

pub fn parse_type() -> impl Parser<Token, Type, Error = Simple<Token>> {
    just(Token::Question)
        .ignore_then(parse_type())
        .map(|ty| Type::Nullable(Box::new(ty)))
        .or(select! {
            Token::Int => Type::Simple("int".to_string()),
            Token::Float => Type::Simple("float".to_string()),
            Token::StringType => Type::Simple("string".to_string()),
            Token::Bool => Type::Simple("bool".to_string()),
            Token::Array => Type::Simple("array".to_string()),
            Token::Void => Type::Simple("void".to_string()),
            Token::Self_ => Type::Simple("self".to_string()),
            Token::Parent => Type::Simple("parent".to_string()),
            Token::Callable => Type::Simple("callable".to_string()),
            Token::Iterable => Type::Simple("iterable".to_string()),
            Token::Ident(name) => Type::Simple(name),
        })
}

fn parse_param() -> impl Parser<Token, Param, Error = Simple<Token>> {
    parse_type()
        .or_not()
        .then(just(Token::Ref).or_not().map(|b| b.is_some()))
        .then(just(Token::Ellipsis).or_not().map(|b| b.is_some()))
        .then_ignore(just(Token::Dollar))
        .then(select! { Token::Ident(name) => name })
        .map(|(((ty, by_ref), variadic), name)| Param {
            name,
            ty,
            by_ref,
            variadic,
        })
}

// 表达式优先级递归下降
fn parse_expr_atom() -> impl Parser<Token, Expr, Error = Simple<Token>> {
    select! {
        Token::Ident(name) => Expr::Ident(name),
        Token::Number(num) => Expr::Number(num.parse().unwrap()),
        Token::Str(s)      => Expr::String(s),
        Token::Null        => Expr::Null,
        Token::True        => Expr::Bool(true),
        Token::False       => Expr::Bool(false),
    }
    .or(just(Token::LParen)
        .ignore_then(parse_expr())
        .then_ignore(just(Token::RParen)))
}

fn parse_expr_binop() -> impl Parser<Token, Expr, Error = Simple<Token>> {
    let atom = parse_expr_atom();

    // 优先级从高到底
    let mul_div = atom
        .clone()
        .then(
            just(Token::Star)
                .to(BinOp::Mul)
                .or(just(Token::Slash).to(BinOp::Div))
                .or(just(Token::Percent).to(BinOp::Mod))
                .then(atom.clone())
                .repeated(),
        )
        .foldl(|lhs, (op, rhs)| Expr::BinOp {
            left: Box::new(lhs),
            op,
            right: Box::new(rhs),
        });

    let add_sub = mul_div
        .clone()
        .then(
            just(Token::Plus)
                .to(BinOp::Add)
                .or(just(Token::Minus).to(BinOp::Sub))
                .then(mul_div.clone())
                .repeated(),
        )
        .foldl(|lhs, (op, rhs)| Expr::BinOp {
            left: Box::new(lhs),
            op,
            right: Box::new(rhs),
        });

    // 这里只实现部分优先级，可继续扩展
    add_sub
}

pub fn parse_expr() -> impl Parser<Token, Expr, Error = Simple<Token>> {
    parse_expr_binop()
}

fn parse_stmt() -> impl Parser<Token, Stmt, Error = Simple<Token>> {
    // 支持 if、while、return、expr、function/class/...
    let expr_stmt = parse_expr().then_ignore(just(Token::Semi)).map(Stmt::Expr);

    let return_stmt = just(Token::Return)
        .ignore_then(parse_expr().or_not())
        .then_ignore(just(Token::Semi))
        .map(Stmt::Return);

    let function_decl = just(Token::Function)
        .ignore_then(select! { Token::Ident(name) => name })
        .then_ignore(just(Token::LParen))
        .then(parse_param().separated_by(just(Token::Comma)))
        .then_ignore(just(Token::RParen))
        .then(just(Token::Colon).ignore_then(parse_type()).or_not())
        .then_ignore(just(Token::LBrace))
        .then(parse_stmt().repeated())
        .then_ignore(just(Token::RBrace))
        .map(|((((name, params), ret_type), body))| Stmt::FunctionDecl {
            name,
            params,
            return_type: ret_type,
            body,
        });

    let class_decl = just(Token::Class)
        .ignore_then(select! { Token::Ident(name) => name })
        .then_ignore(just(Token::LBrace))
        .then(parse_class_member().repeated())
        .then_ignore(just(Token::RBrace))
        .map(|(name, members)| Stmt::Class { name, members });

    expr_stmt.or(return_stmt).or(function_decl).or(class_decl)
    // ... if/while/for 等
}

// 类成员解析
fn parse_class_member() -> impl Parser<Token, ClassMember, Error = Simple<Token>> {
    let property = select! {
        Token::Public => Visibility::Public,
        Token::Protected => Visibility::Protected,
        Token::Private => Visibility::Private,
    }
    .then(just(Token::Static).or_not().map(|b| b.is_some()))
    .then(parse_type().or_not())
    .then_ignore(just(Token::Dollar))
    .then(select! { Token::Ident(name) => name })
    .then(just(Token::Assign).ignore_then(parse_expr()).or_not())
    .then_ignore(just(Token::Semi))
    .map(
        |((((vis, is_static), ty), name), default)| ClassMember::Property {
            name,
            ty,
            is_static,
            visibility: vis,
            default,
        },
    );

    let method = select! {
        Token::Public => Visibility::Public,
        Token::Protected => Visibility::Protected,
        Token::Private => Visibility::Private,
    }
    .then(just(Token::Static).or_not().map(|b| b.is_some()))
    .then_ignore(just(Token::Function))
    .then(select! { Token::Ident(name) => name })
    .then_ignore(just(Token::LParen))
    .then(parse_param().separated_by(just(Token::Comma)))
    .then_ignore(just(Token::RParen))
    .then(just(Token::Colon).ignore_then(parse_type()).or_not())
    .then_ignore(just(Token::LBrace))
    .then(parse_stmt().repeated())
    .then_ignore(just(Token::RBrace))
    .map(
        |((((((vis, is_static), name), params), ret_type), body))| ClassMember::Method {
            name,
            params,
            return_type: ret_type,
            is_static,
            visibility: vis,
            body,
        },
    );

    property.or(method)
    // 可继续支持 const 等
}
