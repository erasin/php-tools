use logos::Logos;

#[derive(Logos, Debug, Clone, PartialEq)]
pub enum Token {
    // 关键字
    #[token("fn")]
    Fn,
    #[token("function")]
    Function,
    #[token("class")]
    Class,
    #[token("public")]
    Public,
    #[token("protected")]
    Protected,
    #[token("private")]
    Private,
    #[token("static")]
    Static,
    #[token("const")]
    Const,
    #[token("return")]
    Return,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("while")]
    While,
    #[token("for")]
    For,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("new")]
    New,
    #[token("null")]
    Null,
    #[token("true")]
    True,
    #[token("false")]
    False,

    // 类型
    #[token("int")]
    Int,
    #[token("float")]
    Float,
    #[token("string")]
    StringType,
    #[token("bool")]
    Bool,
    #[token("array")]
    Array,
    #[token("void")]
    Void,
    #[token("self")]
    Self_,
    #[token("parent")]
    Parent,
    #[token("callable")]
    Callable,
    #[token("iterable")]
    Iterable,

    // 符号
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token(";")]
    Semi,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token("=")]
    Assign,
    #[token("=>")]
    FatArrow,
    #[token("->")]
    ObjectOp,
    #[token("::")]
    StaticOp,
    #[token("&")]
    Ref,
    #[token("...")]
    Ellipsis,
    #[token("?")]
    Question,
    #[token(".")]
    Dot,
    #[token("$")]
    Dollar,

    // 操作符
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,

    #[token("==")]
    Eq,
    #[token("===")]
    Identical,
    #[token("!=")]
    Neq,
    #[token("!==")]
    NotIdentical,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("<=")]
    Le,
    #[token(">=")]
    Ge,
    #[token("&&")]
    AndAnd,
    #[token("||")]
    OrOr,
    #[token("!")]
    Not,
    #[token("??")]
    NullCoalesce,
    #[token("??=")]
    NullCoalesceAssign,

    // 标识符
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident,

    // 数字
    #[regex("[0-9]+")]
    Number,

    // 字符串
    #[regex#""([^"\\]|\\.)*""#]
    #[regex#"'([^'\\]|\\.)*'"#]
    Str,

    // 跳过空白和注释
    #[regex(r"[ \t\r\n\f]+", logos::skip)]
    #[regex(r"//[^\n]*", logos::skip)]
    #[regex(r"/\*([^*]|\*[^/])*\*/", logos::skip)]

    #[error]
    Error,
}
