#[derive(Debug, Clone)]
pub enum Type {
    Simple(String),      // int, string, array, 类名等
    Nullable(Box<Type>), // ?Type
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub ty: Option<Type>,
    pub by_ref: bool,
    pub variadic: bool,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Ident(String),
    Number(i64),
    String(String),
    Bool(bool),
    Null,
    Array(Vec<Expr>),
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
    },
    Member {
        object: Box<Expr>,
        property: String,
    },
    StaticMember {
        class_name: String,
        property: String,
    },
    Assign {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    NullCoalesceAssign {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    BinOp {
        left: Box<Expr>,
        op: BinOp,
        right: Box<Expr>,
    },
    ArrowFunction {
        params: Vec<Param>,
        return_type: Option<Type>,
        body: Box<Expr>,
    },
    // ... 可扩展其它表达式
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Neq,
    Identical,
    NotIdentical,
    Lt,
    Gt,
    Le,
    Ge,
    And,
    Or,
    NullCoalesce,
    // ...
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),
    Return(Option<Expr>),
    If {
        cond: Expr,
        then: Vec<Stmt>,
        else_: Option<Vec<Stmt>>,
    },
    While {
        cond: Expr,
        body: Vec<Stmt>,
    },
    For {
        init: Vec<Expr>,
        cond: Vec<Expr>,
        step: Vec<Expr>,
        body: Vec<Stmt>,
    },
    Break,
    Continue,
    FunctionDecl {
        name: String,
        params: Vec<Param>,
        return_type: Option<Type>,
        body: Vec<Stmt>,
    },
    Class {
        name: String,
        members: Vec<ClassMember>,
    },
}

#[derive(Debug, Clone)]
pub enum ClassMember {
    Property {
        name: String,
        ty: Option<Type>,
        is_static: bool,
        visibility: Visibility,
        default: Option<Expr>,
    },
    Method {
        name: String,
        params: Vec<Param>,
        return_type: Option<Type>,
        is_static: bool,
        visibility: Visibility,
        body: Vec<Stmt>,
    },
    Const {
        name: String,
        value: Expr,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum Visibility {
    Public,
    Protected,
    Private,
}
