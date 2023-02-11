use crate::{tokens::*, utils::Span};

#[derive(Debug, PartialEq)]
pub struct Module {
    pub body: Vec<Declaration>,
    pub submodules: Vec<Module>,
    pub requirements: Vec<Module>,
    pub name: String,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum Declaration {
    Function(FunctionDeclaration),
    Class(ClassDeclaration),
    Module(Module),
}

#[derive(Debug, PartialEq)]
pub struct FunctionDeclaration {
    pub name: String,
    pub return_type: Option<String>,
    pub params: Vec<FunctionParameter>,
    pub body: Block,
    pub variadic: bool,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct ClassDeclaration {
    pub name: String,
    pub fields: Vec<ClassField>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct ClassField {
    pub field_name: String,
    pub type_name: String,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct ClassInitializer {
    pub class_name: String,
    pub fields: Vec<ClassInitializerField>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct ClassInitializerField {
    pub field_name: String,
    pub value: Expression,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct FunctionCall {
    pub callee: Box<Expression>,
    pub args: Vec<Expression>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct FunctionParameter {
    pub name: String,
    pub type_name: String,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    BinaryOp {
        left: Box<Expression>,
        right: Box<Expression>,
        op: Operator,
    },
    LogicalOp {
        left: Box<Expression>,
        right: Box<Expression>,
        op: Operator,
    },
    UnaryOp {
        expr: Box<Expression>,
        op: Operator,
    },
    Identifier(String),
    Literal(Literal),
    If(IfExpression),
    Block(Block),
    VarAssignment(VarAssignment),
    FnCall(FunctionCall),
    MemberAccess(MemberAccess),
    StructInitializer(ClassInitializer),
}

#[derive(Debug, PartialEq)]
pub struct MemberAccess {
    pub object: Box<Expression>,
    pub member: Box<Expression>,
    pub computed: bool,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    VariableDeclaration(VariableDeclaration),
    ExpressionStatement(Expression),
    LoopStatement(LoopStatement),
    ReturnStatement(ReturnStatement),
    ContinueStatement,
    BreakStatement,
}

#[derive(Debug, PartialEq)]
pub struct VarAssignment {
    pub operator: AssignmentOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct VariableDeclaration {
    pub name: String,
    pub type_name: String,
    pub initializer: Option<Expression>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub body: Box<Statement>,
    pub else_body: Box<Option<Statement>>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct LoopStatement {
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct ReturnStatement {
    pub value: Option<Expression>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub span: Span,
}
