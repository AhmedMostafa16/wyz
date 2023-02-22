use std::{cell::RefCell, rc::Rc};

use crate::{tokens::*, utils::Span};

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Module {
    pub function_definitions: Vec<FunctionDefinition>,
    pub function_declarations: Vec<FunctionDeclaration>,
    pub classes: Vec<ClassDeclaration>,
    pub submodules: Vec<Rc<RefCell<Module>>>,
    pub global_variables: Vec<Statement>,
    pub imports: Vec<Import>,
    pub parent: Option<Rc<RefCell<Module>>>,
    pub name: String,
    pub path: Vec<String>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDeclaration {
    pub name: String,
    pub return_type: Option<String>,
    pub params: Vec<FunctionParameter>,
    pub variadic: bool,
    pub is_extern: bool,
    pub is_public: bool,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDefinition {
    pub function_declaration: FunctionDeclaration,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ClassDeclaration {
    pub name: String,
    pub fields: Vec<ClassField>,
    pub is_public: bool,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ClassField {
    pub field_name: String,
    pub type_name: String,
    pub index: u32,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ClassInitializer {
    pub class_name: String,
    pub fields: Vec<ClassInitializerField>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ClassInitializerField {
    pub field_name: String,
    pub value: Expression,
    pub index: u32,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionCall {
    pub callee: Box<Expression>,
    pub args: Vec<Expression>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionParameter {
    pub name: String,
    pub type_name: String,
    pub index: u32,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    BinaryOp {
        left: Box<Expression>,
        right: Box<Expression>,
        operator: Operator,
        span: Span,
    },
    LogicalOp {
        left: Box<Expression>,
        right: Box<Expression>,
        operator: Operator,
        span: Span,
    },
    UnaryOp {
        expression: Box<Expression>,
        operator: Operator,
        span: Span,
    },
    Identifier {
        name: String,
    },
    Literal {
        literal: Literal,
        span: Span,
    },
    If {
        expression: IfExpression,
    },
    Block {
        block: Block,
    },
    VariableAssignment {
        variable_assignment: VariableAssignment,
    },
    FunctionCall {
        function_call: FunctionCall,
    },
    MemberAccess {
        member_access: MemberAccess,
    },
    ClassInitializer {
        class_initializer: ClassInitializer,
    },
    AsExpr {
        expr: Box<Expression>,
        type_name: String,
        span: Span,
    },
    ScopeResolution {
        scope_resolution: ScopeResolution,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct MemberAccess {
    pub object: Box<Expression>,
    pub member: Box<Expression>,
    pub computed: bool,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ScopeResolution {
    pub object: Box<Expression>,
    pub member: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    VariableDeclaration(VariableDeclaration),
    ExpressionStatement(Expression),
    LoopStatement(LoopStatement),
    ReturnStatement(ReturnStatement),
    ContinueStatement,
    BreakStatement,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VariableAssignment {
    pub operator: AssignmentOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VariableDeclaration {
    pub name: String,
    pub type_name: String,
    pub initializer: Option<Expression>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub body: Box<Expression>,
    pub else_body: Box<Option<Expression>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LoopStatement {
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStatement {
    pub value: Option<Expression>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Import {
    pub item_path: Vec<String>,
    pub local: bool,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum AccessLevel {
    None,
    Default,
    Public,
}
