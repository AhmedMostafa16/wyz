use std::fmt::{Display, Formatter};

use crate::utils::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub slice: String,
    pub span: Span,
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Identifier(String),
    Literal(Literal),
    Operator(Operator),
    Keyword(Keyword),
    Symbol(Symbol),
}

#[repr(u8)]
#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    String(String),
    Integer(i64),
    Float(f64),
    Char(char),
    Bool(bool),
    HexidecmialNumber(i64),
    OctalNumber(i64),
    BinaryNumber(i64),
}

#[repr(u8)]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Keyword {
    Module,
    Class,
    Interface,
    Enum,
    Function,
    Extern,
    Let,
    Mutable,
    Var,
    If,
    Else,
    While,
    For,
    Foreach,
    Loop,
    Return,
    Unsafe,
    New,
    Static,
    Self_,
    Import,
    Extend,
    Continue,
    Break,
    Public,
}

#[repr(u8)]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Operator {
    Assign(AssignmentOperator),
    As,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Modulo,
    Power,
    Equals,
    NotEquals,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
    And,
    Or,
    Not,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseNot,
    BitwiseLeftShift,
    BitwiseRightShift,
}

impl Operator {
    pub fn op_type(&self) -> OperatorType {
        match self {
            Operator::Assign(_) => OperatorType::Assignment,
            Operator::Plus => OperatorType::Additive,
            Operator::Minus => OperatorType::Additive,
            Operator::Asterisk => OperatorType::Multiplicative,
            Operator::Slash => OperatorType::Multiplicative,
            Operator::Modulo => OperatorType::Multiplicative,
            Operator::Power => OperatorType::Multiplicative,
            Operator::Equals => OperatorType::Equality,
            Operator::NotEquals => OperatorType::Equality,
            Operator::LessThan => OperatorType::Relational,
            Operator::LessOrEqual => OperatorType::Relational,
            Operator::GreaterThan => OperatorType::Relational,
            Operator::GreaterOrEqual => OperatorType::Relational,
            Operator::And => OperatorType::LogicalAnd,
            Operator::Or => OperatorType::LogicalOr,
            Operator::Not => OperatorType::LogicalNot,
            Operator::BitwiseAnd => OperatorType::Bitwise,
            Operator::BitwiseOr => OperatorType::Bitwise,
            Operator::BitwiseXor => OperatorType::Bitwise,
            Operator::BitwiseNot => OperatorType::Bitwise,
            Operator::BitwiseLeftShift => OperatorType::Bitwise,
            Operator::BitwiseRightShift => OperatorType::Bitwise,
            Operator::As => OperatorType::Cast,
        }
    }

    pub fn is_unary(&self) -> bool {
        let t = self.op_type();
        if t == OperatorType::LogicalNot || t == OperatorType::Additive {
            return true;
        }
        match self {
            Operator::Plus | Operator::Minus | Operator::Not | Operator::Asterisk => true,
            _ => false,
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Assign(op) => {
                write!(f, "{:?}", op)
            }
            Operator::Plus => write!(f, "+"),
            Operator::Minus => write!(f, "-"),
            Operator::Asterisk => write!(f, "*"),
            Operator::Slash => write!(f, "/"),
            Operator::Modulo => write!(f, "%"),
            Operator::Power => write!(f, "**"),
            Operator::Equals => write!(f, "=="),
            Operator::NotEquals => write!(f, "!="),
            Operator::LessThan => write!(f, "<"),
            Operator::LessOrEqual => write!(f, "<="),
            Operator::GreaterThan => write!(f, ">"),
            Operator::GreaterOrEqual => write!(f, ">="),
            Operator::And => write!(f, "&&"),
            Operator::Or => write!(f, "||"),
            Operator::Not => write!(f, "!"),
            Operator::BitwiseAnd => write!(f, "&"),
            Operator::BitwiseOr => write!(f, "|"),
            Operator::BitwiseXor => write!(f, "^"),
            Operator::BitwiseNot => write!(f, "~"),
            Operator::BitwiseLeftShift => write!(f, "<<"),
            Operator::BitwiseRightShift => write!(f, ">>"),
            Operator::As => write!(f, "as"),
        }
    }
}

#[repr(u8)]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum OperatorType {
    Assignment,
    Additive,
    Multiplicative,
    LogicalAnd,
    LogicalOr,
    LogicalNot,
    Equality,
    Relational,
    Bitwise,
    Cast,
}

#[repr(u8)]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum AssignmentOperator {
    AdditiveAssign,
    SubtractiveAssign,
    MultiplicativeAssign,
    DivisionAssign,
    ModuloAssign,
    RightShiftAssign,
    LeftShiftAssign,
    Assign,
}

#[repr(u8)]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Symbol {
    ParenthesesOpen,
    ParenthesesClose,
    CurlyBraceOpen,
    CurlyBraceClose,
    SquareBracketOpen,
    SquareBracketClose,
    Semicolon,
    Colon,
    DoubleColon,
    Comma,
    Dot,
    DotDot,
    DotDotDot,
    Arrow,
}

impl TokenKind {
    pub fn from_string(string: String) -> TokenKind {
        match string.as_str() {
            "let" => TokenKind::Keyword(Keyword::Let),
            "if" => TokenKind::Keyword(Keyword::If),
            "else" => TokenKind::Keyword(Keyword::Else),
            "loop" => TokenKind::Keyword(Keyword::Loop),
            "break" => TokenKind::Keyword(Keyword::Break),
            "return" => TokenKind::Keyword(Keyword::Return),
            "continue" => TokenKind::Keyword(Keyword::Continue),
            "class" => TokenKind::Keyword(Keyword::Class),
            "interface" => TokenKind::Keyword(Keyword::Interface),
            "enum" => TokenKind::Keyword(Keyword::Enum),
            "function" => TokenKind::Keyword(Keyword::Function),
            "module" => TokenKind::Keyword(Keyword::Module),
            "var" => TokenKind::Keyword(Keyword::Var),
            "while" => TokenKind::Keyword(Keyword::While),
            "for" => TokenKind::Keyword(Keyword::For),
            "foreach" => TokenKind::Keyword(Keyword::Foreach),
            "unsafe" => TokenKind::Keyword(Keyword::Unsafe),
            "mutable" => TokenKind::Keyword(Keyword::Mutable),
            "new" => TokenKind::Keyword(Keyword::New),
            "static" => TokenKind::Keyword(Keyword::Static),
            "self" => TokenKind::Keyword(Keyword::Self_),
            "import" => TokenKind::Keyword(Keyword::Import),
            "extend" => TokenKind::Keyword(Keyword::Extend),
            "extern" => TokenKind::Keyword(Keyword::Extern),
            "public" => TokenKind::Keyword(Keyword::Public),
            "true" => TokenKind::Literal(Literal::Bool(true)),
            "false" => TokenKind::Literal(Literal::Bool(false)),
            "as" => TokenKind::Operator(Operator::As),
            _ => TokenKind::Identifier(string),
        }
    }
}

impl ToString for TokenKind {
    fn to_string(&self) -> String {
        format!(
            "{}",
            match self {
                TokenKind::Keyword(Keyword::Class) => "class",
                TokenKind::Keyword(Keyword::Interface) => "interface",
                TokenKind::Keyword(Keyword::Function) => "function",
                TokenKind::Keyword(Keyword::Extern) => "extern",
                TokenKind::Keyword(Keyword::Let) => "let",
                TokenKind::Keyword(Keyword::Mutable) => "mutable",
                TokenKind::Keyword(Keyword::If) => "if",
                TokenKind::Keyword(Keyword::Else) => "else",
                TokenKind::Keyword(Keyword::While) => "while",
                TokenKind::Keyword(Keyword::Loop) => "loop",
                TokenKind::Keyword(Keyword::For) => "for",
                TokenKind::Keyword(Keyword::Foreach) => "foreach",
                TokenKind::Keyword(Keyword::Return) => "return",
                TokenKind::Keyword(Keyword::Module) => "module",
                TokenKind::Keyword(Keyword::New) => "new",
                TokenKind::Keyword(Keyword::Enum) => "enum",
                TokenKind::Keyword(Keyword::Static) => "static",
                TokenKind::Keyword(Keyword::Self_) => "self",
                TokenKind::Keyword(Keyword::Import) => "import",
                TokenKind::Keyword(Keyword::Var) => "var",
                TokenKind::Keyword(Keyword::Extend) => "extend",
                TokenKind::Keyword(Keyword::Unsafe) => "unsafe",
                TokenKind::Keyword(Keyword::Break) => "break",
                TokenKind::Keyword(Keyword::Continue) => "continue",
                TokenKind::Keyword(Keyword::Public) => "public",
                TokenKind::Symbol(Symbol::Arrow) => "->",
                TokenKind::Symbol(Symbol::Comma) => ",",
                TokenKind::Symbol(Symbol::Colon) => ":",
                TokenKind::Symbol(Symbol::DoubleColon) => "::",
                TokenKind::Symbol(Symbol::Semicolon) => ";",
                TokenKind::Symbol(Symbol::Dot) => ".",
                TokenKind::Symbol(Symbol::DotDot) => "..",
                TokenKind::Symbol(Symbol::DotDotDot) => "...",
                TokenKind::Symbol(Symbol::ParenthesesOpen) => "(",
                TokenKind::Symbol(Symbol::ParenthesesClose) => ")",
                TokenKind::Symbol(Symbol::CurlyBraceOpen) => "{",
                TokenKind::Symbol(Symbol::CurlyBraceClose) => "}",
                TokenKind::Symbol(Symbol::SquareBracketOpen) => "[",
                TokenKind::Symbol(Symbol::SquareBracketClose) => "]",
                TokenKind::Operator(Operator::Plus) => "+",
                TokenKind::Operator(Operator::Minus) => "-",
                TokenKind::Operator(Operator::Asterisk) => "*",
                TokenKind::Operator(Operator::Slash) => "/",
                TokenKind::Operator(Operator::Modulo) => "%",
                TokenKind::Operator(Operator::Power) => "**",
                TokenKind::Operator(Operator::BitwiseAnd) => "&",
                TokenKind::Operator(Operator::BitwiseOr) => "|",
                TokenKind::Operator(Operator::BitwiseXor) => "^",
                TokenKind::Operator(Operator::BitwiseNot) => "~",
                TokenKind::Operator(Operator::Equals) => "==",
                TokenKind::Operator(Operator::NotEquals) => "!=",
                TokenKind::Operator(Operator::GreaterThan) => ">",
                TokenKind::Operator(Operator::GreaterOrEqual) => ">=",
                TokenKind::Operator(Operator::LessThan) => "<",
                TokenKind::Operator(Operator::LessOrEqual) => "<=",
                TokenKind::Operator(Operator::BitwiseRightShift) => ">>",
                TokenKind::Operator(Operator::BitwiseLeftShift) => "<<",
                TokenKind::Operator(Operator::Not) => "!",
                TokenKind::Operator(Operator::And) => "&&",
                TokenKind::Operator(Operator::Or) => "||",
                TokenKind::Operator(Operator::Assign(AssignmentOperator::Assign)) => "=",
                TokenKind::Operator(Operator::Assign(AssignmentOperator::AdditiveAssign)) => "+=",
                TokenKind::Operator(Operator::Assign(AssignmentOperator::SubtractiveAssign)) =>
                    "-=",
                TokenKind::Operator(Operator::Assign(AssignmentOperator::MultiplicativeAssign)) =>
                    "*=",
                TokenKind::Operator(Operator::Assign(AssignmentOperator::DivisionAssign)) => "/=",
                TokenKind::Operator(Operator::Assign(AssignmentOperator::RightShiftAssign)) =>
                    ">>=",
                TokenKind::Operator(Operator::Assign(AssignmentOperator::LeftShiftAssign)) => "<<=",
                TokenKind::Operator(Operator::Assign(AssignmentOperator::ModuloAssign)) => "%=",
                TokenKind::Operator(Operator::As) => "as",
                TokenKind::Identifier(value) => value,
                TokenKind::Literal(Literal::HexidecmialNumber(number)) => {
                    return format!("hexadecimal number {:?}", number);
                }
                TokenKind::Literal(Literal::OctalNumber(number)) => {
                    return format!("octal number {:?}", number);
                }
                TokenKind::Literal(Literal::BinaryNumber(number)) => {
                    return format!("binary number {:?}", number);
                }
                TokenKind::Literal(Literal::Float(number)) => {
                    return format!("float number {:?}", number);
                }
                TokenKind::Literal(Literal::Integer(number)) => {
                    return format!("integer number {:?}", number);
                }
                TokenKind::Literal(Literal::Bool(value)) => {
                    return format!("boolean {:?}", value);
                }
                TokenKind::Literal(Literal::Char(value)) => {
                    return format!("char {:?}", value);
                }
                TokenKind::Literal(Literal::String(value)) => {
                    return format!("string {:?}", value);
                }
            }
        )
    }
}
