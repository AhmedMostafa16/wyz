use crate::utils::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    kind: TokenKind,
    slice: String,
    span: Span,
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Types
    Int8,
    UInt8,
    Int16,
    UInt16,
    Int32,
    UInt32,
    Int64,
    UInt64,
    Float32,
    Float64,
    Boolean,
    USize,
    Void,

    // Keywords
    Class,
    Function,
    Extern,
    Let,
    Mut,
    Var,
    If,
    Else,
    While,
    For,
    Foreach,
    Loop,
    Return,
    True,
    False,
    As,
    Unsafe,

    // Symbols
    Semicolon,
    Colon,
    DoubleColon,
    Dot,
    DotDot,
    DotDotDot,
    NewLine,
    ParenthesesOpen,
    ParenthesesClose,
    CurlyBracketOpen,
    CurlyBracketClose,
    SquareBracketOpen,
    SquareBracketClose,
    Plus,
    Minus,
    Astrisk,
    Slash,
    DoubleSlash,
    Equal,
    Bang,
    DoubleEqual,
    BangEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    RightShift,
    LeftShift,
    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
    RightShiftEqual,
    LeftShiftEqual,
    Ampersand,
    DoubleAmpersand,
    Pipe,
    DoublePipe,

    // Literal
    Identifier,
    Integer,
    Float,

    // Other
    Unknown,
}

impl TokenKind {
    pub fn is_premative_datatype(&self) -> bool {
        match self {
            TokenKind::Int8
            | TokenKind::UInt8
            | TokenKind::Int16
            | TokenKind::UInt16
            | TokenKind::Int32
            | TokenKind::UInt32
            | TokenKind::Int64
            | TokenKind::UInt64
            | TokenKind::Float32
            | TokenKind::Float64
            | TokenKind::Boolean
            | TokenKind::USize
            | TokenKind::Void => true,
            _ => false,
        }
    }

    pub fn is_binary_operator(&self) -> bool {
        match self {
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Astrisk
            | TokenKind::Slash
            | TokenKind::Equal
            | TokenKind::DoubleEqual
            | TokenKind::BangEqual
            | TokenKind::Greater
            | TokenKind::GreaterEqual
            | TokenKind::Less
            | TokenKind::LessEqual
            | TokenKind::RightShift
            | TokenKind::LeftShift
            | TokenKind::PlusEqual
            | TokenKind::MinusEqual
            | TokenKind::StarEqual
            | TokenKind::SlashEqual
            | TokenKind::RightShiftEqual
            | TokenKind::LeftShiftEqual
            | TokenKind::Pipe
            | TokenKind::DoublePipe
            | TokenKind::DoubleAmpersand
            | TokenKind::As => true,
            _ => false,
        }
    }

    pub fn is_unary_operator(&self) -> bool {
        match self {
            TokenKind::Bang | TokenKind::Plus | TokenKind::Minus | TokenKind::Ampersand => true,
            _ => false,
        }
    }

    pub fn is_literal(&self) -> bool {
        match self {
            TokenKind::True
            | TokenKind::False
            | TokenKind::Identifier
            | TokenKind::Integer
            | TokenKind::Float => true,
            _ => false,
        }
    }
}
