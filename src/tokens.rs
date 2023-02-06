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
    Char,
    String,

    // Keywords
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
    True,
    False,
    As,
    Unsafe,
    New,
    Static,
    Import,
    Extend,

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
    Asterisk,
    AsteriskEqual,
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
    SlashEqual,
    RightShiftEqual,
    LeftShiftEqual,
    Ampersand,
    DoubleAmpersand,
    Pipe,
    DoublePipe,
    Arrow,
    Comma,

    // Literal
    Identifier,
    IntegerNumber,
    FloatNumber,
    HexidecmialNumber,
    OctalNumber,
    BinaryNumber,

    // Other
    EndLine,
    Unknown,
}

impl TokenKind {
    pub fn from_string(string: String) -> TokenKind {
        match string.as_str() {
            "class" => TokenKind::Class,
            "interface" => TokenKind::Interface,
            "enum" => TokenKind::Enum,
            "function" => TokenKind::Function,
            "extern" => TokenKind::Extern,
            "let" => TokenKind::Let,
            "mutable" => TokenKind::Mutable,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "while" => TokenKind::While,
            "loop" => TokenKind::Loop,
            "return" => TokenKind::Return,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "as" => TokenKind::As,
            "unsafe" => TokenKind::Unsafe,
            "new" => TokenKind::New,
            "static" => TokenKind::Static,
            "import" => TokenKind::Import,
            "extend" => TokenKind::Extend,
            "int8" => TokenKind::Int8,
            "uint8" => TokenKind::UInt8,
            "int16" => TokenKind::Int16,
            "uint16" => TokenKind::UInt16,
            "int32" => TokenKind::Int32,
            "uint32" => TokenKind::UInt32,
            "int64" => TokenKind::Int64,
            "uint64" => TokenKind::UInt64,
            "float32" => TokenKind::Float32,
            "float64" => TokenKind::Float64,
            "bool" => TokenKind::Boolean,
            "usize" => TokenKind::USize,
            "void" => TokenKind::Void,
            _ => TokenKind::Identifier,
        }
    }

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
            | TokenKind::Void
            | TokenKind::Char
            | TokenKind::String => true,
            _ => false,
        }
    }

    pub fn is_binary_operator(&self) -> bool {
        match self {
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Asterisk
            | TokenKind::AsteriskEqual
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
            | TokenKind::SlashEqual
            | TokenKind::RightShiftEqual
            | TokenKind::LeftShiftEqual
            | TokenKind::Pipe
            | TokenKind::DoublePipe
            | TokenKind::DoubleAmpersand
            | TokenKind::As
            | TokenKind::Comma
            | TokenKind::Arrow => true,
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
            | TokenKind::IntegerNumber
            | TokenKind::FloatNumber
            | TokenKind::HexidecmialNumber
            | TokenKind::BinaryNumber
            | TokenKind::OctalNumber => true,
            _ => false,
        }
    }
}
