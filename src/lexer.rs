use crate::{
    error::{Error, ErrorKind},
    tokens::{AssignmentOperator, Literal, Operator, Symbol, Token, TokenKind},
    utils::Span,
};

pub struct Lexer {
    cursor: usize,
    column: usize,
    line: usize,
    input: String,
    chars: Vec<char>,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let chars = input.chars().collect();
        Self {
            cursor: 0,
            line: 0,
            column: 0,
            chars,
            input,
        }
    }

    /// Set the current character buffer to the character on the next index.
    fn read_char(&self, relative_index: usize) -> char {
        if let Some(value) = self.chars.get(self.cursor + relative_index) {
            *value
        } else {
            '\0'
        }
    }

    fn read_while(&mut self, predicate: fn(&char) -> bool) -> String {
        let mut result = String::new();

        while let Some(character) = self.chars.get(self.cursor) {
            if !predicate(character) {
                break;
            }

            result.push(*character);
            self.read_char(1);
        }

        result
    }

    fn skip_whitespace(&mut self) -> () {
        loop {
            let s = self.read_char(0);
            if !s.is_whitespace() {
                return;
            }
            if s == '\n' {
                self.line += 1;
                self.column = 0;
                self.cursor += 1;
            } else {
                self.column += 1;
                self.cursor += 1;
            }
        }
    }

    #[inline(always)]
    fn get_str_from_input(&self, length: usize) -> String {
        self.input
            .get(self.cursor..self.cursor + length)
            .unwrap()
            .to_string()
    }

    fn is_next_char(&self, chr: char) -> bool {
        self.read_char(1) == chr
    }

    fn are_next_chars(&self, chrs: Vec<char>) -> bool {
        if let Some(values) = self
            .chars
            .get(self.cursor + 1..self.cursor + 1 + chrs.len())
        {
            chrs == values
        } else {
            false
        }
    }

    fn create_token(&mut self, kind: TokenKind, length: usize) -> Result<Option<Token>, Error> {
        let result = Token {
            span: Span {
                id: 0,
                line: self.line,
                column: self.column,
                start: self.cursor,
                end: self.cursor + length,
            },
            kind,
            slice: self
                .input
                .get(self.cursor..self.cursor + length)
                .unwrap()
                .to_string(),
        };
        self.cursor += length;
        self.column += length;
        Ok(Some(result))
    }

    fn keyword_or_identifier(&mut self) -> Result<Option<Token>, Error> {
        let mut length = 1;
        loop {
            let chr = self.read_char(length);
            if !(chr.is_alphanumeric() || chr == '_') {
                break;
            }
            length += 1;
        }
        let string = self.get_str_from_input(length);

        self.create_token(TokenKind::from_string(string), length)
    }
    fn scan_char(&mut self) -> Result<Option<Token>, Error> {
        let mut length = 1;
        let character = self.read_char(length);
        if character == '\0' {
            return Err(Error {
                kind: ErrorKind::ExpectedButFound("character".to_string(), "EOF".to_string()),
                severity: ariadne::ReportKind::Error,
                span: Span {
                    line: self.line,
                    column: self.column,
                    start: self.cursor,
                    end: self.cursor + length,
                    id: 0,
                },
            });
        }
        length += 1;

        self.create_token(TokenKind::Literal(Literal::Char(character)), length)
    }

    fn scan_string(&mut self) -> Result<Option<Token>, Error> {
        let mut length = 1;
        let mut escaped = false;
        loop {
            let chr = self.read_char(length);
            if chr == '\0' {
                self.cursor += 1;
                return Err(Error {
                    kind: ErrorKind::ExpectedButFound("character".to_string(), "EOF".to_string()),
                    severity: ariadne::ReportKind::Error,
                    span: Span {
                        line: self.line,
                        column: self.column,
                        start: self.cursor,
                        end: self.cursor + length,
                        id: 0,
                    },
                });
            }
            length += 1;
            if !escaped && chr == '\"' {
                break;
            }
            escaped = !escaped && chr == '\\';
        }
        self.create_token(
            TokenKind::Literal(Literal::String(self.get_str_from_input(length))),
            length,
        )
    }

    #[inline]
    fn calculate_digit_length(&self, mut length: usize, radix: u32) -> usize {
        length += 1;
        while self.read_char(length).is_digit(radix) {
            length += 1;
        }
        length
    }

    fn scan_number(&mut self, current: char) -> Result<Option<Token>, Error> {
        let mut length = 1;

        if current == '0' {
            match self.read_char(length) {
                'x' => {
                    length = self.calculate_digit_length(length, 16);
                    return self.create_token(
                        TokenKind::Literal(Literal::HexidecmialNumber(
                            i64::from_str_radix(self.get_str_from_input(length).as_str(), 16)
                                .unwrap(),
                        )),
                        length,
                    );
                }
                'o' => {
                    length = self.calculate_digit_length(length, 8);
                    return self.create_token(
                        TokenKind::Literal(Literal::OctalNumber(
                            i64::from_str_radix(self.get_str_from_input(length).as_str(), 8)
                                .unwrap(),
                        )),
                        length,
                    );
                }
                'b' => {
                    length = self.calculate_digit_length(length, 2);
                    return self.create_token(
                        TokenKind::Literal(Literal::BinaryNumber(
                            i64::from_str_radix(self.get_str_from_input(length).as_str(), 2)
                                .unwrap(),
                        )),
                        length,
                    );
                }
                _ => {}
            }
        }

        let mut is_float = false;
        loop {
            match self.read_char(length) {
                '.' if self.read_char(length + 1) != '.' => {
                    if is_float {
                        self.cursor += 1;
                        return Err(Error {
                            kind: ErrorKind::UnexpectedToken(self.get_str_from_input(length)),
                            severity: ariadne::ReportKind::Error,
                            span: Span {
                                line: self.line,
                                column: self.column,
                                start: self.cursor,
                                end: self.cursor + length,
                                id: 0,
                            },
                        });
                    }
                    length += 1;
                    is_float = true;
                }
                '_' if (self.read_char(length + 1).is_numeric()) => length += 2,
                chr if (chr.is_numeric()) => length += 1,
                _ => break,
            }
        }

        self.create_token(
            if is_float {
                TokenKind::Literal(Literal::Float(
                    self.get_str_from_input(length).parse::<f64>().unwrap(),
                ))
            } else {
                TokenKind::Literal(Literal::Integer(
                    self.get_str_from_input(length).parse::<i64>().unwrap(),
                ))
            },
            length,
        )
    }

    fn next(&mut self) -> Result<Option<Token>, Error> {
        self.skip_whitespace();

        match self.read_char(0) {
            '\0' => Ok(None),
            '{' => self.create_token(TokenKind::Symbol(Symbol::CurlyBraceOpen), 1),
            '}' => self.create_token(TokenKind::Symbol(Symbol::CurlyBraceClose), 1),
            '(' => self.create_token(TokenKind::Symbol(Symbol::ParenthesesOpen), 1),
            ')' => self.create_token(TokenKind::Symbol(Symbol::ParenthesesClose), 1),
            '[' => self.create_token(TokenKind::Symbol(Symbol::SquareBracketOpen), 1),
            ']' => self.create_token(TokenKind::Symbol(Symbol::SquareBracketClose), 1),
            ':' => {
                if self.is_next_char(':') {
                    self.create_token(TokenKind::Symbol(Symbol::DoubleColon), 2)
                } else {
                    self.create_token(TokenKind::Symbol(Symbol::Colon), 1)
                }
            }
            ';' => self.create_token(TokenKind::Symbol(Symbol::Semicolon), 1),
            ',' => self.create_token(TokenKind::Symbol(Symbol::Comma), 1),
            '-' => {
                if self.is_next_char('>') {
                    self.create_token(TokenKind::Symbol(Symbol::Arrow), 2)
                } else if self.is_next_char('=') {
                    self.create_token(
                        TokenKind::Operator(Operator::Assign(
                            AssignmentOperator::SubtractiveAssign,
                        )),
                        2,
                    )
                } else {
                    self.create_token(TokenKind::Operator(Operator::Minus), 2)
                }
            }
            '+' => {
                if self.is_next_char('=') {
                    self.create_token(
                        TokenKind::Operator(Operator::Assign(AssignmentOperator::AdditiveAssign)),
                        2,
                    )
                } else {
                    self.create_token(TokenKind::Operator(Operator::Plus), 1)
                }
            }
            '*' => {
                if self.is_next_char('=') {
                    self.create_token(
                        TokenKind::Operator(Operator::Assign(
                            AssignmentOperator::MultiplicativeAssign,
                        )),
                        2,
                    )
                } else if self.is_next_char('*') {
                    self.create_token(TokenKind::Operator(Operator::Power), 2)
                } else {
                    self.create_token(TokenKind::Operator(Operator::Asterisk), 1)
                }
            }
            '/' => {
                if self.is_next_char('=') {
                    self.create_token(
                        TokenKind::Operator(Operator::Assign(AssignmentOperator::DivisionAssign)),
                        2,
                    )
                } else {
                    self.create_token(TokenKind::Operator(Operator::Slash), 1)
                }
            }
            '%' => {
                if self.is_next_char('=') {
                    self.create_token(
                        TokenKind::Operator(Operator::Assign(AssignmentOperator::ModuloAssign)),
                        2,
                    )
                } else {
                    self.create_token(TokenKind::Operator(Operator::Modulo), 1)
                }
            }
            '=' => {
                if self.is_next_char('=') {
                    self.create_token(TokenKind::Operator(Operator::Equals), 2)
                } else {
                    self.create_token(
                        TokenKind::Operator(Operator::Assign(AssignmentOperator::Assign)),
                        1,
                    )
                }
            }
            '!' => {
                if self.is_next_char('=') {
                    self.create_token(TokenKind::Operator(Operator::NotEquals), 2)
                } else {
                    self.create_token(TokenKind::Operator(Operator::Not), 1)
                }
            }
            '>' => {
                if self.are_next_chars(">=".chars().collect()) {
                    self.create_token(
                        TokenKind::Operator(Operator::Assign(AssignmentOperator::RightShiftAssign)),
                        3,
                    )
                } else if self.is_next_char('>') {
                    self.create_token(TokenKind::Operator(Operator::BitwiseRightShift), 2)
                } else if self.is_next_char('=') {
                    self.create_token(TokenKind::Operator(Operator::GreaterOrEqual), 2)
                } else {
                    self.create_token(TokenKind::Operator(Operator::GreaterThan), 1)
                }
            }
            '<' => {
                if self.are_next_chars("<=".chars().collect()) {
                    self.create_token(
                        TokenKind::Operator(Operator::Assign(AssignmentOperator::LeftShiftAssign)),
                        3,
                    )
                } else if self.is_next_char('=') {
                    self.create_token(TokenKind::Operator(Operator::LessOrEqual), 2)
                } else if self.is_next_char('<') {
                    self.create_token(TokenKind::Operator(Operator::BitwiseLeftShift), 2)
                } else {
                    self.create_token(TokenKind::Operator(Operator::LessThan), 1)
                }
            }
            '.' => {
                if self.are_next_chars("..".chars().collect()) {
                    self.create_token(TokenKind::Symbol(Symbol::DotDotDot), 3)
                } else if self.is_next_char('.') {
                    self.create_token(TokenKind::Symbol(Symbol::DotDot), 2)
                } else {
                    self.create_token(TokenKind::Symbol(Symbol::Dot), 1)
                }
            }
            '&' => {
                if self.is_next_char('&') {
                    self.create_token(TokenKind::Operator(Operator::And), 2)
                } else {
                    self.create_token(TokenKind::Operator(Operator::BitwiseAnd), 1)
                }
            }
            '|' => {
                if self.is_next_char('|') {
                    self.create_token(TokenKind::Operator(Operator::Or), 2)
                } else {
                    self.create_token(TokenKind::Operator(Operator::BitwiseOr), 1)
                }
            }
            '^' => self.create_token(TokenKind::Operator(Operator::BitwiseXor), 1),
            '~' => self.create_token(TokenKind::Operator(Operator::BitwiseNot), 1),
            current => {
                if (current).is_alphabetic() || current == '_' {
                    self.keyword_or_identifier()
                } else if (current).is_numeric() {
                    self.scan_number(current)
                } else if current == '"' {
                    self.scan_string()
                } else if current == '\'' {
                    self.scan_char()
                } else {
                    self.cursor += 1;
                    Err(Error {
                        kind: ErrorKind::UnexpectedToken(self.get_str_from_input(1).to_string()),
                        severity: ariadne::ReportKind::Error,
                        span: Span {
                            line: self.line,
                            column: self.column,
                            start: self.cursor - 1,
                            end: self.cursor,
                            id: 0,
                        },
                    })
                }
            }
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, Error> {
        let mut result = vec![];
        while let Some(token) = self.next()? {
            result.push(token);
        }
        Ok(result)
    }
}
