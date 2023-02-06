use crate::{
    error::{Error, ErrorKind},
    tokens::{Token, TokenKind},
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

    fn get_str_from_input(&self, length: usize) -> String {
        self.input
            .get(self.cursor..self.cursor + length)
            .unwrap()
            .to_string()
    }

    fn is_next_char(&self, chr: char) -> bool {
        self.read_char(1) == chr
    }

    fn are_next_chars(&self, chrs: &[char]) -> bool {
        if let Some(values) = self
            .chars
            .get(self.cursor + 1..self.cursor + 1 + chrs.len())
        {
            chrs == values
        } else {
            false
        }
    }

    fn create_token(&mut self, kind: TokenKind, length: usize) -> Token {
        let result = Token {
            span: Span {
                id: 0,
                start: self.column,
                end: self.column + length,
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
        result
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
        Ok(Some(
            self.create_token(TokenKind::from_string(string), length),
        ))
    }

    fn scan_string(&mut self, quote: char) -> Result<Option<Token>, Error> {
        let mut length = 1;
        let mut escaped = false;
        loop {
            let chr = self.read_char(length);
            if chr == '\0' {
                return Err(Error {
                    kind: ErrorKind::UnexpectedToken(self.get_str_from_input(length)),
                    severity: ariadne::ReportKind::Error,
                    span: Span {
                        start: self.cursor,
                        end: self.cursor + length,
                        id: 0,
                    },
                });
            }
            length += 1;
            if !escaped && chr == quote {
                break;
            }
            escaped = !escaped && chr == '\\';
        }
        Ok(Some(self.create_token(
            if quote == '\'' {
                TokenKind::Char
            } else {
                TokenKind::String
            },
            length,
        )))
    }

    fn scan_number(&mut self, current: char) -> Result<Option<Token>, Error> {
        let mut length = 1;

        if current == '0' {
            match self.read_char(length) {
                'x' => {
                    length += 1;
                    while self.read_char(length).is_digit(16) {
                        length += 1;
                    }
                    return Ok(Some(
                        self.create_token(TokenKind::HexidecmialNumber, length),
                    ));
                }
                'o' => {
                    length += 1;
                    while (self.read_char(length)).is_digit(8) {
                        length += 1;
                    }
                    return Ok(Some(self.create_token(TokenKind::OctalNumber, length)));
                }
                'b' => {
                    length += 1;
                    while self.read_char(length).is_digit(2) {
                        length += 1;
                    }
                    return Ok(Some(self.create_token(TokenKind::BinaryNumber, length)));
                }
                _ => {}
            }
        }

        let mut is_float = false;
        loop {
            match self.read_char(length) {
                '.' if self.read_char(length + 1) != '.' => {
                    if is_float {
                        return Err(Error {
                            kind: ErrorKind::UnexpectedToken(self.get_str_from_input(length)),
                            severity: ariadne::ReportKind::Error,
                            span: Span {
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

        Ok(Some(self.create_token(
            if is_float {
                TokenKind::FloatNumber
            } else {
                TokenKind::IntegerNumber
            },
            length,
        )))
    }

    fn rest(&mut self, current: char) -> Result<Option<Token>, Error> {
        if (current).is_alphabetic() || current == '_' {
            self.keyword_or_identifier()
        } else if (current).is_numeric() {
            self.scan_number(current)
        } else if current == '"' || current == '\'' {
            self.scan_string(current)
        } else {
            Err(Error {
                kind: ErrorKind::UnexpectedToken(self.get_str_from_input(1).to_string()),
                severity: ariadne::ReportKind::Error,
                span: Span {
                    start: self.cursor - 1,
                    end: self.cursor,
                    id: 0,
                },
            })
        }
    }

    fn next(&mut self) -> Result<Option<Token>, Error> {
        self.skip_whitespace();

        match self.read_char(0) {
            '\0' => Ok(None),
            '{' => Ok(Some(self.create_token(TokenKind::CurlyBracketOpen, 1))),
            '}' => Ok(Some(self.create_token(TokenKind::CurlyBracketClose, 1))),
            '(' => Ok(Some(self.create_token(TokenKind::ParenthesesOpen, 1))),
            ')' => Ok(Some(self.create_token(TokenKind::ParenthesesClose, 1))),
            '[' => Ok(Some(self.create_token(TokenKind::SquareBracketOpen, 1))),
            ']' => Ok(Some(self.create_token(TokenKind::SquareBracketClose, 1))),
            ':' => Ok(Some(self.create_token(TokenKind::Colon, 1))),
            ',' => Ok(Some(self.create_token(TokenKind::Comma, 1))),
            '-' if self.is_next_char('>') => Ok(Some(self.create_token(TokenKind::Arrow, 2))),
            rest => self.rest(rest),
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
