use crate::{
    ast::*,
    error::{Error, ErrorKind},
    tokens::*,
    utils::Span,
};

struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
    lookahead: usize,
    start: usize,
    current_span: Span,
    previous_span: Span,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            cursor: 0,
            lookahead: 1,
            start: 0,
            previous_span: Span::default(),
            current_span: Span::default(),
        }
    }

    pub fn check(&self, n: usize, token: &Token) -> bool {
        self.tokens.get(self.cursor + n) == Some(token)
    }

    pub fn current(&self) -> Option<TokenKind> {
        match self.tokens.get(self.cursor) {
            Some(token) => Some(token.kind.clone()),
            None => None,
        }
    }

    pub fn current_position(&self) -> Option<Span> {
        match self.tokens.get(self.cursor) {
            Some(token) => Some(token.span.clone()),
            None => None,
        }
    }

    fn debug_position(&self) -> String {
        match self.current_position() {
            Some(span) => format!("{}", span.start),
            None => "at EOF".to_string(),
        }
    }

    fn generate_error(&self, kind: ErrorKind) -> Error {
        Error {
            kind,
            severity: ariadne::ReportKind::Error,
            span: self.get_current_span().unwrap().clone(),
        }
    }

    pub fn lookahead(&self) -> Option<TokenKind> {
        match self.tokens.get(self.lookahead) {
            Some(token) => Some(token.kind.clone()),
            None => None,
        }
    }

    pub fn advance(&mut self) -> Option<TokenKind> {
        self.previous_span = self.current_span;
        let curr = self.current();
        if curr.is_some() {
            self.current_span = self.get_current_span().unwrap();
        }
        self.cursor += 1;
        self.lookahead += 1;
        curr
    }

    pub fn eat(&mut self, token: TokenKind) -> Result<(), Error> {
        let current_token = self.current();
        if current_token == Some(token.clone()) {
            self.advance();
            Ok(())
        } else {
            Err(self.generate_error(ErrorKind::ExpectedButFound(
                format!("{:?}", token),
                format!("{:?}", current_token.unwrap()),
            )))
        }
    }

    pub fn expect(&self, expected: &TokenKind) -> Result<(), Error> {
        if let Some(token) = self.current() {
            if token == *expected {
                Ok(())
            } else {
                Err(
                    self.generate_error(ErrorKind::ExpectedButFoundWithExpectGuard(
                        format!("{:?}", expected),
                        format!("{:?}", token),
                    )),
                )
            }
        } else {
            Err(
                self.generate_error(ErrorKind::ExpectedButFoundWithExpectGuard(
                    format!("{:?}", expected),
                    "EOF".to_string(),
                )),
            )
        }
    }

    fn get_current_span(&self) -> Result<Span, Error> {
        let Some(span) = self.tokens.get(self.cursor).map(|token| token.span) else {
            return Err(self.generate_error(ErrorKind::ExpectedSpan));
        };

        Ok(span)
    }

    fn is_eof(&self) -> bool {
        if let Some(token) = self.tokens.get(self.cursor) {
            token.kind == TokenKind::EndOfFile
        } else {
            true
        }
    }

    pub fn module(&mut self, mod_name: String) -> Result<Module, Error> {
        let mut body = Vec::new();
        let mut submodules = Vec::new();
        let span = self.get_current_span()?;

        loop {
            match self.current() {
                Some(TokenKind::Keyword(Keyword::Function)) => {
                    let function = self.function_decl()?;
                    body.push(Declaration::Function(function));
                }
                Some(TokenKind::Keyword(Keyword::Class)) => {
                    let class = self.class_decl()?;
                    body.push(Declaration::Class(class));
                }
                Some(TokenKind::Keyword(Keyword::Module)) => {
                    let module = self.module_decl()?;
                    submodules.push(module);
                }
                Some(TokenKind::Symbol(Symbol::CurlyBraceClose)) => {
                    break;
                }
                Some(_) => {
                    return Err(self.generate_error(ErrorKind::UnexpectedToken(format!(
                        "{:?}",
                        self.current().unwrap().to_string()
                    ))));
                }
                None => break,
            }
        }

        Ok(Module {
            body,
            submodules,
            requirements: Vec::new(),
            name: mod_name,
            span,
        })
    }

    pub fn module_decl(&mut self) -> Result<Module, Error> {
        self.eat(TokenKind::Keyword(Keyword::Module))?;
        let name = if let Some(TokenKind::Identifier(name)) = self.advance() {
            name.clone()
        } else {
            return Err(self.generate_error(ErrorKind::ExpectedButFound(
                "identifier".to_string(),
                format!("{:?}", self.current().unwrap().to_string()),
            )));
        };

        self.eat(TokenKind::Symbol(Symbol::CurlyBraceOpen))?;
        let module = self.module(name)?;
        self.advance();
        Ok(module)
    }

    pub fn function_params(&mut self) -> Result<(Vec<FunctionParameter>, bool), Error> {
        let mut params = Vec::new();
        let mut variadic = false;
        loop {
            if let Some(TokenKind::Symbol(Symbol::ParenthesesClose)) = self.current() {
                self.advance();
                break;
            }
            let span = self.get_current_span()?;

            if let Some(TokenKind::Symbol(Symbol::DotDotDot)) = self.current() {
                self.advance();
                variadic = true;
                continue;
            }

            let param_type = if let Some(TokenKind::Identifier(param_type)) = self.current() {
                param_type.clone()
            } else {
                return Err(self.generate_error(ErrorKind::ExpectedButFound(
                    "type name".to_string(),
                    format!("{:?}", self.current().unwrap().to_string()),
                )));
            };

            self.advance();
            let param_name = if let Some(TokenKind::Identifier(param_name)) = self.current() {
                param_name.clone()
            } else {
                return Err(self.generate_error(ErrorKind::ExpectedButFound(
                    "identifier".to_string(),
                    format!("{:?}", self.current().unwrap().to_string()),
                )));
            };

            self.advance();

            if let Some(TokenKind::Symbol(Symbol::Comma)) = self.current() {
                self.advance();
            } else if let Some(TokenKind::Symbol(Symbol::ParenthesesClose)) = self.current() {
            } else {
                return Err(self.generate_error(ErrorKind::ExpectedButFound(
                    "comma or close parentheses".to_string(),
                    format!("{:?}", self.current().unwrap().to_string()),
                )));
            }
            params.push(FunctionParameter {
                name: param_name,
                type_name: param_type,
                span: span,
            });
        }
        Ok((params, variadic))
    }

    pub fn function_decl(&mut self) -> Result<crate::ast::FunctionDeclaration, Error> {
        let span = self.get_current_span()?;
        self.advance(); // Skip function keyword
        let name = if let Some(TokenKind::Identifier(name)) = self.current() {
            name.clone()
        } else {
            return Err(self.generate_error(ErrorKind::ExpectedButFound(
                "identifier".to_string(),
                format!("{:?}", self.current().unwrap().to_string()),
            )));
        };

        self.advance();
        self.expect(&TokenKind::Symbol(Symbol::ParenthesesOpen))?;
        self.advance();

        let (params, variadic): (Vec<FunctionParameter>, bool) = self.function_params()?;

        let return_type = if let Some(TokenKind::Symbol(Symbol::Arrow)) = self.current() {
            self.advance();
            if let Some(TokenKind::Identifier(return_type)) = self.current() {
                self.advance();
                Some(return_type.clone())
            } else {
                return Err(self.generate_error(ErrorKind::ExpectedButFound(
                    "type name".to_string(),
                    format!("{:?}", self.current().unwrap().to_string()),
                )));
            }
        } else {
            None
        };

        let body = self.block()?;

        Ok(FunctionDeclaration {
            name,
            return_type,
            params,
            body,
            variadic,
            span,
        })
    }

    pub fn class_decl(&mut self) -> Result<ClassDeclaration, Error> {
        let class_span = self.get_current_span()?;
        self.eat(TokenKind::Keyword(Keyword::Class))?;
        let name = if let Some(TokenKind::Identifier(name)) = self.advance() {
            name.clone()
        } else {
            return Err(self.generate_error(ErrorKind::ExpectedButFound(
                "identifier".to_string(),
                format!("{:?}", self.current().unwrap().to_string()),
            )));
        };
        let mut fields = Vec::new();
        self.eat(TokenKind::Symbol(Symbol::CurlyBraceOpen))?;
        loop {
            if let Some(TokenKind::Symbol(Symbol::CurlyBraceClose)) = self.current() {
                self.advance();
                break;
            }

            let span = self.get_current_span()?;

            let Some(TokenKind::Identifier(field_name)) = self.advance() else {
                return Err(self.generate_error(ErrorKind::ExpectedButFound(
                    "identifier".to_string(),
                    format!("{:?}", self.current().unwrap().to_string()),
                )));
            };

            self.eat(TokenKind::Symbol(Symbol::Colon))?;

            let Some(TokenKind::Identifier(type_name)) = self.advance() else {
                return Err(self.generate_error(ErrorKind::ExpectedButFound(
                    "identifier".to_string(),
                    format!("{:?}", self.current().unwrap().to_string()),
                )));
            };

            fields.push(ClassField {
                field_name,
                type_name,
                span,
            });

            match self.current() {
                Some(TokenKind::Symbol(Symbol::Comma)) => {
                    self.advance();
                }
                Some(TokenKind::Symbol(Symbol::CurlyBraceClose)) => {}
                _ => {
                    return Err(self.generate_error(ErrorKind::ExpectedButFound(
                        "comma or close curly brace".to_string(),
                        format!("{:?}", self.current().unwrap().to_string()),
                    )));
                }
            }
        }

        Ok(ClassDeclaration {
            name,
            fields,
            span: class_span,
        })
    }

    pub fn block(&mut self) -> Result<Block, Error> {
        let span = self.get_current_span()?;
        self.expect(&TokenKind::Symbol(Symbol::CurlyBraceOpen))?;
        self.advance();

        let mut statements = Vec::new();
        loop {
            if let Some(TokenKind::Symbol(Symbol::CurlyBraceClose)) = self.current() {
                self.advance();
                break;
            }

            let statement = self.statement()?;
            statements.push(statement);
        }

        Ok(Block { statements, span })
    }

    pub fn assignment(&mut self) -> Result<Expression, Error> {
        let left = self.logical_or()?;

        let span = self.get_current_span()?;

        if let Some(TokenKind::Operator(Operator::Assign(op))) = self.current() {
            Ok(Expression::VarAssignment(VarAssignment {
                operator: op,
                left: Box::from(self.validate_assign_target(left)?),
                right: Box::from(self.expression()?),
                span,
            }))
        } else {
            Ok(left)
        }
    }

    fn validate_assign_target(&self, target: Expression) -> Result<Expression, Error> {
        match &target {
            Expression::Identifier { .. } => Ok(target),
            Expression::MemberAccess { .. } => Ok(target),
            Expression::UnaryOp { op, .. } => {
                if let Operator::Asterisk = op {
                    Ok(target)
                } else {
                    Err(
                        self.generate_error(ErrorKind::InvalidAssignmentTarget(format!(
                            "{:?}",
                            target
                        ))),
                    )
                }
            }
            _ => {
                Err(self
                    .generate_error(ErrorKind::InvalidAssignmentTarget(format!("{:?}", target))))
            }
        }
    }

    pub fn if_expression(&mut self) -> Result<Expression, Error> {
        let span = self.get_current_span()?;
        self.advance();
        let condition = self.expression()?;
        let body = self.statement()?;
        let else_body = if let Some(TokenKind::Keyword(Keyword::Else)) = self.current() {
            self.advance();
            Some(self.statement()?)
        } else {
            None
        };
        Ok(Expression::If(IfExpression {
            condition: Box::from(condition),
            body: Box::from(body),
            else_body: Box::from(else_body),
            span: span,
        }))
    }

    pub fn class_init(&mut self) -> Result<Expression, Error> {
        let init_span = self.get_current_span()?;
        let name = if let Some(TokenKind::Identifier(name)) = self.advance() {
            name
        } else {
            return Err(self.generate_error(ErrorKind::ExpectedButFound(
                "identifier".to_string(),
                format!("{:?}", self.current().unwrap().to_string()),
            )));
        };

        self.eat(TokenKind::Symbol(Symbol::CurlyBraceOpen))?;
        let mut fields = Vec::new();
        loop {
            if let Some(TokenKind::Symbol(Symbol::CurlyBraceClose)) = self.current() {
                self.advance();
                break;
            }

            let field_span = self.get_current_span()?;

            let Some(TokenKind::Identifier(field_name)) = self.advance() else {
                return Err(self.generate_error(ErrorKind::ExpectedButFound(
                    "identifier".to_string(),
                    format!("{:?}", self.current().unwrap().to_string()),
                )));
            };

            if let Some(TokenKind::Symbol(Symbol::Colon)) = self.current() {
                self.advance();
            } else if let Some(TokenKind::Symbol(Symbol::Comma)) = self.current() {
                self.advance();
                let value = Expression::Identifier(field_name.clone());
                fields.push(ClassInitializerField {
                    field_name: field_name.clone(),
                    value,
                    span: field_span,
                });
                continue;
            } else if let Some(TokenKind::Symbol(Symbol::CurlyBraceClose)) = self.current() {
                let value = Expression::Identifier(field_name.clone());
                fields.push(ClassInitializerField {
                    field_name: field_name.clone(),
                    value,
                    span: self.get_current_span()?,
                });
                break;
            } else {
                return Err(self.generate_error(ErrorKind::ExpectedButFound(
                    "comma, colon, or close curly brace".to_string(),
                    format!("{:?}", self.current().unwrap().to_string()),
                )));
            }

            let value = self.expression()?;
            fields.push(ClassInitializerField {
                field_name,
                value,
                span: field_span,
            });

            if let Some(TokenKind::Symbol(Symbol::Comma)) = self.current() {
                self.advance();
            } else {
                self.expect(&TokenKind::Symbol(Symbol::CurlyBraceClose))?;
            }
        }
        Ok(Expression::StructInitializer(ClassInitializer {
            class_name: name,
            fields,
            span: init_span,
        }))
    }

    pub fn expression(&mut self) -> Result<Expression, Error> {
        let curr = self.current();
        // Check for If, Block, and Fn Calls
        if let Some(TokenKind::Keyword(Keyword::If)) = curr {
            return self.if_expression();
        } else if let Some(TokenKind::Symbol(Symbol::CurlyBraceOpen)) = curr {
            return Ok(Expression::Block(self.block()?));
        } else if let Some(TokenKind::Identifier(_)) = curr {
            if let Some(TokenKind::Symbol(Symbol::CurlyBraceOpen)) = self.lookahead() {
                return Ok(self.class_init()?);
            }
        }

        self.assignment()
    }

    pub fn var_declaration(&mut self) -> Result<Statement, Error> {
        let span = self.get_current_span()?;
        self.advance();
        let type_name = if let Some(TokenKind::Identifier(type_name)) = self.current() {
            type_name.clone()
        } else {
            return Err(self.generate_error(ErrorKind::ExpectedButFound(
                "type name".to_string(),
                format!("{:?}", self.current().unwrap().to_string()),
            )));
        };
        self.advance();
        let name = if let Some(TokenKind::Identifier(name)) = self.current() {
            name.clone()
        } else {
            return Err(self.generate_error(ErrorKind::ExpectedButFound(
                "identifier".to_string(),
                format!("{:?}", self.current().unwrap().to_string()),
            )));
        };
        self.advance();

        let initializer =
            if let Some(TokenKind::Operator(Operator::Assign(AssignmentOperator::Assign))) =
                self.current()
            {
                self.advance();
                let initializer = self.expression()?;
                Some(initializer)
            } else {
                None
            };
        Ok(Statement::VariableDeclaration(VariableDeclaration {
            name,
            type_name,
            initializer,
            span,
        }))
    }

    pub fn statement(&mut self) -> Result<crate::ast::Statement, Error> {
        let statement = match self.current() {
            Some(TokenKind::Keyword(Keyword::Return)) => {
                let span = self.get_current_span()?;
                self.advance();
                let value = if let Some(TokenKind::Symbol(Symbol::Semicolon)) = self.current() {
                    None
                } else {
                    let value = self.expression()?;
                    Some(value)
                };
                Ok(Statement::ReturnStatement(ReturnStatement { value, span }))
            }
            Some(TokenKind::Keyword(Keyword::Break)) => {
                self.advance();
                Ok(Statement::BreakStatement)
            }
            Some(TokenKind::Keyword(Keyword::Continue)) => {
                self.advance();
                Ok(Statement::ContinueStatement)
            }
            Some(TokenKind::Keyword(Keyword::Loop)) => {
                let loop_span = self.get_current_span()?;
                self.advance();
                let body = self.block()?;
                Ok(Statement::LoopStatement(LoopStatement {
                    body,
                    span: loop_span,
                }))
            }
            Some(TokenKind::Keyword(Keyword::Let)) => self.var_declaration(),
            Some(_) => {
                let expression = self.expression()?;
                Ok(Statement::ExpressionStatement(expression))
            }
            None => Err(self.generate_error(ErrorKind::UnexpectedToken("EOF".to_string()))),
        }?;
        self.expect(&TokenKind::Symbol(Symbol::Semicolon))?;
        self.advance();
        Ok(statement)
    }

    fn logical_expr_helper(
        &mut self,
        builder: fn(&mut Parser) -> Result<Expression, Error>,
        _op_type: OperatorType,
    ) -> Result<Expression, Error> {
        let mut left = builder(self)?;

        while let Some(TokenKind::Operator(op)) = self.current() {
            self.advance();
            let right = builder(self)?;
            left = Expression::LogicalOp {
                left: Box::from(left),
                right: Box::from(right),
                op,
            };
        }

        Ok(left)
    }

    fn binary_expr_helper(
        &mut self,
        builder: fn(&mut Parser) -> Result<Expression, Error>,
        _op_type: OperatorType,
    ) -> Result<Expression, Error> {
        let mut left = builder(self)?;

        while let Some(TokenKind::Operator(op)) = self.current() {
            self.advance();
            let right = builder(self)?;
            left = Expression::BinaryOp {
                left: Box::from(left),
                right: Box::from(right),
                op,
            };
        }

        Ok(left)
    }

    pub fn logical_or(&mut self) -> Result<Expression, Error> {
        self.logical_expr_helper(Self::logical_and, OperatorType::LogicalOr)
    }

    pub fn logical_and(&mut self) -> Result<Expression, Error> {
        self.logical_expr_helper(Self::equality, OperatorType::LogicalAnd)
    }

    pub fn additive(&mut self) -> Result<Expression, Error> {
        self.binary_expr_helper(Self::multiplicative, OperatorType::Additive)
    }

    pub fn multiplicative(&mut self) -> Result<Expression, Error> {
        self.binary_expr_helper(Self::unary, OperatorType::Multiplicative)
    }

    pub fn equality(&mut self) -> Result<Expression, Error> {
        self.binary_expr_helper(Self::relational, OperatorType::Equality)
    }

    pub fn relational(&mut self) -> Result<Expression, Error> {
        self.binary_expr_helper(Self::additive, OperatorType::Relational)
    }

    pub fn unary(&mut self) -> Result<Expression, Error> {
        let curr = self.current();
        match curr {
            Some(TokenKind::Operator(op)) => {
                self.advance();
                if op.is_unary() {
                    Ok(Expression::UnaryOp {
                        op,
                        expr: Box::from(self.unary()?),
                    })
                } else {
                    Err(self.generate_error(ErrorKind::ExpectedButFound(
                        "unary operator".to_string(),
                        format!("{:?}", op.to_string()),
                    )))
                }
            }
            Some(_) => self.left_hand_side_expr(),
            None => Err(self.generate_error(ErrorKind::UnexpectedToken("EOF".to_string()))),
        }
    }

    pub fn left_hand_side_expr(&mut self) -> Result<Expression, Error> {
        self.call_member_expr()
    }

    pub fn call_member_expr(&mut self) -> Result<Expression, Error> {
        let member = self.member_expr()?;

        if let Some(TokenKind::Symbol(Symbol::ParenthesesOpen)) = self.current() {
            self.call_expr(member)
        } else {
            Ok(member)
        }
    }

    fn call_expr(&mut self, callee: Expression) -> Result<Expression, Error> {
        Ok(Expression::FnCall(FunctionCall {
            callee: Box::from(callee),
            args: self.arguments()?,
            span: self.get_current_span()?,
        }))
    }

    pub fn arguments(&mut self) -> Result<Vec<Expression>, Error> {
        let mut args = Vec::new();
        self.expect(&TokenKind::Symbol(Symbol::ParenthesesOpen))?;
        self.advance();
        loop {
            match self.current() {
                Some(TokenKind::Symbol(Symbol::ParenthesesClose)) => {
                    self.advance();
                    break;
                }
                Some(_) => {
                    let arg = self.expression()?;
                    args.push(arg);
                    if let Some(TokenKind::Symbol(Symbol::ParenthesesClose)) = self.current() {
                        self.advance();
                        break;
                    }
                    self.expect(&TokenKind::Symbol(Symbol::Comma))?;
                    self.advance();
                }
                None => {
                    return Err(self.generate_error(ErrorKind::UnexpectedToken("EOF".to_string())))
                }
            };
        }
        Ok(args)
    }

    pub fn member_expr(&mut self) -> Result<Expression, Error> {
        let mut object: Expression = self.primary_expr()?;

        while let Some(TokenKind::Symbol(Symbol::Dot))
        | Some(TokenKind::Symbol(Symbol::SquareBracketOpen)) = self.current()
        {
            if let Some(TokenKind::Symbol(Symbol::Dot)) = self.current() {
                self.advance();
                let prop = self.identifier()?;
                object = Expression::MemberAccess(MemberAccess {
                    object: Box::from(object),
                    member: Box::from(prop),
                    computed: false,
                });
            } else {
                self.advance();
                let prop = self.expression()?;
                self.expect(&TokenKind::Symbol(Symbol::SquareBracketClose))?;
                self.advance();
                object = Expression::MemberAccess(MemberAccess {
                    object: Box::from(object),
                    member: Box::from(prop),
                    computed: true,
                });
            }
        }

        Ok(object)
    }

    pub fn identifier(&mut self) -> Result<Expression, Error> {
        match self.current() {
            Some(TokenKind::Identifier(id)) => {
                self.advance();
                Ok(Expression::Identifier(id))
            }
            Some(_) => Err(self.generate_error(ErrorKind::ExpectedButFound(
                "identifier".to_string(),
                format!("{:?}", self.current().unwrap().to_string()),
            ))),
            None => Err(self.generate_error(ErrorKind::UnexpectedToken("EOF".to_string()))),
        }
    }

    pub fn primary_expr(&mut self) -> Result<Expression, Error> {
        match self.current() {
            Some(TokenKind::Literal(literal)) => Ok(self.literal()?),
            Some(TokenKind::Symbol(Symbol::ParenthesesOpen)) => Ok(self.paren_expr()?),
            Some(TokenKind::Identifier(_)) => Ok(self.identifier()?),
            Some(_) => Ok(self.left_hand_side_expr()?),
            None => Err(self.generate_error(ErrorKind::UnexpectedToken("EOF".to_string()))),
        }
    }

    pub fn literal(&mut self) -> Result<Expression, Error> {
        let Some(TokenKind::Literal(literal)) = self.current() else {
            return  Err(self.generate_error(ErrorKind::ExpectedButFound("literal".to_string(), format!("{:?}", self.current().unwrap().to_string()))));
        };
        self.advance();
        Ok(Expression::Literal(literal))
    }

    fn paren_expr(&mut self) -> Result<Expression, Error> {
        self.expect(&TokenKind::Symbol(Symbol::ParenthesesOpen))?;
        self.advance();
        let expr = self.expression()?;
        self.expect(&TokenKind::Symbol(Symbol::ParenthesesClose))?;
        self.advance();
        Ok(expr)
    }
}

pub fn parse(tokens: Vec<Token>, mod_name: String) -> Result<Module, Error> {
    let mut parser = Parser::new(tokens);
    let module = parser.module(mod_name)?;
    Ok(module)
}
