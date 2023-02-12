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
    current_span: Span,
    previous_span: Span,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            cursor: 0,
            lookahead: 1,
            previous_span: Span::default(),
            current_span: Span::default(),
        }
    }

    pub fn check(&self, n: usize, token: &Token) -> bool {
        self.tokens.get(self.cursor + n) == Some(token)
    }

    pub fn get_current_token(&self) -> Option<TokenKind> {
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
        let curr = self.get_current_token();
        if curr.is_some() {
            self.current_span = self.get_current_span().unwrap();
        }
        self.cursor += 1;
        self.lookahead += 1;
        curr
    }

    pub fn eat(&mut self, token: TokenKind) -> Result<(), Error> {
        let current_token = self.get_current_token();
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
        if let Some(token) = self.get_current_token() {
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

    pub fn parse_module(&mut self, mod_name: String) -> Result<Module, Error> {
        let mut body = Vec::new();
        let mut submodules = Vec::new();
        let span = self.get_current_span()?;

        loop {
            match self.get_current_token() {
                Some(TokenKind::Keyword(Keyword::Function)) => {
                    let function = self.parse_function_decl()?;
                    body.push(Declaration::Function(function));
                }
                Some(TokenKind::Keyword(Keyword::Class)) => {
                    let class = self.parse_class_declaration()?;
                    body.push(Declaration::Class(class));
                }
                Some(TokenKind::Keyword(Keyword::Module)) => {
                    let module = self.parse_module_declaration()?;
                    submodules.push(module);
                }
                Some(TokenKind::Symbol(Symbol::CurlyBraceClose)) => {
                    break;
                }
                Some(_) => {
                    return Err(self.generate_error(ErrorKind::UnexpectedToken(format!(
                        "{:?}",
                        self.get_current_token().unwrap().to_string()
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

    pub fn parse_module_declaration(&mut self) -> Result<Module, Error> {
        self.eat(TokenKind::Keyword(Keyword::Module))?;
        let name = self.parse_identifier()?;
        self.eat(TokenKind::Symbol(Symbol::CurlyBraceOpen))?;
        let module = self.parse_module(name)?;
        self.advance();

        Ok(module)
    }

    pub fn parse_function_params(&mut self) -> Result<(Vec<FunctionParameter>, bool), Error> {
        let mut params = Vec::new();
        let mut variadic = false;
        loop {
            if let Some(TokenKind::Symbol(Symbol::ParenthesesClose)) = self.get_current_token() {
                self.advance();
                break;
            }
            let span = self.get_current_span()?;

            if let Some(TokenKind::Symbol(Symbol::DotDotDot)) = self.get_current_token() {
                self.advance();
                variadic = true;
                continue;
            }

            let param_name = self.parse_identifier()?;
            self.eat(TokenKind::Symbol(Symbol::Colon))?;
            let param_type = self.parse_type_name()?;

            // Handle the end of a parameter
            if let Some(TokenKind::Symbol(Symbol::Comma)) = self.get_current_token() {
                self.advance();
            } else if let Some(TokenKind::Symbol(Symbol::ParenthesesClose)) =
                self.get_current_token()
            {} else {
                return Err(self.generate_error(ErrorKind::ExpectedButFound(
                    "comma or close parentheses".to_string(),
                    format!("{:?}", self.get_current_token().unwrap().to_string()),
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

    pub fn parse_function_decl(&mut self) -> Result<crate::ast::FunctionDeclaration, Error> {
        let span = self.get_current_span()?;

        self.eat(TokenKind::Keyword(Keyword::Function))?;
        let name = self.parse_identifier()?;
        self.eat(TokenKind::Symbol(Symbol::ParenthesesOpen))?;
        let (params, variadic): (Vec<FunctionParameter>, bool) = self.parse_function_params()?;

        let return_type = if let Some(TokenKind::Symbol(Symbol::Arrow)) = self.get_current_token() {
            self.eat(TokenKind::Symbol(Symbol::Arrow))?;
            Some(self.parse_type_name()?)
        } else {
            None
        };

        let body = self.parse_block()?;

        Ok(FunctionDeclaration {
            name,
            return_type,
            params,
            body,
            variadic,
            span,
        })
    }

    pub fn parse_class_declaration(&mut self) -> Result<ClassDeclaration, Error> {
        let class_span = self.get_current_span()?;
        self.eat(TokenKind::Keyword(Keyword::Class))?;
        let name = self.parse_identifier()?;

        let mut fields = Vec::new();
        self.eat(TokenKind::Symbol(Symbol::CurlyBraceOpen))?;
        loop {
            if let Some(TokenKind::Symbol(Symbol::CurlyBraceClose)) = self.get_current_token() {
                self.advance();
                break;
            }

            let span = self.get_current_span()?;

            let field_name = self.parse_identifier()?;
            self.eat(TokenKind::Symbol(Symbol::Colon))?;
            let type_name = self.parse_identifier()?;

            fields.push(ClassField {
                field_name,
                type_name,
                span,
            });

            match self.get_current_token() {
                Some(TokenKind::Symbol(Symbol::Comma)) => {
                    self.advance();
                }
                Some(TokenKind::Symbol(Symbol::CurlyBraceClose)) => {}
                _ => {
                    return Err(self.generate_error(ErrorKind::ExpectedButFound(
                        "comma or close curly brace".to_string(),
                        format!("{:?}", self.get_current_token().unwrap().to_string()),
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

    pub fn parse_block(&mut self) -> Result<Block, Error> {
        let span = self.get_current_span()?;
        self.expect(&TokenKind::Symbol(Symbol::CurlyBraceOpen))?;
        self.advance();

        let mut statements = Vec::new();
        loop {
            if let Some(TokenKind::Symbol(Symbol::CurlyBraceClose)) = self.get_current_token() {
                self.advance();
                break;
            }

            let statement = self.parse_statement()?;
            statements.push(statement);
        }

        Ok(Block { statements, span })
    }

    pub fn parse_assignment(&mut self) -> Result<Expression, Error> {
        let left = self.parse_logical_or_expression()?;

        let span = self.get_current_span()?;

        if let Some(TokenKind::Operator(Operator::Assign(op))) = self.get_current_token() {
            Ok(Expression::VarAssignment(VarAssignment {
                operator: op,
                left: Box::from(self.validate_assign_target(left)?),
                right: Box::from(self.parse_expression()?),
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

    pub fn parse_if_expression(&mut self) -> Result<Expression, Error> {
        let span = self.get_current_span()?;
        self.advance();
        let condition = self.parse_expression()?;
        let body = self.parse_statement()?;
        let else_body = if let Some(TokenKind::Keyword(Keyword::Else)) = self.get_current_token() {
            self.advance();
            Some(self.parse_statement()?)
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

    pub fn parse_class_initializer(&mut self) -> Result<Expression, Error> {
        let init_span = self.get_current_span()?;
        let name = self.parse_identifier()?;
        self.eat(TokenKind::Symbol(Symbol::CurlyBraceOpen))?;

        let mut fields = Vec::new();
        loop {
            if let Some(TokenKind::Symbol(Symbol::CurlyBraceClose)) = self.get_current_token() {
                self.advance();
                break;
            }

            let field_span = self.get_current_span()?;
            let field_name = self.parse_identifier()?;

            if let Some(TokenKind::Symbol(Symbol::Colon)) = self.get_current_token() {
                self.advance();
            } else if let Some(TokenKind::Symbol(Symbol::Comma)) = self.get_current_token() {
                self.advance();
                let value = Expression::Identifier(field_name.clone());
                fields.push(ClassInitializerField {
                    field_name: field_name.clone(),
                    value,
                    span: field_span,
                });
                continue;
            } else if let Some(TokenKind::Symbol(Symbol::CurlyBraceClose)) =
                self.get_current_token()
            {
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
                    format!("{:?}", self.get_current_token().unwrap().to_string()),
                )));
            }

            let value = self.parse_expression()?;
            fields.push(ClassInitializerField {
                field_name,
                value,
                span: field_span,
            });

            if let Some(TokenKind::Symbol(Symbol::Comma)) = self.get_current_token() {
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

    pub fn parse_expression(&mut self) -> Result<Expression, Error> {
        let curr = self.get_current_token();
        // Check for If, Block, and Fn Calls
        if let Some(TokenKind::Keyword(Keyword::If)) = curr {
            return self.parse_if_expression();
        } else if let Some(TokenKind::Symbol(Symbol::CurlyBraceOpen)) = curr {
            return Ok(Expression::Block(self.parse_block()?));
        } else if let Some(TokenKind::Identifier(_)) = curr {
            if let Some(TokenKind::Symbol(Symbol::CurlyBraceOpen)) = self.lookahead() {
                return Ok(self.parse_class_initializer()?);
            }
        }

        self.parse_assignment()
    }

    pub fn parse_variable_declaration(&mut self) -> Result<Statement, Error> {
        let span = self.get_current_span()?;
        self.advance();
        let type_name = self.parse_type_name()?;
        let name = self.parse_identifier()?;

        let initializer =
            if let Some(TokenKind::Operator(Operator::Assign(AssignmentOperator::Assign))) =
                self.get_current_token()
            {
                self.advance();
                let initializer = self.parse_expression()?;
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

    pub fn parse_statement(&mut self) -> Result<Statement, Error> {
        let statement = match self.get_current_token() {
            Some(TokenKind::Keyword(Keyword::Return)) => {
                let span = self.get_current_span()?;
                self.advance();
                let value =
                    if let Some(TokenKind::Symbol(Symbol::Semicolon)) = self.get_current_token() {
                        None
                    } else {
                        let value = self.parse_expression()?;
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
                let body = self.parse_block()?;
                Ok(Statement::LoopStatement(LoopStatement {
                    body,
                    span: loop_span,
                }))
            }
            Some(TokenKind::Keyword(Keyword::Let)) => self.parse_variable_declaration(),
            Some(_) => {
                let expression = self.parse_expression()?;
                Ok(Statement::ExpressionStatement(expression))
            }
            None => Err(self.generate_error(ErrorKind::UnexpectedToken("EOF".to_string()))),
        }?;
        self.expect(&TokenKind::Symbol(Symbol::Semicolon))?;
        self.advance();
        Ok(statement)
    }

    fn logical_expression_helper(
        &mut self,
        builder: fn(&mut Parser) -> Result<Expression, Error>,
        _op_type: OperatorType,
    ) -> Result<Expression, Error> {
        let mut left = builder(self)?;

        while let Some(TokenKind::Operator(op)) = self.get_current_token() {
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

    fn binary_expression_helper(
        &mut self,
        builder: fn(&mut Parser) -> Result<Expression, Error>,
        _op_type: OperatorType,
    ) -> Result<Expression, Error> {
        let mut left = builder(self)?;

        while let Some(TokenKind::Operator(op)) = self.get_current_token() {
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

    pub fn parse_logical_or_expression(&mut self) -> Result<Expression, Error> {
        self.logical_expression_helper(Self::parse_logical_and_expression, OperatorType::LogicalOr)
    }

    pub fn parse_logical_and_expression(&mut self) -> Result<Expression, Error> {
        self.logical_expression_helper(Self::parse_equality_expression, OperatorType::LogicalAnd)
    }

    pub fn parse_additive_expression(&mut self) -> Result<Expression, Error> {
        self.binary_expression_helper(
            Self::parse_multiplicative_expression,
            OperatorType::Additive,
        )
    }

    pub fn parse_multiplicative_expression(&mut self) -> Result<Expression, Error> {
        self.binary_expression_helper(Self::parse_unary_expression, OperatorType::Multiplicative)
    }

    pub fn parse_equality_expression(&mut self) -> Result<Expression, Error> {
        self.binary_expression_helper(Self::parse_relational_expression, OperatorType::Equality)
    }

    pub fn parse_relational_expression(&mut self) -> Result<Expression, Error> {
        self.binary_expression_helper(Self::parse_additive_expression, OperatorType::Relational)
    }

    pub fn parse_unary_expression(&mut self) -> Result<Expression, Error> {
        let curr = self.get_current_token();
        match curr {
            Some(TokenKind::Operator(op)) => {
                self.advance();
                if op.is_unary() {
                    Ok(Expression::UnaryOp {
                        op,
                        expr: Box::from(self.parse_unary_expression()?),
                    })
                } else {
                    Err(self.generate_error(ErrorKind::ExpectedButFound(
                        "unary operator".to_string(),
                        format!("{:?}", op.to_string()),
                    )))
                }
            }
            Some(_) => self.parse_left_hand_side_expression(),
            None => Err(self.generate_error(ErrorKind::UnexpectedToken("EOF".to_string()))),
        }
    }

    pub fn parse_left_hand_side_expression(&mut self) -> Result<Expression, Error> {
        self.parse_call_member_expression()
    }

    pub fn parse_call_member_expression(&mut self) -> Result<Expression, Error> {
        let member = self.parse_member_expr()?;

        if let Some(TokenKind::Symbol(Symbol::ParenthesesOpen)) = self.get_current_token() {
            self.parse_call_expression(member)
        } else {
            Ok(member)
        }
    }

    fn parse_call_expression(&mut self, callee: Expression) -> Result<Expression, Error> {
        Ok(Expression::FnCall(FunctionCall {
            callee: Box::from(callee),
            args: self.parse_arguments()?,
            span: self.get_current_span()?,
        }))
    }

    pub fn parse_arguments(&mut self) -> Result<Vec<Expression>, Error> {
        let mut args = Vec::new();
        self.expect(&TokenKind::Symbol(Symbol::ParenthesesOpen))?;
        self.advance();
        loop {
            match self.get_current_token() {
                Some(TokenKind::Symbol(Symbol::ParenthesesClose)) => {
                    self.advance();
                    break;
                }
                Some(_) => {
                    let arg = self.parse_expression()?;
                    args.push(arg);
                    if let Some(TokenKind::Symbol(Symbol::ParenthesesClose)) =
                        self.get_current_token()
                    {
                        self.advance();
                        break;
                    }
                    self.expect(&TokenKind::Symbol(Symbol::Comma))?;
                    self.advance();
                }
                None => {
                    return Err(self.generate_error(ErrorKind::UnexpectedToken("EOF".to_string())));
                }
            };
        }
        Ok(args)
    }

    pub fn parse_member_expr(&mut self) -> Result<Expression, Error> {
        let mut object: Expression = self.parse_primary_expr()?;

        while let Some(TokenKind::Symbol(Symbol::Dot))
        | Some(TokenKind::Symbol(Symbol::SquareBracketOpen)) = self.get_current_token()
        {
            if let Some(TokenKind::Symbol(Symbol::Dot)) = self.get_current_token() {
                self.advance();
                let prop = Expression::Identifier(self.parse_identifier()?);
                object = Expression::MemberAccess(MemberAccess {
                    object: Box::from(object),
                    member: Box::from(prop),
                    computed: false,
                });
            } else {
                self.advance();
                let prop = self.parse_expression()?;
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

    pub fn parse_primary_expr(&mut self) -> Result<Expression, Error> {
        match self.get_current_token() {
            Some(TokenKind::Literal(literal)) => Ok(self.parse_literal()?),
            Some(TokenKind::Symbol(Symbol::ParenthesesOpen)) => {
                Ok(self.parse_parentheses_expression()?)
            }
            Some(TokenKind::Identifier(_)) => Ok(Expression::Identifier(self.parse_identifier()?)),
            Some(_) => Ok(self.parse_left_hand_side_expression()?),
            None => Err(self.generate_error(ErrorKind::UnexpectedToken("EOF".to_string()))),
        }
    }

    pub fn parse_literal(&mut self) -> Result<Expression, Error> {
        let Some(TokenKind::Literal(literal)) = self.get_current_token() else {
            return Err(self.generate_error(ErrorKind::ExpectedButFound("literal".to_string(),
                                                                       format!("{:?}",
                                                                       self.get_current_token().unwrap().to_string()))));
        };
        self.advance();
        Ok(Expression::Literal(literal))
    }

    fn parse_parentheses_expression(&mut self) -> Result<Expression, Error> {
        self.expect(&TokenKind::Symbol(Symbol::ParenthesesOpen))?;
        self.advance();
        let expr = self.parse_expression()?;
        self.expect(&TokenKind::Symbol(Symbol::ParenthesesClose))?;
        self.advance();
        Ok(expr)
    }

    fn parse_identifier(&mut self) -> Result<String, Error> {
        if let Some(TokenKind::Identifier(name)) = self.advance() {
            Ok(name.clone())
        } else {
            Err(self.generate_error(ErrorKind::ExpectedButFound(
                "identifier".to_string(),
                format!("{:?}", self.get_current_token().unwrap().to_string()),
            )))
        }
    }

    fn parse_type_name(&mut self) -> Result<String, Error> {
        if let Some(TokenKind::Identifier(type_name)) = self.advance() {
            Ok(type_name.clone())
        } else {
            Err(self.generate_error(ErrorKind::ExpectedButFound(
                "type name".to_string(),
                format!("{:?}", self.get_current_token().unwrap().to_string()),
            )))
        }
    }
}

pub fn parse(tokens: Vec<Token>, mod_name: String) -> Result<Module, Error> {
    let mut parser = Parser::new(tokens);
    let module = parser.parse_module(mod_name)?;
    Ok(module)
}
