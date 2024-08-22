#![allow(dead_code)]

pub mod nodes;
use crate::lexer::{Lexer, TokenType};

pub struct Parser {
    lexer: Lexer,
    current_token: TokenType,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Parser {
        let current_token = lexer.next_token();
        Parser {
            lexer,
            current_token,
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.lexer.next_token();
        if self.current_token == TokenType::Illegal {
            panic!("Illegal Token");
        }
    }

    fn consume(&mut self, expects: TokenType) {
        if self.current_token != expects {
            panic!("Expected {:?}, found {:?}", expects, self.current_token);
        }
        
        self.current_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> nodes::Program {
        let mut program = nodes::Program {
            statements: Vec::new(),
        };

        while self.current_token != TokenType::EOF {
            let stmt = self.parse_function_declaration();
            program.statements.push(stmt);
            self.next_token();
        }

        program
    }

    fn parse_function_declaration(&mut self, ) -> nodes::FuncDecl {
        if self.current_token != TokenType::Keyword("int".to_string()) {
            panic!("Expected int, got {:?}", self.current_token);
        }
        self.next_token();

        let name = match self.current_token {
            TokenType::Identifier(ref s) => s.clone(),
            _ => panic!("Expected identifier, got {:?}", self.current_token),
        };
        self.next_token();

        self.consume(TokenType::LParen);

        let mut args: Vec<nodes::Identifier> = Vec::new();

        while self.current_token != TokenType::RParen {
            if self.current_token != TokenType::Keyword("int".to_string()) {
                panic!("Expected type, got {:?}", self.current_token);
            }
            self.next_token();
            let arg = self.parse_lvalue();
            args.push(arg);
            if self.current_token == TokenType::Comma {
                self.next_token();
            } else if self.current_token != TokenType::RParen {
                panic!("Expected ',' or ')', got {:?}", self.current_token);
            }
        }

        self.next_token();

        if self.current_token != TokenType::LBrace {
            panic!("Expected '{{', got {:?}", self.current_token);
        }
        self.next_token();

        let mut body: Vec<nodes::BlockItem> = Vec::new();

        while self.current_token != TokenType::RBrace {
            let stmt = self.parse_block_item();
            body.push(stmt);
        }

        nodes::FuncDecl {
            name,
            params: args,
            body,
        }
    }

    fn parse_block_item(&mut self) -> nodes::BlockItem {
        match self.current_token {
            TokenType::Keyword(ref keyword) => {
                match keyword.as_str() {
                    "int" => nodes::BlockItem::Declaration(self.parse_declaration()),
                    _ => nodes::BlockItem::Statement(self.parse_statement())
                }
            },
            _ => nodes::BlockItem::Statement(self.parse_statement())
        }
    }

    fn parse_statement(&mut self) -> nodes::Statement {
        match self.current_token {
            TokenType::Keyword(ref keyword) => {
                match keyword.as_str() {
                    "return" => self.parse_return_statement(),
                    "if" => self.parse_if_statement(),
                    "while" => self.parse_while_statement(),
                    "do" => self.parse_do_while(),
                    "for" => self.parse_for_loop(),
                    "break" => {
                        self.next_token();
                        self.consume(TokenType::Semicolon);
                        nodes::Statement::Break("".to_string())
                    },
                    "continue" => {
                        self.next_token();
                        self.consume(TokenType::Semicolon);
                        nodes::Statement::Continue("".to_string())
                    },
                    _ => panic!("Unknown keyword: {:?}", keyword),
                }
            },
            TokenType::LBrace => {
                self.next_token();
                let mut stmts: Vec<nodes::BlockItem> = Vec::new();

                while self.current_token != TokenType::RBrace {
                    stmts.push(self.parse_block_item());
                    if self.current_token == TokenType::EOF {
                        panic!("Expected '}}', found EOF");
                    }
                }

                self.next_token();

                nodes::Statement::Compound(stmts)
            }
            TokenType::Semicolon => {
                self.next_token();
                nodes::Statement::Empty
            },
            _ => {
                let expr = nodes::Statement::Expression(self.parse_expression(0));
                self.consume(TokenType::Semicolon);
                expr
            },
        }
    }

    fn parse_lvalue(&mut self) -> nodes::Identifier {
        match self.current_token.clone() {
            TokenType::Identifier(s) => {
                self.next_token();
                nodes::Identifier::Var(s.clone())
            },
            _ => panic!("Expected lvalue, got {:?}", self.current_token),
        }
    }

    fn parse_declaration(&mut self) -> nodes::Declaration {
        if self.current_token != TokenType::Keyword("int".to_string()) {
            panic!("Expected int, got {:?}", self.current_token);
        }
        self.next_token();

        let name = self.parse_lvalue();

        if self.current_token == TokenType::Semicolon {
            self.next_token();
            nodes::Declaration::VarDecl(nodes::VarDecl {
                name,
                expr: None
            })
        } else if self.current_token == TokenType::Equals {
            self.next_token();
            let expr = self.parse_expression(0);
            if self.current_token != TokenType::Semicolon {
                panic!("Expected semicolon, got {:?}", self.current_token);
            }
            self.next_token();
            nodes::Declaration::VarDecl(nodes::VarDecl {
                name,
                expr: Some(expr)
            })
        } else {
            panic!("Unexpected token: {:?}", self.current_token);
        }
    }

    fn parse_if_statement(&mut self) -> nodes::Statement {
        self.consume(TokenType::Keyword("if".to_string()));

        self.consume(TokenType::LParen);

        let cond = self.parse_expression(0);

        self.consume(TokenType::RParen);

        let then = self.parse_statement();

        let else_ = if self.current_token == TokenType::Keyword("else".to_string()) {
            self.next_token();

            Some(self.parse_statement())
        } else {
            None
        };

        nodes::Statement::If(cond, Box::new(then), Box::new(else_))
    }

    fn parse_while_statement(&mut self) -> nodes::Statement {
        self.next_token();

        self.consume(TokenType::LParen);

        let cond = self.parse_expression(0);

        self.consume(TokenType::RParen);

        let stmt = self.parse_statement();

        nodes::Statement::While(cond, Box::new(stmt), "".to_string())
    }

    fn parse_do_while(&mut self) -> nodes::Statement {
        self.next_token();

        let stmt = self.parse_statement();

        self.consume(TokenType::Keyword("while".to_string()));
        self.consume(TokenType::LParen);

        let cond = self.parse_expression(0);

        self.consume(TokenType::RParen);
        self.consume(TokenType::Semicolon);

        nodes::Statement::DoWhile(Box::new(stmt), cond, "".to_string())
    }

    fn parse_for_loop(&mut self) -> nodes::Statement {
        self.next_token();

        self.consume(TokenType::LParen);

        // parse forinit
        let for_init = if self.current_token == TokenType::Semicolon {
            nodes::ForInit::Empty
        } else if self.current_token == TokenType::Keyword("int".to_string()) {
            let decl = self.parse_declaration(); // consumes semicolon
            nodes::ForInit::Declaration(decl)
        } else {
            let expr = self.parse_expression(0);
            self.consume(TokenType::Semicolon);
            nodes::ForInit::Expression(expr)
        };

        let condition = if self.current_token == TokenType::Semicolon {
            None
        } else {
            Some(self.parse_expression(0))
        };
    
        self.consume(TokenType::Semicolon);
    
        // Parse increment
        let increment = if self.current_token == TokenType::RParen {
            None
        } else {
            Some(self.parse_expression(0))
        };

        self.consume(TokenType::RParen);

        let stmt = self.parse_statement();

        nodes::Statement::For(for_init, condition, increment, Box::new(stmt), "".to_string())
    }

    fn parse_return_statement(&mut self) -> nodes::Statement {
        self.next_token();

        let expr = self.parse_expression(0);

        if self.current_token != TokenType::Semicolon {
            panic!("Expected semicolon, got {:?}", self.current_token);
        }

        self.next_token();

        nodes::Statement::Return(expr)
    }

    fn parse_unop(&mut self) -> nodes::Expression {
        match self.current_token {
            TokenType::Tilde => {
                self.next_token();
                let expr = self.parse_factor();
                nodes::Expression::Unop(nodes::Unop::BitwiseNot, Box::new(expr))
            }
            TokenType::Minus => {
                self.next_token();
                let expr = self.parse_factor();
                nodes::Expression::Unop(nodes::Unop::Negate, Box::new(expr))
            }
            TokenType::LogicalNot => {
                self.next_token();
                let expr = self.parse_factor();
                nodes::Expression::Unop(nodes::Unop::LogicalNot, Box::new(expr))
            }
            _ => panic!("Unknown token: {:?}", self.current_token),
        }
    }

    fn get_precedence(&self, token: &TokenType) -> i32 {
        match token {
            TokenType::Increment | TokenType::Decrement => 50,
            TokenType::Plus | TokenType::Minus => 45,
            TokenType::LessThan | TokenType::GreaterThan |
            TokenType::LessThanEqual | TokenType::GreaterThanEqual => 35,
            TokenType::Equal | TokenType::NotEqual => 30,
            TokenType::LogicalAnd => 25,
            TokenType::LogicalOr => 20,
            TokenType::QuestionMark => 5,
            TokenType::Equals |
            TokenType::AddAssign | TokenType::SubAssign => 1,
            _ => -1,
        }
    }

    fn convert_binop(&self, token: &TokenType) -> nodes::Binop {
        match token {
            TokenType::Plus => nodes::Binop::Add,
            TokenType::Minus => nodes::Binop::Subtract,
            TokenType::LogicalAnd => nodes::Binop::And,
            TokenType::LogicalOr => nodes::Binop::Or,
            TokenType::Equal => nodes::Binop::Equal,
            TokenType::NotEqual => nodes::Binop::NotEqual,
            TokenType::LessThan => nodes::Binop::LessThan,
            TokenType::GreaterThan => nodes::Binop::GreaterThan,
            TokenType::LessThanEqual => nodes::Binop::LessThanEqual,
            TokenType::GreaterThanEqual => nodes::Binop::GreaterThanEqual,
            _ => panic!("Unknown token: {:?}", token),
        }
    }

    fn is_op_assign(&self, token: &TokenType) -> (bool, nodes::Binop) {
        match token {
            TokenType::AddAssign => (true, nodes::Binop::Add),
            TokenType::SubAssign => (true, nodes::Binop::Subtract),
            _ => (false, nodes::Binop::Add),
        }
    }

    fn parse_expression(&mut self, min_prec: i32) -> nodes::Expression {
        let mut expr = self.parse_factor();

        // dont mind the weird fuckery
        let mut prec: i32;
        let mut is_op_assign: (bool, nodes::Binop);
        while (prec = self.get_precedence(&self.current_token), prec).1 >= min_prec {
            if self.current_token == TokenType::Equals {
                self.next_token();
                expr = nodes::Expression::Assign(Box::new(expr), Box::new(self.parse_expression(prec)));
            // more fuckery :tongue:
            } else if (is_op_assign = self.is_op_assign(&self.current_token), is_op_assign.0).1 {
                self.next_token();
                expr = nodes::Expression::Assign(Box::new(expr.clone()), Box::new(nodes::Expression::Binop(is_op_assign.1, Box::new(expr), Box::new(self.parse_expression(prec)))))
            } else if self.current_token == TokenType::QuestionMark {
                self.next_token();
                let middle = self.parse_expression(0);
                self.consume(TokenType::Colon);
                let right = self.parse_expression(prec);
                expr = nodes::Expression::Conditional(Box::new(expr), Box::new(middle), Box::new(right));
            } else if self.current_token == TokenType::Increment || self.current_token == TokenType::Decrement {
                if self.current_token == TokenType::Increment {
                    expr = nodes::Expression::Increment(Box::new(expr));
                } else {
                    expr = nodes::Expression::Decrement(Box::new(expr));
                }
                self.next_token();
            } else {
                let op = self.convert_binop(&self.current_token);
                self.next_token();
                let right = self.parse_expression(prec + 1);
                expr = nodes::Expression::Binop(op, Box::new(expr), Box::new(right));
            }
        }

        expr
    }

    fn parse_factor(&mut self) -> nodes::Expression {
        let cur_tok = self.current_token.clone();
        match cur_tok {
            TokenType::IntegerLiteral(i) => {
                self.next_token();
                nodes::Expression::IntegerLiteral(i)
            }
            TokenType::Identifier(ident) => {
                self.next_token();
                if self.current_token == TokenType::LParen {
                    self.next_token();
                    let mut args: Vec<nodes::Expression> = Vec::new();
                    while self.current_token != TokenType::RParen {
                        args.push(self.parse_expression(0));
                        if self.current_token == TokenType::Comma {
                            self.next_token();
                        } else if self.current_token != TokenType::RParen {
                            panic!("Expected ',' or ')', got {:?}", self.current_token);
                        }
                    }
                    self.next_token();
                    nodes::Expression::FunctionCall(ident, args)
                } else {
                    nodes::Expression::Var(nodes::Identifier::Var(ident))
                }
            }
            TokenType::Tilde | TokenType::Minus |
            TokenType::LogicalNot => self.parse_unop(),
            TokenType::LParen => {
                self.next_token();
                let expr = self.parse_expression(0);
                if self.current_token != TokenType::RParen {
                    panic!("Expected ')', got {:?}", self.current_token);
                }
                self.next_token();
                expr
            }
            _ => panic!("Unexpected token: {:?}", self.current_token),
        }

    }
}