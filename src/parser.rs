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
        if self.current_token != TokenType::Int {
            panic!("Expected int, got {:?}", self.current_token);
        }
        self.next_token();

        let name = match self.current_token {
            TokenType::Identifier(ref s) => s.clone(),
            _ => panic!("Expected identifier, got {:?}", self.current_token),
        };
        self.next_token();

        if self.current_token != TokenType::LParen {
            panic!("Expected '(', got {:?}", self.current_token);
        }
        self.next_token();
        if self.current_token != TokenType::RParen {
            panic!("Expected ')', got {:?}", self.current_token);
        }

        self.next_token();

        if self.current_token != TokenType::LBrace {
            panic!("Expected '{{', got {:?}", self.current_token);
        }
        self.next_token();

        let mut body: Vec<nodes::Statement> = Vec::new();

        while self.current_token != TokenType::RBrace {
            let stmt = self.parse_statement();
            body.push(stmt);
        }

        nodes::FuncDecl {
            name,
            body,
        }
    }

    fn parse_statement(&mut self) -> nodes::Statement {
        match self.current_token {
            TokenType::Return => self.parse_return_statement(),
            _ => panic!("Unknown token: {:?}", self.current_token),
        }
    }

    fn parse_return_statement(&mut self) -> nodes::Statement {
        self.next_token();

        let expr = self.parse_expression();

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
                let expr = self.parse_expression();
                nodes::Expression::Unop(nodes::Unop::BitwiseNot, Box::new(expr))
            }
            TokenType::Minus => {
                self.next_token();
                let expr = self.parse_expression();
                nodes::Expression::Unop(nodes::Unop::Negate, Box::new(expr))
            }
            _ => panic!("Unknown token: {:?}", self.current_token),
        }
    }

    fn parse_expression(&mut self) -> nodes::Expression {
        match self.current_token {
            TokenType::IntegerLiteral(i) => {
                self.next_token();
                nodes::Expression::IntegerLiteral(i)
            }
            TokenType::Tilde | TokenType::Minus => self.parse_unop(),
            TokenType::LParen => {
                self.next_token();
                let expr = self.parse_expression();
                if self.current_token != TokenType::RParen {
                    panic!("Expected ')', got {:?}", self.current_token);
                }
                self.next_token();
                expr
            }
            _ => panic!("Unknown token: {:?}", self.current_token),
        }
    }
}