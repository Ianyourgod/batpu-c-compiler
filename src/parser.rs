#![allow(dead_code)]

pub mod nodes;
use crate::lexer::{Lexer, TokenType};

pub struct Parser {
    lexer: Lexer,
    current_token: TokenType,
}

#[derive(Debug)]
enum Declarator {
    Identifier(String),
    Pointer(Box<Declarator>),
    Array(Box<Declarator>, i16),
    Function(Vec<(nodes::Type, Declarator)>, String),
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
            let stmt = self.parse_declaration();
            program.statements.push(stmt);
            self.next_token();
        }

        program
    }

    fn parse_type_and_storage_class(&self, specifiers: Vec<TokenType>) -> (nodes::Type, nodes::StorageClass) {
        let mut types: Vec<nodes::Type> = Vec::new();
        let mut storage_classes: Vec<nodes::StorageClass> = Vec::new();

        for specifier in specifiers {
            let specifier = match specifier {
                TokenType::Keyword(kwd) => kwd,
                unknown => panic!("Expected keyword, found {:?}", unknown)
            };

            match specifier.as_str() {
                "int" => types.push(nodes::Type::Int),
                "char" => types.push(nodes::Type::Char),
                "extern" => storage_classes.push(nodes::StorageClass::Extern),
                "static" => storage_classes.push(nodes::StorageClass::Static),
                _ => panic!("Invalid type specifier, {:?}", specifier),
            }
        };

        if types.len() != 1 {
            panic!("Invalid type specifier, {:?}", types);
        }
        let st_cl_len = storage_classes.len();
        if st_cl_len > 1 {
            panic!("Invalid storage class");
        }

        let type_ = types[0].clone();

        let storage_class = if st_cl_len == 1 { storage_classes[0] } else { nodes::StorageClass::Auto };

        return (type_, storage_class);
    }

    fn is_valid_var_starter(&self, token: &TokenType) -> bool {
        match token {
            TokenType::Keyword(kwd) => {
                match kwd.as_str() {
                    "int" => true,
                    "char" => true,
                    "extern" => true,
                    "static" => true,
                    _ => false,
                }
            },
            _ => false,
        }
    }

    fn parse_declarator(&mut self, ident: Option<String>) -> Declarator {
        match self.current_token {
            TokenType::Star => {
                self.next_token();
                Declarator::Pointer(Box::new(self.parse_declarator(None)))
            },
            TokenType::Identifier(ref name) => {
                let name = name.clone();
                self.next_token();
                if self.current_token == TokenType::LParen || self.current_token == TokenType::LBracket {
                    let decl = self.parse_declarator(Some(name.clone()));
                    return decl;
                }
                Declarator::Identifier(name)
            },
            TokenType::LParen => {
                if ident.is_none() {
                    self.next_token();
                    let decl = self.parse_declarator(None);
                    self.consume(TokenType::RParen);
                    return decl;
                }

                self.next_token();
                let mut params: Vec<(nodes::Type, Declarator)> = Vec::new();
                while self.current_token != TokenType::RParen {
                    let mut types: Vec<TokenType> = Vec::new();
                    while self.is_valid_var_starter(&self.current_token) {
                        types.push(self.current_token.clone());
                        self.next_token();
                    }

                    let (type_, _) = self.parse_type_and_storage_class(types);

                    let decl = self.parse_declarator(None);
                    let (name, ty, _) = self.process_declarator(decl, type_);

                    params.push((ty, Declarator::Identifier(name)));

                    if self.current_token == TokenType::Comma {
                        self.next_token();
                    } else if self.current_token != TokenType::RParen {
                        panic!("Expected ',' or ')', got {:?}", self.current_token);
                    }
                }
                self.next_token();
                Declarator::Function(params, ident.unwrap())
            },
            TokenType::LBracket => {
                self.next_token();
                let size = match self.current_token {
                    TokenType::IntegerLiteral(i) => {
                        self.next_token();
                        i
                    },
                    _ => panic!("Expected integer literal, got {:?}", self.current_token),
                };
                self.consume(TokenType::RBracket);
                Declarator::Array(Box::new(self.parse_declarator(ident)), size as i16)
            },
            _ => {
                if ident.is_some() {
                    return Declarator::Identifier(ident.unwrap());
                } else {
                    panic!("Expected identifier, got {:?}", self.current_token);
                }
            }
        }
    }

    fn process_declarator(&mut self, declarator: Declarator, base_type: nodes::Type) -> (String, nodes::Type, Vec<String>) {
        match declarator {
            Declarator::Identifier(name) => (name, base_type, Vec::new()),
            Declarator::Pointer(inner) => {
                let (name, ty, ptrs) = self.process_declarator(*inner, base_type);
                (name, nodes::Type::Pointer(Box::new(ty)), ptrs)
            },
            Declarator::Function(params, name) => {
                let mut param_types: Vec<nodes::Type> = Vec::new();
                let mut param_names: Vec<String> = Vec::new();

                for (ty, decl) in params {
                    let (name, ty, _) = self.process_declarator(decl, ty);
                    param_types.push(ty);
                    param_names.push(name);
                }

                (name, nodes::Type::Fn(param_types, Box::new(base_type)), param_names)
            },
            Declarator::Array(inner, size) => {
                let (name, ty, ptrs) = self.process_declarator(*inner, base_type);
                (name, nodes::Type::Array(Box::new(ty), size), ptrs)
            },
        }
    }

    fn parse_initializer(&mut self) -> nodes::Initializer {
        match self.current_token {
            TokenType::LBrace => {
                self.next_token();
                let mut inits: Vec<nodes::Initializer> = Vec::new();
                while self.current_token != TokenType::RBrace {
                    inits.push(self.parse_initializer());
                    if self.current_token == TokenType::Comma {
                        self.next_token();
                    } else if self.current_token != TokenType::RBrace {
                        panic!("Expected ',' or '}}', got {:?}", self.current_token);
                    }
                }
                self.next_token();
                nodes::Initializer::Compound(inits)
            },
            _ => nodes::Initializer::Single(self.parse_expression(0)),
        }
    }

    fn parse_declaration(&mut self) -> nodes::Declaration {
        let mut types: Vec<TokenType> = Vec::new();
        while self.is_valid_var_starter(&self.current_token.clone()) {
            types.push(self.current_token.clone());
            self.next_token();
        }

        #[allow(unused_variables)]
        let (type_, class_specifier) = self.parse_type_and_storage_class(types);

        let declarator = self.parse_declarator(None);

        let (name, type_, args) = self.process_declarator(declarator, type_);

        match type_ {
            nodes::Type::Fn(_, _) => {
                if self.current_token == TokenType::Semicolon {
                    return nodes::Declaration::FuncDecl(nodes::FuncDecl {
                        name,
                        params: args,
                        body: Vec::new(),
                        storage_class: class_specifier,
                        ty: type_,
                    })
                }
        
                self.consume(TokenType::LBrace);
        
                let mut body: Vec<nodes::BlockItem> = Vec::new();
        
                while self.current_token != TokenType::RBrace {
                    let stmt = self.parse_block_item();
                    body.push(stmt);
                }
        
                return nodes::Declaration::FuncDecl(nodes::FuncDecl {
                    name,
                    params: args,
                    body,
                    storage_class: class_specifier,
                    ty: type_,
                });
            }
            _ => (),
        }
        
        if self.current_token == TokenType::Semicolon {
            self.next_token();
            nodes::Declaration::VarDecl(nodes::VarDecl {
                name,
                expr: None,
                storage_class: class_specifier,
                ty: type_,
            })
        } else if self.current_token == TokenType::Equals {
            self.next_token();
            
            let expr = self.parse_initializer();

            if self.current_token != TokenType::Semicolon {
                panic!("Expected semicolon, got {:?}", self.current_token);
            }
            self.next_token();
            nodes::Declaration::VarDecl(nodes::VarDecl {
                name,
                expr: Some(expr),
                storage_class: class_specifier,
                ty: type_,
            })
        } else {
            panic!("Unexpected token: {:?}", self.current_token);
        }
    }

    fn parse_block_item(&mut self) -> nodes::BlockItem {
        if self.is_valid_var_starter(&self.current_token.clone()) {
            nodes::BlockItem::Declaration(self.parse_declaration())
        } else { nodes::BlockItem::Statement(self.parse_statement()) }
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
        } else if self.is_valid_var_starter(&self.current_token) {
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
        nodes::Expression::new(match self.current_token {
            TokenType::Tilde => {
                self.next_token();
                let expr = self.parse_factor();
                nodes::ExpressionEnum::Unop(nodes::Unop::BitwiseNot, Box::new(expr))
            }
            TokenType::Minus => {
                self.next_token();
                let expr = self.parse_factor();
                nodes::ExpressionEnum::Unop(nodes::Unop::Negate, Box::new(expr))
            }
            TokenType::LogicalNot => {
                self.next_token();
                let expr = self.parse_factor();
                nodes::ExpressionEnum::Unop(nodes::Unop::LogicalNot, Box::new(expr))
            }
            TokenType::Star => {
                self.next_token();
                let expr = self.parse_factor();
                nodes::ExpressionEnum::Dereference(Box::new(expr))
            }
            TokenType::Ampersand => {
                self.next_token();
                let expr = self.parse_factor();
                nodes::ExpressionEnum::AddressOf(Box::new(expr))
            }
            _ => panic!("Unknown token: {:?}", self.current_token),
        })
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
                expr = nodes::Expression::new(nodes::ExpressionEnum::Assign(Box::new(expr), Box::new(self.parse_expression(prec))));
            } else if (is_op_assign = self.is_op_assign(&self.current_token), is_op_assign.0).1 {
                self.next_token();
                expr = nodes::Expression::new(
                    nodes::ExpressionEnum::Assign(
                        Box::new(expr.clone()),
                        Box::new(nodes::Expression::new(
                            nodes::ExpressionEnum::Binop(
                                is_op_assign.1,Box::new(expr),
                                Box::new(self.parse_expression(prec))
                            )
                        ))
                    )
                );
            } else if self.current_token == TokenType::QuestionMark {
                self.next_token();
                let middle = self.parse_expression(0);
                self.consume(TokenType::Colon);
                let right = self.parse_expression(prec);
                expr = nodes::Expression::new(nodes::ExpressionEnum::Conditional(Box::new(expr), Box::new(middle), Box::new(right)));
            } else if self.current_token == TokenType::Increment || self.current_token == TokenType::Decrement {
                if self.current_token == TokenType::Increment {
                    expr = nodes::Expression::new(nodes::ExpressionEnum::Increment(Box::new(expr)));
                } else {
                    expr = nodes::Expression::new(nodes::ExpressionEnum::Decrement(Box::new(expr)));
                }
                self.next_token();
            } else {
                let op = self.convert_binop(&self.current_token);
                self.next_token();
                let right = self.parse_expression(prec + 1);
                expr = nodes::Expression::new(nodes::ExpressionEnum::Binop(op, Box::new(expr), Box::new(right)));
            }
        }

        expr
    }

    fn parse_factor(&mut self) -> nodes::Expression {
        let fac = self.parse_primary_factor();
        match self.current_token {
            TokenType::LBracket => {
                self.next_token();
                let idx = self.parse_expression(0);
                self.consume(TokenType::RBracket);
                nodes::Expression::new(nodes::ExpressionEnum::Subscript(Box::new(fac), Box::new(idx)))
            },
            _ => fac,
        }
    }

    fn parse_primary_factor(&mut self) -> nodes::Expression {
        let cur_tok = self.current_token.clone();
        match cur_tok {
            TokenType::IntegerLiteral(i) => {
                self.next_token();
                nodes::Expression::new(nodes::ExpressionEnum::IntegerLiteral(i))
            }
            TokenType::CharLiteral(ch) => {
                self.next_token();
                nodes::Expression::new(nodes::ExpressionEnum::CharLiteral(ch))
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
                    nodes::Expression::new(nodes::ExpressionEnum::FunctionCall(ident, args))
                } else {
                    nodes::Expression::new(nodes::ExpressionEnum::Var(ident))
                }
            }
            TokenType::Tilde | TokenType::Minus |
            TokenType::LogicalNot |
            TokenType::Star | TokenType::Ampersand => self.parse_unop(),
            TokenType::LParen => {
                self.next_token();
                let expr = self.parse_expression(0);
                if self.current_token != TokenType::RParen {
                    panic!("Expected ')', got {:?}", self.current_token);
                }
                self.next_token();
                expr
            },
            _ => panic!("Unexpected token: {:?}", self.current_token),
        }
    }
}