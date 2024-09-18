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
    Function(Vec<(nodes::Type, Declarator)>, Box<Declarator>),
}

#[derive(Debug)]
enum AbstractDeclarator {
    Pointer(Box<AbstractDeclarator>),
    Array(Box<AbstractDeclarator>, i16),
    Base,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Parser {
        let current_token = lexer.next_token();
        if let TokenType::Illegal(err) = current_token {
            panic!("Illegal Token: {}", err);
        }
        
        Parser {
            lexer,
            current_token,
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.lexer.next_token();
        if let TokenType::Illegal(ref err) = self.current_token {
            panic!("Illegal Token: {}", err);
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
        }

        program
    }

    fn parse_type_and_storage_class(&self, specifiers: Vec<TokenType>) -> (nodes::Type, nodes::StorageClass) {
        let mut types: Vec<nodes::Type> = Vec::new();
        let mut storage_classes: Vec<nodes::StorageClass> = Vec::new();

        let mut i = 0;
        while i < specifiers.len() {
            let specifier = &specifiers[i];
            i += 1;

            let specifier = match specifier {
                TokenType::Keyword(kwd) => kwd,
                unknown => panic!("Expected keyword, found {:?}", unknown)
            };

            match specifier.as_str() {
                "int" => types.push(nodes::Type::Int),
                "char" => types.push(nodes::Type::Char),
                "void" => types.push(nodes::Type::Void),
                "extern" => storage_classes.push(nodes::StorageClass::Extern),
                "static" => storage_classes.push(nodes::StorageClass::Static),

                "struct" => {
                    let name = match specifiers[i] {
                        TokenType::Identifier(ref name) => name.clone(),
                        _ => panic!("Expected identifier, got {:?}", self.current_token),
                    };
                    i += 1;
                    types.push(nodes::Type::Struct(name));
                }
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
                    "int"  |
                    "char" |
                    "void" |
                    "extern" |
                    "static" |
                    "struct" => true,
                    _ => false,
                }
            },
            _ => false,
        }
    }

    fn get_identifier(&self, decl: &Declarator, base_type: &nodes::Type) -> (String, nodes::Type) {
        match decl {
            Declarator::Identifier(name) => (name.clone(), base_type.clone()),
            Declarator::Array(inner, size) => {
                let (name, ty) = self.get_identifier(&*inner, base_type);
                (name, nodes::Type::Array(Box::new(ty), *size))
            },
            Declarator::Pointer(inner) => {
                let (name, ty) = self.get_identifier(&*inner, base_type);
                (name, nodes::Type::Pointer(Box::new(ty)))
            },
            Declarator::Function(params, name) => {
                let (name, ty) = self.get_identifier(&*name, base_type);

                (name, nodes::Type::Fn(params.iter().map(|(ty, decl)| self.get_identifier(decl, ty).1).collect(), Box::new(ty)))
            },
        }
    }

    fn parse_declarator(&mut self) -> Declarator {
        if self.current_token == TokenType::Star {
            self.next_token();
            return Declarator::Pointer(Box::new(self.parse_declarator()));
        }

        self.parse_direct_declarator()
    }

    fn parse_direct_declarator(&mut self) -> Declarator {
        let inner = self.parse_simple_declarator();

        if self.current_token == TokenType::LParen {
            self.next_token();
            if self.current_token == TokenType::Keyword("void".to_string()) && self.lexer.peek_token() == TokenType::RParen {
                self.next_token();
                self.consume(TokenType::RParen);
                return Declarator::Function(Vec::new(), Box::new(inner));
            }

            let mut params: Vec<(nodes::Type, Declarator)> = Vec::new();

            while self.current_token != TokenType::RParen {
                let types = self.parse_types();

                let (type_, _) = self.parse_type_and_storage_class(types);

                let decl = self.parse_declarator();

                params.push((type_, decl));

                if self.current_token == TokenType::Comma {
                    self.next_token();
                } else if self.current_token != TokenType::RParen {
                    panic!("Expected ',' or ')', got {:?}", self.current_token);
                }
            }

            self.next_token();

            Declarator::Function(params, Box::new(inner))
        } else if self.current_token == TokenType::LBracket {
            let mut inner = inner;
            while self.current_token == TokenType::LBracket {
                self.next_token();
                let size = match self.current_token {
                    TokenType::IntegerLiteral(i) => {
                        self.next_token();
                        i
                    },
                    _ => panic!("Expected integer literal, got {:?}", self.current_token),
                };
                self.consume(TokenType::RBracket);
                inner = Declarator::Array(Box::new(inner), size as i16);
            }

            inner
        } else {
            inner
        }
    }

    fn parse_simple_declarator(&mut self) -> Declarator {
        if let TokenType::Identifier(name) = self.current_token.clone() {
            self.next_token();
            return Declarator::Identifier(name);
        } else if self.current_token == TokenType::LParen {
            self.next_token();
            let decl = self.parse_declarator();
            self.consume(TokenType::RParen);
            return decl;
        } else {
            panic!("Expected identifier or '(', got {:?}", self.current_token);
        }
    }

    fn parse_types(&mut self) -> Vec<TokenType> {
        let mut types: Vec<TokenType> = Vec::new();

        //println!("{:?}", self.current_token);

        while self.is_valid_var_starter(&self.current_token) {
            if self.current_token == TokenType::Keyword("struct".to_string()) {
                self.next_token();
                let name = match self.current_token {
                    TokenType::Identifier(ref name) => name.clone(),
                    _ => panic!("Expected identifier, got {:?}", self.current_token),
                };
                types.push(TokenType::Keyword("struct".to_string()));
                types.push(TokenType::Identifier(name));
                self.next_token();
                continue;
            }

            types.push(self.current_token.clone());
            self.next_token();
        }

        types
    }
    
    fn process_declarator(&mut self, declarator: Declarator, base_type: nodes::Type) -> (String, nodes::Type, Vec<String>) {
        match declarator {
            Declarator::Identifier(name) => (name, base_type, Vec::new()),
            Declarator::Pointer(inner) => {
                let derived_type = nodes::Type::Pointer(Box::new(base_type));
                self.process_declarator(*inner, derived_type)
            },
            Declarator::Function(params, name) => {
                let name = match *name {
                    Declarator::Identifier(name) => name,
                    _ => panic!("Can't apply additional type derivations to a function type, expected identifier, got {:?}", name),
                };

                let mut param_names = Vec::new();
                let mut param_types = Vec::new();

                for (ty, decl) in params {
                    let (name, ty, _) = self.process_declarator(decl, ty);

                    if let nodes::Type::Fn(_, _) = ty {
                        panic!("Function parameters cannot be functions");
                    }

                    param_names.push(name);
                    param_types.push(ty);
                }

                let derived_type = nodes::Type::Fn(param_types, Box::new(base_type));

                return (name, derived_type, param_names);
            },
            Declarator::Array(inner, size) => {
                let derived_type = nodes::Type::Array(Box::new(base_type), size);
                self.process_declarator(*inner, derived_type)
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
        let types = self.parse_types();

        //println!("types: {:?}", types);

        if types[0] == TokenType::Keyword("struct".to_string()) {
            let (tag, t1_is_ident) = match types.get(1) {
                Some(TokenType::Identifier(tag)) => (tag.clone(), true),
                _ => (String::new(), false),
            };

            if t1_is_ident && self.current_token == TokenType::LBrace {
                return self.parse_struct_declaration(tag);
            }
        }

        #[allow(unused_variables)]
        let (type_, class_specifier) = self.parse_type_and_storage_class(types);

        let declarator = self.parse_declarator();

        let (name, type_, args) = self.process_declarator(declarator, type_);

        match type_ {
            nodes::Type::Fn(_, _) => {
                if self.current_token == TokenType::Semicolon {
                    self.next_token();
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
                self.next_token();
        
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

    fn parse_struct_declaration(&mut self, tag: String) -> nodes::Declaration {
        let decl = if self.current_token == TokenType::LBrace {
            self.next_token();
            let mut members: Vec<nodes::MemberDecl> = Vec::new();

            while self.current_token != TokenType::RBrace {
                let types = self.parse_types();

                let (type_, _) = self.parse_type_and_storage_class(types);

                let declarator = self.parse_declarator();

                let (name, ty, _) = self.process_declarator(declarator, type_);

                members.push(nodes::MemberDecl {
                    name,
                    ty,
                });

                if self.current_token == TokenType::Semicolon {
                    self.next_token();
                    continue;
                } else if self.current_token != TokenType::RBrace {
                    panic!("Expected ';' or '}}', got {:?}", self.current_token);
                }
            }
            self.next_token();

            if members.is_empty() {
                panic!("Empty struct declaration");
            }

            nodes::Declaration::StructDecl(nodes::StructDecl {
                tag,
                members,
            })
        } else {
            nodes::Declaration::StructDecl(nodes::StructDecl {
                tag,
                members: Vec::new(),
            })
        };

        self.consume(TokenType::Semicolon);

        decl
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
                    "sizeof" => {
                        nodes::Statement::Expression(self.parse_expression(0))
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

        if self.current_token == TokenType::Semicolon {
            self.next_token();
            return nodes::Statement::Return(None);
        }

        let expr = self.parse_expression(0);

        if self.current_token != TokenType::Semicolon {
            panic!("Expected semicolon, got {:?}", self.current_token);
        }

        self.next_token();

        nodes::Statement::Return(Some(expr))
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
        let mut fac = self.parse_primary_factor();
        loop {
            match self.current_token {
                TokenType::LBracket => {
                    self.next_token();
                    let idx = self.parse_expression(0);
                    self.consume(TokenType::RBracket);
                    fac = nodes::Expression::new(nodes::ExpressionEnum::Subscript(Box::new(fac), Box::new(idx)));
                },
                TokenType::Period => {
                    self.next_token();
                    let field = match self.current_token {
                        TokenType::Identifier(ref name) => name.clone(),
                        _ => panic!("Expected identifier, got {:?}", self.current_token),
                    };
                    self.next_token();
                    fac = nodes::Expression::new(nodes::ExpressionEnum::Dot(Box::new(fac), field));
                },
                TokenType::Arrow => {
                    self.next_token();
                    let field = match self.current_token {
                        TokenType::Identifier(ref name) => name.clone(),
                        _ => panic!("Expected identifier, got {:?}", self.current_token),
                    };
                    self.next_token();
                    fac = nodes::Expression::new(nodes::ExpressionEnum::Arrow(Box::new(fac), field));
                }
                _ => return fac,
            }
        }
    }

    fn parse_abstract_declarator(&mut self) -> AbstractDeclarator {
        match self.current_token {
            TokenType::Star => {
                self.next_token();
                AbstractDeclarator::Pointer(Box::new(self.parse_abstract_declarator()))
            },
            TokenType::LParen => {
                self.next_token();
                let decl = self.parse_abstract_declarator();
                self.consume(TokenType::RParen);
                decl
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
                let inner = self.parse_abstract_declarator();
                match inner {
                    AbstractDeclarator::Array(_, _) | AbstractDeclarator::Base => (),
                    a => panic!("Expected array, found {:?}", a)
                }
                AbstractDeclarator::Array(Box::new(inner), size as i16)
            },
            _ => AbstractDeclarator::Base,
        }
    }

    fn process_abstract_declarator(&mut self, declarator: AbstractDeclarator, base_type: nodes::Type) -> nodes::Type {
        match declarator {
            AbstractDeclarator::Pointer(inner) => {
                let ty = self.process_abstract_declarator(*inner, base_type);
                nodes::Type::Pointer(Box::new(ty))
            },
            AbstractDeclarator::Array(inner, size) => {
                let ty = self.process_abstract_declarator(*inner, base_type);
                nodes::Type::Array(Box::new(ty), size)
            },
            AbstractDeclarator::Base => base_type,
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
            TokenType::Keyword(kwd) => {
                if kwd == "sizeof" {
                    self.next_token();
                    let peek_token = self.lexer.peek_token();
                    if self.current_token == TokenType::LParen && self.is_valid_var_starter(&peek_token) {
                        self.next_token();
                        let types = self.parse_types();
                        let (type_, _) = self.parse_type_and_storage_class(types);
                        self.consume(TokenType::RParen);
                        nodes::Expression::new(nodes::ExpressionEnum::SizeOfType(type_))
                    } else {
                        nodes::Expression::new(nodes::ExpressionEnum::SizeOf(Box::new(self.parse_expression(1))))
                    }
                } else {
                    panic!("Unexpected keyword: {:?}", kwd);
                }
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

                if let Some(expr) = self.check_for_cast() {
                    return expr;
                }

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

    fn check_for_cast(&mut self) -> Option<nodes::Expression> {
        if self.is_valid_var_starter(&self.current_token) {
            // explicit cast
            let types = self.parse_types();

            let (type_, spec) = self.parse_type_and_storage_class(types);

            if spec != nodes::StorageClass::Auto {
                panic!("Cast cannot have storage class");
            }

            // parse abstract declarator
            let decl = self.parse_abstract_declarator();
            let type_ = self.process_abstract_declarator(decl, type_);

            self.consume(TokenType::RParen);

            let expr = self.parse_expression(0);

            return Some(nodes::Expression::new(nodes::ExpressionEnum::Cast(type_, Box::new(expr))));
        } 

        if self.current_token == TokenType::LParen {
            self.next_token();
            self.check_for_cast()
        } else { None }
    }
}