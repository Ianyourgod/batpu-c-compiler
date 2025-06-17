#![allow(dead_code)]

pub mod nodes;
use crate::lexer::{Lexer, TokenType, Token, Keyword};
use super::errors;

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
}

#[derive(Debug)]
enum Declarator {
    Identifier(String, usize),
    Pointer(Box<Declarator>),
    Array(Box<Declarator>, i16),
    Function(Vec<(nodes::Type, Declarator)>, Box<Declarator>),
}

impl Declarator {
    pub fn get_line(&self) -> usize {
        match self {
            Declarator::Identifier(_, line) => *line,
            Declarator::Pointer(inner) => inner.get_line(),
            Declarator::Array(inner, _) => inner.get_line(),
            Declarator::Function(_, inner) => inner.get_line(),
        }
    }
}

#[derive(Debug)]
enum AbstractDeclarator {
    Pointer(Box<AbstractDeclarator>),
    Array(Box<AbstractDeclarator>, i16),
    Base,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Result<Parser, errors::Error> {
        let current_token = lexer.next_token()?;
        
        Ok(Parser {
            lexer,
            current_token,
        })
    }

    fn next_token(&mut self) -> Result<(), errors::Error> {
        self.current_token = self.lexer.next_token()?;
        Ok(())
    }

    fn consume(&mut self, expects: TokenType) -> Result<(), errors::Error> {
        if self.current_token.token_type != expects {
            return Err(errors::Error::new(errors::ErrorType::Error, format!("Expected {:?}, found {:?}", expects, self.current_token.token_type), self.current_token.line));
        }
        
        self.current_token = self.lexer.next_token()?;

        Ok(())
    }

    pub fn parse_program(&mut self) -> Result<nodes::Program, errors::Error> {
        let mut program = nodes::Program {
            statements: Vec::new(),
        };

        while self.current_token.token_type != TokenType::EOF {
            let stmt = self.parse_declaration()?;
            program.statements.push(stmt);
        }

        Ok(program)
    }

    fn parse_type_and_storage_class(&self, specifiers: Vec<Token>) -> Result<(nodes::Type, nodes::StorageClass), errors::Error> {
        let mut types: Vec<nodes::Type> = Vec::new();
        let mut storage_classes: Vec<nodes::StorageClass> = Vec::new();

        let mut i = 0;
        while i < specifiers.len() {
            let specifier_tok = &specifiers[i];
            i += 1;

            let specifier = match &specifier_tok.token_type {
                TokenType::Keyword(kwd) => kwd,
                unknown => return Err(errors::Error::new(errors::ErrorType::Error, format!("Invalid type specifier {:?}", unknown), specifier_tok.line)),
            };

            match specifier {
                Keyword::Int => types.push(nodes::Type::Int),
                Keyword::Char => types.push(nodes::Type::Char),
                Keyword::Void => types.push(nodes::Type::Void),
                Keyword::Extern => storage_classes.push(nodes::StorageClass::Extern),
                Keyword::Static => storage_classes.push(nodes::StorageClass::Static),
                Keyword::Struct => {
                    let name = match specifiers[i].token_type {
                        TokenType::Identifier(ref name) => name.clone(),
                        _ => return Err(errors::Error::new(errors::ErrorType::Error, format!("Expected identifier, got {:?}", specifiers[i]), specifiers[i].line)),
                    };
                    i += 1;
                    types.push(nodes::Type::Struct(name));
                }
                Keyword::TDName(name) => {
                    if let Some(ty) = self.lexer.get_type_def(name) {
                        types.push(ty.clone());
                    } else {
                        unreachable!()
                    }
                },
                kwd => return Err(errors::Error::new(errors::ErrorType::Error, format!("Invalid type specifier {:?}", kwd), specifier_tok.line)),
            }
        };

        if types.len() != 1 {
            return Err(errors::Error::new(errors::ErrorType::Error, format!("Invalid type specifier"), specifiers[0].line));
        }
        let st_cl_len = storage_classes.len();
        if st_cl_len > 1 {
            return Err(errors::Error::new(errors::ErrorType::Error, format!("Invalid storage class specifier"), specifiers[0].line));
        }

        let type_ = types[0].clone();

        let storage_class = if st_cl_len == 1 { storage_classes[0] } else { nodes::StorageClass::Auto };

        return Ok((type_, storage_class));
    }

    fn is_valid_var_starter(&self, token: &TokenType) -> bool {
        match token {
            TokenType::Keyword(kwd) => {
                match kwd {
                    Keyword::Int  |
                    Keyword::Char |
                    Keyword::Void |
                    Keyword::Extern |
                    Keyword::Static |
                    Keyword::Typedef | // for the shittery :)
                    Keyword::TDName(_) |
                    Keyword::Struct => true,
                    _ => false,
                }
            },
            _ => false,
        }
    }

    fn get_identifier(&self, decl: &Declarator, base_type: &nodes::Type) -> (String, nodes::Type) {
        match decl {
            Declarator::Identifier(name, _) => (name.clone(), base_type.clone()),
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

    fn parse_declarator(&mut self) -> Result<Declarator, errors::Error> {
        if self.current_token.token_type == TokenType::Star {
            self.next_token()?;
            return Ok(Declarator::Pointer(Box::new(self.parse_declarator()?)));
        }

        self.parse_direct_declarator()
    }

    fn parse_direct_declarator(&mut self) -> Result<Declarator, errors::Error> {
        let inner = self.parse_simple_declarator()?;

        if self.current_token.token_type == TokenType::LParen {
            self.next_token()?;
            if self.current_token.token_type == TokenType::Keyword(Keyword::Void) && self.lexer.peek_token()?.token_type == TokenType::RParen {
                self.next_token()?;
                self.consume(TokenType::RParen)?;
                return Ok(Declarator::Function(Vec::new(), Box::new(inner)));
            }

            let mut params: Vec<(nodes::Type, Declarator)> = Vec::new();

            while self.current_token.token_type != TokenType::RParen {
                let types = self.parse_types()?;

                let (type_, _) = self.parse_type_and_storage_class(types)?;

                let decl = self.parse_declarator()?;

                params.push((type_, decl));

                if self.current_token.token_type == TokenType::Comma && self.lexer.peek_token()?.token_type != TokenType::RParen {
                    self.next_token()?;
                } else if self.current_token.token_type != TokenType::RParen {
                    return Err(errors::Error::new(errors::ErrorType::Error, format!("Expected ',' or ')', got {:?}", self.current_token), self.current_token.line));
                }
            }

            self.next_token()?;

            Ok(Declarator::Function(params, Box::new(inner)))
        } else if self.current_token.token_type == TokenType::LBracket {
            let mut inner = inner;
            while self.current_token.token_type == TokenType::LBracket {
                self.next_token()?;
                let size = match self.current_token.token_type {
                    TokenType::IntegerLiteral(i) => {
                        self.next_token()?;
                        i
                    },
                    _ => return Err(errors::Error::new(errors::ErrorType::Error, format!("Expected integer literal, got {:?}", self.current_token), self.current_token.line)),
                };
                self.consume(TokenType::RBracket)?;
                inner = Declarator::Array(Box::new(inner), size as i16);
            }

            Ok(inner)
        } else {
            Ok(inner)
        }
    }

    fn parse_simple_declarator(&mut self) -> Result<Declarator, errors::Error> {
        if let TokenType::Identifier(name) = self.current_token.token_type.clone() {
            self.next_token()?;
            return Ok(Declarator::Identifier(name, self.current_token.line));
        } else if self.current_token.token_type == TokenType::LParen {
            self.next_token()?;
            let decl = self.parse_declarator();
            self.consume(TokenType::RParen)?;
            return decl;
        } else {
            return Err(errors::Error::new(errors::ErrorType::Error, format!("Expected identifier or '(', got {:?}", self.current_token), self.current_token.line));
        }
    }

    fn parse_types(&mut self) -> Result<Vec<Token>, errors::Error> {
        let mut types = Vec::new();

        while self.is_valid_var_starter(&self.current_token.token_type) {
            if self.current_token.token_type == TokenType::Keyword(Keyword::Struct) {
                let cur_tok = self.current_token.clone();
                self.next_token()?;
                match self.current_token.token_type {
                    TokenType::Identifier(_) => (),
                    _ => return Err(errors::Error::new(errors::ErrorType::Error, format!("Expected identifier, got {:?}", self.current_token), self.current_token.line)),
                };
                types.push(cur_tok);
                types.push(self.current_token.clone());
                self.next_token()?;
                continue;
            }

            types.push(self.current_token.clone());
            self.next_token()?;
        }

        Ok(types)
    }
    
    fn process_declarator(&mut self, declarator: Declarator, base_type: nodes::Type) -> Result<(String, nodes::Type, Vec<String>), errors::Error> {
        Ok(match declarator {
            Declarator::Identifier(name, _) => (name, base_type, Vec::new()),
            Declarator::Pointer(inner) => {
                let derived_type = nodes::Type::Pointer(Box::new(base_type));
                self.process_declarator(*inner, derived_type)?
            },
            Declarator::Function(params, name) => {
                let name = match *name {
                    Declarator::Identifier(name, _) => name,
                    _ => return Err(errors::Error::new(errors::ErrorType::Error, format!("Cannot apply additional type derivations to a function type, expected Identifier got {:?}", *name), name.get_line())),
                };

                let mut param_names = Vec::new();
                let mut param_types = Vec::new();

                for (ty, decl) in params {
                    let line = decl.get_line();
                    let (name, ty, _) = self.process_declarator(decl, ty)?;

                    if let nodes::Type::Fn(_, _) = ty {
                        return Err(errors::Error::new(errors::ErrorType::Error, format!("Function parameters cannot be functions"), line));
                    }

                    param_names.push(name);
                    param_types.push(ty);
                }

                let derived_type = nodes::Type::Fn(param_types, Box::new(base_type));

                return Ok((name, derived_type, param_names));
            },
            Declarator::Array(inner, size) => {
                let derived_type = nodes::Type::Array(Box::new(base_type), size);
                return self.process_declarator(*inner, derived_type);
            },
        })
    }
        

    fn parse_initializer(&mut self) -> Result<nodes::Initializer, errors::Error> {
        let line = self.current_token.line;
        Ok(match self.current_token.token_type {
            TokenType::LBrace => {
                self.next_token()?;
                let mut inits: Vec<nodes::Initializer> = Vec::new();
                while self.current_token.token_type != TokenType::RBrace {
                    inits.push(self.parse_initializer()?);
                    if self.current_token.token_type == TokenType::Comma && self.lexer.peek_token()?.token_type != TokenType::RBrace {
                        self.next_token()?;
                    } else if self.current_token.token_type != TokenType::RBrace {
                        return Err(errors::Error::new(errors::ErrorType::Error, format!("Expected '}}' or ',', got {:?}", self.current_token), self.current_token.line));
                    }
                }
                self.next_token()?;
                nodes::Initializer::Compound(inits, line)
            },
            _ => nodes::Initializer::Single(self.parse_expression(0)?, line),
        })
    }

    fn parse_declaration(&mut self) -> Result<nodes::Declaration, errors::Error> {
        let line = self.current_token.line;

        if let TokenType::Keyword(Keyword::Typedef) = self.current_token.token_type {
            return Ok(match self.parse_typedef()? {
                Some(decl) => decl,
                None => nodes::Declaration::Empty(self.current_token.line),
            });
        }
        
        let types = self.parse_types()?;

        if types.is_empty() {
            return Err(errors::Error::new(errors::ErrorType::Error, format!("Expected type specifier, got {:?}", self.current_token.token_type), self.current_token.line));
        }

        if types[0].token_type == TokenType::Keyword(Keyword::Struct) {
            let (tag, t1_is_ident) = match types.get(1) {
                Some(Token { token_type: TokenType::Identifier(tag), line: _ }) => (tag.clone(), true),
                _ => (String::new(), false),
            };

            if t1_is_ident && self.current_token.token_type == TokenType::LBrace {
                let st_decl = self.parse_struct_declaration(tag)?;
                self.next_token()?;
                self.consume(TokenType::Semicolon)?;
                return Ok(st_decl);
            }
        }

        #[allow(unused_variables)]
        let (type_, class_specifier) = self.parse_type_and_storage_class(types)?;

        let declarator = self.parse_declarator()?;

        let (name, type_, args) = self.process_declarator(declarator, type_)?;

        match type_ {
            nodes::Type::Fn(_, _) => {
                if self.current_token.token_type == TokenType::Semicolon {
                    self.next_token()?;
                    return Ok(nodes::Declaration::FuncDecl(nodes::FuncDecl {
                        name,
                        params: args,
                        body: Vec::new(),
                        storage_class: class_specifier,
                        ty: type_,
                        line,
                        has_body: false,
                    }, line));
                }
        
                self.consume(TokenType::LBrace)?;
        
                let mut body: Vec<nodes::BlockItem> = Vec::new();
        
                while self.current_token.token_type != TokenType::RBrace {
                    let stmt = self.parse_block_item()?;
                    body.push(stmt);
                    if self.current_token.token_type == TokenType::EOF {
                        return Err(errors::Error::new(errors::ErrorType::Error, format!("Expected '}}', got EOF"), self.current_token.line));
                    }
                }
                self.next_token()?;
        
                return Ok(nodes::Declaration::FuncDecl(nodes::FuncDecl {
                    name,
                    params: args,
                    body,
                    storage_class: class_specifier,
                    ty: type_,
                    line,
                    has_body: true,
                }, line));
            }
            _ => (),
        }
        
        if self.current_token.token_type == TokenType::Semicolon {
            self.next_token()?;
            Ok(nodes::Declaration::VarDecl(nodes::VarDecl {
                name,
                expr: None,
                storage_class: class_specifier,
                ty: type_,
                line,
            }, line))
        } else if self.current_token.token_type == TokenType::Equals {
            self.next_token()?;
            
            let expr = self.parse_initializer()?;

            self.consume(TokenType::Semicolon)?;
            Ok(nodes::Declaration::VarDecl(nodes::VarDecl {
                name,
                expr: Some(expr),
                storage_class: class_specifier,
                ty: type_,
                line,
            }, line))
        } else {
            Err(errors::Error::new(errors::ErrorType::Error, format!("Expected ';', got {:?}", self.current_token), self.current_token.line))
        }
    }

    fn parse_struct_declaration(&mut self, tag: String) -> Result<nodes::Declaration, errors::Error> {
        let line = self.current_token.line;
        let decl = if self.current_token.token_type == TokenType::LBrace {
            self.next_token()?;
            let mut members: Vec<nodes::MemberDecl> = Vec::new();

            while self.current_token.token_type != TokenType::RBrace {
                let types = self.parse_types()?;

                let line = self.current_token.line;

                let (type_, _) = self.parse_type_and_storage_class(types)?;

                let declarator = self.parse_declarator()?;

                let (name, ty, _) = self.process_declarator(declarator, type_)?;

                members.push(nodes::MemberDecl {
                    name,
                    ty,
                    line,
                });

                if self.current_token.token_type == TokenType::Semicolon {
                    self.next_token()?;
                    continue;
                } else if self.current_token.token_type != TokenType::RBrace {
                    return Err(errors::Error::new(errors::ErrorType::Error, format!("Expected ';' or '}}', got {:?}", self.current_token.token_type), self.current_token.line));
                }
            }

            if members.is_empty() {
                return Err(errors::Error::new(errors::ErrorType::Error, format!("Empty struct declaration"), self.current_token.line));
            }

            nodes::Declaration::StructDecl(nodes::StructDecl {
                tag,
                members,
                line,
            }, line)
        } else {
            nodes::Declaration::StructDecl(nodes::StructDecl {
                tag,
                members: Vec::new(),
                line
            }, line)
        };

        Ok(decl)
    }

    fn parse_block_item(&mut self) -> Result<nodes::BlockItem, errors::Error> {
        let line = self.current_token.line;
        if self.is_valid_var_starter(&self.current_token.token_type) {
            Ok(nodes::BlockItem::Declaration(self.parse_declaration()?, line))
        } else { Ok(nodes::BlockItem::Statement(self.parse_statement()?, line)) }
    }

    fn parse_statement(&mut self) -> Result<nodes::Statement, errors::Error> {
        let line = self.current_token.line;
        Ok(match self.current_token.token_type {
            TokenType::Keyword(ref keyword) => {
                match keyword {
                    Keyword::Return => self.parse_return_statement()?,
                    Keyword::If => self.parse_if_statement()?,
                    Keyword::While => self.parse_while_statement()?,
                    Keyword::Do => self.parse_do_while()?,
                    Keyword::For => self.parse_for_loop()?,
                    Keyword::Break => {
                        self.next_token()?;
                        self.consume(TokenType::Semicolon)?;
                        nodes::Statement::Break("".to_string(), line)
                    },
                    Keyword::Continue => {
                        self.next_token()?;
                        self.consume(TokenType::Semicolon)?;
                        nodes::Statement::Continue("".to_string(), line)
                    },
                    Keyword::Sizeof => {
                        let expr = self.parse_expression(0)?;
                        self.consume(TokenType::Semicolon)?;

                        nodes::Statement::Expression(expr, line)
                    },
                    Keyword::Asm => {
                        self.next_token()?;

                        self.consume(TokenType::LParen)?;

                        let asm = if let TokenType::StringLiteral(ref s) = self.current_token.token_type {
                            s.clone()
                        } else {
                            return Err(errors::Error { ty: errors::ErrorType::Error, message: format!("Expected string literal in ASM statement, found {:?}", self.current_token.token_type), line: self.current_token.line });
                        };

                        self.next_token()?;

                        self.consume(TokenType::RParen)?;
                        self.consume(TokenType::Semicolon)?;

                        nodes::Statement::InlineAsm(asm, line)
                    }
                    _ => return Err(errors::Error::new(errors::ErrorType::Error, format!("Invalid keyword {:?}", keyword), self.current_token.line)),
                }
            },
            TokenType::LBrace => {
                self.next_token()?;
                let mut stmts: Vec<nodes::BlockItem> = Vec::new();

                while self.current_token.token_type != TokenType::RBrace {
                    stmts.push(self.parse_block_item()?);
                    if self.current_token.token_type == TokenType::EOF {
                        return Err(errors::Error::new(errors::ErrorType::Error, format!("Expected '}}', got EOF"), self.current_token.line));
                    }
                }

                self.next_token()?;

                nodes::Statement::Compound(stmts, line)
            }
            TokenType::Semicolon => {
                self.next_token()?;
                nodes::Statement::Empty(line)
            },
            _ => {
                let expr = nodes::Statement::Expression(self.parse_expression(0)?, line);
                self.consume(TokenType::Semicolon)?;
                expr
            },
        })
    }

    fn parse_typedef(&mut self) -> Result<Option<nodes::Declaration>, errors::Error> {
        self.next_token()?;

        let (ty, decl) = match self.current_token.token_type {
            TokenType::Keyword(ref kwd) => {
                match kwd {
                    Keyword::Int => (nodes::Type::Int, None),
                    Keyword::Char => (nodes::Type::Char, None),
                    Keyword::Void => (nodes::Type::Void, None),

                    Keyword::Extern |
                    Keyword::Static => return Err(errors::Error::new(errors::ErrorType::Error, format!("Invalid type specifier {:?}", kwd), self.current_token.line)),
    
                    Keyword::Struct => {
                        self.next_token()?;

                        if self.current_token.token_type == TokenType::LBrace {
                            let tmp_name = format!("__anon_struct_{}", self.lexer.get_type_defs_len());
                            let decl = self.parse_struct_declaration(tmp_name.clone())?;

                            (nodes::Type::Struct(tmp_name), Some(decl))
                        } else {
                            let name = match self.current_token.token_type {
                                TokenType::Identifier(ref name) => name.clone(),
                                _ => return Err(errors::Error::new(errors::ErrorType::Error, format!("Expected identifier, got {:?}", self.current_token), self.current_token.line)),
                            };

                            let str_decl = if (self.lexer.peek_token()?).token_type == TokenType::LBrace {
                                self.next_token()?;
                                Some(self.parse_struct_declaration(name.clone())?)
                            } else { None };

                            (nodes::Type::Struct(name), str_decl)
                        }
                    }
                    Keyword::TDName(name) => {
                        if let Some(ty) = self.lexer.get_type_def(&name) {
                            (ty.clone(), None)
                        } else {
                            return Err(errors::Error::new(errors::ErrorType::Error, format!("Invalid type specifier {:?}", kwd), self.current_token.line));
                        }
                    },
                    kwd => return Err(errors::Error::new(errors::ErrorType::Error, format!("Invalid type specifier {:?}", kwd), self.current_token.line)),
                }
            },
            _ => return Err(errors::Error::new(errors::ErrorType::Error, format!("Expected type specifier, got {:?}", self.current_token.token_type), self.current_token.line)),
        };

        self.next_token()?;

        let name = match self.current_token.token_type {
            TokenType::Identifier(ref name) => name.clone(),
            _ => return Err(errors::Error::new(errors::ErrorType::Error, format!("Expected identifier, got {:?}", self.current_token.token_type), self.current_token.line)),
        };

        self.next_token()?;

        self.consume(TokenType::Semicolon)?;

        self.lexer.add_type_def(name, ty);

        Ok(decl)
    }

    fn parse_if_statement(&mut self) -> Result<nodes::Statement, errors::Error> {
        let line = self.current_token.line;
        self.next_token()?;

        self.consume(TokenType::LParen)?;

        let cond = self.parse_expression(0)?;

        self.consume(TokenType::RParen)?;

        let then = self.parse_statement()?;

        let else_ = if self.current_token.token_type == TokenType::Keyword(Keyword::Else) {
            self.next_token()?;

            Some(self.parse_statement()?)
        } else {
            None
        };

        Ok(nodes::Statement::If(cond, Box::new(then), Box::new(else_), line))
    }

    fn parse_while_statement(&mut self) -> Result<nodes::Statement, errors::Error> {
        let line = self.current_token.line;
        self.next_token()?;

        self.consume(TokenType::LParen)?;

        let cond = self.parse_expression(0)?;

        self.consume(TokenType::RParen)?;

        let stmt = self.parse_statement()?;

        Ok(nodes::Statement::While(cond, Box::new(stmt), "".to_string(), line))
    }

    fn parse_do_while(&mut self) -> Result<nodes::Statement, errors::Error> {
        let line = self.current_token.line;
        self.next_token()?;

        let stmt = self.parse_statement()?;

        self.consume(TokenType::Keyword(Keyword::While))?;
        self.consume(TokenType::LParen)?;

        let cond = self.parse_expression(0)?;

        self.consume(TokenType::RParen)?;
        self.consume(TokenType::Semicolon)?;

        Ok(nodes::Statement::DoWhile(Box::new(stmt), cond, "".to_string(), line))
    }

    fn parse_for_loop(&mut self) -> Result<nodes::Statement, errors::Error> {
        let line = self.current_token.line;
        self.next_token()?;

        self.consume(TokenType::LParen)?;

        let init_line = self.current_token.line;
        // parse forinit
        let for_init = if self.current_token.token_type == TokenType::Semicolon {
            self.consume(TokenType::Semicolon)?;
            nodes::ForInit::Empty(init_line)
        } else if self.is_valid_var_starter(&self.current_token.token_type) {
            let decl = self.parse_declaration()?; // consumes semicolon
            nodes::ForInit::Declaration(decl, init_line)
        } else {
            let expr = self.parse_expression(0)?;
            self.consume(TokenType::Semicolon)?;
            nodes::ForInit::Expression(expr, init_line)
        };

        let condition = if self.current_token.token_type == TokenType::Semicolon {
            None
        } else {
            Some(self.parse_expression(0)?)
        };
    
        self.consume(TokenType::Semicolon)?;
    
        // Parse increment
        let increment = if self.current_token.token_type == TokenType::RParen {
            None
        } else {
            Some(self.parse_expression(0)?)
        };

        self.consume(TokenType::RParen)?;

        let stmt = self.parse_statement()?;

        Ok(nodes::Statement::For(for_init, condition, increment, Box::new(stmt), "".to_string(), line))
    }

    fn parse_return_statement(&mut self) -> Result<nodes::Statement, errors::Error> {
        let line = self.current_token.line;

        self.next_token()?;

        if self.current_token.token_type == TokenType::Semicolon {
            self.next_token()?;
            return Ok(nodes::Statement::Return(None, line));
        }

        let expr = self.parse_expression(0)?;

        if self.current_token.token_type != TokenType::Semicolon {
            return Err(errors::Error::new(errors::ErrorType::Error, format!("Expected semicolon, got {:?}", self.current_token.token_type), self.current_token.line));
        }

        self.next_token()?;

        Ok(nodes::Statement::Return(Some(expr), line))
    }

    fn parse_unop(&mut self) -> Result<nodes::Expression, errors::Error> {
        let line = self.current_token.line;
        Ok(nodes::Expression::new(match self.current_token.token_type {
            TokenType::Tilde => {
                self.next_token()?;
                let expr = self.parse_factor()?;
                nodes::ExpressionEnum::Unop(nodes::Unop::BitwiseNot, Box::new(expr), line)
            }
            TokenType::Minus => {
                self.next_token()?;
                let expr = self.parse_factor()?;
                nodes::ExpressionEnum::Unop(nodes::Unop::Negate, Box::new(expr), line)
            }
            TokenType::LogicalNot => {
                self.next_token()?;
                let expr = self.parse_factor()?;
                nodes::ExpressionEnum::Unop(nodes::Unop::LogicalNot, Box::new(expr), line)
            }
            TokenType::Star => {
                self.next_token()?;
                let expr = self.parse_factor()?;
                nodes::ExpressionEnum::Dereference(Box::new(expr), line)
            }
            TokenType::Ampersand => {
                self.next_token()?;
                let expr = self.parse_factor()?;
                nodes::ExpressionEnum::AddressOf(Box::new(expr), line)
            }
            _ => unreachable!("INTERNAL ERROR. PLEASE REPORT: Expected unary operator, got {:?}", self.current_token),
        }, line))
    }

    fn get_precedence(&self, token: &TokenType) -> i32 {
        match token {
            TokenType::Increment | TokenType::Decrement => 60,
            TokenType::Star | TokenType::Slash | TokenType::Percent => 55,
            TokenType::Plus | TokenType::Minus => 45,
            TokenType::LeftShift | TokenType::RightShift => 40,
            TokenType::LessThan | TokenType::GreaterThan |
            TokenType::LessThanEqual | TokenType::GreaterThanEqual => 35,
            TokenType::Equal | TokenType::NotEqual => 30,
            TokenType::Ampersand => 28,
            TokenType::BitwiseXor => 27,
            TokenType::BitwiseOr => 26,
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
            TokenType::LeftShift => nodes::Binop::LeftShift,
            TokenType::RightShift => nodes::Binop::RightShift,
            TokenType::Star => nodes::Binop::Multiply,
            TokenType::Slash => nodes::Binop::Divide,
            TokenType::Ampersand => nodes::Binop::BitwiseAnd,
            TokenType::BitwiseOr => nodes::Binop::BitwiseOr,
            TokenType::BitwiseXor => nodes::Binop::BitwiseXor,
            TokenType::Percent => nodes::Binop::Modulus,
            _ => unreachable!("INTERNAL ERROR. PLEASE REPORT: Expected binary operator, got {:?}", token),
        }
    }

    fn is_op_assign(&self, token: &TokenType) -> (bool, nodes::Binop) {
        match token {
            TokenType::AddAssign => (true, nodes::Binop::Add),
            TokenType::SubAssign => (true, nodes::Binop::Subtract),
            TokenType::MulAssign => (true, nodes::Binop::Multiply),
            TokenType::DivAssign => (true, nodes::Binop::Divide),
            _ => (false, nodes::Binop::Add),
        }
    }

    fn parse_expression(&mut self, min_prec: i32) -> Result<nodes::Expression, errors::Error> {
        let mut expr = self.parse_factor()?;

        // dont mind the weird fuckery
        let mut prec: i32;
        let mut is_op_assign: (bool, nodes::Binop);
        while (prec = self.get_precedence(&self.current_token.token_type), prec).1 >= min_prec {
            let line = self.current_token.line;
            if self.current_token.token_type == TokenType::Equals {
                self.next_token()?;
                expr = nodes::Expression::new(nodes::ExpressionEnum::Assign(Box::new(expr), Box::new(self.parse_expression(prec)?), line), line);
            } else if (is_op_assign = self.is_op_assign(&self.current_token.token_type), is_op_assign.0).1 {
                self.next_token()?;
                expr = nodes::Expression::new(nodes::ExpressionEnum::OpAssign(is_op_assign.1, Box::new(expr), Box::new(self.parse_expression(prec)?), line), line);
            } else if self.current_token.token_type == TokenType::QuestionMark {
                self.next_token()?;
                let middle = self.parse_expression(0)?;
                self.consume(TokenType::Colon)?;
                let right = self.parse_expression(prec)?;
                expr = nodes::Expression::new(nodes::ExpressionEnum::Conditional(Box::new(expr), Box::new(middle), Box::new(right), line), line);
            } else if self.current_token.token_type == TokenType::Increment || self.current_token.token_type == TokenType::Decrement {
                if self.current_token.token_type == TokenType::Increment {
                    expr = nodes::Expression::new(nodes::ExpressionEnum::Increment(Box::new(expr), line), line);
                } else {
                    expr = nodes::Expression::new(nodes::ExpressionEnum::Decrement(Box::new(expr), line), line);
                }
                self.next_token()?;
            } else {
                let op = self.convert_binop(&self.current_token.token_type);
                self.next_token()?;
                let right = self.parse_expression(prec + 1)?;
                expr = nodes::Expression::new(nodes::ExpressionEnum::Binop(op, Box::new(expr), Box::new(right), line), line);
            }
        }

        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<nodes::Expression, errors::Error> {
        let mut fac = self.parse_primary_factor()?;
        loop {
            let line = self.current_token.line;
            match self.current_token.token_type {
                TokenType::LBracket => {
                    self.next_token()?;
                    let idx = self.parse_expression(0)?;
                    self.consume(TokenType::RBracket)?;
                    fac = nodes::Expression::new(nodes::ExpressionEnum::Subscript(Box::new(fac), Box::new(idx), line), line);
                },
                TokenType::Period => {
                    self.next_token()?;
                    let field = match self.current_token.token_type {
                        TokenType::Identifier(ref name) => name.clone(),
                        _ => return Err(errors::Error::new(errors::ErrorType::Error, format!("Expected identifier, got {:?}", self.current_token.token_type), self.current_token.line)),
                    };
                    self.next_token()?;
                    fac = nodes::Expression::new(nodes::ExpressionEnum::Dot(Box::new(fac), field, line), line);
                },
                TokenType::Arrow => {
                    self.next_token()?;
                    let field = match self.current_token.token_type {
                        TokenType::Identifier(ref name) => name.clone(),
                        _ => return Err(errors::Error::new(errors::ErrorType::Error, format!("Expected identifier, got {:?}", self.current_token.token_type), self.current_token.line)),
                    };
                    self.next_token()?;
                    fac = nodes::Expression::new(nodes::ExpressionEnum::Arrow(Box::new(fac), field, line), line);
                }
                _ => return Ok(fac),
            }
        }
    }

    fn parse_abstract_declarator(&mut self) -> Result<AbstractDeclarator, errors::Error> {
        Ok(match self.current_token.token_type {
            TokenType::Star => {
                self.next_token()?;
                AbstractDeclarator::Pointer(Box::new(self.parse_abstract_declarator()?))
            },
            TokenType::LParen => {
                self.next_token()?;
                let decl = self.parse_abstract_declarator()?;
                self.consume(TokenType::RParen)?;
                decl
            },
            TokenType::LBracket => {
                self.next_token()?;
                let size = match self.current_token.token_type {
                    TokenType::IntegerLiteral(i) => {
                        self.next_token()?;
                        i
                    },
                    _ => return Err(errors::Error::new(errors::ErrorType::Error, format!("Expected integer literal, got {:?}", self.current_token.token_type), self.current_token.line)),
                };
                self.consume(TokenType::RBracket)?;
                let inner = self.parse_abstract_declarator()?;
                match inner {
                    AbstractDeclarator::Array(_, _) | AbstractDeclarator::Base => (),
                    a => return Err(errors::Error::new(errors::ErrorType::Error, format!("Invalid array declarator, {:?}", a), self.current_token.line)),
                }
                AbstractDeclarator::Array(Box::new(inner), size as i16)
            },
            _ => AbstractDeclarator::Base,
        })
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

    fn parse_primary_factor(&mut self) -> Result<nodes::Expression, errors::Error> {
        let cur_tok = self.current_token.token_type.clone();
        let line = self.current_token.line;
        Ok(match cur_tok {
            TokenType::IntegerLiteral(i) => {
                self.next_token()?;
                nodes::Expression::new(nodes::ExpressionEnum::IntegerLiteral(i, line), line)
            }
            TokenType::CharLiteral(ch) => {
                self.next_token()?;
                nodes::Expression::new(nodes::ExpressionEnum::CharLiteral(ch, line), line)
            }
            TokenType::StringLiteral(s) => {
                self.next_token()?;
                nodes::Expression::new(nodes::ExpressionEnum::StringLiteral(s, line), line)
            }
            TokenType::Keyword(kwd) => {
                if kwd == Keyword::Sizeof {
                    self.next_token()?;
                    let peek_token = self.lexer.peek_token()?.token_type;
                    if self.current_token.token_type == TokenType::LParen && self.is_valid_var_starter(&peek_token) {
                        self.next_token()?;
                        let types = self.parse_types()?;
                        let (type_, _) = self.parse_type_and_storage_class(types)?;
                        self.consume(TokenType::RParen)?;
                        nodes::Expression::new(nodes::ExpressionEnum::SizeOfType(type_, line), line)
                    } else {
                        nodes::Expression::new(nodes::ExpressionEnum::SizeOf(Box::new(self.parse_expression(1)?), line), line)
                    }
                } else {
                    return Err(errors::Error::new(errors::ErrorType::Error, format!("Unexpected keyword {:?} in expression", kwd), self.current_token.line));
                }
            }
            TokenType::Identifier(ident) => {
                self.next_token()?;
                if self.current_token.token_type == TokenType::LParen {
                    self.next_token()?;
                    let mut args: Vec<nodes::Expression> = Vec::new();
                    while self.current_token.token_type != TokenType::RParen {
                        args.push(self.parse_expression(0)?);
                        if self.current_token.token_type == TokenType::Comma && self.lexer.peek_token()?.token_type != TokenType::RParen {
                            self.next_token()?;
                        } else if self.current_token.token_type != TokenType::RParen {
                            return Err(errors::Error::new(errors::ErrorType::Error, format!("Expected ',' or ')', got {:?}", self.current_token), self.current_token.line));
                        }
                    }
                    self.next_token()?;
                    nodes::Expression::new(nodes::ExpressionEnum::FunctionCall(ident, args, line), line)
                } else {
                    nodes::Expression::new(nodes::ExpressionEnum::Var(ident, line), line)
                }
            }
            TokenType::Tilde | TokenType::Minus |
            TokenType::LogicalNot |
            TokenType::Star | TokenType::Ampersand => self.parse_unop()?,
            TokenType::LParen => {
                self.next_token()?;

                if let Some(expr) = self.check_for_cast()? {
                    return Ok(expr);
                }

                let expr = self.parse_expression(0)?;
                self.consume(TokenType::RParen)?;
                expr
            },
            _ => return Err(errors::Error::new(errors::ErrorType::Error, format!("Unexpected token {:?}", self.current_token.token_type), self.current_token.line)),
        })
    }

    fn check_for_cast(&mut self) -> Result<Option<nodes::Expression>, errors::Error> {
        if self.is_valid_var_starter(&self.current_token.token_type) {
            let line = self.current_token.line;
            // explicit cast
            let types = self.parse_types()?;

            let (type_, spec) = self.parse_type_and_storage_class(types)?;

            if spec != nodes::StorageClass::Auto {
                return Err(errors::Error::new(errors::ErrorType::Error, String::from("Cast cannot have storage class"), self.current_token.line));
            }

            // parse abstract declarator
            let decl = self.parse_abstract_declarator()?;
            let type_ = self.process_abstract_declarator(decl, type_);

            self.consume(TokenType::RParen)?;

            let expr = self.parse_expression(0)?;

            return Ok(Some(nodes::Expression::new(nodes::ExpressionEnum::Cast(type_, Box::new(expr), line), line)));
        } else { Ok(None) }
    }
}