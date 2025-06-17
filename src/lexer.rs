use std::collections::HashMap;

use super::parser::nodes::Type;
use super::errors;

// this macro takes in tuples of the form (String, TokenType). the first tuple is the (0) current char and then (1) what to return if nothing matches
// for the rest of the tuples, if the current char matches the first element, return the second element
macro_rules! after_char {
    ($slf: expr, $n:expr, $(($c:expr, $t:expr)),*) => {
        match ($slf.peek_char())? {
            $(
                $c => { $slf.read_char(); $t },
            )*
            _ => $n,
        }
    };
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Keyword(Keyword),
    // Symbols
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Semicolon,
    Comma,
    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Tilde,
    QuestionMark,
    Colon,
    Ampersand,
    BitwiseOr,
    BitwiseXor,
    // Assignment operators
    Equals,
    
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,

    Increment,
    Decrement,
    // Logical & Relational operators
    LogicalNot,
    LogicalAnd,
    LogicalOr,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,

    RightShift,
    LeftShift,

    Period,
    Arrow,

    // Literals
    Identifier(String),
    IntegerLiteral(i16),
    CharLiteral(char),
    StringLiteral(String),
    // End of file
    EOF,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    Int,
    Void,
    Char,
    Static,
    Extern,
    Struct,
    Return,
    If,
    Else,
    While,
    For,
    Do,
    Break,
    Continue,
    Sizeof,
    Typedef,
    Asm,
    TDName(String),
}

impl Keyword {
    pub fn from_str(s: &str) -> Option<Keyword> {
        match s {
            "int" => Some(Keyword::Int),
            "void" => Some(Keyword::Void),
            "char" => Some(Keyword::Char),
            "static" => Some(Keyword::Static),
            "extern" => Some(Keyword::Extern),
            "struct" => Some(Keyword::Struct),
            "return" => Some(Keyword::Return),
            "if" => Some(Keyword::If),
            "else" => Some(Keyword::Else),
            "while" => Some(Keyword::While),
            "for" => Some(Keyword::For),
            "do" => Some(Keyword::Do),
            "break" => Some(Keyword::Break),
            "continue" => Some(Keyword::Continue),
            "sizeof" => Some(Keyword::Sizeof),
            "typedef" => Some(Keyword::Typedef),
            "__asm__" => Some(Keyword::Asm),
            _ => None
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub line: usize,
}

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
    type_defs: HashMap<String, Type>,
    current_line: usize,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: '\0',
            type_defs: HashMap::new(),
            current_line: 1,
        };
        l.read_char();
        l
    }

    pub fn add_type_def(&mut self, name: String, ty: Type) {
        self.type_defs.insert(name, ty);
    }

    pub fn get_type_def(&self, name: &str) -> Option<&Type> {
        self.type_defs.get(name)
    }

    pub fn get_type_defs_len(&self) -> usize {
        self.type_defs.len()
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input.chars().nth(self.read_position).unwrap();
        }
        if self.ch == '\n' {
            self.current_line += 1;
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> Result<char, errors::Error> {
        if self.read_position >= self.input.len() {
            Ok('\0')
        } else {
            match self.input.chars().nth(self.read_position) {
                Some(ch) => Ok(ch),
                None => Err(errors::Error::new(errors::ErrorType::Error, "Failed to peek char".to_string(), 0)),
            }
        }
    }

    pub fn next_token(&mut self) -> Result<Token, errors::Error> {
        self.skip_whitespace();

        let cur_line = self.current_line;

        let tok = match self.ch {
            '(' => TokenType::LParen,
            ')' => TokenType::RParen,
            '{' => TokenType::LBrace,
            '}' => TokenType::RBrace,
            '[' => TokenType::LBracket,
            ']' => TokenType::RBracket,
            ';' => TokenType::Semicolon,
            ',' => TokenType::Comma,
            '+' => after_char!(self,
                TokenType::Plus,
                ('=', TokenType::AddAssign),
                ('+', TokenType::Increment)
            ),
            '-' => after_char!(self,
                TokenType::Minus,
                ('=', TokenType::SubAssign),
                ('-', TokenType::Decrement),
                ('>', TokenType::Arrow)
            ),
            '*' => after_char!(self,
                TokenType::Star,
                ('=', TokenType::MulAssign)
            ),
            '/' => after_char!(self,
                TokenType::Slash,
                ('=', TokenType::DivAssign)
            ),
            '%' => after_char!(self,
                TokenType::Percent,
                ('=', TokenType::ModAssign)
            ),
            '~' => TokenType::Tilde,
            '=' => after_char!(self,
                TokenType::Equals,
                ('=', TokenType::Equal)
            ),
            '!' => after_char!(self,
                TokenType::LogicalNot,
                ('=', TokenType::NotEqual)
            ),
            '&' => after_char!(self,
                TokenType::Ampersand,
                ('&', TokenType::LogicalAnd)
            ),
            '|' => after_char!(self,
                TokenType::BitwiseOr,
                ('|', TokenType::LogicalOr)
            ),
            '^' => TokenType::BitwiseXor,
            '<' => after_char!(self,
                TokenType::LessThan,
                ('=', TokenType::LessThanEqual),
                ('<', TokenType::LeftShift)
            ),
            '>' => after_char!(self,
                TokenType::GreaterThan,
                ('=', TokenType::GreaterThanEqual),
                ('>', TokenType::RightShift)
            ),
            '\'' => {
                self.read_char();
                let ch = if self.ch == '\\' {
                    self.parse_backslash()?
                } else {
                    self.ch
                };
                self.read_char();
                if self.ch == '\'' {
                    TokenType::CharLiteral(ch)
                } else {
                    return Err(errors::Error::new(errors::ErrorType::Error, "Expected closing single quote".to_string(), self.current_line));
                }
            },
            '"' => {
                let mut s = String::new();

                self.read_char();
                while self.ch != '"' {
                    if self.ch == '\\' {
                        s.push(self.parse_backslash()?);
                    } else {
                        s.push(self.ch);
                    }
                    self.read_char();
                }

                TokenType::StringLiteral(s)
            },
            '?' => TokenType::QuestionMark,
            ':' => TokenType::Colon,
            '.' => TokenType::Period,
            '\0' => TokenType::EOF,
            _ => {
                if is_letter(self.ch) {
                    let ident = self.read_identifier();

                    if self.get_type_def(&ident).is_some() {
                        return Ok(Token {
                            token_type: TokenType::Keyword(Keyword::TDName(ident)),
                            line: cur_line,
                        });
                    }

                    if let Some(kwd) = Keyword::from_str(&ident) {
                        return Ok(Token {
                            token_type: TokenType::Keyword(kwd),
                            line: cur_line
                        });
                    }

                    return Ok(Token {
                        token_type: TokenType::Identifier(ident),
                        line: cur_line,
                    });
                } else if is_digit(self.ch) {
                    return Ok(Token {
                        token_type: TokenType::IntegerLiteral(self.read_number()?),
                        line: cur_line,
                    });
                } else {
                    return Err(errors::Error::new(errors::ErrorType::Error, format!("Invalid character ' {} '", self.ch), self.current_line));
                }
            }
        };

        self.read_char();
        Ok(Token {
            token_type: tok,
            line: cur_line,
        })
    }

    pub fn peek_token(&mut self) -> Result<Token, errors::Error> {
        let pos = self.position;
        let read_pos = self.read_position;
        let ch = self.ch;
        let line = self.current_line;
        let tok = self.next_token();
        self.position = pos;
        self.read_position = read_pos;
        self.ch = ch;
        self.current_line = line;
        tok
    }

    fn parse_backslash(&mut self) -> Result<char, errors::Error> {
        self.read_char();
        match self.ch {
            '0' => Ok('\0'),
            'n' => Ok('\n'),
            '\\' => Ok('\\'),
            _ => Err(errors::Error::new(errors::ErrorType::Error, format!("invalid escape sequence \\{}", self.ch), 0)),
        }
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            self.read_char();
        }
    }

    fn read_identifier(&mut self) -> String {
        let start = self.position;
        while is_letter(self.ch) || is_digit(self.ch) {
            self.read_char();
        }
        self.input[start..self.position].to_string()
    }

    fn read_number(&mut self) -> Result<i16, errors::Error> {
        let start = self.position;
        let start_line = self.current_line;
        while is_digit(self.ch) {
            self.read_char();
        }
        let num = self.input[start..self.position].parse::<i16>();

        let n = match num {
            Ok(n) => n,
            Err(h) => return Err(errors::Error::new(errors::ErrorType::Error, h.to_string(), start_line)),
        };

        if n < -128 || n > 255 {
            return Err(errors::Error::new(errors::ErrorType::Error, "Number out of range".to_string(), start_line));
        }

        Ok(n)
    }
}

fn is_letter(ch: char) -> bool {
    ch.is_ascii_alphabetic() || ch == '_'
}

fn is_digit(ch: char) -> bool {
    ch.is_ascii_digit()
}