use std::collections::HashMap;

use super::parser::nodes::Type;

// this macro takes in tuples of the form (String, TokenType). the first tuple is the (0) current char and then (1) what to return if nothing matches
// for the rest of the tuples, if the current char matches the first element, return the second element
macro_rules! after_char {
    ($slf: expr, $ch:expr, $n:expr, $(($c:expr, $t:expr)),*) => {
        match $ch {
            $(
                $c => { $slf.read_char(); $t },
            )*
            _ => $n,
        }
    };
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Keyword(String),
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
    // Error
    Illegal(String),
}

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
    type_defs: HashMap<String, Type>,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: '\0',
            type_defs: HashMap::new(),
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
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input.chars().nth(self.read_position).unwrap()
        }
    }

    pub fn next_token(&mut self) -> TokenType {
        self.skip_whitespace();

        let tok = match self.ch {
            '(' => TokenType::LParen,
            ')' => TokenType::RParen,
            '{' => TokenType::LBrace,
            '}' => TokenType::RBrace,
            '[' => TokenType::LBracket,
            ']' => TokenType::RBracket,
            ';' => TokenType::Semicolon,
            ',' => TokenType::Comma,
            '+' => after_char!(self, self.peek_char(),
                TokenType::Plus,
                ('=', TokenType::AddAssign),
                ('+', TokenType::Increment)
            ),
            '-' => after_char!(self, self.peek_char(),
                TokenType::Minus,
                ('=', TokenType::SubAssign),
                ('-', TokenType::Decrement),
                ('>', TokenType::Arrow)
            ),
            '*' => after_char!(self, self.peek_char(),
                TokenType::Star,
                ('=', TokenType::MulAssign)
            ),
            '/' => after_char!(self, self.peek_char(),
                TokenType::Slash,
                ('=', TokenType::DivAssign)
            ),
            '~' => TokenType::Tilde,
            '=' => after_char!(self, self.peek_char(),
                TokenType::Equals,
                ('=', TokenType::Equal)
            ),
            '!' => after_char!(self, self.peek_char(),
                TokenType::LogicalNot,
                ('=', TokenType::NotEqual)
            ),
            '&' => after_char!(self, self.peek_char(),
                TokenType::Ampersand,
                ('&', TokenType::LogicalAnd)
            ),
            '|' => after_char!(self, self.peek_char(),
                TokenType::BitwiseOr,
                ('|', TokenType::LogicalOr)
            ),
            '^' => TokenType::BitwiseXor,
            '<' => after_char!(self, self.peek_char(),
                TokenType::LessThan,
                ('=', TokenType::LessThanEqual),
                ('<', TokenType::LeftShift)
            ),
            '>' => after_char!(self, self.peek_char(),
                TokenType::GreaterThan,
                ('=', TokenType::GreaterThanEqual),
                ('>', TokenType::RightShift)
            ),
            '\'' => {
                self.read_char();
                let ch = if self.ch == '\\' {
                    self.parse_backslash()
                } else {
                    self.ch
                };
                self.read_char();
                if self.ch == '\'' {
                    TokenType::CharLiteral(ch)
                } else {
                    TokenType::Illegal(format!("invalid char literal (expected ' found {:?}", self.ch))
                }
            },
            '"' => {
                let mut s = String::new();

                self.read_char();
                while self.ch != '"' {
                    if self.ch == '\\' {
                        s.push(self.parse_backslash());
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

                    let keywords = [
                        "int", "void", "char",
                        "static", "extern",
                        "struct",
                        "return",
                        "if", "else",
                        "while", "for", "do", "break", "continue",
                        "sizeof",
                        "typedef",
                    ];

                    if self.get_type_def(&ident).is_some() {
                        return TokenType::Keyword(ident);
                    }

                    if keywords.contains(&ident.as_str()) {
                        return TokenType::Keyword(ident);
                    }

                    return TokenType::Identifier(ident);
                } else if is_digit(self.ch) {
                    return TokenType::IntegerLiteral(self.read_number());
                } else {
                    TokenType::Illegal(format!("invalid char {:?}", self.ch))
                }
            }
        };

        self.read_char();
        tok
    }

    pub fn peek_token(&mut self) -> TokenType {
        let pos = self.position;
        let read_pos = self.read_position;
        let ch = self.ch;
        let tok = self.next_token();
        self.position = pos;
        self.read_position = read_pos;
        self.ch = ch;
        tok
    }

    fn parse_backslash(&mut self) -> char {
        self.read_char();
        match self.ch {
            '0' => '\0',
            'n' => '\n',
            '\\' => '\\',
            _ => panic!("invalid char after \\")
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

    fn read_number(&mut self) -> i16 {
        let start = self.position;
        while is_digit(self.ch) {
            self.read_char();
        }
        let num = self.input[start..self.position].parse::<i16>();

        match num {
            Ok(n) => n as i16,
            Err(_) => panic!("Failed to parse number"),
        }
    }
}

fn is_letter(ch: char) -> bool {
    ch.is_ascii_alphabetic() || ch == '_'
}

fn is_digit(ch: char) -> bool {
    ch.is_ascii_digit()
}