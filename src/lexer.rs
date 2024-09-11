#![allow(dead_code)]

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
    // Assignment operators
    Equals,
    AddAssign,
    SubAssign,
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
    // Literals
    Identifier(String),
    IntegerLiteral(i8),
    CharLiteral(char),
    // End of file
    EOF,
    // Error
    Illegal,
}

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: '\0',
        };
        l.read_char();
        l
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
                ('-', TokenType::Decrement)
            ),
            '*' => TokenType::Star,
            '/' => TokenType::Slash,
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
                TokenType::Illegal,
                ('|', TokenType::LogicalOr)
            ),
            '<' => after_char!(self, self.peek_char(),
                TokenType::LessThan,
                ('=', TokenType::LessThanEqual)
            ),
            '>' => after_char!(self, self.peek_char(),
                TokenType::GreaterThan,
                ('=', TokenType::GreaterThanEqual)
            ),
            '\'' => {
                self.read_char();
                let ch = self.ch;
                self.read_char();
                if self.ch == '\'' {
                    TokenType::CharLiteral(ch)
                } else {
                    TokenType::Illegal
                }
            },
            '?' => TokenType::QuestionMark,
            ':' => TokenType::Colon,
            '\0' => TokenType::EOF,
            _ => {
                if is_letter(self.ch) {
                    let ident = self.read_identifier();

                    let keywords = [
                        "int", "void", "char",
                        "return",
                        "if", "else",
                        "while", "for", "do", "break", "continue",
                        "static", "extern",
                    ];
                    if keywords.contains(&ident.as_str()) {
                        return TokenType::Keyword(ident);
                    } else {
                        return TokenType::Identifier(ident);
                    }
                } else if is_digit(self.ch) {
                    return TokenType::IntegerLiteral(self.read_number());
                } else {
                    TokenType::EOF
                }
            }
        };

        self.read_char();
        tok
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            self.read_char();
        }
        while self.ch == '/' && (self.peek_char() == '/' || self.peek_char() == '*') {
            if self.peek_char() == '*' {
                self.read_char();
                self.read_char();
                while self.ch != '*' && self.peek_char() != '/' {
                    self.read_char();
                }
                self.read_char();
                self.read_char();
            } else {
                while self.ch != '\n' {
                    self.read_char();
                }
                self.read_char();
            }
            while self.ch.is_whitespace() {
                self.read_char();
            }
        }
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

    fn read_number(&mut self) -> i8 {
        let start = self.position;
        while is_digit(self.ch) {
            self.read_char();
        }
        let num = self.input[start..self.position].parse::<i16>();

        match num {
            Ok(n) => n as i8,
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