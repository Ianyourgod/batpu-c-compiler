#![allow(dead_code)]

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    // Keywords
    Int,
    Return,
    // Symbols
    LParen,
    RParen,
    LBrace,
    RBrace,
    Semicolon,
    Comma,
    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Tilde,
    // Literals
    Identifier(String),
    IntegerLiteral(i8),
    // End of file
    EOF,
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
            ';' => TokenType::Semicolon,
            ',' => TokenType::Comma,
            '+' => TokenType::Plus,
            '-' => TokenType::Minus,
            '*' => TokenType::Star,
            '/' => TokenType::Slash,
            '~' => TokenType::Tilde,
            '\0' => TokenType::EOF,
            _ => {
                if is_letter(self.ch) {
                    let ident = self.read_identifier();
                    return match ident.as_str() {
                        "int" => TokenType::Int,
                        "return" => TokenType::Return,
                        _ => TokenType::Identifier(ident),
                    };
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
        let num = self.input[start..self.position].parse::<i8>();

        match num {
            Ok(n) => n,
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