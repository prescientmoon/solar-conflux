use std::vec::Vec;

pub enum PunctuationKind {
    OpenParenthesis,
    CloseParenthesis,
}

pub enum TokenKind {
    NumberLit,
    StringLit,
    BooleanLit,
    Operator,
    Punctuation(PunctuationKind),
    MissingToken,
}

pub struct Token {
    kind: TokenKind,
    start: u32,
    end: u32,
}

pub struct Lexer {
    pub position: u32,
    pub tokens: Vec<Token>,
}

impl Lexer {
    pub fn new() -> Lexer {
        Lexer {
            position: 0,
            tokens: Vec::new(),
        }
    }
}

pub fn skip_whitespace(string: &str) -> &str {
    return string.trim_start();
}
