use std::ops::Range;

use lasso::{Rodeo, Spur};
use logos::Logos;

use crate::intern::INTERNER;

type Span = Range<usize>;

#[derive(Logos, Debug, PartialEq)]
enum RawToken {
    #[token("fn")]
    Fn,

    #[token("if")]
    If,

    #[token("else")]
    Else,

    #[token("let")]
    Let,

    #[token("=")]
    Assign,

    #[token("==")]
    Equals,

    #[token(";")]
    Semicolon,

    #[token("{")]
    OpenCurly,

    #[token("}")]
    CloseCurly,

    #[token("(")]
    OpenRound,

    #[token(")")]
    CloseRound,

    #[token(",")]
    Comma,

    #[regex("[a-zA-Z]+")]
    Identifier,

    #[regex(r#""(\\.|[^"])*""#)]
    String,

    #[regex(r#"[0-9]+"#)]
    Number,

    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Fn,
    If,
    Else,
    Let,
    Equals,
    Assign,
    Semicolon,
    OpenCurly,
    CloseCurly,
    OpenRound,
    CloseRound,
    Comma,
    Identifier(Spur),
    String(Spur),
    Number(i64),
}

pub struct Lexer<'source> {
    lex: logos::Lexer<'source, RawToken>,
}

#[derive(Debug)]
pub enum LexError {
    Unknown(Span),
}

type Result<T, E = LexError> = std::result::Result<T, E>;

impl Lexer<'_> {
    pub fn new(source: &str) -> Lexer {
        Lexer {
            lex: RawToken::lexer(source),
        }
    }

    pub fn next(&mut self) -> Result<Option<(Token, Span)>> {
        let interner = &*INTERNER;
        let token = match self.lex.next() {
            Some(t) => t,
            None => return Ok(None),
        };

        let slice = self.lex.slice();
        let mut intern = |v: &str| interner.get_or_intern(v);

        let token = match token {
            RawToken::Fn => Token::Fn,
            RawToken::If => Token::If,
            RawToken::Else => Token::Else,
            RawToken::Let => Token::Let,
            RawToken::Semicolon => Token::Semicolon,
            RawToken::OpenCurly => Token::OpenCurly,
            RawToken::CloseCurly => Token::CloseCurly,
            RawToken::OpenRound => Token::OpenRound,
            RawToken::CloseRound => Token::CloseRound,
            RawToken::Comma => Token::Comma,

            RawToken::Identifier => Token::Identifier(intern(slice)),
            RawToken::String => Token::String(intern(&slice[1..slice.len() - 1])),
            RawToken::Number => Token::Number(slice.parse().unwrap()),
            RawToken::Error => return Err(LexError::Unknown(self.lex.span())),
            RawToken::Assign => Token::Assign,
            RawToken::Equals => Token::Equals,
        };

        Ok(Some((token, self.lex.span())))
    }

    pub fn collect(&mut self) -> Result<Vec<(Token, Span)>> {
        let mut tokens = Vec::new();
        while let Some(token) = self.next()? {
            tokens.push(token);
        }
        Ok(tokens)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn simple() {
        let source = r#"fn main() { print("hello"); }"#;
        let mut lexer = Lexer::new(source);
        let tokens = lexer.collect().unwrap();

        let key = |v| INTERNER.get(v).unwrap();

        use Token::*;
        assert_eq!(
            tokens,
            vec![
                (Fn, 0..2),
                (Identifier(key("main")), 3..7),
                (OpenRound, 7..8),
                (CloseRound, 8..9),
                (OpenCurly, 10..11),
                (Identifier(key("print")), 12..17),
                (OpenRound, 17..18),
                (String(key("hello")), 18..25),
                (CloseRound, 25..26),
                (Semicolon, 26..27),
                (CloseCurly, 28..29)
            ]
        );
    }
}
