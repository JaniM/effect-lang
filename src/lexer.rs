use std::ops::Range;

use lasso::Spur;
use logos::Logos;

use crate::intern::INTERNER;

type Span = Range<usize>;

#[derive(Logos, Debug, PartialEq)]
enum RawToken {
    #[token("effect")]
    Effect,
    #[token("handle")]
    Handle,
    #[token("ctl")]
    Ctl,
    #[token("fn")]
    Fn,
    #[token("return")]
    Return,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("while")]
    While,
    #[token("let")]
    Let,
    #[token("=")]
    Assign,
    #[token("==")]
    Equals,
    #[token("<")]
    LessThan,
    #[token(">")]
    GreaterThan,
    #[token("->")]
    RightArrow,
    #[token("+")]
    Plus,
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,
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
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
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
pub enum Delim {
    Round,
    Curly,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Effect,
    Handle,
    Ctl,
    Fn,
    Return,
    If,
    Else,
    While,
    Let,
    Equals,
    LessThan,
    GreaterThan,
    RightArrow,
    Assign,
    Plus,
    Semicolon,
    Colon,
    Open(Delim),
    Close(Delim),
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
        let intern = |v: &str| interner.get_or_intern(v);

        let token = match token {
            RawToken::Effect => Token::Effect,
            RawToken::Handle => Token::Handle,
            RawToken::Ctl => Token::Ctl,
            RawToken::Fn => Token::Fn,
            RawToken::Return => Token::Return,
            RawToken::If => Token::If,
            RawToken::Else => Token::Else,
            RawToken::While => Token::While,
            RawToken::Let => Token::Let,
            RawToken::Semicolon => Token::Semicolon,
            RawToken::Colon => Token::Colon,
            RawToken::OpenCurly => Token::Open(Delim::Curly),
            RawToken::CloseCurly => Token::Close(Delim::Curly),
            RawToken::OpenRound => Token::Open(Delim::Round),
            RawToken::CloseRound => Token::Close(Delim::Round),
            RawToken::Comma => Token::Comma,

            RawToken::Identifier => Token::Identifier(intern(slice)),
            RawToken::String => Token::String(intern(&slice[1..slice.len() - 1])),
            RawToken::Number => Token::Number(slice.parse().unwrap()),
            RawToken::Error => return Err(LexError::Unknown(self.lex.span())),
            RawToken::Assign => Token::Assign,
            RawToken::Equals => Token::Equals,
            RawToken::LessThan => Token::LessThan,
            RawToken::GreaterThan => Token::GreaterThan,
            RawToken::RightArrow => Token::RightArrow,
            RawToken::Plus => Token::Plus,
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
                (Open(Delim::Round), 7..8),
                (Close(Delim::Round), 8..9),
                (Open(Delim::Curly), 10..11),
                (Identifier(key("print")), 12..17),
                (Open(Delim::Round), 17..18),
                (String(key("hello")), 18..25),
                (Close(Delim::Round), 25..26),
                (Semicolon, 26..27),
                (Close(Delim::Curly), 28..29)
            ]
        );
    }
}
