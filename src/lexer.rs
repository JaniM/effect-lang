use std::ops::Range;

use lasso::{Rodeo, Spur};
use logos::Logos;

type Span = Range<usize>;

#[derive(Logos, Debug, PartialEq)]
enum RawToken {
    #[token("fn")]
    Fn,

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

    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Fn,
    Semicolon,
    OpenCurly,
    CloseCurly,
    OpenRound,
    CloseRound,
    Comma,
    Identifier(Spur),
    String(Spur),
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

    pub fn next(&mut self, interner: &mut Rodeo) -> Result<Option<(Token, Span)>> {
        let token = match self.lex.next() {
            Some(t) => t,
            None => return Ok(None),
        };

        let slice = self.lex.slice();
        let mut intern = |v: &str| interner.get_or_intern(v);

        let token = match token {
            RawToken::Fn => Token::Fn,
            RawToken::Semicolon => Token::Semicolon,
            RawToken::OpenCurly => Token::OpenCurly,
            RawToken::CloseCurly => Token::CloseCurly,
            RawToken::OpenRound => Token::OpenRound,
            RawToken::CloseRound => Token::CloseRound,
            RawToken::Comma => Token::Comma,

            RawToken::Identifier => Token::Identifier(intern(slice)),
            RawToken::String => Token::String(intern(&slice[1..slice.len() - 1])),
            RawToken::Error => return Err(LexError::Unknown(self.lex.span())),
        };

        Ok(Some((token, self.lex.span())))
    }

    pub fn collect(&mut self, interner: &mut Rodeo) -> Result<Vec<(Token, Span)>> {
        let mut tokens = Vec::new();
        while let Some(token) = self.next(interner)? {
            tokens.push(token);
        }
        Ok(tokens)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use lasso::Rodeo;

    #[test]
    fn simple() {
        let source = r#"fn main() { print("hello"); }"#;
        let mut interner = Rodeo::default();
        let mut lexer = Lexer::new(source);
        let tokens = lexer.collect(&mut interner).unwrap();

        let key = |v| interner.get(v).unwrap();

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
