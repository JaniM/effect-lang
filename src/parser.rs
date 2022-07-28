use std::ops::Range;

use chumsky::{prelude::*, Parser, Stream};
use lasso::{Rodeo, Spur};

use crate::lexer::{Lexer, Token};

pub type Span = Range<usize>;

/// Parser alias
trait MParser<T, E: chumsky::Error<Token> = Simple<Token>>:
    chumsky::Parser<Token, T, Error = E>
{
}
impl<P, T, E: chumsky::Error<Token>> MParser<T, E> for P where
    P: chumsky::Parser<Token, T, Error = E>
{
}

#[derive(Debug, PartialEq)]
pub struct FnDef {
    pub name: Option<Spur>,
    pub body: Vec<Node>,
}

#[derive(Debug, PartialEq)]
pub struct Call {
    pub callee: Box<Node>,
    pub args: Vec<Node>,
}

#[derive(Debug, PartialEq)]
pub enum RawNode {
    FnDef(FnDef),
    Call(Call),
    Name(Spur),
    String(Spur),
}

#[derive(Debug, PartialEq)]
pub struct Spanned<T>(pub T, pub Span);

pub type Node = Spanned<RawNode>;

fn ident() -> impl MParser<Spur> {
    select! { Token::Identifier(x) => x }
}

fn atom() -> impl MParser<Node> {
    select! {
        Token::String(x) => RawNode::String(x),
        Token::Identifier(x) => RawNode::Name(x),
    }
    .map_with_span(Spanned)
}

fn list_of<O>(base: impl MParser<O>) -> impl MParser<Vec<O>> {
    base.separated_by(just(Token::Comma))
        .delimited_by(just(Token::OpenRound), just(Token::CloseRound))
}

fn fn_def(expression: impl MParser<Node>) -> impl MParser<Node> {
    let block = expression
        .then_ignore(just(Token::Semicolon))
        .repeated()
        .delimited_by(just(Token::OpenCurly), just(Token::CloseCurly));

    just(Token::Fn)
        .ignore_then(ident().or_not())
        .then(list_of(ident()))
        .then(block)
        .map(|((name, _args), body)| RawNode::FnDef(FnDef { name, body }))
        .map_with_span(Spanned)
}

fn parser() -> impl MParser<Vec<Node>> {
    let expression = recursive(|expression| {
        let call = atom()
            .then(list_of(expression.clone()))
            .map(|(callee, args)| {
                RawNode::Call(Call {
                    callee: Box::new(callee),
                    args,
                })
            })
            .map_with_span(Spanned);

        fn_def(expression).or(call).or(atom())
    });

    fn_def(expression).repeated().then_ignore(end())
}

pub fn parse(source: &str, interner: &mut Rodeo) -> Result<Vec<Node>, Vec<Simple<Token>>> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.collect(interner).unwrap();

    parse_tokens(tokens)
}

pub fn parse_tokens(tokens: Vec<(Token, Span)>) -> Result<Vec<Node>, Vec<Simple<Token>>> {
    let stream = Stream::from_iter(0..0, tokens.into_iter());
    parser().parse(stream)
}

#[cfg(test)]
mod test {
    use super::*;
    use lasso::Rodeo;

    #[test]
    fn simple() {
        let source = r#"fn main() { print("hello"); }"#;
        let mut interner = Rodeo::default();
        let ast = parse(source, &mut interner).unwrap();

        let key = |v| interner.get(v).unwrap();

        assert_eq!(
            ast,
            vec![Spanned(
                RawNode::FnDef(FnDef {
                    name: Some(key("main")),
                    body: vec![Spanned(
                        RawNode::Call(Call {
                            callee: Box::new(Spanned(RawNode::Name(key("print")), 12..17)),
                            args: vec![Spanned(RawNode::String(key("hello")), 18..25)]
                        }),
                        12..26
                    )]
                }),
                0..29
            )]
        );
    }
}
