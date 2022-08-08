use std::ops::Range;

use chumsky::{prelude::*, Parser, Stream};
use lasso::Spur;

use crate::lexer::{Lexer, Token};

pub type Span = Range<usize>;

/// Parser alias
trait MParser<T, E: chumsky::Error<Token> = Simple<Token>>:
    chumsky::Parser<Token, T, Error = E> + Clone
{
}

impl<P, T, E: chumsky::Error<Token>> MParser<T, E> for P where
    P: chumsky::Parser<Token, T, Error = E> + Clone
{
}

#[derive(Debug, PartialEq)]
pub struct Block {
    pub stmts: Vec<Node>,
}

#[derive(Debug, PartialEq)]
pub enum TypeProto {
    Name(Spur),
    Function {
        arguments: Vec<TypeProto>,
        return_ty: Box<TypeProto>,
    },
    Unknown,
}

#[derive(Debug, PartialEq)]
pub struct Argument {
    pub name: Spur,
    pub ty: TypeProto,
}

#[derive(Debug, PartialEq)]
pub struct FnDef {
    pub name: Option<Spur>,
    pub args: Vec<Argument>,
    pub return_ty: TypeProto,
    pub body: Spanned<Block>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum EffectKind {
    Function,
    Control,
}

#[derive(Debug, PartialEq)]
pub struct EffectDef {
    pub name: Spur,
    pub kind: EffectKind,
    pub args: Vec<Argument>,
    pub return_ty: TypeProto,
}

#[derive(Debug, PartialEq)]
pub struct EffectGroup {
    pub name: Spur,
    pub effects: Vec<EffectDef>,
}

#[derive(Debug, PartialEq)]
pub struct Call {
    pub callee: Box<Node>,
    pub args: Vec<Node>,
}

#[derive(Debug, PartialEq)]
pub struct If {
    pub cond: Box<Node>,
    pub if_true: Spanned<Block>,
    pub if_false: Option<Spanned<Block>>,
}

#[derive(Debug, PartialEq)]
pub struct While {
    pub cond: Box<Node>,
    pub body: Spanned<Block>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinopKind {
    Equals,
    Less,
    Greater,
    Add,
}

#[derive(Debug, PartialEq)]
pub struct Binop {
    pub kind: BinopKind,
    pub left: Box<Node>,
    pub right: Box<Node>,
}

#[derive(Debug, PartialEq)]
pub struct Let {
    pub name: Spanned<Spur>,
    pub value: Box<Node>,
}

#[derive(Debug, PartialEq)]
pub struct EffectHandler {
    pub name: Spur,
    pub args: Vec<Argument>,
    pub return_ty: TypeProto,
    pub body: Spanned<Block>,
}

#[derive(Debug, PartialEq)]
pub struct Handle {
    pub name: Spur,
    pub effects: Vec<EffectHandler>,
    pub expr: Spanned<Block>,
}

#[derive(Debug, PartialEq)]
pub enum RawNode {
    Effect(EffectGroup),
    Handle(Handle),
    FnDef(FnDef),
    Call(Call),
    Name(Spur),
    String(Spur),
    Number(i64),
    If(If),
    While(While),
    Binop(Binop),
    Let(Let),
    Assign(Let),
    Return(Option<Box<Node>>),
}

#[derive(Debug, PartialEq)]
pub struct Spanned<T>(pub T, pub Span);

pub type Node = Spanned<RawNode>;

impl From<Option<TypeProto>> for TypeProto {
    fn from(opt: Option<TypeProto>) -> Self {
        match opt {
            Some(x) => x,
            None => TypeProto::Unknown,
        }
    }
}

fn ident() -> impl MParser<Spur> {
    select! { Token::Identifier(x) => x }
}

fn parenthesized<T>(p: impl MParser<T>) -> impl MParser<T> {
    p.delimited_by(just(Token::OpenRound), just(Token::CloseRound))
}

fn atom(expression: impl MParser<Node>) -> impl MParser<Node> {
    let literal = select! {
        Token::String(x) => RawNode::String(x),
        Token::Identifier(x) => RawNode::Name(x),
        Token::Number(x) => RawNode::Number(x),
    };

    literal.map_with_span(Spanned).or(parenthesized(expression))
}

fn list_of<O>(base: impl MParser<O>) -> impl MParser<Vec<O>> {
    base.separated_by(just(Token::Comma))
        .delimited_by(just(Token::OpenRound), just(Token::CloseRound))
}

fn block_undelimited(
    block_expression: impl MParser<Node>,
    expression: impl MParser<Node>,
) -> impl MParser<Spanned<Block>> {
    let let_stmt = just(Token::Let)
        .ignore_then(ident().map_with_span(Spanned))
        .then_ignore(just(Token::Assign))
        .then(expression.clone().map(Box::new))
        .then_ignore(just(Token::Semicolon))
        .map(|(name, value)| RawNode::Let(Let { name, value }))
        .map_with_span(Spanned);

    let stmt = choice((
        let_stmt,
        block_expression,
        expression.then_ignore(just(Token::Semicolon)),
    ));

    stmt.repeated()
        .map(|stmts| Block { stmts })
        .map_with_span(Spanned)
}

fn block(
    block_expression: impl MParser<Node>,
    expression: impl MParser<Node>,
) -> impl MParser<Spanned<Block>> {
    block_undelimited(block_expression, expression)
        .map(|x| x.0)
        .delimited_by(just(Token::OpenCurly), just(Token::CloseCurly))
        .map_with_span(Spanned)
}

fn effect_group() -> impl MParser<Node> {
    let ty = ty();

    let argument = ident()
        .then(just(Token::Colon).ignore_then(ty.clone()).or_not())
        .map(|(name, ty)| Argument {
            name,
            ty: ty.into(),
        });

    let kind = select! {
        Token::Fn => EffectKind::Function,
        Token::Ctl => EffectKind::Control,
    };

    let effect = kind
        .then(ident())
        .then(list_of(argument))
        .then(just(Token::RightArrow).ignore_then(ty).or_not())
        .then_ignore(just(Token::Semicolon))
        .map(|(((kind, name), args), return_ty)| EffectDef {
            name,
            kind,
            args,
            return_ty: return_ty.into(),
        });

    let group = effect
        .repeated()
        .delimited_by(just(Token::OpenCurly), just(Token::CloseCurly));

    just(Token::Effect)
        .ignore_then(ident())
        .then(group)
        .map(|(name, effects)| RawNode::Effect(EffectGroup { name, effects }))
        .map_with_span(Spanned)
}

fn handle_def(
    block_expression: impl MParser<Node>,
    expression: impl MParser<Node>,
) -> impl MParser<Node> {
    let ty = ty();

    let argument = ident()
        .then(just(Token::Colon).ignore_then(ty.clone()).or_not())
        .map(|(name, ty)| Argument {
            name,
            ty: ty.into(),
        });

    let effect = ident()
        .then(list_of(argument))
        .then(just(Token::RightArrow).ignore_then(ty).or_not())
        .then(block(block_expression.clone(), expression.clone()))
        .map(|(((name, args), return_ty), body)| EffectHandler {
            name,
            args,
            return_ty: return_ty.into(),
            body,
        });

    let group = effect
        .clone()
        .repeated()
        .delimited_by(just(Token::OpenCurly), just(Token::CloseCurly))
        .or(effect.map(|x| vec![x]));

    let expr = block_undelimited(block_expression, expression);

    just(Token::Handle)
        .ignore_then(ident())
        .then(group)
        .then(expr)
        .map(|((name, effects), expr)| {
            RawNode::Handle(Handle {
                name,
                effects,
                expr: expr.into(),
            })
        })
        .map_with_span(Spanned)
}

fn ty() -> impl MParser<TypeProto> {
    let ty = recursive(|ty| {
        let func = list_of(ty.clone())
            .then_ignore(just(Token::RightArrow))
            .then(ty.clone().map(Box::new))
            .map(|(arguments, return_ty)| TypeProto::Function {
                arguments,
                return_ty,
            });
        let name = ident().map(TypeProto::Name);
        choice((func, name))
    });
    ty
}

fn fn_def(
    block_expression: impl MParser<Node>,
    expression: impl MParser<Node>,
) -> impl MParser<Node> {
    let ty = ty();

    let argument = ident()
        .then(just(Token::Colon).ignore_then(ty.clone()).or_not())
        .map(|(name, ty)| Argument {
            name,
            ty: ty.into(),
        });

    just(Token::Fn)
        .ignore_then(ident().or_not())
        .then(list_of(argument))
        .then(just(Token::RightArrow).ignore_then(ty).or_not())
        .then(block(block_expression, expression))
        .map(|(((name, args), return_ty), body)| {
            RawNode::FnDef(FnDef {
                name,
                args,
                return_ty: return_ty.into(),
                body,
            })
        })
        .map_with_span(Spanned)
}

fn if_clause(
    block_expression: impl MParser<Node>,
    expression: impl MParser<Node>,
) -> impl MParser<Node> {
    let block = block(block_expression, expression.clone());
    just(Token::If)
        .ignore_then(parenthesized(expression.clone()))
        .map(Box::new)
        .then(block.clone())
        .then(just(Token::Else).ignore_then(block).or_not())
        .map(|((cond, if_true), if_false)| {
            RawNode::If(If {
                cond,
                if_true,
                if_false,
            })
        })
        .map_with_span(Spanned)
}

fn while_clause(
    block_expression: impl MParser<Node>,
    expression: impl MParser<Node>,
) -> impl MParser<Node> {
    let block = block(block_expression, expression.clone());
    just(Token::While)
        .ignore_then(parenthesized(expression.clone()))
        .map(Box::new)
        .then(block.clone())
        .map(|(cond, body)| RawNode::While(While { cond, body }))
        .map_with_span(Spanned)
}

fn parser() -> impl MParser<Vec<Node>> {
    let mut block_expression = Recursive::declare();

    let expression = recursive(|expression| {
        let call = atom(expression.clone())
            .then(list_of(expression.clone()))
            .map(|(callee, args)| {
                RawNode::Call(Call {
                    callee: Box::new(callee),
                    args,
                })
            })
            .map_with_span(Spanned);

        let binopkind = select! {
            Token::Equals => BinopKind::Equals,
            Token::LessThan => BinopKind::Less,
            Token::GreaterThan => BinopKind::Greater,
            Token::Plus => BinopKind::Add,
        };

        let binop = atom(expression.clone())
            .map(Box::new)
            .then(binopkind)
            .then(atom(expression.clone()).map(Box::new))
            .map(|((left, kind), right)| RawNode::Binop(Binop { kind, left, right }))
            .map_with_span(Spanned);

        let assign = ident()
            .map_with_span(Spanned)
            .then_ignore(just(Token::Assign))
            .then(expression.clone().map(Box::new))
            .map(|(name, value)| RawNode::Assign(Let { name, value }))
            .map_with_span(Spanned);

        let return_expr = just(Token::Return)
            .ignore_then(expression.clone().map(Box::new).or_not())
            .map(RawNode::Return)
            .map_with_span(Spanned);

        choice((
            assign,
            binop,
            call,
            fn_def(block_expression.clone(), expression.clone()),
            if_clause(block_expression.clone(), expression.clone()),
            return_expr,
            atom(expression),
        ))
    });

    block_expression.define(choice((
        if_clause(block_expression.clone(), expression.clone()),
        while_clause(block_expression.clone(), expression.clone()),
        handle_def(block_expression.clone(), expression.clone()),
    )));

    let item = choice((fn_def(block_expression, expression), effect_group()));

    item.repeated().then_ignore(end())
}

pub fn parse(source: &str) -> Result<Vec<Node>, Vec<Simple<Token>>> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.collect().unwrap();

    parse_tokens(tokens)
}

pub fn parse_tokens(tokens: Vec<(Token, Span)>) -> Result<Vec<Node>, Vec<Simple<Token>>> {
    let stream = Stream::from_iter(0..0, tokens.into_iter());
    parser().parse(stream)
}

#[allow(unused_macros)]
macro_rules! node {
    (box $($t:tt)+) => {
        Box::new(node!($($t)+))
    };
    ($a:literal .. $b:literal, $($t:tt)+) => {
        Spanned(node!($($t)+), $a .. $b)
    };
    (let $name:ident ($s:expr) = $value:expr) => {
        RawNode::Let(Let {
            name: Spanned(crate::intern::INTERNER.get_or_intern(stringify!($name)), $s),
            value: Box::new($value),
        })
    };
    (if $cond:expr, $true:expr) => {
        RawNode::If(If {
            cond: Box::new($cond),
            if_true: $true,
            if_false: None,
        })
    };
    (binop $kind:ident, $left:expr, $right:expr) => {
        RawNode::Binop(Binop {
            kind: BinopKind::$kind,
            left: Box::new($left),
            right: Box::new($right),
        })
    };
    (call $callee:expr, $($arg:expr),*) => {
        RawNode::Call(Call {
            callee: Box::new($callee),
            args: vec![$($arg),*],
        })
    };
    (fn $name:ident ($args:expr) $body:expr) => {
        RawNode::FnDef(FnDef {
            name: Some(crate::intern::INTERNER.get_or_intern(stringify!($name))),
            args: $args,
            return_ty: TypeProto::Unknown,
            body: $body
        })
    };
    (fn $name:ident () $body:expr) => {
        RawNode::FnDef(FnDef {
            name: Some(crate::intern::INTERNER.get_or_intern(stringify!($name))),
            args: vec![],
            return_ty: TypeProto::Unknown,
            body: $body
        })
    };
    (block, $($stmt:expr),*) => {
        Block {
            stmts: vec![$($stmt),+]
        }
    };
    (number $v:expr) => {
        RawNode::Number($v)
    };
    (name $v:expr) => {
        RawNode::Name(crate::intern::INTERNER.get_or_intern($v))
    };
    (string $v:expr) => {
        RawNode::String(crate::intern::INTERNER.get_or_intern($v))
    };
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;
    use unindent::unindent;

    #[test]
    fn simple() {
        let source = r#"fn main() { print("hello"); }"#;
        let ast = parse(source).unwrap();

        let main = node!(0..29, fn main()
            node!(10..29, block,
                node!(12..26, call
                    node!(12..17, name "print"),
                    node!(18..25, string "hello")
                )
            )
        );

        assert_eq!(ast, vec![main]);
    }

    #[test]
    fn condition() {
        let source = unindent(
            r#"
            fn main() {
                let a = 1;
                if (a == 1) {
                    print("hello");
                }
            }
        "#,
        );
        let ast = parse(&source).unwrap();

        let assign = node!(16..26,
            let a (20..21) = node!(24..25, number 1));

        let if_clause = node!(31..74, if
            node!(35..41, binop Equals,
                node!(35..36, name "a"),
                node!(40..41, number 1)),
            node!(43..74, block,
                node!(53..67, call
                    node!(53..58, name "print"),
                    node!(59..66, string "hello"))
            )
        );

        let main = node!(0..76, fn main()
            node!(10..76, block, assign, if_clause));

        assert_eq!(ast, vec![main]);
    }
}
