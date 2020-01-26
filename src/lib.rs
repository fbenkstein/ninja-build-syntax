mod error;
mod parsers;
mod statements;

pub use {
    error::Error,
    parsers::{
        Binding, Build, Comment, Default, Identifier, Include, Pool, Rule, Statement, Value,
        ValuePiece,
    },
    statements::parse,
};

type IResult<'a, T> = nom::IResult<&'a [u8], T>;
