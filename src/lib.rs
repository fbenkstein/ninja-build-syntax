mod error;
mod parsers;
mod statements;

pub use {
    error::Error,
    parsers::{Binding, Build, Comment, Default, Include, Pool, Rule, Statement},
    statements::parse,
};

type IResult<'a, T> = nom::IResult<&'a [u8], T>;
