mod error;
mod parsers;
mod statements;

pub use {
    error::Error,
    parsers::{Binding, Build, Comment, Default, Include, Pool, Rule},
    statements::{parse, Statements},
};

type IResult<'a, T> = nom::IResult<&'a [u8], T>;
