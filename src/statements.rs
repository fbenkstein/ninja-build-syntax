use nom::{branch::alt, character::complete::line_ending, combinator::map};

use crate::{
    error::Error,
    parsers::{statement, Statement},
};

struct Statements<'a> {
    data: &'a [u8],
}

pub fn parse(data: &[u8]) -> impl Iterator<Item = Result<Statement, Error>> {
    Statements { data }.filter_map(|item| item.transpose())
}

impl<'a> Iterator for Statements<'a> {
    type Item = Result<Option<Statement<'a>>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.data.is_empty() {
            return None;
        };

        match alt((map(statement, Some), map(line_ending, |_| None)))(self.data) {
            Ok((rest, item)) => {
                self.data = rest;
                Some(Ok(item))
            }
            Err(_) => {
                self.data = &b""[..];
                Some(Err(Error::new()))
            }
        }
    }
}
