use nom::{branch::alt, character::complete::line_ending, combinator::map};

use crate::{
    error::Error,
    parsers::{statement, Statement},
};

pub struct Statements<'a> {
    data: &'a [u8],
}

pub fn parse(data: &[u8]) -> Statements {
    Statements { data: data }
}

impl<'a> Iterator for Statements<'a> {
    type Item = Result<Statement<'a>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.data.is_empty() {
                return None;
            }

            let result = alt((map(statement, Some), map(line_ending, |_| None)))(self.data);

            match result {
                Ok((rest, statement)) => {
                    self.data = rest;

                    // Loop for empty lines.
                    if let Some(statement) = statement {
                        return Some(Ok(statement));
                    }
                }
                Err(_) => {
                    self.data = &b""[..];
                    return Some(Err(Error::new()));
                }
            }
        }
    }
}
