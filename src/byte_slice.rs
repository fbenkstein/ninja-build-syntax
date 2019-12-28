use std::fmt;
use std::iter::{Enumerate, Map};
use std::ops::{Deref, Range, RangeFrom, RangeFull, RangeTo};
use std::slice::Iter;
use std::str::FromStr;

use nom::types::CompleteByteSlice;
use nom::{AsBytes, AtEof, Compare, CompareResult, Context, Err, ErrorKind, FindSubstring, FindToken, HexDisplay,
          IResult, InputIter, InputLength, InputTake, InputTakeAtPosition, Offset, ParseTo, Slice};

#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct ByteSlice<'a>(pub &'a [u8]);

impl<'a> fmt::Debug for ByteSlice<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        f.write_str("ByteSlice(")?;
        f.write_str("&b\"")?;
        for escaped in self.0.iter().cloned().map(::std::ascii::escape_default) {
            let escaped_bytes: Vec<u8> = escaped.collect();
            let escaped = ::std::str::from_utf8(&escaped_bytes).unwrap();

            if escaped == "\"" {
                f.write_str("\\\"")?
            } else {
                f.write_str(escaped)?
            }
        }
        f.write_str("\"[..])")
    }
}

impl<'a> Deref for ByteSlice<'a> {
    type Target = [u8];
    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'a> AtEof for ByteSlice<'a> {
    fn at_eof(&self) -> bool {
        true
    }
}

impl<'a> Slice<Range<usize>> for ByteSlice<'a> {
    fn slice(&self, range: Range<usize>) -> Self {
        ByteSlice(self.0.slice(range))
    }
}

impl<'a> Slice<RangeTo<usize>> for ByteSlice<'a> {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        ByteSlice(self.0.slice(range))
    }
}

impl<'a> Slice<RangeFrom<usize>> for ByteSlice<'a> {
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        ByteSlice(self.0.slice(range))
    }
}

impl<'a> Slice<RangeFull> for ByteSlice<'a> {
    fn slice(&self, range: RangeFull) -> Self {
        ByteSlice(self.0.slice(range))
    }
}

impl<'a> InputIter for ByteSlice<'a> {
    type Item = u8;
    type RawItem = u8;
    type Iter = Enumerate<Self::IterElem>;
    type IterElem = Map<Iter<'a, Self::Item>, fn(&u8) -> u8>;

    fn iter_indices(&self) -> Self::Iter {
        self.0.iter_indices()
    }
    fn iter_elements(&self) -> Self::IterElem {
        self.0.iter_elements()
    }
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::RawItem) -> bool,
    {
        self.0.position(predicate)
    }
    fn slice_index(&self, count: usize) -> Option<usize> {
        self.0.slice_index(count)
    }
}

impl<'a> InputTake for ByteSlice<'a> {
    fn take(&self, count: usize) -> Self {
        ByteSlice(self.0.take(count))
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (left, right) = self.0.take_split(count);
        (ByteSlice(left), ByteSlice(right))
    }
}

impl<'a> InputLength for ByteSlice<'a> {
    fn input_len(&self) -> usize {
        self.0.input_len()
    }
}

impl<'a, 'b> Compare<&'b [u8]> for ByteSlice<'a> {
    fn compare(&self, t: &'b [u8]) -> CompareResult {
        self.0.compare(t)
    }
    fn compare_no_case(&self, t: &'b [u8]) -> CompareResult {
        self.0.compare_no_case(t)
    }
}

impl<'a, 'b> Compare<&'b str> for ByteSlice<'a> {
    fn compare(&self, t: &'b str) -> CompareResult {
        self.0.compare(t)
    }
    fn compare_no_case(&self, t: &'b str) -> CompareResult {
        self.0.compare_no_case(t)
    }
}

impl<'a, 'b> FindSubstring<&'b [u8]> for ByteSlice<'a> {
    fn find_substring(&self, substr: &'b [u8]) -> Option<usize> {
        self.0.find_substring(substr)
    }
}

impl<'a, 'b> FindSubstring<&'b str> for ByteSlice<'a> {
    fn find_substring(&self, substr: &'b str) -> Option<usize> {
        self.0.find_substring(substr)
    }
}

impl<'a> FindToken<char> for ByteSlice<'a> {
    fn find_token(&self, token: char) -> bool {
        self.0.find_token(token)
    }
}

impl<'a> FindToken<u8> for ByteSlice<'a> {
    fn find_token(&self, token: u8) -> bool {
        self.0.find_token(token)
    }
}

impl<'a, 'b> FindToken<&'a u8> for ByteSlice<'b> {
    fn find_token(&self, token: &u8) -> bool {
        self.0.find_token(token)
    }
}

impl<'a, R: FromStr> ParseTo<R> for ByteSlice<'a> {
    fn parse_to(&self) -> Option<R> {
        self.0.parse_to()
    }
}

impl<'a> Offset for ByteSlice<'a> {
    fn offset(&self, second: &ByteSlice<'a>) -> usize {
        self.0.offset(second.0)
    }
}

impl<'a> AsBytes for ByteSlice<'a> {
    fn as_bytes(&self) -> &[u8] {
        self.0.as_bytes()
    }
}

impl<'a> HexDisplay for ByteSlice<'a> {
    fn to_hex(&self, chunk_size: usize) -> String {
        self.0.to_hex(chunk_size)
    }

    fn to_hex_from(&self, chunk_size: usize, from: usize) -> String {
        self.0.to_hex_from(chunk_size, from)
    }
}

pub trait GetBytes<'a>
where
    Self: 'a,
{
    fn get_bytes(self) -> &'a [u8];
}

impl<'a> GetBytes<'a> for &'a [u8] {
    fn get_bytes(self) -> &'a [u8] {
        self
    }
}

impl<'a> GetBytes<'a> for CompleteByteSlice<'a> {
    fn get_bytes(self) -> &'a [u8] {
        self.0
    }
}

fn adapt_context<'a, I: 'a + GetBytes<'a>, E>(context: Context<I, E>) -> Context<ByteSlice<'a>, E> {
    match context {
        Context::Code(input, error) => Context::Code(ByteSlice(input.get_bytes()), error),
    }
}

fn adapt_error<'a, I: 'a + GetBytes<'a>, E>(error: Err<I, E>) -> Err<ByteSlice<'a>, E> {
    match error {
        Err::Incomplete(needed) => Err::Incomplete(needed),
        Err::Error(context) => Err::Error(adapt_context(context)),
        Err::Failure(context) => Err::Failure(adapt_context(context)),
    }
}

pub fn adapt_result<'a, I: 'a + GetBytes<'a>, O, E>(result: IResult<I, O, E>) -> IResult<ByteSlice<'a>, O, E> {
    result.map(|(i, o)| (ByteSlice(i.get_bytes()), o)).map_err(adapt_error)
}

impl<'a> InputTakeAtPosition for ByteSlice<'a> {
    type Item = u8;

    fn split_at_position<P>(&self, predicate: P) -> IResult<Self, Self, u32>
    where
        P: Fn(Self::Item) -> bool,
    {
        adapt_result(CompleteByteSlice(self.0).split_at_position(predicate)).map(|(i, o)| (i, ByteSlice(o.0)))
    }

    fn split_at_position1<P>(&self, predicate: P, e: ErrorKind<u32>) -> IResult<Self, Self, u32>
    where
        P: Fn(Self::Item) -> bool,
    {
        adapt_result(CompleteByteSlice(self.0).split_at_position1(predicate, e)).map(|(i, o)| (i, ByteSlice(o.0)))
    }
}
