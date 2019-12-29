#![allow(dead_code)]

use failure::Fail;

#[macro_use]
extern crate nom;

use nom::{
    branch::alt,
    bytes::complete::{is_a, is_not, tag, take_while1},
    character::{complete::line_ending, is_alphanumeric},
    combinator::{map, map_opt, opt},
    multi::{many0, many1},
    sequence::{delimited, preceded},
};

use std::ffi::OsStr;
use std::fmt;
use std::mem;
use std::result;
use std::str::from_utf8;

#[cfg(test)]
#[macro_use]
mod test_macros;

type IResult<'a, T> = nom::IResult<&'a [u8], T>;

#[cfg(unix)]
pub fn string(s: &[u8]) -> Option<&OsStr> {
    use std::os::unix::ffi::OsStrExt;
    Some(OsStr::from_bytes(s))
}

#[cfg(windows)]
pub fn string(s: &[u8]) -> Option<&OsStr> {
    // TODO: disallow non-whitespace control characters
    if s.iter().all(u8::is_ascii) {
        Some(from_utf8(s).unwrap().as_ref())
    } else {
        None
    }
}

pub fn nonempty_string(s: &[u8]) -> Option<&OsStr> {
    if !s.is_empty() {
        string(s)
    } else {
        None
    }
}

// TODO: get rid off this function and eliminate empty strings
fn empty_string() -> &'static OsStr {
    "".as_ref()
}

fn whitespace(input: &[u8]) -> IResult<()> {
    map(many0(alt((tag("$\r\n"), tag("$\n"), tag(" ")))), |_| ())(input)
}

named_args!(
    word<'a>(w: &str)<&'a [u8], ()>,
    map!(
        pair!(
            tag!(w),
            alt!(tag!(&b"$\r\n"[..]) | tag!(&b"$\n"[..]) | tag!(&b" "[..]))
        ),
        |_| ()
    )
);

macro_rules! ignore_whitespace (
    ($i:expr, $($args:tt)*) => (
        {
            use nom::Err;

            match sep!($i, whitespace, $($args)*) {
                Err(e) => Err(e),
                Ok((i1, o)) => {
                    match whitespace(i1) {
                        Err(e) => Err(Err::convert(e)),
                        Ok((i2, _))    => Ok((i2, o))
                    }
                }
            }
        }
    )
);

fn is_simple_identifier_character(c: u8) -> bool {
    is_alphanumeric(c) || c == b'_' || c == b'-'
}

fn is_identifier_character(c: u8) -> bool {
    is_simple_identifier_character(c) || c == b'.'
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Identifier<'a>(pub &'a str);

fn identifier(input: &[u8]) -> IResult<Identifier> {
    map(take_while1(is_identifier_character), |s| {
        Identifier(from_utf8(s).unwrap())
    })(input)
}

fn simple_identifier(input: &[u8]) -> IResult<Identifier> {
    map(take_while1(is_simple_identifier_character), |s| {
        Identifier(from_utf8(s).unwrap())
    })(input)
}

#[cfg(test)]
#[test]
fn test_identifier() {
    test_parse!(identifier(b"abc"), id!("abc"));
    test_parse!(identifier(b"ab.c"), id!("ab.c"));
    test_parse!(identifier(b"ab_c"), id!("ab_c"));
    test_parse!(identifier(b"ab-c"), id!("ab-c"));
    test_parse!(identifier(b"3-1"), id!("3-1"));
    test_parse!(identifier(b"3+1"), id!("3"), b"+1");
    test_parse!(identifier(b"0.1"), id!("0.1"));
    test_parse!(identifier(b"abc def"), id!("abc"), b" def");
    test_parse!(identifier(b"abc_def"), id!("abc_def"));
    test_parse_error!(identifier(b""));
}

#[derive(PartialEq, Debug, Eq, Clone)]
pub enum ValuePiece<'a> {
    Evaluated(&'a str),
    Plain(&'a OsStr),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Value<'a>(pub Vec<ValuePiece<'a>>);

impl<'a> IntoIterator for Value<'a> {
    type Item = ValuePiece<'a>;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> IntoIterator for &'a Value<'a> {
    type Item = &'a ValuePiece<'a>;
    type IntoIter = std::slice::Iter<'a, ValuePiece<'a>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

fn value(input: &[u8]) -> IResult<Value> {
    map(
        many0(alt((
            map(map_opt(is_not("$\r\n"), nonempty_string), ValuePiece::Plain),
            map(
                map_opt(preceded(tag("$"), is_a("$ |:")), string),
                ValuePiece::Plain,
            ),
            map(
                preceded(alt((tag("$\n"), tag("$\r\n"))), opt(is_a(" "))),
                |_| ValuePiece::Plain(empty_string()),
            ),
            map(
                delimited(tag("${"), identifier, tag("}")),
                |Identifier(x)| ValuePiece::Evaluated(x),
            ),
            map(preceded(tag("$"), simple_identifier), |Identifier(x)| {
                ValuePiece::Evaluated(x)
            }),
        ))),
        Value,
    )(input)
}

#[cfg(test)]
#[test]
fn test_value() {
    test_parse!(value(b""), value![]);
    test_parse!(value(b"abc"), value![plain!(b"abc")]);
    test_parse!(value(b"$$abc"), value![plain!(b"$"), plain!(b"abc")]);
    test_parse!(value(b"abc def"), value![plain!(b"abc def")]);
    test_parse!(
        value(b"abc$ def"),
        value![plain!(b"abc"), plain!(b" "), plain!(b"def"),]
    );
    test_parse!(
        value(b"abc$ def"),
        value![plain!(b"abc"), plain!(b" "), plain!(b"def"),]
    );
    test_parse!(
        value(b"abc$:def"),
        value![plain!(b"abc"), plain!(b":"), plain!(b"def"),]
    );
    test_parse!(
        value(b"abc$|def"),
        value![plain!(b"abc"), plain!(b"|"), plain!(b"def"),]
    );
    test_parse!(
        value(b"abc$\ndef"),
        value![plain!(b"abc"), plain!(b""), plain!(b"def"),]
    );
    test_parse!(
        value(b"abc$\n    def"),
        value![plain!(b"abc"), plain!(b""), plain!(b"def"),]
    );
    test_parse!(
        value(b"ab${cd}ef"),
        value![plain!(b"ab"), evaluated!("cd"), plain!(b"ef"),]
    );
    test_parse!(
        value(b"abc$def.ghi"),
        value![plain!(b"abc"), evaluated!("def"), plain!(b".ghi"),]
    );
    test_parse!(value(b"abc$/def"), value![plain!(b"abc")], &b"$/def");
    test_parse!(value(b"a | b"), value![plain!(b"a | b")]);
    test_parse!(value(b"a : b"), value![plain!(b"a : b")]);
    test_parse!(value(b"abc\n"), value![plain!(b"abc")], b"\n");
}

fn path(input: &[u8]) -> IResult<Value> {
    map(
        many1(alt((
            map(
                map_opt(is_not("$ :\r\n|\0"), nonempty_string),
                ValuePiece::Plain,
            ),
            map(
                map_opt(preceded(tag("$"), is_a("$ |:")), string),
                ValuePiece::Plain,
            ),
            map(
                preceded(alt((tag("$\n"), tag("$\r\n"))), opt(is_a(" "))),
                |_| ValuePiece::Plain(empty_string()),
            ),
            map(
                delimited(tag("${"), identifier, tag("}")),
                |Identifier(x)| ValuePiece::Evaluated(x),
            ),
            map(preceded(tag("$"), simple_identifier), |Identifier(x)| {
                ValuePiece::Evaluated(x)
            }),
        ))),
        Value,
    )(input)
}

// fn paths0(input: &[u8]) -> Vec<Value> {}

named!(paths0<&[u8], Vec<Value>>, ignore_whitespace!(many0!(path)));
named!(paths1<&[u8], Vec<Value>>, ignore_whitespace!(many1!(path)));

#[cfg(test)]
#[test]
fn test_path() {
    test_parse_error!(path(b""));
    test_parse!(path(b"foo.c"), value![plain!(b"foo.c")]);
    test_parse!(path(b"foo.c bar.c"), value![plain!(b"foo.c")], b" bar.c");
    test_parse!(path(b"foo.c\n"), value![plain!(b"foo.c")], b"\n");

    test_parse!(paths0(b""), vec![]);
    test_parse!(paths0(b"foo.c"), vec![value![plain!(b"foo.c")]]);
    test_parse!(
        paths0(b" foo.c bar.c "),
        vec![value![plain!(b"foo.c")], value![plain!(b"bar.c")]]
    );
    test_parse!(paths0(b"foo.c\n"), vec![value![plain!(b"foo.c")]], b"\n");
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Binding<'a> {
    pub name: Identifier<'a>,
    pub value: Value<'a>,
}

named!(
    binding<&[u8], Binding>,
    map!(
        terminated!(
            separated_pair!(identifier, ignore_whitespace!(tag!(&b"="[..])), value),
            line_ending
        ),
        |(name, value)| Binding { name, value }
    )
);

#[cfg(test)]
// #[test]
fn test_binding() {
    test_parse!(
        binding(b"abc=def\n"),
        Binding {
            name: id!("abc"),
            value: value![plain!(b"def")],
        }
    );
    test_parse_error!(binding(b"abc=def"));
    test_parse!(
        binding(b"abc = def\n"),
        Binding {
            name: id!("abc"),
            value: value![plain!(b"def")],
        }
    );
    test_parse_error!(binding(b" abc = def\n"));
    test_parse!(
        binding(b"abc = \n"),
        Binding {
            name: id!("abc"),
            value: value![],
        }
    );
    test_parse!(
        binding(b"abc = def ghi\n"),
        Binding {
            name: id!("abc"),
            value: value![plain!(b"def ghi")],
        }
    );
    test_parse!(
        binding(b"abc = def $ghi\n"),
        Binding {
            name: id!("abc"),
            value: value![plain!(b"def "), evaluated!("ghi")],
        }
    );
    test_parse!(
        binding(b"abc $\n = $\n    def\n"),
        Binding {
            name: id!("abc"),
            value: value![plain!(b"def")],
        }
    );
    // TODO: check against ninja
    test_parse!(
        binding(b"abc = def # comment\n"),
        Binding {
            name: id!("abc"),
            value: value![plain!(b"def # comment")],
        }
    );
}

named!(bindings<&[u8], Vec<Binding>>, many1!(preceded!(indent, binding)));

#[cfg(test)]
// #[test]
fn test_bindings() {
    test_parse_error!(bindings(b""));
    test_parse!(
        bindings(b" a = b\n"),
        vec![Binding {
            name: id!("a"),
            value: value![plain!(b"b")],
        }]
    );
    test_parse!(
        bindings(
            &br#"
  a = b
  c = d
  x = $y
"#[1..]
        ),
        vec![
            Binding {
                name: id!("a"),
                value: value![plain!(b"b")],
            },
            Binding {
                name: id!("c"),
                value: value![plain!(b"d")],
            },
            Binding {
                name: id!("x"),
                value: value![evaluated!("y")],
            },
        ]
    );
    test_parse!(
        bindings(
            &br#"
  a = b
     c = d
       x = $y
"#[1..]
        ),
        vec![
            Binding {
                name: id!("a"),
                value: value![plain!(b"b")],
            },
            Binding {
                name: id!("c"),
                value: value![plain!(b"d")],
            },
            Binding {
                name: id!("x"),
                value: value![evaluated!("y")],
            },
        ]
    );
    test_parse!(
        bindings(
            &br#"
       a = b
     c = d
   x = $y
"#[1..]
        ),
        vec![
            Binding {
                name: id!("a"),
                value: value![plain!(b"b")],
            },
            Binding {
                name: id!("c"),
                value: value![plain!(b"d")],
            },
            Binding {
                name: id!("x"),
                value: value![evaluated!("y")],
            },
        ]
    );
    test_parse_error!(bindings(b"a = b\n"));
}

#[derive(Debug)]
pub struct Rule<'a> {
    pub name: Identifier<'a>,
    pub bindings: Vec<Binding<'a>>,
}

named!(
    rule<&[u8], Rule>,
    map!(
        pair!(
            delimited!(call!(word, "rule"), ignore_whitespace!(identifier), line_ending),
            bindings
        ),
        |(name, bindings)| Rule { name, bindings }
    )
);

// TODO: add test_rule

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Build<'a> {
    pub outputs: Vec<Value<'a>>,
    pub implicit_outputs: Vec<Value<'a>>,
    pub rule: Identifier<'a>,
    pub inputs: Vec<Value<'a>>,
    pub implicit_inputs: Vec<Value<'a>>,
    pub order_only_inputs: Vec<Value<'a>>,
    pub bindings: Vec<Binding<'a>>,
}

#[cfg_attr(rustfmt, rustfmt_skip)]
named!(
    build<&[u8], Build>,
    do_parse!(
        outputs: preceded!(call!(word, "build"), paths1) >>
        implicit_outputs: opt!(preceded!(tag!(&b"|"[..]), paths1)) >>
        rule: preceded!(tag!(&b":"[..]), ignore_whitespace!(identifier)) >>
        inputs: paths0 >>
        implicit_inputs: opt!(preceded!(tag!(&b"|"[..]), paths1)) >>
        order_only_inputs: opt!(preceded!(tag!(&b"||"[..]), paths1)) >>
        _newline: line_ending >>
        bindings: opt!(bindings) >> (
            Build {
                outputs,
                implicit_outputs: implicit_outputs.unwrap_or_else(Vec::new),
                rule,
                inputs,
                implicit_inputs: implicit_inputs.unwrap_or_else(Vec::new),
                order_only_inputs: order_only_inputs.unwrap_or_else(Vec::new),
                bindings: bindings.unwrap_or_else(Vec::new),
            }
        )
    )
);

#[cfg(test)]
// #[test]
fn test_build() {
    test_parse!(
        build(b"build foo.o:cc foo.c\n"),
        Build {
            outputs: vec![value![plain!(b"foo.o")]],
            implicit_outputs: vec![],
            rule: id!("cc"),
            inputs: vec![value![plain!(b"foo.c")]],
            implicit_inputs: vec![],
            order_only_inputs: vec![],
            bindings: vec![],
        }
    );
    test_parse_error!(build(b"build : cc foo.c\n"));
    test_parse_error!(build(b"buildfoo.o : cc foo.c\n"));
    test_parse!(
        build(b"build foo.o : cc foo.c    \n"),
        Build {
            outputs: vec![value![plain!(b"foo.o")]],
            implicit_outputs: vec![],
            rule: id!("cc"),
            inputs: vec![value![plain!(b"foo.c")]],
            implicit_inputs: vec![],
            order_only_inputs: vec![],
            bindings: vec![],
        }
    );
    test_parse!(
        build(
            b"\
build foo.o | foo.o.d foo.s : cc foo.c | foo.h || bar.so
    restat = 1
    pool = expensive
"
        ),
        Build {
            outputs: vec![value![plain!(b"foo.o")]],
            implicit_outputs: vec![value![plain!(b"foo.o.d")], value![plain!(b"foo.s")]],
            rule: id!("cc"),
            inputs: vec![value![plain!(b"foo.c")]],
            implicit_inputs: vec![value![plain!(b"foo.h")]],
            order_only_inputs: vec![value![plain!(b"bar.so")]],
            bindings: vec![
                Binding {
                    name: id!("restat"),
                    value: value![plain!(b"1")],
                },
                Binding {
                    name: id!("pool"),
                    value: value![plain!(b"expensive")],
                },
            ],
        }
    );
    test_parse!(
        build(b"build nothing : nil\n"),
        Build {
            outputs: vec![value![plain!(b"nothing")]],
            implicit_outputs: vec![],
            rule: id!("nil"),
            inputs: vec![],
            implicit_inputs: vec![],
            order_only_inputs: vec![],
            bindings: vec![],
        }
    );
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Default<'a> {
    pub targets: Vec<Value<'a>>,
}

named!(
    default<&[u8], Default>,
    map!(
        terminated!(
            preceded!(call!(word, "default"), ignore_whitespace!(many1!(path))),
            line_ending
        ),
        |targets| Default { targets }
    )
);

#[cfg(test)]
// #[test]
fn test_default() {
    test_parse!(
        default(b"default all\n"),
        Default {
            targets: vec![value![plain!(b"all")]],
        }
    );
    test_parse_error!(default(b"default\n"));
    test_parse_error!(default(b"defaultall\n"));
    test_parse!(
        default(b"default all nothing $special with$ space\n"),
        Default {
            targets: vec![
                value![plain!(b"all")],
                value![plain!(b"nothing")],
                value![evaluated!("special")],
                value![plain!(b"with"), plain!(b" "), plain!(b"space")],
            ],
        }
    );
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Include<'a> {
    pub path: Value<'a>,
    pub new_scope: bool,
}

named!(
    include<&[u8], Include>,
    terminated!(
        alt!(
            preceded!(call!(word, "include"), ignore_whitespace!(path)) => {
                |path| Include { path, new_scope: false }
            } |
            preceded!(call!(word, "subninja"), ignore_whitespace!(path)) => {
                |path| Include { path, new_scope: true }
            }
        ),
        line_ending
    )
);

#[cfg(test)]
// #[test]
fn test_include() {
    test_parse!(
        include(b"include rules.ninja\n"),
        Include {
            path: value![plain!(b"rules.ninja")],
            new_scope: false,
        }
    );
    test_parse!(
        include(b"include $dir/rules.ninja\n"),
        Include {
            path: value![evaluated!("dir"), plain!(b"/rules.ninja"),],
            new_scope: false,
        }
    );
    test_parse!(
        include(b"subninja dir/build.ninja\n"),
        Include {
            path: value![plain!(b"dir/build.ninja"),],
            new_scope: true,
        }
    );
    test_parse_error!(include(b"include\n"));
    test_parse_error!(include(b"subninja\n"));
    test_parse_error!(include(b"includerules.ninja\n"));
    test_parse_error!(include(b"subninjarules.ninja\n"));
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Pool<'a> {
    pub name: Identifier<'a>,
    pub depth: Value<'a>,
}

named!(
    indent<&[u8], usize>,
    map!(is_a!(" "), |data| {
        data.len()
    })
);

#[cfg_attr(rustfmt, rustfmt_skip)]
named!(pool<&[u8], Pool>, do_parse!(
    name: delimited!(call!(word, "pool"), ignore_whitespace!(identifier), line_ending) >>
    _indent: indent >>
    // Technically ninja allows multiple depth values and just takes the last
    // one.
    depth: ignore_whitespace!(delimited!(
        pair!(
            tag!(&b"depth"[..]), tag!(&b"="[..])
        ),
        value,
        line_ending
    )) >>
    ( Pool{ name, depth } )
));

#[cfg(test)]
// #[test]
fn test_pool() {
    test_parse!(
        pool(b"pool link\n    depth = 3\n"),
        Pool {
            name: Identifier(&"link"[..]),
            depth: value![plain!(b"3")],
        }
    );
    test_parse_error!(pool(b"poollink\n    depth = 3\n"));
    test_parse_error!(pool(b"pool link\n    command = rm -rf /\n"));
    test_parse_error!(pool(b"pool link\n"));
    test_parse_error!(pool(b"pool link\n\n"));
    test_parse_error!(pool(b"pool link\ndepth = 3\n"));
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Comment<'a>(pub &'a [u8]);

named!(
    comment<&[u8], Comment>,
    map!(
        delimited!(tag!(&b"#"[..]), take_while!(|c| c != b'\n'), line_ending),
        Comment
    )
);

#[cfg(test)]
// #[test]
fn test_comment() {
    test_parse!(
        comment(&b"# this is a comment\n"[..]),
        Comment(&b" this is a comment"[..])
    );
    test_parse!(comment(&b"#\n"[..]), Comment(&b""[..]));
}

#[derive(Debug)]
pub enum Statement<'a> {
    Rule(Rule<'a>),
    Build(Build<'a>),
    Binding(Binding<'a>),
    Default(Default<'a>),
    Include(Include<'a>),
    Pool(Pool<'a>),
    Comment(Comment<'a>),
}

named!(
    statement<&[u8], Statement>,
    alt!(
        rule => { Statement::Rule } |
        build => { Statement::Build } |
        binding => { Statement::Binding } |
        default => { Statement::Default } |
        include => { Statement::Include } |
        pool => { Statement::Pool } |
        comment => { Statement::Comment }
    )
);

pub struct Statements<'a> {
    data: &'a [u8],
    original_data: &'a [u8],
}

pub fn parse(data: &[u8]) -> Statements {
    Statements {
        data: data,
        original_data: data,
    }
}

#[derive(Debug, Fail)]
pub struct Error {
    message: String,
}

impl Error {
    fn new<T>(_original_data: &[u8], _data: &[u8], _err: nom::Err<T>) -> Error {
        Error {
            message: "syntax error occurred".to_string(),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&self.message)
    }
}

impl<'a> Iterator for Statements<'a> {
    type Item = result::Result<Statement<'a>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.data.is_empty() {
                return None;
            }

            let result = alt!(self.data, statement => { Some } | line_ending => { |_| None } );

            match result {
                Ok((rest, statement)) => {
                    self.data = rest;

                    // Loop for empty lines.
                    if let Some(statement) = statement {
                        return Some(Ok(statement));
                    }
                }
                Err(e) => {
                    let data = mem::replace(&mut self.data, &b""[..]);
                    return Some(Err(Error::new(self.original_data, data, e)));
                }
            }
        }
    }
}
