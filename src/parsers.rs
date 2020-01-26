use std::convert::identity;
use std::str::from_utf8;

use nom::{
    branch::alt,
    bytes::complete::{is_a, is_not, tag, take_while, take_while1},
    character::{complete::line_ending, is_alphanumeric},
    combinator::{map, map_opt, opt, verify},
    multi::{many0, many1},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
};

#[cfg(test)]
#[macro_use]
mod test_macros;

use crate::IResult;

fn maybe_whitespace(input: &[u8]) -> IResult<()> {
    map(many0(alt((tag("$\r\n"), tag("$\n"), tag(" ")))), |_| ())(input)
}

#[cfg(test)]
#[test]
fn test_maybe_whitespace() {
    test_parse!(maybe_whitespace(b""), ());
    test_parse!(maybe_whitespace(b"        "), ());
    test_parse!(maybe_whitespace(b"    $\n    "), ());
    test_parse!(maybe_whitespace(b"    $\r\n    "), ());
    test_parse!(maybe_whitespace(b"    \t    "), (), b"\t    ");
    test_parse!(maybe_whitespace(b"abc"), (), b"abc");
}

fn maybe_surrounded_by_whitespace<'a, O, F>(f: F) -> impl Fn(&'a [u8]) -> IResult<O>
where
    F: Fn(&'a [u8]) -> IResult<O>,
{
    delimited(maybe_whitespace, f, maybe_whitespace)
}

fn word<'a, Input: 'a>(w: &'a str) -> impl Fn(Input) -> nom::IResult<Input, ()> + 'a
where
    Input: nom::InputTake + nom::Compare<&'a str> + Clone,
{
    map(
        pair(tag(w), alt((tag("$\r\n"), tag("$\n"), tag(" ")))),
        |_| (),
    )
}

fn is_simple_identifier_character(c: u8) -> bool {
    is_alphanumeric(c) || c == b'_' || c == b'-'
}

fn is_identifier_character(c: u8) -> bool {
    is_simple_identifier_character(c) || c == b'.'
}

/// Identifier.
///
/// Used for variable names, rule names and pool names.
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

/// Piece of a value.
#[derive(PartialEq, Debug, Eq, Clone)]
pub enum ValuePiece<'a> {
    /// Reference to a variable.
    Reference(&'a str),
    /// Plain value.
    Plain(&'a [u8]),
}

/// An unevaluated value.
///
/// Used for variable values and paths. A `Value` can contain references to
/// variables in which case it's represented as multiple pieces, plain ones and
/// evaluated ones. To get the evaluated value of a `Value` the plain pieces and
/// variable references must be concatenated.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Value<'a>(Vec<ValuePiece<'a>>);

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
            map(verify(is_not("$\r\n"), |s: &[u8]| !s.is_empty()), |s| {
                Some(ValuePiece::Plain(s))
            }),
            map(preceded(tag("$"), is_a("$ |:")), |s| {
                Some(ValuePiece::Plain(s))
            }),
            map(
                preceded(alt((tag("$\n"), tag("$\r\n"))), opt(is_a(" "))),
                |_| None,
            ),
            map(
                delimited(tag("${"), identifier, tag("}")),
                |Identifier(x)| Some(ValuePiece::Reference(x)),
            ),
            map(preceded(tag("$"), simple_identifier), |Identifier(x)| {
                Some(ValuePiece::Reference(x))
            }),
        ))),
        |pieces| Value(pieces.into_iter().filter_map(identity).collect()),
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
    test_parse!(value(b"abc$\ndef"), value![plain!(b"abc"), plain!(b"def"),]);
    test_parse!(
        value(b"abc$\n    def"),
        value![plain!(b"abc"), plain!(b"def"),]
    );
    test_parse!(
        value(b"ab${cd}ef"),
        value![plain!(b"ab"), reference!("cd"), plain!(b"ef"),]
    );
    test_parse!(
        value(b"abc$def.ghi"),
        value![plain!(b"abc"), reference!("def"), plain!(b".ghi"),]
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
                verify(is_not("$ :\r\n|\0"), |s: &[u8]| !s.is_empty()),
                |s| Some(ValuePiece::Plain(s)),
            ),
            map(preceded(tag("$"), is_a("$ |:")), |s| {
                Some(ValuePiece::Plain(s))
            }),
            map(
                preceded(alt((tag("$\n"), tag("$\r\n"))), opt(is_a(" "))),
                |_| None,
            ),
            map(
                delimited(tag("${"), identifier, tag("}")),
                |Identifier(x)| Some(ValuePiece::Reference(x)),
            ),
            map(preceded(tag("$"), simple_identifier), |Identifier(x)| {
                Some(ValuePiece::Reference(x))
            }),
        ))),
        |pieces| Value(pieces.into_iter().filter_map(identity).collect()),
    )(input)
}

fn paths0(input: &[u8]) -> IResult<Vec<Value>> {
    terminated(many0(preceded(maybe_whitespace, path)), maybe_whitespace)(input)
}

fn paths1(input: &[u8]) -> IResult<Vec<Value>> {
    terminated(many1(preceded(maybe_whitespace, path)), maybe_whitespace)(input)
}

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

/// Variable assignment.
///
/// Example:
/// ```notrust
/// a = 1
/// ```
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Binding<'a> {
    /// Left-hand side of the assignment.
    pub name: Identifier<'a>,
    /// Right-hand side of the assignment.
    pub value: Value<'a>,
}

impl Binding<'_> {
    fn parse(input: &[u8]) -> IResult<Binding> {
        map(
            terminated(
                separated_pair(
                    identifier,
                    tuple((maybe_whitespace, tag("="), maybe_whitespace)),
                    value,
                ),
                line_ending,
            ),
            |(name, value)| Binding { name, value },
        )(input)
    }
}

#[cfg(test)]
#[test]
fn test_binding() {
    let binding = Binding::parse;
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
            value: value![plain!(b"def "), reference!("ghi")],
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

fn bindings(input: &[u8]) -> IResult<Vec<Binding>> {
    many1(preceded(indent, Binding::parse))(input)
}

#[cfg(test)]
#[test]
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
                value: value![reference!("y")],
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
                value: value![reference!("y")],
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
                value: value![reference!("y")],
            },
        ]
    );
    test_parse_error!(bindings(b"a = b\n"));
}

/// Rule definition.
///
/// This defines the command to execute to produce the outputs of a `build`
/// instruction.
#[derive(Debug)]
pub struct Rule<'a> {
    /// Rule name.
    pub name: Identifier<'a>,
    /// Rule variables.
    ///
    /// Only certain variable names are allowed here while some are required.
    /// Please refer to the ninja documentation.
    pub bindings: Vec<Binding<'a>>,
}

impl Rule<'_> {
    fn parse(input: &[u8]) -> IResult<Rule> {
        map(
            pair(
                delimited(
                    word("rule"),
                    maybe_surrounded_by_whitespace(identifier),
                    line_ending,
                ),
                bindings,
            ),
            |(name, bindings)| Rule { name, bindings },
        )(input)
    }
}

// TODO: add test_rule

/// Build instruction.
///
/// The build instructions describe commands that have to be executed in order
/// to produce the outputs. They are called "edges" in ninja jargon while the
/// inputs and outputs are called "nodes".
///
/// Example:
/// ```notrust
/// build foo.o : c foo.c || generated_header.h
///   cflags = -DHAVE_BAR=1
/// ```
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Build<'a> {
    /// Outputs.
    ///
    /// This is what the build instruction produces. There can be many outputs
    /// but having at least one is mandatory. These are populated into the
    /// implicit `$out` variable by ninja.
    pub outputs: Vec<Value<'a>>,
    /// Implicit outputs.
    ///
    /// Also produced by the build instruction. The difference to the `outputs`
    /// is that they are optional and are not populated into the `$out` variable
    /// by ninja.
    pub implicit_outputs: Vec<Value<'a>>,
    /// Rule name.
    ///
    /// The name of the rule that is used to generate the command to execute to
    /// generate an output.
    pub rule: Identifier<'a>,
    /// Inputs.
    ///
    /// These are used to produce the outputs. During rebuilding when any of
    /// these are considered out-of-date the outputs will have to rebuild. They
    /// are populated into the `$in` variable by ninja.
    pub inputs: Vec<Value<'a>>,
    /// Implicit inputs.
    ///
    /// Similar to inputs but not populated into the `$in` variable by ninja.
    pub implicit_inputs: Vec<Value<'a>>,
    /// Order-only inputs.
    ///
    /// These are inputs that have to exist for this build instruction to run.
    /// They are not checked for being up-to-date when rebuilding.
    pub order_only_inputs: Vec<Value<'a>>,
    /// Variable assignments.
    ///
    /// These can be used to customize the commands set forth by the rule.
    pub bindings: Vec<Binding<'a>>,
}

fn opt_default<I: Clone, O: std::default::Default, E: nom::error::ParseError<I>, F>(
    f: F,
) -> impl Fn(I) -> nom::IResult<I, O, E>
where
    F: Fn(I) -> nom::IResult<I, O, E>,
{
    map(opt(f), Option::unwrap_or_default)
}

impl Build<'_> {
    fn parse(input: &[u8]) -> IResult<Build> {
        map(
            tuple((
                preceded(word("build"), paths1),
                opt_default(preceded(tag("|"), paths1)),
                preceded(tag(":"), maybe_surrounded_by_whitespace(identifier)),
                paths0,
                opt_default(preceded(tag("|"), paths1)),
                opt_default(preceded(tag("||"), paths1)),
                line_ending,
                opt_default(bindings),
            )),
            |(
                outputs,
                implicit_outputs,
                rule,
                inputs,
                implicit_inputs,
                order_only_inputs,
                _newline,
                bindings,
            )| {
                Build {
                    outputs,
                    implicit_outputs,
                    rule,
                    inputs,
                    implicit_inputs,
                    order_only_inputs,
                    bindings,
                }
            },
        )(input)
    }
}

#[cfg(test)]
#[test]
fn test_build() {
    let build = Build::parse;
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

/// Default instruction.
///
/// Defines the targets to build when no arguments are given to the `ninja`
/// command.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Default<'a> {
    /// Default targets.
    pub targets: Vec<Value<'a>>,
}

impl<'a> Default<'a> {
    fn parse(input: &[u8]) -> IResult<Default> {
        map(
            terminated(
                preceded(word("default"), maybe_surrounded_by_whitespace(paths1)),
                line_ending,
            ),
            |targets| Default { targets },
        )(input)
    }
}

#[cfg(test)]
#[test]
fn test_default() {
    let default = Default::parse;
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
                value![reference!("special")],
                value![plain!(b"with"), plain!(b" "), plain!(b"space")],
            ],
        }
    );
}

/// Include instruction.
///
/// Instructs ninja to include another file. This comes in two flavors `include`
/// and `subninja`. `include` instructs ninja to include a file as-is while
/// `subninja` instructs ninja to open another "scope" such that rule
/// definitions and variable bindings are not propagated up to the file that
/// contains the `subninja` instruction.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Include<'a> {
    pub path: Value<'a>,
    pub new_scope: bool,
}

impl<'a> Include<'a> {
    fn parse(input: &[u8]) -> IResult<Include> {
        terminated(
            alt((
                map(
                    preceded(word("include"), maybe_surrounded_by_whitespace(path)),
                    |path| Include {
                        path,
                        new_scope: false,
                    },
                ),
                map(
                    preceded(word("subninja"), maybe_surrounded_by_whitespace(path)),
                    |path| Include {
                        path,
                        new_scope: true,
                    },
                ),
            )),
            line_ending,
        )(input)
    }
}

#[cfg(test)]
#[test]
fn test_include() {
    let include = Include::parse;
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
            path: value![reference!("dir"), plain!(b"/rules.ninja"),],
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

/// Pool definition.
///
/// A pool definition is used to limit parallel execution of build commands.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Pool<'a> {
    /// Pool name.
    pub name: Identifier<'a>,
    /// Pool depth.
    pub depth: Value<'a>,
}

fn indent(input: &[u8]) -> IResult<usize> {
    map(is_a(" "), <[u8]>::len)(input)
}

impl<'a> Pool<'a> {
    fn parse(input: &[u8]) -> IResult<Pool> {
        map(
            separated_pair(
                delimited(
                    word("pool"),
                    maybe_surrounded_by_whitespace(identifier),
                    line_ending,
                ),
                indent,
                // Technically ninja allows multiple depth values and just takes the
                // last one.
                map_opt(Binding::parse, |Binding { name, value }| {
                    if name == Identifier("depth") {
                        Some(value)
                    } else {
                        None
                    }
                }),
            ),
            |(name, value)| Pool { name, depth: value },
        )(input)
    }
}

#[cfg(test)]
#[test]
fn test_pool() {
    let pool = Pool::parse;
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

/// Comment.
///
/// A single comment line, that is everything after a `#`.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Comment<'a>(pub &'a [u8]);

impl<'a> Comment<'a> {
    fn parse(input: &[u8]) -> IResult<Comment> {
        map(
            delimited(tag("#"), take_while(|c| c != b'\n'), line_ending),
            Comment,
        )(input)
    }
}

#[cfg(test)]
#[test]
fn test_comment() {
    let comment = Comment::parse;
    test_parse!(
        comment(&b"# this is a comment\n"[..]),
        Comment(&b" this is a comment"[..])
    );
    test_parse!(comment(&b"#\n"[..]), Comment(&b""[..]));
}

/// Statement.
///
/// Sum type of any of the statements that are allowed in a ninja file. Note
/// that this doesn't include the empty statement consisting of a single empty
/// line.
#[derive(Debug)]
pub enum Statement<'a> {
    /// Rule definition.
    Rule(Rule<'a>),
    /// Build instruction.
    Build(Build<'a>),
    /// Variable definition.
    Binding(Binding<'a>),
    /// Default instruction.
    Default(Default<'a>),
    /// Include instruction.
    Include(Include<'a>),
    /// Pool definition.
    Pool(Pool<'a>),
    /// Comment.
    Comment(Comment<'a>),
}

impl Statement<'_> {
    pub(crate) fn parse(input: &[u8]) -> IResult<Statement> {
        alt((
            map(Rule::parse, Statement::Rule),
            map(Build::parse, Statement::Build),
            map(Binding::parse, Statement::Binding),
            map(Default::parse, Statement::Default),
            map(Include::parse, Statement::Include),
            map(Pool::parse, Statement::Pool),
            map(Comment::parse, Statement::Comment),
        ))(input)
    }
}
