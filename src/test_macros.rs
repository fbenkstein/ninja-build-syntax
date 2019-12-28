use std::result;

use nom;

use byte_slice::ByteSlice;

type Error<'a> = nom::Err<ByteSlice<'a>>;
type Result<'a, T> = result::Result<(ByteSlice<'a>, T), Error<'a>>;

pub fn test_parse_impl<T: PartialEq<T> + ::std::fmt::Debug>(
    expected_result: T,
    expected_rest: ByteSlice,
    parse_input: ByteSlice,
    actual_iresult: Result<T>,
    parse_function_name: &str,
    filename: &str,
    line: u32,
) {
    match actual_iresult {
        Ok((ref actual_rest, ref actual_result))
            if *actual_rest == expected_rest && *actual_result == expected_result => {}
        Ok((actual_rest, actual_result)) => {
            panic!(
                r#"
{}:{}: Parsing failed:
    input:         {:?}
    function:      {}
    expected:      {:?}
    expected rest: {:?}
    actual:        {:?}
    actual_rest:   {:?}
"#,
                filename,
                line,
                parse_input,
                parse_function_name,
                expected_result,
                expected_rest,
                actual_result,
                actual_rest,
            );
        }
        Err(error) => {
            panic!(
                r#"
{}:{}: Parsing failed:
    input:         {:?}
    function:      {}
    error:         {:?}
"#,
                filename, line, parse_input, parse_function_name, error,
            );
        }
    }
}

macro_rules! test_parse {
    ($parse_function:ident($parse_input:expr), $expected_result:expr) => {
        test_parse!($parse_function($parse_input), $expected_result, b"")
    };
    ($parse_function:ident($parse_input:expr), $expected_result:expr, $rest:expr) => {
        $crate::test_macros::test_parse_impl(
            $expected_result,
            ByteSlice(&$rest[..]),
            ByteSlice(&$parse_input[..]),
            $parse_function(ByteSlice(&$parse_input[..])),
            stringify!($parse_function),
            file!(),
            line!(),
        );
    };
}
macro_rules! test_parse_error {
    ($parse_function:ident($parse_input:expr)) => {
        match $parse_function(ByteSlice(&$parse_input[..])) {
            Err(nom::Err::Error(_)) | Err(nom::Err::Failure(_)) => (),
            x => panic!(
                "expected {}({:?}) to return Error but got {:?}",
                stringify!($parse_function),
                ByteSlice(&$parse_input[..]),
                x
            ),
        }
    };
}

macro_rules! value {
    ( $( $item:expr ),* ) => {
        Value(vec![ $( $item ),* ])
    };
    ( $( $item:expr ),* , ) => {
        value![ $( $item ),* ]
    };
}

macro_rules! plain {
    ($value:expr) => {
        $crate::ValuePiece::Plain($crate::string(ByteSlice(&$value[..])).unwrap())
    };
}

macro_rules! evaluated {
    ($value:expr) => {
        $crate::ValuePiece::Evaluated(&$value[..])
    };
}

macro_rules! id {
    ($s:expr) => {
        Identifier(&$s[..])
    };
}
