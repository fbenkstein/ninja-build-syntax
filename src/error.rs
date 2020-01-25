/// An error that happened during parsing of a ninja file.
///
/// Since ninja files are usually generated this should only happen if there is
/// a bug in the library or the program that generated the file.
#[derive(Debug)]
pub struct Error(());

impl Error {
    pub(crate) fn new() -> Self {
        Self(())
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "syntax error")
    }
}

impl std::error::Error for Error {}
