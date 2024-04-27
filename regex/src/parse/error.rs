
#[derive(Debug, PartialEq, Eq)]
pub enum ErrorKind {
    InvalidSyntax,
    InvalidQuantifier
}

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    message: String
}

impl Error {
    pub fn new(code: ErrorKind, message: &str) -> Self {
        Error {
            kind: code,
            message: String::from(message)
        }
    }

    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    pub fn message(&self) -> &String {
        &self.message
    }
}

pub type ParseResult<T> = Result<T, Error>;
