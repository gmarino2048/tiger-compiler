
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

#[derive(Debug, PartialEq, Eq)]
enum Quantifier {
    Single,
    AtLeast(usize),
    AtMost(usize),
    Range(usize, usize)
}

impl Quantifier {
    pub fn parse(input: &str) -> ParseResult<Quantifier> {
        let mut characters = input.chars();
        let first_character = characters.next();

        match first_character {
            Some('{') => Self::parse_quantifier_range(&characters.collect()),
            _ => Self::parse_single_character(&first_character)
        }
    }

    fn parse_single_character(input: &Option<char>) -> ParseResult<Quantifier> {
        match input {
            None => Ok(Quantifier::Single),
            Some('?') => Ok(Quantifier::AtMost(1)),
            Some('*') => Ok(Quantifier::AtLeast(0)),
            Some('+') => Ok(Quantifier::AtLeast(1)),
            Some(other) => {
                let msg = format!("Could not determine quantifier type from supplied character '{other}'");
                Err(Error::new(ErrorKind::InvalidSyntax, msg.as_str()))
            }
        }
    }

    fn parse_quantifier_range(input: &String) -> ParseResult<Quantifier> {
        let components = input
            .split(',')
            .map(|s| s.replace(&['{', '}'], ""))
            .map(|s| String::from(s.trim()))
            .map(|s| Self::parse_integer_or_empty(s.as_str()))
            .collect::<Vec<_>>();

        let mut range = Vec::with_capacity(components.len());
        for component in components {
            let count = component?;
            range.push(count);
        }

        if range.len() == 2 {
            match (&range[0], &range[1]) {
                (Some(start), None) => Ok(Quantifier::AtLeast(*start)),
                (None, Some(finish)) => Ok(Quantifier::AtMost(*finish)),
                (Some(start), Some(finish)) => Ok(Quantifier::Range(*start, *finish)),
                _ => Err(Error::new(ErrorKind::InvalidQuantifier, "Cannot create ranged quantifier with two empty fields."))
            }
        }
        else if range.len() == 1 {
            match &range[0] {
                Some(value) => Ok(Quantifier::AtLeast(*value)),
                None => Err(Error::new(ErrorKind::InvalidQuantifier, "Cannot create a ranged quantifier with an empty field."))
            }
        }
        else {
            let range_field_count = range.len();
            let msg = format!("Cannot create a ranged quantifier with {range_field_count} fields.");
            Err(Error::new(ErrorKind::InvalidQuantifier, msg.as_str()))
        }
    }

    fn parse_integer_or_empty(input: &str) -> ParseResult<Option<usize>> {
        if input.is_empty() {
            Ok(None)
        }
        else {
            match input.parse::<usize>() {
                Ok(parse_result) => Ok(Some(parse_result)),
                Err(error) => Err(Error::new(ErrorKind::InvalidSyntax, error.to_string().as_str()))
            }
        }
    }
}

#[cfg(test)]
mod quantifier_tests {
    use crate::parse::ErrorKind;

    use super::Quantifier;

    #[test]
    fn test_single_char_quantifier() {
        let result = Quantifier::parse("").unwrap();
        assert_eq!(result, Quantifier::Single);

        let result = Quantifier::parse("?").unwrap();
        assert_eq!(result, Quantifier::AtMost(1));

        let result = Quantifier::parse("*").unwrap();
        assert_eq!(result, Quantifier::AtLeast(0));

        let result = Quantifier::parse("+").unwrap();
        assert_eq!(result, Quantifier::AtLeast(1));
    }

    #[test]
    fn test_invalid_character() {
        let result = Quantifier::parse("x").unwrap_err();
        assert_eq!(result.kind(), &ErrorKind::InvalidSyntax);

        let result = Quantifier::parse("43").unwrap_err();
        assert_eq!(result.kind(), &ErrorKind::InvalidSyntax);

        let result = Quantifier::parse("hello").unwrap_err();
        assert_eq!(result.kind(), &ErrorKind::InvalidSyntax);
    }

    #[test]
    fn test_ranged() {
        let result = Quantifier::parse("{13,43}").unwrap();
        assert_eq!(result, Quantifier::Range(13, 43));
    }

    #[test]
    fn test_ranged_single() {
        let result = Quantifier::parse("{13}").unwrap();
        assert_eq!(result, Quantifier::AtLeast(13));

        let result = Quantifier::parse("{13,}").unwrap();
        assert_eq!(result, Quantifier::AtLeast(13));

        let result = Quantifier::parse("{ 13 , }").unwrap();
        assert_eq!(result, Quantifier::AtLeast(13));

        let result = Quantifier::parse("{ 13\t, }").unwrap();
        assert_eq!(result, Quantifier::AtLeast(13));

        let result = Quantifier::parse("{,13}").unwrap();
        assert_eq!(result, Quantifier::AtMost(13));

        let result = Quantifier::parse("{, 13 }").unwrap();
        assert_eq!(result, Quantifier::AtMost(13));
    }

}

