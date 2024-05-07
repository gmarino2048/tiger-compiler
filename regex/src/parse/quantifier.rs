use super::*;

#[derive(Debug, PartialEq, Eq)]
pub enum Quantifier {
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
        let components = Self::split_range_components(input);
        let range_elements = Self::collect_parse_errors(components)?;

        if range_elements.len() == 2 {
            Self::create_ranged_quantifier(range_elements[0], range_elements[1])
        }
        else if range_elements.len() == 1 {
            Self::create_ranged_quantifier(None, range_elements[0])
        }
        else {
            let range_field_count = range_elements.len();
            let msg = format!("Cannot create a ranged quantifier with {range_field_count} fields.");
            Err(Error::new(ErrorKind::InvalidQuantifier, msg.as_str()))
        }
    }

    fn split_range_components(input: &String) -> Vec<ParseResult<Option<usize>>> {
        input
            .split(',')
            .map(|s| s.replace(&['{', '}'], ""))
            .map(|s| String::from(s.trim()))
            .map(|s| Self::parse_integer_or_empty(s.as_str()))
            .collect::<Vec<_>>()
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

    fn collect_parse_errors(results: Vec<ParseResult<Option<usize>>>) -> ParseResult<Vec<Option<usize>>> {
        let mut range_elements = Vec::with_capacity(results.len());

        for parse_result in results {
            let component = parse_result?;
            range_elements.push(component);
        }

        Ok(range_elements)
    }

    fn create_ranged_quantifier(start: Option<usize>, end: Option<usize>) -> ParseResult<Quantifier> {
        match (start, end) {
            (Some(start_value), None) => Ok(Quantifier::AtLeast(start_value)),
            (None, Some(end_value)) => Ok(Quantifier::AtMost(end_value)),
            (Some(start_value), Some(end_value)) => Ok(Quantifier::Range(start_value, end_value)),
            _ => Err(Error::new(ErrorKind::InvalidQuantifier, "Cannot create ranged quantifier with two empty fields."))
        }
    }
}

#[cfg(test)]
mod tests {
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

        let result = Quantifier::parse("{43,13}").unwrap();
        assert_eq!(result, Quantifier::Range(43, 13));

        let result = Quantifier::parse("{14, 80}").unwrap();
        assert_eq!(result, Quantifier::Range(14, 80));

        let result = Quantifier::parse("{30,30}").unwrap();
        assert_eq!(result, Quantifier::Range(30, 30));
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

        let result = Quantifier::parse("{, 80 }").unwrap();
        assert_eq!(result, Quantifier::AtMost(80));
    }

    #[test]
    fn test_ranged_error_cases() {
        let result = Quantifier::parse("{}").unwrap_err();
        assert_eq!(result.kind(), &ErrorKind::InvalidQuantifier);

        let result = Quantifier::parse("{x}").unwrap_err();
        assert_eq!(result.kind(), &ErrorKind::InvalidSyntax);

        let result = Quantifier::parse("{,}").unwrap_err();
        assert_eq!(result.kind(), &ErrorKind::InvalidQuantifier);

        let result = Quantifier::parse("{13, y}").unwrap_err();
        assert_eq!(result.kind(), &ErrorKind::InvalidSyntax);

        let result = Quantifier::parse("{1, 2, 3}").unwrap_err();
        assert_eq!(result.kind(), &ErrorKind::InvalidQuantifier);

        let result = Quantifier::parse("a{}").unwrap_err();
        assert_eq!(result.kind(), &ErrorKind::InvalidSyntax);
    }

}