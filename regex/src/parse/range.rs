
use super::{Error, ErrorKind, ParseResult};

#[derive(Debug, Clone)]
pub enum RangeMatch {
    Single(char),
    Range(char, char)
}

#[derive(Debug, Clone, Default)]
pub struct Range {
    matches: Vec<RangeMatch>
}

impl Range {
    pub fn add_range(&mut self, start: char, end: char) {
        self.matches.push(
            RangeMatch::Range(start, end)
        )
    }

    pub fn add_single(&mut self, character: char) {
        self.matches.push(
            RangeMatch::Single(character)
        )
    }

    pub fn parse(input: &String) -> ParseResult<Self> {
        let input = Self::remove_enclosing_brackets(input);

        input
            .chars()
            .try_fold(RangeParseInfo::default(), RangeParseInfo::parse_iterable)?
            .finish_parse()
    }

    fn remove_enclosing_brackets(input: &String) -> String {
        let (mut start_idx, mut end_idx) = (0 as usize, input.len());

        if input.starts_with('[') {
            start_idx += 1;
        }

        if input.ends_with(']') {
            end_idx -= 1;
        }

        String::from(&input[start_idx..end_idx])
    }
}

#[derive(Debug, Clone, Default)]
struct RangeParseInfo {
    strict_literal: bool,
    range_context: bool,
    previous_character: Option<char>,
    current_range: Range
}

impl RangeParseInfo {

    const CHARACTER_CLOSE: char = ']';
    const CHARACTER_ESCAPE: char = '\\';
    const CHARACTER_NEWLINE: char = 'n';
    const CHARACTER_OPEN: char = '[';
    const CHARACTER_RANGE: char = '-';
    const CHARACTER_TAB: char = 't';

    fn parse_iterable(info: Self, current_character: char) -> ParseResult<Self> {
        if info.strict_literal {
            Self::parse_literal_context(current_character, info)
        }
        else {
            Self::parse_standard_context(current_character, info)
        }
    }

    fn parse_literal_context(current_character: char, mut info: Self) -> ParseResult<Self> {
        let parsed_character: char;

        match current_character {
            // TODO: Fill this in
            _ => parsed_character = current_character
        }

        info.strict_literal = false;
        Self::parse_character(parsed_character, info)
    }

    fn parse_standard_context(current_character: char, mut info: Self) -> ParseResult<Self> {
        match current_character {
            Self::CHARACTER_ESCAPE => {
                info.strict_literal = true;
                Ok(info)
            },
            Self::CHARACTER_RANGE => {
                info.range_context = true;
                Ok(info)
            },
            Self::CHARACTER_CLOSE => {
                Err(Error::new(ErrorKind::InvalidSyntax, "Reached end of match range while parsing"))
            }
            _ => Self::parse_character(current_character, info)
        }
    }

    fn parse_character(current_character: char, mut info: Self) -> ParseResult<Self> {
        if info.range_context {
            info.range_context = false;
            Self::add_range_to_group(current_character, info)
        }
        else {
            Self::add_single_to_group(current_character, info)
        }
    }

    fn add_range_to_group(current_character: char, mut info: Self) -> ParseResult<Self> {
        todo!()
    }

    fn add_single_to_group(current_character: char, mut info: Self) -> ParseResult<Self> {
        todo!()
    }

    fn finish_parse(self) -> ParseResult<Range> {
        todo!()
    }
}

#[cfg(test)]
mod test {
    use super::Range;

    #[test]
    fn test_bracket_removal() {
        let expected_result = String::from("abc");
        let test_cases = ["abc", "abc]", "[abc", "[abc]"];

        for case in test_cases {
            let string_value = String::from(case);
            let removed_brackets = Range::remove_enclosing_brackets(&string_value);

            assert_eq!(removed_brackets, expected_result);
        }
    }

    #[test]
    fn test_intermediate_bracket_removal() {
        let intermediate_test = String::from("[[]");
        let expected_result = String::from("[");

        assert_eq!(Range::remove_enclosing_brackets(&intermediate_test), expected_result);
    }

    #[test]
    fn test_bracket_removal_edge_cases() {
        let expected_result = String::from("");
        let test_cases = ["[", "]", "", "[]"];

        for case in test_cases {
            let string_value = String::from(case);
            let removed_brackets = Range::remove_enclosing_brackets(&string_value);

            assert_eq!(removed_brackets, expected_result);
        }
    }
}
