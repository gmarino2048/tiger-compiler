
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

    const CHARACTER_CARRIAGE_RETURN: char = 'r';
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
            Self::CHARACTER_CARRIAGE_RETURN => parsed_character = '\r',
            Self::CHARACTER_NEWLINE => parsed_character = '\n',
            Self::CHARACTER_TAB => parsed_character = '\t',
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
            Self::CHARACTER_OPEN => {
                Err(Error::new(ErrorKind::InvalidSyntax, "Reached beginning of new match range while parsing"))
            },
            Self::CHARACTER_CLOSE => {
                Err(Error::new(ErrorKind::InvalidSyntax, "Reached end of match range while parsing"))
            }
            _ => Self::parse_character(current_character, info)
        }
    }

    fn parse_character(current_character: char, info: Self) -> ParseResult<Self> {
        if info.range_context {
            Self::process_ranged_match(current_character, info)
        }
        else {
            Self::add_single_to_group(current_character, info)
        }
    }

    fn process_ranged_match(current_character: char, mut info: Self) -> ParseResult<Self> {
        match info.previous_character {
            Some(last_character) => {
                info.range_context = false;
                info.previous_character = None;
                Self::check_range_and_add(last_character, current_character, info)
            }
            None => {
                let errmsg = format!("Malformed range statement: could not complete range ending in '{current_character}'.");
                Err(Error::new(ErrorKind::InvalidSyntax, &errmsg))
            }
        }
    }

    fn add_single_to_group(current_character: char, mut info: Self) -> ParseResult<Self> {
        if let Some(last_character) = info.previous_character {
            info.current_range.add_single(last_character);
        }

        info.previous_character = Some(current_character);
        Ok(info)
    }

    fn check_range_and_add(start: char, end: char, mut info: Self) -> ParseResult<Self> {
        if start > end {
            let errmsg = format!("Could not form match range: '{start}' is preceeded by '{end}'");
            Err(Error::new(ErrorKind::InvalidMatchRange, &errmsg))
        }
        else {
            info.current_range.add_range(start, end);
            Ok(info)
        }
    }

    fn finish_parse(self) -> ParseResult<Range> {
        match self.previous_character {
            Some(remaining_character) => {
                Self::add_single_to_group(remaining_character, self)
                    .map(|rpi| rpi.current_range)
            }
            None => Ok(self.current_range)
        }
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
