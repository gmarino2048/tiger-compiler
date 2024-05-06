
use super::*;

#[derive(Debug, Clone)]
pub enum RangeMatch {
    Single(char),
    Range(char, char)
}

#[derive(Debug)]
pub struct Range {
    matches: Vec<RangeMatch>
}

impl Range {
    pub fn new() -> Self {
        Range {
            matches: Vec::new()
        }
    }

    pub fn matches(&self) -> &Vec<RangeMatch> {
        &self.matches
    }

    pub fn add(&mut self, matcher: RangeMatch) {
        self.matches.push(matcher)
    }
}

impl Range {
    pub fn parse(input: &str) -> ParseResult<Range> {
        let mut input_characters = input.chars();

        todo!()
    }

}
