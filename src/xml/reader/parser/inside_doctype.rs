use crate::xml::reader::lexer::Token;
use crate::xml::reader::parser::{ParserOutcome, PullParser, State};

impl PullParser {
    pub fn inside_doctype(&mut self, t: Token) -> Option<ParserOutcome> {
        match t {
            Token::TagEnd => {
                self.lexer.enable_errors();
                self.as_state_continue(State::OutsideTag)
            }

            _ => None,
        }
    }
}
