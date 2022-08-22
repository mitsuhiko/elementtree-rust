use crate::xml::reader::lexer::Token;
use crate::xml::reader::parser::{ParserOutcome, PullParser, State};

impl PullParser {
    pub fn inside_cdata(&mut self, t: Token) -> Option<ParserOutcome> {
        match t {
            Token::CDataEnd => {
                self.lexer.enable_errors();
                self.as_state(State::OutsideTag, None)
            }

            Token::Whitespace(_) => {
                t.push_to_string(&mut self.buf);
                None
            }

            _ => {
                self.inside_whitespace = false;
                t.push_to_string(&mut self.buf);
                None
            }
        }
    }
}
