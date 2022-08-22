use crate::xml::reader::lexer::Token;
use crate::xml::reader::parser::{ParserOutcome, PullParser, State};

impl PullParser {
    pub fn inside_comment(&mut self, t: Token) -> Option<ParserOutcome> {
        match t {
            // Double dash is illegal inside a comment
            Token::Chunk(s) if s == "--" => {
                Some(self_error!(self; "Unexpected token inside a comment: --"))
            }

            Token::CommentEnd => {
                self.lexer.outside_comment();
                self.as_state_continue(State::OutsideTag)
            }

            _ => None, // Do not modify buffer if ignoring the comment
        }
    }
}
