use crate::xml::reader::events::XmlEvent;
use crate::xml::reader::lexer::Token;
use crate::xml::reader::parser::{ParserOutcome, PullParser, State};

impl PullParser {
    pub fn inside_comment(&mut self, t: Token) -> Option<ParserOutcome> {
        match t {
            // Double dash is illegal inside a comment
            Token::Chunk(s) if s == "--" => {
                Some(self_error!(self; "Unexpected token inside a comment: --"))
            }

            Token::CommentEnd if self.config.ignore_comments => {
                self.lexer.outside_comment();
                self.as_state_continue(State::OutsideTag)
            }

            Token::CommentEnd => {
                self.lexer.outside_comment();
                let data = self.take_buf();
                self.as_state_emit(State::OutsideTag, Ok(XmlEvent::Comment(data)))
            }

            _ if self.config.ignore_comments => None, // Do not modify buffer if ignoring the comment

            _ => {
                t.push_to_string(&mut self.buf);
                None
            }
        }
    }
}
