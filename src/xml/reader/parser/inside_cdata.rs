use crate::xml::reader::events::XmlEvent;
use crate::xml::reader::lexer::Token;
use crate::xml::reader::parser::{ParserOutcome, PullParser, State};

impl PullParser {
    pub fn inside_cdata(&mut self, t: Token) -> Option<ParserOutcome> {
        match t {
            Token::CDataEnd => {
                self.lexer.enable_errors();
                let event = if self.config.cdata_to_characters {
                    None
                } else {
                    let data = self.take_buf();
                    Some(Ok(XmlEvent::CData(data)))
                };
                self.as_state(State::OutsideTag, event)
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
