use crate::xml::reader::events::XmlEvent;
use crate::xml::reader::lexer::Token;

use crate::xml::reader::parser::{
    ClosingTagSubstate, OpeningTagSubstate, ParserOutcome, ProcessingInstructionSubstate,
    PullParser, State, DEFAULT_ENCODING, DEFAULT_STANDALONE, DEFAULT_VERSION,
};

impl PullParser {
    pub fn outside_tag(&mut self, t: Token) -> Option<ParserOutcome> {
        match t {
            Token::ReferenceStart => {
                self.as_state_continue(State::InsideReference(Box::new(State::OutsideTag)))
            }

            Token::Whitespace(_) if self.depth() == 0 => None, // skip whitespace outside of the root element

            Token::Whitespace(c) => {
                if !self.buf_has_data() {
                    self.push_pos();
                }
                self.append_char_continue(c)
            }

            _ if t.contains_char_data() && self.depth() == 0 => {
                Some(self_error!(self; "Unexpected characters outside the root element: {}", t))
            }

            _ if t.contains_char_data() => {
                // Non-whitespace char data
                if !self.buf_has_data() {
                    self.push_pos();
                }
                self.inside_whitespace = false;
                t.push_to_string(&mut self.buf);
                None
            }

            Token::ReferenceEnd => {
                // Semi-colon in a text outside an entity
                self.inside_whitespace = false;
                Token::ReferenceEnd.push_to_string(&mut self.buf);
                None
            }

            Token::CommentStart => {
                // We need to switch the lexer into a comment mode inside comments
                self.lexer.inside_comment();
                self.as_state_continue(State::InsideComment)
            }

            Token::CDataStart => {
                if !self.buf_has_data() {
                    self.push_pos();
                }
                // We need to disable lexing errors inside CDATA
                self.lexer.disable_errors();
                self.as_state_continue(State::InsideCData)
            }

            _ => {
                // Encountered some markup event, flush the buffer as characters
                // or a whitespace
                let mut next_event = if self.buf_has_data() {
                    let buf = self.take_buf();
                    Some(Ok(XmlEvent::Characters(buf)))
                } else {
                    None
                };
                self.inside_whitespace = true; // Reset inside_whitespace flag
                self.push_pos();
                match t {
                    Token::ProcessingInstructionStart => self.as_state(
                        State::InsideProcessingInstruction(
                            ProcessingInstructionSubstate::PIInsideName,
                        ),
                        next_event,
                    ),

                    Token::DoctypeStart if !self.encountered_element => {
                        // We don't have a doctype event so skip this position
                        // FIXME: update when we have a doctype event
                        self.next_pos();
                        self.lexer.disable_errors();
                        self.as_state(State::InsideDoctype, next_event)
                    }

                    Token::OpeningTagStart => {
                        // If declaration was not parsed and we have encountered an element,
                        // emit this declaration as the next event.
                        if !self.parsed_declaration {
                            self.parsed_declaration = true;
                            let sd_event = XmlEvent::StartDocument {
                                version: DEFAULT_VERSION,
                                encoding: DEFAULT_ENCODING.into(),
                                standalone: DEFAULT_STANDALONE,
                            };
                            // next_event is always none here because we're outside of
                            // the root element
                            next_event = Some(Ok(sd_event));
                            self.push_pos();
                        }
                        self.encountered_element = true;
                        self.nst.push_empty();
                        self.as_state(
                            State::InsideOpeningTag(OpeningTagSubstate::InsideName),
                            next_event,
                        )
                    }

                    Token::ClosingTagStart if self.depth() > 0 => self.as_state(
                        State::InsideClosingTag(ClosingTagSubstate::CTInsideName),
                        next_event,
                    ),

                    Token::CommentStart => {
                        // We need to switch the lexer into a comment mode inside comments
                        self.lexer.inside_comment();
                        self.as_state(State::InsideComment, next_event)
                    }

                    Token::CDataStart => {
                        // We need to disable lexing errors inside CDATA
                        self.lexer.disable_errors();
                        self.as_state(State::InsideCData, next_event)
                    }

                    _ => Some(self_error!(self; "Unexpected token: {}", t)),
                }
            }
        }
    }
}
