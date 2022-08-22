use crate::xml::namespace;
use crate::xml::reader::lexer::Token;
use crate::xml::reader::parser::{
    ClosingTagSubstate, ParserOutcome, PullParser, QualifiedNameTarget, State,
};

impl PullParser {
    pub fn inside_closing_tag_name(
        &mut self,
        t: Token,
        s: ClosingTagSubstate,
    ) -> Option<ParserOutcome> {
        match s {
            ClosingTagSubstate::CTInsideName => self.read_qualified_name(t, QualifiedNameTarget::ClosingTag, |this, token, name| {
                match name.prefix_ref() {
                    Some(prefix) if prefix == namespace::NS_XML_PREFIX ||
                                    prefix == namespace::NS_XMLNS_PREFIX =>
                        // TODO: {:?} is bad, need something better
                        Some(self_error!(this; "'{:?}' cannot be an element name prefix", name.prefix)),
                    _ => {
                        this.data.element_name = Some(name.clone());
                        match token {
                            Token::Whitespace(_) => this.as_state_continue(State::InsideClosingTag(ClosingTagSubstate::CTAfterName)),
                            Token::TagEnd => this.emit_end_element(),
                            _ => Some(self_error!(this; "Unexpected token inside closing tag: {}", token))
                        }
                    }
                }
            }),
            ClosingTagSubstate::CTAfterName => match t {
                Token::Whitespace(_) => None,  //  Skip whitespace
                Token::TagEnd => self.emit_end_element(),
                _ => Some(self_error!(self; "Unexpected token inside closing tag: {}", t))
            }
        }
    }
}
