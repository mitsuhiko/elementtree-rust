//! Contains an implementation of pull-based XML parser.

use std::borrow::Cow;
use std::io::prelude::*;
use std::mem;

use crate::xml::attribute::OwnedAttribute;
use crate::xml::common::{
    self, is_name_char, is_name_start_char, Position, TextPosition, XmlVersion,
};
use crate::xml::name::OwnedName;
use crate::xml::namespace::NamespaceStack;
use crate::xml::reader::config::ParserConfig;
use crate::xml::reader::events::XmlEvent;
use crate::xml::reader::lexer::{Lexer, Token};

macro_rules! gen_takes(
    ($($field:ident -> $method:ident, $t:ty);+) => (
        $(
        impl MarkupData {
            #[inline]
            fn $method(&mut self) -> $t {
                mem::take(&mut self.$field)
            }
        }
        )+
    )
);

gen_takes!(
    name         -> take_name, String;
    ref_data     -> take_ref_data, String;

    version      -> take_version, Option<common::XmlVersion>;
    encoding     -> take_encoding, Option<String>;
    standalone   -> take_standalone, Option<bool>;

    element_name -> take_element_name, Option<OwnedName>;

    attr_name    -> take_attr_name, Option<OwnedName>;
    attributes   -> take_attributes, Vec<OwnedAttribute>
);

macro_rules! self_error(
    ($this:ident; $msg:expr) => ($this.error($msg));
    ($this:ident; $fmt:expr, $($arg:expr),+) => ($this.error(format!($fmt, $($arg),+)))
);

mod inside_cdata;
mod inside_closing_tag_name;
mod inside_comment;
mod inside_declaration;
mod inside_doctype;
mod inside_opening_tag;
mod inside_processing_instruction;
mod inside_reference;
mod outside_tag;

static DEFAULT_VERSION: XmlVersion = XmlVersion::Version10;
static DEFAULT_ENCODING: &str = "UTF-8";
static DEFAULT_STANDALONE: Option<bool> = None;

type ElementStack = Vec<OwnedName>;
pub type ParserOutcome = super::Outcome<XmlEvent>;

/// Pull-based XML parser.
pub struct PullParser {
    config: ParserConfig,
    lexer: Lexer,
    st: State,
    buf: String,
    nst: NamespaceStack,

    data: MarkupData,
    final_result: Option<ParserOutcome>,
    next_event: Option<ParserOutcome>,
    est: ElementStack,
    pos: Vec<TextPosition>,

    encountered_element: bool,
    parsed_declaration: bool,
    inside_whitespace: bool,
    read_prefix_separator: bool,
    pop_namespace: bool,
}

impl PullParser {
    /// Returns a new parser using the given config.
    pub fn new(config: ParserConfig) -> PullParser {
        PullParser {
            config,
            lexer: Lexer::new(),
            st: State::OutsideTag,
            buf: String::new(),
            nst: NamespaceStack::default(),

            data: MarkupData {
                name: String::new(),
                version: None,
                encoding: None,
                standalone: None,
                ref_data: String::new(),
                element_name: None,
                quote: None,
                attr_name: None,
                attributes: Vec::new(),
            },
            final_result: None,
            next_event: None,
            est: Vec::new(),
            pos: vec![TextPosition::new()],

            encountered_element: false,
            parsed_declaration: false,
            inside_whitespace: true,
            read_prefix_separator: false,
            pop_namespace: false,
        }
    }
}

impl Position for PullParser {
    /// Returns the position of the last event produced by the parser
    #[inline]
    fn position(&self) -> TextPosition {
        self.pos[0]
    }
}

#[derive(Clone, PartialEq)]
pub enum State {
    OutsideTag,
    InsideOpeningTag(OpeningTagSubstate),
    InsideClosingTag(ClosingTagSubstate),
    InsideProcessingInstruction(ProcessingInstructionSubstate),
    InsideComment,
    InsideCData,
    InsideDeclaration(DeclarationSubstate),
    InsideDoctype,
    InsideReference(Box<State>),
}

#[derive(Clone, Eq, PartialEq)]
pub enum OpeningTagSubstate {
    InsideName,

    InsideTag,

    InsideAttributeName,
    AfterAttributeName,

    InsideAttributeValue,
}

#[derive(Clone, Eq, PartialEq)]
pub enum ClosingTagSubstate {
    CTInsideName,
    CTAfterName,
}

#[derive(Clone, Eq, PartialEq)]
pub enum ProcessingInstructionSubstate {
    PIInsideName,
    PIInsideData,
}

#[derive(Clone, Eq, PartialEq)]
pub enum DeclarationSubstate {
    BeforeVersion,
    InsideVersion,
    AfterVersion,

    InsideVersionValue,
    AfterVersionValue,

    InsideEncoding,
    AfterEncoding,

    InsideEncodingValue,

    BeforeStandaloneDecl,
    InsideStandaloneDecl,
    AfterStandaloneDecl,

    InsideStandaloneDeclValue,
    AfterStandaloneDeclValue,
}

#[derive(PartialEq)]
enum QualifiedNameTarget {
    Attribute,
    OpeningTag,
    ClosingTag,
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum QuoteToken {
    SingleQuoteToken,
    DoubleQuoteToken,
}

impl QuoteToken {
    fn from_token(t: &Token) -> QuoteToken {
        match *t {
            Token::SingleQuote => QuoteToken::SingleQuoteToken,
            Token::DoubleQuote => QuoteToken::DoubleQuoteToken,
            _ => panic!("Unexpected token: {}", t),
        }
    }

    fn as_token(self) -> Token {
        match self {
            QuoteToken::SingleQuoteToken => Token::SingleQuote,
            QuoteToken::DoubleQuoteToken => Token::DoubleQuote,
        }
    }
}

struct MarkupData {
    name: String,     // used for processing instruction name
    ref_data: String, // used for reference content

    version: Option<common::XmlVersion>, // used for XML declaration version
    encoding: Option<String>,            // used for XML declaration encoding
    standalone: Option<bool>,            // used for XML declaration standalone parameter

    element_name: Option<OwnedName>, // used for element name

    quote: Option<QuoteToken>, // used to hold opening quote for attribute value
    attr_name: Option<OwnedName>, // used to hold attribute name
    attributes: Vec<OwnedAttribute>, // used to hold all accumulated attributes
}

impl PullParser {
    /// Returns next event read from the given buffer.
    ///
    /// This method should be always called with the same buffer. If you call it
    /// providing different buffers each time, the result will be undefined.
    pub fn next<R: Read>(&mut self, r: &mut R) -> ParserOutcome {
        if let Some(ref ev) = self.final_result {
            return ev.clone();
        }

        if let Some(ev) = self.next_event.take() {
            return ev;
        }

        if self.pop_namespace {
            self.pop_namespace = false;
            self.nst.pop();
        }

        loop {
            // While lexer gives us Ok(maybe_token) -- we loop.
            // Upon having a complete XML-event -- we return from the whole function.
            match self.lexer.next_token(r) {
                Ok(maybe_token) => match maybe_token {
                    None => break,
                    Some(token) => match self.dispatch_token(token) {
                        None => {} // continue
                        Some(Ok(XmlEvent::EndDocument)) => {
                            return {
                                self.next_pos();
                                self.set_final_result(Ok(XmlEvent::EndDocument))
                            }
                        }
                        Some(Ok(xml_event)) => {
                            return {
                                self.next_pos();
                                Ok(xml_event)
                            }
                        }
                        Some(Err(xml_error)) => {
                            return {
                                self.next_pos();
                                self.set_final_result(Err(xml_error))
                            }
                        }
                    },
                },
                Err(lexer_error) => return self.set_final_result(Err(lexer_error)),
            }
        }

        // Handle end of stream
        // Forward pos to the lexer head
        self.next_pos();
        let ev = if self.depth() == 0 {
            if self.encountered_element && self.st == State::OutsideTag {
                // all is ok
                Ok(XmlEvent::EndDocument)
            } else if !self.encountered_element {
                self_error!(self; "Unexpected end of stream: no root element found")
            } else {
                // self.st != State::OutsideTag
                self_error!(self; "Unexpected end of stream") // TODO: add expected hint?
            }
        } else {
            self_error!(self; "Unexpected end of stream: still inside the root element")
        };
        self.set_final_result(ev)
    }

    // This function is to be called when a terminal event is reached.
    // The function sets up the `self.final_result` into `Some(result)` and return `result`.
    fn set_final_result(&mut self, result: ParserOutcome) -> ParserOutcome {
        self.final_result = Some(result.clone());
        result
    }

    #[inline]
    fn error<M: Into<Cow<'static, str>>>(&self, msg: M) -> ParserOutcome {
        Err((&self.lexer, msg).into())
    }

    #[inline]
    fn next_pos(&mut self) {
        if self.pos.len() > 1 {
            self.pos.remove(0);
        } else {
            self.pos[0] = self.lexer.position();
        }
    }

    #[inline]
    fn push_pos(&mut self) {
        self.pos.push(self.lexer.position());
    }

    fn dispatch_token(&mut self, t: Token) -> Option<ParserOutcome> {
        match self.st.clone() {
            State::OutsideTag => self.outside_tag(t),
            State::InsideProcessingInstruction(s) => self.inside_processing_instruction(t, s),
            State::InsideDeclaration(s) => self.inside_declaration(t, s),
            State::InsideDoctype => self.inside_doctype(t),
            State::InsideOpeningTag(s) => self.inside_opening_tag(t, s),
            State::InsideClosingTag(s) => self.inside_closing_tag_name(t, s),
            State::InsideComment => self.inside_comment(t),
            State::InsideCData => self.inside_cdata(t),
            State::InsideReference(s) => self.inside_reference(t, *s),
        }
    }

    #[inline]
    fn depth(&self) -> usize {
        self.est.len()
    }

    #[inline]
    fn buf_has_data(&self) -> bool {
        !self.buf.is_empty()
    }

    #[inline]
    fn take_buf(&mut self) -> String {
        std::mem::take(&mut self.buf)
    }

    #[inline]
    fn append_char_continue(&mut self, c: char) -> Option<ParserOutcome> {
        self.buf.push(c);
        None
    }

    #[inline]
    fn as_state(&mut self, st: State, ev: Option<ParserOutcome>) -> Option<ParserOutcome> {
        self.st = st;
        ev
    }

    #[inline]
    fn as_state_continue(&mut self, st: State) -> Option<ParserOutcome> {
        self.as_state(st, None)
    }

    #[inline]
    fn as_state_emit(&mut self, st: State, ev: ParserOutcome) -> Option<ParserOutcome> {
        self.as_state(st, Some(ev))
    }

    /// Dispatches tokens in order to process qualified name. If qualified name cannot be parsed,
    /// an error is returned.
    ///
    /// # Parameters
    /// * `t`       --- next token;
    /// * `on_name` --- a callback which is executed when whitespace is encountered.
    fn read_qualified_name<F>(
        &mut self,
        t: Token,
        target: QualifiedNameTarget,
        on_name: F,
    ) -> Option<ParserOutcome>
    where
        F: Fn(&mut PullParser, Token, OwnedName) -> Option<ParserOutcome>,
    {
        // We can get here for the first time only when self.data.name contains zero or one character,
        // but first character cannot be a colon anyway
        if self.buf.len() <= 1 {
            self.read_prefix_separator = false;
        }

        let invoke_callback = |this: &mut PullParser, t| {
            let name = this.take_buf();
            match name.parse() {
                Ok(name) => on_name(this, t, name),
                Err(_) => Some(self_error!(this; "Qualified name is invalid: {}", name)),
            }
        };

        match t {
            // There can be only one colon, and not as the first character
            Token::Character(':') if self.buf_has_data() && !self.read_prefix_separator => {
                self.buf.push(':');
                self.read_prefix_separator = true;
                None
            }

            Token::Character(c)
                if c != ':'
                    && (!self.buf_has_data() && is_name_start_char(c)
                        || self.buf_has_data() && is_name_char(c)) =>
            {
                self.append_char_continue(c)
            }

            Token::EqualsSign if target == QualifiedNameTarget::Attribute => {
                invoke_callback(self, t)
            }

            Token::EmptyTagEnd if target == QualifiedNameTarget::OpeningTag => {
                invoke_callback(self, t)
            }

            Token::TagEnd
                if target == QualifiedNameTarget::OpeningTag
                    || target == QualifiedNameTarget::ClosingTag =>
            {
                invoke_callback(self, t)
            }

            Token::Whitespace(_) => invoke_callback(self, t),

            _ => Some(self_error!(self; "Unexpected token inside qualified name: {}", t)),
        }
    }

    /// Dispatches tokens in order to process attribute value.
    ///
    /// # Parameters
    /// * `t`        --- next token;
    /// * `on_value` --- a callback which is called when terminating quote is encountered.
    fn read_attribute_value<F>(&mut self, t: Token, on_value: F) -> Option<ParserOutcome>
    where
        F: Fn(&mut PullParser, String) -> Option<ParserOutcome>,
    {
        match t {
            Token::Whitespace(_) if self.data.quote.is_none() => None, // skip leading whitespace

            Token::DoubleQuote | Token::SingleQuote => match self.data.quote {
                None => {
                    // Entered attribute value
                    self.data.quote = Some(QuoteToken::from_token(&t));
                    None
                }
                Some(q) if q.as_token() == t => {
                    self.data.quote = None;
                    let value = self.take_buf();
                    on_value(self, value)
                }
                _ => {
                    t.push_to_string(&mut self.buf);
                    None
                }
            },

            Token::ReferenceStart => {
                let st = Box::new(self.st.clone());
                self.as_state_continue(State::InsideReference(st))
            }

            Token::OpeningTagStart => {
                Some(self_error!(self; "Unexpected token inside attribute value: <"))
            }

            // Every character except " and ' and < is okay
            _ => {
                t.push_to_string(&mut self.buf);
                None
            }
        }
    }

    fn emit_start_element(&mut self, emit_end_element: bool) -> Option<ParserOutcome> {
        let mut name = self.data.take_element_name().unwrap();
        let mut attributes = self.data.take_attributes();

        // check whether the name prefix is bound and fix its namespace
        match self.nst.get(name.borrow().prefix_repr()) {
            Some("") => name.namespace = None, // default namespace
            Some(ns) => name.namespace = Some(ns.into()),
            None => return Some(self_error!(self; "Element {} prefix is unbound", name)),
        }

        // check and fix accumulated attributes prefixes
        for attr in attributes.iter_mut() {
            if let Some(ref pfx) = attr.name.prefix {
                let new_ns = match self.nst.get(pfx) {
                    Some("") => None, // default namespace
                    Some(ns) => Some(ns.into()),
                    None => {
                        return Some(self_error!(self; "Attribute {} prefix is unbound", attr.name))
                    }
                };
                attr.name.namespace = new_ns;
            }
        }

        if emit_end_element {
            self.pop_namespace = true;
            self.next_event = Some(Ok(XmlEvent::EndElement { name: name.clone() }));
        } else {
            self.est.push(name.clone());
        }
        let namespace = self.nst.squash();
        self.as_state_emit(
            State::OutsideTag,
            Ok(XmlEvent::StartElement {
                name,
                attributes,
                namespace,
            }),
        )
    }

    fn emit_end_element(&mut self) -> Option<ParserOutcome> {
        let mut name = self.data.take_element_name().unwrap();

        // check whether the name prefix is bound and fix its namespace
        match self.nst.get(name.borrow().prefix_repr()) {
            Some("") => name.namespace = None, // default namespace
            Some(ns) => name.namespace = Some(ns.into()),
            None => return Some(self_error!(self; "Element {} prefix is unbound", name)),
        }

        let op_name = self.est.pop().unwrap();

        if name == op_name {
            self.pop_namespace = true;
            self.as_state_emit(State::OutsideTag, Ok(XmlEvent::EndElement { name }))
        } else {
            Some(self_error!(self; "Unexpected closing tag: {}, expected {}", name, op_name))
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::BufReader;

    use crate::xml::attribute::OwnedAttribute;
    use crate::xml::common::{Position, TextPosition};
    use crate::xml::name::OwnedName;
    use crate::xml::reader::events::XmlEvent;
    use crate::xml::reader::parser::PullParser;
    use crate::xml::reader::ParserConfig;

    fn new_parser() -> PullParser {
        PullParser::new(ParserConfig::new())
    }

    macro_rules! expect_event(
        ($r:expr, $p:expr, $t:pat) => (
            match $p.next(&mut $r) {
                $t => {}
                e => panic!("Unexpected event: {:?}", e)
            }
        );
        ($r:expr, $p:expr, $t:pat => $c:expr ) => (
            match $p.next(&mut $r) {
                $t if $c => {}
                e => panic!("Unexpected event: {:?}", e)
            }
        )
    );

    macro_rules! test_data(
        ($d:expr) => ({
            static DATA: &'static str = $d;
            let r = BufReader::new(DATA.as_bytes());
            let p = new_parser();
            (r, p)
        })
    );

    #[test]
    fn issue_3_semicolon_in_attribute_value() {
        let (mut r, mut p) = test_data!(
            r#"
            <a attr="zzz;zzz" />
        "#
        );

        expect_event!(r, p, Ok(XmlEvent::StartDocument { .. }));
        expect_event!(r, p, Ok(XmlEvent::StartElement { ref name, ref attributes, ref namespace }) =>
            *name == OwnedName::local("a") &&
             attributes.len() == 1 &&
             attributes[0] == OwnedAttribute::new(OwnedName::local("attr"), "zzz;zzz") &&
             namespace.is_essentially_empty()
        );
        expect_event!(r, p, Ok(XmlEvent::EndElement { ref name }) => *name == OwnedName::local("a"));
        expect_event!(r, p, Ok(XmlEvent::EndDocument));
    }

    #[test]
    fn issue_140_entity_reference_inside_tag() {
        let (mut r, mut p) = test_data!(
            r#"
            <bla>&#9835;</bla>
        "#
        );

        expect_event!(r, p, Ok(XmlEvent::StartDocument { .. }));
        expect_event!(r, p, Ok(XmlEvent::StartElement { ref name, .. }) => *name == OwnedName::local("bla"));
        expect_event!(r, p, Ok(XmlEvent::Characters(ref s)) => s == "\u{266b}");
        expect_event!(r, p, Ok(XmlEvent::EndElement { ref name, .. }) => *name == OwnedName::local("bla"));
        expect_event!(r, p, Ok(XmlEvent::EndDocument));
    }

    #[test]
    fn opening_tag_in_attribute_value() {
        let (mut r, mut p) = test_data!(
            r#"
            <a attr="zzz<zzz" />
        "#
        );

        expect_event!(r, p, Ok(XmlEvent::StartDocument { .. }));
        expect_event!(r, p, Err(ref e) =>
            e.msg() == "Unexpected token inside attribute value: <" &&
            e.position() == TextPosition { row: 1, column: 24 }
        );
    }
}
