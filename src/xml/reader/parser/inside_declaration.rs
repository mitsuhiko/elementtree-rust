use crate::xml::common::XmlVersion;
use crate::xml::reader::events::XmlEvent;
use crate::xml::reader::lexer::Token;
use crate::xml::reader::parser::{
    DeclarationSubstate, ParserOutcome, PullParser, QualifiedNameTarget, State, DEFAULT_ENCODING,
    DEFAULT_VERSION,
};

impl PullParser {
    // TODO: remove redundancy via macros or extra methods
    pub fn inside_declaration(
        &mut self,
        t: Token,
        s: DeclarationSubstate,
    ) -> Option<ParserOutcome> {
        macro_rules! unexpected_token(
            ($this:expr; $t:expr) => (Some($this.error(format!("Unexpected token inside XML declaration: {}", $t))));
            ($t:expr) => (unexpected_token!(self; $t));
        );

        #[inline]
        fn emit_start_document(this: &mut PullParser) -> Option<ParserOutcome> {
            this.parsed_declaration = true;
            let version = this.data.take_version();
            let encoding = this.data.take_encoding();
            let standalone = this.data.take_standalone();
            this.as_state_emit(
                State::OutsideTag,
                Ok(XmlEvent::StartDocument {
                    version: version.unwrap_or(DEFAULT_VERSION),
                    encoding: encoding.unwrap_or_else(|| DEFAULT_ENCODING.into()),
                    standalone,
                }),
            )
        }

        match s {
            DeclarationSubstate::BeforeVersion => match t {
                Token::Whitespace(_) => None, // continue
                Token::Character('v') => self.as_state_continue(State::InsideDeclaration(
                    DeclarationSubstate::InsideVersion,
                )),
                _ => unexpected_token!(t),
            },

            DeclarationSubstate::InsideVersion => {
                self.read_qualified_name(t, QualifiedNameTarget::Attribute, |this, token, name| {
                    match &name.local_name[..] {
                        "ersion" if name.namespace.is_none() => this.as_state_continue(
                            State::InsideDeclaration(if token == Token::EqualsSign {
                                DeclarationSubstate::InsideVersionValue
                            } else {
                                DeclarationSubstate::AfterVersion
                            }),
                        ),
                        _ => unexpected_token!(this; name),
                    }
                })
            }

            DeclarationSubstate::AfterVersion => match t {
                Token::Whitespace(_) => None,
                Token::EqualsSign => self.as_state_continue(State::InsideDeclaration(
                    DeclarationSubstate::InsideVersionValue,
                )),
                _ => unexpected_token!(t),
            },

            DeclarationSubstate::InsideVersionValue => {
                self.read_attribute_value(t, |this, value| {
                    this.data.version = match &value[..] {
                        "1.0" => Some(XmlVersion::Version10),
                        "1.1" => Some(XmlVersion::Version11),
                        _ => None,
                    };
                    if this.data.version.is_some() {
                        this.as_state_continue(State::InsideDeclaration(
                            DeclarationSubstate::AfterVersionValue,
                        ))
                    } else {
                        Some(self_error!(this; "Unexpected XML version value: {}", value))
                    }
                })
            }

            DeclarationSubstate::AfterVersionValue => match t {
                Token::Whitespace(_) => None, // skip whitespace
                Token::Character('e') => self.as_state_continue(State::InsideDeclaration(
                    DeclarationSubstate::InsideEncoding,
                )),
                Token::Character('s') => self.as_state_continue(State::InsideDeclaration(
                    DeclarationSubstate::InsideStandaloneDecl,
                )),
                Token::ProcessingInstructionEnd => emit_start_document(self),
                _ => unexpected_token!(t),
            },

            DeclarationSubstate::InsideEncoding => {
                self.read_qualified_name(t, QualifiedNameTarget::Attribute, |this, token, name| {
                    match &name.local_name[..] {
                        "ncoding" if name.namespace.is_none() => this.as_state_continue(
                            State::InsideDeclaration(if token == Token::EqualsSign {
                                DeclarationSubstate::InsideEncodingValue
                            } else {
                                DeclarationSubstate::AfterEncoding
                            }),
                        ),
                        _ => unexpected_token!(this; name),
                    }
                })
            }

            DeclarationSubstate::AfterEncoding => match t {
                Token::Whitespace(_) => None,
                Token::EqualsSign => self.as_state_continue(State::InsideDeclaration(
                    DeclarationSubstate::InsideEncodingValue,
                )),
                _ => unexpected_token!(t),
            },

            DeclarationSubstate::InsideEncodingValue => {
                self.read_attribute_value(t, |this, value| {
                    this.data.encoding = Some(value);
                    this.as_state_continue(State::InsideDeclaration(
                        DeclarationSubstate::BeforeStandaloneDecl,
                    ))
                })
            }

            DeclarationSubstate::BeforeStandaloneDecl => match t {
                Token::Whitespace(_) => None, // skip whitespace
                Token::Character('s') => self.as_state_continue(State::InsideDeclaration(
                    DeclarationSubstate::InsideStandaloneDecl,
                )),
                Token::ProcessingInstructionEnd => emit_start_document(self),
                _ => unexpected_token!(t),
            },

            DeclarationSubstate::InsideStandaloneDecl => {
                self.read_qualified_name(t, QualifiedNameTarget::Attribute, |this, token, name| {
                    match &name.local_name[..] {
                        "tandalone" if name.namespace.is_none() => this.as_state_continue(
                            State::InsideDeclaration(if token == Token::EqualsSign {
                                DeclarationSubstate::InsideStandaloneDeclValue
                            } else {
                                DeclarationSubstate::AfterStandaloneDecl
                            }),
                        ),
                        _ => unexpected_token!(this; name),
                    }
                })
            }

            DeclarationSubstate::AfterStandaloneDecl => match t {
                Token::Whitespace(_) => None,
                Token::EqualsSign => self.as_state_continue(State::InsideDeclaration(
                    DeclarationSubstate::InsideStandaloneDeclValue,
                )),
                _ => unexpected_token!(t),
            },

            DeclarationSubstate::InsideStandaloneDeclValue => {
                self.read_attribute_value(t, |this, value| {
                    let standalone = match &value[..] {
                        "yes" => Some(true),
                        "no" => Some(false),
                        _ => None,
                    };
                    if standalone.is_some() {
                        this.data.standalone = standalone;
                        this.as_state_continue(State::InsideDeclaration(
                            DeclarationSubstate::AfterStandaloneDeclValue,
                        ))
                    } else {
                        Some(self_error!(this; "Invalid standalone declaration value: {}", value))
                    }
                })
            }

            DeclarationSubstate::AfterStandaloneDeclValue => match t {
                Token::Whitespace(_) => None, // skip whitespace
                Token::ProcessingInstructionEnd => emit_start_document(self),
                _ => unexpected_token!(t),
            },
        }
    }
}
