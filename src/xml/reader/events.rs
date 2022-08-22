//! Contains `XmlEvent` datatype, instances of which are emitted by the parser.

use std::fmt;

use crate::xml::attribute::OwnedAttribute;
use crate::xml::common::XmlVersion;
use crate::xml::name::OwnedName;
use crate::xml::namespace::Namespace;

/// An element of an XML input stream.
///
/// Items of this enum are emitted by `reader::EventReader`. They correspond to different
/// elements of an XML document.
#[derive(PartialEq, Eq, Clone)]
pub enum XmlEvent {
    /// Corresponds to XML document declaration.
    ///
    /// This event is always emitted before any other event. It is emitted
    /// even if the actual declaration is not present in the document.
    StartDocument {
        /// XML version.
        ///
        /// If XML declaration is not present, defaults to `Version10`.
        version: XmlVersion,

        /// XML document encoding.
        ///
        /// If XML declaration is not present or does not contain `encoding` attribute,
        /// defaults to `"UTF-8"`. This field is currently used for no other purpose than
        /// informational.
        encoding: String,

        /// XML standalone declaration.
        ///
        /// If XML document is not present or does not contain `standalone` attribute,
        /// defaults to `None`. This field is currently used for no other purpose than
        /// informational.
        standalone: Option<bool>,
    },

    /// Denotes to the end of the document stream.
    ///
    /// This event is always emitted after any other event (except `Error`). After it
    /// is emitted for the first time, it will always be emitted on next event pull attempts.
    EndDocument,

    /// Denotes an XML processing instruction.
    ///
    /// This event contains a processing instruction target (`name`) and opaque `data`. It
    /// is up to the application to process them.
    ProcessingInstruction {
        /// Processing instruction target.
        name: String,

        /// Processing instruction content.
        data: Option<String>,
    },

    /// Denotes a beginning of an XML element.
    ///
    /// This event is emitted after parsing opening tags or after parsing bodiless tags. In the
    /// latter case `EndElement` event immediately follows.
    StartElement {
        /// Qualified name of the element.
        name: OwnedName,

        /// A list of attributes associated with the element.
        ///
        /// Currently attributes are not checked for duplicates (TODO)
        attributes: Vec<OwnedAttribute>,

        /// Contents of the namespace mapping at this point of the document.
        namespace: Namespace,
    },

    /// Denotes an end of an XML element.
    ///
    /// This event is emitted after parsing closing tags or after parsing bodiless tags. In the
    /// latter case it is emitted immediately after corresponding `StartElement` event.
    EndElement {
        /// Qualified name of the element.
        name: OwnedName,
    },

    /// Denotes character data outside of tags.
    ///
    /// Contents of this event will always be unescaped, so no entities like `&lt;` or `&amp;` or `&#123;`
    /// will appear in it.
    ///
    /// It is possible to configure a parser to trim leading and trailing whitespace for this event.
    /// See `pull::ParserConfiguration` structure for more information.
    Characters(String),
}

impl fmt::Debug for XmlEvent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            XmlEvent::StartDocument {
                ref version,
                ref encoding,
                ref standalone,
            } => write!(
                f,
                "StartDocument({}, {}, {:?})",
                version, *encoding, *standalone
            ),
            XmlEvent::EndDocument => write!(f, "EndDocument"),
            XmlEvent::ProcessingInstruction { ref name, ref data } => write!(
                f,
                "ProcessingInstruction({}{})",
                *name,
                match *data {
                    Some(ref data) => format!(", {}", data),
                    None => String::new(),
                }
            ),
            XmlEvent::StartElement {
                ref name,
                ref attributes,
                namespace: Namespace(ref namespace),
            } => write!(
                f,
                "StartElement({}, {:?}{})",
                name,
                namespace,
                if attributes.is_empty() {
                    String::new()
                } else {
                    let attributes: Vec<String> = attributes
                        .iter()
                        .map(|a| format!("{} -> {}", a.name, a.value))
                        .collect();
                    format!(", [{}]", attributes.join(", "))
                }
            ),
            XmlEvent::EndElement { ref name } => write!(f, "EndElement({})", name),
            XmlEvent::Characters(ref data) => write!(f, "Characters({})", data),
        }
    }
}

impl XmlEvent {
    /// Obtains a writer event from this reader event.
    ///
    /// This method is useful for streaming processing of XML documents where the output
    /// is also an XML document. With this method it is possible to process some events
    /// while passing other events through to the writer unchanged:
    #[cfg(test)]
    pub fn as_writer_event(&self) -> Option<crate::xml::writer::events::XmlEvent<'_>> {
        match *self {
            XmlEvent::StartDocument {
                version,
                ref encoding,
                standalone,
            } => Some(crate::xml::writer::events::XmlEvent::StartDocument {
                version,
                encoding: Some(encoding),
                standalone,
            }),
            XmlEvent::ProcessingInstruction { ref name, ref data } => Some(
                crate::xml::writer::events::XmlEvent::ProcessingInstruction {
                    name,
                    data: data.as_ref().map(|s| &s[..]),
                },
            ),
            XmlEvent::StartElement {
                ref name,
                ref attributes,
                ref namespace,
            } => Some(crate::xml::writer::events::XmlEvent::StartElement {
                name: name.borrow(),
                attributes: attributes.iter().map(|a| a.borrow()).collect(),
                namespace: std::borrow::Cow::Borrowed(namespace),
            }),
            XmlEvent::EndElement { ref name } => {
                Some(crate::xml::writer::events::XmlEvent::EndElement {
                    name: Some(name.borrow()),
                })
            }
            XmlEvent::Characters(ref data) => {
                Some(crate::xml::writer::events::XmlEvent::Characters(data))
            }
            _ => None,
        }
    }
}

#[test]
fn test_xml_event() {
    use std::str;

    use crate::xml::reader::XmlEvent as ReaderEvent;
    use crate::xml::writer::XmlEvent as WriterEvent;
    use crate::xml::{EventReader, EventWriter};

    let mut input: &[u8] = b"<hello>world</hello>";
    let mut output: Vec<u8> = Vec::new();

    {
        let reader = EventReader::new(&mut input);
        let mut writer = EventWriter::new(&mut output);

        for e in reader {
            match e.unwrap() {
                ReaderEvent::Characters(s) => writer
                    .write(WriterEvent::characters(&s.to_uppercase()))
                    .unwrap(),
                e => {
                    if let Some(e) = e.as_writer_event() {
                        writer.write(e).unwrap()
                    }
                }
            }
        }
    }

    assert_eq!(
        str::from_utf8(&output).unwrap(),
        r#"<?xml version="1.0" encoding="UTF-8"?><hello>WORLD</hello>"#
    );
}
