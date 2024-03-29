//! Contains `XmlEvent` datatype, instances of which are consumed by the writer.

use std::borrow::Cow;

use crate::xml::attribute::Attribute;
use crate::xml::common::XmlVersion;
use crate::xml::name::Name;
use crate::xml::namespace::Namespace;

/// A part of an XML output stream.
///
/// Objects of this enum are consumed by `EventWriter`. They correspond to different parts of
/// an XML document.
#[derive(Debug)]
pub enum XmlEvent<'a> {
    /// Corresponds to XML document declaration.
    ///
    /// This event should always be written before any other event. If it is not written
    /// at all, a default XML declaration will be outputted if the corresponding option
    /// is set in the configuration. Otherwise an error will be returned.
    StartDocument {
        /// XML version.
        ///
        /// Defaults to `XmlVersion::Version10`.
        version: XmlVersion,

        /// XML document encoding.
        ///
        /// Defaults to `Some("UTF-8")`.
        encoding: Option<&'a str>,

        /// XML standalone declaration.
        ///
        /// Defaults to `None`.
        standalone: Option<bool>,
    },

    /// Denotes an XML processing instruction.
    #[cfg(test)]
    ProcessingInstruction {
        /// Processing instruction target.
        name: &'a str,

        /// Processing instruction content.
        data: Option<&'a str>,
    },

    /// Denotes a beginning of an XML element.
    StartElement {
        /// Qualified name of the element.
        name: Name<'a>,

        /// A list of attributes associated with the element.
        ///
        /// Currently attributes are not checked for duplicates (TODO). Attribute values
        /// will be escaped, and all characters invalid for attribute values like `"` or `<`
        /// will be changed into character entities.
        attributes: Cow<'a, [Attribute<'a>]>,

        /// Contents of the namespace mapping at this point of the document.
        ///
        /// This mapping will be inspected for "new" entries, and if at this point of the document
        /// a particular pair of prefix and namespace URI is already defined, no namespace
        /// attributes will be emitted.
        namespace: Cow<'a, Namespace>,
    },

    /// Denotes an end of an XML element.
    EndElement {
        /// Optional qualified name of the element.
        ///
        /// If `None`, then it is assumed that the element name should be the last valid one.
        /// If `Some` and element names tracking is enabled, then the writer will check it for
        /// correctness.
        name: Option<Name<'a>>,
    },

    /// Denotes a comment.
    ///
    /// The string will be checked for invalid sequences and error will be returned by the
    /// write operation
    #[cfg(test)]
    Comment(&'a str),

    /// Denotes character data outside of tags.
    ///
    /// Contents of this event will be escaped.
    Characters(&'a str),
}

impl<'a> XmlEvent<'a> {
    /// Returns a builder for a starting element.
    ///
    /// This builder can then be used to tweak attributes and namespace starting at
    /// this element.
    #[inline]
    #[cfg(test)]
    pub fn start_element<S>(name: S) -> StartElementBuilder<'a>
    where
        S: Into<Name<'a>>,
    {
        StartElementBuilder {
            name: name.into(),
            attributes: Vec::new(),
            namespace: Namespace::empty(),
        }
    }

    /// Returns a builder for an closing element.
    ///
    /// This method, unline `start_element()`, does not accept a name because by default
    /// the writer is able to determine it automatically. However, when this functionality
    /// is disabled, it is possible to specify the name with `name()` method on the builder.
    #[inline]
    #[cfg(test)]
    pub fn end_element() -> EndElementBuilder<'a> {
        EndElementBuilder { name: None }
    }

    /// Returns a regular characters (PCDATA) event.
    ///
    /// All offending symbols, in particular, `&` and `<`, will be escaped by the writer.
    #[inline]
    #[cfg(test)]
    pub fn characters(data: &'a str) -> XmlEvent<'a> {
        XmlEvent::Characters(data)
    }

    /// Returns a comment event.
    #[inline]
    #[cfg(test)]
    pub fn comment(data: &'a str) -> XmlEvent<'a> {
        XmlEvent::Comment(data)
    }
}

impl<'a> From<&'a str> for XmlEvent<'a> {
    #[inline]
    fn from(s: &'a str) -> XmlEvent<'a> {
        XmlEvent::Characters(s)
    }
}

pub struct EndElementBuilder<'a> {
    name: Option<Name<'a>>,
}

impl<'a> From<EndElementBuilder<'a>> for XmlEvent<'a> {
    fn from(b: EndElementBuilder<'a>) -> XmlEvent<'a> {
        XmlEvent::EndElement { name: b.name }
    }
}

/// A builder for a starting element event.
pub struct StartElementBuilder<'a> {
    name: Name<'a>,
    attributes: Vec<Attribute<'a>>,
    namespace: Namespace,
}

impl<'a> StartElementBuilder<'a> {
    /// Sets an attribute value of this element to the given string.
    ///
    /// This method can be used to add attributes to the starting element. Name is a qualified
    /// name; its namespace is ignored, but its prefix is checked for correctness, that is,
    /// it is checked that the prefix is bound to some namespace in the current context.
    ///
    /// Currently attributes are not checked for duplicates. Note that duplicate attributes
    /// are a violation of XML document well-formedness.
    ///
    /// The writer checks that you don't specify reserved prefix names, for example `xmlns`.
    #[inline]
    #[cfg(test)]
    pub fn attr<N>(mut self, name: N, value: &'a str) -> StartElementBuilder<'a>
    where
        N: Into<Name<'a>>,
    {
        self.attributes.push(Attribute::new(name.into(), value));
        self
    }

    /// Adds a namespace to the current namespace context.
    ///
    /// If no namespace URI was bound to the provided prefix at this point of the document,
    /// then the mapping from the prefix to the provided namespace URI will be written as
    /// a part of this element attribute set.
    ///
    /// If the same namespace URI was bound to the provided prefix at this point of the document,
    /// then no namespace attributes will be emitted.
    ///
    /// If some other namespace URI was bound to the provided prefix at this point of the document,
    /// then another binding will be added as a part of this element attribute set, shadowing
    /// the outer binding.
    #[inline]
    #[cfg(test)]
    pub fn ns<S1, S2>(mut self, prefix: S1, uri: S2) -> StartElementBuilder<'a>
    where
        S1: Into<String>,
        S2: Into<String>,
    {
        self.namespace.put(prefix, uri);
        self
    }
}

impl<'a> From<StartElementBuilder<'a>> for XmlEvent<'a> {
    #[inline]
    fn from(b: StartElementBuilder<'a>) -> XmlEvent<'a> {
        XmlEvent::StartElement {
            name: b.name,
            attributes: Cow::Owned(b.attributes),
            namespace: Cow::Owned(b.namespace),
        }
    }
}
