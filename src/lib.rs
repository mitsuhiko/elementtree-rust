//! A simple library for parsing an XML file into an in-memory tree structure
//!
//! Not recommended for large XML files, as it will load the entire file into memory.
extern crate xml;
extern crate string_cache;

use std::collections::HashMap;
use std::io::Read;
use std::fmt;
use std::ops::Deref;
use std::hash::{Hash, Hasher};
use std::borrow::Cow;

use string_cache::DefaultAtom as Atom;

use xml::reader::{EventReader, XmlEvent};
use xml::attribute::OwnedAttribute;
use xml::name::OwnedName;

enum XmlAtom<'a> {
    Shared(Atom),
    Borrowed(&'a str),
}

impl<'a> Deref for XmlAtom<'a> {
    type Target = str;

    #[inline(always)]
    fn deref(&self) -> &str {
        match *self {
            XmlAtom::Shared(ref atom) => atom.deref(),
            XmlAtom::Borrowed(s) => s,
        }
    }
}

impl<'a> XmlAtom<'a> {
    #[inline(always)]
    pub fn borrow(&self) -> &str {
        &self
    }
}

impl<'a> fmt::Debug for XmlAtom<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.borrow())
    }
}

impl<'a> Clone for XmlAtom<'a> {
    fn clone(&self) -> XmlAtom<'a> {
        XmlAtom::Shared(Atom::from(self.borrow()))
    }
}

/// Convenience trait to get a `QName` from an object.
///
/// This is used for the accessor interface on elements.
pub trait AsQName<'a> {
    /// Returns a Cow'ed `QName` from the given object.
    fn as_qname(&self) -> Cow<'a, QName<'a>>;
}

impl<'a> AsQName<'a> for &'a QName<'a> {
    #[inline(always)]
    fn as_qname(&self) -> Cow<'a, QName<'a>> {
        Cow::Borrowed(self)
    }
}

impl<'a> AsQName<'a> for &'a str {
    #[inline(always)]
    fn as_qname(&self) -> Cow<'a, QName<'a>> {
        Cow::Owned(QName::from(self))
    }
}

impl<'a> AsQName<'a> for (&'a str, &'a str) {
    #[inline(always)]
    fn as_qname(&self) -> Cow<'a, QName<'a>> {
        Cow::Owned(QName::from_ns_name(Some(self.0), self.1))
    }
}

/// A `QName` represents a qualified name.
///
/// A qualified name is a tag or attribute name that has a namespace and a
/// local name.  If the namespace is empty no namespace is assumed.  It
/// can be constructed from a qualified name string with the ``from``
/// method.
///
/// ## Notes on Memory Management
///
/// Qualified names that are user constructed for comparison purposes
/// usually have a static lifetime because they are created from static
/// strings.  Creating qualified names from other strings might make
/// memory management harder which is why `share()` exists which moves
/// the `QName` internal strings to shared storage in which the lifetime
/// changes to `'static`.
///
/// Common usage examples:
///
/// ```no_run
/// # use elementtree::QName;
/// let href = QName::from_name("href");
/// let a = QName::from("{http://www.w3.org/1999/xhtml}a");
/// ```
#[derive(Clone)]
pub struct QName<'a> {
    ns: Option<XmlAtom<'a>>,
    name: XmlAtom<'a>,
}

impl<'a> QName<'a> {
    /// Creates a qualified name from a given string.
    ///
    /// Two formats are supported ``{namespace}tag`` or just ``tag``.
    ///
    /// ```
    /// # use elementtree::QName;
    /// let a = QName::from("{http://www.w3.org/1999/xhtml}a");
    /// ```
    pub fn from(s: &'a str) -> QName<'a> {
        let mut ns = None;
        let mut name = None;
        if s.starts_with('{') {
            if let Some(index) = s.find('}') {
                if index > 1 {
                    ns = Some(XmlAtom::Borrowed(&s[1..index]));
                }
                name = Some(XmlAtom::Borrowed(&s[index + 1..]));
            }
        }

        QName {
            ns: ns,
            name: name.unwrap_or_else(|| XmlAtom::Borrowed(s)),
        }
    }

    /// Creates a qualified name from a given string without namespace.
    ///
    /// This is slightly faster than using ``from()``.
    pub fn from_name(name: &'a str) -> QName<'a> {
        QName {
            ns: None,
            name: XmlAtom::Borrowed(name),
        }
    }

    /// Creates a qualified name from a namespace and name.
    pub fn from_ns_name(ns: Option<&'a str>, name: &'a str) -> QName<'a> {
        QName {
            ns: ns.map(|x| XmlAtom::Borrowed(x)),
            name: XmlAtom::Borrowed(name),
        }
    }

    /// Returns the name portion of the qualified name.  This is the local
    /// tag or attribute name.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Returns the optional namespace of this element.  This is the URL of
    /// the namespace and not the prefix.  The information about the latter
    /// is not retained.
    pub fn ns(&self) -> Option<&str> {
        self.ns.as_ref().map(|x| x.borrow())
    }

    /// Creates a shared `QName` with static lifetime from an already
    /// existing `QName`.  The internal strings are interned and might
    /// be shared with other instances.
    pub fn share(self) -> QName<'static> {
        QName {
            name: XmlAtom::Shared(Atom::from(self.name.borrow())),
            ns: self.ns.map(|x| XmlAtom::Shared(Atom::from(x.borrow()))),
        }
    }

    fn from_owned_name(name: OwnedName) -> QName<'static> {
        QName {
            name: XmlAtom::Shared(Atom::from(name.local_name)),
            ns: name.namespace.map(|x| XmlAtom::Shared(Atom::from(x))),
        }
    }
}

impl<'a> PartialEq for QName<'a> {
    fn eq(&self, other: &QName<'a>) -> bool {
         self.name() == other.name() && self.ns() == other.ns()
    }
}

impl<'a> fmt::Debug for QName<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "QName(\"{}\")", self)
    }
}

impl<'a> fmt::Display for QName<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref ns) = self.ns {
            write!(f, "{{{}}}", ns.borrow())?;
        }
        write!(f, "{}", self.name.borrow())
    }
}

impl<'a> Eq for QName<'a> {}

impl<'a> Hash for QName<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        if let Some(ref ns) = self.ns {
            ns.hash(state);
        }
    }
}

/// Represents an XML element.
#[derive(Debug, Clone)]
pub struct Element {
    tag: QName<'static>,
    attributes: HashMap<QName<'static>, String>,
    children: Vec<Element>,
    text: Option<String>,
    tail: Option<String>,
}

/// An iterator over children of an element.
pub struct Children<'a> {
    idx: usize,
    element: &'a Element,
}

/// An iterator over matching children.
pub struct FindChildren<'a> {
    tag: Cow<'a, QName<'a>>,
    child_iter: Children<'a>,
}

/// Errors that can occur parsing XML
#[derive(Debug)]
pub enum ParseError {
    /// The XML is invalid
    MalformedXml(xml::reader::Error),
    /// This library is unable to process this XML. This can occur if, for
    /// example, the XML contains processing instructions.
    UnexpectedEvent(XmlEvent),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ParseError::MalformedXml(ref e) => write!(f, "Malformed XML. {}", e),
            &ParseError::UnexpectedEvent(..) => write!(f, "Unexpected XML event"),
        }
    }
}

impl std::error::Error for ParseError {
    fn description(&self) -> &str {
        match self {
            &ParseError::MalformedXml(..) => "Malformed XML",
            &ParseError::UnexpectedEvent(..) => "Unexpected XML event",
        }
    }

    fn cause(&self) -> Option<&std::error::Error> {
        match self {
            &ParseError::MalformedXml(ref e) => Some(e),
            &ParseError::UnexpectedEvent(..) => None,
        }
    }
}

impl<'a> Iterator for Children<'a> {
    type Item = &'a Element;

    fn next(&mut self) -> Option<&'a Element> {
        if self.idx < self.element.children.len() {
            let rv = &self.element.children[self.idx];
            self.idx += 1;
            Some(rv)
        } else {
            None
        }
    }
}

impl<'a> Iterator for FindChildren<'a> {
    type Item = &'a Element;

    fn next(&mut self) -> Option<&'a Element> {
        use std::borrow::Borrow;
        loop {
            if let Some(child) = self.child_iter.next() {
                if child.tag() == self.tag.borrow() {
                    return Some(child);
                }
            } else {
                return None;
            }
        }
    }
}

impl Element {
    /// Parses some data into an Element
    pub fn from_reader<R: Read>(r: R) -> Result<Element, ParseError> {
        let mut reader = EventReader::new(r);
        loop {
            match reader.next() {
                Ok(XmlEvent::StartElement { name, attributes, .. }) => {
                    return Element::from_start_element(name, attributes, &mut reader);
                }
                Ok(XmlEvent::Comment(..)) |
                Ok(XmlEvent::Whitespace(..)) |
                Ok(XmlEvent::StartDocument { .. }) |
                Ok(XmlEvent::ProcessingInstruction { .. }) => continue,
                Ok(evt) => return Err(ParseError::UnexpectedEvent(evt)),
                Err(e) => return Err(ParseError::MalformedXml(e)),
            }
        }
    }

    fn from_start_element<R: Read>(name: OwnedName,
                                   attributes: Vec<OwnedAttribute>,
                                   reader: &mut EventReader<R>)
        -> Result<Element, ParseError>
    {
        let mut attr_map = HashMap::new();
        for attr in attributes {
            attr_map.insert(QName::from_owned_name(attr.name), attr.value);
        }

        let mut root = Element {
            tag: QName::from_owned_name(name),
            attributes: attr_map,
            children: vec![],
            text: None,
            tail: None,
        };
        root.parse_children(reader)?;
        Ok(root)
    }

    fn parse_children<R: Read>(&mut self,
                               reader: &mut EventReader<R>)
        -> Result<(), ParseError>
    {
        loop {
            match reader.next() {
                Ok(XmlEvent::EndElement { ref name }) => {
                    if &name.local_name == self.tag.name() &&
                       name.namespace.as_ref().map(|x| x.as_str()) == self.tag.ns() {
                        return Ok(());
                    } else {
                        return Err(ParseError::UnexpectedEvent(XmlEvent::EndElement {
                            name: name.clone(),
                        }));
                    }
                }
                Ok(XmlEvent::StartElement { name, attributes, .. }) => {
                    self.children.push(Element::from_start_element(
                        name, attributes, reader)?);
                }
                Ok(XmlEvent::Characters(s)) => {
                    let child_count = self.children.len();
                    if child_count > 0 {
                        self.children[child_count - 1].tail = Some(s);
                    } else {
                        self.text = Some(s);
                    }
                }
                Ok(XmlEvent::CData(s)) => self.text = Some(s),
                Ok(XmlEvent::Comment(..)) |
                Ok(XmlEvent::Whitespace(..)) |
                Ok(XmlEvent::StartDocument { .. }) |
                Ok(XmlEvent::ProcessingInstruction { .. }) => continue,
                Ok(evt) => return Err(ParseError::UnexpectedEvent(evt)),
                Err(e) => return Err(ParseError::MalformedXml(e)),
            }
        }
    }

    /// Returns the text of a tag
    pub fn text(&self) -> &str {
        self.text.as_ref().map(|x| x.as_str()).unwrap_or("")
    }

    /// Returns the tail text of a tag
    pub fn tail(&self) -> &str {
        self.tail.as_ref().map(|x| x.as_str()).unwrap_or("")
    }

    /// The tag of the element as qualified name
    pub fn tag(&self) -> &QName {
        &self.tag
    }

    /// Returns the number of children
    pub fn child_count(&self) -> usize {
        self.children.len()
    }

    /// Returns the nth child.
    pub fn get_child(&self, idx: usize) -> Option<&Element> {
        self.children.get(idx)
    }

    /// Returns an iterator over all children.
    pub fn children<'a>(&'a self) -> Children<'a> {
        Children {
            idx: 0,
            element: self,
        }
    }

    /// Returns all children with the given name.
    pub fn find_all<'a, Q: AsQName<'a>>(&'a self, tag: Q) -> FindChildren<'a>
    {
        FindChildren {
            tag: tag.as_qname(),
            child_iter: self.children(),
        }
    }

    /// Finds the first matching child
    pub fn find<'a, Q: AsQName<'a>>(&'a self, tag: Q) -> Option<&'a Element> {
        use std::borrow::Borrow;
        let tag = tag.as_qname();

        for child in self.children() {
            if child.tag() == tag.borrow() {
                return Some(child);
            }
        }
        None
    }

    /// Look up an attribute by qualified name.
    pub fn get_attr<'a, Q: AsQName<'a>>(&'a self, name: Q) -> Option<&'a str> {
        self.attributes.get(&name.as_qname()).map(|x| x.as_str())
    }

    /// Finds the first element that match a given path downwards
    pub fn navigate<'a, Q: AsQName<'a>>(&'a self, path: &[Q]) -> Option<&'a Element> {
        use std::borrow::Borrow;
        let mut node = self;

        'outer: for piece in path {
            let reftag = piece.as_qname();
            for child in node.children() {
                if child.tag() == reftag.borrow() {
                    node = child;
                    continue 'outer;
                }
            }
            return None;
        }

        Some(node)
    }
}
