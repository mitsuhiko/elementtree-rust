//! A simple library for parsing an XML file into an in-memory tree structure
//!
//! Not recommended for large XML files, as it will load the entire file into memory.
//!
//! ## Reading
//!
//! For reading XML you can use the `Element::from_reader` method which will
//! parse from a given reader.  Afterwards you end up with a fancy element
//! tree that can be accessed in various different ways.
//!
//! You can use ``("ns", "tag")`` or ``{ns}tag`` to refer to fully qualified
//! elements.
//!
//! ```rust
//! # use elementtree::Element;
//! let root = Element::from_reader(r#"<?xml version="1.0"?>
//! <root xmlns="tag:myns" xmlns:foo="tag:otherns">
//!     <list a="1" b="2" c="3">
//!         <item foo:attr="foo1"/>
//!         <item foo:attr="foo2"/>
//!         <item foo:attr="foo3"/>
//!     </list>
//! </root>
//! "#.as_bytes()).unwrap();
//! let list = root.find("{tag:myns}list").unwrap();
//! for child in list.find_all("{tag:myns}item") {
//!     println!("attribute: {}", child.get_attr("{tag:otherns}attr").unwrap());
//! }
//! ```
//!
//! ## Writing
//!
//! Writing is easy as well but if you work with namespaces you will need to
//! register them with the root.  If namespaces are not used yet they will
//! otherwise be registered with an empty (and once that is used a random prefix)
//! on the element itself which will blow up the XML size.
//!
//! Most methods for modification support chaining in one form or another which
//! makes modifications slightly more ergonomic.
//!
//! ```
//! # use elementtree::Element;
//! let ns = "http://example.invalid/#myns";
//! let other_ns = "http://example.invalid/#otherns";
//!
//! let mut root = Element::new((ns, "mydoc"));
//! root.set_namespace_prefix(other_ns, "other");
//!
//! {
//!     let mut list = root.append_new_child((ns, "list"));
//!     for x in 0..3 {
//!         list.append_new_child((ns, "item"))
//!             .set_text(format!("Item {}", x))
//!             .set_attr((other_ns, "id"), x.to_string());
//!     }
//! }
//! ```
//!
//! ## Design Notes
//!
//! This library largely follows the ideas of Python's ElementTree but it has some
//! specific changes that simplify the model for Rust.  In particular nodes do not
//! know about their parents or siblings.  While this obviously reduces a lot of
//! what would be possible with the library it significantly simplifies memory
//! management and the external API.
//!
//! If you are coming from a DOM environment the following differences are the
//! most striking:
//!
//! *   There are no text nodes, instead text is stored either in the `text`
//!     attribute of a node or in the `tail` of a child node.  This means that
//!     for most situations just working with the `text` is what you want and
//!     you can ignore the existence of the `tail`.
//! *   tags and attributes are implemented through a `QName` abstraction that
//!     simplifies working wiht namespaces.  Most APIs just accept strings and
//!     will create `QName`s automatically.
//! *   namespace prefixes never play a role and are in fact not really exposed.
//!     Instead all namespaces are managed through their canonical identifier.
//!
//! ## Notes on Namespaces
//!
//! Namespaces are internally tracked in a shared map attached to elements.  The
//! map is not exposed but when an element is created another element can be passed
//! in and the namespace map is copied over.  Internally a copy on write mechanism
//! is used so when changes are performed on the namespace the namespaces will be
//! copied and the writer will emit them accordingly.
//!
//! Namespaces need to be registered or the XML generated will be malformed.
use std::borrow::Cow;
use std::cmp::Ord;
use std::cmp::Ordering;
use std::collections::btree_map::Iter as BTreeMapIter;
use std::collections::BTreeMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::io;
use std::io::{Read, Write};
use std::mem;
use std::ops::Deref;
use std::rc::Rc;
use std::str::Utf8Error;

use string_cache::DefaultAtom as Atom;

use xml::attribute::{Attribute, OwnedAttribute};
use xml::common::{Position as XmlPosition, XmlVersion};
use xml::name::{Name, OwnedName};
use xml::namespace::{Namespace as XmlNamespaceMap, NS_EMPTY_URI, NS_XMLNS_URI, NS_XML_URI};
use xml::reader::{
    Error as XmlReadError, ErrorKind as XmlReadErrorKind, EventReader, ParserConfig, XmlEvent,
};
use xml::writer::{Error as XmlWriteError, EventWriter, XmlEvent as XmlWriteEvent};
use xml::EmitterConfig;

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
        self
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

impl<'a> PartialEq for XmlAtom<'a> {
    fn eq(&self, other: &XmlAtom<'a>) -> bool {
        self.borrow().eq(other.borrow())
    }
}

impl<'a> Eq for XmlAtom<'a> {}

impl<'a> PartialOrd for XmlAtom<'a> {
    fn partial_cmp(&self, other: &XmlAtom<'a>) -> Option<Ordering> {
        self.borrow().partial_cmp(other.borrow())
    }
}

impl<'a> Ord for XmlAtom<'a> {
    fn cmp(&self, other: &XmlAtom<'a>) -> Ordering {
        self.borrow().cmp(other.borrow())
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
            ns,
            name: name.unwrap_or(XmlAtom::Borrowed(s)),
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
            ns: ns.map(XmlAtom::Borrowed),
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
    pub fn share(&self) -> QName<'static> {
        QName {
            name: XmlAtom::Shared(Atom::from(self.name.borrow())),
            ns: self
                .ns
                .as_ref()
                .map(|x| XmlAtom::Shared(Atom::from(x.borrow()))),
        }
    }

    fn from_owned_name(name: OwnedName) -> QName<'static> {
        QName {
            name: XmlAtom::Shared(Atom::from(name.local_name)),
            ns: match name.namespace {
                Some(ns) => {
                    if !ns.is_empty() {
                        Some(XmlAtom::Shared(Atom::from(ns)))
                    } else {
                        None
                    }
                }
                _ => None,
            },
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

impl<'a> PartialOrd for QName<'a> {
    fn partial_cmp(&self, other: &QName<'a>) -> Option<Ordering> {
        self.name().partial_cmp(other.name())
    }
}

impl<'a> Ord for QName<'a> {
    fn cmp(&self, other: &QName<'a>) -> Ordering {
        self.name().cmp(other.name())
    }
}

#[derive(Debug, Clone)]
struct NamespaceMap {
    prefix_to_ns: BTreeMap<XmlAtom<'static>, XmlAtom<'static>>,
    ns_to_prefix: BTreeMap<XmlAtom<'static>, XmlAtom<'static>>,
}

impl NamespaceMap {
    pub fn new() -> NamespaceMap {
        NamespaceMap {
            prefix_to_ns: BTreeMap::new(),
            ns_to_prefix: BTreeMap::new(),
        }
    }

    pub fn get_prefix(&self, url: &str) -> Option<&str> {
        // same shit as with remove_attr below for the explanation.
        let atom = XmlAtom::Borrowed(url);
        let static_atom: &XmlAtom<'static> = unsafe { mem::transmute(&atom) };
        self.ns_to_prefix.get(static_atom).map(|x| x.borrow())
    }

    pub fn set_prefix(&mut self, url: &str, prefix: &str) -> Result<(), Error> {
        let prefix = XmlAtom::Shared(Atom::from(prefix));
        if self.prefix_to_ns.contains_key(&prefix) {
            return Err(Error::DuplicateNamespacePrefix);
        }

        let url = XmlAtom::Shared(Atom::from(url));
        if let Some(old_prefix) = self.ns_to_prefix.remove(&url) {
            self.prefix_to_ns.remove(&old_prefix);
        }

        self.ns_to_prefix.insert(url.clone(), prefix.clone());
        self.prefix_to_ns.insert(prefix.clone(), url.clone());

        Ok(())
    }

    fn generate_prefix(&self) -> XmlAtom<'static> {
        let mut i = 1;
        loop {
            let random_prefix = format!("ns{}", i);
            if !self
                .prefix_to_ns
                .contains_key(&XmlAtom::Borrowed(&random_prefix))
            {
                return XmlAtom::Shared(Atom::from(random_prefix));
            }
            i += 1;
        }
    }

    pub fn register_if_missing(&mut self, url: &str, prefix: Option<&str>) -> bool {
        if self.get_prefix(url).is_some() {
            return false;
        }

        let stored_prefix = if let Some(prefix) = prefix {
            let prefix = XmlAtom::Borrowed(prefix);
            if self.prefix_to_ns.get(&prefix).is_some() {
                self.generate_prefix()
            } else {
                XmlAtom::Shared(Atom::from(prefix.borrow()))
            }
        } else {
            self.generate_prefix()
        };

        let url = XmlAtom::Shared(Atom::from(url));
        self.prefix_to_ns.insert(stored_prefix.clone(), url.clone());
        self.ns_to_prefix.insert(url, stored_prefix);
        true
    }
}

/// Represents an XML element.
///
/// Usually constructed from either parsing or one of the two constructors
/// an element is part of a tree and represents an XML element and the
/// children contained.
///
/// Imagine a structure like this:
///
/// ```xml
/// <p>Hello <strong>World</strong>!</p>
/// ```
///
/// In this case the structure is more or less represented like this:
///
/// ```ignore
/// Element {
///   tag: "p",
///   text: "Hello ",
///   tail: None,
///   children: [
///     Element {
///       tag: "strong",
///       text: "World",
///       tail: Some("!")
///     }
///   ]
/// }
/// ```
///
/// Namespaces are internally managed and inherited downwards when an
/// element is created.
#[derive(Debug, Clone)]
pub struct Element {
    tag: QName<'static>,
    attributes: BTreeMap<QName<'static>, String>,
    children: Vec<Element>,
    nsmap: Option<Rc<NamespaceMap>>,
    emit_nsmap: bool,
    text: String,
    tail: String,
}

/// An iterator over children of an element.
pub struct Children<'a> {
    idx: usize,
    element: &'a Element,
}

/// A mutable iterator over children of an element.
pub struct ChildrenMut<'a> {
    iter: ::std::slice::IterMut<'a, Element>,
}

/// An iterator over attributes of an element.
pub struct Attrs<'a> {
    iter: BTreeMapIter<'a, QName<'a>, String>,
}

/// An iterator over matching children.
pub struct FindChildren<'a> {
    tag: Cow<'a, QName<'a>>,
    child_iter: Children<'a>,
}

/// A mutable iterator over matching children.
pub struct FindChildrenMut<'a> {
    tag: Cow<'a, QName<'a>>,
    child_iter: ChildrenMut<'a>,
}

/// Represents a position in the source.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub struct Position {
    line: u64,
    column: u64,
}

impl Position {
    /// Creates a new position.
    pub fn new(line: u64, column: u64) -> Position {
        Position { line, column }
    }

    fn from_xml_position(pos: &dyn XmlPosition) -> Position {
        let pos = pos.position();
        Position::new(pos.row, pos.column)
    }

    /// Returns the line number of the position
    pub fn line(&self) -> u64 {
        self.line
    }
    /// Returns the column of the position
    pub fn column(&self) -> u64 {
        self.column
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

/// Errors that can occur parsing XML
#[derive(Debug)]
pub enum Error {
    /// The XML is invalid
    MalformedXml {
        msg: Cow<'static, str>,
        pos: Position,
    },
    /// An IO Error
    Io(io::Error),
    /// A UTF-8 Error
    Utf8(Utf8Error),
    /// This library is unable to process this XML. This can occur if, for
    /// example, the XML contains processing instructions.
    UnexpectedEvent {
        msg: Cow<'static, str>,
        pos: Position,
    },
    /// A namespace prefix was already used
    DuplicateNamespacePrefix,
}

impl Error {
    /// Returns the position of the error if known
    pub fn position(&self) -> Option<Position> {
        match self {
            Error::MalformedXml { pos, .. } => Some(*pos),
            Error::UnexpectedEvent { pos, .. } => Some(*pos),
            _ => None,
        }
    }

    /// Returns the line number of the error or 0 if unknown
    pub fn line(&self) -> u64 {
        self.position().map(|x| x.line()).unwrap_or(0)
    }

    /// Returns the column of the error or 0 if unknown
    pub fn column(&self) -> u64 {
        self.position().map(|x| x.column()).unwrap_or(0)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::MalformedXml { ref pos, ref msg } => {
                write!(f, "Malformed XML: {} ({})", msg, pos)
            }
            Error::Io(ref e) => write!(f, "{}", e),
            Error::Utf8(ref e) => write!(f, "{}", e),
            Error::UnexpectedEvent { ref msg, .. } => write!(f, "Unexpected XML event: {}", msg),
            Error::DuplicateNamespacePrefix => {
                write!(f, "Encountered duplicated namespace prefix")
            }
        }
    }
}

impl std::error::Error for Error {
    fn cause(&self) -> Option<&dyn std::error::Error> {
        match *self {
            Error::Io(ref e) => Some(e),
            Error::Utf8(ref e) => Some(e),
            _ => None,
        }
    }
}

impl From<XmlReadError> for Error {
    fn from(err: XmlReadError) -> Error {
        match *err.kind() {
            XmlReadErrorKind::Io(ref err) => Error::Io(io::Error::new(err.kind(), err.to_string())),
            XmlReadErrorKind::Utf8(err) => Error::Utf8(err),
            XmlReadErrorKind::UnexpectedEof => Error::Io(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "Encountered unexpected eof",
            )),
            XmlReadErrorKind::Syntax(ref msg) => Error::MalformedXml {
                msg: msg.clone(),
                pos: Position::from_xml_position(&err),
            },
        }
    }
}

impl From<XmlWriteError> for Error {
    fn from(err: XmlWriteError) -> Error {
        match err {
            XmlWriteError::Io(err) => Error::Io(err),
            err => Err(err).unwrap(),
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

impl<'a> Iterator for ChildrenMut<'a> {
    type Item = &'a mut Element;

    fn next(&mut self) -> Option<&'a mut Element> {
        self.iter.next()
    }
}

impl<'a> Iterator for Attrs<'a> {
    type Item = (&'a QName<'a>, &'a str);

    fn next(&mut self) -> Option<(&'a QName<'a>, &'a str)> {
        if let Some((k, v)) = self.iter.next() {
            Some((k, v.as_str()))
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

impl<'a> Iterator for FindChildrenMut<'a> {
    type Item = &'a mut Element;

    fn next(&mut self) -> Option<&'a mut Element> {
        use std::borrow::Borrow;
        let tag: &QName = self.tag.borrow();
        self.child_iter.find(|x| x.tag() == tag)
    }
}

impl Element {
    /// Creates a new element without any children but a given tag.
    ///
    /// This can be used at all times to create a new element however when you
    /// work with namespaces it's recommended to only use this for the root
    /// element and then create further children through `new_with_namespaces`
    /// as otherwise namespaces will not be propagaged downwards properly.
    pub fn new<'a, Q: AsQName<'a>>(tag: Q) -> Element {
        Element::new_with_nsmap(&tag.as_qname(), None)
    }

    /// Creates a new element without any children but inheriting the
    /// namespaces from another element.
    ///
    /// This has the advantage that internally the map will be shared
    /// across elements for as long as no further modifications are
    /// taking place.
    pub fn new_with_namespaces<'a, Q: AsQName<'a>>(tag: Q, reference: &Element) -> Element {
        Element::new_with_nsmap(&tag.as_qname(), reference.nsmap.clone())
    }

    fn new_with_nsmap(tag: &QName<'_>, nsmap: Option<Rc<NamespaceMap>>) -> Element {
        let mut rv = Element {
            tag: tag.share(),
            attributes: BTreeMap::new(),
            nsmap,
            emit_nsmap: false,
            children: vec![],
            text: String::new(),
            tail: String::new(),
        };
        if let Some(url) = tag.ns() {
            let prefix = rv.get_namespace_prefix(url).unwrap_or("").to_string();
            rv.register_namespace(url, Some(&prefix));
        }
        rv
    }

    /// Parses some XML data into an `Element` from a reader.
    pub fn from_reader<R: Read>(r: R) -> Result<Element, Error> {
        let cfg = ParserConfig::new().whitespace_to_characters(true);
        let mut reader = cfg.create_reader(r);
        loop {
            match reader.next() {
                Ok(XmlEvent::StartElement {
                    name,
                    attributes,
                    namespace,
                }) => {
                    return Element::from_start_element(
                        name,
                        attributes,
                        namespace,
                        None,
                        &mut reader,
                    );
                }
                Ok(XmlEvent::Comment(..))
                | Ok(XmlEvent::Whitespace(..))
                | Ok(XmlEvent::StartDocument { .. })
                | Ok(XmlEvent::ProcessingInstruction { .. }) => {
                    continue;
                }
                Ok(_) => {
                    return Err(Error::UnexpectedEvent {
                        msg: Cow::Borrowed("xml construct"),
                        pos: Position::from_xml_position(&reader),
                    })
                }
                Err(e) => return Err(e.into()),
            }
        }
    }

    /// Dump an element as XML document into a writer.
    ///
    /// This will create an XML document with a processing instruction
    /// to start it.  There is currently no API to only serialize a non
    /// standalone element.
    ///
    /// Currently the writer has no way to customize what is generated
    /// in particular there is no support yet for automatically indenting
    /// elements.  The reason for this is that there is no way to ignore
    /// this information automatically in the absence of DTD support which
    /// is not really planned.
    pub fn to_writer<W: Write>(&self, w: W) -> Result<(), Error> {
        self.to_writer_with_options(w, WriteOptions::new())
    }

    /// Dump an element as XML document into a writer with option.
    ///
    /// This will create an XML document with a processing instruction
    /// to start it.  There is currently no API to only serialize a non
    /// standalone element.
    ///
    /// Currently the writer has no way to customize what is generated
    /// in particular there is no support yet for automatically indenting
    /// elements.  The reason for this is that there is no way to ignore
    /// this information automatically in the absence of DTD support which
    /// is not really planned.
    pub fn to_writer_with_options<W: Write>(
        &self,
        w: W,
        options: WriteOptions,
    ) -> Result<(), Error> {
        let mut writer = EmitterConfig::new()
            .write_document_declaration(options.xml_prolog.is_some())
            .create_writer(w);

        if options.xml_prolog.is_some() {
            writer.write(XmlWriteEvent::StartDocument {
                version: match options.xml_prolog.unwrap() {
                    XmlProlog::Version10 => XmlVersion::Version10,
                    XmlProlog::Version11 => XmlVersion::Version11,
                },
                encoding: Some("utf-8"),
                standalone: None,
            })?;
        }

        self.dump_into_writer(&mut writer)
    }

    /// Dump an element as XML document into a string
    pub fn to_string(&self) -> Result<String, Error> {
        let mut out: Vec<u8> = Vec::new();
        self.to_writer(&mut out)?;
        Ok(String::from_utf8(out).unwrap())
    }

    fn get_xml_name<'a>(&'a self, qname: &'a QName<'a>) -> Name<'a> {
        let mut name = Name::local(qname.name());
        if let Some(url) = qname.ns() {
            name.namespace = Some(url);
            if let Some(prefix) = self.get_namespace_prefix(url) {
                if !prefix.is_empty() {
                    name.prefix = Some(prefix);
                }
            }
        }
        name
    }

    fn dump_into_writer<W: Write>(&self, w: &mut EventWriter<W>) -> Result<(), Error> {
        let name = self.get_xml_name(&self.tag);

        let mut attributes = Vec::with_capacity(self.attributes.len());
        for (k, v) in self.attributes.iter() {
            attributes.push(Attribute {
                name: self.get_xml_name(k),
                value: v,
            });
        }

        let mut namespace = XmlNamespaceMap::empty();
        if self.emit_nsmap {
            if let Some(ref nsmap) = self.nsmap {
                for (prefix, url) in &nsmap.prefix_to_ns {
                    namespace.put(prefix.borrow(), url.borrow());
                }
            }
        }

        w.write(XmlWriteEvent::StartElement {
            name,
            attributes: Cow::Owned(attributes),
            namespace: Cow::Owned(namespace),
        })?;

        let text = self.text();
        if !text.is_empty() {
            w.write(XmlWriteEvent::Characters(text))?;
        }

        for elem in &self.children {
            elem.dump_into_writer(w)?;
            let text = elem.tail();
            if !text.is_empty() {
                w.write(XmlWriteEvent::Characters(text))?;
            }
        }

        w.write(XmlWriteEvent::EndElement { name: Some(name) })?;

        Ok(())
    }

    fn from_start_element<R: Read>(
        name: OwnedName,
        attributes: Vec<OwnedAttribute>,
        namespace: XmlNamespaceMap,
        parent_nsmap: Option<Rc<NamespaceMap>>,
        reader: &mut EventReader<R>,
    ) -> Result<Element, Error> {
        let mut root = Element {
            tag: QName::from_owned_name(name),
            attributes: BTreeMap::new(),
            nsmap: parent_nsmap,
            emit_nsmap: false,
            children: vec![],
            text: String::new(),
            tail: String::new(),
        };
        for attr in attributes {
            root.attributes
                .insert(QName::from_owned_name(attr.name), attr.value);
        }

        if !namespace.is_essentially_empty() {
            for (prefix, url) in namespace.0.iter() {
                root.register_namespace(url, Some(prefix));
            }
        };

        root.parse_children(reader)?;
        Ok(root)
    }

    fn parse_children<R: Read>(&mut self, reader: &mut EventReader<R>) -> Result<(), Error> {
        loop {
            match reader.next() {
                Ok(XmlEvent::EndElement { ref name }) => {
                    if name.local_name == self.tag.name()
                        && name.namespace.as_deref() == self.tag.ns()
                    {
                        return Ok(());
                    } else {
                        return Err(Error::UnexpectedEvent {
                            msg: Cow::Owned(format!("Unexpected end element {}", &name.local_name)),
                            pos: Position::from_xml_position(reader),
                        });
                    }
                }
                Ok(XmlEvent::StartElement {
                    name,
                    attributes,
                    namespace,
                }) => {
                    self.children.push(Element::from_start_element(
                        name,
                        attributes,
                        namespace,
                        self.nsmap.clone(),
                        reader,
                    )?);
                }
                Ok(XmlEvent::Characters(s)) => {
                    let child_count = self.children.len();
                    if child_count > 0 {
                        self.children[child_count - 1].tail = s;
                    } else {
                        self.text = s;
                    }
                }
                Ok(XmlEvent::CData(s)) => {
                    self.text = s;
                }
                Ok(XmlEvent::Comment(..))
                | Ok(XmlEvent::Whitespace(..))
                | Ok(XmlEvent::StartDocument { .. })
                | Ok(XmlEvent::ProcessingInstruction { .. }) => {
                    continue;
                }
                Ok(_) => {
                    return Err(Error::UnexpectedEvent {
                        msg: Cow::Borrowed("unknown element"),
                        pos: Position::from_xml_position(reader),
                    })
                }
                Err(e) => {
                    return Err(e.into());
                }
            }
        }
    }

    /// Returns the text of a tag.
    ///
    /// Note that this does not trim or modify whitespace so the return
    /// value might contain structural information from the XML file.
    pub fn text(&self) -> &str {
        &self.text
    }

    /// Sets a new text value for the tag.
    pub fn set_text<S: Into<String>>(&mut self, value: S) -> &mut Element {
        self.text = value.into();
        self
    }

    /// Returns the tail text of a tag.
    ///
    /// The tail is the text following an element.
    pub fn tail(&self) -> &str {
        &self.tail
    }

    /// Sets a new tail text value for the tag.
    pub fn set_tail<S: Into<String>>(&mut self, value: S) -> &mut Element {
        self.tail = value.into();
        self
    }

    /// The tag of the element as qualified name.
    ///
    /// Use the `QName` functionality to extract the information from the
    /// tag name you care about (like the local name).
    pub fn tag(&self) -> &QName {
        &self.tag
    }

    /// Sets a new tag for the element.
    pub fn set_tag<'a>(&mut self, tag: &QName<'a>) -> &mut Element {
        self.tag = tag.share();
        self
    }

    /// Returns the number of children
    pub fn child_count(&self) -> usize {
        self.children.len()
    }

    /// Returns the nth child.
    pub fn get_child(&self, idx: usize) -> Option<&Element> {
        self.children.get(idx)
    }

    /// Returns the nth child as a mutable reference.
    pub fn get_child_mut(&mut self, idx: usize) -> Option<&mut Element> {
        self.children.get_mut(idx)
    }

    /// Removes a child.
    ///
    /// This returns the element if it was removed or None if the
    /// index was out of bounds.
    pub fn remove_child(&mut self, idx: usize) -> Option<Element> {
        if self.children.len() > idx {
            Some(self.children.remove(idx))
        } else {
            None
        }
    }

    /// Appends a new child and returns a reference to self.
    pub fn append_child(&mut self, child: Element) -> &mut Element {
        self.children.push(child);
        self
    }

    /// Appends a new child to the element and returns a reference to it.
    ///
    /// This uses ``Element::new_with_namespaces`` internally and can
    /// then be used like this:
    ///
    /// ```
    /// use elementtree::Element;
    ///
    /// let ns = "http://example.invalid/#ns";
    /// let mut root = Element::new((ns, "mydoc"));
    ///
    /// {
    ///     let mut list = root.append_new_child((ns, "list"));
    ///     for x in 0..3 {
    ///         list.append_new_child((ns, "item")).set_text(format!("Item {}", x));
    ///     }
    /// }
    /// ```
    pub fn append_new_child<'a, Q: AsQName<'a>>(&'a mut self, tag: Q) -> &'a mut Element {
        let child = Element::new_with_namespaces(tag, self);
        self.append_child(child);
        let idx = self.children.len() - 1;
        &mut self.children[idx]
    }

    /// Returns an iterator over all children.
    pub fn children(&self) -> Children<'_> {
        Children {
            idx: 0,
            element: self,
        }
    }

    /// Returns a mutable iterator over all children.
    pub fn children_mut(&mut self) -> ChildrenMut<'_> {
        ChildrenMut {
            iter: self.children.iter_mut(),
        }
    }

    /// Returns all children with the given name.
    pub fn find_all<'a, Q: AsQName<'a>>(&'a self, tag: Q) -> FindChildren<'a> {
        FindChildren {
            tag: tag.as_qname(),
            child_iter: self.children(),
        }
    }

    /// Returns all children with the given name.
    pub fn find_all_mut<'a, Q: AsQName<'a>>(&'a mut self, tag: Q) -> FindChildrenMut<'a> {
        FindChildrenMut {
            tag: tag.as_qname(),
            child_iter: self.children_mut(),
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

    /// Finds the first matching child and returns a mut ref
    pub fn find_mut<'a, Q: AsQName<'a>>(&'a mut self, tag: Q) -> Option<&'a mut Element> {
        self.find_all_mut(tag).next()
    }

    /// Look up an attribute by qualified name.
    pub fn get_attr<'a, Q: AsQName<'a>>(&'a self, name: Q) -> Option<&'a str> {
        self.attributes.get(&name.as_qname()).map(|x| x.as_str())
    }

    /// Sets a new attribute.
    ///
    /// This returns a reference to the element so you can chain the calls.
    pub fn set_attr<'a, Q: AsQName<'a>, S: Into<String>>(
        &'a mut self,
        name: Q,
        value: S,
    ) -> &'a mut Element {
        self.attributes
            .insert(name.as_qname().share(), value.into());
        self
    }

    /// Removes an attribute and returns the stored string.
    pub fn remove_attr<'a, Q: AsQName<'a>>(&'a mut self, name: Q) -> Option<String> {
        // so this requires some explanation.  We store internally QName<'static>
        // which means the QName has a global lifetime.  This works because we
        // move the internal string storage into a global string cache or we are
        // pointing to static memory in the binary.
        //
        // However while Rust can coerce our BTreeMap from QName<'static> to
        // QName<'a> when reading, we can't do the same when writing.  This is
        // to prevent us from stashing a QName<'a> into the btreemap.  However on
        // remove that restriction makes no sense so we can unsafely transmute it
        // away.  I wish there was a better way though.
        use std::borrow::Borrow;
        let name = name.as_qname();
        let name_ref: &QName<'a> = name.borrow();
        let name_ref_static: &QName<'static> = unsafe { mem::transmute(name_ref) };
        self.attributes.remove(name_ref_static)
    }

    /// Returns an iterator over all attributes
    pub fn attrs(&self) -> Attrs<'_> {
        Attrs {
            iter: self.attributes.iter(),
        }
    }

    /// Count the attributes
    pub fn attr_count(&self) -> usize {
        self.attributes.len()
    }

    fn get_nsmap_mut(&mut self) -> &mut NamespaceMap {
        let new_map = match self.nsmap {
            Some(ref mut nsmap) if Rc::strong_count(nsmap) == 1 => None,
            Some(ref mut nsmap) => Some(Rc::new((**nsmap).clone())),
            None => Some(Rc::new(NamespaceMap::new())),
        };
        if let Some(nsmap) = new_map {
            self.nsmap = Some(nsmap);
        }
        Rc::get_mut(self.nsmap.as_mut().unwrap()).unwrap()
    }

    /// Registers a namespace with the internal namespace map.
    ///
    /// Note that there is no API to remove namespaces from an element once
    /// the namespace has been set so be careful with modifying this!
    ///
    /// This optionally also registers a specific prefix however if that prefix
    /// is already used a random one is used instead.
    pub fn register_namespace(&mut self, url: &str, prefix: Option<&str>) {
        if self.get_namespace_prefix(url).is_none()
            && self.get_nsmap_mut().register_if_missing(url, prefix)
        {
            self.emit_nsmap = true;
        }
    }

    /// Sets a specific namespace prefix.  This will also register the
    /// namespace if it was unknown so far.
    ///
    /// In case a prefix is set that is already set elsewhere an error is
    /// returned.  It's recommended that this method is only used on the
    /// root node before other prefixes are added.
    pub fn set_namespace_prefix(&mut self, url: &str, prefix: &str) -> Result<(), Error> {
        if self.get_namespace_prefix(url) == Some(prefix) {
            Ok(())
        } else {
            self.get_nsmap_mut().set_prefix(url, prefix)
        }
    }

    /// Returns the assigned prefix for a namespace.
    pub fn get_namespace_prefix(&self, url: &str) -> Option<&str> {
        match url {
            NS_EMPTY_URI => Some(""),
            NS_XML_URI => Some("xml"),
            NS_XMLNS_URI => Some("xmlns"),
            _ => {
                if let Some(ref nsmap) = self.nsmap {
                    nsmap.get_prefix(url)
                } else {
                    None
                }
            }
        }
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

/// Xml Prolog version handle by elementtree
pub enum XmlProlog {
    Version10,
    Version11,
}

/// A struct that define write options.
pub struct WriteOptions {
    xml_prolog: Option<XmlProlog>,
}

impl Default for WriteOptions {
    fn default() -> WriteOptions {
        WriteOptions {
            xml_prolog: Some(XmlProlog::Version10),
        }
    }
}

impl WriteOptions {
    pub fn new() -> WriteOptions {
        WriteOptions {
            ..WriteOptions::default()
        }
    }

    /// Define which xml prolog will be displayed when rendering an Element.
    ///
    /// Note that prolog is optional, an XML document with a missing prolog is well-formed but not valid.
    ///
    /// See RFC: [W3C XML 26 November 2008](https://www.w3.org/TR/xml/#sec-prolog-dtd)
    pub fn set_xml_prolog(mut self, prolog: Option<XmlProlog>) -> Self {
        self.xml_prolog = prolog;

        self
    }
}
