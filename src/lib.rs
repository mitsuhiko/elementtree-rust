//! A simple library for parsing an XML file into an in-memory tree structure
//!
//! Not recommended for large XML files, as it will load the entire file into memory.
//!
//! ## Installation
//!
//! ```ini
//! [dependencies]
//! elementtree = "0"
//! ```
//!
//! ## Usage
//!
//! ```rust
//! use elementtree::Element;
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
extern crate xml;
extern crate string_cache;

use std::collections::{HashMap, BTreeMap};
use std::collections::hash_map::Iter as HashMapIter;
use std::io::{Read, Write};
use std::io;
use std::fmt;
use std::mem;
use std::ops::Deref;
use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use std::borrow::Cow;
use std::rc::Rc;

use string_cache::DefaultAtom as Atom;

use xml::reader::{EventReader, XmlEvent};
use xml::writer::{EventWriter, XmlEvent as XmlWriteEvent, Error as XmlWriteError};
use xml::common::XmlVersion;
use xml::attribute::{Attribute, OwnedAttribute};
use xml::name::{Name, OwnedName};
use xml::namespace::{Namespace as XmlNamespaceMap, NS_EMPTY_URI,
                     NS_XMLNS_URI, NS_XML_URI};

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
    pub fn share(&self) -> QName<'static> {
        QName {
            name: XmlAtom::Shared(Atom::from(self.name.borrow())),
            ns: self.ns.as_ref().map(|x| XmlAtom::Shared(Atom::from(x.borrow()))),
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
            if !self.prefix_to_ns.contains_key(&XmlAtom::Borrowed(&random_prefix)) {
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
#[derive(Debug, Clone)]
pub struct Element {
    tag: QName<'static>,
    attributes: HashMap<QName<'static>, String>,
    children: Vec<Element>,
    nsmap: Option<Rc<NamespaceMap>>,
    emit_nsmap: bool,
    text: Option<String>,
    tail: Option<String>,
}

/// An iterator over children of an element.
pub struct Children<'a> {
    idx: usize,
    element: &'a Element,
}

/// An iterator over attributes of an element.
pub struct Attrs<'a> {
    iter: HashMapIter<'a, QName<'a>, String>,
}

/// An iterator over matching children.
pub struct FindChildren<'a> {
    tag: Cow<'a, QName<'a>>,
    child_iter: Children<'a>,
}

/// Errors that can occur parsing XML
#[derive(Debug)]
pub enum Error {
    /// The XML is invalid
    MalformedXml(xml::reader::Error),
    /// An IO Error
    Io(io::Error),
    /// This library is unable to process this XML. This can occur if, for
    /// example, the XML contains processing instructions.
    UnexpectedEvent,
    /// A namespace prefix was already used
    DuplicateNamespacePrefix,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Error::MalformedXml(ref e) => write!(f, "Malformed XML. {}", e),
            &Error::Io(ref e) => write!(f, "{}", e),
            &Error::UnexpectedEvent => write!(f, "Unexpected XML event"),
            &Error::DuplicateNamespacePrefix => write!(
                f, "Encountered duplicated namespace prefix"),
        }
    }
}

impl std::error::Error for Error {
    fn description(&self) -> &str {
        match self {
            &Error::MalformedXml(..) => "Malformed XML",
            &Error::Io(..) => "IO error",
            &Error::UnexpectedEvent => "Unexpected XML element",
            &Error::DuplicateNamespacePrefix => "Duplicated namespace prefix",
        }
    }

    fn cause(&self) -> Option<&std::error::Error> {
        match self {
            &Error::MalformedXml(ref e) => Some(e),
            &Error::Io(ref e) => Some(e),
            _ => None,
        }
    }
}

impl From<XmlWriteError> for Error {
    fn from(err: XmlWriteError) -> Error {
        match err {
            XmlWriteError::Io(err) => Error::Io(err),
            err => { return Err(err).unwrap(); }
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

impl Element {
    /// Creates a new element without any children but a given tag.
    pub fn new<'a>(tag: &QName<'a>) -> Element {
        Element::new_with_nsmap(tag, None)
    }

    /// Creates a new element without any children but inheriting the
    /// namespaces from another element.
    ///
    /// This has the advantage that internally the map will be shared
    /// across elements for as long as no further modifications are
    /// taking place.
    pub fn new_with_namespaces<'a>(tag: &QName<'a>, reference: &Element) -> Element {
        Element::new_with_nsmap(tag, reference.nsmap.clone())
    }

    fn new_with_nsmap<'a>(tag: &QName<'a>, nsmap: Option<Rc<NamespaceMap>>) -> Element {
        let mut rv = Element {
            tag: tag.share(),
            attributes: HashMap::new(),
            nsmap: nsmap,
            emit_nsmap: false,
            children: vec![],
            text: None,
            tail: None,
        };
        if let Some(url) = tag.ns() {
            let prefix = rv.get_namespace_prefix(url).unwrap_or("").to_string();
            rv.register_namespace(url, Some(&prefix));
        }
        rv
    }

    /// Parses some data into an Element
    pub fn from_reader<R: Read>(r: R) -> Result<Element, Error> {
        let mut reader = EventReader::new(r);
        loop {
            match reader.next() {
                Ok(XmlEvent::StartElement { name, attributes, namespace }) => {
                    return Element::from_start_element(
                        name, attributes, namespace, None, &mut reader);
                }
                Ok(XmlEvent::Comment(..)) |
                Ok(XmlEvent::Whitespace(..)) |
                Ok(XmlEvent::StartDocument { .. }) |
                Ok(XmlEvent::ProcessingInstruction { .. }) => {
                    continue;
                }
                Ok(_) => return Err(Error::UnexpectedEvent),
                Err(e) => return Err(Error::MalformedXml(e)),
            }
        }
    }

    /// Dump an element as XML document into a writer
    pub fn to_writer<W: Write>(&self, w: W) -> Result<(), Error> {
        let mut writer = EventWriter::new(w);
        writer.write(XmlWriteEvent::StartDocument {
            version: XmlVersion::Version10,
            encoding: Some("utf-8"),
            standalone: None,
        })?;
        self.dump_into_writer(&mut writer)
    }

    fn dump_into_writer<W: Write>(&self, w: &mut EventWriter<W>) -> Result<(), Error> {
        let mut name = Name::local(self.tag.name());
        if let Some(url) = self.tag.ns() {
            name.namespace = Some(url);
            name.prefix = self.get_namespace_prefix(url);
        }

        let mut attributes = Vec::with_capacity(self.attributes.len());
        for (k, v) in self.attributes.iter() {
            let attr_name = Name::local(k.name());
            if let Some(url) = k.ns() {
                name.namespace = Some(url);
                name.prefix = self.get_namespace_prefix(url);
            }
            attributes.push(Attribute {
                name: attr_name,
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
            name: name,
            attributes: Cow::Owned(attributes),
            namespace: Cow::Owned(namespace),
        })?;

        for elem in &self.children {
            elem.dump_into_writer(w)?;
        }

        w.write(XmlWriteEvent::EndElement {
            name: Some(name),
        })?;

        Ok(())
    }

    fn from_start_element<R: Read>(name: OwnedName,
                                   attributes: Vec<OwnedAttribute>,
                                   namespace: XmlNamespaceMap,
                                   parent_nsmap: Option<Rc<NamespaceMap>>,
                                   reader: &mut EventReader<R>)
        -> Result<Element, Error>
    {
        let mut root = Element {
            tag: QName::from_owned_name(name),
            attributes: HashMap::new(),
            nsmap: parent_nsmap,
            emit_nsmap: false,
            children: vec![],
            text: None,
            tail: None,
        };
        for attr in attributes {
            root.attributes.insert(QName::from_owned_name(attr.name), attr.value);
        }

        if !namespace.is_essentially_empty() {
            for (prefix, url) in namespace.0.iter() {
                root.register_namespace(url, Some(prefix));
            }
        };

        root.parse_children(reader)?;
        Ok(root)
    }

    fn parse_children<R: Read>(&mut self,
                               reader: &mut EventReader<R>)
        -> Result<(), Error>
    {
        loop {
            match reader.next() {
                Ok(XmlEvent::EndElement { ref name }) => {
                    if &name.local_name == self.tag.name() &&
                       name.namespace.as_ref().map(|x| x.as_str()) == self.tag.ns() {
                        return Ok(());
                    } else {
                        return Err(Error::UnexpectedEvent);
                    }
                }
                Ok(XmlEvent::StartElement { name, attributes, namespace }) => {
                    self.children.push(Element::from_start_element(
                        name, attributes, namespace, self.nsmap.clone(), reader)?);
                }
                Ok(XmlEvent::Characters(s)) => {
                    let child_count = self.children.len();
                    if child_count > 0 {
                        self.children[child_count - 1].tail = Some(s);
                    } else {
                        self.text = Some(s);
                    }
                }
                Ok(XmlEvent::CData(s)) => {
                    self.text = Some(s);
                }
                Ok(XmlEvent::Comment(..)) |
                Ok(XmlEvent::Whitespace(..)) |
                Ok(XmlEvent::StartDocument { .. }) |
                Ok(XmlEvent::ProcessingInstruction { .. }) => {
                    continue;
                }
                Ok(_) => {
                    return Err(Error::UnexpectedEvent);
                }
                Err(e) => {
                    return Err(Error::MalformedXml(e));
                }
            }
        }
    }

    /// Returns the text of a tag
    pub fn text(&self) -> &str {
        self.text.as_ref().map(|x| x.as_str()).unwrap_or("")
    }

    /// Sets a new text value for the tag.
    pub fn set_text(&mut self, value: &str) {
        if value.is_empty() {
            self.text = None;
        } else {
            self.text = Some(value.to_string());
        }
    }

    /// Returns the tail text of a tag
    pub fn tail(&self) -> &str {
        self.tail.as_ref().map(|x| x.as_str()).unwrap_or("")
    }

    /// Sets a new tail text value for the tag.
    pub fn set_tail(&mut self, value: &str) {
        if value.is_empty() {
            self.tail = None;
        } else {
            self.tail = Some(value.to_string());
        }
    }

    /// The tag of the element as qualified name
    pub fn tag(&self) -> &QName {
        &self.tag
    }

    /// Sets a new tag for the element.
    pub fn set_tag<'a>(&mut self, tag: &QName<'a>) {
        self.tag = tag.share();
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

    /// Appends a new child
    pub fn append_child(&mut self, child: Element) {
        self.children.push(child);
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

    /// Sets a new attribute.
    pub fn set_attr<'a, Q: AsQName<'a>, S: AsRef<str>>(&'a mut self, name: Q, value: S) {
        self.attributes.insert(name.as_qname().share(), value.as_ref().to_string());
    }

    /// Removes an attribute and returns the stored string.
    pub fn remove_attr<'a, Q: AsQName<'a>>(&'a mut self, name: Q) -> Option<String> {
        // so this requires some explanation.  We store internally QName<'static>
        // which means the QName has a global lifetime.  This works because we
        // move the internal string storage into a global string cache or we are
        // pointing to static memory in the binary.
        //
        // However while Rust can coerce our HashMap from QName<'static> to
        // QName<'a> when reading, we can't do the same when writing.  This is
        // to prevent us from stashing a QName<'a> into the hashmap.  However on
        // remove that restriction makes no sense so we can unsafely transmute it
        // away.  I wish there was a better way though.
        use std::borrow::Borrow;
        let name = name.as_qname();
        let name_ref: &QName<'a> = name.borrow();
        let name_ref_static: &QName<'static> = unsafe { mem::transmute(name_ref) };
        self.attributes.remove(name_ref_static)
    }

    /// Returns an iterator over all attributes
    pub fn attrs<'a>(&'a self) -> Attrs<'a> {
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
        if self.get_namespace_prefix(url).is_none() {
            if self.get_nsmap_mut().register_if_missing(url, prefix) {
                self.emit_nsmap = true;
            }
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
