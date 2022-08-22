//! Contains high-level interface for a pull-based XML parser.
//!
//! The most important type in this module is `EventReader`, which provides an iterator
//! view for events in XML document.

use std::io::Read;
use std::result;

use crate::xml::common::{Position, TextPosition};

pub use self::config::ParserConfig;
pub use self::events::XmlEvent;

use self::parser::PullParser;

mod config;
mod events;
mod lexer;
mod parser;

mod error;
pub use self::error::{Error, ErrorKind};

/// A result type yielded by `XmlReader`.
pub type Outcome<T> = result::Result<T, Error>;

/// A wrapper around an `std::io::Read` instance which provides pull-based XML parsing.
pub struct EventReader<R: Read> {
    source: R,
    parser: PullParser,
}

impl<R: Read> EventReader<R> {
    /// Creates a new reader, consuming the given stream.
    #[inline]
    #[cfg(test)]
    pub fn new(source: R) -> EventReader<R> {
        EventReader::new_with_config(source, ParserConfig::new())
    }

    /// Creates a new reader with the provded configuration, consuming the given stream.
    #[inline]
    pub fn new_with_config(source: R, config: ParserConfig) -> EventReader<R> {
        EventReader {
            source,
            parser: PullParser::new(config),
        }
    }

    /// Pulls and returns next XML event from the stream.
    ///
    /// If returned event is `XmlEvent::Error` or `XmlEvent::EndDocument`, then
    /// further calls to this method will return this event again.
    #[inline]
    pub fn next_event(&mut self) -> Outcome<XmlEvent> {
        self.parser.next(&mut self.source)
    }
}

impl<B: Read> Position for EventReader<B> {
    /// Returns the position of the last event produced by the reader.
    #[inline]
    fn position(&self) -> TextPosition {
        self.parser.position()
    }
}

impl<R: Read> IntoIterator for EventReader<R> {
    type Item = Outcome<XmlEvent>;
    type IntoIter = Events<R>;

    fn into_iter(self) -> Events<R> {
        Events {
            reader: self,
            finished: false,
        }
    }
}

/// An iterator over XML events created from some type implementing `Read`.
///
/// When the next event is `xml::event::Error` or `xml::event::EndDocument`, then
/// it will be returned by the iterator once, and then it will stop producing events.
pub struct Events<R: Read> {
    reader: EventReader<R>,
    finished: bool,
}

impl<R: Read> Events<R> {
    #[cfg(test)]
    pub fn source_mut(&mut self) -> &mut R {
        &mut self.reader.source
    }
}

impl<R: Read> Iterator for Events<R> {
    type Item = Outcome<XmlEvent>;

    #[inline]
    fn next(&mut self) -> Option<Outcome<XmlEvent>> {
        if self.finished {
            None
        } else {
            let ev = self.reader.next_event();
            match ev {
                Ok(XmlEvent::EndDocument) | Err(_) => self.finished = true,
                _ => {}
            }
            Some(ev)
        }
    }
}
