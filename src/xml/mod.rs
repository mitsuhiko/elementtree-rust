//! This module currently provides an almost XML 1.0/1.1-compliant pull parser.

pub use crate::xml::reader::EventReader;
pub use crate::xml::reader::ParserConfig;
pub use crate::xml::writer::EmitterConfig;
pub use crate::xml::writer::EventWriter;

pub mod attribute;
pub mod common;
pub mod escape;
pub mod macros;
pub mod name;
pub mod namespace;
pub mod reader;
mod util;
pub mod writer;

#[cfg(test)]
mod tests;
