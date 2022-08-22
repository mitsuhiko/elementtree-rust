//! Contains emitter configuration structure.

use std::borrow::Cow;
use std::io::Write;

use crate::xml::writer::EventWriter;

/// Emitter configuration structure.
///
/// This structure contains various options which control XML document emitter behavior.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct EmitterConfig {
    pub line_separator: Cow<'static, str>,
    pub indent_string: Cow<'static, str>,
    pub perform_indent: bool,

    pub normalize_empty_elements: bool,
    pub cdata_to_characters: bool,
    pub autopad_comments: bool,
    pub pad_self_closing: bool,

    /// Whether or not to write XML document declaration at the beginning of a document.
    /// Default is true.
    ///
    /// This option controls whether the document declaration should be emitted automatically
    /// before a root element is written if it was not emitted explicitly by the user.
    pub write_document_declaration: bool,
}

impl EmitterConfig {
    /// Creates an emitter configuration with default values.
    #[inline]
    pub fn new() -> EmitterConfig {
        EmitterConfig {
            line_separator: "\n".into(),
            indent_string: "  ".into(), // two spaces
            perform_indent: false,
            write_document_declaration: true,
            normalize_empty_elements: true,
            cdata_to_characters: false,
            autopad_comments: true,
            pad_self_closing: true,
        }
    }

    /// Creates an XML writer with this configuration.
    #[inline]
    pub fn create_writer<W: Write>(self, sink: W) -> EventWriter<W> {
        EventWriter::new_with_config(sink, self)
    }
}

impl Default for EmitterConfig {
    #[inline]
    fn default() -> EmitterConfig {
        EmitterConfig::new()
    }
}

gen_setters!(EmitterConfig,
    line_separator: into Cow<'static, str>,
    indent_string: into Cow<'static, str>,
    perform_indent: val bool,
    write_document_declaration: val bool,
    normalize_empty_elements: val bool,
    cdata_to_characters: val bool,
    autopad_comments: val bool,
    pad_self_closing: val bool
);
