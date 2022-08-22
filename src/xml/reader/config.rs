//! Contains parser configuration structure.
use std::collections::HashMap;
use std::io::Read;

use crate::xml::reader::EventReader;

/// Parser configuration structure.
///
/// This structure contains various configuration options which affect
/// behavior of the parser.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ParserConfig {
    /// A map of extra entities recognized by the parser. Default is an empty map.
    ///
    /// By default the XML parser recognizes the entities defined in the XML spec. Sometimes,
    /// however, it is convenient to make the parser recognize additional entities which
    /// are also not available through the DTD definitions (especially given that at the moment
    /// DTD parsing is not supported).
    pub extra_entities: HashMap<String, String>,
}

impl ParserConfig {
    /// Returns a new config with default values.
    pub fn new() -> ParserConfig {
        ParserConfig {
            extra_entities: HashMap::new(),
        }
    }

    /// Creates an XML reader with this configuration.
    ///
    /// This method is exactly equivalent to calling `EventReader::new_with_config()` with
    /// this configuration object.
    #[inline]
    pub fn create_reader<R: Read>(self, source: R) -> EventReader<R> {
        EventReader::new_with_config(source, self)
    }

    /// Adds a new entity mapping and returns an updated config object.
    #[cfg(test)]
    pub fn add_entity<S: Into<String>, T: Into<String>>(
        mut self,
        entity: S,
        value: T,
    ) -> ParserConfig {
        self.extra_entities.insert(entity.into(), value.into());
        self
    }
}

impl Default for ParserConfig {
    #[inline]
    fn default() -> ParserConfig {
        ParserConfig::new()
    }
}
