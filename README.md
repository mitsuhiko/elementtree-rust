# ElementTree for Rust

[![Build Status](https://github.com/mitsuhiko/elementtree-rust/workflows/Tests/badge.svg?branch=master)](https://github.com/mitsuhiko/elementtree-rust/actions?query=workflow%3ATests)
[![Crates.io](https://img.shields.io/crates/d/elementtree.svg)](https://crates.io/crates/elementtree)
[![License](https://img.shields.io/github/license/mitsuhiko/elementtree-rust)](https://github.com/mitsuhiko/elementtree-rust/blob/master/LICENSE)
[![rustc 1.42.0](https://img.shields.io/badge/rust-1.42%2B-orange.svg)](https://img.shields.io/badge/rust-1.42%2B-orange.svg)
[![Documentation](https://docs.rs/elementtree/badge.svg)](https://docs.rs/elementtree)

This library parses XML into a Python ElementTree like structure.  It currently
has basic support for reading and writing with pretty good namespace support and the
ability to inspect the file.

It's not recommended to use this for larger documents as the entire document
will be loaded into memory.  However it's pretty good for working with configuration
files and similar things.

## Example

```rust
let root = Element::from_reader(r#"<?xml version="1.0"?>
<root xmlns="tag:myns" xmlns:foo="tag:otherns">
    <list a="1" b="2" c="3">
        <item foo:attr="foo1"/>
        <item foo:attr="foo2"/>
        <item foo:attr="foo3"/>
    </list>
</root>
"#.as_bytes()).unwrap();
let list = root.find("{tag:myns}list").unwrap();
for child in list.find_all("{tag:myns}item") {
    println!("attribute: {}", child.get_attr("{tag:otherns}attr").unwrap());
}
```

## License and Links

- [Documentation](https://docs.rs/elementtree/)
- [Issue Tracker](https://github.com/mitsuhiko/elementtree/issues)
- [Examples](https://github.com/mitsuhiko/elementtree/tree/master/examples)
- License: [BSD 3-Clause](https://github.com/mitsuhiko/elementtree/blob/master/LICENSE)