ElementTree for Rust
====================

[API Documentation](https://docs.rs/elementtree/)

This library parses XML into a Python ElementTree like structure.  It currently
has basic support for reading and writing with pretty good namespace support and the
ability to inspect the file.

It's not recommended to use this for larger documents as the entire document
will be loaded into memory.  However it's pretty good for working with configuration
files and similar things.

## Installation

Add this to your `Cargo.toml`

```toml
[dependencies]
elementtree = "0"
```


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
