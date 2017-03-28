extern crate elementtree;

use elementtree::{Element, QName};


#[test]
fn test_basics() {
    let root = Element::from_reader(r#"<?xml version="1.0"?>
    <root>
        <list>
            <item>Item 1</item>Tail 1
            <item>Item 2</item>Tail 2
            <item>Item 3</item>Tail 3
        </list>
    </root>
    "#.as_bytes()).unwrap();

    let list = root.find("list").unwrap();
    assert_eq!(list.tag(), &QName::from("list"));

    let items: Vec<_> = list.children().map(|x| x.text()).collect();
    assert_eq!(items.as_slice(), &["Item 1", "Item 2", "Item 3"]);

    let tails: Vec<_> = list.children().map(|x| x.tail().trim()).collect();
    assert_eq!(tails.as_slice(), &["Tail 1", "Tail 2", "Tail 3"]);
}
