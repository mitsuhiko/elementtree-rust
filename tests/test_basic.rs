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

#[test]
fn test_attributes() {
    let root = Element::from_reader(r#"<?xml version="1.0"?>
    <root>
        <list a="1" b="2" c="3">
            <item attr="foo1"/>
            <item attr="foo2"/>
            <item attr="foo3"/>
        </list>
    </root>
    "#.as_bytes()).unwrap();

    let list = root.find("list").unwrap();
    assert_eq!(list.tag(), &QName::from("list"));

    let items: Vec<_> = list.children().map(|x| x.get_attr("attr").unwrap_or("")).collect();
    assert_eq!(items.as_slice(), &["foo1", "foo2", "foo3"]);

    let mut attrs: Vec<_> = list.attrs().map(|(k, v)| format!("{}={}", k, v)).collect();
    attrs.sort();
    assert_eq!(attrs.iter().map(|x| x.as_str()).collect::<Vec<_>>(), vec!["a=1", "b=2", "c=3"]);

    assert_eq!(list.attr_count(), 3);
}

#[test]
fn test_namespaces() {
    let root = Element::from_reader(r#"<?xml version="1.0"?>
    <root xmlns="root" xmlns:foo="child">
        <list a="1" b="2" c="3">
            <item foo:attr="foo1"/>
            <item foo:attr="foo2"/>
            <item foo:attr="foo3"/>
        </list>
    </root>
    "#.as_bytes()).unwrap();

    let list = root.find("{root}list").unwrap();
    assert_eq!(list.tag(), &QName::from("{root}list"));

    let items: Vec<_> = list.children().map(|x| x.get_attr("{child}attr").unwrap_or("")).collect();
    assert_eq!(items.as_slice(), &["foo1", "foo2", "foo3"]);

    let mut attrs: Vec<_> = list.attrs().map(|(k, v)| format!("{}={}", k, v)).collect();
    attrs.sort();
    assert_eq!(attrs.iter().map(|x| x.as_str()).collect::<Vec<_>>(), vec!["a=1", "b=2", "c=3"]);

    assert_eq!(list.attr_count(), 3);
}
