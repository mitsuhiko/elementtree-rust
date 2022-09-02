use elementtree::{Element, QName, WriteOptions, XmlProlog};
use std::str;

#[test]
fn test_basics() {
    let root = Element::from_reader(
        r#"<?xml version="1.0"?>
    <root>
        <list>
            <item>Item 1</item>Tail 1
            <item>Item 2</item>Tail 2
            <item>Item 3</item>Tail 3
        </list>
    </root>
    "#
        .as_bytes(),
    )
    .unwrap();

    let list = root.find("list").unwrap();
    assert_eq!(list.tag(), &QName::from("list"));

    let items: Vec<_> = list.children().map(|x| x.text()).collect();
    assert_eq!(items.as_slice(), &["Item 1", "Item 2", "Item 3"]);

    let tails: Vec<_> = list.children().map(|x| x.tail().trim()).collect();
    assert_eq!(tails.as_slice(), &["Tail 1", "Tail 2", "Tail 3"]);
}

#[test]
fn test_attributes() {
    let root = Element::from_reader(
        r#"<?xml version="1.0"?>
    <root>
        <list a="1" b="2" c="3">
            <item attr="foo1"/>
            <item attr="foo2"/>
            <item attr="foo3"/>
        </list>
    </root>
    "#
        .as_bytes(),
    )
    .unwrap();

    let list = root.find("list").unwrap();
    assert_eq!(list.tag(), &QName::from("list"));

    let items: Vec<_> = list
        .children()
        .map(|x| x.get_attr("attr").unwrap_or(""))
        .collect();
    assert_eq!(items.as_slice(), &["foo1", "foo2", "foo3"]);

    let mut attrs: Vec<_> = list.attrs().map(|(k, v)| format!("{}={}", k, v)).collect();
    attrs.sort();
    assert_eq!(
        attrs.iter().map(|x| x.as_str()).collect::<Vec<_>>(),
        vec!["a=1", "b=2", "c=3"]
    );

    assert_eq!(list.attr_count(), 3);
}

#[test]
fn test_namespaces() {
    let root = Element::from_reader(
        r#"<?xml version="1.0"?>
    <root xmlns="root" xmlns:foo="child">
        <list a="1" b="2" c="3">
            <item foo:attr="foo1"/>
            <item foo:attr="foo2"/>
            <item foo:attr="foo3"/>
        </list>
    </root>
    "#
        .as_bytes(),
    )
    .unwrap();

    let list = root.find("{root}list").unwrap();
    assert_eq!(list.tag(), &QName::from("{root}list"));

    let items: Vec<_> = list
        .children()
        .map(|x| x.get_attr("{child}attr").unwrap_or(""))
        .collect();
    assert_eq!(items.as_slice(), &["foo1", "foo2", "foo3"]);

    let mut attrs: Vec<_> = list.attrs().map(|(k, v)| format!("{}={}", k, v)).collect();
    attrs.sort();
    assert_eq!(
        attrs.iter().map(|x| x.as_str()).collect::<Vec<_>>(),
        vec!["a=1", "b=2", "c=3"]
    );

    assert_eq!(list.attr_count(), 3);

    assert_eq!(
        root.get_namespace_prefix("http://www.w3.org/2000/xmlns/"),
        Some("xmlns")
    );

    assert_eq!(root.get_namespace_prefix("child"), Some("foo"));
    assert_eq!(root.get_namespace_prefix("root"), Some(""));
    assert_eq!(root.get_namespace_prefix("missing"), None);

    assert_eq!(list.get_namespace_prefix("child"), Some("foo"));
    assert_eq!(list.get_namespace_prefix("root"), Some(""));
    assert_eq!(list.get_namespace_prefix("missing"), None);
}

#[test]
fn test_entities() {
    let root = Element::from_reader(
        r#"<?xml version="1.0"?>
    <root>&#83;</root>
    "#
        .as_bytes(),
    )
    .unwrap();

    assert_eq!(root.text(), "S");
}

#[test]
fn test_write_stuff() {
    let mut root = Element::new(&QName::from("{myns}root"));
    root.set_namespace_prefix("myns", "x").unwrap();
    let mut out: Vec<u8> = Vec::new();
    root.to_writer(&mut out).unwrap();
    let out = String::from_utf8(out).unwrap();
    assert_eq!(
        &out,
        "<?xml version=\"1.0\" encoding=\"utf-8\"?><x:root xmlns:x=\"myns\" />"
    );
}

#[test]
fn test_basic_creation() {
    let mut root = Element::new("{demo}mydoc");
    root.set_namespace_prefix("demo", "").unwrap();

    let mut list = Element::new_with_namespaces("{demo}list", &root);

    for x in 0..3 {
        let mut child = Element::new_with_namespaces("{demo}item", &root);
        child.set_text(format!("Item {}", x));
        list.append_child(child);
    }

    root.append_child(list);
    assert_eq!(
        &root.to_string().unwrap(),
        "\
        <?xml version=\"1.0\" encoding=\"utf-8\"?>\
        <mydoc xmlns=\"demo\">\
            <list>\
                <item>Item 0</item>\
                <item>Item 1</item>\
                <item>Item 2</item>\
            </list>\
        </mydoc>"
    );
}

#[test]
fn test_alternative_creation() {
    let mut root = Element::new("{demo}mydoc");
    root.set_namespace_prefix("demo", "").unwrap();

    {
        let list = root.append_new_child("{demo}list");
        for x in 0..3 {
            let child = list.append_new_child("{demo}item");
            child.set_text(format!("Item {}", x));
        }
    }

    assert_eq!(
        &root.to_string().unwrap(),
        "\
        <?xml version=\"1.0\" encoding=\"utf-8\"?>\
        <mydoc xmlns=\"demo\">\
            <list>\
                <item>Item 0</item>\
                <item>Item 1</item>\
                <item>Item 2</item>\
            </list>\
        </mydoc>"
    );
}

#[test]
fn test_whitespace() {
    let root = Element::from_reader(
        r#"<?xml version="1.0"?>
    <root>
        <list>
            <item> Item 1 </item>Tail 1
            <item> Item 2 </item>Tail 2
            <item> Item 3 </item>Tail 3
        </list>
    </root>
    "#
        .as_bytes(),
    )
    .unwrap();

    assert_eq!(root.text(), "\n        ");

    let list = root.find("list").unwrap();
    assert_eq!(list.tag(), &QName::from("list"));

    let items: Vec<_> = list.children().map(|x| x.text()).collect();
    assert_eq!(items.as_slice(), &[" Item 1 ", " Item 2 ", " Item 3 "]);
}

#[test]
fn test_creation_without_xml_declaration() {
    let mut root = Element::new("{demo}mydoc");
    root.set_namespace_prefix("demo", "").unwrap();
    {
        let list = root.append_new_child("{demo}list");
        for x in 0..3 {
            let child = list.append_new_child("{demo}item");
            child.set_text(format!("Item {}", x));
        }
    }

    let mut out: Vec<u8> = Vec::new();
    let options = WriteOptions::new().set_xml_prolog(None);

    root.to_writer_with_options(&mut out, options).unwrap();
    assert_eq!(
        str::from_utf8(&out).unwrap(),
        "\
        <mydoc xmlns=\"demo\">\
            <list>\
                <item>Item 0</item>\
                <item>Item 1</item>\
                <item>Item 2</item>\
            </list>\
        </mydoc>"
    );
}

#[test]
fn test_creation_with_xml_prolog_10() {
    let mut root = Element::new("{demo}mydoc");
    root.set_namespace_prefix("demo", "").unwrap();
    {
        let list = root.append_new_child("{demo}list");
        for x in 0..3 {
            let child = list.append_new_child("{demo}item");
            child.set_text(format!("Item {}", x));
        }
    }

    let mut out: Vec<u8> = Vec::new();
    let options = WriteOptions::new().set_xml_prolog(Some(XmlProlog::Version10));

    root.to_writer_with_options(&mut out, options).unwrap();
    assert_eq!(
        str::from_utf8(&out).unwrap(),
        "\
        <?xml version=\"1.0\" encoding=\"utf-8\"?>\
        <mydoc xmlns=\"demo\">\
            <list>\
                <item>Item 0</item>\
                <item>Item 1</item>\
                <item>Item 2</item>\
            </list>\
        </mydoc>"
    );
}

#[test]
fn test_creation_with_xml_prolog_11() {
    let mut root = Element::new("{demo}mydoc");
    root.set_namespace_prefix("demo", "").unwrap();
    {
        let list = root.append_new_child("{demo}list");
        for x in 0..3 {
            let child = list.append_new_child("{demo}item");
            child.set_text(format!("Item {}", x));
        }
    }

    let mut out: Vec<u8> = Vec::new();
    let options = WriteOptions::new().set_xml_prolog(Some(XmlProlog::Version11));

    root.to_writer_with_options(&mut out, options).unwrap();
    assert_eq!(
        str::from_utf8(&out).unwrap(),
        "\
        <?xml version=\"1.1\" encoding=\"utf-8\"?>\
        <mydoc xmlns=\"demo\">\
            <list>\
                <item>Item 0</item>\
                <item>Item 1</item>\
                <item>Item 2</item>\
            </list>\
        </mydoc>"
    );
}

#[test]
fn test_creation_with_no_xml_prolog_defined() {
    let mut root = Element::new("{demo}mydoc");
    root.set_namespace_prefix("demo", "").unwrap();
    {
        let list = root.append_new_child("{demo}list");
        for x in 0..3 {
            let child = list.append_new_child("{demo}item");
            child.set_text(format!("Item {}", x));
        }
    }

    let mut out: Vec<u8> = Vec::new();
    let options = WriteOptions::new();

    root.to_writer_with_options(&mut out, options).unwrap();
    assert_eq!(
        str::from_utf8(&out).unwrap(),
        "\
        <?xml version=\"1.0\" encoding=\"utf-8\"?>\
        <mydoc xmlns=\"demo\">\
            <list>\
                <item>Item 0</item>\
                <item>Item 1</item>\
                <item>Item 2</item>\
            </list>\
        </mydoc>"
    );
}

#[test]
fn test_render_multiple_times() {
    let mut root = Element::new("{demo}mydoc");
    root.set_namespace_prefix("demo", "").unwrap();
    root.set_attr(("demo", "id"), "some_id".to_string())
        .set_attr(("demo", "name"), "some_name".to_string())
        .set_attr(("demo", "some-other-attr"), "other_attr".to_string());

    let mut out: Vec<u8> = Vec::new();

    root.to_writer_with_options(&mut out, WriteOptions::new())
        .unwrap();
    assert_eq!(
        str::from_utf8(&out).unwrap(),
        "\
        <?xml version=\"1.0\" encoding=\"utf-8\"?>\
        <mydoc xmlns=\"demo\" id=\"some_id\" name=\"some_name\" some-other-attr=\"other_attr\" />"
    );

    let mut out2: Vec<u8> = Vec::new();
    root.to_writer_with_options(&mut out2, WriteOptions::new())
        .unwrap();
    assert_eq!(
        str::from_utf8(&out2).unwrap(),
        "\
        <?xml version=\"1.0\" encoding=\"utf-8\"?>\
        <mydoc xmlns=\"demo\" id=\"some_id\" name=\"some_name\" some-other-attr=\"other_attr\" />"
    );
}

#[test]
fn test_mut_finding() {
    let mut root = Element::from_reader(
        r#"<?xml version="1.0"?>
    <root>
        <list>
            <item> Item 1 </item>Tail 1
            <item> Item 2 </item>Tail 2
            <item> Item 3 </item>Tail 3
        </list>
    </root>
    "#
        .as_bytes(),
    )
    .unwrap();

    {
        let list = root.find_mut("list").unwrap();
        for item in list.find_all_mut("item") {
            item.set_text("wat");
        }
    }

    let v: Vec<_> = root
        .find("list")
        .unwrap()
        .find_all("item")
        .map(|x| x.text())
        .collect();
    assert_eq!(&v, &["wat", "wat", "wat"]);
}

#[test]
fn test_attr() {
    let root = Element::from_reader(
        r#"<?xml version="1.0"?>
    <root xmlns="tag:myns" xmlns:foo="tag:otherns">
        <list a="1" b="2" c="3">
            <item foo:attr="foo1"/>
            <item foo:attr="foo2"/>
            <item foo:attr="foo3"/>
            <foo:item foo:attr="fooitem"/>
        </list>
    </root>
    "#
        .as_bytes(),
    )
    .unwrap();

    let list = root.find("{tag:myns}list").unwrap();
    for (idx, child) in list.find_all("{tag:myns}item").enumerate() {
        assert_eq!(
            child.get_attr("{tag:otherns}attr"),
            Some(format!("foo{}", idx + 1)).as_deref()
        );
        assert_eq!(child.get_attr("{tag:myns}attr"), None);
    }
}
