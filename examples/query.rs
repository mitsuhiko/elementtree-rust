use elementtree::Element;

const XML: &str = include_str!("registry.xml");

fn main() {
    let root = Element::from_reader(XML.as_bytes()).unwrap();

    println!("Primitive example. Just query all \"List\" nodes.");
    for list in root.query("List").all() {
        println!(" -> List[name] {:?}", list.get_attr("name"));
    }

    println!("More complex. All ns1:Items");
    for items in root.query("List/{ns1}Items").all() {
        println!(" -> Items[count] {:?}", items.get_attr("count"));
    }

    println!("Query Item by id");
    println!(
        " -> Item.text {:?}",
        root.query("List/{ns1}Items/{ns1}Item[{ns2}id=4]")
            .one()
            .unwrap()
            .text()
    );

    println!("Default namespace");
    println!(
        " -> Item.text {:?}",
        root.query("List[name=entities]")
            .one()
            .unwrap()
            .query(("ns1", "Items/Item[{ns2}id=5]"))
            .one()
            .unwrap()
            .text()
    );

    println!("Query slice, and attribute filter");
    for item in root
        .query(
            [
                ("", "List[name=items]"),
                ("ns1", "Items"),
                ("ns1", "Item[tagged]"),
            ]
            .as_slice(),
        )
        .all()
    {
        println!(" -> Item[tagged] {:?}", item.get_attr("tagged").unwrap())
    }

    println!("Attribute filter inversion");
    for item in root
        .query(
            [
                ("", "List[name=items]"),
                ("ns1", "Items"),
                ("ns1", "Item[!tagged]"),
            ]
            .as_slice(),
        )
        .all()
    {
        println!(
            " -> Item[tagged] {:?}",
            item.get_attr("tagged").unwrap_or("<not defined>")
        )
    }
}
