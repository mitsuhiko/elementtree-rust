use elementtree::{Element, AsQuery};

const XML: &str = include_str!("test.xml");

pub fn main() {
    let root = Element::from_reader(&mut XML.as_bytes()).unwrap();
    println!("Print some stuff");
    for child in root.children() {
        println!("{}", child.tag());
        for child in child.children() {
            println!("  {}", child.tag());
        }
    }

    println!();
    println!("FIND CHILDREN");
    let ns = "urn:oasis:names:tc:SAML:2.0:assertion";
    let list = root.find((ns, "list")).unwrap();
    for item in list.find_all((ns, "item")) {
        println!("-> {}", item.tag());
    }

    let a = "{urn:oasis:names:tc:SAML:2.0:assertion}list/{urn:oasis:names:tc:SAML:2.0:assertion}item".as_query().unwrap();

    for el in a.filter(root.children()) {
        println!("---> {}", el.tail());
    }
    
}
