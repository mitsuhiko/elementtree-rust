use crate::AsQName;

use super::{Element, AttributeFilter, QueryRule, AsQueryRule, QName, IsMatch};

#[test]
fn test_attr_filter_basic_match() {
    let filter = AttributeFilter::new(QName::from_ns_name(None, "root"), None, false);

    let mut el = Element::new("root");
    el.set_attr("root", "1");

    assert!(filter.is_match(&el))
}

#[test]
#[should_panic]
fn test_attr_filter_basic_not_match() {
    let filter = AttributeFilter::new(QName::from_ns_name(None, "root"), None, false);

    let mut el = Element::new("root");
    el.set_attr("notroot", "1");

    assert!(filter.is_match(&el))
}

#[test]
fn test_attr_filter_ns_match() {
    let tmp = ("ns1", "root");
    let filter = AttributeFilter::new(tmp.as_qname().into_owned(), None, false);

    let mut el = Element::new("root");
    el.set_attr(tmp, "1");

    assert!(filter.is_match(&el))
}

#[test]
#[should_panic]
fn test_attr_filter_ns_not_match() {
    let tmp = ("ns1", "root");
    let filter = AttributeFilter::new(tmp.as_qname().into_owned(), None, false);

    let mut el = Element::new("root");
    el.set_attr(("ns2", "root"), "1");

    assert!(filter.is_match(&el))
}

#[test]
fn test_attr_filter_value_match() {
    let filter = AttributeFilter::new(QName::from_ns_name(None, "root"), Some("1"), false);

    let mut el = Element::new("root");
    el.set_attr("root", "1");

    assert!(filter.is_match(&el))
}

#[test]
#[should_panic]
fn test_attr_filter_value_not_match() {
    let filter = AttributeFilter::new(QName::from_ns_name(None, "root"), Some("2"), false);

    let mut el = Element::new("root");
    el.set_attr("root", "1");

    assert!(filter.is_match(&el))
}

#[test]
#[should_panic]
fn test_attr_filter_invert_match() {
    let filter = AttributeFilter::new(QName::from_ns_name(None, "root"), Some("1"), true);

    let mut el = Element::new("root");
    el.set_attr("root", "1");

    assert!(filter.is_match(&el))
}

#[test]
fn test_attr_filter_invert_not_match() {
    let filter = AttributeFilter::new(QName::from_ns_name(None, "root"), Some("2"), true);

    let mut el = Element::new("root");
    el.set_attr("root", "1");

    assert!(filter.is_match(&el))
}

#[test]
fn test_query_from_str_basic() {
    let q = "item".as_query_rule().expect("Not parsed");
    
    assert_eq!(q.name, QName::from_ns_name(None, "item"))
}

#[test]
fn test_query_from_str_ns() {
    let q = "{ns}item".as_query_rule().expect("Not parsed");
    
    assert_eq!(q.name, QName::from_ns_name(Some("ns"), "item"))
}

#[test]
fn test_query_from_str_ns_attr() {
    let q = "{ns}item[tag]".as_query_rule().expect("Not parsed");
    
    assert_eq!(q.name, QName::from_ns_name(Some("ns"), "item"));
    assert_eq!(q.filter.expect("filter not parsed").name, QName::from_ns_name(None, "tag"));
}

#[test]
fn test_query_from_str_ns_notattr() {
    let q = "{ns}item[!tag]".as_query_rule().expect("Not parsed");
    
    assert_eq!(q.name, QName::from_ns_name(Some("ns"), "item"));
    assert_eq!(q.filter.clone().expect("filter not parsed").name, QName::from_ns_name(None, "tag"));
    assert!(q.filter.expect("filter not parsed").invert);
}

#[test]
fn test_query_from_str_ns_nsattr() {
    let q = "{ns}item[{ns}tag]".as_query_rule().expect("Not parsed");
    
    assert_eq!(q.name, QName::from_ns_name(Some("ns"), "item"));
    assert_eq!(q.filter.clone().expect("filter not parsed").name, QName::from_ns_name(Some("ns"), "tag"));
}


#[test]
fn test_query_from_str_ns_notnsattr() {
    let q = "{ns}item[!{ns}tag]".as_query_rule().expect("Not parsed");
    
    assert_eq!(q.name, QName::from_ns_name(Some("ns"), "item"));
    assert_eq!(q.filter.clone().expect("filter not parsed").name, QName::from_ns_name(Some("ns"), "tag"));
    assert!(q.filter.expect("filter not parsed").invert);
}


#[test]
fn test_query_from_str_ns_notnsattr_value() {
    let q = "{ns}item[!{ns}tag=123]".as_query_rule().expect("Not parsed");
    
    assert_eq!(q.name, QName::from_ns_name(Some("ns"), "item"));
    assert_eq!(q.filter.clone().expect("filter not parsed").name, QName::from_ns_name(Some("ns"), "tag"));
    assert_eq!(q.filter.clone().expect("filter not parsed").value.expect("value not parsed"), "123");
    assert!(q.filter.expect("filter not parsed").invert);
}