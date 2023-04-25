use elementtree::Element;

static XML: &str = r#"<?xml version="1.0" encoding="utf-8"?>
<CHECK xmlns:ns1="ns1" stationcode="1" restaurantcode="335350001" cashservername="DEV_MID"  generateddatetime="2023-04-24T11:45:27" chmode="6" locale="1049" shiftdate="2023-04-20" shiftnum="105">
	<EXTINFO reservation="1">
		<INTERFACES current="1000411">
			<INTERFACE cardcode="13484511" type="PDS" id="1000411" mode="0" interface="11">
				<HOLDERS>
					<ITEM cardcode="13484511"/>
				</HOLDERS>
				<ALLCARDS/>
			</INTERFACE>
		</INTERFACES>
	</EXTINFO>
	<CHECKDATA checknum="100825" printnum="0" fiscdocnum="0" delprintnum="0" delfiscdocnum="0" extfiscid="" tablename="2" startservice="2023-04-24T11:44:59" ordernum="1959/1" guests="1" orderguid="{97F9A0DD-2354-4BBC-A56D-1B02B4F98162}" checkguid="{E14BCB77-8D85-412E-9CD3-4ECFAD017A06}" order_cat="1" order_type="9001" persistentcomment="">
		<CHECKPERSONS count="1">
			<PERSON id="{75279A26-238B-42C8-AB51-00E1F5E8CB23}" name="carbis" code="8" role="7"/>
		</CHECKPERSONS>
		<CHECKLINES count="1">
			<LINE id="{3BE341E8-C74D-4705-BD9D-58DD4DBD66D4}" code="100" uni="42" type="dish" price="2" pr_list_sum="2" categ_id="" servprint_id="" egais_categ_id="" quantity="1" sum="1.96">
				<LINETAXES count="1">
					<TAX id="{34694A49-F862-4D07-9ACD-5EECF76E1B6E}" sum="0.3"/>
				</LINETAXES>
				<DISCOUNTS>
					<DISCOUNTPART id="{BD2D6F09-8AA5-4BAB-85EC-3B847F1B0645}" disclineuni="46" sum="-0.2"/>
					<DISCOUNTPART id="{6A490541-9521-46C9-973D-4CDB988B71C9}" disclineuni="202" sum="0.16"/>
				</DISCOUNTS>
			</LINE>
			<LINE id="{3BE341E8-C74D-4705-BD9D-58DD4DBD66D5}" code="101" parent="42" uni="42" type="dish" price="2" pr_list_sum="2" categ_id="" servprint_id="" egais_categ_id="" quantity="1" sum="1.96">
				<LINETAXES count="1">
					<TAX id="{34694A49-F862-4D07-9ACD-5EECF76E1B6E}" sum="0.3"/>
				</LINETAXES>
				<DISCOUNTS>
					<DISCOUNTPART id="{BD2D6F09-8AA5-4BAB-85EC-3B847F1B0645}" disclineuni="46" sum="-0.2"/>
					<DISCOUNTPART id="{6A490541-9521-46C9-973D-4CDB988B71C9}" disclineuni="202" sum="0.16"/>
				</DISCOUNTS>
			</LINE>
		</CHECKLINES>
		<CHECKCATEGS count="1">
			<CATEG id="0" code="0" name="all" sum="1.96" discsum="-0.2"/>
		</CHECKCATEGS>
		<ns1:CURRENCIES count="6">
			<ns1:CURRENCY name="VISA" id="{D7D5E50D-7246-4A71-939F-9B37AA300ACF}" amount="196"/>
			<ns1:CURRENCY name="MASTERCARD" id="{9CA200F8-5961-4A6F-AE22-A095C65D1485}" amount="196"/>
		</ns1:CURRENCIES>
	</CHECKDATA>
</CHECK>
"#;

#[test]
fn test_no_deep_str() {
    let xml = Element::from_reader(XML.as_bytes()).unwrap();

    let extinfo_query = xml.query("EXTINFO");
    // let extinfo = xml.query("EXTINFO");

    assert_eq!(extinfo_query.one().expect("no extinfo").get_attr("reservation").expect("no attr"), "1");
}

#[test]
fn test_no_deep_mut_str() {
    let mut xml = Element::from_reader(XML.as_bytes()).unwrap();

    let mut extinfo_query = xml.query_mut("EXTINFO");
    
    assert_eq!(extinfo_query.one().unwrap().get_attr("reservation").expect("no attr"), "1");
}

#[test]
fn test_deep_two_str() {
    let xml = Element::from_reader(XML.as_bytes()).unwrap();

    let interfaces_query = xml.query("EXTINFO/INTERFACES");
    
    assert_eq!(interfaces_query.one().unwrap().get_attr("current").expect("no attr"), "1000411");
}

#[test]
fn test_deep_two_mut_str() {
    let mut xml = Element::from_reader(XML.as_bytes()).unwrap();

    let mut interfaces_query = xml.query_mut("EXTINFO/INTERFACES");
    
    assert_eq!(interfaces_query.one().unwrap().get_attr("current").expect("no attr"), "1000411");
}

#[test]
fn test_find_attr() {
    let xml = Element::from_reader(XML.as_bytes()).unwrap();

    let interfaces_query = xml.query("CHECKDATA/CHECKLINES/LINE[parent]");
    
    assert_eq!(interfaces_query.one().unwrap().get_attr("code").expect("no attr"), "101");
}

#[test]
fn test_find_not_attr() {
    let xml = Element::from_reader(XML.as_bytes()).unwrap();

    let interfaces_query = xml.query("CHECKDATA/CHECKLINES/LINE[!parent]");
    
    assert_eq!(interfaces_query.one().unwrap().get_attr("code").expect("no attr"), "100");
}

#[test]
fn test_find_namespace() {
    let xml = Element::from_reader(XML.as_bytes()).unwrap();

    let interfaces_query = xml.query("CHECKDATA/{ns1}CURRENCIES/{ns1}CURRENCY");
    
    assert_eq!(interfaces_query.all().len(), 2);
}

#[test]
fn test_find_namespace_and_tag() {
    let xml = Element::from_reader(XML.as_bytes()).unwrap();

    let interfaces_query = xml.query("CHECKDATA/{ns1}CURRENCIES/{ns1}CURRENCY[name=VISA]");
    
    assert_eq!(interfaces_query.one().expect("No element").get_attr("id").expect("no attr"), "{D7D5E50D-7246-4A71-939F-9B37AA300ACF}");
}

#[test]
fn test_find_namespace_and_not_tag() {
    let xml = Element::from_reader(XML.as_bytes()).unwrap();

    let interfaces_query = xml.query("CHECKDATA/{ns1}CURRENCIES/{ns1}CURRENCY[!name=VISA]");
    
    assert_eq!(interfaces_query.one().expect("No element").get_attr("id").expect("no attr"), "{9CA200F8-5961-4A6F-AE22-A095C65D1485}");
}