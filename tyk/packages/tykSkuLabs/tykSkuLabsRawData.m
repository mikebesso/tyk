(* Wolfram Language Package *)

BeginPackage[
	"tykSkuLabsRawData`"
	,
	{
		"ww`"
		, "wwBootstrap`"
		, "wwJSON`"
		, "wwDatasets`"
		, "wwDates`"
		, "wwStrings`"
	}
]

tykRawInventory::usage = "tykRawInventory  "

tykRawItems::usage = "tykRawItems  "

tykRawKits::usage = "tykRawKits  "

tykRawOrders::usage = "tykRawOrders  "

dsTakeColumns::usage = "dsTakeColumns  "

dsDeleteDuplicates::usage = "dsDeleteDuplicates  "


tykSkuLabsDateCard::usage = " "

tykSkuLabsSetDate::usage = "tykSkuLabsSetDate  "

skulabsRawData::usage = "skulabsRawData  ";



(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 


rawDataFolder[endPoint_String, dateCard_String] := FileNameJoin[
	{
		"~"
		, "projects"
		, "tykables"
		, "files"
		, "tykSkuLabs"
		, "json"
		, dateCard
		, endPoint
	}
	
];


Options[skulabsRawData] = {
	"Environment" -> "Production"
	, "TestMode" -> True
}


skulabsRawData[dateCard_String, opt:OptionsPattern[]] := <| 
		"Items" -> tykRawItems[dateCard]
		, "Inventory" -> tykRawInventory[dateCard]
		, "Kits" -> tykRawKits[dateCard]
		, "Orders" -> tykRawOrders[dateCard]
	|>
	
skulabsRawData[date_DateObject, opt:OptionsPattern[]] := skulabsRawData[str$Tidy[date], opt];


parseJsonItem[{}] = Nothing;
parseJsonItem[Null] = Nothing;
parseJsonItem[json_Association] := <|
   "Item ID" -> json["_id"]
   , "Item SKU" -> json["sku"]
   , "Item Active" -> json["active"]
   , "Item Listings" -> json["listings"]
   , "Item Name" -> json["name"]
(*   , "Item Retail" -> nullIfMissing[json["retail"]]
   , "Item Cost" -> nullIfMissing[json["cost"]]
   , "Item Wholesale" -> nullIfMissing[json["wholesale"]]
   , "Item Weight" -> nullIfMissing[Quantity[json["weight"], json["weight_unit"]]]
*)   |> ;



parseJsonInventory[{}] = Nothing;
parseJsonInventory[Null] = Nothing;
parseJsonInventory[json_Association] := <|
	"Location ID" -> json["location_id"]
	, "Item ID" -> json["item_id"]
	, "Item On Hand" -> json["on_hand"]
|>;


parseJsonKit[{}] = Nothing;
parseJsonKit[Null] = Nothing;
parseJsonKit[json_Association] := <|
	"Kit ID" -> json["_id"]
	, "Kit Name" -> json["name"]
	, "Kit Items" -> json["items"]
	, "Kit SKU" -> json["listing_sku"]
	, "Kit Listings" -> json["listings"]
|>;


parseStash[json_Association, key_String] := Module[
	{
		stash = nullIfMissing[json["stash"]]
	}
	,
	If[
		MissingQ[stash]
		,
		Null
		,
		nullIfMissing[stash[key]]
	]
];

parseJsonOrder[{}] = Nothing;
parseJsonOrder[Null] = Nothing;
parseJsonOrder[json_Association] := Module[
	{
		orderDateRaw
		, orderDateTime
	}
	,
	
	If[
		! KeyExistsQ[json, "stash"]
		,
		Return[Nothing]
	];
	
	orderDateRaw = parseStash[json, "date"];
	
	orderDateTime =	StringSplit[parseStash[json, "date"], "T"];
	
	<|
		"Order ID" -> json["_id"]
		, "Store ID" -> json["store_id"]
		, "Order Number" -> json["order_number"]
		, "Order Status" -> json["status"]
		, "Order Time" -> orderDateTime[[2]]
		, "Order Date" -> orderDateTime[[1]]
		, "Order Items" -> parseStash[json, "items"]
	|>

];


tykRawInventory[dateCard_String] := importRawJsonFolder[
	rawDataFolder["inventory", dateCard]
	, parseJsonInventory
][
	All
	,
	<|
		"Inventory Date" -> dateCard
		, #
	|>&
];
	

tykRawItems[dateCard_String] := importRawJsonFolder[
	rawDataFolder["item", dateCard]
	, parseJsonItem
]


tykRawKits[dateCard_String] :=  Module[
	{
		rawKits
	},
	
	
	rawKits = importRawJsonFolder[
		rawDataFolder["kit", dateCard]
		, parseJsonKit
	];

	rawKits[Select[Length[#["Kit Items"]] > 0 &], All]

];




Options[tykRawOrders] = {
	"Environment" -> "Production"
	, "TestMode" -> True
}

tykRawOrders[dateCard_String, OptionsPattern[]] = Module[
	{
		limit = If[OptionValue["TestMode"], 3, All],
		orders
	}
	,
	
	orders = importRawJsonFolder[
		rawDataFolder["order", dateCard]
		, parseJsonOrder
		, "Limit" -> limit
	];
	
	orders[
		Select[Length[#["Order Items"]] > 0 &]
		, All
	]
	
];
	


End[] (* End Private Context *)

EndPackage[]