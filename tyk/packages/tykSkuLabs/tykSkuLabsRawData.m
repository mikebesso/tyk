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
	}
]

tykRawInventory::usage = "tykRawInventory  "

tykRawItems::usage = "tykRawItems  "

tykRawKits::usage = "tykRawKits  "

tykRawOrders::usage = "tykRawOrders  "

dsTakeColumns::usage = "dsTakeColumns  "

dsDeleteDuplicates::usage = "dsDeleteDuplicates  "






(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 





parseJsonItem[{}] = Nothing;
parseJsonItem[Null] = Nothing;
parseJsonItem[json_Association] := <|
   "Item ID" -> json["_id"]
   , "Item SKU" -> json["sku"]
   , "Item Active" -> json["active"]
   , "Item Listings" -> json["listings"]
   , "Item Name" -> json["name"]
   , "Item Retail" -> nullIfMissing[json["retail"]]
   , "Item Cost" -> nullIfMissing[json["cost"]]
   , "Item Wholesale" -> nullIfMissing[json["wholesale"]]
   , "Item Weight" -> nullIfMissing[Quantity[json["weight"], json["weight_unit"]]]
   |> ;



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
parseJsonOrder[json_Association] := With[
	{
		orderDateTime = StringSplit[parseStash[json, "date"], "T"]
	}
	,
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


tykRawInventory[] := tykRawInventory[] = importRawJsonFolder[
	"/Users/mike/projects/tykables/files/tykSkuLabs/json/2021-01-15/inventory"
	, parseJsonInventory
];
	

tykRawItems[] := tykRawItems[] = importRawJsonFolder[
	"/Users/mike/projects/tykables/files/tykSkuLabs/json/2021-01-15/item"
	, parseJsonItem
]


tykRawKits[] := tykRawKits[] = importRawJsonFolder[
	"/Users/mike/projects/tykables/files/tykSkuLabs/json/2021-01-15/kit"
	, parseJsonKit
]

tykRawOrders[] := tykRawOrders[] = With[
	{
		orders = importRawJsonFolder[
			"/Users/mike/projects/tykables/files/tykSkuLabs/json/2021-01-15/order"
			, parseJsonOrder
		]
	}
	,
	orders[
		Select[Length[#["Order Items"]] > 0 &]
		, All
	]
	
];
	


End[] (* End Private Context *)

EndPackage[]