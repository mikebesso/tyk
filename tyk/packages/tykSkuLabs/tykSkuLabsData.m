(* Wolfram Language Package *)

BeginPackage["tykSkuLabsData`", 
	{ 
		"ww`"
		, "wwBootstrap`"
		, "wwPackages`"
		, "wwJSON`"
		, "wwDatasets`"
		, "wwDates`"
		, "tykSkuLabsRawData`"
	}
]
(* Exported symbols added here with SymbolName::usage *)  

packageClearSymbols["tykSkuLabsData`"];

tykInventory::usage = "";
tykItems::usage = "";
tykItemListings::usage = "";
tykItemVariants::usage = "";
tykKits::usage = "";
tykKitListings::usage = "";
tykKitVariants::usage = "";
tykKitItems::usage = "";
tykOrders::usage = "";
tykOrderItems::usage = "";
tykSKUs::usage = ""

skulabsData::usage = "skulabsData  "



Begin["`Private`"] (* Begin Private Context *) 


itemColumns = {
	"Item ID"
	, "Item SKU"
	, "Item Active"
	, "Item Name"
};

orderColumns = {
	"Order ID"
	, "Store ID"
	, "Order Number"
	, "Order Status"
	, "Order Date"
	, "Order Time"
};

kitColumns = {
	"Kit ID"
	, "Kit Name"
	, "Kit SKU"
}


Options[skulabsData] = {
	"Environment" -> "Production"
	, "TestMode" -> True
}

skulabsData[date_DateObject, opt:OptionsPattern[]] := Module[
	{
		rawData = skulabsRawData[date, opt]
		
		
		, items
		, itemListings
		, itemVariants
		, kits
		, kitItems
		, kitVariants
		, kitListings
		, orders
		, orderItems
		, inventory
	}
	,


	items = rawData["Items"][All, itemColumns] // dsDeleteDuplicates;
	orders = rawData["Orders"][All, orderColumns];
	kits = rawData["Kits"][All, kitColumns];
	
	inventory = dsInnerJoin[
		dsAggregate$sum[rawData["Inventory"], {"Item ID", "Inventory Date"}, {"Item On Hand"}]
		, items
		, {"Item ID"}
	];
	
	   		
	   		
	itemListings = Module[
		{
			mapListings
			, rawListings
		},
		mapListings[item_Association] := Map[
			<|
				"Item ID" -> item["Item ID"]
				, "Listing ID" -> #["item_id"]
				, "Variant ID" -> #["variant_id"]
				, "Store ID" -> #["store_id"]
				
			|> &
		    ,
		    item["Item Listings"]
		];
		
		rawListings = Dataset@Flatten@Map[mapListings, Normal@rawData["Items"]];
	
		dsInnerJoin[rawListings, items, {"Item ID"}]
	];   		
	
	
	itemVariants = Module[
		{
			mapVariants
			, rawVariants
		},
		mapVariants[item_Association] := Map[
			nothingIfNull[
				#["variant_id"]
				,
				<|
					"Item ID" -> item["Item ID"]
					, "Listing ID" -> #["_id"]
					, "Variant ID" -> #["variant_id"]
					, "Store ID" -> #["store_id"]
					
				|>
			] &
		    ,
		    item["Item Listings"]
		];
	
		rawVariants = Dataset@Flatten@Map[mapVariants, Normal@rawData["Items"]];
	
		dsInnerJoin[rawVariants, items, {"Item ID"}]
	];	

	   		
	kitItems = Module[
		{
			mapKitItems
			, rawKitItems
		}
		,
		mapKitItems[kit_Association] := Map[
			<|
				"Kit ID" -> kit["Kit ID"]
				, "Item ID" -> #["item_id"]
				, "Item Quantity" -> #["quantity"]
			|> &
		    ,
		    kit["Kit Items"]
		];
			
		rawKitItems = Dataset@Flatten@Map[mapKitItems, Normal@rawData["Kits"]];
		
		
		dsInnerJoin[
			dsInnerJoin[
				rawKitItems
				, kits
				, {"Kit ID"}
			]
			, dsTakeColumns[items, {"Item ID", "Item SKU"}]
			, {"Item ID"}
		]
	];
	
	
	   		
	orderItems = Module[
		{
			mapOrderItems
			, rawOrderItems
		},
		mapOrderItems[order_Association] := Map[
			<|
				"Order ID" -> order["Order ID"]
				, "Store ID" -> order["Store ID"]
				, "Line Item Key" -> nullIfMissing[#["_id"]]
				, "Line Item ID" -> nullIfMissing[#["id"]]
				, "Line Item Variant" -> nullIfMissing[#["variant_id"]]
				, "Line Item SKU" -> nullIfMissing[#["lineSku"]]
				, "Line Item Name" -> nullIfMissing[#["lineName"]]
				, "Line Item Quantity" -> #["quantity"]
				, "Line Item Drop Shipped" -> #["dropshipped"]
				, "Line Item Price" -> #["price"]
				, "Line Item Match ID" -> Null
				, "Line Item Match By" -> Null
			|> &
		    ,
		    order["Order Items"]
		];
		
		rawOrderItems = Dataset@Flatten@Map[mapOrderItems, Normal@rawData["Orders"]];
	
	
		dsInnerJoin[rawOrderItems, orders, {"Order ID"}]
	];   		
	   		
	   		



	<|
		"Items" -> items
		, "ItemListings" -> itemListings
		, "ItemVariants" -> itemVariants
		, "Kits" -> kits
		, "KitItems" -> kitItems
		, "KitListings" -> kitListings
		, "KitVariants" -> kitVariants
		, "Orders" -> orders
		, "OrderItems" -> orderItems
		, "Inventory" -> inventory
	|>

];

(*


tykItems[rawData_Association] = Module[
	{}
	,
	rawData["Items"][
   			All
   			, 
   			{
   				"Item ID"
   				, "Item SKU"
   				, "Item Active"
   				, "Item Name"
   			}
   		] // dsDeleteDuplicates
];


tykInventory[rawData_Association, items_Dataset] := tykInventory[] = Module[
	{},
	dsInnerJoin[
		dsAggregate$sum[rawData["Inventory"], {"Item ID", "Inventory Date"}, {"Item On Hand"}]
		, tykItems[]
		, {"Item ID"}
	]
];



tykItemListings[] := tykItemListings[] = Module[
	{
		mapListings
		, rawListings
	},
	mapListings[item_Association] := Map[
		<|
			"Item ID" -> item["Item ID"]
			, "Listing ID" -> #["item_id"]
			, "Variant ID" -> #["variant_id"]
			, "Store ID" -> #["store_id"]
			
		|> &
	    ,
	    item["Item Listings"]
	];
	
	rawListings = Dataset@Flatten@Map[
		mapListings,
		Normal@tykRawItems[]
	];

	dsInnerJoin[
		rawListings
		, tykItems[]
		, {"Item ID"}
	]
];



tykItemVariants[] := tykItemVariants[] = Module[
	{
		mapVariants
		, rawVariants
	},
	mapVariants[item_Association] := Map[
		nothingIfNull[
			#["variant_id"]
			,
			<|
				"Item ID" -> item["Item ID"]
				, "Listing ID" -> #["_id"]
				, "Variant ID" -> #["variant_id"]
				, "Store ID" -> #["store_id"]
				
			|>
		] &
	    ,
	    item["Item Listings"]
	];

	rawVariants = Dataset@Flatten@Map[
		mapVariants,
		Normal@tykRawItems[]
	];

	dsInnerJoin[
		rawVariants
		, tykItems[]
		, {"Item ID"}
	]
];	


tykKits[] := tykKits[] = Module[
	{
		rawKits = tykRawKits[][Select[Length[#["Kit Items"]] > 0 &], All]
	}
	,
	rawKits[
		All
		, 
		{
			"Kit ID"
			, "Kit Name"
			, "Kit SKU"
		}
	]
];


tykKitItems[] := tykKitItems[] = Module[
	{
		rawKits = tykRawKits[][Select[Length[#["Kit Items"]] > 0 &], All]
		, mapKitItems
		, rawKitItems
	}
	,
	mapKitItems[kit_Association] := Map[
		<|
			"Kit ID" -> kit["Kit ID"]
			, "Item ID" -> #["item_id"]
			, "Item Quantity" -> #["quantity"]
		|> &
	    ,
	    kit["Kit Items"]
	];
		
	rawKitItems = Dataset@Flatten@Map[
		mapKitItems,
		Normal@rawKits
	];
	
	
	dsInnerJoin[
		dsInnerJoin[
			rawKitItems
			, tykKits[]
			, {"Kit ID"}
		]
		, dsTakeColumns[tykItems[], {"Item ID", "Item SKU"}]
		, {"Item ID"}
	]
];



tykOrders[] := tykOrders[] = Module[
	{
	}
	,
	tykRawOrders[][
		All
		, {
			"Order ID"
			, "Store ID"
			, "Order Number"
			, "Order Status"
			, "Order Date"
			, "Order Time"
		}
	]
];

tykOrderItems[] := tykOrderItems[] = Module[
	{
		mapOrderItems
		, rawOrderItems
	},
	mapOrderItems[order_Association] := Map[
		<|
			"Order ID" -> order["Order ID"]
			, "Store ID" -> order["Store ID"]
			, "Line Item Key" -> nullIfMissing[#["_id"]]
			, "Line Item ID" -> nullIfMissing[#["id"]]
			, "Line Item Variant" -> nullIfMissing[#["variant_id"]]
			, "Line Item SKU" -> nullIfMissing[#["lineSku"]]
			, "Line Item Name" -> nullIfMissing[#["lineName"]]
			, "Line Item Quantity" -> #["quantity"]
			, "Line Item Drop Shipped" -> #["dropshipped"]
			, "Line Item Price" -> #["price"]
			, "Line Item Match ID" -> Null
			, "Line Item Match By" -> Null
		|> &
	    ,
	    order["Order Items"]
	];
	
	rawOrderItems = Dataset@Flatten@Map[
		mapOrderItems,
		Normal@tykRawOrders[]
	];


	dsInnerJoin[
		rawOrderItems
		, tykOrders[]
		, {"Order ID"}
	]
];
*)
	
End[] (* End Private Context *)


(*packageProtectSymbols["tykSkuLabsData`"];
*)
EndPackage[]





(*
importSkuLabsItems[] := Module[
	{
	}
	,

	tykItemListings = Module[
		{
			mapListings
			, rawListings
		},
		mapListings[item_Association] := Map[
			<|
				"Item ID" -> item["Item ID"]
				, "Listing ID" -> #["item_id"]
				, "Variant ID" -> #["variant_id"]
				, "Store ID" -> #["store_id"]
				
			|> &
		    ,
		    item["Item Listings"]
		];
		
		rawListings = Dataset@Flatten@Map[
			mapListings,
			Normal@tykRawItems[]
		];
	
		dsInnerJoin[
			rawListings
			, tykItems[]
			, {"Item ID"}
		]
	];
	
	tykItemVariants = Module[
		{
			mapVariants
			, rawVariants
		},
		mapVariants[item_Association] := Map[
			nothingIfNull[
				#["variant_id"]
				,
				<|
					"Item ID" -> item["Item ID"]
					, "Listing ID" -> #["_id"]
					, "Variant ID" -> #["variant_id"]
					, "Store ID" -> #["store_id"]
					
				|>
			] &
		    ,
		    item["Item Listings"]
		];
	
		rawVariants = Dataset@Flatten@Map[
			mapVariants,
			Normal@tykRawItems[]
		];
	
		dsInnerJoin[
			rawVariants
			, tykItems[]
			, {"Item ID"}
		]
	];	
	
];*)

(*
importSkuLabsKits[] := Module[
	{
		rawKits = tykRawKits[][Select[Length[#["Kit Items"]] > 0 &], All]
	}
	,
	

	tykKits = rawKits[All, kitColumns];
	
	tykKitItems = Module[
		{
			mapKitItems
			, rawKitItems
		},
		mapKitItems[kit_Association] := Map[
			<|
				"Kit ID" -> kit["Kit ID"]
				, "Item ID" -> #["item_id"]
				, "Item Quantity" -> #["quantity"]
			|> &
		    ,
		    kit["Kit Items"]
		];
		
		rawKitItems = Dataset@Flatten@Map[
			mapKitItems,
			Normal@rawKits
		];
	
	
		dsInnerJoin[
			dsInnerJoin[
				rawKitItems
				, tykKits
				, {"Kit ID"}
			]
			, dsTakeColumns[tykItems[], {"Item ID", "Item SKU"}]
			, {"Item ID"}
		]
	];
	
	
	tykKitListings = Module[
		{
			mapListings
			, rawListings
		},
		mapListings[kit_Association] := Map[
			<|
				"Kit ID" -> kit["Kit ID"]
				, "Listing ID" -> #["item_id"]
				, "Variant ID" -> #["variant_id"]
				, "Store ID" -> #["store_id"]
				
			|> &
		    ,
		    kit["Kit Listings"]
		];
		
		rawListings = Dataset@Flatten@Map[
			mapListings,
			Normal@rawKits
		];
	
		dsInnerJoin[
			rawListings
			, tykKits
			, {"Kit ID"}
		]
];
*)

	