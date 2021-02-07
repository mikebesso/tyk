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



Begin["`Private`"] (* Begin Private Context *) 



tykItems[] := tykItems[] = Module[
	{}
	,
	tykRawItems[][
   			All
   			, 
   			{
   				"Item ID"
   				, "Item SKU"
   				, "Item Active"
   				, "Item Name"
   				, "Item Cost"
   				, "Item Retail"
   				, "Item Wholesale"
   			}
   		] // dsDeleteDuplicates
];


tykInventory[] := (*tykInventory[] =*) Module[
	{
	}
	,
	dsInnerJoin[
		dsAggregate$sum[tykRawInventory[], {"Item ID"}, {"On Hand"}]
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

	