(* Wolfram Language Package *)

(* Created by the Wolfram Workbench Nov 7, 2020 *)

BeginPackage[
	"tykShopifyTags`",
	{ 
		"ww`",
		"wwBootstrap`",
		"wwPackages`",
		"wwPatterns`",
		"wwMessages`",
		"wwExcel`",
		"wwDates`",
		"wwLists`",
		"wwStrings`",
		"wwShopify`",
		"tykCommon`"
	}
]

packageClearSymbols["tykShopifyTags`"]

tykShopifyInit::usage = "tykShopifyInit  "

buildTagUpdates::usage = "buildTagUpdates  "

tagsFromOrder::usage = "tagsFromOrder  "

tagsFromSku::usage = "tagsFromSku  "

tagsFromSnappiesSku::usage = "tagsFromSnappiesSku  ";


(* Exported symbols added here with SymbolName::usage *) 

Begin["`Private`"]
(* Implementation of the package *)

mapSkuToTags = Null;


tykShopifyInit[] := Module[
	{}
	,
	shopifyInit[
		"tykables", 
		"3e39af6f03f0573e8eeb511df6398ef8", 
		"89d38d49f7c08bee3ea90183fe38ed78", 
		$limit -> 200
	];

	mapSkuToTags = With[
		{
			raw = excelImport["tykables-tags-by-sku.xlsx"]
		}
		,
		Map[
			#["sku"] -> ReplaceAll[Rest[Values[#]], "" -> Nothing] &
			,
			raw // Normal
		]
	];


];




tagsFromDiaperSku[sku_String] := Lookup[
	mapSkuToTags, 
	StringReplace[sku, {"MC" -> "", "HC" -> "", "S" -> ""}], 
	{}
];

tagsFromDiaperSku[sku_?NumericQ] := tagsFromDiaperSku[str$Tidy[sku]];


tagsFromSnappiesSku[sku_String] := Module[
	{}
	,
	If[
		StringLength[sku] == 9
		,
		Lookup[mapSkuToTags, StringTake[sku, 8], {}]
		,
		{}
	]
];

tagsFromUndiesSku[sku_String] := Module[
	{}
	,
	If[
		Length[sku] == 9
		,
		Lookup[mapSkuToTags, StringTake[sku, 8], {}]
		,
		{}
	]
];


tagsFromDenimSku[sku_String]  := Module[
	{}
	,
	If[
		StringLength[sku] == 9
		,
		Lookup[mapSkuToTags, StringTake[sku, 8], {}]
		,
		{}
	]
];


tagsFromPlushieSku[sku_String]  := Module[
	{}
	,
	If[
		StringLength[sku] == 9
		,
		Lookup[mapSkuToTags, StringTake[sku, 8], {}]
		,
		{}
	]
]

tagsFromSku[""] = {};
tagsFromSku[Null] = {};
tagsFromSku[sku_String] := Module[
	{
		tags = {}
	}
	,
   
	tags = Catch[
		tags = tagsFromDiaperSku[sku];
		If[Length[tags] > 0, Throw[tags]];
     
		tags = tagsFromSnappiesSku[sku];
		If[Length[tags] > 0, Throw[tags]];
     
 		tags = tagsFromUndiesSku[sku];
		If[Length[tags] > 0, Throw[tags]];
     
		tags = tagsFromDenimSku[sku];
		If[Length[tags] > 0, Throw[tags]];
     
		tags = tagsFromPlushieSku[sku];
		If[Length[tags] > 0, Throw[tags]];
	];
   
	tags
];



cleanTags[tags_List] := Module[
   {
    tagsCleaned
    },
   
   tagsCleaned = Map[
     StringTrim,
     tags
     ];
   
   tagsCleaned = ReplaceAll[
     tagsCleaned,
     {
      "Diapers Overnight" -> "Diapers Overnights",
      "Camelot Diapers" -> "Diapers Camelot",
      "Cammies Diapers" -> "Diapers Cammies",
      "Drylife Diapers" -> "Diapers Drylife",
      "Galactic Diapers" -> "Diapers Galactic",
      "Metro Diapers" -> "Diapers Metro",
      "Overnight Diapers" -> "Diapers Overnight",
      "Pride Diapers" -> "Diapers Pride",
      "Puppers Diapers" -> "Diapers Puppers",
      "Rascals Diapers" -> "Diapers Rascals",
      "Rawr Diapers" -> "Diapers Rawr",
      "Playdayz Diapers" -> "Diapers Playdayz",
      "Unicorns Diapers" -> "Diapers Unicorns"
      }
     ];
   
   tagsCleaned = Map[
     StringReplace[
      {
       "Small" -> "Sm"
       , "Medium" -> "Md"
       , "Large" -> "Lg"
       }
      ],
     tagsCleaned
     ];
   
   
   tagsCleaned = Select[tagsCleaned,  Not[StringStartsQ[#, "lookup" , IgnoreCase -> True]] &];
   tagsCleaned = Select[tagsCleaned,  Not[StringStartsQ[#, "{" , IgnoreCase -> True]] &];
   
   tagsCleaned
   
    
   
   ];


tagsFromOrder[order_Association] := Module[
   {
    customer = order["customer"],
    number = order["name"],
    retVal
    }
   ,
   
(*   Echo[customer, "Customer"];
   Echo[number, "Order Number"];*)
   
   Catch[
    
    throwNothingIfMissing[customer];
    
    Module[
     {
      customerTags = cleanTags@StringSplit[order["customer"]["tags"], ","],
      lineItemSkus,
      orderTags ,
      newTags,
      overwriteTags
      },
     
     lineItemSkus =  Map[
       #["sku"] &,
       order["line_items"]
       ];
     
     orderTags = Map[
         	tagsFromSku[#["sku"]] &,
         	order["line_items"]
         	]  // Flatten // DeleteCases[Null];
     
     newTags = Complement[orderTags, customerTags];
     
     overwriteTags = 
      If[Length[newTags] > 0, Union[customerTags, newTags], {}];
     
     
     retVal = <|
      	"order_number" -> number
      	, "customer_id" -> customer["id"]
      	, "current_tags" ->  listToCSV[customerTags]
      	, "order_tags" -> listToCSV[orderTags]
      	, "new_tags" -> listToCSV[newTags]
      	, "overwrite_tags" -> listToCSV[overwriteTags]
      	, "line_item_skus" -> listToCSV[lineItemSkus]
      |>;
      
      
      (*If[Length[newTags] > 0, Echo[retVal]];*)
      
      Throw[retVal];
      
     ]
    ]
   
   ];



buildTagUpdates[info_Association] := Module[
   {},
   
   If[
    And[
     info["new_tags"] != ""
     , info["overwrite_tags"] != ""
     ]
    ,
    With[
     {
      id = info["customer_id"],
      tags = info["overwrite_tags"]
      },
     <| "id" -> id, 
      "put" -> 
       ExportString[<|
         "customer" -> <| "id" -> id, "tags" -> tags |>|>, "JSON", 
        "Compact" -> True] |>
     ]
    ,
    Nothing
    ]
   
   ];

End[]


packageProtectSymbols["tykShopifyTags`"]

EndPackage[]

