(* Wolfram Language Package *)



BeginPackage[
	"tykODU`", 
	{ 
		"tykCommon`"
		, "ww`"
		, "wwBootstrap`"
		, "wwExcel`"
		, "wwHTTP`"
		, "wwDatasets`"
		, "wwFileSystem`"
		, "wwDates`"
	}
]

oduScrapeInventory::usage = "oduScrapeInventory  "

(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 


oduScrape[meta_Association] := Module[
	{
		url = URLBuild[{"https://onesiesdownunder.com/", "collections", meta["Collection"], "products", meta["Path"]}]
		, scrape
		, parsed
	}
	,
	scrape = With[
		{
			contents = httpDownloadString[url]
			, startJson = "<script type=\"application/json\" id=\"ProductJson-product-template\">"
			, endJson = "</script>"
     	},
		Pause[1];
		StringCases[contents, 
			Shortest[startJson ~~ data___ ~~ endJson] -> data, 
			IgnoreCase -> False]
	];
   
	If[Length[scrape] == 0, Return[Nothing]];
   
	parsed = With[
		{
			json = ImportString[First@scrape, "RawJSON"]
		}
		,
		Map[
			KeyTake[{"title", "available", "inventory_quantity", "price"}]
			,
			json["variants"]
		]
	];
	
	Join[meta, <|"JSON" -> parsed|>]
];

oduFlattenScrape[data_Association] := Module[
	{
		mapVariants
	},
   
	mapVariants[variant_Association] := With[
		{
			size = variant["title"]
		}
		,
		Join[
			KeyDrop[data, {"JSON"}],
			<|
				"Date" -> dtToday[], 
				"Size" -> size, 
				"Quantity" -> 1, 
				"Available" -> variant["available"], 
				"On Hand" -> variant["inventory_quantity"], 
				"Product Key" -> data["Product Key"] ~~ "-" ~~ size
			|>
		]
	];
   Map[mapVariants, data["JSON"]]
];


oduScrapeInventory[] := Module[
	{
		meta = excelImport["odu-meta.xlsx"]
		, inventory
		, filename = fs$AddDateToFileName[
      		FileNameJoin[
      			{
      				tyk$DataFolder["tykCompetitors"], 
      				"odu", 
        			"odu.xlsx"
        		}
        	]
        ]
	}
	, 
	inventory = dsMap[oduFlattenScrape, dsMap[oduScrape, meta]];
	exportExcel[filename, inventory];
	inventory
];



End[] (* End Private Context *)

EndPackage[]