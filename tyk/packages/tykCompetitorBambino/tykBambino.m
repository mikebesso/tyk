(* Wolfram Language Package *)

BeginPackage[
	"tykBambino`", 
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

bambinoScrapeInventory::usage = "bambinoScrapeInventory  "
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 


bambinoScrape[meta_Association] := Module[
	{
		url = 
			URLBuild[{"https://bambinodiapers.com", "products", 
			meta["Path"]}]
		, scrape
		, parsed
    }
	,
   
	scrape = With[
		{
			contents = httpDownloadString[url]
			, startJson = "<script class=\"product-json\" type=\"application/json\">"
      		, endJson  = "</script>"
		}
		,
		StringCases[contents, 
		Shortest[startJson  ~~ data___ ~~ endJson] -> data, 
		IgnoreCase -> False]
	];
   
	If[Length[scrape] == 0, Return[Nothing]];
   
	parsed = With[
		{
			json = ImportString[First@scrape, "RawJSON"]
		}
		,
		Map[
			KeyTake[ {"title", "option2", "available", "inventory_quantity", "price"}]
			, 
			Select[json["variants"], #["option2"] == meta["Packaging"] &  ]
		]
	];
   
	Join[meta, <| "JSON" -> parsed |>]
   
];

bambinoFlattenScrape[data_Association] := Module[
	{
		mapVariants
	}
	,
	mapVariants[variant_Association] := With[
		{
			size = First@StringSplit[variant["title"], " "]
			, bags = ToExpression[First@StringSplit[variant["option2"], " "]]
		}
		,
		Join[
			KeyDrop[data, {"JSON", "Packaging"}],
			<|
				"Date" -> dtToday[],
				"Size" -> size, 
				"Quantity" -> bags, 
				"Available" -> variant["available"], 
				"On Hand" -> variant["inventory_quantity"] * bags, 
				"Product Key" -> data["Product Key"] ~~ "-" ~~ size
			|>
		]
	];
   
	Map[mapVariants, data["JSON"]]
   
];


bambinoScrapeInventory[] := Module[
	{
		meta = excelImport["bambino-meta.xlsx"]
		, inventory
		, filename = fs$AddDateToFileName[FileNameJoin[{tyk$DataFolder["tykCompetitors"], "bambino", "Bambino.xlsx"}]]
	}
	,
	inventory = dsMap[bambinoFlattenScrape, dsMap[bambinoScrape, meta]];
	exportExcel[filename, inventory];
	inventory
];



End[] (* End Private Context *)

EndPackage[]