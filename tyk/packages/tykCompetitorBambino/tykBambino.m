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
		
		(* Helper Functions *)
		, filterPackaging
    }
	,
   
    (* We only have "option2" if there are different types of packaging *)
   	filterPackaging = If[
   		meta["Packaging"] == "Single"
   		, 
   		True &
   		,
   		#["option2"] == meta["Packaging"] &
   	];
   		
   		
   
   
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
			Select[json["variants"], filterPackaging]
		]
	];
   
   
	Join[meta, <| "JSON" -> parsed |>]
   
];




bambinoFlattenScrape[data_Association] := Module[
	{
		(* helper functions *)
		  mapVariants
		, parseBags
		, parseSize
	}
	,
	
	(* Default to "One Size" if product does not have sizes *)
	parseSize[variant_Association] := If[
		data["Has Sizes"]
		,
		First@StringSplit[variant["title"], " "]
		,
		"One Size"
	];

	(* Default to single quantity if product does not have multiple packagings *)
	parseBags[variant_Association] := If[
		data["Packaging"] == "Single"
		,
		1
		,
		ToExpression[First@StringSplit[variant["option2"], " "]]
	];
	
	
	mapVariants[variant_Association] := With[
		{
			size = parseSize[variant]
			, bags = parseBags[variant]
		}
		,
		
		Join[
			KeyDrop[data, {"JSON", "Packaging", "Has Sizes"}],
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