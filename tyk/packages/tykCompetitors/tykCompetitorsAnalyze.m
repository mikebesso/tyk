(* Wolfram Language Package *)

BeginPackage[
	"tykCompetitorsAnalyze`",
	{
		"tykCommon`", 
		"tykCompetitors`",
		"ww`", 
		"wwBootstrap`",
		"wwAssociations`",
		"wwFileSystem`", 
		"wwMessages`",
		"wwExcel`", 
		"wwCSV`", 
		"wwStrings`", 
		"wwDatasets`", 
		"wwLists`",
		"wwInventory`",
		"wwDates`"
	}	
]

tykCompetitors$Analyze::usage = "tykCompetitors$Analyze  "


(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 


impute[rawData_Dataset, key_String] := Block[
	{
		data = rawData[Select[#["Key"] == key &], All]
	},
	
	(*Echo[{key, Length[data]}, "Impute"];*)
	
	If[
		Length[data] > 12
		,
		imputeSales[
			data, 
			"Date", 
			"On Hand", 
			"TransferThresholdRatio" -> 2, 
			"TransferDelayAfterRestock" -> 0
		] // Normal
    	,
		Nothing
	]
];



config = Null;
productMetadata = Null;



Options[tykCompetitors$Analyze] = tykCompetitors$Options;

tykCompetitors$Analyze[OptionsPattern[]] := Module[ 
	{
		testMode = OptionValue["TestMode"],
		data,
		keys,
		filteredKeys,
		imputed,
		result
	}
	,
	config = tykCompetitors$GetConfiguration["abu", "TestMode" -> testMode];
	productMetadata = config["Product Metadata"];

	data = excelImportFolder[
		FileNameJoin[{tykCompetitors$ParsedFolder, "abu"}]
		][
			All,
 			<|
     			#,
				"Date" -> dtAsDay[#["Date"]]
			|> &
		];

		keys = 
			Union @
			Normal @
			data[All, "Key"];

	
	filteredKeys = keys;

	(*
	filteredKeys = Select[keys, StringContainsQ["cri-crinklz-m"]];
	*)
	
	imputed = 
		Dataset @ 
		listFlatten @ 
		ParallelMap[
			impute[data, #] &,
			filteredKeys
		];

	Echo[Length[imputed], "Length[imputed]"];

	result = dsInnerJoin[
		imputed[
			All, 
			Complement[dsColumnNames[imputed ], {"Key", "Color", "Pattern"}]
		],
    	productMetadata[
     		All, 
     		{"Page", "Category", "Subcategory", "Packaging"}
     	],
    	"Page"
	][
		All,
		<|
			#,
			"Sales" -> #["Price"] * #["Sold"],
			"Orders" -> If[
				#["Category"] == "Incontinence"
				, 
				Ceiling[#["Sold"] / 4.0]
				, 
       			1.0
       		]
		|> &
   ] ;

	Echo[Length[result], "Length[result]"];

	Echo[tykCompetitors$OutputFolder, "Output Folder"];

	csvExport[
		File[
			FileNameJoin[
				{tykCompetitors$OutputFolder, "sku-velocity.csv"}
			]
		],
  		result[Select[#["Latest"] &], All] 
  	];
  	
	csvExport[
  		File[
  			FileNameJoin[
  				{tykCompetitors$OutputFolder, "sku-sales.csv"}
  			]
  		],
  
  		result
 
	];

];

End[] (* End Private Context *)

EndPackage[]