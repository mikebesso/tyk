(* Wolfram Language Package *)

BeginPackage[
	"tykCompetitorsData`",
	{
			"tyk`",
		"tykCommon`", 
		"tykCompetitors`",
		"ww`", 
		"wwBootstrap`",
		"wwAssociations`",
		"wwFileSystem`", 
		"wwExcel`", 
		"wwStrings`", 
		"wwDatasets`", 
		"wwLists`",
		"wwMessages`"
	}
]
(* Exported symbols added here with SymbolName::usage *)  

tdyURLDownload::usage = "";
downloadPage::usage = "";
publishStageToScrapes::usage = "";

tykCompetitors$GetData::usage = "tykCompetitors$GetData  ";


Begin["`Private`"] (* Begin Private Context *) 



publishStageToScrapes[config_Association] := Module[
	{
		publishFile
	},
	publishFile[filePath_String] := Module[
	{
	  filename = Last[FileNameSplit[filePath]],
	  destination =  tykCompetitors$TodaysScrapesFolder,
	  result
	},
     result = CopyFile[filePath, FileNameJoin[{destination, filename}], "OverwriteTarget" -> True];
     If[
     	! FailureQ[result],
     	DeleteFile[filePath]
     ];
     
     result
    ];
   
   FileSystemScan[publishFile, tykCompetitors$StageFolder]
   
];


tdyURLDownload[url_String, path_String] := Module[
	{
	}
	,
   
	Pause[RandomInteger[{1, 3}]];
	
	URLDownload[
		url, 
		path
	];
    
	url -> path
   
];


downloadPages[pages_List] := Map[downloadPage, pages];
downloadPages[args___] := msgDefinitionNotFound["downloadPages", args];

downloadPage[$data_Association] := Module[
   {
    path = $data["filePath"],
    url = $data["url"],
    result 
    },
 
   result = 
	   Catch[
	    If[
	     FileExistsQ[path],
	     url -> "Skipped",
	     Throw[
	      tdyURLDownload[url, path]
	      ]
	     ]
	    ];
	    
	(*Echo[result, "GetData"];*)
	
	Nothing
];
   
downloadPage[args___] := msgDefinitionNotFound["downloadPage", args];





(* TODO: pull this from metadata *)

selectByRegion[urls_List, region_String] := Select[urls, StringStartsQ[#["url"], "https://" <> region] &];
splitByRegion[urls_List] := {
	selectByRegion[urls, "au"]
	, selectByRegion[urls, "eu"]
	, selectByRegion[urls, "jp"]
	, selectByRegion[urls, "mx"]
	, selectByRegion[urls, "uk"]
	, selectByRegion[urls, "us"]
}

 Options[tykCompetitors$GetData] = tykCompetitors$Options;
 
tykCompetitors$GetData[OptionsPattern[]] := Module[
	{
		testMode = OptionValue["TestMode"],

		config,
		baseUrl,
		pages,
		regions,
		combos,
		urls,
		results
	}
	,
	

	
	Print["Getting Data"];
	config = tykCompetitors$GetConfiguration["abu", "TestMode" -> testMode];
	
	baseUrl = config["Base URL"];
	pages = config["Company Metadata"]["Pages"];
	regions = config["Company Metadata"]["Regions"];
	
	Echo[baseUrl, "base URL"];
	
	combos = Join @@ Outer[List, regions, pages];
	
	urls = Block[
		{
			constructor = <|
				"url" -> StringReplace[
					baseUrl,
					{
						"%REGION%" -> #[[1]],
	 					"%PAGE%" -> #[[2]]
					}
				], 
				"page" -> #[[2]], 
				"country" -> #[[1]],
				"filePath" -> FileNameJoin[
					{
						tykCompetitors$StageFolder, 
						StringJoin[{#[[1]], "-", #[[2]], ".html"}]
					}
				]
			|> &
		},
		Map[constructor, combos] // RandomSample
	];

	urls = listTdyTake[urls, If[testMode, 10, All]];
	
	
	Echo[Length[urls], "Number of URLs"];
	
	results = Block[
		{
			temp
			, regionalUrls = splitByRegion[urls]
		}
		,
		
		Echo[Length[regionalUrls], "Number of Regions"];
		LaunchKernels[];
		DistributeDefinitions[downloadPages];
		temp = AbsoluteTiming[
			ParallelMap[downloadPages, regionalUrls]
		];
		<|
			"Timing" -> Quantity[First[temp], "Seconds"]
			, "PageCount" -> Length[Flatten[Rest[temp]]]
		|>
		
	];
	
	publishStageToScrapes[config];
	results
];



End[] (* End Private Context *)

EndPackage[]