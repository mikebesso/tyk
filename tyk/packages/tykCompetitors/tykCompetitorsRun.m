(* Wolfram Language Package *)

BeginPackage[
	"tykCompetitorsRun`",
		{
		"tykCommon`", 
		"tykCompetitors`",
		"tykCompetitorsData`",
		"tykCompetitorsParse`",
		"tykCompetitorsAnalyze`",
		"ww`", 
		"wwBootstrap`",
		"wwAssociations`",
		"wwFileSystem`", 
		"wwExcel`", 
		"wwStrings`", 
		"wwDatasets`", 
		"wwLists`"
	}	
]

tykCompetitors$Run::usage = "tykCompetitors$Run  "
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 




Options[tykCompetitors$Run] = tykCompetitors$Options;

tykCompetitors$Run[OptionsPattern[]] := Module[ 
	{
		testMode = OptionValue["TestMode"]
	}
	,
	
	Enclose[

		tykCompetitors$GetData["TestMode" -> testMode];
		tykCompetitors$Parse["TestMode" -> testMode];
		tykCompetitors$Analyze["TestMode" -> testMode];
		
	
	]
]


End[] (* End Private Context *)

EndPackage[]