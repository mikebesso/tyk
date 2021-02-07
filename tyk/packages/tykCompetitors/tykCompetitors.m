(* Wolfram Language Package *)

(* Created by the Wolfram Workbench Jun 19, 2020 *)

BeginPackage[
	"tykCompetitors`",  
	{
		"tykCommon`", 
		"ww`", 
		"wwFileSystem`", 
		"wwExcel`", 
		"wwStrings`", 
		"wwDatasets`", 
		"wwLists`"
	}
];



(*
tdyURLDownload::usage = "";
downloadPage::usage = "";
publishStageToScrapes::usage = "";
*)


tdyKeySortBySize::usage = "";

todaysScrapes::usage = "";

tykCompetitors$GetConfiguration::usage = "";




tykCleanColor::usage = "";
tykCleanPattern::usage = "";

tykCompetitors$Folder::usage = "";
tykCompetitors$MetadataFolder::usage = "";

tykCompetitors$DataFolder::usage = "tykCompetitors$DataFolder  ";

tykCompetitors$OutputFolder::usage = "tykCompetitors$OutputFolder  ";

tykCompetitors$StageFolder::usage = "tykCompetitors$StageFolder  ";

tykCompetitors$HtmlFolder::usage = "tykCompetitors$HtmlFolder  ";

tykCompetitors$TodaysScrapesFolder::usage = "tykCompetitors$TodaysScrapesFolder  ";

tykCompetitors$CleansedFolder::usage = "tykCompetitors$CleansedFolder  ";

tykCompetitors$ParsedFolder::usage = "tykCompetitors$ParsedFolder  ";


tykCompetitors$InitializeFolders::usage = "tykCompetitors$InitializeFolders  ";
tykCompetitors$ProjectName::usage = ""


tykCompetitors$Options

Begin["`Private`"];


tykCompetitors$ProjectName =  "tykCompetitors";

tykCompetitors$Options = {
	"TestMode" -> False, 
	"Verbose" -> False
};

Options[tykCompetitors$InitializeFolders] = tykCompetitors$Options;



tykCompetitors$InitializeFolders[company_String, OptionsPattern[]] := Block[
	{
		retVal
	},
	
	tyk$TestMode = OptionValue["TestMode"];
	
	retVal = tyk$CreateProjectFolders[tykCompetitors$ProjectName];

	tykCompetitors$Folder = tyk$ProjectFolder[tykCompetitors$ProjectName];
	tykCompetitors$MetadataFolder = tyk$MetadataFolder[tykCompetitors$ProjectName];
	tykCompetitors$DataFolder = tyk$DataFolder[tykCompetitors$ProjectName];
	tykCompetitors$OutputFolder = tyk$OutputFolder[tykCompetitors$ProjectName];
	
	
	(* 
		As we download files, we will stage them by company.
		
		This will:
		
			- Allow us to restart an import
			- Protect previously downloaded files
	*)
	tykCompetitors$StageFolder = createFolder[
    	FileNameJoin[ 
    		{
    			tykCompetitors$DataFolder, 
    			"stage", 
    			company
    			}
    		]
    	];
    assertFolderExists[tykCompetitors$StageFolder];
    
    	
   	(* 
   	
   	*)
	tykCompetitors$HtmlFolder = createFolder[
    	FileNameJoin[ 
    		{
    			tykCompetitors$DataFolder, 
    			"scrapes", 
    			company
    			}
    		]
    	];
    assertFolderExists[tykCompetitors$HtmlFolder];
    
    
       
	tykCompetitors$TodaysScrapesFolder = createFolder[
		FileNameJoin[
			{
				tykCompetitors$HtmlFolder, 
				str$Tidy[Today]
			}
		]
	];
	assertFolderExists[tykCompetitors$TodaysScrapesFolder]; 	
    
    	
	tykCompetitors$CleansedFolder = createFolder[
    	FileNameJoin[ 
    		{
    			tykCompetitors$DataFolder, 
    			"cleansed"
    			}
    		]
    	];   
	assertFolderExists[tykCompetitors$CleansedFolder]; 	
	

	tykCompetitors$ParsedFolder = createFolder[
    	FileNameJoin[ 
    		{
    			tykCompetitors$DataFolder, 
    			"parsed"
    			}
    		]
    	];   
	assertFolderExists[tykCompetitors$ParsedFolder]; 		

	retVal = Join[
		retVal,
		<|
			"Stage" -> tykCompetitors$StageFolder,
			"Cleansed" -> tykCompetitors$CleansedFolder,
			"Parsed" -> tykCompetitors$ParsedFolder,
			"Html" -> tykCompetitors$HtmlFolder,
			"TodaysScrapes" -> tykCompetitors$TodaysScrapesFolder
		|>
	];
	
	retVal
];






Attributes[tykCleanColor] = {Listable};
tykCleanColor[""] = "";
tykCleanColor[color_String] := 
  tdyTitleCase[
   StringReplace[color,
    {
     "_" -> " ",
     "-" -> " "
     }
    ]
   ];

Attributes[tykCleanPattern] = {Listable};
tykCleanPattern[""] = "";
tykCleanPattern[pattern_String] := 
  tdyTitleCase[
   StringReplace[pattern,
    {
     "_" -> " ",
     "-" -> " "
     }
    ]
   ];


tykCompetitors$MetaDataFile[] := FileNameJoin[{tykCompetitors$MetadataFolder, "competitors.xlsx"}]

tykCompetitors$GetProductMetadata[company_String] := Block[
	{
		retVal
	},
	
	assertFileExists[tykCompetitors$MetaDataFile[]];
	
	retVal = importDatasetFromExcelSheet[
      tykCompetitors$MetaDataFile[], 
      company
    ];
    
    retVal
	
];

tykCompetitors$GetCompanyMetadata[company_String] := Block[
	{
		retVal
	},
	
	assertFileExists[tykCompetitors$MetaDataFile[]];
	
	retVal = dsSelectOne[
      
      importDatasetFromExcelSheet[
       tykCompetitors$MetaDataFile[], 
       "companies"
       ],
      #["Company"] == company &
      
      ];
      
    retVal["Regions"] = StringTrim[StringSplit[retVal["Regions"], ","]];
    retVal["Pages"] =  StringTrim[StringSplit[retVal["Pages"], ","]];
     
	retVal
    
];




Options[tykCompetitors$GetConfiguration] = tykCompetitors$Options;


tykCompetitors$GetConfiguration[company_String, opt:OptionsPattern[]] := Module[
   {
    productMetadata,
    companyMetadata 
    },
   
   
   
   
   
   Catch[
   
   	tykCompetitors$InitializeFolders[company, opt];
   
    productMetadata = tykCompetitors$GetProductMetadata[company];
    companyMetadata = tykCompetitors$GetCompanyMetadata[company];
    
	
     
    
    Throw[
     <|
      "Company" -> company,
       
      "TestMode" -> OptionValue["TestMode"],
      
   
      "Product Metadata" -> productMetadata,
      "Company Metadata" -> companyMetadata,
      
      "Base URL" -> companyMetadata["URL"],
      
      "Regions" -> StringSplit[companyMetadata["Regions"], ","],
      "Pages" -> StringSplit[companyMetadata["Pages"], ","]
      

      |>
     ]
    
    
    ]
   
   ];



tykCompetitors$GetConfiguration[___] = Failure[
   "getTykConfiguration failed", 
   <|
    "MessageTemplate"  ->  "invalid parameters",
    "MessageParameters" -> {}
    |>
   ];



sizeSortOrder["S"] = 1;
sizeSortOrder["M"] = 2;
sizeSortOrder["ML"] = 3;
sizeSortOrder["L"] = 4;
sizeSortOrder["LL"] = 5;
sizeSortOrder["XL"] = 6;
sizeSortOrder["XLL"] = 7;
sizeSortOrder["2XL"] = 8;
sizeSortOrder["2XLL"] = 9;
sizeSortOrder["3XL"] = 10;
sizeSortOrder["3XLL"] = 11;


tdyKeySortBySize = KeySortBy[sizeSortOrder];






End[]; (*End Private Context*)

EndPackage[];

