(* Wolfram Language Package *)

(* Created by the Wolfram Workbench Jun 14, 2020 *)

BeginPackage[
	"tykCommon`", 
	{
		"ww`",
		"wwFileSystem`",
		"wwStrings`",
		"wwBootstrap`"
	}
];

tyk$Sizes::usage = "";

buildTykablesNotebook::usage = "";
mapSizeToSizeOrder::usage = "";
mapSizeOrderToSize::usage = "";
lookupSizeOrder::usage = "";

mapSizeToAbbreviation::usage = "";
lookupSizeAbbreviation::usage = "";


chapterCell::usage = "";
sectionCell::usage = "";
textCell::usage = "";
pageBreakCell::usage = "";
outputCell::usage = "";


tyk$FilesFolder::usage = "";
tyk$ProjectFolder::usage = "";
tyk$DataFolder::usage = "";
tyk$MetadataFolder::usage = "";
tyk$OutputFolder::usage = "";

tyk$CreateProjectFolders::usage = "";

tyk$TestMode::usage = ""

tyk$DownloadsFolder::usage = "tyk$DownloadsFolder  "

tyk$ParseGender::usage = "parse  ";
tyk$ParseSize::usage = "parse  "

tyk$TemplatesFolder::usage = "tyk$TemplatesFolder  "

tyk$HyperlinkToSkulabs::usage = "tyk$HyperlinkToSkulabs  ";

tyk$CleanOrderNumber::usage = "tyk$CleanOrderNumber "

tyk$CleanTrackingNumber::usage = "tyk$CleanTrackingNumber  ";


Begin["`Private`"];

tyk$TestMode = False;



tyk$CleanTrackingNumber[trackingNumber_] := str$Tidy[trackingNumber];

tyk$CleanOrderNumber[orderNumber_String] := StringReplace[str$Tidy[orderNumber], "T#" -> ""];


tyk$HyperlinkToSkulabs[orderNumber_String] := With[
	{
		text = tyk$CleanOrderNumber[orderNumber]
	},
	Hyperlink[
		text, 
		StringJoin[
			"https://app.skulabs.com/order?store_id=58535cbf05e51010205d7932&order_number=",
			text
		]
	]
];

tyk$HyperlinkToSkulabs[orderNumber_Integer] := tyk$HyperlinkToSkulabs[str$Tidy[orderNumber]];
	

tyk$FilesFolder = FileNameJoin[
	{
	    $HomeDirectory,
	    "projects", 
	    "tykables",
	    "files"
    }
];

tyk$DownloadsFolder = getDownloadFolder[];
	    
tyk$ProjectFolder[project_String] := FileNameJoin[
	{
		tyk$FilesFolder,
		project
	}
];
	   
tyk$MetadataFolder[project_String] := FileNameJoin[
	{
		tyk$ProjectFolder[project],
		"metadata"
	}
];

tyk$DataFolder[project_String] := FileNameJoin[
	{
		tyk$ProjectFolder[project],
		If[tyk$TestMode, "data-test", "data"]
	}
];

tyk$OutputFolder[project_String] := FileNameJoin[
	{
		tyk$ProjectFolder[project],
		If[tyk$TestMode, "output-test", "output"]
	}
];


tyk$CreateProjectFolders[project_String] := Block[
	{
		retVal
	},
	
	retVal =
	<|
		"Project" -> createFolder[tyk$ProjectFolder[project]],
		"Metadata" -> createFolder[tyk$MetadataFolder[project]],
		"Data" -> createFolder[tyk$DataFolder[project]],
		"Output" -> createFolder[tyk$OutputFolder[project]]
	|>;
	
	
	Map[
		Assert[DirectoryQ[#]]&,
		Values[retVal]
	];
	
	retVal

];


pageHeaders[$title_String] :=Table[
{
$title, 
None,
"Tykables"
},
2
];

pageFooters[] :=Table[
{
None,
None,
"Tykables"
},
2
];


Options[buildTykablesNotebook] = {
"title" -> "A Yet To Be Titled Tykables Report",
"PrintOrientation" -> "Portait"
};

buildTykablesNotebook[cells_, OptionsPattern[]] := Block[
	{
		title= OptionValue["title"],
		orientation = OptionValue["PrintOrientation"]
	},
	DocumentNotebook[
		cells,
		PageHeaders -> pageHeaders[title],
		PageFooters -> pageFooters[];
		PrintingOptions -> {
			"PaperOrientation"->orientation,
			"FirstPageHeader" -> True,
			"FirstPageFooter" -> True,
			"PrintingMargins" -> {{36,36}, {36, 36}}
		}
	]
];
buildTykablesNotebook[___, OptionsPattern[]] := Failure["buildTykablesNotebook", "Message Template"->"Pattern Not Found" ];

tyk$Sizes = {"XS", "S", "M", "M-L", "L", "L-L", "XL", "XL-L", "2XL", "3XL", "4XL"};


mapSizeToAbbreviation = <|
"One Size" -> "",
"XS" -> "XS",
"X-Small" -> "XS",
"S" -> "S",
"Small" -> "S",
"M" -> "M", 
"Medium" -> "M",
"Med" -> "M",
"M-L" -> "M-L", 
"Medium-L" -> "M-L",
"Medium-Long" -> "M-L",
"Medium Long" -> "M-L",
"MediumLong" -> "M-L",
"Med-L" -> "M-L",
"L" -> "L",
"Large" -> "L",
"L-L" -> "L-L",
"Large-L" -> "L-L",
"Large-Long" -> "L-L",
"Large Long" -> "L-L",
"XL" -> "XL",
"X-Large" -> "XL",
"XL-L" -> "XL-L",
"XL-Long" -> "XL-L",
"X-Large Long" ->  "XL-L",
"2XL" -> "2XL",
"2X-Large" -> "2XL",
"XXL" -> "2XL",
"3XL" -> "3XL",
"3X-Large" -> "3XL",
"XXXL" -> "3XL",
"4XL" -> "4XL",
"4X-Large" -> "4XL",
"XXXXL" -> "4XL"
|>;

lookupSizeAbbreviation=Function[{size}, Lookup[mapSizeToAbbreviation, size]];

mapSizeToSizeOrder = With[
{
sizes = {
"One Size",
"X-Small" ,
"XS" ,
"Small" ,
"S",
"Medium",
"M",
"Med",
"M-L",
"Med-Long",
"Med Long",
"Medium-Long",
"Medium Long",
"Large",
"L" ,
"Large Long",
"Large-Long",
"L-L",
"L-Long" ,
"X-Large",
"XL" ,
"XL-L",
"2X-Large",
"2XL" ,
"3X-Large",
"3XL",
"4XL"
}
},

Association[MapIndexed[
#1 -> First[#2] &,
sizes
]
]
];

mapSizeOrderToSize = AssociationThread[Values[mapSizeToSizeOrder], Keys[mapSizeToSizeOrder]];

lookupSizeOrder =Function[{size}, Lookup[mapSizeToSizeOrder, size]];


tyk$ParseSize[product_String] := Catch[
   throwIfStringContains[product, "3XL"];
   throwIfStringContains[product, "2XL"];
   throwIfStringContains[product, "XL"];
   throwIfStringContains[product, "XLlong", "XL-L"];
   throwIfStringContains[product, "XL2", "XL"];
   throwIfStringContains[product, "largelong", "L-L"];
   throwIfStringContains[product, "large", "L"];
   throwIfStringContains[product, "medlong", "M-L"];
   throwIfStringContains[product, "med", "M"];
   throwIfStringContains[product, "small", "S"]; 
   throwIfStringContains[product, "28 - 30", "S"];
   throwIfStringContains[product, "31 - 33", "M"];
   throwIfStringContains[product, "34 - 36", "L"];
   throwIfStringContains[product, "37 - 39", "XL"];
   throwIfStringContains[product, "40 - 42", "XL"];
   throwIfStringContains[product, "43 - 46", "3XL"];
   Throw[""];
   ];
 tyk$ParseSize[___] = "";  


tyk$ParseGender[product_String] := With[
   {
    hasMasc = StringContainsQ[product, "masc"],
    hasFem = 
     StringContainsQ[product, "fem"] \[Or] 
      StringContainsQ[product, "pink"]
    },
   
   Catch[
    If[hasMasc \[And] hasFem, Throw["Both"]];
    If[hasMasc, Throw["Masc"]];
    If[hasFem, Throw["Fem"]];
    
    Throw["Both"];
    ]
   ];

chapterCell[title_String] := TextCell[title, "Chapter", PageBreakAbove->True];
sectionCell[title_String] := TextCell[title, "Section"];
textCell[text_String] := TextCell[text, "Text"];
pageBreakCell[] := TextCell["", "Output", PageBreakBelow-> True];


Options[outputCell] = Options[ExpressionCell];
outputCell[widget_, opt:OptionsPattern[]] := ExpressionCell[widget, "Output", opt];
outputCell[Nothing, opt:OptionsPattern[]] = Nothing;


End[]; (*End Private Context*)

EndPackage[];
