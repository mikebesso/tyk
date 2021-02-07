(* ::Package:: *)



(* ::Input::Initialization:: *)

BeginPackage[
	"tykWarehouse`", 
	{
		"wwBootstrap`",
		"wwStrings`", 
		"wwDates`",
		"wwFileSystem`", 
		"wwCSV`", 
		"wwExcel`", 
		"wwDatasets`", 
		"wwHTML`",  
		"wwEmail`", 
		"chartUtil`", 
		"gridCharts`",
		"tykCommon`",
		"wwMath`"
	}
]


inspect::usage ="";

tykWarehouse$Init::usage = "";
getWarehouseData::usage = "";
analyze::usage = "";
buildReport::usage = "";
publish::usage = "";


downloadFolderPath::usage = "";
downloadWarehouseInventory::usage = "";

warehouseUri::usage = "";
projectFolderPath::usage = "";
downloadFolderPath::usage = "";

scraped::usage = "";

jobPublishQ::usage = ""

downloadFileNamePattern::usage = "";
downloadFileName::usage = ""

tykWarehouse$Run::usage = "tykWarehouse$Run  ";


Begin["`Private`"];

keyScraped= "Scraped";
keyDownloaded = "Downloaded";




keyAsOf = "As Of";
keyRawData = "Raw Data";
keyData = "Data";
keyAnalysis= "Analysis";
keyReport = "Report";


keyFailed = "FailedQ";
keyJobPublishQ = "PublishQ";

keyProjectFolderPath = "Project Folder Path";
keyScrapesFolderPath = "Scrapes Folder Path";
keyScrapeFilePath = "Scrape File Path";
keyCleansedFolderPath = "Cleansed Folder Path";
keyOutputFolderPath = "Output Folder Path";
keyDownloadFilePath = "Download File Path";
keyDownloadFolderPath = "Download Folder Path";
keyReportFilePath = "Report File Path";
keyFailureReason = "Failure Reason";

keyPreviousReportDate = "Previous Report Date";

keyWillEmail = "Will Email";

(*
warehouse[keyProjectFolderPath] :=  ExpandFileName[FileNameJoin[{warehouse[keyCodeFolderPath], ".."}]];

warehouse[keyScrapesFolderPath] := createFolder[FileNameJoin[{warehouse[keyProjectFolderPath], "data", "scrapes"}]];
warehouse[keyScrapeFilePath] := FileNameJoin[{warehouse[keyScrapesFolderPath], StringJoin[{str$Tidy[Today], ".xlsx"}]}];
warehouse[keyCleansedFolderPath] := createFolder[FileNameJoin[{warehouse[keyProjectFolderPath], "data", "cleansed"}]];
warehouse[keyOutputFolderPath] := createFolder[FileNameJoin[{warehouse[keyProjectFolderPath], "Output"}]];
warehouse[keyDownloadFolderPath]  :=  ExpandFileName[FileNameJoin[{$HomeDirectory, "downloads"}]];

warehouse[keyReportFilePath] := FileNameJoin[{warehouse[keyOutputFolderPath], StringJoin[{"warehouse-supplies-",str$Tidy[Today], ".pdf"}]}];
*)

downloadFileName:= "Warehouse Shipping Inventory (Responses).xlsx";
downloadFileNamePattern[] = StringReplace[
downloadFileName,
{
".xlsx" -> "*.*"
}
];



warehouseUri[] =
"https://docs.google.com/spreadsheets/d/1dzY44urkw5jsj6McMo9YG0anRBMcb5mv30gF9GghoII/export";




abort[$warehouse_Association, $reason_String] := Block[
{
warehouse = $warehouse
},
warehouse[keyFailed] = True;
warehouse[keyFailureReason] = $reason;

Throw[warehouse];
];

abortIfFailed[warehouse_Association]:=If[warehouse[keyFailed], Throw[warehouse]];



inspect[warehouse_Association] :=Block[
{},

Grid[
Thread[
{
Keys[warehouse],
ReplaceAll[Values[warehouse], Null-> "Null"]
}
],
Frame -> All,
Alignment->Left
]

];



tykWarehouse$Run[jobPublishQ_?BooleanQ] := Block[
	{},
	
	tykWarehouse$Init[jobPublishQ] // getWarehouseData // 
     analyze  // buildReport // publish // inspect
];


tykWarehouse$ProjectName = "tykWarehouse";



tykWarehouse$Init[jobPublishQ_?BooleanQ] := Block[
	{
		warehouse = <|

			keyProjectFolderPath -> Null,
			keyScrapesFolderPath -> Null,
			keyScrapeFilePath -> Null,
			keyCleansedFolderPath -> Null,
			keyOutputFolderPath -> Null,
			keyDownloadFolderPath -> Null,
			keyReportFilePath -> Null,
			
			keyJobPublishQ -> jobPublishQ,
			
			keyFailed -> False,
			keyFailureReason -> Null,
			
			keyScraped-> False,
			keyDownloaded -> False,
			
			keyRawData -> Null,
			keyAsOf -> Null
		|>
	}, 
	
	tyk$CreateProjectFolders[tykWarehouse$ProjectName];

	warehouse[keyProjectFolderPath] = tyk$ProjectFolder[tykWarehouse$ProjectName];
	warehouse[keyScrapesFolderPath] = createFolder[
		FileNameJoin[{tyk$DataFolder[tykWarehouse$ProjectName], "scrapes"}]
	];

	warehouse[keyScrapeFilePath] = FileNameJoin[{warehouse[keyScrapesFolderPath], StringJoin[{str$Tidy[Today], ".xlsx"}]}];

	warehouse[keyCleansedFolderPath] = createFolder[
		FileNameJoin[{tyk$DataFolder[tykWarehouse$ProjectName], "cleansed"}]
	];

	warehouse[keyOutputFolderPath] = tyk$OutputFolder[tykWarehouse$ProjectName];
	
	warehouse[keyDownloadFolderPath]  =  tyk$DownloadsFolder;
	
	warehouse[keyReportFilePath] = FileNameJoin[{tyk$OutputFolder[tykWarehouse$ProjectName], StringJoin[{"warehouse-supplies-",str$Tidy[Today], ".pdf"}]}];


	getMatchingDownloads[downloadFileNamePattern[]]// DeleteFile;

	warehouse
];



cleanWarehouseData[$warehouse_Association] := Module[
{
warehouse = $warehouse,

(* helper functions, used only here *)
replaceBadData,
export
},

Catch[

(**)
abortIfFailed[warehouse];

(* we will need to add to this list as we learn what other data entry errors can occur *)
replaceBadData[$data_Dataset] :=ReplaceAll[
$data ,
{
""->0.0,
"01"->1
}
];

(* save the data with a filename that indicates the last date in the file *)
export[$data_Dataset]:=Export[
FileNameJoin[
{
warehouse[keyCleansedFolderPath], 
StringJoin[
$data[[-1]]["Date"], 
".csv"
]
}
], 
$data
];

(* we want a clean date column, named "Date" *)
warehouse[keyData] = With[
{
rawData = warehouse[keyRawData]
},
dsAddColumn[
replaceBadData[rawData],
<| "Date" -> str$Tidy[#"Timestamp"] |>&
][
All,
Prepend[ 
Complement[dsColumnNames[rawData], {"Timestamp"} ], 
"Date"
]
]
];

(* 
Since we want dates to look like dates, 
we have to export before changing the type
*)
export[warehouse[keyData]];

(**)
warehouse[keyData] =dsSetDateColumnTypes[warehouse[keyData]];

(**)
Throw[warehouse];
]

];


getWarehouseData[$warehouse_Association] := Module[
	{
	warehouse = $warehouse
	},

	Catch[

		(**)
		abortIfFailed[warehouse];
		
		
		
		(**)
		warehouse = downloadWarehouseInventory[warehouse];
		
		(*
		If we have a good scrape, save it with previous scrapes;
		
		Since we deleted previously downloaded files during initialization, we do not have to worry about getting more than one.
		*)
		If[
			warehouse[keyScraped],
			CopyFile[getMatchingDownloads[downloadFileNamePattern[]][[1]],warehouse[keyScrapeFilePath],OverwriteTarget->True],
			abort[warehouse, "Scrape failed"]
		];
		
		(**)
		If[
			FileExistsQ[warehouse[keyScrapeFilePath]],
			warehouse[keyDownloaded] = True,
			abort[warehouse, "Download failed"]
		];



		(**)
		warehouse[keyRawData] = importDatasetFromExcelSheet[warehouse[keyScrapeFilePath],"Form Responses 1"];
		
		(*
		Grab the latest date in the file and consider it our "as of" date.
		*)
		warehouse[keyAsOf] = Max[dsColumnValues[warehouse[keyRawData], "Timestamp"]];
		
		(**)
		warehouse = cleanWarehouseData[warehouse];
		
		(**)
		Throw[warehouse];
	]

];




loginGoogleDrive[session_] := Module[ 
{},
WebExecute[
session, {
"OpenPage" -> "https://drive.google.com/drive/u/1/my-drive",
"TypeElement"-> {"CSSSelector"->"#identifierId", "m.besso@tykables.com"},
"ClickElement" -> "CSSSelector"->"#identifierNext"
}
];

Pause[2];

WebExecute[
session, {
"TypeElement"-> {"CSSSelector"->"#password input", "Tyk@bl35"},
"ClickElement" -> "CSSSelector"->"#passwordNext"
}
];

Pause[3];
];

downloadGoogleDrive[session_, uri_String , fileNamePattern_String]:= Module[
{},
WebExecute[session, {"OpenPage" ->uri}];
Pause[2];

loopCounter = 5;
While[
(loopCounter > 0) \[And]hasMatchingDownloads[fileNamePattern],
Block[
{},
loopCounter -= 1;
Pause[2];
];

];
hasMatchingDownloads[fileNamePattern]
];


downloadWarehouseInventory[$warehouse_Association] := Module[
	{
	warehouse = $warehouse,
	session
	},


	(*warehouse[keyScraped] = "/Users/mike/projects/tykables/files/tykWarehouse/data/scrapes/Warehouse Shipping Inventory (Responses).xlsx";
	*)
	warehouse[keyScraped] = "/Users/mike/projects/tykables/files/tykWarehouse/data/scrapes/2020-10-09.xlsx";
	Return[warehouse];



	(*
	Catch[
		(**)
		abortIfFailed[warehouse];
		
		(**)
		session = StartWebSession["Chrome"];
		
		(**)
		loginGoogleDrive[session];
		
		(**)
		warehouse[keyScraped] = downloadGoogleDrive[session, warehouseUri[], downloadFileNamePattern[]];
		
		(**)
		DeleteObject[session];
		
		(**)
		Throw[warehouse];
	]
*)
];




analyze[$warehouse_Association] :=Module[
{
warehouse = $warehouse,
data,
header,
onHand,
previousHeader,

unshiftedHeader,
unshifted,
shifted,
deltaDays,
deltaCounts
},

Catch[

(**)
abortIfFailed[warehouse];

(*
Tidy up our date;
I'm not sure why we have to do this, will figure that out later.
*)
data = dsAddColumn[
 warehouse[keyData],
<|"Date" -> DayPlus[tdyToDate[#["Date"]],0]|>&
];

(* 
The last row are the current on hand inventory, 
but we want just the values (without the date) and in list form 
*)
onHand = Rest[data[[-1]]]//Values// Normal;

(* 
We will need these later.
*)
header = dsColumnNames[data];
previousHeader = Map[StringJoin["Previous ", #]&, header];
unshiftedHeader = header;


(*
Simplify our code with a "duplicate" dataset with all values shifted down one.
*)

shifted = 
Dataset[
Append[
data // Normal,
Last[data // Normal]
]
];


(* 
Simplify our code with by assuming that before we started, we had nothing.
*)

unshifted =Block[
{
zeros =  Prepend[ 
Table[0, Length[unshiftedHeader]-1],
DayPlus[Min[dsColumnValues[data, "Date"]] , -1]],

initialZeros
},
initialZeros = Dataset[
Association[
Thread[unshiftedHeader ->zeros]
]
];

Dataset[
Prepend[
data // Normal,
initialZeros // Normal
]
]
];


deltaCounts = Dataset[
Table[
If[
unshifted[row,col] >shifted[row ,col], 
unshifted[row,col] -shifted[row ,col], 
Missing["NA"]
], 
{row, 1, Length[shifted] - 1}, 
{col,
 2, Length[header] } 
]
];


deltaDays = Dataset[
Table[
If[
!MissingQ[deltaCounts[row,col]], 
wwNFloor[QuantityMagnitude[DateDifference[unshifted[row,1],shifted[row ,1]]]], 
Missing["NA"]
], 
{row, 1, Length[shifted] - 1}, 
{col, 1, Length[header]  -1} 
]
];

(*
Put it all together.
*)

warehouse[keyAnalysis] = 
 Dataset[

MapThread[

With[
{
daysRemaining = wwNFloor[tdyDivide[#2, #3]]
},
 <|
"Supply" -> #1,
"On Hand" -> #2, 
"Velocity" -> Round[#3, 0.01], 
"Days Remaining" -> daysRemaining,
"Need 7 Days" -> If[ (daysRemaining< 7),  wwNCeiling[(7 * #3 )-#2], 0],
"Need 14 Days" -> If[ (daysRemaining< 14),  wwNCeiling[(14* #3 )-#2], 0],
"Need 30 Days" -> If[ (daysRemaining< 30),  wwNCeiling[(30 * #3 )-#2], 0]

|>
]&,
{
header[[2;;Length[header] ]],
onHand,
Table[
tdyDivide[
Total[DeleteMissing[deltaCounts[All, col]]],
Total[DeleteMissing[deltaDays[All, col]]],
"default"-> 0],
{col, 1,Length[header] - 1}
]
}
]

];

Throw[warehouse];

]

];




buildReport[$warehouse_Association] := Block[
{
warehouse = $warehouse,

reportBody =  tdyToGrid[$warehouse[keyAnalysis],"paging" -> False],
buildReportCaption
},

(**)
buildReportCaption[asOf_DateObject] :="The table below was generated on " <> DateString[Today] <> " with inventory counts as of "<> DateString[asOf] <> ".";

(**)
Catch[

(**)
abortIfFailed[warehouse];


(*
*)
warehouse[keyReport] = Column[
{
" ",
"Warehouse Supplies Inventory Report",
buildReportCaption[warehouse[keyAsOf]],
reportBody, 
" "
},
Spacings -> 3,
Alignment -> Center
];

(**)
Throw[warehouse];

]
];


publish[$warehouse_Association ] := Module[
{
warehouse = $warehouse,
previousReportDate,

previousReportFileNames = Map[
FileBaseName, 
FileNames["*.PDF", $warehouse[keyOutputFolderPath]]
]
},

Catch[

(**)
abortIfFailed[warehouse];

(*
Determine most recent report; 
If none, assume a long time ago.
*)

warehouse[keyPreviousReportDate]=If [
Length[previousReportFileNames] > 0,
Max[
tdyToDate[
Map[
StringTake[#, -10]&,
previousReportFileNames
]
]
],
DateObject["2000-01-01"]
];

(**)
Export[warehouse[keyReportFilePath], warehouse[keyReport], "PDF"];

(**)
If[
\[Not]FileExistsQ[warehouse[keyReportFilePath]],
abort[warehouse, "Report file not found"]
];

(*
We only send email if we have a new report.
*)
warehouse[keyWillEmail] =  warehouse[keyJobPublishQ] \[And] (warehouse[keyAsOf] >warehouse[keyPreviousReportDate]);

(**)
If[
warehouse[keyWillEmail] ,
sendGmail[
{"j.williams@tykables.com", "m.besso@tykables.com"}, 
"Warehouse Supplies Inventory Report", 
"OK...time to automate this", 
"AttachedFiles" -> {warehouse[keyReportFilePath]}
]
];

(**)
Throw[warehouse]

]

];

End[];

EndPackage[];




