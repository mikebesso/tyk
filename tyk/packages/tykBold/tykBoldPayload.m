(* Wolfram Language Package *)

BeginPackage["tykBoldPayload`", 
	{
		"tykCommon`",
		"wwPayload`",
		"wwDates`",
		"wwFileSystem`"
	} 
]


tykBold$ProjectName::usage = "";

tykBold$TestMode::usage = "tykBold$TestMode  "

tykBold$Inspect::usage = "tykBold$Inspect  "

tykBold$Payload::usage = "tykBold$Payload  "


tykBold$SnappiesOnlyCount::usage = "";
tykBold$SnappiesDiapersCount::usage = "";
tykBold$SnappiesOutfitsCount::usage = "";
tykBold$SnappiesDiapersOutfitsCount::usage = "";
tykBold$LittlesGearCount::usage = "";

tykBold$DiapersBoxSubscriptions::usage = "";
tykBold$DiapersBoxCount::usage = "";
tykBold$DiapersBoxAverageDuration::usage = "";

tykBold$DiapersBagSubscriptions::usage = "";
tykBold$DiapersBagCount::usage = "";
tykBold$DiapersBagAverageDuration::usage = "";

tykBold$SubscriptionsCount::usage = "";
		
tykBold$SubscriptionSummaryTable::usage = "tykBold$SubscriptionSummaryTable  ";


tykBold$DataFolder::usage = "";
tykBold$OutputFolder::usage = "";
tykBold$RawDataFile::usage = "";

tykBold$ReportPDFFile::usage = "";
tykBold$ReportNotebookFile::usage = "";
tykBold$SubscriptionsExcelFile::usage = "";
tykBold$Briefs::usage = "";
tykBold$Denim::usage = "";
tykBold$Diapers::usage = "";
tykBold$DiaperBoxes::usage = "";
tykBold$DiaperBoxes::usage = "";
tykBold$DiaperBags::usage = "";
tykBold$LittlesGear::usage = "";
tykBold$LittlesGearDiapers::usage = "";
tykBold$LittlesGearSnappies::usage = "";
tykBold$LittlesGearOutfits::usage = ""

tykBold$UniqueProducts::usage = "tykBold$UniqueProducts  "

tykBold$ByProducts::usage = "tykBold$ByProducts  "

tykBold$Inspect::usage = "tykBold$Inspect  "

tykBold$RawData::usage = "tykBold$RawData  "
tykBold$ActiveSubscriptions::usage = "";
tykBold$PausedSubscriptions::usage = "";
tykBold$InactiveSubscriptions::usage = "";

tykBold$UniqueProducts::usage = "tykBold$UniqueProducts  "



tykBold$StartTime::usage = ""
tykBold$EndTime::usage = "tykBold$EndTime  "
tykBold$RunTime::usage = "tykBold$RunTime  "

tykBold$AsOfDate::usage = "tykBold$AsOfDate  "

tykBold$Today::usage = "tykBold$Today  "

tykBold$DataAge::usage = "tykBold$DataAge  "

tykBold$InitPayload::usage = "tykBold$InitPayload  "

tykbold$DiapersCount::usage = "tykbold$DiapersCount  "
tykbold$DiapersAverageDuration::usage = "  "


tykBold$DiapersSummaryTable::usage = ""
tykBold$LittlesGearSummaryTable::usage = ""

tykBold$TemplatesFolder::usage = "tykBold$TemplatesFolder  "

tykBold$ReportTemplate::usage = "tykBold$ReportTemplate  "

tykBold$LittlesGearAverageDuration::usage = "tykBold$LittlesGearAverageDuration  "

tykBold$DiapersBoxAverageDuration::usage = "tykBold$DiapersBoxAverageDuration  "

tykBold$DiapersAverageDuration::usage = "tykBold$DiapersAverageDuration  "

tykBold$AllAverageDuration::usage = "tykBold$AllAverageDuration  "


tykBold$SnappiesOnlySubscriptions::usage = "  "
tykBold$SnappiesOnlyAverageDuration::usage = "  "

tykBold$SnappiesDiapersSubscriptions::usage = "  "
tykBold$SnappiesDiapersAverageDuration::usage = "  "


tykBold$SnappiesOutfitsSubscriptions::usage = "  "
tykBold$SnappiesOutfitsAverageDuration::usage = "  "
	
tykBold$SnappiesDiapersOutfitsSubscriptions::usage = "  "
tykBold$SnappiesDiapersOutfitsAverageDuration::usage = "  "


(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 



tykBold$Inspect[] := payload$inspect[tykBold$Payload[]];

tykBold$Payload[] := <|

	"Title" -> "A Title",
	
	"Project Name" -> tykBold$ProjectName,
	
	"Test Mode" -> tykBold$TestMode,
	"Start Time" -> tykBold$StartTime,
	"End Time" -> tykBold$EndTime,
	"Run Time" -> tykBold$RunTime, 
	

	"Folders" -> <|
		"Data" -> tykBold$DataFolder,
		"Output" -> tykBold$OutputFolder,
		"Templates" -> tykBold$TemplatesFolder
	|>,
	
	"Files" -> <|
		"Raw Data File" -> tykBold$RawDataFile,
		"Report PDF File" -> tykBold$ReportPDFFile,
   		"Report Notebook File" ->  tykBold$ReportNotebookFile,
   		"Subscrptions Excel File" -> tykBold$SubscriptionsExcelFile
   	|>,


	"Dates" ->    <|
		"As Of Date" -> tykBold$AsOfDate,
		"Today" -> tykBold$Today,
		"Data Age" -> tykBold$DataAge
	|>,

	"Subscription Data" -> <|
			"Raw Data" -> tykBold$RawData,
			"Active" -> tykBold$ActiveSubscriptions,
			"Paused" -> tykBold$PausedSubscriptions,
			"Inactive" -> tykBold$InactiveSubscriptions
	|>,


	
	"Unique Products" -> tykBold$UniqueProducts,

	"By Products" -> tykBold$ByProducts,
	
	"Briefs" -> tykBold$Briefs,
	"Denim" -> tykBold$Denim,
	
	"Diapers" -> tykBold$Diapers,
	"Diaper Bags" -> tykBold$DiaperBags,
	"Diaper Boxes" -> tykBold$DiaperBoxes,
	
	"Littles Gear" -> tykBold$LittlesGear,
	"Littles Gear Diapers" -> tykBold$LittlesGearDiapers,
	"Littles Gear Snappies" -> tykBold$LittlesGearSnappies,
	"Littles Gear Outfits" -> tykBold$LittlesGearOutfits,
	

	"Subscription Counts" -> <|
		"Littles Gear" -> <|
			"Snappies Only" ->  tykBold$SnappiesOnlyCount,
			"Snappies & Diapers" -> tykBold$SnappiesDiapersCount,
			"Snappies & Outfits" -> tykBold$SnappiesOutfitsCount,
			"Snappies & Outfits & Diapers" -> tykBold$SnappiesDiapersOutfitsCount, 
			"Total" -> tykBold$LittlesGearCount
		|>,
		"Diapers" -> <|
			"Boxes" -> tykBold$DiapersBoxCount,
			"Bags" -> tykBold$DiapersBagCount,
			"Littles Gear" -> tykBold$SnappiesDiapersCount,
			"Total" -> tykBold$DiapersBoxCount
		|>,
		"Total" -> tykBold$SubscriptionsCount
	|>,
	
	"Summaries" -> <|
		"All" -> tykBold$SubscriptionSummaryTable,
		"Diapers" -> tykBold$DiapersSummaryTable,
		"Littles Gear" -> tykBold$LittlesGearSummaryTable
	|>
		


|>;


tykBold$InitPayload[] := Block[
	{
	},
	
	tykBold$TestMode = "False";
	
	tykBold$StartTime = Now;
	
	tykBold$ProjectName = "tykBold";
	tyk$CreateProjectFolders[tykBold$ProjectName];

	tykBold$EndTime = Null;
	tykBold$RunTime = Null;

	tykBold$DataFolder = tyk$DataFolder[tykBold$ProjectName];
	tykBold$OutputFolder = tyk$OutputFolder[tykBold$ProjectName];
	
	tykBold$RawData = Null;
	tykBold$ActiveSubscriptions = Null;
	tykBold$PausedSubscriptions = Null;
	tykBold$InactiveSubscriptions = Null;
	
	tykBold$Briefs = Null;
	tykBold$Denim = Null;
	
	tykBold$Diapers = Null;	
	tykBold$DiaperBags = Null;	
	tykBold$DiaperBoxes = Null;		
	
	tykBold$LittlesGear = Null;
	tykBold$LittlesGearDiapers = Null;
	tykBold$LittlesGearSnappies = Null;
	tykBold$LittlesGearOutfits = Null;
	

    tykBold$RawDataFile = FileNames[
        {"*products.csv"},
        tykBold$DataFolder
        ]  // Sort // Last;
    

	tykBold$TemplatesFolder := FileNameJoin[
		{
			Global`tykBold$AppFolder,
			"templates"
		}
	];
    
    tykBold$ReportPDFFile = fs$AddDateToFileName[
    	FileNameJoin[{tykBold$OutputFolder, "Tykables Subscription Report.pdf"}]
    ];
    
    tykBold$SubscriptionsExcelFile = fs$AddDateToFileName[
    	FileNameJoin[{tykBold$OutputFolder, "Tykables Subscriptions.xlsx"}]
    ];
    
    tykBold$AsOfDate = fs$GetDateFromFileName[tykBold$RawDataFile];
	tykBold$Today = Today;
	tykBold$DataAge = DateDifference[tykBold$AsOfDate, tykBold$Today];
    	
	tykBold$ReportNotebookFile = fs$AddDateToFileName[
     	FileNameJoin[{tykBold$OutputFolder, "Tykables Subscription Report.nb"}]
	];
	
	tykBold$ReportTemplate = FileNameJoin[
		{
			tykBold$TemplatesFolder,
			"subscription-report-template.nb"
		}
	];
	

	tykBold$SnappiesOnlySubscriptions = Null;
	tykBold$SnappiesOnlyCount = Null;
	tykBold$SnappiesOnlyAverageDuration = Null;
	
	tykBold$SnappiesDiapersSubscriptions = Null;
	tykBold$SnappiesDiapersCount = Null;
	tykBold$SnappiesDiapersAverageDuration = Null;
	
	
	tykBold$SnappiesOutfitsSubscriptions = Null;
	tykBold$SnappiesOutfitsCount = Null;	
	tykBold$SnappiesOutfitsAverageDuration = Null;
		
	tykBold$SnappiesDiapersOutfitsSubscriptions = Null;
	tykBold$SnappiesDiapersOutfitsCount = Null;
	tykBold$SnappiesDiapersOutfitsAverageDuration = Null;
	
	
	tykBold$LittlesGearSubscriptions = Null;
	tykBold$LittlesGearCount = Null;
	tykBold$LittlesGearAverageDuration = Null;
	

	tykBold$DiapersBoxCount = Null;
	tykBold$DiapersBoxAverageDuration = Null;
	tykBold$DiapersBagCount = Null;
	tykBold$DiapersBagAverageDuration = Null;
	tykbold$DiapersCount = Null;
	tykBold$DiapersAverageDuration = Null;
	
	tykBold$SubscriptionsCount = Null;
	tykBold$AllAverageDuration = Null;
	
	tykBold$UniqueProducts = Null;

	tykBold$ByProducts = Null;
	
	tykBold$SubscriptionSummaryTable = Null;
	tykBold$DiapersSummaryTable = Null;
	tykBold$LittlesGearSummaryTable = Null;
	
	
	True
	
];


End[] (* End Private Context *)

EndPackage[]