(* Wolfram Language Package *)

BeginPackage[
	"tykFedExPayload`", 
	{
		"ww`",
		"wwBootstrap`",
		"wwDates`",
		"wwStrings`",
		"wwMoney`",
		"tykCommon`",
		"wwPayload`",
		"wwDates`",
		"wwFileSystem`"
	}
]

tykFedEx$Payload::usage = "tykFedEx$Payload  "

tykFedEx$Inspect::usage = "tykFedEx$Inspect  "

tykFedEx$InitPayload::usage = "tykFedEx$InitPayload  "

tykFedEx$ProjectName::usage = "tykFedEx$ProjectName  "

tykFedEx$InvoiceData::usage = "tykFedEx$InvoiceData  ";
tykFedEx$ProjectFolder::usage = "  ";
tykFedEx$DataFolder::usage = "  ";
tykFedEx$InvoicesFolder::usage = "  ";
tykFedEx$CleansedFolder::usage = "  ";
tykFedEx$SkulabsFolder::usage = "  ";
tykFedEx$OutputFolder::usage = "  ";
tykFedEx$ReportFile::usage = "  ";
tykFedEx$InvoicesFile::usage = "  ";
tykFedEx$ShipmentsFile::usage = "  ";
tykFedEx$DatasetFile::usage = "  ";

tykFedEx$OrderItemPricesFile::usage = "  ";

tykFedEx$DupeTrackingIDs::usage = "";

tykFedEx$rawInvoicesFolderPath::usage = "fedex$rawInvoicesFolderPath  "

tykFedEx$dataFolderPath::usage = "fedex$dataFolderPath  "

tykFedEx$shipmentsFilePath::usage = "fedex$shipmentsFilePath  "

tykFedEx$DomesticData::usage = "tykFedEx$DomesticData  "

tykFedEx$InternationalShippedItemsFile::usage = "";
tykFedEx$InternationalShippedItems::usage = "";

tykFedEx$InternationalOrderFile::usage = "";

tykFedEx$InternationalOrderData::usage = "";

tykFedEx$InternationalShippedItemData::usage = ""
tykFedEx$InternationalData::usage = "tykFedEx$InternationalData  ";
tykFedEx$Data::usage = "";

tykFedEx$InternationalShipmentData::usage = "  "

tykFedEx$InternationalFile::usage = "tykFedEx$InternationalFile  ";
tykFedEx$InternationalFolder::usage = "tykFedEx$InternationalFolder  "



tykFedEx$HistogramsOverUnder::usage = "  "
tykFedEx$HistogramsCostByZone::usage = "  "

tykFedEx$BiggestIssues::usage = "  "
tykFedEx$BiggestIssuesUS::usage = "  "
tykFedEx$BiggestIssuesCA::usage = "  "

tykFedEx$Report::usage = "tykFedEx$Report  "

tykFedEx$ShipmentData::usage = "tykFedEx$ShipmentData  "

tykFedEx$Inspect::usage = "tykFedEx$Inspect  "

tykFedEx$MaxAsOf::usage = "tykFedEx$MaxAsOf  "
tykFedEx$DataAge::usage = "tykFedEx$DataAge  "

tykFedEx$EndTime::usage = "tykFedEx$EndTime  "

tykFedEx$RunTime::usage = "tykFedEx$RunTime  "

tykFedEx$StartTime::usage = "tykFedEx$StartTime  "

tykFedEx$IssuesSince15June::usage = "tykFedEx$IssuesSince15June  "

tykFedEx$Mismatched::usage = "tykFedEx$Mismatched  ";





tykFedEx$IssueDollarThreshold::usage = "";
tykFedEx$IssuePercentThreshold::usage = ""

tykFedEx$RecentThreshold::usage = "tykFedEx$RecentThreshold  ";




tykFedEx$OrderItemPricesFile::usage = "tykFedEx$OrderItemPricesFile  ";
tykFedEx$OrderItemPriceData::usage = "tykFedEx$OrderItemPriceData  ";	

(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 




tykFedEx$Inspect[] := payload$inspect[tykFedEx$Payload[]];

tykFedEx$Payload[] := <|
	"Test Mode" -> tykFedEx$TestMode,
	"Start Time" -> tykFedEx$StartTime,
	"End Time" -> tykFedEx$EndTime,
	"Run Time" -> tykFedEx$RunTime,
	
	"Dates" ->    <|
		"Today" -> tykFedEx$Today,
		"Data Age" -> tykFedEx$DataAge,
		"Max As Of" -> tykFedEx$MaxAsOf
	|>,
	
	"Parameters" -> <|
		"Dollar Threshold" -> tykFedEx$IssueDollarThreshold,
		"Percent Threshold" -> tykFedEx$IssuePercentThreshold,
		"Recent Threshold" -> tykFedEx$RecentThreshold
	|>,
	
	
	"Folders" -> <|
		"Project" -> tykFedEx$ProjectFolder,
		"Data" -> tykFedEx$DataFolder,
		"Invoices" -> tykFedEx$InvoicesFolder,
		"International" -> tykFedEx$InternationalFolder,
		"Cleansed" -> tykFedEx$CleansedFolder,
		"Skulabs" -> tykFedEx$SkulabsFolder,
		"Output" -> tykFedEx$OutputFolder
	|>,

	"Files" -> <|
		"Inputs" -> <|
			"Invoices" -> tykFedEx$InvoicesFile,
			"International" -> tykFedEx$InternationalFile,
			"International Shipped Items" -> tykFedEx$InternationalShippedItemsFile,
			"International Orders" -> tykFedEx$InternationalOrderFile,
			"Order Item Prices" -> tykFedEx$OrderItemPricesFile,
			"Report" -> tykFedEx$ReportFile
		|>,
		"Outputs" -> <|
			"Shipments" -> tykFedEx$SkulabsFolder,
			"Dataset" ->  tykFedEx$DatasetFile 
		|>
	|>,

	"Data" -> <|
	
		"Sources" -> <|
		
	
			"Invoices" -> tykFedEx$InvoiceData,
			"Shipments" -> tykFedEx$ShipmentData,
			"International" -> tykFedEx$InternationalShipmentData,
			"International Shipped Items" -> tykFedEx$InternationalShippedItemData
		|>,
		"Analysis" -> <|
		
			"Domestic" -> tykFedEx$DomesticData,
			"International" -> tykFedEx$InternationalData,
			"Data" -> tykFedEx$Data
		|>

	|>,
	
	"Histograms" -> <|
		"Over Under" -> tykFedEx$HistogramsOverUnder,
		"Cost by Zone" -> tykFedEx$HistogramsCostByZone
	|>,

	
	"Tables" -> <|
		"Biggest Issues" -> <|
			"All" -> tykFedEx$BiggestIssues,
			"US" -> tykFedEx$BiggestIssuesUS,
			"CA" -> tykFedEx$BiggestIssuesCA
		|>,
		"Issues Since 15 June" -> tykFedEx$IssuesSince15June,
		"Mismatched" -> tykFedEx$Mismatched,
		"Dupe Tracking IDs" -> tykFedEx$DupeTrackingIDs
	|>,


	"Report" -> tykFedEx$Report


|>;


tykFedEx$InitPayload[] := Block[
	{
	},
	

	tykFedEx$StartTime = Now;
	tykFedEx$Today = Today;
	
	tykFedEx$ProjectName = "tykFedEx";
	
	

	tykFedEx$IssueDollarThreshold = moneyUSD[5];
	tykFedEx$IssuePercentThreshold = Quantity[20, "%"];
	tykFedEx$RecentThreshold = Quantity[60, "Days"];
	

	tykFedEx$ProjectFolder := tyk$ProjectFolder[tykFedEx$ProjectName];
	tykFedEx$DataFolder := tyk$DataFolder[tykFedEx$ProjectName];
	tykFedEx$InvoicesFolder = createFolder[FileNameJoin[{tykFedEx$DataFolder, "invoices"}]];
	tykFedEx$InternationalFolder = createFolder[FileNameJoin[{tykFedEx$DataFolder, "international"}]];

	tykFedEx$CleansedFolder := createFolder[FileNameJoin[{tykFedEx$DataFolder, "cleansed"}]];
	tykFedEx$SkulabsFolder := createFolder[FileNameJoin[{tykFedEx$DataFolder, "skulabs"}]];
	
	
	
	tykFedEx$OutputFolder := tyk$OutputFolder[tykFedEx$ProjectName];

	tykFedEx$InvoicesFile := FileNameJoin[{tykFedEx$CleansedFolder, "fedex-invoices.csv"}];
	
	tykFedEx$InternationalFile = FileNames["International Extract.xlsx", tykFedEx$InternationalFolder] // Sort // Last;
	tykFedEx$InternationalShippedItemsFile = FileNameJoin[{tykFedEx$InternationalFolder, "international-shipped-items.csv"}];
	tykFedEx$InternationalOrderFile = FileNameJoin[{tykFedEx$InternationalFolder, "international-orders.csv"}];
	
	
	tykFedEx$OrderItemPricesFile = FileNameJoin[{tykFedEx$InternationalFolder, "order-item-prices.csv"}];

	
	tykFedEx$ShipmentsFile := FileNameJoin[{tykFedEx$SkulabsFolder, "shipments.csv"}];
	tykFedEx$DatasetFile := fs$AddDateToFileName[FileNameJoin[{tykFedEx$OutputFolder, "fedex-dataset.xlsx"}]];
	tykFedEx$ReportFile := fs$AddDateToFileName[FileNameJoin[{tykFedEx$OutputFolder, "fedex-report.pdf"}]];


	
	tykFedEx$InvoiceData = Null;
	tykFedEx$MaxAsOf = Null;
	tykFedEx$ShipmentData = Null;
	tykFedEx$DomesticData = Null;
	tykFedEx$InternationalData = Null;
	tykFedEx$InternationalShipmentData = Null;
	tykFedEx$InternationalShippedItemData = Null;
	tykFedEx$InternationalOrderData = Null;
		
	tykFedEx$OrderItemPriceData = Null;
		
	tykFedEx$Data = Null;

	tykFedEx$HistogramsOverUnder = Null;
	tykFedEx$HistogramsCostByZone = Null;
	
	tykFedEx$BiggestIssues = Null;
	tykFedEx$BiggestIssuesUS = Null;
	tykFedEx$BiggestIssuesCA = Null;

	
	tykFedEx$IssuesSince15June = Null;
	tykFedEx$Mismatched = Null;
	
	tykFedEx$DupeTrackingIDs = Null;
	
	tykFedEx$InternationalShippedItems = Null;
	
	tykFedEx$Report = Null;
	
	True

]
End[] (* End Private Context *)

EndPackage[]