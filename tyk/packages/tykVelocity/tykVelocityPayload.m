(* Wolfram Language Package *)

BeginPackage[
	"tykVelocityPayload`", 
	{ 
		"ww`",
		"wwPayload`",
		"wwFileSystem`",
		"tykCommon`",
		"tykVelocityUtils`"
	}
]

tykVelocity$InitPayload::usage = "tykVelocity$InitPayload  "

tykVelocity$Inspect::usage = "tykVelocity$Inspect  "

tykVelocity$Payload::usage = "tykVelocity$Payload  "

tykVelocity$ProjectName::usage = "tykVelocity$ProjectName";


tykVelocity$VelocityData::usage = "  "

tykVelocity$DubblerInventory::usage = "  "

tykVelocity$DiaperVelocityData::usage = "  "
tykVelocity$DiaperInventory::usage = "  "
tykVelocity$DiapersOrderForm::usage = "  "

tykVelocity$DenimVelocityData::usage = "  "
tykVelocity$DenimInventory::usage = "  "
tykVelocity$DenimOrderForm::usage = "  "

tykVelocity$UnderwearVelocityData::usage = "  "
tykVelocity$UnderwearInventory::usage = "  "
tykVelocity$UnderwearOrderForm::usage = "  "

tykVelocity$SnappiesVelocityData::usage = "  "
tykVelocity$SnappiesInventory::usage = "  "
tykVelocity$SnappiesOrderForm::usage = "  "
tykVelocity$SnappiesOrderFormPivot::usage = "  "
	
tykVelocity$Report::usage = "  "



tykVelocity$ProjectFolder::usage = "  "
tykVelocity$DataFolder::usage = "  "
tykVelocity$DatasetFolder::usage = "  "
tykVelocity$OutputFolder::usage = "  "
   
    
tykVelocity$RawSalesVelocityFile::usage = "  "
tykVelocity$ReportFile::usage = "  "
tykVelocity$ExcelFile::usage = "  "
tykVelocity$OrderFormFile::usage = "  "
tykVelocity$OrderItemPricesFile::usage = "  ";
tykVelocity$OrderItemPrices::usage = "  ";	

tykVelocity$Report::usage = "  "


tykVelocity$EndTime::usage = "  "

tykVelocity$RunTime::usage = "  "

tykVelocity$StartTime::usage = "  "


(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 




tykVelocity$InitPayload[] := Block[
	{},
	
	tykVelocity$ProjectName = "tykVelocity";
	
	tyk$CreateProjectFolders[tykVelocity$ProjectName];
	
	
	tykVelocity$StartTime = Now;
	tykVelocity$EndTime = Null;
	tykVelocity$RunTime = Null;
				
	tykVelocity$VelocityData = Null;

	tykVelocity$DubblerInventory = Null;

	tykVelocity$DiaperVelocityData = Null;
	tykVelocity$DiaperInventory = Null;
	tykVelocity$DiapersOrderForm = Null;
	
	tykVelocity$DenimVelocityData = Null;
	tykVelocity$DenimInventory = Null;
	tykVelocity$DenimOrderForm = Null;	

	tykVelocity$UnderwearVelocityData = Null;
	tykVelocity$UnderwearInventory = Null;
	tykVelocity$UnderwearOrderForm = Null;
	
	tykVelocity$SnappiesVelocityData = Null;
	tykVelocity$SnappiesInventory = Null;
	tykVelocity$SnappiesOrderForm = Null;	
	tykVelocity$SnappiesOrderFormPivot = Null;
	
	tykVelocity$Report = Null;
	
   
	tykVelocity$ProjectFolder = tyk$ProjectFolder[tykVelocity$ProjectName];
	tykVelocity$DataFolder = tyk$DataFolder[tykVelocity$ProjectName];
	tykVelocity$DatasetFolder = tyk$DataFolder[tykVelocity$ProjectName];
	tykVelocity$OutputFolder = tyk$OutputFolder[tykVelocity$ProjectName];
	   
	    
	tykVelocity$RawSalesVelocityFile = FileNameJoin[{tykVelocity$DatasetFolder, "sales-velocity.csv"}];
	tykVelocity$ReportFile = fs$AddDateToFileName[FileNameJoin[{tykVelocity$OutputFolder, "weekly-velocity.pdf"}]];
	tykVelocity$ExcelFile = fs$AddDateToFileName[FileNameJoin[{tykVelocity$OutputFolder, "weekly-velocity.xlsx"}]];
	tykVelocity$OrderFormFile = fs$AddDateToFileName[FileNameJoin[{tykVelocity$OutputFolder, "order-form-data.xlsx"}]];
	tykVelocity$OrderItemPricesFile = FileNameJoin[{tykVelocity$DataFolder, "order-item-prices.csv"}];





];


tykVelocity$Payload[] := <|

	"Start Time" -> tykVelocity$StartTime,
	"End Time" -> tykVelocity$EndTime,
	"Run Time" -> tykVelocity$RunTime,

	"Folders" -> <|
		"project" -> tykVelocity$ProjectFolder,
		"data" -> tykVelocity$DataFolder,
		"datasets" ->  tykVelocity$DatasetFolder,
		"output" -> tykVelocity$OutputFolder
	   |>,
	   
	"Files" -> <|
		"rawSalesVelocity" -> tykVelocity$RawSalesVelocityFile,
		"report" ->  tykVelocity$ReportFile,
		"excel" -> tykVelocity$ExcelFile,
		"orderForm" -> tykVelocity$OrderFormFile
	|>,


	"Velocity Data" -> tykVelocity$VelocityData,
	
	"Dubbler Inventory" -> tykVelocity$DubblerInventory,
	"Diaper Inventory" -> tykVelocity$DiaperInventory,
	"Diaper Velocity Data" -> tykVelocity$DiaperVelocityData,
	"Diapers Order Form" -> tykVelocity$DiapersOrderForm,
	
	"Denim Inventory" -> tykVelocity$DenimInventory,
	"Denim Velocity Data" -> tykVelocity$DenimVelocityData,
	"Denim Order Form" -> tykVelocity$DenimOrderForm,
		
	"Underwear Inventory" -> tykVelocity$UnderwearInventory,
	"Underwear Velocity Data" -> tykVelocity$UnderwearVelocityData,
	"Underwear Order Form" -> tykVelocity$UnderwearOrderForm,
	
	
	"Snappies Inventory" -> tykVelocity$SnappiesInventory,
	"Snappies Velocity Data" -> tykVelocity$SnappiesVelocityData,
	"Snappies Order Form" -> tykVelocity$SnappiesOrderForm,
	"Snappies Order Form Pivot" -> tykVelocity$SnappiesOrderFormPivot,
	
		
	"Report" -> tykVelocity$Report
|>


tykVelocity$Inspect[] := payload$inspect[tykVelocity$Payload[]];


End[] (* End Private Context *)

EndPackage[]