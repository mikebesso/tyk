(* Wolfram Language Package *)

BeginPackage[
	"tykFedExData`",
	{
		"ww`",
		"wwBootstrap`",
		"tykCommon`",
		"tykFedExPayload`",
		"tykFedExUtils`",
		"wwFileSystem`",
		"wwCSV`",
		"wwExcel`",
		"wwDates`",
		"wwStrings`",
		"wwLists`",
		"wwNumbers`",
		"wwMoney`",
		"wwDatasets`",
		"wwUnits`"
	}
]

tykFedEx$ReplicateData::usage = "tykFedEx$ReplicateData  "

tykFedEx$GetOrderItemPriceData::usage = "tykFedEx$GetOrderItemPriceData  "

tykFedEx$GetInternationalOrderData::usage = "tykFedEx$GetInternationalOrderData  "

tykFedEx$GetShipmentData::usage = "tykFedEx$GetShipmentData  "

tykFedEx$GetShippedItemData::usage = "tykFedEx$GetShippedItemData  "

tykFedEx$GetInternationalShipmentData::usage = "tykFedEx$GetInternationalShipmentData  "

tykFedEx$GetInvoiceData::usage = "tykFedEx$GetInvoiceData  "

tykFedEx$ExportData::usage = "tykFedEx$ExportData  "

tykFedEx$GetRawInvoiceData::usage = "tykFedEx$GetRawInvoiceData  "

tykFedEx$GetRawShippedItemData::usage = "tykFedEx$GetRawShippedItemData  "

tykFedEx$GetRawCustomsData::usage = "tykFedEx$GetRawCustomsData  "


(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 



tykFedEx$ReplicateData["Replicate Data"] :=  Block[
	{},
	fs$CopyIfNewer[
		"/Volumes/mike/Code/tykables/tykables-data/skulabs/csv/shipments.csv",
		tykFedEx$ShipmentsFile
	];
	
	fs$CopyIfNewer[
		"/Volumes/mike/Code/tykables/tykables-tableau/datasets/skulabs/international-shipped-items.csv",
		tykFedEx$InternationalShippedItemsFile
	];
	
	
	fs$CopyIfNewer[
		"/Volumes/mike/Code/tykables/tykables-tableau/datasets/skulabs/order-item-prices.csv",
		tykFedEx$OrderItemPricesFile
	];	
	
	
	fs$CopyIfNewer[
		"/Volumes/mike/Code/tykables/tykables-tableau/datasets/skulabs/international-orders.csv",
		tykFedEx$InternationalOrderFile
	];	
	
	
	(*TODO Add validatation before returning True*)
  
	True

];


tykFedEx$GetOrderItemPriceData[] := If[
	nullQ[tykFedEx$OrderItemPriceData]
	,
	tykFedEx$OrderItemPriceData = csv$Import[tykFedEx$OrderItemPricesFile]
	,
	tykFedEx$OrderItemPriceData
];


tykFedEx$GetInternationalOrderData[] := If[
	nullQ[tykFedEx$InternationalOrderData]
	,
	tykFedEx$InternationalOrderData = csv$Import[
		tykFedEx$InternationalOrderFile
		][
			All,
			<|
				#,
				"Tracking Number" -> tyk$CleanTrackingNumber[#["Tracking Number"]],
				"Order Number" -> tyk$CleanOrderNumber[#["Order Number"]],
				"Order Value" -> moneyUSD[#["Order Value"]]
			|> &
		]
	,
	tykFedEx$InternationalOrderData
];



tykFedEx$GetRawCustomsData[] := Module[
	{
		rawData = excelImport[tykFedEx$InternationalFile][
			All,
			<|
				"Invoice File" -> "International",
				"Tykables Order" -> tyk$CleanOrderNumber[
					str$Coalesce[
						{
							str$Tidy[#["Reference Notes Line 3"]], 
							str$Tidy[#["Reference Notes Line 2"]], 
							str$BlankIf[str$Tidy[#["Reference Notes Line 1"]], "NO REFERENCE INFORMATION"]
						}
					]
				],
			
				"Tracking Number" -> str$Tidy[#["Shipment Tracking Number"]],
				"Invoice Number" -> str$Tidy[#["Invoice Number"]],
				"Invoice Date" -> dt$MDY[#["Invoice Date (mm/dd/yyyy)"]], 
	
				"Ground or Express" -> str$Tidy[#["OPCO"]], 
				"Service" -> str$Tidy[#["Service Description"]], 
				"Response Service" ->  str$Tidy[#["Service Description"]],
				
								
				"Packaging" -> str$Tidy[#["Package type"]], 
				"Pay Type" -> str$Tidy[#["Pay type"]], 
	
	 			"Origin Code" -> str$Tidy[#["Origin Location Code"]], 
				"Destination Code" -> str$Tidy[#["Destination Location Code"]], 
	
				"Recipient Name" -> str$Tidy[#["Recipient Name"]], 
				"Recipient Company" -> str$Tidy[#["Recipient Company Name"]], 
				"Recipient Address Line 1" -> str$Tidy[#["Recipient Address"]], 
				"Recipient Address Line 2" -> "", 
				"Recipient Address City" -> str$Tidy[#["Recipient City"]], 
				"Recipient Address State" -> str$Tidy[#["Recipient State/Province"]], 
				"Recipient Address Country" -> str$Tidy[#["Recipient Country/Territory"]], 
				"Recipient Address Postal Code" -> str$Tidy[#["Recipient Postal Code"]], 
	
				"Shipper Name" -> str$Tidy[#["Shipper Name"]], 
				"Shipper Company" -> str$Tidy[#["Shipper Company Name"]], 
				"Shipper Address" -> str$Tidy[#["Shipper Address"]],
				"Shipper Address City" -> str$Tidy[#["Shipper City"]],
				"Shipper Address State" -> str$Tidy[#["Shipper State/Province"]],
				"Shipper Address Postal Code" -> str$Tidy[#["Shipper Postal Code"]],
				"Shipper Address Country" -> str$Tidy[#["Shipper Country/Territory"]],
	
	
				"Charge Freight" -> moneyUSD[#["Shipment Freight Charge Amount USD"]],
				"Charge Misc" -> moneyUSD[#["Shipment Miscellaneous Charge USD"]],
				"Charge Duty and Tax" -> moneyUSD[#["Shipment Duty and Tax Charge USD"]],
				"Charge Discount" -> moneyUSD[#["Shipment Discount Amount USD"]],
				"Charge Net" -> moneyUSD[#["Net Charge Amount USD"]],
				
				"Billed Currency" -> str$Tidy[#["Billed Currency Code"]], 
				"Currency Conversion Rate" -> #["Exchange Rate to USD"], 
	
				"Pieces" -> #["Pieces in Shipment"], 
	
	
				"Actual Weight" -> unitsWeightInPounds[#["Shipment Rated Weight(Pounds)"], "Pounds"],
				"Rated Weight" -> unitsWeightInPounds[#["Original weight(Pounds)"], "Pounds"],	
		

	  
				"Zone" -> str$Tidy[#["Pricing zone"]], 
				"Dim Flag" -> str$Tidy[#["Shipment Dim Flag (Y or N)"]], 
	
				"Dim Height" -> numTidy[#["Dimmed Height (in)"]], 
				"Dim Width" ->  numTidy[#["Dimmed Width (in)"]],  
				"Dim Length" -> numTidy[#["Dimmed Length (in)"]], 

				"Dim Divisor" -> Missing["NotAvailable"],
		
				"Dim Prod" -> numTidy[#["Dimmed Height (in)"]] * numTidy[#["Dimmed Length (in)"]] * numTidy[#["Dimmed Width (in)"]], 
				"Dim Sum" -> numTidy[#["Dimmed Height (in)"]] + numTidy[#["Dimmed Length (in)"]] + numTidy[#["Dimmed Width (in)"]],
			
				"Charge Grace Discount" -> Missing["NotAvailable"],
				"Charge Fuel Surcharge" -> Missing["NotAvailable"],
				"Charge Performance Pricing" -> Missing["NotAvailable"],
				"Charge Residential" -> Missing["NotAvailable"],
				"Charge DAS Extended Resi" -> Missing["NotAvailable"],
				"Charge DAS Resi" -> Missing["NotAvailable"],
				"Charge Discount" -> Missing["NotAvailable"],
				"Charge Weekday Delivery" ->  Missing["NotAvailable"],
				"Charge Weekly Service" -> Missing["NotAvailable"],
				
				"Charge Address Correction Discount" -> Missing["NotAvailable"],
				"Charge Address Correction" -> Missing["NotAvailable"],
				
				
				"Charge Transportation" -> Missing["NotAvailable"],

	
				"Customs Value" -> moneyUSD[#["Customs Value"]], 
	
				"Customs Number" -> str$Tidy[#["Customs Number"]], 
				"Declared Value" -> moneyUSD[#["Shipment Declared Value Amount USD"]],
				
				"Shipment Date" ->  dt$MDY[#["Shipment Date (mm/dd/yyyy)"]]
			
			|> &
		]
	},
	
	With[
		{
			duties = rawData[
				Select[#["Charge Duty and Tax" ] > moneyUSD[0] &], 
		   		{"Tracking Number", "Charge Duty and Tax", "Customs Value"}
		   ],
		   nonduties = rawData[
				Select[#["Charge Duty and Tax" ] == moneyUSD[0] &],
		   		All
		   ] // KeyDrop[{"Charge Duty and Tax", "Customs Value"}]
		},
				
		dsInnerJoin[
			duties,
			nonduties,
			"Tracking Number"
		]
	]
			
];


tykFedEx$GetInternationalShipmentData[] := If[
	nullQ[tykFedEx$InternationalShipmentData]
	,
	tykFedEx$InternationalShipmentData = 	
		dsInnerJoin[
			dsInnerJoin[tykFedEx$GetRawCustomsData[], tykFedEx$ShipmentData, "Tracking Number"],
			tykFedEx$GetInternationalOrderData[],
			"Tracking Number"
	][
		All,
		<|
			#,
			"Estimated Duty and Tax" -> #["Order Value"] * .17
		|> &
	][
		All,
		<|
			#,
			"Difference Duty and Tax" -> #["Charge Duty and Tax"] - #["Estimated Duty and Tax"]
		|> &
	],
	tykFedEx$InternationalShipmentData
];




tykFedEx$GetShipmentData[] := If[
	nullQ[tykFedEx$ShipmentData]
	,
	tykFedEx$MaxAsOf = dt$YMD[Import[tykFedEx$ShipmentsFile, {"CSV", "Data", 2, 1}]];
	tykFedEx$ShipmentData = csv$Import[tykFedEx$ShipmentsFile][
		Select[
			#["Incoming or Outgoing"] == "Outgoing" (*&& NumericQ[#["Response Tracking Number"]]*) &
		],
		<|
			"Store" -> str$Tidy[#["Store"]],
			"Order Number" -> tyk$CleanOrderNumber[#["Order Number"]],
			"Order Number Link" -> tyk$HyperlinkToSkulabs[#["Order Number"]],
			"Date" -> dt$MDY[#["Last Tracking Update"]],
			"Estimated Cost" -> moneyUSD[#["Response Amount"]],
			"Tracking Number" -> str$Tidy[#["Response Tracking Number"]],
			
			"Request Weight" -> unitsWeightInOunces[#["Request Weight"], #["Request Weight Unit"]],
			"Request Length" -> unitsDistanceInInches[#["Request Length"], #["Request Dimensions Unit"]],
			"Request Width" -> unitsDistanceInInches[#["Request Width"], #["Request Dimensions Unit"]],

			"Request Height" -> unitsDistanceInInches[#["Request Height"], #["Request Dimensions Unit"]],
			
			"Request Service" -> str$Tidy[#["Service Type"]],
			"Response Service" -> str$Tidy[#["Service Type"]]
		|> &
	],
	tykFedEx$ShipmentData
];


tykFedEx$GetRawShippedItemData[] := csv$Import[tykFedEx$InternationalShippedItemsFile][
	All,
	<|
		#,
		"Tracking Number" -> str$Tidy[#["Response Tracking Number"]],
		"Amount" -> moneyUSD[#["Amount"]]
	|> &
] // KeyDrop["Response Tracking Number"];


tykFedEx$GetShippedItemData[] := If[
	nullQ[tykFedEx$InternationalShippedItemData]
	,
	tykFedEx$InternationalShippedItemData = dsInnerJoin[
		tykFedEx$GetRawShippedItemData[],
		tykFedEx$GetInternationalShipmentData[][
			All, 
			{
				"Shipment Date", 
				"Service",
				"Tracking Number", 
				"Charge Duty and Tax", 
				"Customs Value", 
				"Charge Freight", 
				"Charge Misc", 
				"Charge Net", 
				"Charge Discount", 
				"Actual Weight", 
				"Zone"
			}
		],
		"Tracking Number"
	][
		All,
		<|
			#,
			"Percent Charge Duty and Tax" -> tdyPercentOf[#["Charge Duty and Tax"], #["Amount"]],
			"Charge Freight Per Pound" -> tdyDivide[QuantityMagnitude[#["Charge Freight"]], QuantityMagnitude[#["Actual Weight"]]]
		|> &
	]
	,
	tykFedEx$InternationalShippedItemData

];



tykFedEx$GetRawInvoiceData[] := Module[
	{}
	,
	csv$Import[
		"*.csv",
		tykFedEx$InvoicesFolder,
		csv$$IncludeFileName -> True,
		csv$$IncludeFileNameColumn -> "Invoice File"
	] // KeyDrop[
		{
			"Bill to Account Number",
			"Store ID",
			"Original Amount Due",
			"Current Balance",
			"Payor",
			"POD Service Area Code",
			"POD Signature",
			"Meter Number",
			"Bundle Number",
			"TDMasterTrackingID",
			"Service Packaging",
			"RMA#",
			"Sort Hub",
			"Cost Allocation",
			"Currency Conversion Date",
			"Bill To Account Number",
			"Ground Tracking ID Prefix",
			"POD Delivery Date",
			"POD Delivery Time", 
			"POD Signature Description"
		}
	]
];
		

tykFedEx$UnpivotCharges[assoc_Association, measure_String] := Block[
	{
		measureColumns = Table[
			{
				"Tracking ID Charge Description " <> str$Tidy[n], 
				"Tracking ID Charge Amount " <> str$Tidy[n]
			},
			{n, 1, 10}
		]
	},

   	Module[
   		{
   			retVal = moneyUSD[0.]
   		},
		Scan[
			If[
				assoc[#[[1]]] == measure,
				retVal = moneyUSD[assoc[#[[2]]]]
			] &,
			measureColumns
		];
		retVal
	]
];


tykFedEx$GetInvoiceData[] := If[
	nullQ[tykFedEx$InvoiceData]
	,
	tykFedEx$InvoiceData = Module[
		{
			rawInvoiceData = tykFedEx$GetRawInvoiceData[],
			unneededColumns
		}
		,
		unneededColumns = Select[
			dsColumnNames[rawInvoiceData],
			Or[
				StringStartsQ[#, "Alternative"],
				StringStartsQ[#, "Original"],
				StringStartsQ[#, "Shipper"],
				StringStartsQ[#, "Updated"],
				StringStartsQ[#, "Alternate"],
				StringStartsQ[#, "Multiweight"],
				StringStartsQ[#, "Commodity"],
				StringStartsQ[#, "Tracking ID Charge"],
				StringStartsQ[#, "Original"],
				StringStartsQ[#, "CrossRef"],
				StringStartsQ[#, "Tendered"],
				StringStartsQ[#, "Shipment Notes"]
					
			] &
		];
		
		tykFedEx$InvoiceData = rawInvoiceData[
 			All,
			<|
			  	"Invoice File" -> StringTake[#["Invoice File"], 16],
			    "Invoice Number" -> str$Tidy[#["Invoice Number"]],
				"Invoice Date" -> tdyToDate[serializedDate[#["Invoice Date"]]],
				
				"Tracking Number" -> str$Tidy[#["Express or Ground Tracking ID"]],
				"Tykables Order" -> tyk$CleanOrderNumber[
					str$Coalesce[
						{
							str$Tidy[#["Original Ref#2"]], 
							str$Tidy[#["Original Ref#3/PO Number"]], 
							str$BlankIf[str$Tidy[#["Original Customer Reference"]], "NO REFERENCE INFORMATION"]
						}
					]
				],
				
				"Shipment Date" -> DateObject[tdyToDate[serializedDate[#["Shipment Date"]]], "Day"],
				
				"Recipient Name" -> str$Tidy[#["Recipient Name"]], 
				"Recipient Company" -> str$Tidy[#["Recipient Company Name"]], 
				"Recipient Address Line 1" -> str$Tidy[#["Recipient Address Line 1"]], 
				"Recipient Address Line 2" -> str$Tidy[#["Recipient Address Line 2"]], 		
				"Recipient Address City" -> str$Tidy[#["Recipient City"]], 
				"Recipient Address State" -> str$Tidy[#["Recipient State"]], 
				"Recipient Address Country" -> str$Tidy[#["Recipient Country/Territory"]], 
				"Recipient Address Postal Code" -> str$Tidy[#["Recipient Zip Code"]], 
	
				"Shipper Name" -> str$Tidy[#["Shipper Name"]], 
				"Shipper Company" -> str$Tidy[#["Shipper Company Name"]],
	 			"Shipper Address" -> str$Tidy[#["Shipper Address"]],
				"Shipper Address City" -> str$Tidy[#["Shipper City"]],
				"Shipper Address State" -> str$Tidy[#["Shipper State"]],
				"Shipper Address Postal Code" -> str$Tidy[#["Shipper Zip Code"]],
				"Shipper Address Country" -> str$Tidy[#["Shipper Country/Territory"]],
						
			
			
				"Actual Weight" -> unitsWeightInPounds[#["Actual Weight Amount"], "Pounds"],
				"Rated Weight" -> unitsWeightInPounds[#["Rated Weight Amount"], "Pounds"],
				
	
				"Pieces" -> #["Number of Pieces"],
					
				"Zone" -> str$Tidy[#["Zone Code"]],
				
				"Dim Length" -> numTidy[#["Dim Length"]],
				"Dim Width" -> numTidy[#["Dim Width"]],
				"Dim Height" -> numTidy[#["Dim Height"]],
	
				"Dim Divisor" -> numTidy[#["Dim Divisor"]],
				"Dim Prod" -> #["Dim Height"] * #["Dim Length"] * #["Dim Width"], 
				"Dim Sum" -> #["Dim Height"] + #["Dim Length"] + #["Dim Width"],
			
				"Charge Grace Discount" -> tykFedEx$UnpivotCharges[#, "Grace Discount"],
				"Charge Fuel Surcharge" -> tykFedEx$UnpivotCharges[#, "Fuel Surcharge"],
				"Charge Performance Pricing" -> tykFedEx$UnpivotCharges[#, "Performance Pricing"],
				"Charge Residential" -> tykFedEx$UnpivotCharges[#, "Residential"],
				"Charge DAS Extended Resi" -> tykFedEx$UnpivotCharges[#, "DAS Extended Resi"],
				"Charge DAS Resi" -> tykFedEx$UnpivotCharges[#, "DAS Resi"],
				"Charge Discount" -> tykFedEx$UnpivotCharges[#, "Discount"],
				"Charge Weekday Delivery" ->  tykFedEx$UnpivotCharges[#, "Weekday Delivery"],
				"Charge Weekly Service" -> tykFedEx$UnpivotCharges[#, "Weekly Service Chg"],
				
				"Charge Address Correction Discount" -> moneyUSD[#["Ground Tracking ID Address Correction Discount Charge Amount"]],
				"Charge Address Correction" -> moneyUSD[#["Ground Tracking ID Address Correction Gross Charge Amount"]],
				
				
				"Charge Transportation" -> moneyUSD[#["Transportation Charge Amount"]],
	
				"Charge Net" -> moneyUSD[#["Net Charge Amount"]],
				
				"Customs Value" -> Missing["NotAvailable"], 
				"Customs Number" -> Missing["NotAvailable"], 
				"Declared Value" -> Missing["NotAvailable"]
	
		    |> &
	    ] // KeyDrop[
	    	Join[
			    {
			    	"Express or Ground Tracking ID",
			    	"Net Charge Amount",
			    	"Zone Code",
			    	"Transportation Charge Amount",
			    	"Ground Tracking ID Address Correction Discount Charge Amount",
			    	"Ground Tracking ID Address Correction Gross Charge Amount"
			    },
			    unneededColumns
	    	]
	    ]
	
	]
	,
	tykFedEx$InvoiceData
];



tykFedEx$ExportData := Block[
	{
		dutyAndTaxes
	},
	
	dutyAndTaxes[category_String] :=  tykFedEx$InternationalShippedItemData[
		Select[#[category] * #["Pieces"] == 1 &],
		{"Shipment Date", "Product Name", "Charge Duty and Tax", "Customs Value"}
	] // ReverseSortBy[#["Shipment Date"]];
  	
  	Export[tykFedEx$InvoicesFile, tykFedEx$InvoiceData];
  	
  	
  	With[
  		{
  	
		  	us = tykFedEx$DomesticData[
			  	Select[
			  		#["Recipient Address Country"] == "US" &
			  	], 
			   	Complement[
			    	dsColumnNames[tykFedEx$DomesticData], 
			    	{"Date", "Order Number", "Invoice Date"}
		    	]
			 ],
			 
			 ca = tykFedEx$InternationalData[
			  	Select[
			  		#["Recipient Address Country"] == "CA" &
			  	], 
			   	Complement[
			    	dsColumnNames[tykFedEx$InternationalData], 
			    	{"Date", "Order Number", "Invoice Date"}
		    	]
			 ]
  		},
		  	
  	

  	
		exportExcel[
		  tykFedEx$DatasetFile, 
		  {
		  	"US" -> us,
		  	If[Length[ca] > 0, "CA" -> ca, Nothing],
		  	"Biggest Issues US" -> tykFedEx$BiggestIssuesUS,
		  	"Biggest Issues CA" -> tykFedEx$BiggestIssuesCA,
		  	"On or After 15 Jun" -> tykFedEx$IssuesSince15June,
			"Should Be 8.2" -> tykFedEx$Mismatched,
			If[Length[tykFedEx$DupeTrackingIDs] > 0, "Dupes" -> tykFedEx$DupeTrackingIDs, Nothing],
			"CA Diaper Taxes" -> dutyAndTaxes["Diapers"],
			"CA Dubbler Taxes" -> dutyAndTaxes["Dubbler"],
			"CA Underwear Taxes" -> dutyAndTaxes["Underwear"],
			"CA Shirt Taxes" -> dutyAndTaxes["Shirts"],			
			"CA Shirt Taxes" -> dutyAndTaxes["Pants"]
		  }
	  ]
	  
  	];
  	
  	
  	With[
  		{
  			taxes = tykFedEx$InternationalShipmentData[
  				Select[
					And[
						#["Recipient Address Country"] == "CA",
						#["Number of Shipments"] == 1
					] &
				],
				{
					"Tracking Number", 
					"Number of Shipments", 
					"Recipient Address Country", 
					"Order Number", 
					"Order Value", 
					"Charge Duty and Tax", 
					"Estimated Duty and Tax", 
					"Invoice Number", 
					"Invoice Date", 
					"Number of Shipments", 
					"Difference Duty and Tax"
				}
			] 
  		}
  		,
  		exportExcel[
 			FileNameJoin[{tykFedEx$OutputFolder, "CA Taxes.xls"}], 
 			taxes
 		];
  	];


];




End[] (* End Private Context *)

EndPackage[]