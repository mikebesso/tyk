(* Wolfram Language Package *)

BeginPackage[
	"tykFedExReport`",
	{
		"ww`",
		"wwBootstrap`",
		"tykCommon`",
		"tykFedExPayload`",
		"tykFedExUtils`",
		"wwDates`",
		"wwStrings`",
		"wwLists`",
		"wwNumbers`",
		"wwMoney`",
		"wwDatasets`",
		"chartLayouts`",
		"gridCharts`",
		"chartUtil`",
		"wwChartScatter`"
	}

]

tykFedEx$BuildHistograms::usage = "tykFedEx$BuildHistograms  "

tykFedEx$BuildTables::usage = "tykFedEx$BuildTables  "

tykFedEx$BuildReport::usage = "tykFedEx$BuildReport  "

tykFedEx$TableColumns::usage = "tykFedEx$TableColumns  "


(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 


tykFedEx$BuildHistograms[] :=  Module[
	{
		overunder
	},

	overunder[country_String] := With[
		{
			data = 	tykFedEx$Data[Select[#["Country"] == country &], All]
		}, 
		
		Histogram[
			{
 				data[
 					 Select[#["Over or Under"] == "Under" &],
  					"Cost Difference %"
  				] // Normal,
  				data[
 					 Select[#["Over or Under"] == "Over" &],
  					"Cost Difference %"
	  				] // Normal
			},
 			{10}, 
 			PlotRange -> {{-200, 1000}, {0, 1000}}, 
			PlotLabel -> country <> " Percent Under- or Over-charged", 
			AxesLabel -> {"%", "#"}, 
			ImageSize -> {650, 400}, 
			ImageMargins -> 0, 
			AspectRatio -> 400/650,
			ScalingFunctions -> "Log"
		]
	];

	

	tykFedEx$HistogramsOverUnder = tdyColumn[
		{
			overunder["US"],
			overunder["CA"]
		}
	];
	
	tykFedEx$HistogramsCostByZone = tdyFacetColumns[
		{"2", "3", "4", "5", "6", "7", "8", "51", "52"},
		Histogram[tykFedEx$Data[tykFedEx$SelectByZone[#], "Charge Net"] // Normal, 
		PlotRange -> {{0, 100}, {0, 400}}] &, 
		"columnCount" -> 3,
		"Title" -> "Actual Cost by Zone"
	];



	(*TODO Add validatation before returning True*)
  
	True
  
  
];




tykFedEx$TableColumns = {
		"Invoice File",
		"Shipment Date",
		"Invoice Number",
		"Tracking Number",
		"Order Number",
		"Zone", 
		"Box",
		"Estimated Cost",
		"Charge Net",
		"Cost Difference",
		"Cost Difference %"
};
	

tykFedEx$BuildTables[] := Block[
	{
	},

	tykFedEx$BiggestIssues = tykFedEx$Data[
		Select[
			And[
				#["Recent"],
				#["Over Threshold"],
				#["Over or Under"] == "Over"
			] &
		],
	   	{
	   		"Country",
			"Invoice File",
			"Shipment Date",
			"Invoice Number",
			"Tracking Number",
			"Order Number",
			"Zone", 
			"Box",
			"Estimated Cost",
			"Charge Net",
			"Cost Difference",
			"Cost Difference %"
		}
	] // SortBy[ #["Cost Difference"] &] // Reverse;
	    
	
	tykFedEx$BiggestIssuesUS = tykFedEx$BiggestIssues[
		Select[
			And[
				#["Country"] == "US"
			] &
		],
	   	All
	] // KeyDrop["Country"] // SortBy[ #["Cost Difference"] &] // Reverse;
	
	tykFedEx$BiggestIssuesCA = tykFedEx$BiggestIssues[
		Select[
			And[
				#["Country"] == "CA"
			] &
		],
	   	All
	] // KeyDrop["Country"] // SortBy[ #["Cost Difference"] &] // Reverse;	    
	    
	tykFedEx$IssuesSince15June := tykFedEx$Data[
		Select[
			And[
				DateObject[#["Shipment Date"], "Day"] >= DateObject[{2020, 6, 15}, "Day"],
				#["Cost Difference"] > moneyUSD[0]
			] &
		], 
	    {
			"Invoice File",
			"Shipment Date",
			"Invoice Number",
			"Tracking Number",
			"Order Number",
			"Zone", 
			"Box",
			"Estimated Cost",
			"Charge Net",
			"Cost Difference",
			"Cost Difference %"
		}
	] // SortBy[ #["Cost Difference"] &] // Reverse;    
	
	
	
	tykFedEx$Mismatched := tykFedEx$Data[
		Select[
			And[
				#["Estimated Cost"] == moneyUSD[8.2],
				! StringStartsQ[#["Box"], "12x15x01"],
				#["Cost Difference"] > moneyUSD[0]
			] &
		],
	    {
			"Invoice File",
			"Shipment Date",
			"Invoice Number",
			"Tracking Number",
			"Order Number",
			"Zone", 
			"Box",
			"Estimated Cost",
			"Charge Net",
			"Cost Difference",
			"Cost Difference %"
		}
	] // SortBy[ #["Cost Difference"] &] // Reverse;


	tykFedEx$DupeTrackingIDs := With[
		{
			dupes = Flatten[list$GetDupePositions[dsColumnValues[tykFedEx$InvoiceData, "Tracking Number"]]]
		},
		
		tykFedEx$InvoiceData[dupes, All]
	];
		

	
	
	(*TODO Add validatation before returning True*)
  
	True
	    
];



tykFedEx$BuildReport[]:= Block[
	{
	},
	
	tykFedEx$BuildHistograms[];
	tykFedEx$BuildTables[];
	
	tykFedEx$Report = Join[
   		{
    		outputCell[tykFedEx$HistogramsOverUnder, PageBreakBelow -> True],
    		outputCell[tykFedEx$HistogramsCostByZone, PageBreakBelow -> True],
			outputCell[
				With[
					 {
					  filters = {
					    "Diapers" -> Select[#["Diapers"] == 1 &],
					    "Shirts" -> Select[#["Shirts"] == 1 &],
					    "Underwear" -> Select[#["Underwear"] == 1 &],
					    "Pants" -> Select[#["Pants"] == 1 &]
					    }
					  },
					 
					 tdyColumn[
					  {
					   chartScatter[
					    tykFedEx$InternationalShippedItemData, 
					    {"Amount", "Charge Duty and Tax"},
					    filters,
					    PlotLabel -> "Duty and Tax ($)"
					    ],
					   chartScatterAcrossTime[
					    tykFedEx$InternationalShippedItemData, 
					    {"Date", "Percent Charge Duty and Tax"},
					    filters,
					    PlotLabel -> "Change in Percent Duty/Tax over time"
					    ]
					   }
					  ]
					 ], 
					 PageBreakBelow -> True
				],
				
				outputCell[
					With[
						 {
						  filters = {
						    "Ground 51" -> 
						     Select[#["Service"] == "International Ground" && #["Zone"] == 
						         "51" &],
						    "Ground 54" -> 
						     Select[#["Service"] == "International Ground" && #["Zone"] == 
						         "54" &]
						    }
						  },
						 
						 tdyColumn[
						  {
						   chartScatter[
						    tykFedEx$InternationalShippedItemData,
						    {"Actual Weight", "Charge Freight"},
						    filters
						    ],
						   chartScatterAcrossTime[
						    tykFedEx$InternationalShippedItemData,
						    {"Shipment Date", "Charge Freight Per Pound"},
						    filters
						    ]
						   }
						  ]
						 ], 
					 PageBreakBelow -> True
					
				],
				
				
				outputCell[
					
					With[
					 {
					  filters = {
					    "Ground 51" -> 
					     Select[#["Service"] == "International Ground" && #["Zone"] == 
					         "51" &],
					    "Ground 54" -> 
					     Select[#["Service"] == "International Ground" && #["Zone"] == 
					         "54" &]
					    }
					  },
					 
					 tdyColumn[
					  {
					   chartScatter[
					    tykFedEx$InternationalShippedItemData,
					    {"Actual Weight", "Charge Misc"},
					    filters
					    ],
					   chartScatterAcrossTime[
					    tykFedEx$InternationalShippedItemData,
					    {"Shipment Date", "Charge Misc"},
					    filters
					    ]
					   }
					  ]
					 ], 
					 PageBreakBelow -> True
				]
			
		},
   		tdyToCells[tykFedEx$BiggestIssuesUS, "title" -> "US Biggest Recent Overcharges"],
   		tdyToCells[tykFedEx$BiggestIssuesCA, "title" -> "CA Biggest Recent Overcharges"]
   		
	
	];
	
	(*TODO Add validatation before returning True*)
  
	True
];



End[] (* End Private Context *)

EndPackage[]