(* Wolfram Language Package *)

BeginPackage["tykBoldReport`", { 
	"ww`",
	"wwBootstrap`",
	"tykCommon`",
	"tykBoldUtils`", 
	"tykBoldPayload`",
	"chartLayouts`",
	"gridCharts`",
	"wwDatasets`",
	"wwStrings`",
	"wwPayload`",
	"wwExcel`",
	"gridCharts`",
	"chartLayouts`",
	"wwDates`",
	"wwSubscriptionModel`"
	}
]

tykBold$BuildReport::usage = "tykBold$BuildReport  "

buildWidgetWithoutBreakout::usage = "buildWidgetWithoutBreakout  "
buildWidgetWithBreakout::usage = "buildWidgetWithBreakout  "

subscriptionSummary::usage = "subscriptionSummary  "

chapterDiapers::usage = "chapterDiapers  "

chapterLittlesGear::usage = "chapterLittlesGear  "

subscriptionInfoLittlesGearSnappies::usage = "subscriptionInfoLittlesGearSnappies  "

subscriptionInfoLittlesGearOutfits::usage = "subscriptionInfoLittlesGearOutfits  "

tykBold$Item::usage = "tykBold$Item  "

tykBold$Report$BuildWidget$Counts::usage = "tykBold$Report$BuildWidget$Counts  "

tykBold$PlotSubscriptionTrend::usage = "tykBold$PlotSubscriptionTrend  "

(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 


tykBold$Item[expr_] := Item[expr, Alignment -> {Center, Top}];


buildProductTable[title_String, products_Dataset] := tdyToGrid[
   products[
     All,
     <|
       "ID" -> #["ID"],
       "Product" -> 
        Item[#["Product"], Alignment -> {Left, Top}, 
         ItemSize -> {24, 2}, BaseStyle -> {LineIndent -> 0}],
       "Size" -> #["Size"],
       "Gender" -> #["Gender"],
       "Items" -> #["Items"]
       |> &
     ] // Sort,
   "rowsPerGrid" -> 40
   ];


widgetColumns$Counts[hideProduct_?BooleanQ] := If[
   hideProduct,
   {
    "Gender",
    "Size", 
    "Subs",
    "Total Items"
    },
   {
    "Gender",
    "Size", 
    "Subs",
    "Total Items"
    }
   ];

widgetColumns$BreakoutCounts[hideProduct_?BooleanQ]  := If[
   hideProduct,
   {
    "Gender",
    "Size", 
    "Items",
    "Subs",
    "Total Items"
    },
   {
    "Gender",
    "Product Name",
    "Size", 
    "Items",
    "Subs",
    "Total Items"
    }
   ];


Options[buildWidgetWithoutBreakout] = {
   "hideProduct" -> True,
   "title" -> Nothing,
   "additionalSubscriptions" -> 0
   };

buildWidgetWithoutBreakout[subData_Association, OptionsPattern[]] := Block[
   {
   		countsByGender = subData["Counts by Gender"] // sortByGenderSizeAndBags,
		countsBySize = subData["Counts by Size"] // sortBySizeAndBags,
		
   
    	subscriptionCount = 
     		subData["Counts by Gender"] // KeyDrop[{"Items", "Total Items"}]
	},
   
   
	tdyRow[
		{
			tykBold$Item[
		     	tdyToGrid[
		     		dsSpanDuplicates[
		 				Dataset[
		  					Append[
		   						countsBySize // dsSpanDuplicates // Normal,
								Join[
									Prepend[
										countsBySize[
											Total[#] &,
											{"Subs", "Total Items", "Next 7 Days Subs", "Next 7 Days Items", "Previous Failed"}
										] // Normal,
										<| "Size" -> "All" |>
			     					]
		    					]
		   					]
		  				] 
		 			], 
		     		"paging" -> False
		     	]
	     	],
			tykBold$Item[
		   		tdyToGrid[
					dsSpanDuplicates[
						Dataset[
							Append[
								countsByGender // dsSpanDuplicates // Normal,
								Join[
									Prepend[
											countsByGender[
												Total[#] &,
												{"Subs", "Total Items", "Next 7 Days Subs", "Next 7 Days Items", "Previous Failed"}
										] // Normal,
										<| "Gender" -> "All", "Size" -> "All" |>
									]
								]
							]
						] 
					], 
				    "paging" -> False
		   		]
			]
   		}
	]
   
   
	
];

Options[buildWidgetWithBreakout] = {
   "hideProduct" -> True,
   "title" -> Nothing
   };



tykBold$Report$BuildWidget$Counts[counts_Dataset] := Module[
	{
		dimensions = Select[dsColumnNames[counts], MemberQ[{"Gender", "Size", "Product Name", "Items"}, #]&],
		measures = Select[dsColumnNames[counts], MemberQ[{"Subs", "Total Items", "Next 7 Days Subs", "Next 7 Days Items", "Previous Failed"}, #]&],
		sortedCounts,
		headers
	},
	
	headers = {
		{
			If[MemberQ[dimensions, "Gender"], Item["Gender", Alignment -> Center], Nothing],
	  		If[MemberQ[dimensions, "Product Name"], Item["Product", Alignment -> Center], Nothing],
	  		If[MemberQ[dimensions, "Size"], Item["Size", Alignment -> Center], Nothing],
	  		If[MemberQ[dimensions, "Items"], Item["Items", Alignment -> Center], Nothing],
	  		If[MemberQ[measures, "Subs"], Item["Subs", Alignment -> Center], Nothing],
	  		If[MemberQ[measures, "Total Items"], Item[tdyStringJoinWithNewLines[{"Total", "Items"}], Alignment -> Center], Nothing],
	  		Item["Next 7 Days", Alignment -> Center],
	  		SpanFromLeft,
	  		Item[tdyStringJoinWithNewLines[{"Previous", "Failed"}], Alignment -> Center]
	   		
		},
		{
			If[MemberQ[dimensions, "Gender"], SpanFromAbove, Nothing],
			If[MemberQ[dimensions, "Product Name"], SpanFromAbove, Nothing],
			If[MemberQ[dimensions, "Size"], SpanFromAbove, Nothing],
			If[MemberQ[dimensions, "Items"], SpanFromAbove, Nothing],
			If[MemberQ[measures, "Subs"], SpanFromAbove, Nothing],
			If[MemberQ[measures, "Total Items"], SpanFromAbove, Nothing],
			Item["Subs", Alignment -> Center],
			Item["Items", Alignment -> Center],
			SpanFromAbove
		}
	};
	
	sortedCounts = counts // sortByGenderSizeAndBags;
	
	tdyToGrid[
		dsSpanDuplicates[
			Dataset[
				Append[
					sortedCounts // dsSpanDuplicates // Normal,
					Join[
						Prepend[
							sortedCounts[
								Total[#] &,
								measures
							] // Normal,
							Association[
								Map[
									# -> "All" &,
									dimensions
								]
							]
     					]
					]
  				] 
 			]
 		], 
     	"paging" -> False,
     	"ColumnHeaders" -> headers
	]
	
];


buildWidgetWithBreakout[subData_Association, OptionsPattern[]] := Module[
	{
		
		countsByGender = subData["Counts by Gender"] // sortByGenderSizeAndBags,
		countsBySize = subData["Counts by Size"] // sortBySizeAndBags,
	    
	    breakoutCounts = RightComposition[
			If[
				OptionValue["hideProduct"],
				sortByGenderSizeAndBags,
				sortByGenderProductSizeAndBags
			],
			dsSpanDuplicates
		][
			subData["Breakout Counts"][
				All,
				widgetColumns$BreakoutCounts[OptionValue["hideProduct"]]
			]
		]
    },
   


	tdyGrid[
		{
			{
				tykBold$Item[
			     	tdyToGrid[
			     		dsSpanDuplicates[
			 				Dataset[
			  					Append[
			   						countsBySize // dsSpanDuplicates // Normal,
									Join[
										Prepend[
											countsBySize[
												Total[#] &,
												{"Subs", "Total Items", "Next 7 Days Subs", "Next 7 Days Items", "Previous Failed"}
											] // Normal,
											<| "Size" -> "All" |>
				     					]
			    					]
			   					]
			  				] 
			 			], 
			     		"paging" -> False
			     	]
		     	],
		     	
		     	SpanFromLeft
			},
			{
				
		     	tykBold$Item[
			     	tdyToGrid[
			     		dsSpanDuplicates[
			 				Dataset[
			  					Append[
			   						countsByGender // dsSpanDuplicates // Normal,
									Join[
										Prepend[
											countsByGender[
												Total[#] &,
												{"Subs", "Total Items", "Next 7 Days Subs", "Next 7 Days Items", "Previous Failed"}
											] // Normal,
											<| "Gender" -> "All", "Size" -> "All" |>
				     					]
			    					]
			   					]
			  				] 
			 			], 
			     		"paging" -> False
			     	]
		     	],
		     	tykBold$Item[
					tdyToGrid[breakoutCounts , "paging" -> False]
		     	]
			}
		}
	]
];
   

   chapterInfo[title_String, text_String : Nothing, widget_] :=
    {
     chapterCell[title],
     ifNotNothing[text, textCell[text]],
     ifNotNothing[widget, outputCell[widget]],
     TextCell["", PageBreakBelow -> True]
     };
   
   chapterInfo[title_String, text_String] := 
    chapterInfo[title, text, Nothing];
   
   subscriptionInfo[title_String, text_String, widget_] := {
     sectionCell[title],
     textCell[text],
     outputCell[widget]
     };
   
   productInfo[subData_Assoication] := With[
     {
      products = If[
        Or[
         StringContainsQ[subData["Key"], "Diaper"],
         StringContainsQ[subData["Key"], "Brief"]
         ],
        subData["Data"],
        subData["Data"] // KeyDrop["Items"]
        ],
      rowsPerGrid = If[
        StringContainsQ[subData["Key"], "Box"],
        15,
        45
        ]
      },
     Flatten[
      {
       sectionCell[subData["Key"]],
       Map[
        outputCell,
        tdyToGrid[products, "rowsPerGrid" -> rowsPerGrid]
        ],
       TextCell["", PageBreakBelow -> True]
       }
      ]
     ];
     
     
 chapterInfo[title_String, text_String : Nothing, widget_] :=
    {
     chapterCell[title],
     ifNotNothing[text, textCell[text]],
     ifNotNothing[widget, outputCell[widget]],
     TextCell["", PageBreakBelow -> True]
     };
   
   chapterInfo[title_String, text_String] := 
    chapterInfo[title, text, Nothing];
   
   subscriptionInfo[title_String, text_String, widget_] := {
     sectionCell[title],
     textCell[text],
     outputCell[widget]
     };
   
   productInfo[subData_Assoication] := With[
     {
      products = If[
        Or[
         StringContainsQ[subData["Key"], "Diaper"],
         StringContainsQ[subData["Key"], "Brief"]
         ],
        subData["Data"],
        subData["Data"] // KeyDrop["Items"]
        ],
      rowsPerGrid = If[
        StringContainsQ[subData["Key"], "Box"],
        15,
        45
        ]
      },
     Flatten[
      {
       sectionCell[subData["Key"]],
       Map[
        outputCell,
        tdyToGrid[products, "rowsPerGrid" -> rowsPerGrid]
        ],
       TextCell["", PageBreakBelow -> True]
       }
      ]
     ];     



subscriptionSummary[] := chapterInfo[
	"Subscription Summary",
	tdyStringJoinWithNewLines[
		{
			"This report was generated on " <> str$Tidy[tykBold$Today],
			"with data pulled from Bold on " <> str$Tidy[tykBold$AsOfDate],
			"(" <> str$Tidy[tykBold$DataAge] <> " days ago)", 
			"",
			"Remember, Littles Gear Diapers are counted under both Diapers and Littles Gear subscriptions.",
			""
		}
	],
	
	tdyGrid[
		{
			{
				Item[
					tdyToGrid[
					
						tykBold$SubscriptionSummaryTable,
						"paging" -> False,
						"title" -> "All Subscriptions"
					],
					Alignment -> {"Center", "Top"}
				],
				SpanFromLeft
			},
			{
				Item[
					tdyToGrid[
						tykBold$DiapersSummaryTable,
						"paging" -> False,
						"title" -> "Diapers"
					],
					Alignment -> {"Center", "Top"}
				],
				Item[
					tdyToGrid[
						tykBold$LittlesGearSummaryTable,
						"paging" -> False,
						"title" -> "Littles Gear"
					],
					Alignment -> {"Center", "Top"}
				]
					
			},
			{
				Item[
					"There are currently " <> str$Tidy[Length[tykBold$PausedSubscriptions]] <> " paused subscriptions.",
					Alignment -> {"Center", "Top"}
				],
				SpanFromLeft,
				SpanFromLeft	
			}
		}
	]
		
			
];

chapterDiapers[] := chapterInfo[
	"Diapers",
	tdyColumn[
		{
			(*TODO Need to set this up first*)
			(*
			tdyToGrid[
				tykBold$Diapers["Counts by Gender"], 
				"paging" -> False,
				"title" -> "All Diapers"
			],
			*)
			Item[
				buildWidgetWithBreakout[
					tykBold$DiaperBoxes, 
					"title" -> "Big Littles Diaper Boxes"
				],
				Alignment -> {"Center", "Top"}
			],
			Item[
				buildWidgetWithBreakout[
					tykBold$LittlesGearDiapers, 
					"title" -> "Littles Gear Diapers"
				],
				Alignment -> {"Center", "Top"}
			],
			Item[
				buildWidgetWithBreakout[
					tykBold$DiaperBags, 
					"hideProduct" -> False, 
					"title" -> "Diaper Bags"
				],
				Alignment -> {"Center", "Top"}
			]
		}
	]
];

(*TODO: figure out how to show the additionals in a more compact form.*)
(*
chapterLittlesGear[] := chapterInfo[
	"Littles Gear",
	"In this section we look at the different Littles Gear subscription options.  At a high-level we have:",
	tdyToGrid[
		tykBold$LittlesGearSummaryTable,
		"paging" -> False,
		"title" -> "Littles Gear"
	]
];*)


chapterLittlesGear[] := chapterInfo[
	"Littles Gear",
	tdyGrid[
		{
			{
				Item[
					tdyToGrid[
						tykBold$LittlesGearSummaryTable,
						"paging" -> False,
						"title" -> "Summary"
					],
					Alignment -> {Center, Top}
				],
				Item[
					buildWidgetWithoutBreakout[tykBold$LittlesGearSnappies, "title" -> "Snappies"],
					Alignment -> {Center, Top}
				],
				Item[
					buildWidgetWithoutBreakout[tykBold$LittlesGearOutfits, "title" -> "Outfits"],
					Alignment -> {Center, Top}
				]
			},
			{
				Item[
					buildWidgetWithBreakout[
						tykBold$LittlesGearDiapers, 
						"title" -> "Diapers"
					],
					Alignment -> {Center, Top}
				],
				SpanFromLeft,
				SpanFromLeft
			}
		}
	]
			
];


subscriptionInfoLittlesGearSnappies[] := subscriptionInfo[
	"Littles Gear Snappies",
	"The Snappies option for Littles Gear Subscriptions",
	buildWidgetWithoutBreakout[
		tykBold$LittlesGearSnappies,
		"additionalSubscriptions" -> Plus[
			Length[Complement[tykBold$LittlesGearOutfits["IDs"], tykBold$LittlesGearSnappies["IDs"]]],
			Length[Complement[tykBold$LittlesGearDiapers["IDs"], tykBold$LittlesGearSnappies["IDs"]]]
		]
	]
];

subscriptionInfoLittlesGearOutfits[] := subscriptionInfo[        
	"Little Gears Outfits",
    "The Outfits option for Littles Gear Subscriptions",
    buildWidgetWithoutBreakout[tykBold$LittlesGearOutfits]
];

subscriptionInfoLittlesGearDiapers[] := subscriptionInfo[
	"Littles Gear Diapers", 
	"The Littles Gear Diapers is an Little Gear subscription \
option.  These diapers are part of a larger Littles Gear \
subscription.  Please note that these subscriptions are counted under \
both Diapers and Littles Gear.",
        buildWidgetWithBreakout[tykBold$LittlesGearDiapers]
]


tykBold$BuildReport[] := Block[
	{
		cells,
    	notebook
	},
   
   
	Catch[
   	
		throwFalseIfFileExists[tykBold$ReportNotebookFile];

		notebook := GenerateDocument[
			tyk$ReportTemplate["subscription-report-template.nb"],
			tykBold$Payload[]
		];
		
		NotebookSave[notebook, tykBold$ReportNotebookFile];
		NotebookClose[notebook];
		
	   
	   
	   
		(*cells = Join[
	   
			subscriptionSummary[],
			chapterDiapers[],
			chapterLittlesGear[],
			
			chapterInfo[
				"Appendix 1: Anomalies",
				"This section provides a list of strange data conditions."
			],
	       
			{
				textCell[
					"The following Littles Gear subscription Ids include diapers but not snappies:"
				],
	        
				outputCell[Complement[tykBold$LittlesGearDiapers["IDs"], tykBold$LittlesGearSnappies["IDs"]]]
			},
	       
			{
				textCell[
					"The following Littles Gear subscription Ids include diapers but not snappies:"
	         	],
	        
				outputCell[Complement[tykBold$LittlesGearOutfits["IDs"], tykBold$LittlesGearSnappies["IDs"]]]
			}
		];
	       
	      
	  
	     
	    
		notebook = buildTykablesNotebook[
			cells,
			"title" -> "Tykables Subscription Report"
		];
	    
	    
		NotebookPrint[
			notebook,
			tykBold$ReportPDFFile
		];
	    
	    NotebookClose[notebook];*)
	    
	    Throw True;
    
   ]

];
   

tykBold$PlotSubscriptionTrend[] := Block[
	{
		actives = tykBold$ActiveSubscriptions[
			All, 
			{"First Order Date", "Last Order Date"}
		] // Normal // Values // tdyToDate,
		inactives = tykBold$InactiveSubscriptions[
      		All, 
			{"First Order Date", "Last Order Date"}
		] // Normal // Values // tdyToDate,
		subscriptionModel
	},

	subscriptionModel := subscriptions$CreateModel[actives, inactives, "Week"];

	subscriptionModel["Plot"]

]   




End[] (* End Private Context *)



EndPackage[]