(* Wolfram Language Package *)

(* Created by the Wolfram Workbench Jun 19, 2020 *)

BeginPackage[
	"tykBold`", 
	{
		"wwBootstrap`",
		"tykCommon`", 
		"wwStrings`",
		"wwMath`",
		"wwFileSystem`",
		"wwCSV`",
		"wwDatasets`",
		"wwDates`",
		"tykBoldUtils`",
		"tykBoldPayload`",
		"tykBoldReport`",
		"wwExcel`"
	}
];



tykBold$CountThings::usage = "tykBold$CountThings  "

tykBold$Run::usage = "tykBold$Run  "
tykBold$Step::usage = "tykBold$Step  "




(* Exported symbols added here with SymbolName::usage *) 

Begin["`Private`"]
(* Implementation of the package *)



tykBold$Step[_, False] := False;
tykBold$Step[step_String, True] := Block[
	{
		time,
		result
	},
	Print[step];
	{time, result} = AbsoluteTiming[tykBold$Step[step]];
	Print[Round[time, 0.001]];
	result
	
];

tykBold$Step["Done"] := Block[
	{},
	tykBold$EndTime = Now;
	tykBold$RunTime = tykBold$EndTime - tykBold$StartTime;
	True
];


tykBold$Step["Init Project"] := Block[
	{},
(*	tykBold$TestMode = "False";
	tykBold$StartTime = Now;
	
	tykBold$ProjectName = "tykBold";
	tyk$CreateProjectFolders[tykBold$ProjectName];*)
	
	True
];


tykBold$Step["Init Payload"] := tykBold$InitPayload[];

(*Block[
	{
	},
	

	tykBold$DataFolder = tyk$DataFolder[tykBold$ProjectName];
	tykBold$OutputFolder = tyk$OutputFolder[tykBold$ProjectName];
	
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
    

    
    tykBold$ReportPDFFile = fs$AddDateToFileName[
    	FileNameJoin[{tykBold$OutputFolder, "Tykables Subscription Report.pdf"}]
    ];
    	
	tykBold$ReportNotebookFile = fs$AddDateToFileName[
     	FileNameJoin[{tykBold$OutputFolder, "Tykables Subscription Report.nb"}]
	];
	
	
	tykBold$UniqueProducts = Null;

	tykBold$ByProducts = Null;
	
	
	True
	
];*)




tykBold$Run[] := Block[
   {},
   
  
	RightComposition[
		tykBold$Step["Init Project", #] &,
		tykBold$Step["Init Payload", #] &,
		tykBold$Step["Get Data", #] &, 
		tykBold$Step["Split Products", #] &,
		tykBold$Step["Identify Unique Products", #] &,
    
    
		tykBold$Step["Parse Product Name", #] &,
		tykBold$Step["Parse Line Item Type", #] &,
		tykBold$Step["Parse Line Item", #] &,
		tykBold$Step["Parse Item Count", #] &,
		tykBold$Step["Parse Gender", #] &,
		tykBold$Step["Parse Size", #] &,
		tykBold$Step["Parse Bag Count", #] &,
		tykBold$Step["Parse Flags", #] &,
		
		tykBold$Step["Count Products", #] &,
		
		tykBold$Step["Count Subscriptions", #] &,
		
		tykBold$Step["Build Report", #] &,
		
		tykBold$Step["Export Data", #] &
     
     ][True];
     
     
     tykBold$Step["Done"]
     

   
   ];



tykBold$Step["Get Data"] := Block[
   {
    rawBoldProductData,
    columnNames,
    columnsToDrop,
    columnsToKeep
    },
   
   Catch[   
	   	tykBold$RawData = csv$Import[
	   		tykBold$RawDataFile
	   	][
	   		Select[
		       And[
		         ! StringContainsQ[ToLowerCase[#["Products"]], "print your"],
		         ! StringContainsQ[ToLowerCase[#["Products"]], "change"]
		         ] &
		    ],
	   		<|
	   			"Active" -> #["Active"] == "Yes", 
	   			"Email" -> str$Tidy[#["Customer E-mail"]],
	   			"ID" -> str$Tidy[#["Subscription ID"]],
	   			"Is Paused" -> #["Is Paused"] == 1,
	   			"Products" -> #["Products"], 
	   			(*"Products SKUs",  *)
	   			"Interval Number" -> #["Interval Number"], 
	   			"Interval Type" -> #["Interval Type"], 
	   			"First Order Date" -> #["Subscription Date"],
	   			"Last Order Date" -> #["Last Order Date"], 
	   			"Next Order Date" -> #["Next Order Date"],
	   			"Next 7 Days" -> Catch[
   					If[
						#["Next Order Date"] == "",
						Throw[0]
   					];
   					If[dt$DaysDifference[Today, tdyToDate[#["Next Order Date"]]] <= 7, 1, 0]
	   			],
	   			"Previous Failure" -> #["Last Transaction Failure Code"],
	   			"Previous Failed" -> If[#["Last Transaction Failure Code"] != "", 1, 0],
	   			"Duration" -> wwNFloor[dt$WeeksDifference[tdyToDate[#["Subscription Date"]], tdyToDate[#["Last Order Date"]]]]
	   		|> &
	   	];
	   		
	   			
	    tykBold$ActiveSubscriptions = tykBold$RawData[
	   		Select[
	   			And[
	   				! #["Is Paused"],
	   				#["Active"]
	   			] &
	   		], 
	   		All
	   ] // KeyDrop[{"Active", "Is Paused"}];
	   	
	   	tykBold$PausedSubscriptions = tykBold$RawData[
	   		Select[
	   			And[
	   				#["Is Paused"],
	   				#["Active"]
	   			] &
	   		], 
		    All
	   	] // KeyDrop[{"Active", "Is Paused"}];
	   	
	    tykBold$InactiveSubscriptions = tykBold$RawData[
	   		Select[! #["Active"] &],
	   		All
	   ] // KeyDrop[{"Active", "Is Paused"}];
	   	
	   	
	   	tykBold$Next7Days = tykBold$ActiveSubscriptions[
	   		Select[#["Next 7 Days"] > 0 &],
		    All
	   	] // KeyDrop["Next 7 Days"];
	   	
	   	tykBold$Failures = tykBold$ActiveSubscriptions[
	   		Select[#["Previous Failed"] > 0 &],
		    All
	   	] // KeyDrop["Previous Failed"];	   	


		Throw[True];

	
	]
];











tykBold$Step["Split Products"] := Block[
   {
    data = tykBold$ActiveSubscriptions,
    parseProducts,
    mapProducts
    },
   
   
   Catch[
    
    mapProducts[key_String, id_String, list_List] := 
     Map[<|"ID" -> id, key -> #|> &, list];
    
    parseProducts[from_String -> to_String] := Module[
      {
       products = data[
          All, 
          <|
            
            "ID" -> #["ID"],
            from -> First[ImportString[str$Tidy[#[from]], "CSV"]]
            |> &
          ] // Normal
       },
      
      Dataset[
       Flatten[
        Map[
          mapProducts[
           to, 
           #["ID"], 
           str$Tidy[#[from]]
           ] 
          &,
         products
         
         ]
        ]
       ]
      ];
    
    tykBold$ByProducts = dsInnerJoin[
    	parseProducts["Products" -> "Product"],
    	tykBold$ActiveSubscriptions[All, {"ID", "Next 7 Days", "Previous Failed", "Duration"}],
    	"ID"
    ];
    
    
    Throw[True]
    ]
   ];




tykBold$Step["Identify Unique Products"] := Block[
   {
    },
   
  
   Catch[
    
    tykBold$UniqueProducts = dsAggregate$count[
       tykBold$ByProducts, 
       "Product",
       dsAggregate$optAggregateName ->  "Subs"
       ][
      All,
      <|
        #,
        "Product Key" -> ToLowerCase[str$Tidy[#["Product"]]],
        "Line Item Type" -> "",
        "Line Item" -> "",
        "Gender" -> "Both",
        "Size" -> "",
        "Size Order" -> 0,
        "Item Count" -> 0,
        "Items" -> 0, 
        "Has Diaper Bags" -> False,
        "Has Diaper Boxes" -> False,
        "Has Littles Gear Diapers" -> False,
        "Has Littles Gear Snappies" -> False,
        "Has Littles Gear Outfits" -> False,
        "Has Briefs" -> False,
        "Has Denim" -> False
        |> &
      ];
    
    Throw[True];
    ]
   ];



tykBold$Step["Parse Product Name"] := Block[
   {
    parse
    },
   
   parse[product_String] := RightComposition[
      ToLowerCase,
      StringReplace[#, {
         "- outfit" ->  "outfit",
         "- snappies" ->  "snappies",
         "(old)" -> ""
         }
        ] &,
      StringSplit[# <> "-", "-"] &,
      First,
      StringTrim,
      StringReplace[
        #, 
        {
         "subscription" -> "",
          "fem" -> "", 
          "masc" -> "", 
         "both" -> "",
         "option level" -> ""
         }
        ] &,
      Capitalize[#, "TitleCase"] &
      ][product];
   
   tykBold$UniqueProducts = 
    tykBold$UniqueProducts[
     All,
     <|
       #,
       "Product Name" -> parse[#["Product Key"]]
       |> &
     ];
   
   True
   ];



tykBold$Step["Parse Size"] := Block[
   {
    },
   
    
   Catch[
    tykBold$UniqueProducts = 
     tykBold$UniqueProducts[
       All,
       <|
         #,
         "Size" -> tyk$ParseSize[#["Product Key"]]
         |> &
       ][
      All,
      <|
        #,
        "Size Order" -> lookupSizeOrder[#["Size"]]
        |> &
      ];
    
    
    Throw[True];
    ]
   ];




tykBold$Step["Parse Line Item Type"] := Block[
   {
    parse
    },
   
   
   parse[product_String, size_String] := Catch[
     
     throwIfStringStartsWith[product, "Big Littles", "Diaper Box"];
     
     If[size != "", Throw[""]];
     
     throwIfStringContains[product, "Option Level"];
     
     throwIfStringContains[product, "Snappies & Diapers"];
     throwIfStringContains[product, "Snappies & Outfit"];
     
     throwIfStringStartsWith[product, "Mix & Match", 
      "Diapers - Mix & Match"];
     
     throwIfStringStartsWith[product, "Littles Gear"];
     throwIfStringContains[product, "Print Your", "N/A"];
     Throw["Product"]
     ];
   
   Catch[
    tykBold$UniqueProducts = 
     tykBold$UniqueProducts[
      All,
      <|
        #,
        "Line Item Type" -> parse[
          #["Product Key"], 
          #["Size"]
          ]
        |> &
      ];
    
    Throw[True];
    ]
   ];




tykBold$Step["Parse Line Item"] := Block[
   {
    parse
    },
   
   
  
   parse[product_String, size_String] := Catch[
     
     throwIfStringContains[product, "Print Your", "N/A"];
     
     
     throwIfStringContains[product, "Little Rascals", 
      "Diapers - Little Rascals"];
     throwIfStringContains[product, "Play Days Blue", 
      "Diapers - Play Days Blue"];
     throwIfStringContains[product, "Play Dayz Blue", 
      "Diapers - Play Days Blue"];
     throwIfStringContains[product, "Play Days Pink", 
      "Diapers - Play Days Pink"];
     throwIfStringContains[product, "Play Dayz Pink", 
      "Diapers - Play Days Pink"];
     throwIfStringContains[product, "Overnights", 
      "Diapers - Overnights"];
     
     throwIfStringContains[product, "Unicorns", 
      "Diapers - Unicorns"];
     
     throwIfStringContains[product, "Cammies", 
      "Diapers - Cammies"];
      
     throwIfStringContains[product, "Little Rawrs", 
      "Diapers - Little Rawrs"];
     
     throwIfStringContains[product, "Outfit"];
     throwIfStringContains[product, "Snappies"];
     
     throwIfStringContains[product, "Briefs"];
     throwIfStringContains[product, "Denim"];
     
     Throw["Unknown"];
     ];
   
   Catch[
    tykBold$UniqueProducts = 
     tykBold$UniqueProducts[
      All,
      <|
        #,
        "Line Item" -> If[
          #["Line Item Type"] == "Product",
          parse[#["Product Key"], #["Size"]],
          "N/A"
          ]
        |> &
      ];
    
    Throw[True];
    ]
   ];



tykBold$Step["Parse Item Count"] := Block[
   {
    parse
    },
   
   parse[product_String] := Catch[
     If[StringEndsQ[product, "x 0"], Throw [0]];
     If[StringEndsQ[product, "x 1"], Throw [1]];
     If[StringEndsQ[product, "x 2"], Throw [2]];
     If[StringEndsQ[product, "x 3"], Throw [3]];
     Throw[1];
     ];
   
   Catch[
    tykBold$UniqueProducts = 
     tykBold$UniqueProducts[
      All,
      <|
        #,
        "Item Count" -> parse[#["Product Key"]]
        |> &
      ];
    
    Throw[True];
    ]
   ];




tykBold$Step["Parse Gender"] := Block[
  {
  },
  
  Catch[
   tykBold$UniqueProducts = 
    tykBold$UniqueProducts[
     All,
     <|
       #,
       "Gender" -> tyk$ParseGender[#["Product Key"]]
       |> &
     ];
   
   Throw[True];
   ]
  ];




tykBold$Step["Parse Bag Count"] := Block[
   {
    parse
    },
   
   
 
   
   parse[product_String] := Catch[
     If[StringStartsQ[product, "bag "] \[And] 
       StringEndsQ[product, "x 2"], Throw[2.]];
     If[StringStartsQ[product, "bag "] \[And] 
       StringEndsQ[product, "x 1"], Throw[1.]];
     If[StringContainsQ[product, "8 bag"], Throw [8.]];
     If[StringContainsQ[product, "48"], Throw [4.]];
     If[StringContainsQ[product, "4 bag"], Throw [4.]];
     If[StringContainsQ[product, "full"], Throw [1.]];
     If[StringContainsQ[product, "half"], Throw [0.5]];
     If[StringContainsQ[product, "80 diaper"], Throw [8.]];
     If[StringContainsQ[product, "40 diaper"], Throw [4.]];
     
     throwIfStringContains[product, 
      "Subscription Masc Diapers - Large2", 2.];
     throwIfStringContains[product, 
      "Subscription Masc Diapers - Large1", 1.];
     throwIfStringContains[product, 
      "Subscription Masc Diapers - Large", 1.];
     throwIfStringContains[product, 
      "Subscription Masc Diapers - Med2", 2.];
     throwIfStringContains[product, 
      "Subscription Masc Diapers - Med1", 1.];
     throwIfStringContains[product, 
      "Subscription Masc Diapers - Med", 1.];
     throwIfStringContains[product, 
      "Subscription Masc Diapers - XL2", 2.];
     throwIfStringContains[product, 
      "Subscription Masc Diapers - XL1", 1.];
     throwIfStringContains[product, "Subscription Masc Diapers - XL", 
      1.];
     
     throwIfStringContains[product, 
      "Subscription Fem Diapers - Large2", 2.];
     throwIfStringContains[product, 
      "Subscription Fem Diapers - Large1", 1.];
     throwIfStringContains[product, 
      "Subscription Fem Diapers - Large", 1.];
     throwIfStringContains[product, 
      "Subscription Fem Diapers - Med2", 2.];
     throwIfStringContains[product, 
      "Subscription Fem Diapers - Med1", 1.];
     throwIfStringContains[product, "Subscription Fem Diapers - Med", 
      1.];
     throwIfStringContains[product, "Subscription Fem Diapers - XL2", 
      2.];
     throwIfStringContains[product, "Subscription Fem Diapers - XL1", 
      1.];
     throwIfStringContains[product, "Subscription Fem Diapers - XL", 
      1.];
     
     throwIfStringContains[product, 
      "Subscription Both Diapers - Large2", 2.];
     throwIfStringContains[product, 
      "Subscription Both Diapers - Large1", 1.];
     throwIfStringContains[product, 
      "Subscription Both Diapers - Large", 1.];
     throwIfStringContains[product, 
      "Subscription Both Diapers - Med2", 2.];
     throwIfStringContains[product, 
      "Subscription Both Diapers - Med1", 1.];
     throwIfStringContains[product, 
      "Subscription Both Diapers - Med", 1.];
     throwIfStringContains[product, "Subscription Both Diapers - XL2",
       2.];
     throwIfStringContains[product, 
      "Subscription Both Diapers - XL1", 1.];
     throwIfStringContains[product, "Subscription Both Diapers - XL", 
      1.];
     
     throwIfStringContains[product, "1 Pair", 1.];
     throwIfStringContains[product, "3 Pair", 3.];
     throwIfStringContains[product, "5 Pair", 5.];
     
     
     
     (*If[StringContainsQ[product, "diaper"], Throw [1.]];*)
     
     Throw[1.];
     ];
   
   Catch[
    tykBold$UniqueProducts = 
     tykBold$UniqueProducts[
      All,
      <|
        #,
        "Items" -> parse[#["Product Key"]]
        |> &
      ];
    
    Throw[True];
    ]
   ];



tykBold$Step["Parse Flags"] := Block[
   {
    },
   
   
   Catch[
    tykBold$UniqueProducts = 
     tykBold$UniqueProducts[
      All,
      <|
        #,
        "Has Diaper Boxes" -> #["Line Item Type"] ==  "Diaper Box",
        
        "Has Diaper Bags" -> Or[
        	StringStartsQ[#["Line Item"], "Diapers -"],
        	StringContainsQ[#["Line Item"], "Unicorns Diapers"],
        	StringContainsQ[#["Line Item"], "Cammies Diapers"],
        	StringContainsQ[#["Line Item"], "Littles Rawrs Diapers"]
        ],
        
        "Has Littles Gear Diapers" -> 
         Or[
          
          StringStartsQ[#["Product Key"], "subscription fem diapers"],
          StringStartsQ[#["Product Key"], "subscription masc diapers"],
          
          StringStartsQ[#["Product Key"], 
           "subscription both diapers"]
          ],
        
        "Has Littles Gear Snappies" -> Or[
          StringStartsQ[#["Line Item Type"], "Snappies"],
          StringStartsQ[#["Line Item"], "Snappies"]
          ],
        
        "Has Briefs" -> #["Line Item"] ==  "Briefs",
        
        
        "Has Littles Gear Outfits" -> Or[
          StringStartsQ[#["Line Item Type"], "Outfit"],
          StringStartsQ[#["Line Item"], "Outfit"]
          ],
        
        "Has Denim" -> #["Line Item"] ==  "Denim"
        
        |> &
      ];
    
    Throw[True];
    ]
   ];



tykBold$CountThings[$key_String] := Block[
	{
		slots = tykBold$EmptySlots[$key],
   		uniqueProducts = Null
    },


	Catch[
		
    
		uniqueProducts = tykBold$UniqueProducts[
			Select[#[slots["Has Key"]] &], 
			{
				"Product",
				"Gender",
				"Product Name",
				"Size",
				"Item Count",
				"Items"
			}
		];
    
		If[Length[uniqueProducts] == 0, Throw[slots]];





		slots["Data"] = dsInnerJoin[
			tykBold$ByProducts,
			uniqueProducts,
			"Product"
		][
			All, 
			<|
				"ID" -> #["ID"],
				"Product" -> #["Product"],
				"Gender" -> #["Gender"],
				"Product Name" -> #["Product Name"],
				"Size" -> #["Size"],
				"Items" -> #["Items"],
				"Subs" -> 1,
				"Item Count" -> #["Item Count"],
				"Total Items" -> #["Item Count"] * #["Items"],
				"Next 7 Days Subs" -> #["Next 7 Days"],
				"Next 7 Days Items" ->  #["Next 7 Days"] * #["Item Count"] * #["Items"],
				"Previous Failed" -> #["Previous Failed"],
				"Duration" -> #["Duration"]
			|> &
		
		] // sortById;
        
   
    
		slots["Breakout Counts"] = dsAggregate$sum[
			slots["Data"],
 			{"Gender", "Product Name", "Size", "Items"},
			{"Subs", "Total Items", "Next 7 Days Subs", "Next 7 Days Items", "Previous Failed"}
		] // sortByGenderSizeAndBags;
    
		(*slots["Breakout Counts"] = dsAggregate$count[
			agg,
			{"Gender", "Product Name", "Size", "Items", "Next 7 Days Subs", "Next 7 Days Items", "Previous Failed"},
			dsAggregate$optAggregateName -> "Subs"
		][
			All, 
			<|
				"Gender", 
				"Product Name", 
				"Size" -> #["Size"], 
				"Items" -> #["Items"], 
				"Total Items" -> #["Subs"] * #["Items"],
				"Subs Next 7 Days" -> #["Next 7 Days"],
				"Subs Previous Failed" -> #["Previous Failed"]
			|> &
		] // sortByGenderSizeAndBags;*)
    
    	slots["Counts by Gender and Size"] = dsAggregate$sum[
			slots["Breakout Counts"],
			{"Gender", "Size"},
			{"Subs", "Next 7 Days Subs", "Next 7 Days Items", "Previous Failed"}
		] // sortBySizeAndBags;
		
		
		slots["Counts by Gender"] = dsAggregate$sum[
			slots["Breakout Counts"],
			{"Gender"},
			{"Subs", "Total Items", "Next 7 Days Subs", "Next 7 Days Items", "Previous Failed"}
		] // sortBySizeAndBags;
		
    	slots["Counts by Size"] = dsAggregate$sum[
			slots["Breakout Counts"],
			{"Size"},
			{"Subs", "Item Count", "Total Items", "Next 7 Days Subs", "Next 7 Days Items", "Previous Failed"}
		] // sortBySizeAndBags;		
    
    	slots["Subscription Counts"] = Dataset[
    		{
				<|
					"Subs" -> Total[dsColumnValues[slots["Counts by Gender and Size"], "Subs"]],
					"Total Items" -> Total[dsColumnValues[slots["Counts by Gender and Size"], "Total Items"]],
					"Next 7 Days Subs" ->  Total[dsColumnValues[slots["Counts by Gender and Size"], "Next 7 Days Subs"]],
					"Next 7 Days Items" ->  Total[dsColumnValues[slots["Counts by Gender and Size"], "Next 7 Days Items"]],
					"Previous Failed" ->  Total[dsColumnValues[slots["Counts by Gender and Size"], "Previous Failed"]]
				|>
			}
		];
    
    
    	slots["IDs"] = Union[dsColumnValues[slots["Data"], "ID"]];
   
		slots["Average Duration"] = wwNFloor[Mean[slots["Data"][All, "Duration"] // Normal]];
    
    	Throw[slots]
    ]
];





tykBold$Step["Count Subscriptions"] := Block[
	{
		calcDuration
		
	},
	calcDuration[durations_List] := wwNFloor[Mean[durations]];
	calcDuration[ds_Dataset] := calcDuration[ds[All, "Duration"] // Normal];
	calcDuration[slots_Association] := calcDuration[slots["Data"]];
	
   	tykBold$SnappiesOnlySubscriptions = 
   		tykBold$ActiveSubscriptions[
   			Select[
   				MemberQ[
   					Complement[
						tykBold$LittlesGearSnappies["IDs"],
				     	Union[
				     		tykBold$LittlesGearOutfits["IDs"], 
				     		tykBold$LittlesGearDiapers["IDs"]
				     	]
   					], 
				    #["ID"]
   				]&
   			],
   			{"ID", "Duration"}
  	   	];
   	
	tykBold$SnappiesOnlyCount = Length[tykBold$SnappiesOnlySubscriptions];   	
 	tykBold$SnappiesOnlyAverageDuration = calcDuration[tykBold$SnappiesOnlySubscriptions];
  	
   	tykBold$SnappiesDiaperSubscriptions = 
   		tykBold$ActiveSubscriptions[
   			Select[
   				MemberQ[
					Complement[
						tykBold$LittlesGearDiapers["IDs"], 
						tykBold$LittlesGearOutfits["IDs"]
					], 
				    #["ID"]
   				]&
   			],
   			{"ID", "Duration"}
  	   	];   	
   	
	tykBold$SnappiesDiapersCount = Length[tykBold$SnappiesDiaperSubscriptions];
	tykBold$SnappiesDiapersAverageDuration = calcDuration[tykBold$SnappiesDiaperSubscriptions];
		
		
	tykBold$SnappiesOutfitsSubscriptions = 
   		tykBold$ActiveSubscriptions[
   			Select[
   				MemberQ[
					Complement[
						tykBold$LittlesGearOutfits["IDs"], 
						tykBold$LittlesGearDiapers["IDs"]					
					], 
				    #["ID"]
   				]&
   			],
   			{"ID", "Duration"}
  	   	];   	
   			   	
	tykBold$SnappiesOutfitsCount = Length[tykBold$SnappiesOutfitsSubscriptions];
	tykBold$SnappiesOutfitsAverageDuration = calcDuration[tykBold$SnappiesOutfitsSubscriptions];
		
	tykBold$SnappiesDiapersOutfitsSubscriptions	= 
   		tykBold$ActiveSubscriptions[
   			Select[
   				MemberQ[
					Intersection[
						tykBold$LittlesGearOutfits["IDs"], 
						tykBold$LittlesGearDiapers["IDs"]
					], 
				    #["ID"]
   				]&
   			],
   			{"ID", "Duration"}
  	   	]; 
  	   			
	tykBold$SnappiesDiapersOutfitsCount =  Length[tykBold$SnappiesDiapersOutfitsSubscriptions];
	tykBold$SnappiesDiapersOutfitsAverageDuration =  calcDuration[tykBold$SnappiesDiapersOutfitsSubscriptions];

		
		
	tykBold$LittlesGearSubscriptions = tykBold$ActiveSubscriptions[
   		Select[
   			MemberQ[
				Union[
					tykBold$LittlesGearDiapers["IDs"], 
					tykBold$LittlesGearSnappies["IDs"], 
					tykBold$LittlesGearOutfits["IDs"]
				], 
				#["ID"]
   				]&
   			],
    		{"ID", "Duration"}
  	   	]; 	
  	   
		
	tykBold$LittlesGearCount = Length[tykBold$LittlesGearSubscriptions];
	tykBold$LittlesGearAverageDuration = calcDuration[tykBold$LittlesGearSubscriptions];

		
	tykBold$DiapersBoxCount = Length[tykBold$DiaperBoxes["Data"]];
	tykBold$DiapersBagCount = Length[tykBold$DiaperBags["Data"]];
	tykBold$DiapersCount = tykBold$DiapersBoxCount + tykBold$DiapersBagCount + tykBold$SnappiesDiapersCount;

	tykBold$SubscriptionsCount = tykBold$LittlesGearCount + tykBold$DiapersBoxCount + tykBold$DiapersBagCount;			
      

	

	tykBold$AllAverageDuration = calcDuration[tykBold$ActiveSubscriptions[All, "Duration"] // Normal];
	
	
	tykBold$DiapersAverageDuration = calcDuration[
		Join[
			tykBold$DiaperBoxes["Data"][All, "Duration"] // Normal,
			tykBold$DiaperBags["Data"][All, "Duration"] // Normal,
			tykBold$LittlesGearDiapers["Data"][All, "Duration"] // Normal
		]
	];
	
	tykBold$LittlesGearSnappies["Average Duration"] = calcDuration[tykBold$LittlesGearSnappies];		
	
	tykBold$DiapersBoxAverageDuration = calcDuration[tykBold$DiaperBoxes];
	tykBold$DiapersBagAverageDuration = calcDuration[tykBold$DiaperBags];
	
	tykBold$LittlesGearDiapersAverageDuration = calcDuration[tykBold$LittlesGearDiapers["Data"]];
	
	tykBold$LittlesGearOutfitsAverageDuration = calcDuration[tykBold$LittersGearOutfits["Data"]];
	
		
	



	tykBold$SubscriptionSummaryTable = Dataset[
		{
			<| "Type" -> "Diapers", "Subs" -> tykBold$DiapersCount, "Avg Duration" -> tykBold$DiapersAverageDuration |>,
			<| "Type" -> "Littles Gear", "Subs" -> tykBold$LittlesGearCount, "Avg Duration" -> tykBold$LittlesGearAverageDuration |>,
			<| "Type" -> "Total", "Subs" -> tykBold$SubscriptionsCount, "Avg Duration" -> tykBold$AllAverageDuration |>
		}
	];
	

	tykBold$DiapersSummaryTable = Dataset[
		{
			<| "Type" -> "Diaper Boxes", "Subs" -> tykBold$DiapersBoxCount, "Avg Duration" -> tykBold$DiapersBoxAverageDuration |>,
			<| "Type" -> "Diaper Bags", "Subs" -> tykBold$DiapersBagCount, "Avg Duration" -> tykBold$DiapersBagAverageDuration |>,
			<| "Type" -> "Littles Gear", "Subs" -> tykBold$SnappiesDiapersCount, "Avg Duration" -> tykBold$SnappiesDiapersAverageDuration |>
		}
	];
	
	tykBold$LittlesGearSummaryTable = Dataset[
		{
			<| "Type" -> "Snappies Only", "Subs" -> tykBold$SnappiesOnlyCount, "Avg Duration" -> tykBold$SnappiesOnlyAverageDuration |>,
			<| "Type" -> "Snappies & Outfits", "Subs" -> tykBold$SnappiesOutfitsCount, "Avg Duration" -> tykBold$SnappiesOutfitsAverageDuration |>,
			<| "Type" -> "Snappies & Diapers", "Subs" -> tykBold$SnappiesDiapersCount, "Avg Duration" -> tykBold$SnappiesDiapersAverageDuration |>,
			<| "Type" -> "Snappies & Outfits & Diapers", "Subs" -> tykBold$SnappiesDiapersOutfitsCount, "Avg Duration" -> tykBold$SnappiesDiapersOutfitsAverageDuration |>,
			<| "Type" -> "Total", "Subs" -> tykBold$LittlesGearCount, "Avg Duration" -> tykBold$LittlesGearAverageDuration |>
		}
	];	
	
	True
			

];


tykBold$Step["Count Products"] := 
  Block[
   {},

	


   tykBold$DiaperBags = tykBold$CountThings["Diaper Bags"];
   tykBold$DiaperBoxes = tykBold$CountThings["Diaper Boxes"];
   tykBold$LittlesGearDiapers = tykBold$CountThings["Littles Gear Diapers"];
   tykBold$LittlesGearOutfits = tykBold$CountThings["Littles Gear Outfits"];
   tykBold$LittlesGearSnappies = tykBold$CountThings["Littles Gear Snappies"];
   
   
   tykBold$Briefs = tykBold$CountThings["Briefs"];
   tykBold$Denim = tykBold$CountThings["Denim"];   

   
   
   
   	True
   ];






tykBold$Step["Build Report"] := tykBold$BuildReport[];


tykBold$Step["Export Data"] := Block[
	{},
	
	     
    exportExcel[
    	tykBold$SubscriptionsExcelFile,
    	{
    		"Active" -> tykBold$ActiveSubscriptions,
    		"Paused" -> tykBold$PausedSubscriptions,
    		"Next 7 Days" -> tykBold$ActiveSubscriptions[Select[#["Next 7 Days"] > 0 &], All],
    		"Previous Failed" -> tykBold$ActiveSubscriptions[Select[#["Previous Failed"] > 0 &], All],
    		"Diaper Boxes" -> tykBold$DiaperBoxes["Data"],
    		"Diaper Bags" -> tykBold$DiaperBags["Data"],
    		"Littles Gear Diapers" -> tykBold$LittlesGearDiapers["Data"],
    		"Littles Gear Snappies" -> tykBold$LittlesGearSnappies["Data"],
    		"Littles Gear Outfits" -> tykBold$LittlesGearOutfits["Data"],
     		"Inactive" -> tykBold$InactiveSubscriptions
   		

    	}
    ];
    
    True
];

End[]

EndPackage[]

