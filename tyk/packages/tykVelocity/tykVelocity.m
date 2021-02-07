(* Wolfram Language Package *)

(* Created by the Wolfram Workbench Jun 21, 2020 *)


BeginPackage[
	"tykVelocity`",
	{
		"ww`",
		"tykCommon`",
		"tykVelocityUtils`",
		"tykVelocityPayload`",
		"tykVelocityReport`",
		
		"wwBootstrap`",
		"wwCSV`",
		"wwExcel`",
		"wwStrings`",
		"wwDatasets`",      
		"wwFileSystem`",
		"wwPatterns`",
		"wwMath`"
	}
];

tykVelocity$Run::usage = "tykVelocity$Run  "


tykVelocity$Step::usage = "tykVelocity$Step  ";





(* Exported symbols added here with SymbolName::usage *) 

Begin["`Private`"]
(* Implementation of the package *)


tykVelocity$Step[_, False, stopAfter_String: ""] := False;
tykVelocity$Step[step_String, True, stopAfter_String: ""] := Block[
	{
		time,
		result
	},
	Print[step];
	{time, result} = AbsoluteTiming[tykVelocity$Step[step]];
	Print[Round[time, 0.001]];
	
	If[
		stopAfter != step, 
		result,
		False
	]
	
];

tykVelocity$Step["Done"] := Block[
	{},
	tykVelocity$EndTime = Now;
	tykVelocity$RunTime = tykVelocity$EndTime - tykVelocity$StartTime;
	True
];


tykVelocity$Step[_] := False;

tykVelocity$Run[stopAfter_String: ""] := Block[
	{
		step
	},
	
	step[stepName_String] := tykVelocity$Step[stepName, #, stopAfter] &;
	
	
	RightComposition[
		step["Init Project"],
		step["Init Payload"],
		
		step["Replicate Data"],
		step["Get Data"],
		
		step["Get Diaper Velocity Data"],
		step["Get Underwear Velocity Data"],
		step["Get Denim Velocity Data"],
		step["Get Snappies Velocity Data"],
		
		step["Build Report"],
		step["Publish Report"],

		step["Build Order Forms"],
		step["Publish Order Forms"],
		
		step["Done"]

	][True];	
	


	(*TODO Add validatation before returning True*)
  
	True


];



tykVelocity$Step["Init Project"] := Block[
	{
	},
	True
];


tykVelocity$Step["Init Payload"] := Block[
	{
	},
	tykVelocity$InitPayload[];
	True
];


tykVelocity$Step["Replicate Data"] := Block[
	{
	},
	

	
	fs$CopyIfNewer[
		"/Volumes/mike/Code/tykables/tykables-data/skulabs/datasets/sales-velocity.csv",
		tykVelocity$RawSalesVelocityFile
	];
	
	
	fs$CopyIfNewer[
		"/Volumes/mike/Code/tykables/tykables-tableau/datasets/skulabs/order-item-prices.csv",
		tykVelocity$OrderItemPricesFile
	];	
	

	
	True
];





tykVelocity$Step["Get Data"] := Block[
	{
		raw
	},
  
	raw = csv$Import[
		tykVelocity$RawSalesVelocityFile
	][
		All, 
		<|
			"SKU" -> "Product Sku",
			"Product" -> "Product Name",
			"On Hand" -> "On Hand",
			"Category" -> "Product Category",
			"Color" -> "Product Color",
			"Pattern" -> "Product Pattern",
			"Brand" -> "Product Brand",
			"Theme" -> "Product Theme",
			"Gender" -> "Product Gender",
			"Size" -> "Product Size",
			"Style" -> "Product Style",
			"Type" -> "Product Type",
			"Days On Sale 90" -> "Days On Sale 90 Adj",
			"Days On Sale 180" -> "Days On Sale 180 Adj",
			"Velocity 90" -> "Velocity 90 Adj",
			"Velocity 180" -> "Velocity 180 Adj",
			"Sold 90" -> "Sold 90 Adj",
			"Sold 180" -> "Sold 180 Adj",
			"Unshipped" -> "Sold But Not Shipped",
			"Velocity 14" -> "Velocity 14 Adj"
		|>
    
	][
		Select[
			And[
				! StringStartsQ[#["Product"], "Subscript"],
				! StringStartsQ[#["Pattern"], "Subscript"],
				! StringStartsQ[#["Color"], "Subscript"],
				! StringStartsQ[#["Product"], "Plush"],
				! StringStartsQ[#["Product"], "Clear"]
			] &
		],
		<|
			#,
			
			"Brand" -> If[
				StringStartsQ[#["Product"], "Tena Slip"], 
				"Tena Slip", 
				#["Brand"]
			],
			"Color" -> firstNonOther[{ #["Color"], #["Pattern"], #["Theme"]}]	
		|> &
	];
  

  
  
	raw = raw[
		All,
		<|
			#,
			"SKU" -> StringReplace[str$Tidy[#["SKU"]], {PunctuationCharacter, WhitespaceCharacter} -> ""],
			"Color" -> firstNonOther[{ #["Color"], #["Pattern"], #["Theme"]}],
			"Pattern" -> firstNonOther[{ #["Pattern"], #["Theme"], #["Color"]}],
			"Theme" ->  firstNonOther[{ #["Theme"], #["Pattern"], #["Color"]}],
			"Need 90" -> wwNFloor[tdyZeroIfNegative[(90 *#["Velocity 90"]) - #["On Hand"] - #["Unshipped"] ]],
			"Need 180" -> wwNFloor[tdyZeroIfNegative[(180 *  #["Velocity 180"]) - #["On Hand"] - #["Unshipped"]]],
			"Days Left 90" -> wwNFloor[tdyZeroIfNegative[tdyDivide[#["On Hand"] - #["Unshipped"], #["Velocity 90"]]]],
			"Days Left 180" -> wwNFloor[tdyZeroIfNegative[tdyDivide[#["On Hand"] - #["Unshipped"] , #["Velocity 180"]]]],
			"Available" -> #["On Hand"] - #["Unshipped"],
			"Inventory" -> tdyZeroIfNegative[#["On Hand"] - #["Unshipped"]],
			"Velocity 90" -> Round[#["Velocity 90"], 0.01],
			"Velocity 180" -> Round[#["Velocity 180"], 0.01],
			"Velocity 14" -> Round[#["Velocity 14"], 0.01],
			"Size Order" -> Lookup[mapSizeToSizeOrder, #["Size"], 0],
			"Section" ->
				Catch[
					If[#["Category"] == "Diapers", Throw["Diapers"]] ;
					If[#["Pattern"] == "Jeans", Throw["Denim"]];
					If[#["Style"] == "Pants", Throw["Denim"]];
					If[#["Style"] == "Underwear", Throw["Underwear"]];
					If[#["Style"] == "Snappies", Throw["Snappies"]];
				],
			"Subsection" ->
				Catch[
					If[#["Category"] == "Diapers" && #["Brand"] != "Tena Slip", Throw["Diapers by Tykables and Get Nappy"]];
					If[#["Category"] == "Diapers", Throw["Diapers by Tena Slip"]];
					If[#["Style"] == "Snappies", Throw[ firstNonOther[{ #["Pattern"], #["Theme"], #["Color"]}]]];
					Throw[Null]
				],
			"Type" -> If[#["Style"] == "Jeans", "Jeans", #["Type"]]
      
		|> &
	];
  
	raw = raw[
		Select[
			! MemberQ[
				{
					"Bear Hugs",
					"Bright Days",
					"Inspire+Incontrol",
					"Nursery",
					"Rebel",
					"Princess",
					"Safari",
					"Waddler",
					"Metro"
				},
				#["Pattern"]
			] &
		],
		<|
			#,
			"Pattern" -> If[#["Style"] == "Jeans", "No Wash", #["Pattern"]]
		|> &
	];
  
  tykVelocity$VelocityData = raw[
    Select[
     ! StringMatchQ[#["SKU"], 
        "02" ~~ "01" | "03" | "05" | "11" ~~ DigitCharacter ~~ 
         DigitCharacter ~~ "ON0"] &],
    All
    ];
  
  
  True
  
];








tykVelocity$Step["Build Report"] := Block[

	{
		
		inventoryBySection = tykVelocity$VelocityData[GroupBy["Section"], Total, "Inventory"]
	},


	tykVelocity$Report = Join[
		tykVelocity$BuildDiaperIntroPage[inventoryBySection],
		tykVelocity$BuildDiaperPages[],
		tykVelocity$BuildUnderwearPages[],
		tykVelocity$BuildDenimPages[],
		tykVelocity$BuildSnappiesPages[]
	];


  
  	True
	
];


tykVelocity$Step["Publish Report"] := Module[
	{},
	buildTykablesNotebook[
		tykVelocity$Report,
		"title" -> "Weekly Sales Velocity Report"
	] // NotebookPrint[#, tykVelocity$ReportFile] &;
	
	True
];



  


tykVelocity$Step["Get Underwear Velocity Data"] := Block[

	{
	},

	tykVelocity$UnderwearVelocityData = tykVelocity$VelocityData[
	     Select[
	      #["Section"] == "Underwear" &
	      ],
	     {
	      "Color",
	      "Gender",
	      "Size",
	      "Inventory",
	      "Days On Sale 180",
	      "Velocity 180",
	      "Days Left 180",
	      "Need 180",
	      "Size Order",
	      "Velocity 14"
	      }
	     ] // sortUnderwear // KeyDrop[{"Size Order", "Section"}];

	tykVelocity$UnderwearVelocityData = dsAddColumn[
	   tykVelocity$UnderwearVelocityData,
	   <|
	     "Gender" -> "Boy"
	     |> &
	   ];
	
	
	tykVelocity$UnderwearInventory = Total[dsColumnValues[tykVelocity$UnderwearVelocityData, "Inventory"]];
	
	tykVelocity$UnderwearVelocityData =  dsAddColumn[
	   tykVelocity$UnderwearVelocityData,
	   <|
	     
	     "% of Inventory" -> 
	      Round[100 *tdyDivide[#["Inventory"], tykVelocity$UnderwearInventory]]
	     |> &
	   ];
	   
	True

];













tykVelocity$Step["Get Diaper Velocity Data"] := Block[
	{
	},
	

	tykVelocity$DiaperVelocityData = tykVelocity$VelocityData[
	   Select[
	    #["Section"] == "Diapers" &
	    ],
	   <|
	     #,
	     "Need Cases 90" -> wwNCeiling[#["Need 90"] / 4. ]
	     |> &
	   ][
	   All,
	   {
	    "Pattern",
	    "Product",
	    "Size",
	    "Inventory",
	    "Days On Sale 90",
	    "Velocity 90",
	    "Days Left 90",
	    "Need 90",
	    "Need Cases 90",
	    "Size Order",
	    "Section",
	    "Subsection",
	    "Velocity 14"
	    }
	   ] ;
	
	
	tykVelocity$DiaperVelocityData = tykVelocity$DiaperVelocityData[
	   All,
	   <|
	     #,
	     "Pattern" -> Catch[
	       If[StringContainsQ[#["Product"], "Play Dayz Pink"], 
	        Throw[#["Pattern"] <> " Pink"]];
	       If[StringContainsQ[#["Product"], "Play Dayz Blue"], 
	        Throw[#["Pattern"] <> " Blue"]];
	       Throw[#["Pattern"]];
	       ]
	     |> &
	   ];
	
	tykVelocity$DiaperVelocityData = tykVelocity$DiaperVelocityData[
	   All,
	   <|
	     #,
	     "Pattern" -> Catch[
	       If[StringContainsQ[#["Product"], "Plastic"], 
	        Throw[#["Pattern"] <> " (Plastic)"]];
	       If[StringContainsQ[#["Product"], "Cloth"], 
	        Throw[#["Pattern"] <> " (Cloth)"]];
	       Throw[#["Pattern"]];
	       ]
	     |> &
	   ];
	   
	 True

];



tykVelocity$Step["Get Denim Velocity Data"] := Block[
	{
		
	},
	
	tykVelocity$DenimVelocityData = tykVelocity$VelocityData[
	     Select[
	      And[
	        #["Section"] == "Denim",
	        notMemberQ[
	         {
	          "No Wash Shorts",
	          "No Wash Skirt", 
	          "No Wash Skirtall",
	          "Kaki Shortalls"
	          },
	         #["Pattern"] <> " " <> #["Type"]
	         ]
	        ] &
	      ],
	     {
	      "Pattern",
	      "Type",
	      "Size",
	      "Inventory",
	      "Days On Sale 180",
	      "Velocity 180",
	      "Days Left 180",
	      "Need 180",
	      "Size Order",
	      "Velocity 14"
	      }
	     ] // sortDenim // KeyDrop[{"Size Order", "Section"}];
	
	
	tykVelocity$DenimInventory = Total[dsColumnValues[tykVelocity$DenimVelocityData, "Inventory"]];
	
	tykVelocity$DenimVelocityData =  dsAddColumn[
	   tykVelocity$DenimVelocityData,
	   <|
	     
	     "% of Inventory" -> 
	      Round[100 *tdyDivide[#["Inventory"], tykVelocity$DenimInventory]]
	     |> &
	   ];
	   
	True
	   
];





tykVelocity$Step["Get Snappies Velocity Data"] := Block[
	{
	},
	 
	tykVelocity$SnappiesVelocityData = tykVelocity$VelocityData[
		Select[
			And[
				#["Section"] == "Snappies",
				! MemberQ[{"Pink", "Little Builder", "Metro"}, #["Pattern"]] 
    			] &
    	],
   		{
			"Pattern",
			"Type",
			"Size",
			"Inventory",
			"Days On Sale 180",
			"Velocity 180",
			"Days Left 180",
			"Need 180",
			"Size Order",
			"Subsection",
			"Velocity 14"
		}
	];

	tykVelocity$SnappiesVelocityData = 
	  Dataset[Union[tykVelocity$SnappiesVelocityData // Normal]] // sortSnappies // 
	   KeyDrop[{"Size Order", "Section"}] ;
	
	exportExcel[ tykVelocity$ExcelFile, 
	  tykVelocity$SnappiesVelocityData // KeyDrop["Subsection"]];



	tykVelocity$SnappiesInventory = 
	  Total[dsColumnValues[tykVelocity$SnappiesVelocityData, "Inventory"]];

	tykVelocity$SnappiesVelocityData = dsAddColumn[
	   tykVelocity$SnappiesVelocityData,
	   <|
	     
	     "% of Inventory" -> 
	      Round[100 *tdyDivide[#["Inventory"], tykVelocity$SnappiesInventory]]
	     |> &
	   ];
	   
	True
];
 
 


tykVelocity$Step["Build Order Forms"] := Block[
	{},
	
	
	tykVelocity$BuildDiapersOrderForm[];
	tykVelocity$BuildDenimOrderForm[];
	tykVelocity$BuildUnderwearOrderForm[];
	tykVelocity$BuildSnappiesOrderForm[];
	
	True

];

tykVelocity$Step["Publish Order Forms"] := Block[
	{},
	
	exportExcel[ 
		tykVelocity$OrderFormFile, 
		{
			"Order Form" -> dsUnion[
			  	tykVelocity$DiapersOrderForm,
			 	tykVelocity$DenimOrderForm,
			  	tykVelocity$SnappiesOrderForm,
			  	tykVelocity$UnderwearOrderForm
		  	]
		}
		
	];
	 
	 
	True
];







End[];

EndPackage[]

