(* Wolfram Language Package *)

BeginPackage[
	"tykVelocityReport`", 
	{
		"ww`",
		"tykCommon`",
		"tykVelocityUtils`",
		"tykVelocityPayload`",
		"wwBootstrap`",
		"wwCSV`",
		"wwExcel`",
		"wwStrings`",
		"wwDatasets`",      
		"wwFileSystem`",
		"wwPatterns`",
		"gridCharts`",
		"chartLayouts`",
		"chartUtil`"
	}]
	
	wwPackage`packageClearSymbols["tykVelocityReport"];

tykVelocity$BuildDiaperIntroPage::usage = "tykVelocity$BuildDiaperIntroPage  "

tykVelocity$BuildDiaperPages::usage = "tykVelocity$BuildDiaperPages  "

tykVelocity$BuildUnderwearPages::usage = "tykVelocity$BuildUnderwearPages  "

tykVelocity$BuildDenimPages::usage = "tykVelocity$BuildDenimPages  "

tykVelocity$BuildSnappiesCell::usage = "tykVelocity$BuildSnappiesCell  "

tykVelocity$BuildSnappiesPages::usage = "tykVelocity$BuildSnappiesPages  "

tykVelocity$DiapersOrderFormTemplate::usage = "";

tykVelocity$BuildDiapersOrderForm::usage = "tykVelocity$BuildDiapersOrderForm  "

tykVelocity$BuildSnappiesOrderForm::usage = "tykVelocity$BuildSnappiesOrderForm  "

tykVelocity$BuildUnderwearOrderForm::usage = "tykVelocity$BuildUnderwearOrderForm  "

tykVelocity$BuildDenimOrderForm::usage = "tykVelocity$BuildDenimOrderForm  "

tykVelocity$DenimOrderFormTemplate::usage = "tykVelocity$DenimOrderFormTemplate  "

(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 






tykVelocity$BuildDiaperIntroPage[inventoryBySection_Dataset] := Block[
	{

		diaperInventoryBySize,
		diaperInventoryPercent,
		diaperIntroPage
	},
	
	
	tykVelocity$DubblerInventory = tykVelocity$DiaperVelocityData[
		Select[#["Pattern"] == "Dubbler" &], 
    	"Inventory"
    	] // Normal // First;
	
	tykVelocity$DiaperInventory = inventoryBySection["Diapers"] - tykVelocity$DubblerInventory;
	
	


	diaperInventoryBySize = tykVelocity$VelocityData[
		Select[#["Category"] == "Diapers" &], 
		All
	][ 
	    GroupBy["Size Order"], 
	    Total, 
	    "Inventory"
	] // Normal;
	

	diaperInventoryPercent = Round[
		100 * Map[
		    tdyDivide[#, tykVelocity$DiaperInventory] &, 
		     Values[diaperInventoryBySize]
		]
	];


	diaperIntroPage = {
		outputCell[
			dsCreateDataset[
				<|
					"Size" -> Lookup[mapSizeOrderToSize, Keys[diaperInventoryBySize], "Unknown"],
					"Size Order" ->  Keys[diaperInventoryBySize],
					"Inventory" -> Values[diaperInventoryBySize],
					"% Inventory" -> diaperInventoryPercent
				|>
			][
				All,
					<| 
						#,
						"% Inventory" -> If[#["Size"] == "One Size", "", #["% Inventory"]]
					|> &
	   		] //  SortBy[#"Size Order" &] // KeyDrop["Size Order"],
	   		PageBreakBelow -> True
	   ]
	}



];


tykVelocity$BuildDiaperPages[] := Block[

	{
		format,
		diaperVelocityTykablesData,
		diaperVelocityTenaSlipData
		
	},



	   
   format[$ds_Dataset] := 
    Block[
     {
      ds = $ds
      },
     
     dsSpanDuplicates[
      ds[
       All, <|
         #, 
         "Size" -> highlight[#["Days Left 90"], #["Size"]],
         "Inventory" -> highlight[#["Days Left 90"], #["Inventory"]],
         "Days On Sale 90" -> highlight[#["Days Left 90"], #["Days On Sale 90"]],
         "Velocity 90" -> highlight[#["Days Left 90"], #["Velocity 90"]],
         "Days Left 90" -> highlight[#["Days Left 90"], #["Days Left 90"]],
         "Need 90" -> highlight[#["Days Left 90"], #["Need 90"]],
         "Need Cases 90" -> highlight[#["Days Left 90"], #["Need Cases 90"]],
         "% of Inventory" -> If[
         	#["Pattern"] == "Dubbler",
         	highlight[#["Days Left 90"], " "],
         	highlight[#["Days Left 90"], #["% of Inventory"]]
         ],
         "Velocity 14" -> highlight[#["Days Left 90"], #["Velocity 14"]]
         |> &
       ],
      "colCount" -> 3
      ]
     ];
  
  

   
   diaperVelocityTykablesData = dsAddColumn[
	   tykVelocity$DiaperVelocityData[
	      Select[#["Subsection"] == "Diapers by Tykables and Get Nappy" &],
	      All
	      ]  // sortDiapers // 
	    KeyDrop[{"Size Order", "Section", "Subsection", "Product"}],
	   <|
	     
	     "% of Inventory" -> 
	      Round[100 *tdyDivide[#["Inventory"], tykVelocity$DiaperInventory]]
	     |> &
	   ];


	diaperVelocityTenaSlipData =  dsAddColumn[
	   tykVelocity$DiaperVelocityData[
	      Select[#["Subsection"] == "Diapers by Tena Slip" &],
	      All
	      ]  // sortDiapers // 
	    KeyDrop[{"Size Order", "Section", "Subsection", "Product"}],
	   <|
	     
	     "% of Inventory" -> 
	      Round[ tdyDivide[#["Inventory"], tykVelocity$DiaperInventory]]
	     |> &
	   ];
   
   
   Join[
    tdyToCells[ 
     format[diaperVelocityTykablesData],
     "title" -> "Tykables Diaper Sales Velocities",
     "rowsPerGrid" -> 36
     ],
    tdyToCells[ 
     format[diaperVelocityTenaSlipData],
     "title" -> "Tena Slip Diaper Sales Velocities",
     "rowsPerGrid" -> 36
     ]
    ]
   
   ];
	

tykVelocity$BuildUnderwearPages[] := Block[
{
	format
},

  format[$ds_Dataset] := Block[
    {
     ds = $ds
     },
    
    dsSpanDuplicates[
     ds[
      All, <|
        #, 
        "Size" -> highlight[#["Days Left 180"], #["Size"]],
        "Inventory" -> highlight[#["Days Left 180"], #["Inventory"]],
        "Days On Sale 180" -> 
         highlight[#["Days Left 180"], #["Days On Sale 180"]],
        "Velocity 180" -> 
         highlight[#["Days Left 180"], #["Velocity 180"]],
        "Days Left 180" -> 
         highlight[#["Days Left 180"], #["Days Left 180"]],
        "Need 180" -> highlight[#["Days Left 180"], #["Need 180"]],
        "% of Inventory" -> 
         highlight[#["Days Left 180"], #["% of Inventory"]],
        "Velocity 14" -> 
         highlight[#["Days Left 180"], #["Velocity 14"]]
        |> &
      ], 
     "colCount" -> 3
     ]
    ];
  
  tdyToCells[ 
   format[tykVelocity$UnderwearVelocityData],
   "title" -> "Underwear Sales Velocities",
   "rowsPerGrid" -> 36
   ]
  
];


tykVelocity$BuildDenimPages[] := Block[
   {
    format
    },
   
   tykVelocity$Step["Get Denim Velocity Data"][];
   
   format[$ds_Dataset] := 
    Block[
     {
      ds = $ds
      },
     
     dsSpanDuplicates[
      ds[
       All, <|
         #, 
         "Size" -> highlight[#["Days Left 180"], #["Size"]],
         "Inventory" -> 
          highlight[#["Days Left 180"], #["Inventory"]],
         "Days On Sale 180" -> 
          highlight[#["Days Left 180"], #["Days On Sale 180"]],
         "Velocity 180" -> 
          highlight[#["Days Left 180"], #["Velocity 180"]],
         "Days Left 180" -> 
          highlight[#["Days Left 180"], #["Days Left 180"]],
         "Need 180" -> highlight[#["Days Left 180"], #["Need 180"]],
         "% of Inventory" -> 
          highlight[#["Days Left 180"], #["% of Inventory"]],
         "Velocity 14" -> 
          highlight[#["Days Left 180"], #["Velocity 14"]]
         |> &
       ], 
      "colCount" -> 3
      ]
     ];
   
   tdyToCells[ 
    format[tykVelocity$DenimVelocityData],
    "title" -> "Denim Sales Velocities",
    "rowsPerGrid" -> 36
    ]
 ];
   
  
tykVelocity$BuildSnappiesCell[subsection_String] := Module[
   {
    data = tykVelocity$SnappiesVelocityData[
    	Select[#["Subsection"] == subsection &], 
       	All
    ] // KeyDrop["Subsection"],
    totalInventory,
    totalVelocity,
    need,
    totalVelocity14
    },
   
   
   
   
   totalInventory = Total[dsColumnValues[data, "Inventory"]];
   totalVelocity = Total[dsColumnValues[data, "Velocity 180"]];
   need = Total[dsColumnValues[data, "Need 180"]];
   totalVelocity14 = Total[dsColumnValues[data, "Velocity 14"]];
   
   data = Dataset[
     Append[
      data // Normal,
      <|
       "Pattern" -> subsection,
       "Type" -> "Total",
       "Size" -> "",
       "Days On Sale 180" -> "",
       "Inventory" -> totalInventory,
       "Velocity 180" -> totalVelocity,
       "Days Left 180" -> "",
       "Need 180" -> need,
       "Velocity 14" -> totalVelocity14,
       "% of Inventory" -> ""
       |>
      ]
     ];
   
   
   data = data[
     All, <|
       #, 
       "Size" -> highlight[#["Days Left 180"], #["Size"]],
       "Inventory" -> highlight[#["Days Left 180"], #["Inventory"]],
       "Days On Sale 180" -> 
        highlight[#["Days Left 180"], #["Days On Sale 180"]],
       "Velocity 180" -> 
        highlight[#["Days Left 180"], #["Velocity 180"]],
       "Days Left 180" -> 
        highlight[#["Days Left 180"], #["Days Left 180"]],
       "Need 180" -> highlight[#["Days Left 180"], #["Need 180"]],
       "% of Inventory" -> 
        highlight[#["Days Left 180"], #["% of Inventory"]],
       "Velocity 14" ->  
        highlight[#["Days Left 180"], #["Velocity 14"]]
       |> &
     ];
   
   
   
   
   tdyToCells[ 
    dsSpanDuplicates[data, "colCount" -> 3],
    "title" -> "Snappies Sales Velocities",
    "rowsPerGrid" -> 48
    ]
   ];


tykVelocity$BuildSnappiesPages[] := Block[
	{},
  

  
  tdyFacetCells[
   Sort[Union[dsColumnValues[tykVelocity$SnappiesVelocityData, "Subsection"]]],
   tykVelocity$BuildSnappiesCell[#] &
   ]
];


tykVelocity$DiapersOrderFormTemplate = Dataset[
   {
    <|"Print" -> "Cammies", "SKU" -> "TCAM", "Size" -> "M"|>,
    <|"Print" -> "Cammies", "SKU" -> "TCAL", "Size" -> "L"|>,
    <|"Print" -> "Camelot", "SKU" -> "UNKM", "Size" -> "M"|>,
    <|"Print" -> "Camelot", "SKU" -> "UNKL", "Size" -> "L"|>,
    <|"Print" -> "Camelot", "SKU" -> "UNKX", "Size" -> "XL"|>,
    <|"Print" -> "Dubbler", "SKU" -> "TDBL", "Size" -> ""|>,
    <|"Print" -> "Galactic", "SKU" -> "TGLM", "Size" -> "M"|>,
    <|"Print" -> "Galactic", "SKU" -> "TGLL", "Size" -> "L"|>,
    <|"Print" -> "Little Rascals", "SKU" -> "NU18001", "Size" -> "M"|>,
    <|"Print" -> "Little Rascals", "SKU" -> "NU18002", "Size" -> "L"|>,
    <|"Print" -> "Overnights", "SKU" -> "TONM", "Size" -> "M"|>,
    <|"Print" -> "Overnights", "SKU" -> "TONL", "Size" -> "L"|>,
    <|"Print" -> "Play Dayz Cloth Blue", "SKU" -> "NU18011", "Size" -> "M"|>,
    <|"Print" -> "Play Dayz Cloth Blue", "SKU" -> "NU18012", "Size" -> "L"|>,
    <|"Print" -> "Play Dayz Plastic Blue", "SKU" -> "NU18013", "Size" -> "M"|>,
    <|"Print" -> "Play Dayz Plastic Blue", "SKU" -> "NU18014", "Size" -> "L"|>,
    <|"Print" -> "Play Dayz Cloth Pink", "SKU" -> "NU18021", "Size" -> "M"|>,
    <|"Print" -> "Play Dayz Cloth Pink", "SKU" -> "NU18022", "Size" -> "L"|>,
    <|"Print" -> "Play Dayz Plastic Pink", "SKU" -> "NU18023", "Size" -> "M"|>,
    <|"Print" -> "Play Dayz Plastic Pink", "SKU" -> "NU18024", "Size" -> "L"|>,
    <|"Print" -> "Pride diaper", "SKU" -> "NU18030", "Size" -> "M"|>,
    <|"Print" -> "Pride diaper", "SKU" -> "NU18031", "Size" -> "L"|>,
    <|"Print" -> "Puppers", "SKU" -> "TPUM", "Size" -> "M"|>,
    <|"Print" -> "Puppers", "SKU" -> "TPUL", "Size" -> "L"|>,
    <|"Print" -> "Little Rawr", "SKU" -> "TLRM", "Size" -> "M"|>,
    <|"Print" -> "Little Rawr", "SKU" -> "TLRL", "Size" -> "L"|>,
    <|"Print" -> "Unicorns", "SKU" -> "TUCM", "Size" -> "M"|>,
    <|"Print" -> "Unicorns", "SKU" -> "TUCL", "Size" -> "L"|>
    }
   ];

tykVelocity$BuildDiapersOrderForm[] := With[
   {
    velocity = tykVelocity$DiaperVelocityData[
      Select[
       And[
         #["Pattern"] != "DryLife",
         #["Pattern"] != "Other"
         ] &
       ],
      <|
        "Print" -> StringReplace[
          #["Pattern"],
          {
           "Pride" -> "Pride diaper",
           "Blue (Cloth)" -> "Cloth Blue",
           "Blue (Plastic)" -> "Plastic Blue",
           "Pink (Cloth)" -> "Cloth Pink",
           "Pink (Plastic)" -> "Plastic Pink",
           "Rawrs" -> "Little Rawr"
           }
          ],
        "Size" -> ReplaceAll[
          #["Size"],
          {
           "Large" -> "L",
           "Medium" -> "M",
           "One Size" -> ""
           }
          ],
        "Stock" -> #["Inventory"],
        "Sales Vol" -> #["Velocity 90"]
        
        |> &
      ]
    },
   
   tykVelocity$DiapersOrderForm = dsLeftJoin[
      tykVelocity$DiapersOrderFormTemplate,
      velocity,
      {"Print", "Size"}
      ][
     All,
     <|
       "Print" -> #["Print"],
       "Size" -> Replace[#["Size"], _Missing -> ""], 
       "SKU" -> #["SKU"], 
       "Stock" -> Replace[#["Stock"], _Missing -> 0], 
       "Price" -> "",
       "Status" -> "USA",
       "Product" -> "Diapers",
       "Sales Vol" -> Replace[#["Sales Vol"], _Missing -> 0],
       "Style" -> ""
       |> &
     ] // SortBy[{#["Print"] &, lookupSizeOrder[#["Size"]] &}];
];
 

tykVelocity$DenimOrderFormTemplate = Dataset[
   {
   	
  
   	
    <|"Key" -> "Kaki Cargo Shorts", "Print" -> "", "Size" -> "S",    "SKU" -> "", "Product" -> "Cargo Shorts", "Style" -> ""|>,
    <|"Key" -> "Kaki Cargo Shorts", "Print" -> "", "Size" -> "M",    "SKU" -> "", "Product" -> "Cargo Shorts", "Style" -> ""|>,
    <|"Key" -> "Kaki Cargo Shorts", "Print" -> "", "Size" -> "L",    "SKU" -> "", "Product" -> "Cargo Shorts", "Style" -> ""|>,
    <|"Key" -> "Kaki Cargo Shorts", "Print" -> "", "Size" -> "XL",   "SKU" -> "", "Product" -> "Cargo Shorts", "Style" -> ""|>,
    <|"Key" -> "Kaki Cargo Shorts", "Print" -> "", "Size" -> "2XL",  "SKU" -> "", "Product" -> "Cargo Shorts", "Style" -> ""|>,
    <|"Key" -> "Kaki Cargo Shorts", "Print" -> "", "Size" -> "3XL",  "SKU" -> "", "Product" -> "Cargo Shorts", "Style" -> ""|>,
    
    <|"Key" -> "No Wash Jeans", "Print" -> "", "Size" -> "S",    "SKU" -> "", "Product" -> "Jeans", "Style" -> "No Wash"|>,
    <|"Key" -> "No Wash Jeans", "Print" -> "", "Size" -> "M",    "SKU" -> "", "Product" -> "Jeans", "Style" -> "No Wash"|>,
    <|"Key" -> "No Wash Jeans", "Print" -> "", "Size" -> "M-L",  "SKU" -> "", "Product" -> "Jeans", "Style" -> "No Wash"|>,
    <|"Key" -> "No Wash Jeans", "Print" -> "", "Size" -> "L",    "SKU" -> "", "Product" -> "Jeans", "Style" -> "No Wash"|>,
    <|"Key" -> "No Wash Jeans", "Print" -> "", "Size" -> "L-L",  "SKU" -> "", "Product" -> "Jeans", "Style" -> "No Wash"|>,
    <|"Key" -> "No Wash Jeans", "Print" -> "", "Size" -> "XL",   "SKU" -> "", "Product" -> "Jeans", "Style" -> "No Wash"|>,
    <|"Key" -> "No Wash Jeans", "Print" -> "", "Size" -> "XL-L", "SKU" -> "", "Product" -> "Jeans", "Style" -> "No Wash"|>,
    <|"Key" -> "No Wash Jeans", "Print" -> "", "Size" -> "2XL",  "SKU" -> "", "Product" -> "Jeans", "Style" -> "No Wash"|>,
    <|"Key" -> "No Wash Jeans", "Print" -> "", "Size" -> "3XL",  "SKU" -> "", "Product" -> "Jeans", "Style" -> "No Wash"|>,
    
    <|"Key" -> "No Wash Shortalls", "Print" -> "", "Size" -> "S",   "SKU" -> "", "Product" -> "Shortalls", "Style" -> "No Wash"|>,
    <|"Key" -> "No Wash Shortalls", "Print" -> "", "Size" -> "M",   "SKU" -> "", "Product" -> "Shortalls", "Style" -> "No Wash"|>,
    <|"Key" -> "No Wash Shortalls", "Print" -> "", "Size" -> "L",   "SKU" -> "", "Product" -> "Shortalls", "Style" -> "No Wash"|>,
    <|"Key" -> "No Wash Shortalls", "Print" -> "", "Size" -> "XL",  "SKU" -> "", "Product" -> "Shortalls", "Style" -> "No Wash"|>,
    <|"Key" -> "No Wash Shortalls", "Print" -> "", "Size" -> "2XL", "SKU" -> "", "Product" -> "Shortalls", "Style" -> "No Wash"|>,
    <|"Key" -> "No Wash Shortalls", "Print" -> "", "Size" -> "3XL", "SKU" -> "", "Product" -> "Shortalls", "Style" -> "No Wash"|>
    }
   ];
   
   
tykVelocity$BuildDenimOrderForm[] := With[
   {
    velocity = tykVelocity$DenimVelocityData[
      All,
      <|
        "Key" -> #["Pattern"] <> " " <> #["Type"],
        "Size" -> mapSizeToAbbreviation[#["Size"]],
        "Stock" -> #["Inventory"],
        "Sales Vol" -> #["Velocity 180"]
        |> &
      ]
    },
   
   tykVelocity$DenimOrderForm = dsLeftJoin[
      tykVelocity$DenimOrderFormTemplate,
      velocity,
      {"Key", "Size"}
      ][
     All,
     <|
       "Print" -> #["Print"],
       "Size" -> Replace[#["Size"], _Missing -> ""], 
       "SKU" -> #["SKU"], 
       "Stock" -> Replace[#["Stock"], _Missing -> 0], 
       "Price" -> "",
       "Status" -> "USA",
       "Product" -> #["Product"],
       "Sales Vol" -> Replace[#["Sales Vol"], _Missing -> 0],
       "Style" -> #["Style"]
       |> &
     ] // SortBy[{#["Print"] &, lookupSizeOrder[#["Size"]] &}];
     
     
     True
     
  ];


tykVelocity$BuildUnderwearOrderForm[] := With[
   {
   	template = Dataset[
      dsCrossJoin[
       Dataset[
        Map[
         <|"Print" -> #, "SKU" -> ""|> &,
         tykVelocity$UnderwearVelocityData[
             All,
             <|
               "Print" -> #["Color"] 
               |> &
             ][All, "Print"]  // Normal // Union
         ]
        ] ,
       Dataset[
        Map[
         <|"Size" -> #|> &,
         Union[Values[mapSizeToAbbreviation]] /. {"" -> Nothing}
         ]
        ]
       ]
      ] [All, All] // Select[
     ! StringEndsQ[#["Size"], "-L"] &
     ] // SortBy[{#["Print"] &, lookupSizeOrder[#["Size"]] &}],
     
    velocity = tykVelocity$UnderwearVelocityData[
      All,
      <|
        "Print" -> #["Color"],
        "Size" -> mapSizeToAbbreviation[#["Size"]],
        "Stock" -> #["Inventory"],
        "Sales Vol" -> #["Velocity 180"]
        |> &
      ]
    },
   
   tykVelocity$UnderwearOrderForm = dsLeftJoin[
      template,
      velocity,
      {"Print", "Size"}
      ][
     All,
     <|
       "Print" -> #["Print"],
       "Size" -> Replace[#["Size"], _Missing -> ""], 
       "SKU" -> #["SKU"], 
       "Stock" -> Replace[#["Stock"], _Missing -> 0], 
       "Price" -> "",
       "Status" -> "USA",
       "Product" -> "Snappies",
       "Sales Vol" -> Replace[#["Sales Vol"], _Missing -> 0],
       "Style" -> "Underwear"
       |> &
     ] // SortBy[{#["Print"] &, lookupSizeOrder[#["Size"]] &}];
     
   ];

tykVelocity$BuildSnappiesOrderForm[] := Module[
   {
   	template,
   	velocity
   }
   ,
   	
    template = Dataset[
      dsCrossJoin[
       Dataset[
        tykVelocity$SnappiesVelocityData[
          All,
           <|
             "Print" -> #["Pattern"] ,
             "Style" -> #["Type"]
             |> &
           ] // Normal // Union
        ] ,
       Dataset[
        Map[
         <|"Size" -> #|> &,
         Union[Values[mapSizeToAbbreviation]] /. {"" -> Nothing}
         ]
        ]
       ]
      ] [All, All] // Select[
     Or[
      ! StringEndsQ[#["Size"], "-L"],
       Or[
        StringContainsQ[#["Style"], "Romper"],
        StringContainsQ[#["Style"], "Sleeper"]
        ]
       ] &
     ] // SortBy[{#["Print"] &, #["Style"] &, 
     lookupSizeOrder[#["Size"]] &}];
    
    
    velocity = tykVelocity$SnappiesVelocityData[
      All,
      <|
        "Print" -> #["Pattern"],
        "Style" -> #["Type"],
        "Size" -> mapSizeToAbbreviation[#["Size"]],
        "Stock" -> #["Inventory"],
        "Sales Vol" -> #["Velocity 180"],
        "180" -> #["Need 180"],
        "Days Left" -> #["Days Left 180"]
        |> &
      ];
   
   tykVelocity$SnappiesOrderForm = dsLeftJoin[
      template,
      velocity,
      {"Print", "Style", "Size"}
      ][
     All,
     <|
       "Print" -> #["Print"],
       "Size" -> Replace[#["Size"], _Missing -> ""], 
       "SKU" -> Replace[#["SKU"], _Missing -> ""], 
       "Stock" -> Replace[#["Stock"], _Missing -> 0], 
       "Price" -> "", 
       "Status" -> "USA",
       "Product" -> "Snappies",
       "Sales Vol" -> Replace[#["Sales Vol"], _Missing -> 0],
       "Style" -> #["Style"]
       |> &
     ] //  
    SortBy[{#["Print"] &, #["Style"] &, 
      lookupSizeOrder[#["Size"]] &}];
      
      
(*	tykVelocity$SnappiesOrderFormPivot = dsPivot[
		tykVelocity$SnappiesOrderForm, 
		"Print", 
		"Style", 
		"180", 
		Total
	];     
*)
  		 
   ];
  


End[]; (* End Private Context *)

wwPackage`packageProtectSymbols["tykVelocityReport"];

EndPackage[]