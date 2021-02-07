(* Wolfram Language Package *)

(* Created by the Wolfram Workbench Jun 21, 2020 *)

BeginPackage[
	"tykFedEx`",
	{
		"tykCommon`",
		"tykFedExUtils`",
		"tykFedExPayload`",
		"tykFedExData`",
		"tykFedExReport`",
		"ww`",
		"wwBootstrap`",
		"wwMath`",
		"wwCSV`",
		"wwExcel`",
		"wwFileSystem`",
		"wwStrings`",
		"wwNumbers`",
		"wwDatasets`",
		"wwDates`",
		"wwMoney`",
		"wwUnits`"
		
	}
]


tykFedEx$Run::usage = "tykFedEx$Run  "









(* Exported symbols added here with SymbolName::usage *) 

Begin["`Private`"]
(* Implementation of the package *)





(* ::Section:: *)
(* Stepper *)


tykFedEx$Step[_, False, stopAfter_String: ""] := False;
tykFedEx$Step[step_String, True, stopAfter_String: ""] := Block[
	{
		time,
		result
	},
	Print[step];
	{time, result} = AbsoluteTiming[tykFedEx$Step[step]];
	Print[Round[time, 0.001]];
	
	If[
		stopAfter != step, 
		result,
		False
	]
	
];

tykFedEx$Step["Done"] := Block[
	{},
	tykFedEx$EndTime = Now;
	tykFedEx$RunTime = tykFedEx$EndTime - tykFedEx$StartTime;
	True
];


tykFedEx$Step[_] := False;

tykFedEx$Run[stopAfter_String: ""] := Block[
	{
		step
	},
	
	step[stepName_String] := tykFedEx$Step[stepName, #, stopAfter] &;
	
	
	RightComposition[
		step["Init Project"]
		, step["Init Payload"]
		, step["Get Data"]
		, step["Calc Data"]
		, step["Build Report"]
		, step["Publish Report"]
		, step["Export Data"]
	][True];	
	
	
	(*TODO Add validatation before returning True*)
  
	True


];


(* ::Section:: *)
(* Initializaitons *)


tykFedEx$Step["Init Project"] :=  Block[
	{},
	
	tykFedEx$StartTime = Now;
	
	True

];




tykFedEx$Step["Init Payload"] := tykFedEx$InitPayload[];


(* ::Section:: *)
(* Data Sources *)



tykFedEx$Step["Get Data"] := Module[
	{}
	,
	tykFedEx$ReplicateData[];
	tykFedEx$GetOrderItemPriceData[];
	tykFedEx$GetShipmentData[];
	tykFedEx$GetShippedItemData[];
	tykFedEx$GetInternationalShipmentData[];
	tykFedEx$GetInvoiceData[];
	True
];





(* ::Section:: *)
(* Calculate *)


tykFedEx$Step["Calc Data"] :=  Block[
	{
		calc
	},

	calc[ds_Dataset] := Block[
		{
			joined = dsInnerJoin[
				  ds, 
				  tykFedEx$ShipmentData, 
				  {"Tracking Number"}
			]
		},
		joined[
			All,
			<|
				#,
			    "Country" -> #["Recipient Address Country"],
			    
			    "Box" -> tykFedEx$CalculateBox[#["Dim Length"], #["Dim Width"], #["Dim Height"]],
			    
			    "Cost Difference" -> moneyRound[#["Charge Net"] - #["Estimated Cost"]],
			    
			    "Overcharged" -> moneyRound[
			    	If[
			    		#["Charge Net"] > #["Estimated Cost"], 
			    		#["Charge Net"] - #["Estimated Cost"], 
			    		moneyUSD[0]
			    	]
			    ],
			    "Undercharged" -> moneyRound[
			    	If[
			    		#["Estimated Cost"] > #["Charge Net"], 
			    		#["Estimated Cost"] - #["Charge Net"], 
			    		moneyUSD[0]
			    	]
			    ]
    		|> &
		][
			All,
			<|
				#,
			   "Overcharged %" -> tdyPercentOf[#["Overcharged"], #["Estimated Cost"]],
			   "Undercharged %" -> tdyPercentOf[#["Undercharged"], #["Estimated Cost"]],
			   "Cost Difference %" -> tdyPercentOf[#["Cost Difference"], #["Estimated Cost"]]
			   |> &
		][
			All,
			<|
				#,
				"Over or Under" -> Catch[
					If[
						QuantityMagnitude[#["Overcharged %"]] >= 1,
						Throw["Over"]
					];
					If[
						QuantityMagnitude[#["Undercharged %"]] >= 1,
						Throw["Under"]
					];
					Throw["OK"];
				],
				"Over Threshold" -> Or[
					And[
						#["Overcharged %"] >= tykFedEx$IssuePercentThreshold,
						#["Overcharged"] >= tykFedEx$IssueDollarThreshold
					],
					And[
						#["Undercharged %"] >= tykFedEx$IssuePercentThreshold / 2.0,
						#["Undercharged"] >= tykFedEx$IssueDollarThreshold / 2.0
					]
				],
				"Recent" -> #["Shipment Date"] >= tykFedEx$MaxAsOf - tykFedEx$RecentThreshold
					
				
			|> &
		]
	];
	
	tykFedEx$DomesticData = calc[tykFedEx$InvoiceData] 
		// KeyDrop[{"Customs Number", "Customs Value", "Declared Value"}];
	
	
	
	tykFedEx$InternationalData = calc[tykFedEx$InternationalShipmentData];
	
	
	

	tykFedEx$Data = Union[
		tykFedEx$DomesticData, 
		tykFedEx$InternationalData[All, dsColumnNames[tykFedEx$DomesticData]]
	];
			
  (*TODO Add validation before returning True *)
  True
];





(* ::Section:: *)
(* Report *)



tykFedEx$Step["Build Report"] := tykFedEx$BuildReport[];

	

tykFedEx$Step["Publish Report"] := Module[
	{
	},
	
	buildTykablesNotebook[
		tykFedEx$Report,
  		"title" -> "FedEx Invoice Analysis"
  	] // NotebookPrint[tykFedEx$Report, tykFedEx$ReportFile]; 
  	
  	tykFedEx$ExportData[];
  	
  
  
  (*TODO Add validatation before returning True*)
  
  True

];



End[];

EndPackage[];

