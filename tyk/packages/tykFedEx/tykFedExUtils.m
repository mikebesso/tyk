(* Wolfram Language Package *)

BeginPackage[
	"tykFedExUtils`",
	{		
		"ww`",
		"wwBootstrap`",
		"wwDates`",
		"wwStrings`"
	}
]



tykFedEx$CalculateBox::usage = "tykFedEx$CalculateBox  "
(* Exported symbols added here with SymbolName::usage *)  


tykFedEx$SelectRecent::usage = "selectRecent  "

tykFedEx$SelectByZone::usage = "selectByZone  "

tykFed$SelectBiggestIssues::usage = "selectBiggestIssues  "
tykFed$SelectOveragesOnOrAfter::usage = "tykFed$SelectOveragesOnOrAfter  "

tykFed$SelectMismatchedBoxVsEstimate::usage = "tykFed$SelectMismatchedBoxVsEstimate  "

tykFedEx$Step::usage = "tykFedEx$Step  "

tykFedEx$SelectZones::usage = "tykFedEx$SelectZones  "



Begin["`Private`"] (* Begin Private Context *) 






tykFedEx$CalculateBox[length_?NumericQ, width_?NumericQ, height_?NumericQ] := With[
   {
   		dimA = Max[height, length, width],
   		dimB = Catch[
			If[
				height >= width && width >= length, 
				Throw[width]
			];
			If[
				width >= height && height >= length, 
				Throw[height]
			];
			If[
				height <= width && width <= length, 
				Throw[width]
			];
			If[
				width <= height && height <= length, 
					Throw[height]
			];
			Throw[length]
		],
   		dimC = Min[height, length, width],
   		dimSum = length + width + height
    },
    
    With[
    	{
			boxName = Catch[
				If[(dimA * dimB * dimC) == 0, Throw[ "Z - Flat"]];
				If[(dimA == 12) && (dimB == 12), Throw["A - Poly Bag"]];
				If[dimSum <= 24, Throw["A - Poly Bag"]];
				If[dimSum <= 31, Throw["B - Small Box"]];
				If[dimSum <= 36, Throw["C - Single Bag"]];
				If[dimSum <= 42, Throw["D - Double Bag"]];
				If[dimSum <= 49, Throw["E - Triple Bag"]];
				If[dimSum <= 55, Throw["F - Four Bag"]];
		
				Throw["G - Eight Bag"];
		    ]
    	},

    
	    StringJoin[		      
			StringPadLeft[str$Tidy[Round[dimA]], 2, "0"], "x",
			StringPadLeft[str$Tidy[Round[dimB]], 2, "0"], "x",
			StringPadLeft[str$Tidy[Round[dimC]], 2, "0"],
			" (",
			boxName,
			")"
		]
    ]
	   
];

tykFedEx$CalculateBox[___] = "Missing Dimensions";

zoneQ = MemberQ[{"2", "3", "4", "5", "6", 2, 3, 4, 5, 6}, #] &;



tykFedEx$SelectRecent[$keyDate_String, $asOfDate_DateObject, $daysAgo_Integer : 60] := 
  Select[#[$keyDate] >= DatePlus[$asOfDate, -$daysAgo] &];
  
  
tykFedEx$SelectByZone[$zone_String] := Select[#["Zone"] == $zone &];


tykFed$SelectBiggestIssues[] := Select[
	And[
		zoneQ[#["Zone"]],
		#["Over or Under"] == "Over",
		True
	] &
]; 
   
tykFed$SelectOveragesOnOrAfter[date_DateObject] := Select[
	And[
		DateObject[#["Shipment Date"], "Day"] >= DateObject[date, "Day"],
		#["Cost Difference"] > moneyUSD[0]
	] &
];    

tykFed$SelectMismatchedBoxVsEstimate[] := Select[
	And[
		#["Estimated Cost"] == moneyUSD[8.2],
		! StringStartsQ[#["Box"], "12x15x01"],
		#["Cost Difference"] > moneyUSD[0]
	] &
];






End[] (* End Private Context *)

EndPackage[]