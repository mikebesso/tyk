(* Wolfram Language Package *)

BeginPackage["tykVelocityUtils`", { 
	"tykCommon`",
	"wwBootstrap`",
	"wwStrings`",
	"wwPatterns`"
	
}]


(* Exported symbols added here with SymbolName::usage *)  


sortSnappies::usage = "";
sortDenim::usage = "";
sortDiapers::usage = "";
sortUnderwear::usage = "";
firstNonOther::usage = "";
highlight::usage = "";
(*includeSnappies::usage = "";*)
(*excludeSubscriptions::usage = "";*)

Begin["`Private`"] (* Begin Private Context *) 


highlight[daysLeft_String, text_] := text;
highlight[daysLeft_?NumericQ, text_] := With[
	{
		background = White,
		color = Which[
			daysLeft > 30, Black,
			daysLeft > 0, Red,
			True, Darker[Red]
		],
		weight = Which[
			daysLeft > 30, Plain,
			daysLeft > 0, Plain,
			True, Bold
    	],
    	slant = Which[
    		daysLeft > 0, Plain,
    		True, Italic
    	]
	},
	Item[text, Background -> background, BaseStyle -> {color, FontWeight -> weight, FontSlant -> slant}]
];



firstNonOther[{value_String}] := value;
firstNonOther[values_?matchListOfStringsQ] := With[
    {
     firstValue = tdyFirst[values]
     },
    If[
     ! MemberQ[{"Other", "Snappies"}, firstValue], 
     firstValue, 
     firstNonOther[Rest[values]]
     ]
    ];
firstNonOther[___] = "Error";


sortUnderwear = 
  SortBy[{ #["Color"] &, #["Gender"] &, #["Size Order"] &, #[
      "SKU"] &}];
      
sortDiapers = SortBy[{#["Pattern"] &, #["Size Order"] &, #["SKU"] &}];      

sortDenim = 
  SortBy[{ #["Pattern"] &, #["Type"] &, #["Size Order"] &, #[
      "SKU"] &}];
      
sortSnappies = SortBy[{ #["Pattern"] &, #["Type"] &, #["Size Order"] &, #[
     "SKU"] &}]; 
     
(*includeSnappies = #["Product Style"] == "Snappies" &;*)

(*excludeSubscriptions[] := Select[
			And[
				! StringStartsQ[#["Product"], "Subscript"],
				! StringStartsQ[#["Pattern"], "Subscript"],
				! StringStartsQ[#["Color"], "Subscript"]
			] &
		];*)
     
                 


End[] (* End Private Context *)

EndPackage[]