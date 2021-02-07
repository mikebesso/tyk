(* Wolfram Language Package *)

BeginPackage["tykBoldUtils`", {
		"ww`",
		"tykCommon`"
	}
]

sortById::usage = "sortById  "
sortByGenderProductSizeAndBags::usage = "sortById  "
sortByGenderSizeAndBags::usage = "sortById  "
sortByGenderAndSize::usage = "sortById  "
sortBySizeAndBags::usage = "sortById  "

tykBold$EmptySlots::usage = "tykBold$EmptySlots  "

tykBold$SurrogateSizes::usage = "tykBold$SurrogateSizes  "

(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

End[] (* End Private Context *)




(* Sorting Functoids *)
	
sortById = SortBy[ {#["ID"] &}];

sortByGenderProductSizeAndBags = SortBy[
   {
    #["Gender"] &,
    #["Product Name"] &,
    lookupSizeOrder[#["Size"]] &, 
    #["Items"] &
    }
    ];
    
sortByGenderSizeAndBags = SortBy[
   {
    #["Gender"] &,
   lookupSizeOrder[#["Size"]] &, 
    #["Items"] &
    }
   ];
   
sortByGenderAndSize = SortBy[
   {
    #["Gender"] &,
    lookupSizeOrder[#["Size"]] &
    }
  ];

sortBySizeAndBags = SortBy[
   {
    lookupSizeOrder[#["Size"]] &, 
    #["Items"] &
    }
];




tykBold$EmptySlots[key_String] := With[
	{
		hasKey = "Has " <> key
	},
	<|
		"Key" -> key,
		"Data" -> Null,
		"Subscription" -> Null,
		"Counts by Gender" -> Null,
		"Counts by Size" -> Null,
		"Counts by Gender and Size" -> Null,
		"Breakout Counts" -> Null,
		"Has Key" -> hasKey, 
		"IDs" -> Null,
		"Average Duration" -> Null
	|>
];


tykBold$SurrogateSizes[key_String] := With[
	{
	},
	
	tyk$Sizes
	
	
];


EndPackage[]