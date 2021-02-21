(* Wolfram Language Test file *)

Needs["tyk"];






Test[
	Head[rawInventory = tykRawInventory[]]
	,
	Dataset
	,
	TestID -> "rawInventory - 1"
]