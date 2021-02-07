(* Wolfram Language Package *)

BeginPackage[
	"tykCompetitorsParse`",
	{
		"tykCommon`", 
		"tykCompetitors`",
		"ww`", 
		"wwBootstrap`",
		"wwAssociations`",
		"wwFileSystem`", 
		"wwExcel`", 
		"wwStrings`", 
		"wwDatasets`", 
		"wwLists`"
	}	
]

tykCompetitors$Parse::usage = "tykCompetitors$Parse  "


(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

parseFileName[file_Association] := Module[
   {
    filePathParts = file["filePathParts"]
    },
   Append[
    KeyDrop[file, {"filePathParts"}],
    <|
     "Page" -> StringDrop[StringDrop[filePathParts[[-1]], 3], -5],
     "Region" -> StringTake[filePathParts[[-1]], 2]
     |>
    ]
   
   ];
   
getScrapedFiles[config_Association] := Module[
   {
    files,
    htmlFolderPath = tykCompetitors$HtmlFolder,
    test = config["TestMode"],
    company = config["Company"],
    
    parseFileName
    },
   
   parseFileName[filePath_String] := Module[
     {
      filePathParts = FileNameSplit[filePath]
      },
     <|
      "Date" -> filePathParts[[-2]],
      "Company" -> company,
      "filePath" -> filePath,
      "fileSize" -> FileByteCount[FileNameJoin[filePathParts]],
      "filePathParts"  -> filePathParts
      |>
     
     ];
   
   
   files = FileNames[ "*.html", htmlFolderPath, Infinity];
   
   files = Select[
     Map[
      parseFileName,
      FileNames[ "*.html", htmlFolderPath, Infinity]
      ],
     #fileSize > 40000 &
     ];
   
   Dataset[
    If[
     test,
     files = RandomSample[files, Min[10, Length[files]]],
     files
     ]
    ]
   ];   


 parseAttribute[a_Association, "attribute_pa_size"] := ToUpperCase@StringReplace[
 	Lookup[a, "attribute_pa_size", "ONE-SIZE"],
	{
		"XXL" -> "2XL",
		"XXXL" -> "3XL",
		"XXXXL" -> "4XL"
	}
];
			
parseAttribute[a_Association, attribute_String, default_String : ""] := Lookup[a, attribute, default];


pageFromFileName[scrapeFile_String] := StringRiffle[
	StringSplit[FileBaseName[scrapeFile], "-"][[2 ;;]],
	"-"
];

countryFromFileName[scrapeFile_String] := tdyFirst @ StringSplit[FileBaseName[scrapeFile], "-"] 


parseScrape[scrapeFile_String] := Module[
	{
		text = Import[scrapeFile, "Text"],
		page = pageFromFileName[scrapeFile],
		(*
		country = countryFromFileName[scrapeFile],
		*)
		form,
		formAttributes,
		attributes,
		price,
		metadata
	},
   
   
   (*Echo[scrapeFile -> page, "Parse"];*)
   
	metadata = productMetadata[
		Select[
			Or[
				#["Page"] == page,
				#["Page"] == StringJoin["abu-", page]
			] &
		], 
		All
	] // Normal // tdyFirst;
   
	form = StringCases[
		text, 
		"<form" ~~ WordBoundary ~~ ___  ~~ "</form>" 
	]  // tdyFirst;
   
	With[
		{
			xml = ImportString[form, {"HTML", "XMLObject"}]
		}
		,
   
		formAttributes = With[
			{
				xmlForm = Association[
					Cases[xml, XMLElement["form", l___] :> l, Infinity] // First
				]
			}
			,
			Lookup[xmlForm, "data-product_variations", "[]"]
      	];
    
    
    
		price = Block[
			{
				formLabels = Cases[
					xml,
					XMLElement[
						"label", l___] :> l, 
						Infinity
				] // Flatten // Select[MatchQ[#, _String] &],
       			label,
       			parsePriceLabel
       		}
       		,
      
			parsePriceLabel[{priceLabel_String}] := With[
				{ 
					countAndPrice = StringCases[priceLabel, NumberString ]
				}
				,
				Round[
					ToExpression[countAndPrice[[2]]] / 
					ToExpression[countAndPrice[[1]]], 0.01]
				];
      
				Catch[
					label = Select[formLabels, StringStartsQ[#, "4 Packs"] &]; 
					If[
						Length[label] == 1, 
						Throw[parsePriceLabel[label]], 
        				Nothing
        			];
       
					label = Select[formLabels, StringStartsQ[#, "3 Packs"] &]; 
					If[
						Length[label] == 1, 
						Throw[parsePriceLabel[label]], 
        				Nothing
        			];
       
					label = Select[formLabels, StringStartsQ[#, "1 Pack"] &]; 
					If[
						Length[label] == 1, 
						Throw[parsePriceLabel[label]], 
						Nothing
					];
       
				Throw[0];
			]
		];
	];
   
   
   
   attributes = Module[
		{
			parseMaxQuantity,
			decode
		}
		,
		parseMaxQuantity[""] = 0;
		parseMaxQuantity[quantity_Integer] := quantity;
		parseMaxQuantity[quantity_String] := ToExpression[quantity];
     
     
		decode[a_Association] := Module[
			{
				isSample = parseAttribute[a["attributes"], "attribute_pa_product-type"] == "sample",
        		isScented = StringStartsQ[parseAttribute[a["attributes"], "attribute_pa_scent"], "scent"],
				size = parseAttribute[a["attributes"], "attribute_pa_size"],
				length = ToUpperCase[parseAttribute[a["attributes"], "attribute_pa_length"]],
				color = parseAttribute[a["attributes"], "attribute_pa_color"],
				style = parseAttribute[a["attributes"], "attribute_pa_style"],
				pattern = parseAttribute[a["attributes"], "attribute_pa_pattern"],
				capDesign = parseAttribute[a["attributes"], "attribute_pa_cap-design"],
				hatDesign = parseAttribute[a["attributes"], "attribute_pa_hat-design"],
				onHand = parseMaxQuantity[a["max_qty"]],
				sku
			}
			,
       	
       
			If[isSample || isScented, Return[Nothing]];
       
			If[StringContainsQ[ hatDesign, "Pack", IgnoreCase -> True], Return[Nothing]];
       
			If[StringContainsQ[ capDesign, "Pack", IgnoreCase -> True], Return[Nothing]];
       
			With[
				{
					metaPattern = Lookup[metadata, "Pattern", ""],
					metaColor = Lookup[metadata, "Color", ""]
				}
				,
        
				color = tykCleanColor[str$Coalesce[{metaColor, color}]];
				style = tykCleanPattern[
          			str$Coalesce[{metaPattern, style, pattern, capDesign, hatDesign, color}]
          		];
        		pattern = tykCleanPattern[str$Coalesce[{metaPattern, pattern, style}]];
			];
       
			size = If[length == "TALL", size ~~ "-TALL", size];
       
       
			sku = StringRiffle[
				{
					page, 
					style, 
					size
				}, 
				"-"
            ] // ToLowerCase // StringReplace[{"-" .. -> "-"}] // StringTrim;
       

			<|
				"SKU" -> sku,
				"Size" -> size,
				"Color" -> color,
				"Style" -> style,
				"Pattern" -> pattern,
				"On Hand" -> onHand,
				"Price" -> price
			|>
		];
     
		Map[
      		decode,
			Map[
				keyDropUnusedAttributes,
       			Map[
					ToAssociations, 
					ImportString[formAttributes, "JSON"]
				]
			] 
		]
	]
   
   
   
];


keyDropUnusedAttributes = KeyDrop[
	{
						"image", "dimensions", "backorders_allowed", 
						"dimensions_html" , "availability_html", "sku", "weight", 
						"weight_html", "is_in_stock", "is_purchasable", 
						"is_sold_individually", "is_virtual", "price_html", 
						"image_id", "is_downloadable", "variation_is_visible", 
						"variation_id", "variation_description", 
						"variation_is_active", "min_qty"
	}
];
					


parseScrapes[assoc_Association] := Module[ 
   {
    buildRow
    },
   
   buildRow[scrape_Association, company_, country_, page_, pattern_, 
     date_] := With[
     {
      (*sku = StringRiffle[{page, scrape["SKU"]}, "-"] // 
      StringReplace[ "--" \[Rule] "-"]*)
      },
     
     <|
      
      "Key" -> StringRiffle[{company, country, scrape["SKU"]}, "-"],
      "SKU" ->  scrape["SKU"],
      "Date" -> date,
      "Company" -> company,
      "Country" -> country,
      "Page" -> page,
      "Style" -> str$Coalesce[{pattern, scrape["Style"], page}],
      "Pattern" -> str$Coalesce[{pattern, scrape["Pattern"], page}],
      "Color" -> scrape["Color"],
      "Size" -> scrape["Size"],
      "On Hand" -> scrape["On Hand"],
      "Price" -> scrape["Price"],
      "Status" -> "Good"
      |>
     ];
   
   
	rawData = Map[
		buildRow[#,  assoc[["Company"]], assoc[["Region"]], assoc[["Page"]], assoc[["Pattern"]], assoc[["Date"]]] &,
		parseScrape[assoc[["filePath"]]]
	];
   
   
   
   
   rawData
   
   ];
   
   
config = Null;
productMetadata = Null;


Options[tykCompetitors$Parse] = tykCompetitors$Options;
 
tykCompetitors$Parse[OptionsPattern[]] := Module[
	{
		testMode = OptionValue["TestMode"],
		
		company,
		rawData,
		files
	}
	,
	
	config = tykCompetitors$GetConfiguration["abu", "TestMode" -> testMode];
	productMetadata = config["Product Metadata"];
	company = config["Company"];
	
	
	files = dsInnerJoin[
		productMetadata[All, {"Page", "Pattern"}],
		Map[
			parseFileName,
			getScrapedFiles[config]
		],
		"Page"
	];
	
	If[
		testMode, 
		files = listTdyTake[files, 3]
	];
	
	rawData = Dataset[
		Flatten[
			ParallelMap[
				parseScrapes,
				files // Normal 
			] 
		]
	];


	Module[
		{
			dates = Union[dsColumnValues[rawData, "Date"]],
			export
		},
  
  
		export[ds_Dataset, date_String] := Module[
			{
				folder = createFolder[
					FileNameJoin[{tykCompetitors$ParsedFolder,  company}]
				],
				filename = company ~~ "-" ~~ date ~~ ".xlsx"
			},
    
	    	(*Echo[FileNameJoin[{folder, filename}]];*)
	    	exportExcel[
	    		FileNameJoin[{folder, filename}], 
	     		ds[Select[#["Date"] == date &], All] 
	     	];
    	];
  
		Scan[
			export[rawData, #] &,
			dates
		];
  
  	];
];

tykCompetitors$Parse[args___] := msgDefinitionNotFound["tykCompetitors$Parse", args];

End[] (* End Private Context *)

EndPackage[]