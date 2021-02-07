(* Wolfram Language Init File *)



(* Wolfram Language Init File *)

<<JLink`;
InstallJava[];
ReinstallJava[JVMArguments -> "-Xmx4096m"]


Get[ "tyk`tyk`"];

Needs["ww`"];

ww$TestFolder = FileNameJoin[{
	ParentDirectory[ParentDirectory[DirectoryName[$InputFileName]]], 
	"Tests"}
	];


ww$AppFolder = ParentDirectory[DirectoryName[$InputFileName]];


With[
	{
		packageFolder = FileNameJoin[
			{
				ParentDirectory[DirectoryName[$InputFileName]], 
				"packages"
			}
		]
	},

	ww$PackageFolders = Select[FileNames["*", packageFolder, Infinity], DirectoryQ];
	ww$PackageFiles = FileNames["*.m", ww$PackageFolders];
];

ww$Packages = Map[
	FileBaseName[FileNameTake[#]] <> "`"&,
	ww$PackageFiles
] // Sort;

	
PrependTo[
	$Path, 
	FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "helpers"}]
];

Scan[
	PrependTo[$Path, #]&,
	Complement[
		ww$PackageFolders, 
		$Path
	]
];
	

ww$Bootstrap[] = Block[
	{
		needs
	},
	
	Needs["wwBootstrap`"];
	
	$DateStringFormat = {"Year", "-", "Month", "-", "Day"};
	

	
	Scan[
		Needs, 
		ww$Packages
	];
];

	
ww$Bootstrap[];



