(* Wolfram Language Init File *)



(* Wolfram Language Init File *)

<<JLink`;
InstallJava[];
ReinstallJava[JVMArguments -> "-Xmx4096m"]


Get[ "tyk`tyk`"];

Needs["ww`"];


Global`tyk$AppFolder = ParentDirectory[DirectoryName[$InputFileName]];




tyk$PackageFolder = FileNameJoin[{tyk$AppFolder, "packages"}];

tyk$PackageFolders = Select[FileNames["*", tyk$PackageFolder, Infinity], DirectoryQ];

tyk$PackageFiles = FileNames["*.m", tyk$PackageFolders];


tyk$Packages = Map[
	FileBaseName[FileNameTake[#]] <> "`"&,
	tyk$PackageFiles
] // Sort;

	
PrependTo[
	$Path, 
	FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "helpers"}]
];

Scan[
	PrependTo[$Path, #]&,
	Complement[
		tyk$PackageFolders, 
		$Path
	]
];
	
Scan[
	Needs, 
	tyk$Packages
];