(* ::Package:: *)

BeginPackage["OOP`"];


Unprotect["OOP`*"];
ClearAll["OOP`*"];
ClearAll["OOP`Private`*"];


coreClass::usage="generic class. use as building block for othe classes";
shelfClass::usage="OOP implementaiton for association with set and get methods, stored as class instance.it is inheritabe";
sxmClass::usage="sxm images class";
picClass::usage="image compression class";
imageTest::usage="container of funcitons for image testing";
gridClass::usage="container for spectroscopy files";
gridClassH5::usage="container for spectroscopy that links to stored h5";
gridAutoReport::usage="plug in a bunch of filenames and take a peek of what's inside";
planeSubtract::usage="subtract non-linear background";
parse3DS::usage = "eke out 3ds into WL. Funnel bias and channels into association";
parseSXM::usage = "eke out sxm into WL. Funnel channels into association";
convert3DStoH5::usage="read 3DS. Save data to HDF5 dict-style";



Begin["`Private`"];


(*replace with latest version, rules/based*)
SetAttributes[coreClass,HoldAll]; (*needed?*)
coreClass[inst_, s_]:=
Module[{h5types},
	(*init*)
	
	(*methods*)
	
	(*inst["set",op__:OptionsPattern]:=(s=Union[s, Association@op];inst);
	inst["pop",ind_]:=(KeyDropFrom[s,ind];inst);
	inst["save"] := <|"self" \[Rule] inst[], "name"-> SymbolName@Unevaluated@inst|>;*)
	
	inst[]:=s;
	(*note here we want to take a sequence and use it for query nested association
	if applicable*)
	(*inst[ind__] := s[ind];*)	
	(*UpValues - they are pretty awesome!!*)
	(*these are the new "dotted" approaches"*)
	
	inst[ind__] := If[KeyExistsQ[inst[],ind],
					   inst[][ind],
					   (
					   h5types = Import[inst[]["h5file"],"DataFormat"];
					   inst[ind]=Import[inst[]["h5file"],StringJoin["/", ind]]//If[StringQ@#, Uncompress@#, NumericArray[#,h5types[StringJoin["/", ind]]]]&;
					   
					   Echo@(StringJoin["imported ", ind]);
					   inst[][ind])
					   ];
	
	inst/:Dot[inst,"self"[]]:=(Echo@s;inst);
	inst/:Dot[inst,"get"[ind_]]:=s[ind];
	inst/:Dot[inst,"set"[op__:OptionsPattern]]:=(s=Union[s, Association@op];inst);
	
	inst . "set"["parent"-> coreClass];
	inst . "set"["h5file" -> StringJoin[NotebookDirectory[],SymbolName@Unevaluated@inst,".h5"]];
	
	(*inst/:Dot[inst,op__:OptionsPattern]:=(s=Union[s, Association@op];inst);*)
	
	inst/:Dot[inst,"pop"[ind_]]:=(KeyDropFrom[s,ind];inst);
	inst/:Dot[inst,"save"[]]:=<|"self"->inst[], "name"-> SymbolName@Unevaluated@inst|>;
	inst/:Dot[inst,"clear"[]]:=(Unprotect@inst;ClearAll[inst]);	
	inst/:Dot[inst, "saveH5"[h5name_:inst[]["h5file"]]]:= EchoTiming@Export[h5name,
		Map[If[MatchQ[Head@#, List]||MatchQ[Head@#, NumericArray],#, Compress@#]&, inst[]]//Normal, "Datasets",
		OverwriteTarget-> "Append","AppendMode"-> "Overwrite"];
	
	inst/:Head[inst]="class";
	inst/:Values[inst]:=Values@inst[];
	inst/:Keys[inst]:=Keys@inst[];
	inst/:ByteCount[inst]:=ByteCount@inst[];
	inst/:Set[inst[ind_],val_]:=inst . "set"[{ind->val}];
	(*set name of parent class. I guess coreClass is more or less embedded*)
	
	
	Protect[inst];
	
		
	
	
];


shelfClass[self_Symbol, s:(_?AssociationQ):<||>]:=Module[{props=s},
	coreClass[self,props];
	Unprotect@self;
	self . "set"["parent"->shelfClass];
	self . "set"["h5file" -> StringJoin[NotebookDirectory[],SymbolName@Unevaluated@self,".h5"]];
	Protect@self;
self];

shelfClass[self_Symbol, h5file_String]:=Module[{props=<||>},
	coreClass[self,props];
	Unprotect@self;
	self . "set"["parent"->shelfClass];
	self . "set"["h5file" -> h5file];
	Protect@self;
self];


ClearAll["picClass"];
(*SetAttributes[picClass,HoldAll];*)

picClass[self_String, im_]:=
	picClass[Symbol[self], im];
	
picClass[self_Symbol, im_]:=
	Module[{props=<|"im"->im|>},
		(
		coreClass[self,props];
		Unprotect[self];
		self . "set"["parent"-> picClass];
		
		self/:Dot[self,"pnsr"[otherim_]]:=10Log[10,Times@@ImageDimensions[self["im"]]/ImageDistance[self["im"],otherim]^2];
		self/:Dot[self,"cosineCompress"[c_]]:=
			Module[{f,cf,if},
				(
				f = FourierDCT[ImageData[im],1];
				cf = Chop[f,10^c];
				if = FourierDCT[cf,1];
				Return[<|
				"compressed"-> (if//Image),
				"compressedRatio"-> ( N@(1-Count[cf,0,{2}]/Times@@ImageDimensions@im)),
				"compressedQuality"->  ImageDistance[im,(if//Image), DistanceFunction-> NormalizedSquaredEuclideanDistance]
				|>,
				Module];
				)];

		self/:Dot[self,"compressibility"[cRange_:Range[-3,3,0.1]]]:=
			Module[{},
				Table[{#["compressedRatio"],#["compressedQuality"]}&@(self . "cosineCompress"[o]),
						{o,cRange}]];
		Protect[self];
	self
	)];


ClearAll["sxmClass"];
(*in case name of class is passed as String, we need to convert to Symbol. 
prepend obj, so we know it's a class*)

(*problem: when restoring from save, the fname is not available. So we need to build a simpler core
version of the class and then decorate it with other inputs if necessary*)

sxmClass[self_String, fname_String, s:(_?AssociationQ):<||>]:=
		sxmClass[Symbol[self], fname, s];

sxmClass[self_Symbol, fname_String, s:(_?AssociationQ):<||>]:=
		Module[{cal},
		(
		sxmClass[self,s];
		self . "set"[parseSXM[fname]];
		(self["header","scan_pixels"]/self["header","scan_range"])//
									(Normal@# 1*^-9)&//
									First//
									self . "set"[{"calibration"->#}]&;
		Return[self,Module])
		];

sxmClass[self_String, s:(_?AssociationQ):<||>]:=
	(sxmClass[Symbol[self], s];
	self)

sxmClass[self_Symbol, s:(_?AssociationQ):<||>]:=
Module[{props=s}, (
(*init*)
	coreClass[self,props];
	Unprotect@self;
	self . "set"["parent"->sxmClass];
	
	
(*read image*)
	
	self/:Dot[self,"planeSubtract"[chan_List:{"Z","forward"}]]:=
				self[StringRiffle[#,"|"]&@Join[#, {"flat"}]&@chan]=
					(self[Sequence@@chan]//Normal//planeSubtract[#]&);
	
	self/:Dot[self,"justPlot"[chan_:{"Z","forward"}, cf_:"GrayYellowTones", rescale_:{0,1.0}, op:OptionsPattern[MatrixPlot]]]:=
	  Block[{scaledData, imPlotScale},
		(
		scaledData = chan//self[Sequence@@#]&//Normal//Rescale[#,{Min@#,Max@#},rescale]&;
		
		imPlotScale =
				MatrixPlot[scaledData, Frame->False, ImageMargins->0,ColorFunctionScaling->False,
						ColorFunction -> (ColorData[cf][#]&),op]
				)];
				
			
	self/:Dot[self,"imageScalebar"[chan_:{"Z","forward"}, cf_:"GrayYellowTones", scaleColor_:White, scaleBarScale_:0.2, rescale_:{0,1.0}, modifier_:(#&)]]:=
	
	  Block[{scaledData,scaleBarValue, scaleBar, imPlotScale, min, max},
		(
		scaledData = chan//self[Sequence@@#]&//Normal//Rescale[#,{Min@#,Max@#},rescale]&;
		
		{min,max}= {Min@#,Max@#}&@Flatten@Flatten@scaledData;
		scaleBarValue = (scaleBarScale*First@Dimensions@scaledData/self["calibration"])//NumberForm[#,2]&;
		imPlotScale =
			Graphics[
				{
				Text[Style[ToString@scaleBarValue <> " nm",scaleColor,25],Scaled[{0.15,.1}]],
				{scaleColor,AbsoluteThickness[5],Line[{ImageScaled[{0.05,.05}],ImageScaled[{0.05 + scaleBarScale,0.05}]}]}
				},
				PlotRange->Transpose@{{0,0},Dimensions@scaledData},			
				Prolog ->
				{
					Inset[
						MatrixPlot[scaledData, Frame->False, ImageMargins->0,ColorFunctionScaling->False,
						ColorFunction -> (ColorData[cf][#]&)],
					Center,Center,Dimensions@scaledData]
				}];
				
			GraphicsRow[{imPlotScale,BarLegend[cf]}, Scaled[0.0]]		
		)];
	Protect@self;)];


ClearAll["gridClass"];
gridClass[inst_Symbol, s:(_?AssociationQ):<||>] :=
	Module[{props=s}, (
		(*init*)
		coreClass[inst,props];
		Unprotect@inst;
		(*=====================\[Equal]*)
		inst . "set"["parent"->gridClass];
		
		(*==========ndiv=================*)
		inst/:Dot[inst,"ndiv"[chan_:"Current (A)",bias_:"sweep_signal"]]:=
			Block[{dx, ndiv},
			dx = inst[bias]//Normal//DerivativeFilter[#,{1},1]&;
			ndiv = inst[chan]//
					Normal//
					Flatten[#,1]&//
					Trankvility`denoise[3][#]&//
					Normal//
					DerivativeFilter[#,{0,1},1]&//
					Map[#/dx&,#]&//
					ArrayReshape[#,Dimensions@inst[chan]]&//
					NumericArray;
					
			inst . "set"[chan <> " ndiv" -> ndiv];				
				];
				
		(*======\[Equal]cubePlot=============*)	
		inst/:Dot[inst,"cubePlot"[chan_:"Current (A)", op_:{PlotTheme->"CMYKColors", 
								ColorFunctionScaling->True, 
								Axes->{True,True,True}, 
								OpacityFunctionScaling->True}]]:=
			Block[{},
			inst[chan]//
			RightComposition[
				Normal,
				Rescale,
				ListDensityPlot3D[#,FilterRules[op,Options[ListDensityPlot3D]]]&
				]@#&
				];
		
		(*=======hist2D================*)
		inst/:Dot[inst,"hist2D"[chan_:"Current (A)", bins_:50, bias_:"sweep_signal", 
									op_:{ColorFunction->"LakeColors"}]]:=
			Block[{b=Normal@inst[bias]},		
			inst[chan]//
			RightComposition[
				Normal,
				Flatten[#,1]&,
				 Transpose[{ConstantArray[ b,Length@#],#}]&,
				Transpose@#&/@#& ,
				Flatten[#,1]&,
				DensityHistogram[#, bins, FilterRules[op,Options[DensityHistogram]]]&
				]@#&
				];
		(*=======quickReport==========*)
		
		inst/:Dot[inst, "peek"[chans_:{"Current (A)"}]]:=
			Module[{},
				(
				Echo@#&/@{
					inst["header"]//Normal//TableForm,
					TableForm[{#,inst . "cubePlot"[#],
					inst . "hist2D"[#,150]}&/@chans]
					};
				)];
		
		(*=====================\[Equal]*)
		Protect@inst;	
		)];	
		
		
gridClass[inst_String, s:(_?AssociationQ):<||>] :=
	gridClass[Symbol[inst], s];

gridClass[inst_Symbol, fname_String] :=
	Module[{canread},
		(
		
		If[
		FailureQ[canread = Quiet[parse3DS[fname]]],
		Print["file not readable"],
		gridClass[inst, canread]];
		inst . {"fileName"->fname};
		)];

gridClass[inst_String, fname_String] :=
	(
	gridClass[Symbol[inst], fname];
	)


gridClassH5[inst_Symbol, s:(_?AssociationQ):<||>] :=
	Module[{props=s}, (
		(*init*)
		coreClass[inst,props];
		Unprotect@inst;
		(*=====================\[Equal]*)
		inst . "set"["parent"->gridClassH5];
		(*inst[ind__] := If[KeyExistsQ[inst[],ind],
					   inst[][ind],
					   (inst[ind]=Import[inst[]["h5file"],StringJoin["/", ind]]//NumericArray[#,inst[]["h5types"][StringJoin["/",ind]]]&;Echo@StringJoin["imported ", ind];inst[][ind])];*)
        
	(*=======overloading default keys========\[Equal]*)
	inst/:Keys[inst]:=<|
						"in-memory"-> Keys@inst[],
						"in-h5"-> (Import[inst[]["h5file"]]//StringDelete[#,"/"]&/@#&)
						|>;

		(*==========ndiv=================*)
		inst/:Dot[inst,"ndiv"[chan_:"Current (A)",bias_:"sweep_signal"]]:=
			Block[{dx, ndiv},
			dx = inst[bias]//Normal//DerivativeFilter[#,{1},1]&;
			ndiv = inst[chan]//
					Normal//
					Flatten[#,1]&//
					Trankvility`denoise[3][#]&//
		(Echo@"denoised";#)&//
					Normal//
					DerivativeFilter[#,{0,1},1]&//
					Map[#/dx&,#]&//
					ArrayReshape[#,Dimensions@inst[chan]]&//
					NumericArray;
					
			inst . "set"[chan <> " ndiv" -> ndiv];				
				];
				
		(*======\[Equal]cubePlot=============*)	
		inst/:Dot[inst,"cubePlot"[chan_:"Current (A)", op_:{PlotTheme->"CMYKColors", 
								ColorFunctionScaling->True, 
								Axes->{True,True,True}, 
								OpacityFunctionScaling->True}]]:=
			Block[{},
			inst[chan]//
			RightComposition[
				Normal,
				Rescale,
				ListDensityPlot3D[#,FilterRules[op,Options[ListDensityPlot3D]]]&
				]@#&
				];
		
		(*=======hist2D================*)
		inst/:Dot[inst,"hist2D"[chan_:"Current (A)", bins_:50, bias_:"sweep_signal", 
									op_:{ColorFunction->"LakeColors"}]]:=
			Block[{b=Normal@inst[bias]},		
			inst[chan]//
			RightComposition[
				Normal,
				Flatten[#,1]&,
				 Transpose[{ConstantArray[ b,Length@#],#}]&,
				Transpose@#&/@#& ,
				Flatten[#,1]&,
				DensityHistogram[#, bins, FilterRules[op,Options[DensityHistogram]]]&
				]@#&
				];
		(*=======quickReport==========*)
		
		inst/:Dot[inst, "peek"[chans_:{"Current (A)"}]]:=
			Module[{},
				(
				Echo@#&/@{
					inst["header"]//Normal//TableForm,
					TableForm[{#,inst . "cubePlot"[#],
					inst . "hist2D"[#,150]}&/@chans]
					};
				)];
		
		(*=====================\[Equal]*)

		(*======saveToH5==========*)
		(*inst/:Dot[inst, "saveH5"[]]:= EchoTiming@Export[inst[]["h5file"], inst[]//KeyDrop[#,{"parent","h5types"}]&//Normal, "Datasets", OverwriteTarget-> "Append","AppendMode"-> "Overwrite"];*)
		
		(*======overload save function===\[Equal]*)
		inst/:Dot[inst,"save"[]]:=
							(
							inst . "saveH5"[]; 
							<|"self"->(inst[]//KeyTake[#,{"h5file", "parent"}]&), "name"-> SymbolName@Unevaluated@inst|>
							);
		
		Protect@inst;	
		
		)];	

gridClassH5[inst_String, s:(_?AssociationQ):<||>] :=
	gridClassH5[Symbol[inst], s];

gridClassH5[inst_Symbol, fname_?(StringEndsQ[#,".h5"]&)] :=
	Module[{canread},
		(
		gridClassH5[inst, <||>];
		inst["h5file"]= fname;
	inst["h5types"]=Import[fname,"DataFormat"];
	inst 
		)];

gridClassH5[inst_Symbol, fname_?(StringEndsQ[#,".3ds"]&)] :=
	Module[{canread},
		(
		If[
		FailureQ[canread = Quiet[convert3DStoH5[fname]]],
		Print["file not readable"],
		gridClassH5[inst, <||>]];
	inst["h5file"]= fname<>".h5";
	inst["h5types"]=Import[fname<>".h5","DataFormat"];
	inst
		)];


(* ::Section:: *)
(*reporting functionality*)


ClearAll["gridAutoReport"];
gridAutoReport[gr_, fname_String,chans_:{"Current (A)"}]:=
	Module[{},
		CellPrint[TextCell[fname,"Text",CellFrame->True, CellFrameColor->Orange]];
		gridClass[gr,fname];
		If[MatchQ[Head[gr],"class"],
		gr . "peek"[chans]];
];


(* ::Section:: *)
(*image func*)


imageTest/:Dot[imageTest[im_],"psnr"[otherim_]]:=
	10Log[10,Times@@ImageDimensions[im]/ImageDistance[im,otherim]^2];

imageTest/:Dot[imageTest[im_],"cosineCompress"[c_]]:=
	Module[{f,cf,if},
		(
		f = FourierDCT[ImageData[im],1];
		cf = Chop[f,10^c];
		if = FourierDCT[cf,1];
		<|
	"compressed"-> (if//Image),"compressedRatio"-> ( N@(1-Count[cf,0,{2}]/Times@@ImageDimensions@im)),
	"compressedQuality"->  ImageDistance[im,(if//Image), DistanceFunction-> NormalizedSquaredEuclideanDistance]
	|>
			)];

imageTest/:Dot[imageTest[im_],"compressibility"[cRange_:Range[-3,3,0.1]]]:=
	Table[{#["compressedRatio"],#["compressedQuality"]}&@picFuncs[im]["cosineCompress",o],{o,cRange}];



(*bgSubtract[zArr_, model_:(NonlinearModelFit[#,a2 x^2 + a1 x + b2 y^2+ b1 y + c1,{a2,b2,a1, b1, c1},{x,y}]&)]:=
Block[{zArrIJ, indArray,subIndArray,subZArray,bgFit,bg,xx,yy,a2,b2,a1,b1,c1,x,y},
(
	indArray =zArr//MapIndexed[Last,#,{2}]&//Flatten[#,1]&;
	
	subIndArray = indArray//
			Take[#,{1,-1,IntegerPart@(Length@indArray/300)}]&
	
	subZArray = zArr//Flatten//Take[#,{1,-1,IntegerPart@(Length@#/300)}]&;
		
	zArrIJ = Transpose[{subIndArray,subZArray}]//
					Flatten@#&/@#&;
			
	bgFit = zArrIJ//model;
	zArr - ParallelMap[bgFit[Sequence@@#]&,indArray,{2}]
)];*)


(*bgSubtract[zArr_, model_:(NonlinearModelFit[#,a2 x^2 + a1 x + b2 y^2+ b1 y + c1,{a2,b2,a1, b1, c1},{x,y}]&)]:=
Block[{zArrIJ, indArray,subIndArray,subZArray,bgFit,bg,xx,yy,a2,b2,a1,b1,c1,x,y},
(
	subIndArray =largeFeSe["Z","forward"]//RightComposition[
		Normal,
	    Dimensions@#&,
		Subdivide[1,#,50]&/@#&,
		IntegerPart@#&/@#&/@#&,
	 	meshgrid@@Sequence@#&]@#&;
	
	indArray =zArr//Normal//Dimensions//Range@#&/@#&//meshgrid@@Sequence@#&;
	
	subZArray = zArr//Flatten//Take[#,{1,-1,IntegerPart@(Length@#/300)}]&;
		
	zArrIJ = subIndArray//RightComposition[
		{#,Part[zArr,Sequence@@#]&/@#}&,
		Transpose,
		Flatten/@#&]@#&
	bgFit = zArrIJ//model;
	zArr - ParallelMap[bgFit[Sequence@@#]&,indArray,{2}]
)];*)


planeSubtract[zArr_]:=
Block[{zloc=Normal@zArr, planeDir, zIJ},
	(
	zIJ = zloc//
			{Trankvility`meshgrid[Range@First@Dimensions@#,Range@Last@Dimensions@#],
			Flatten@#}&//
			Transpose//
			Flatten/@#&;
			
	planeDir=zIJ//
		RandomSample[#,1000]&//
		LeastSquares[#,ConstantArray[1,Length[#]]]&;
		
	zIJ//
	Map[planeDir . #&,#]&//
	ArrayReshape[#,Dimensions@zloc]&
	)
	]


(* ::Subsection:: *)
(*dataXray connectivity*)


parse3DS[file3ds_]:=
	Module[{py},
	py=StartExternalSession["Python"];
	Return[parse3DS[file3ds,py],Module];
	DeleteObject[py];
	];
	
parse3DS[file3ds_, py_] := Module[{xasc,gr2,coords,dataVars,attrs},(
  
    ExternalEvaluate[py,"import data_xray.nanonisio as nio; import numpy as np"];
	ExternalEvaluate[py,StringJoin["gr2 = nio.Grid('",file3ds,"')"]];
	xasc = ExternalEvaluate[py,"gr2.signals"];
	xasc["header"] = ExternalEvaluate[py,"gr2.header"];
	xasc
	)]	


convert3DStoH5[file3ds_]:=
	Module[{py},
	py=StartExternalSession["Python"];
	Return[parse3DS2[file3ds,py],Module];
	DeleteObject[py];
	];
convert3DStoH5[file3ds_, py_] := 
Module[
{xasc,gr2,coords,dataVars,attrs},
(

ExternalEvaluate[py,
"
import data_xray.nanonisio as nio
import numpy as np
import h5py
"];
ExternalEvaluate[py,StringJoin["gr2 = nio.Grid('",file3ds,"')"]];
ExternalEvaluate[py,StringJoin["srcfile = ",file3ds]];
ExternalEvaluate[py, StringJoin["hf_object = h5py.File('",file3ds,".h5', 'w')"]];
ExternalEvaluate[py,
"
for key, val in gr2.signals.items():
	print(key)
	hf_object.create_dataset(key, data=val)
#hf_object.create_dataset('header', data=gr2.header);
hf_object.close()
"];
	)
]	


parseSXM[filesxm_]:=
	Module[{py},(
	py=StartExternalSession["Python"];
	Return[parseSXM[filesxm,py],Module];
	DeleteObject[py];
	)];

parseSXM[filesxm_, py_] := Module[{xasc,gr2,coords,dataVars,attrs},
	ExternalEvaluate[py, "import data_xray.nanonisio as nio; import numpy as np"];
	ExternalEvaluate[py, StringJoin["gr2 = nio.Scan('",filesxm,"')"]];
	xasc = ExternalEvaluate[py,"gr2.signals"];
	xasc["header"] = ExternalEvaluate[py,"gr2.header"];
	xasc
	]


ClearAll["parseSXM2"];
parseSXM2[fpath_]:=
Module[{workingStream, headerEndPosition, data, header,
	pixNx,pixNy, pixRx, pixRy, chans, parsed, fwdFunc,bwdFunc, isUp},
(
workingStream = OpenRead[fpath, BinaryFormat -> True];
       
headerEndPosition = workingStream//
Find[#, ":SCANIT_END:"]&//
StreamPosition[workingStream]&;

data = workingStream//
SetStreamPosition[#, headerEndPosition +  5]&//
 BinaryReadList[workingStream, "Real32", ByteOrdering -> 1]&;

header = workingStream//
SetStreamPosition[#, 0]&//
StringJoin[ReadList[workingStream, "Character", headerEndPosition]]&;

workingStream//Close;

(*one-shot header parse*)
header =header//
StringSplit//
SplitBy[#,StringMatchQ[#,":"~~___~~":"]&]&//
#[[;;-2]]&//
Partition[#,2]&//
StringDelete[#,":"]&@First@#[[1]]->Flatten@#[[2]]&/@#&//
Association;

header//
(
{pixNx, pixNy} = ToExpression@#["SCAN_PIXELS"];
{pixRx, pixRy} = #["SCAN_RANGE"]//StringReplace[#,"E":>"*^"]&/@#&//ToExpression;
isUp = StringMatchQ[#["SCAN_DIR"], "up"]//First;
chans = #["DATA_INFO"]//StringSplit//Partition[#,6]&//#[[2]]&/@#&//#[[2;;]]&//
StringJoin@#&/@#&;
)&;

data= data//
   Partition[#, 
{pixNx*pixNy*Quotient[FromDigits@Dimensions@#, pixNx*pixNy*Length@chans]}
]&;

parsed =
                {"pixels" -> {pixNx, pixNy}, "range" -> {pixRx, pixRy},"channels"->chans, "data"-><||>}//
Association;

fwdFunc[isUp_]:=If[isUp,Reverse@#,#]&;
bwdFunc[isUp_]:=If[isUp,Reverse[#,2]&@Reverse@#,#]&;

parsed["data"] =MapThread[
#1->(#2//
Partition[#,{pixNx*pixNy}]&//
ArrayReshape[#,{pixNy,pixNx}]&/@#& //
{fwdFunc[isUp]@#[[1]],bwdFunc[isUp]@#[[2]]}&)&,
{chans, data}]//
Association;

parsed
)
];


ClearAll["parse3DS2"];
parse3DS2[fpath_]:=
Module[{workingStream, headerEndPosition, data, header,
	pixNx,pixNy, pixRx, pixRy, chans, parsed, fwdFunc,bwdFunc, isUp},
(
workingStream = OpenRead[fpath, BinaryFormat -> True];
       
headerEndPosition = workingStream//
Find[#,  ":HEADER_END:"]&//
StreamPosition[workingStream]&;

data = workingStream//
SetStreamPosition[#, headerEndPosition + 2]&//
 BinaryReadList[workingStream, "Real32", ByteOrdering -> 1]&;

header = workingStream//
SetStreamPosition[#, 0]&//
StringJoin[ReadList[workingStream, "Character", headerEndPosition]]&;

workingStream//Close;

(*one-shot header parse*)
header =header//
StringSplit[#,"\n"]&//
StringSplit[#,","]&//
StringTrim@#&/@#&//Map[First@#->Last@#&,#]&@StringSplit[#,"="]&/@#&//
Association;

(*need to continue past this point. How to slice the datacube*)
header
)
];


Scan[SetAttributes[#, {Protected, ReadProtected}]&,
     Select[Symbol /@ Names["OOP`*"], Head[#] === Symbol &]];


End[];


Protect["OOP`*"];


EndPackage[]
