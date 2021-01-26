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
parse3DS::usage = "eke out 3ds into WL. Funnel bias and channels into association";
parseSXM::usage = "eke out sxm into WL. Funnel channels into association";


Begin["`Private`"];


(*replace with latest version, rules/based*)
SetAttributes[coreClass,HoldAll]; (*needed?*)
coreClass[inst_, s_]:=
Module[{},
	(*init*)
	
	(*methods*)
	
	(*inst["set",op__:OptionsPattern]:=(s=Union[s, Association@op];inst);
	inst["pop",ind_]:=(KeyDropFrom[s,ind];inst);
	inst["save"] := <|"self" \[Rule] inst[], "name"-> SymbolName@Unevaluated@inst|>;*)
	
	inst[]:=s;
	(*note here we want to take a sequence and use it for query nested association
	if applicable*)
	inst[ind__] := s[ind];
	(*UpValues - they are pretty awesome!!*)
	(*these are the new "dotted" approaches"*)

	inst/:Dot[inst,"self"]:=(Echo@s;inst);
	inst/:Dot[inst,"get"[ind_]]:=s[ind];
	inst/:Dot[inst,"set"[op__:OptionsPattern]]:=(s=Union[s, Association@op];inst);
	inst/:Dot[inst,op__:OptionsPattern]:=(s=Union[s, Association@op];inst);
	inst/:Dot[inst,"pop"[ind_]]:=(KeyDropFrom[s,ind];inst);
	inst/:Dot[inst,"save"]:=<|"self"->inst[], "name"-> SymbolName@Unevaluated@inst|>;
	
	inst/:Head[inst]="class";
	inst/:Values[inst]:=Values@inst[];
	inst/:Keys[inst]:=Keys@inst[];
	inst/:ByteCount[inst]:=ByteCount@inst[];
	inst/:Set[inst,op__:OptionsPattern]:=inst["set",op];
		
	(*set name of parent class. I guess coreClass is more or less embedded*)
	inst . "set"["parent"-> coreClass];
	Protect[inst];
	
		
	
	
];


ClearAll["shelfClass"];
shelfClass[self_Symbol, s:(_?AssociationQ):<||>]:=Module[{props=s},
	coreClass[self,props];
	Unprotect@self;
	self . "set"["parent"->shelfClass];
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
				Table[{#["compressedRatio"],#["compressedQuality"]}&@self . "cosineCompress"[o],
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
		Module[{},
		(
		sxmClass[self,s];
		self . "set"[parseSXM[fname]];
		self . "set"["calibration"->((self[]["header","scan_pixels"]/self[]["header","scan_range"])//(Normal@#  1*^-9)&//First)];
		)
		];

sxmClass[self_String, s:(_?AssociationQ):<||>]:=
	sxmClass[Symbol[self], s];

sxmClass[self_Symbol, s:(_?AssociationQ):<||>]:=
Module[{props=s}, (
(*init*)
	coreClass[self,props];
	Unprotect@self;
	self . "set"["parent"->sxmClass];
	
	
(*read image*)

	self/:Dot[self,"bgSubtract"[chan_:{"Z","forward"}]]:=
				self . "set"["bgCorr"->(self[][Sequence@@chan]//Normal//Calme`bgSubtract[#]&//Image)];
				
	self/:Dot[self,"imageScalebar"[im_Image, cf_:"GrayYellowTones", scaleColor_:White, scaleBarScale_:0.2, rescale_:{0,1.0}, modifier_:(#&)]]:=
	
	  Block[{scaledData,scaleBarValue, scaleBar, imPlotScale, min, max},
		(
		scaledData = im//ImageData//Rescale//Rescale[#,rescale]&;
		{min,max}= {Min@#,Max@#}&@Flatten@Flatten@scaledData;
		scaleBarValue = (scaleBarScale*First@Dimensions@scaledData/self[]["calibration"])//NumberForm[#,2]&;
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
	Protect@self;
self)];


ClearAll["gridClass"];
gridClass[inst_Symbol, s:(_?AssociationQ):<||>] :=
	Module[{props=s}, (
		(*init*)
		coreClass[inst,props];
		Unprotect@inst;
		inst . "set"["parent"->gridClass];
		
		Protect@inst;	
		
		
		inst )];	


gridClass[inst_String, s:(_?AssociationQ):<||>] :=
	gridClass[Symbol[inst], s];
	
gridClass[inst_Symbol, fname_String, s:(_?AssociationQ):<||>] :=
		Module[{},
		(
		gridClass[inst,s];
		inst . "set"[parse3DS[fname]];
		(*extra methods*)
		Unprotect@inst;
		
		(*==========ndiv=================*)
		inst/:Dot[inst,"ndiv"[chan_:"Current (A)",bias_:"sweep_signal"]]:=
			Block[{b, ndiv},
			b = inst[bias]//Normal;
			ndiv = inst[chan]//RightComposition[
					Normal,
					Flatten[#,1]&,
					Calme`denoise[3][#]&,
					Normal,
					Map[(DerivativeFilter[#,{1},1]/DerivativeFilter[b,{1},1])&,#]&,
					ArrayReshape[#,Dimensions@inst[chan]]&,
					NumericArray
					]@#&;
			inst . {chan <> " ndiv"-> ndiv};				
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
				
		(*===============================*)
		Protect@inst;
		)
		];

gridClass[inst_String, fname_String, s:(_?AssociationQ):<||>] :=
	gridClass[Symbol[inst], fname, s];



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


Scan[SetAttributes[#, {Protected, ReadProtected}]&,
     Select[Symbol /@ Names["OOP`*"], Head[#] === Symbol &]];


End[];


Protect["OOP`*"];


EndPackage[]
