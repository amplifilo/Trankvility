(* ::Package:: *)

AppendTo[$Path,NotebookDirectory[]];


BeginPackage["FFTp`"];


Unprotect["FFTp`*"];
ClearAll["FFTp`*"];
ClearAll["FFTp`Private`*"];


fftshift::usage = "Center Fourier transform (phase-preserving)";
ifftshift::usage = "Uncenter Fourier transform (phase-preserving)";
(*imageCenter::usage = "Return coordinates of image center";
hannWindow::usage = "Return 2D Hann window (useful for FFT)";
*)
fftBase::usage="apply FFT to image. Return association with results";
centerxy::usage="return center coordinates of image";
fftDemod2::usage="demodulate FFT on specific reflex";
hamming2D::usage ="hamming window, arbitrary dimensions";
fftFiltered::usage = "filter FFT based on select points/mask";
pointsOffsets::usage = "convert selected points into offsets from center";
pointMask::usage="make mask for FFT filtering"


Begin["`Private`"];


centerxy[im_]:=If[MatchQ[Head@im,List],Dimensions@im,ImageDimensions@im]/2;


(* ::Code::Initialization::GrayLevel[0]:: *)
fftshift[dat_?ArrayQ,k:(_Integer?Positive|All):All]:=Module[{dims=Dimensions[dat]},RotateRight[dat,If[k===All,Quotient[dims,2],Quotient[dims[[k]],2] UnitVector@@{Length[dims],k}]]];


(* ::Code::Initialization::GrayLevel[0]:: *)
ifftshift[dat_?ArrayQ,k:(_Integer?Positive|All):All]:=Module[{dims=Dimensions[dat]},RotateRight[dat,If[k===All,Ceiling[dims/2],Ceiling[dims[[k]]/2] UnitVector@@{Length[dims],k}]]];


hamming2D[dims_]:=
Module[{wnd},
(
wnd=N@Array[HammingWindow,#,{-.5,.5}]&/@dims;
wnd=Outer[Times,wnd[[1]],wnd[[2]]]
)
]


fftBase[im_]:=
Module[{fft=<||>},
(
fft["ImageData"]=im;
fft["window"] = hamming2D@Dimensions@im;
fft["FFT"]=fft["ImageData"]//Times[Normal@#,fft["window"]]&//Fourier//fftshift;
fft["FFTimage"] =fft["FFT"]// Abs//Log//Image//ImageAdjust;
fft["center"] = fft["ImageData"]//Dimensions//(#/2)&;
Return[fft]
)];


fftDemod2[fft_,shiftfft_:{0,0},gausWidth_:12]:=
Module[{mask},
(
mask = fft["FFT"]//DiskMatrix[1,Dimensions@#]&//GaussianFilter[#,gausWidth]&;
RotateRight[fft["FFT"],shiftfft]//Times[#,mask]&//ifftshift//InverseFourier//Abs//(#/fft["window"])&
)
];


pointsOffsets[pts_List,im_,op_:Subtract]:=
Module[
{xydims = ImageDimensions@im},
(
Return[pts//op[#,xydims/2]&/@#&]
)
];


pointMask[fft_Association,points_,gaussWidth_:10]:=
Module[{mask},
	mask = fft["ImageData"]//ConstantArray[0,Dimensions@#]&;
	Table[mask[[Sequence@@Reverse@ij]]=1,{ij,points}];
	mask//Image//ImageConvolve[#,GaussianMatrix[gaussWidth]]&//ImageData//Rescale//Reverse
]


fftFiltered[fft_Association,coords_,gaussWidth_]:=
Module[{ones, mask},
(
	mask = fft["ImageData"]//ConstantArray[0,Dimensions@#]&//Image;
	mask = ReplacePixelValue[mask,Table[c->1,{c,coords}]];
	mask = mask//Image//ImageConvolve[#,GaussianMatrix[gaussWidth]]&//ImageData//Rescale;
	mask//Image//EchoFunction[Show[#,ImageSize->400]&];
	fft["FFT"]//
		Times[#,mask]&//
		ifftshift//
		Fourier//
		Reverse[#,{1,2}]&//
		Divide[#,hamming2D@Dimensions@fft["ImageData"]]&//
		Abs//
		Image//
		ImageAdjust
)
];

fftFiltered[fft_Association,mask_]:=
Module[{},
(
	fft["FFT"]//
		Times[#,mask]&//
		ifftshift//
		Fourier//
		Reverse[#,{1,2}]&//
		Divide[#,hamming2D@Dimensions@fft["ImageData"]]&//
		Abs//
		Image//
		ImageAdjust
)
];


End[];


Protect["FFTp`*"];


EndPackage[]
