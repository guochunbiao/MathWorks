(* ::Package:: *)

BeginPackage["pbrtLog`"];
SetDirectory[FileNameJoin@{ParentDirectory[NotebookDirectory[]],"Shared"}];
Needs["gUtils`"];
ResetDirectory[];


ClearAll[pbrtLogInitPixel,pbrtLogPrintBounce,pbrtLogPrintElement,pbrtLogPrintPixel,
	pbrtLogBounceLe,pbrtLogBounceLd,pbrtLogStartBounce,pbrtLogEndBounce,pbrtLogPixelLi,
	pbrtCurrPixelLog,pbrtCurrBounceIndex,pbrtCurrBounce,pbrtLogMisLight,pbrtLogMisBsdf];
pbrtCurrPixelLog::usage="pbrtCurrPixelLog";
pbrtCurrBounceIndex::usage="pbrtCurrBounceIndex";
pbrtCurrBounce::usage="pbrtCurrBounce";
pbrtLogInitPixel::usage="pbrtLogInitPixel";
pbrtLogBounceLe::usage="pbrtLogBounceLe";
pbrtLogBounceLd::usage="pbrtLogBounceLd";
pbrtLogPrintBounce::usage="pbrtLogPrintBounce";
pbrtLogPrintElement::usage="pbrtLogPrintElement";
pbrtLogPrintPixel::usage="pbrtLogPrintPixel";
pbrtLogStartBounce::usage="pbrtLogStartBounce";
pbrtLogEndBounce::usage="pbrtLogEndBounce";
pbrtLogPixelLi::usage="pbrtLogPixelLi";
pbrtLogMisLight::usage="pbrtLogMisLight";
pbrtLogMisBsdf::usage="pbrtLogMisBsdf";


Begin["`Private`"];


(*CLogger::logCameraRay*)
pbrtLogInitPixel[pixel_,cameraRay_]:=Module[
	{},
	
	pbrtCurrPixelLog=<|"pixel"->pixel|>;
	pbrtCurrPixelLog["cameraRay"]=<|"o"->cameraRay[[1]],"d"->cameraRay[[2]]|>;
	pbrtCurrPixelLog["bounces"]={};
	pbrtCurrBounceIndex=-1;
	
	pbrtCurrPixelLog
];


(*CLogger::logLi*)
pbrtLogPixelLi[inLi_]:=Module[
	{},
	
	pbrtCurrPixelLog["li"]=inLi;
];


pbrtLogStartBounce[bounceIdx_]:=Module[
	{},
	pbrtCurrBounceIndex=bounceIdx;
	pbrtCurrBounce=<|"base"-><|"index"->bounceIdx|>,"misLight"->"","misBsdf"->""|>;
	
	1
];


pbrtLogEndBounce[inL_]:=Module[
	{base,bounces},
	
	base=pbrtCurrBounce["base"];
	base["l"]=inL;
	pbrtCurrBounce["base"]=base;
	
	bounces=pbrtCurrPixelLog["bounces"];
	AppendTo[bounces,pbrtCurrBounce];
	pbrtCurrPixelLog["bounces"]=bounces;
	
	pbrtCurrBounce
];


pbrtLogBounceLe[le_,beta_]:=Module[
	{base},
	
	base=pbrtCurrBounce["base"];
	base["le"]=le;
	base["leBeta"]=beta;
	pbrtCurrBounce["base"]=base;
	
	pbrtCurrBounce
];


pbrtLogBounceLd[ld_,beta_]:=Module[
	{base},
	
	base=pbrtCurrBounce["base"];
	base["ld"]=ld;
	base["ldBeta"]=beta;
	pbrtCurrBounce["base"]=base;
	
	pbrtCurrBounce
];


pbrtLogMisLight[wo_,wi_,lightPdf_,scatteringPdf_,li_,f_,lo_]:=Module[
	{misLight},
	
	misLight=<||>;
	misLight["l_wo"]=wo;
	misLight["l_wi"]=wi;
	misLight["l_lightPdf"]=lightPdf;
	misLight["l_scatteringPdf"]=scatteringPdf;
	misLight["l_li"]=li;
	misLight["l_f"]=f;
	misLight["l_lo"]=lo;
	pbrtCurrBounce["misLight"]=misLight;
	
	pbrtCurrBounce
];


pbrtLogMisBsdf[wo_,wi_,lightPdf_,scatteringPdf_,li_,f_,tr_,lo_]:=Module[
	{misBsdf},
	
	misBsdf=<||>;
	misBsdf["b_wo"]=wo;
	misBsdf["b_wi"]=wi;
	misBsdf["b_lightPdf"]=lightPdf;
	misBsdf["b_scatteringPdf"]=scatteringPdf;
	misBsdf["b_li"]=li;
	misBsdf["b_f"]=f;
	misBsdf["b_tr"]=tr;
	misBsdf["b_lo"]=lo;
	pbrtCurrBounce["misBsdf"]=misBsdf;
	
	pbrtCurrBounce
];


pbrtLogPrintBounce[bounces_]:=Module[
	{i},

	For[i=1,i<=Length@bounces,i++,
		Print@Style[StringJoin["bounce_",ToString[i-1]],FontSize->16,Background->LightBlue];
		Print@Style["-->base",FontSize->15,Background->Cyan];
		Print[bounces[[i]]["base"]];
		Print@Style["-->misLight",FontSize->15,Background->Cyan];
		Print[bounces[[i]]["misLight"]];
		Print@Style["-->misBsdf",FontSize->15,Background->Cyan];
		Print[bounces[[i]]["misBsdf"]];
	];
];


pbrtLogPrintElement[key_,msg_]:=Module[
	{},

	If[key=="bounces",pbrtLogPrintBounce[msg];Return[]];
	Print@Style[key,FontSize->16,Background->LightBlue]+Print[msg]
];


pbrtLogPrintPixel[logger_]:=Module[
	{},

	Do[pbrtLogPrintElement[key,logger[key]],{key,Keys[logger]}]
];


End[];


EndPackage[];