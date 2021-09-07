(* ::Package:: *)

BeginPackage["pbrtPathLog`"];
SetDirectory[FileNameJoin@{ParentDirectory[NotebookDirectory[]],"Shared"}];
Needs["gUtils`"];
ResetDirectory[];


ClearAll[pbrtLogInitPixel,pbrtLogPrintBounce,pbrtLogPrintElement,pbrtLogPrintPixel,
	pbrtLogBounceLe,pbrtLogBounceLd,pbrtLogStartBounce,pbrtLogEndBounce,pbrtLogPixelLi,
	pbrtCurrPixelLog,pbrtCurrBounceIndex,pbrtCurrBounce,pbrtLogMisLight,pbrtLogMisBsdf,
	pbrtLogNextBounce,pbrtExtractBounceArrows];
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
pbrtLogNextBounce::usage="pbrtLogNextBounce";
pbrtExtractBounceArrows::usage="pbrtExtractBounceArrows";


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


(*CBounce::logNextBounce*)
pbrtLogNextBounce[rayo_,rayd_,beta_]:=Module[
	{nextBounce},
	
	nextBounce=<||>;
	nextBounce["next_rayo"]=rayo;
	nextBounce["next_rayd"]=rayd;
	nextBounce["next_beta"]=beta;
	pbrtCurrBounce["nextBounce"]=nextBounce;
	
	pbrtCurrBounce
];


pbrtLogStartBounce[bounceIdx_,rayo_,rayd_]:=Module[
	{},
	pbrtCurrBounceIndex=bounceIdx;
	pbrtCurrBounce=<|"base"-><|"index"->bounceIdx,"rayo"->rayo,"rayd"->rayd|>,
		"misLight"->"","misBsdf"->""|>;
	
	pbrtCurrBounce
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


pbrtExtractBounceArrows[pixelLog_]:=Module[
	{bounces,bounce,lastBounce,i,base,misLight,misBsdf,nextBounce,
		pathArrows},
	pathArrows={};
	
	bounces=gAssocData[pixelLog,"bounces"];
	
	For[i=1,i<=Length@bounces,i++,
		bounce=bounces[[i]];
		base=gAssocData[bounce,"base"];
		misLight=gAssocData[bounce,"misLight"];
		misBsdf=gAssocData[bounce,"misBsdf"];
		nextBounce=gAssocData[bounce,"nextBounce"];
		
		AppendTo[pathArrows,<|"origin"->base["rayo"],"dir"->base["rayd"],
			"length"->Norm[base["rayo"]-nextBounce["next_rayo"]]|>];
	];
	(*out arrow*)
	lastBounce=bounces[[Length@bounces]];
	base=gAssocData[lastBounce,"base"];
	misLight=gAssocData[lastBounce,"misLight"];
	misBsdf=gAssocData[lastBounce,"misBsdf"];
	nextBounce=gAssocData[lastBounce,"nextBounce"];
	AppendTo[pathArrows,<|"origin"->nextBounce["next_rayo"],"dir"->nextBounce["next_rayd"],
			"length"->100|>];
	
	{pathArrows}
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
		Print@Style["-->nextBounce",FontSize->15,Background->Cyan];
		Print[bounces[[i]]["nextBounce"]];
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
