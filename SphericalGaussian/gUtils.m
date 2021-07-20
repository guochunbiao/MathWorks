(* ::Package:: *)

BeginPackage["gUtils`"];


ClearAll[gStructRules,gPrint,gPrintFunc,gEvalFunc,gCreateCone,gLerp,gRemap,gClampPhi,
	gCalcRectCorners,gReflectVector,gAssocData,gAssocDataOpt];
gPrint::usage="Print messages";
gPrintFunc::usage="Print a function";
gEvalFunc::usage="Evaluate a funciton";
gQuickFinMin::usage="Quick find minimum";
gCreateCone::usgae="Create a cone";
gLerp::usage="gLerp";
gRemap::usage="gRemap";
gClampPhi::usage="gClampPhi";
gCalcRectCorners::usage="gCalcRectCorners";
gReflectVector::usage="gReflectVector";
gAssocData::usage="gAssocData";
gAssocDataOpt::usage="gAssocDataOpt";


SetAttributes[gStructRules,HoldAll]
gStructRules[rules_,expr_]:=First@PreemptProtect@Internal`InheritedBlock[
	{Rule,RuleDelayed},
	SetAttributes[{Rule,RuleDelayed},HoldFirst];
	Hold[expr]/.rules];


Begin["`Private`"];


gPrint[msgs__]:=Module[
{narg,text},
narg=Length[List[msgs]];
text="";
For[i=1,i<=narg,i++,text=StringJoin[text," ",ToString[List[msgs][[i]]]]];
       Print[Style[text,FontSize->18,Background->LightBlue]];
];


gPrintFunc[name_,func_]:=Module[
{ret},
ret=Print[Style[
StringJoin[ToString[name],"=",ToString[TraditionalForm[func]]],
FontSize->18,Background->LightBlue]];
ret
];


gEvalFunc[name_,func_]:=Print[Style[
StringJoin[ToString[name],"=",ToString[func]], FontSize->18,Background->LightBlue]];


gCreateCone[originPt_,refPt0_,refPt1_]:=Module[
	{dirAngle,apertureAngle,refTheta0,refTheta1,minTheta,maxTheta},
	refTheta0=ToPolarCoordinates[refPt0-originPt][[2]];
	refTheta1=ToPolarCoordinates[refPt1-originPt][[2]];
	minTheta=Min[refTheta0,refTheta1];
	If[minTheta<0,minTheta=minTheta+2\[Pi]];
	maxTheta=Max[refTheta0,refTheta1];
	If[maxTheta<0,maxTheta=maxTheta+2\[Pi]];
	dirAngle=(maxTheta+minTheta)/2;
	apertureAngle=(maxTheta-minTheta)/2;
	
	{dirAngle,apertureAngle}
];


gLerp[from_,to_,factor_]:=Module[
	{t,interpolated},
	t=Clip[factor,{0,1}];
	
	interpolated=from+(to-from)*t;
	interpolated
];


gRemap[x_,oldRange_,newRange_]:=Module[
	{os,oe,ns,ne,factor,nx},
	os=oldRange[[1]];
	oe=oldRange[[2]];
	ns=newRange[[1]];
	ne=newRange[[2]];
	
	factor=(x-os)/(oe-os);
	nx=ns+factor*(ne-ns);
	
	nx
];


gClampPhi[inPhi_]:=Module[
	{outPhi},
	
	outPhi=If[inPhi<0,inPhi+2\[Pi],inPhi];
	outPhi
];


gCalcRectCorners[rectInput_]:=Module[
	{rectCenter,rectNormal,rectMajorAxis,rectMinorAxis,rectMajorRadius,rectMinorRadius,
		leftTop,rightTop,leftBtm,rightBtm},
	
	rectCenter=rectInput[[1]];
	rectNormal=Normalize[rectInput[[2]]];
	rectMajorAxis=Normalize[rectInput[[3]]];
	Assert[Dot[rectNormal,rectMajorAxis]==0];
	rectMinorAxis=Normalize@Cross[rectNormal,rectMajorAxis];
	rectMajorRadius=rectInput[[4]];
	rectMinorRadius=rectInput[[5]];
	
	leftTop=rectCenter-rectMinorRadius*rectMinorAxis+rectMajorAxis*rectMajorRadius;
	rightTop=rectCenter+rectMinorRadius*rectMinorAxis+rectMajorAxis*rectMajorRadius;
	leftBtm=rectCenter-rectMinorRadius*rectMinorAxis-rectMajorAxis*rectMajorRadius;
	rightBtm=rectCenter+rectMinorRadius*rectMinorAxis-rectMajorAxis*rectMajorRadius;
	
	{leftTop,rightTop,leftBtm,rightBtm}
];


gReflectVector[viewOrLight_,half_]:=Module[
	{o,h,i},
	
	o=Normalize@viewOrLight;
	h=Normalize@half;
	i=Normalize[2Dot[o,h]*h-o];
	
	i
];


gAssocData[assoc_,key_]:=Module[
	{},
	
	Assert[MemberQ[Keys[assoc],key]];
	assoc[key]
];


gAssocDataOpt[assoc_,key_,default_]:=Module[
	{},
	
	If[MemberQ[Keys[assoc],key],assoc[key],default]
];


End[];


EndPackage[];
