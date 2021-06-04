(* ::Package:: *)

BeginPackage["gUtils`"];


gPrint::usage="Print messages";
gPrintFunc::usage="Print a function";
gEvalFunc::usage="Evaluate a funciton";
gQuickFinMin::usage="Quick find minimum";
gCreateCone::usgae="Create a cone";


ClearAll[gStructRules]
SetAttributes[gStructRules,HoldAll]
gStructRules[rules_,expr_]:=First@PreemptProtect@Internal`InheritedBlock[
	{Rule,RuleDelayed},
	SetAttributes[{Rule,RuleDelayed},HoldFirst];
	Hold[expr]/.rules];


Begin["`Private`"];


ClearAll[gPrint];
gPrint[msgs__]:=Module[
{narg,text},
narg=Length[List[msgs]];
text="";
For[i=1,i<=narg,i++,text=StringJoin[text," ",ToString[List[msgs][[i]]]]];
       Print[Style[text,FontSize->18,Background->LightBlue]];
];


ClearAll[gPrintFunc];
gPrintFunc[name_,func_]:=Module[
{ret},
ret=Print[Style[
StringJoin[ToString[name],"=",ToString[TraditionalForm[func]]],
FontSize->18,Background->LightBlue]];
ret
];


ClearAll[gEvalFunc];
gEvalFunc[name_,func_]:=Print[Style[
StringJoin[ToString[name],"=",ToString[func]], FontSize->18,Background->LightBlue]];


ClearAll[gCreateCone];
gCreateCone[originPt_,refPt0_,refPt1_]:=Module[
	{dirAngle,apertureAngle,refTheta0,refTheta1,minTheta,maxTheta},
	refTheta0=ToPolarCoordinates[refPt0-originPt][[2]];
	refTheta1=ToPolarCoordinates[refPt1-originPt][[2]];
	minTheta=Min[refTheta0,refTheta1];
	maxTheta=Max[refTheta0,refTheta1];
	dirAngle=(maxTheta+minTheta)/2;
	apertureAngle=(maxTheta-minTheta)/2;
	
	{dirAngle,apertureAngle}
];


End[];


EndPackage[];
