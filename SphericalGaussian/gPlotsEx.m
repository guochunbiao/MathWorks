(* ::Package:: *)

BeginPackage["gPlotsEx`"];
Needs["sgCommon`"];
Needs["gUtils`"];
Needs["gBRDF`"];


ClearAll[pltRectLine2D,pltCircle2D];
pltRectLine2D::usage="pltRectLine2D";
pltCircle2D::usage="pltCircle2D";


Begin["`Private`"];


pltRectLine2D[input_]:=Module[
	{center,majorAxis,majorRadius,minorRadius,color,thickness,style,
		pointLT,pointRT,pointRB,pointLB},
	
	center=gAssocData[input,"center"];
	majorAxis=Normalize@gAssocData[input,"majorAxis"];
	majorRadius=gAssocData[input,"majorRadius"];
	minorRadius=gAssocData[input,"minorRadius"];
	
	color=gAssocDataOpt[input,"color",Black];
	thickness=gAssocDataOpt[input,"thickness",0.01];
	style=gAssocDataOpt[input,"style",Nothing];
	
	{pointLT,pointRT,pointRB,pointLB}=
		gCalcRect2DEdgePts[center,majorAxis,majorRadius,minorRadius];
	
	{
		Graphics[{AbsoluteThickness[thickness],style,color,
			Line[{pointLT,pointRT,pointRB,pointLB,pointLT}]}]
	}
];


pltCircle2D[input_]:=Module[
	{center,radius,color,thickness,style},
	
	center=gAssocData[input,"center"];
	radius=gAssocData[input,"radius"];
	
	color=gAssocDataOpt[input,"color", Black];
	thickness=gAssocDataOpt[input,"thickness",0.01];
	style=gAssocDataOpt[input,"style",Nothing];
	
	{
		Graphics[{AbsoluteThickness[thickness],style,color,
			Circle[center,radius]}]
	}
];


End[];


EndPackage[];
