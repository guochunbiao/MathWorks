(* ::Package:: *)

BeginPackage["gPlotsEx`"];
SetDirectory[FileNameJoin@{ParentDirectory[NotebookDirectory[]],"Shared"}];
Needs["sgCommon`"];
Needs["gUtils`"];
Needs["gBRDF`"];
ResetDirectory[];


ClearAll[pltRect2D,pltRectLine2D,pltCircle2D];
pltRect2D::usage="pltRect2D";
pltRectLine2D::usage="pltRectLine2D";
pltCircle2D::usage="pltCircle2D";


Begin["`Private`"];


pltRect2D[input_]:=Module[
	{min,max,colorFunc},
	
	min=gAssocData[input,"min"];
	max=gAssocData[input,"max"];
	
	colorFunc=gAssocDataOpt[input,"colorFunc",Function[{x,y},Cyan]];
	
	{
		ParametricPlot[
			{x,y},
			{x,min[[1]],max[[1]]},{y,min[[2]],max[[2]]},
			ColorFunction->colorFunc]
	}
];


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
