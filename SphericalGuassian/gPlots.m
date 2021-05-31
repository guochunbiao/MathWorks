(* ::Package:: *)

BeginPackage["gPlots`"];
<<sgCommon.m;
Needs["sgCommon`"];
<<gUtils.m;
Needs["gUtils`"];


ClearAll[gParamPlot];
gParamPlot::usage="function{gParamPlot}";


Begin["`Private`"];
On[Assert];


ClearAll[gParamCircle];
gParamCircle[circleInput_,\[Theta]_]:=Module[
	{c,r},
	c=circleInput["center"];
	r=circleInput["radius"];
	c+r*{Cos[\[Theta]],Sin[\[Theta]]}
];


ClearAll[gParamLine];
gParamLine[startPos_,dirVector_,length_,\[Theta]_]:=Module[
	{},
	startPos+(length/2\[Pi])*\[Theta]*Normalize[dirVector]
];


ClearAll[gParamLine2];
gParamLine2[lineInput_,\[Theta]_]:=Module[
	{startPos,dirVector,length},
	startPos=lineInput["startPos"];
	dirVector=lineInput["dirVec"];
	length=lineInput["length"];
	gParamLine[startPos,dirVector,length,\[Theta]]
];


ClearAll[gParamLine3];
gParamLine3[lineInput_,\[Theta]_]:=Module[
	{startPos,dirAngle,length,dirVector},
	startPos=lineInput["startPos"];
	dirAngle=lineInput["dirAngle"];
	length=lineInput["length"];
	dirVector=FromPolarCoordinates[{1,dirAngle}];
	gParamLine[startPos,dirVector,length,\[Theta]]
];


ClearAll[gParamBasicDisb];
gParamBasicDisb[disbInput_,\[Theta]_]:=Module[
	{c,disbFunc},
	c=disbInput["center"];
	disbFunc=disbInput["disbFunc"];
	c+{(\[Theta]-\[Pi]),disbFunc[<|"\[Theta]"->\[Theta]|>]}
];


ClearAll[gParamSGDisb];
gParamSGDisb[disbInput_,\[Theta]_]:=Module[
	{disbCenter,upAxis,sgLambda,sgMu,realTheta,sgPower,upAngle},
	disbCenter=disbInput["disbCenter"];
	upAxis=disbInput["upAxis"];
	sgLambda=disbInput["sgLambda"];
	sgMu=disbInput["sgMu"];
	realTheta=\[Theta]-\[Pi];
	upAngle=ToPolarCoordinates[upAxis][[2]];
	sgPower=If[-\[Pi]/2<=realTheta<=\[Pi]/2,sgPolar[realTheta,sgLambda,sgMu],0];
	(RotationTransform[upAngle-\[Pi]/2,disbCenter]/@{(disbCenter+{realTheta,sgPower})})
];


(*
circles: {center,radius}
dirLines: {start,direction vector}
axisExtent: plot range
*)
gParamPlot:=Module[
	{plotList,plotColors,plotLabels},
	plotList={};
	plotColors={};
	plotLabels={};
	(*append circles*)
	Do[AppendTo[plotList,gParamCircle[circle,\[Theta]]];
	     AppendTo[plotColors,circle["color"]];
	     AppendTo[plotLabels,circle["label"]],
		{circle,#circles}];
	(*append dir lines*)
	Do[AppendTo[plotList,gParamLine2[line,\[Theta]]];
	     AppendTo[plotColors,line["color"]];
	     AppendTo[plotLabels,line["label"]],
		{line,#dirLines}];
	(*append angle lines*)
	Do[AppendTo[plotList,gParamLine3[line,\[Theta]]];
		 AppendTo[plotColors,line["color"]];
	     AppendTo[plotLabels,line["label"]],
		{line,#angleLines}];
	(*append simple distributions*)
	Do[AppendTo[plotList,gParamBasicDisb[basicDisb,\[Theta]]];
		 AppendTo[plotColors,basicDisb["color"]];
	     AppendTo[plotLabels,basicDisb["label"]],
		{basicDisb,#basicDisbs}];
	(*append SG distributions*)
	Do[AppendTo[plotList,gParamSGDisb[sgDisb,\[Theta]]];
		 AppendTo[plotColors,sgDisb["color"]];
	     AppendTo[plotLabels,sgDisb["label"]],
		{sgDisb,#sgDisbs}];
		
	ParametricPlot[plotList,
		{\[Theta],0,2 \[Pi]},
		PlotStyle->plotColors,
		PlotLabels->plotLabels,
		PlotRange->{{-#axisExtent,#axisExtent},{-#axisExtent,#axisExtent}},
		AspectRatio->1]
]&;


End[];


EndPackage[];
