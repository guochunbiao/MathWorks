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
gParamLine[lineInput_,\[Theta]_]:=Module[
	{startPos,dirVector,length},
	startPos=lineInput["startPos"];
	dirVector=lineInput["dirVec"];
	length=lineInput["length"];
	startPos+(length/2\[Pi])*\[Theta]*Normalize[dirVector]
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
	{disbCenter,sg,sgAxis,sgLambda,sgMu,realTheta,sgPower,upAngle},
	disbCenter=disbInput["disbCenter"];
	sg=disbInput["sg"];
	sgAxis=sg[[1]];
	sgLambda=sg[[2]];
	sgMu=sg[[3]];
	realTheta=\[Theta]-\[Pi];
	upAngle=ToPolarCoordinates[sgAxis][[2]];
	sgPower=If[-\[Pi]/2<=realTheta<=\[Pi]/2,sgPolar2[realTheta,sg],0];
	(RotationTransform[upAngle-\[Pi]/2,disbCenter]/@{(disbCenter+{realTheta,sgPower})})
];


(*
circles: {center,radius}
dirLines: {start,direction vector}
axisExtent: plot range
*)
gParamPlot[inputs_,imageSize_:Tiny]:=Module[
	{
		inputKeys,collectFunc,
		plotList,plotColors,plotLabels,
		axisExtent
	},
    inputKeys=Keys[inputs];    
	plotList={};
	plotColors={};
	plotLabels={};
	
	collectFunc[keyName_,paramFunc_]:={
		If[MemberQ[inputKeys,keyName],
			elements=inputs[[keyName]];
			For[i=1,i<=Length[elements],i++,
				element=elements[[i]];
				AppendTo[plotList,paramFunc[element,\[Theta]]];
				elementKeys=Keys[element];
				If[MemberQ[elementKeys,"color"],AppendTo[plotColors,element["color"]]];
				If[MemberQ[elementKeys,"label"],AppendTo[plotLabels,element["label"]]];
			];
		];
	};
	
	(*append circles*)
	collectFunc["circles",gParamCircle];		
	(*append dir lines*)
	collectFunc["lines",gParamLine];
	(*append simple distributions*)
	collectFunc["basicDisbs",gParamBasicDisb];		
	(*append SG distributions*)
	collectFunc["sgDisbs",gParamSGDisb];		
	(*append grounding shading*)

	axisExtent=If[MemberQ[inputKeys,"axisExtent"],inputs[["axisExtent"]],5];
	ParametricPlot[plotList,
		{\[Theta],0,2 \[Pi]},
		PlotStyle->plotColors,
		PlotLabels->plotLabels,
		PlotRange->{{-axisExtent,axisExtent},{-axisExtent,axisExtent}},
		AspectRatio->1,
		ImageSize->imageSize]
];


End[];


EndPackage[];
