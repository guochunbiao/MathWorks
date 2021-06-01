(* ::Package:: *)

BeginPackage["gPlots`"];
<<sgCommon.m;
Needs["sgCommon`"];
<<gUtils.m;
Needs["gUtils`"];


ClearAll[gParamPlot];
gParamPlot::usage="function{gParamPlot}";
gParamSGPointLight::usage="aaa";


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


ClearAll[gParamSGCommon];
gParamSGCommon[disbCenter_,sg_,\[Theta]_]:=Module[
	{sgAxis,sgLambda,sgMu,realTheta,sgPower,upAngle},
	sgAxis=sg[[1]];
	sgLambda=sg[[2]];
	sgMu=sg[[3]];
	realTheta=\[Theta]-\[Pi];
	upAngle=ToPolarCoordinates[sgAxis][[2]];
	sgPower=If[-\[Pi]/2<=realTheta<=\[Pi]/2,sgPolar2[realTheta,sg],0];
	(RotationTransform[upAngle-\[Pi]/2,disbCenter]/@{(disbCenter+{realTheta,sgPower})})
];


ClearAll[gParamSGDisb];
gParamSGDisb[disbInput_,\[Theta]_]:=Module[
	{disbCenter,sg},
	disbCenter=disbInput["disbCenter"];
	sg=disbInput["sg"];
	gParamSGCommon[disbCenter,sg,\[Theta]]
];


ClearAll[gParamSGFuncDisb];
gParamSGFuncDisb[disbInput_,\[Theta]_]:=Module[
	{disbCenter,sgShadingFunc,sgFunc,sg,shadingPos,shadingNormal,
		shadingDist,lightDir,nol},
	disbCenter=disbInput["disbCenter"];
	sgFunc=disbInput["sgFunc"];
	shadingPos={\[Theta]-\[Pi],0};
	shadingNormal=Normalize[{0,1}];
	nol=Dot[lightDir,shadingNormal];
	sg=sgFunc[];
	gParamSGCommon[disbCenter,sg,\[Theta]]
];


ClearAll[gParamGroundShading];
gParamGroundShading[shadingInput_,\[Theta]_]:=Module[
	{shadingFunc,lightCenter,lightRadius,shadingPos,shadingNormal,
		shadingDist,lightDir,nol,
		shadedValue},
	shadingFunc=shadingInput["shadingFunc"];
	lightCenter=shadingInput["lightCenter"];
	lightRadius=shadingInput["lightRadius"];
	shadingPos={\[Theta]-\[Pi],0};
	shadingNormal=Normalize[{0,1}];
	shadingDist=Norm[lightCenter-shadingPos];
	lightDir=Normalize[lightCenter-shadingPos];
	nol=Dot[lightDir,shadingNormal];
	shadedValue=shadingFunc[<|"lightCenter"->lightCenter,"lightRadius"->lightRadius,
		"lightDir"->lightDir,"shadingPos"->shadingPos,"shadingDist"->shadingDist,
		"nol"->nol|>];
	{shadingPos[[1]],shadedValue}
];


ClearAll[gParamSGPointLight];
gParamSGPointLight[input_,\[Theta]_]:=Module[
	{sgFunc,sg,lightCenter,lightRadius,lightIntensity,shadingPos},
	sgFunc=input["sgFunc"];
	lightCenter=input["lightCenter"];
	lightRadius=input["lightRadius"];
	lightIntensity=input["lightIntensity"];
	shadingPos=input["shadingPos"];
	sg=sgFunc[<|"lightCenter"->lightCenter,"lightRadius"->lightRadius,
		"lightIntensity"->lightIntensity,"shadingPos"->shadingPos|>];
	gParamSGCommon[shadingPos,sg,\[Theta]]
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
	(*append SG function distributions*)
	collectFunc["sgFuncDisbs",gParamSGFuncDisb];
	(*append grounding shading*)
	collectFunc["groundShadings",gParamGroundShading];
	(*append SG point lights*)
	collectFunc["sgPointLights",gParamSGPointLight];

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
