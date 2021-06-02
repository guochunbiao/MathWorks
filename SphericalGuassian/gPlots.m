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


ClearAll[gParamVaryingDisbShared];
gParamVaryingDisbShared[disbInput_,\[Theta]_,viewDir_,lightDir_,normalDir_]:=Module[
	{disbFunc,roughness,halfDir,nol,noh,voh,nov},
	disbFunc=disbInput["disbFunc"];
	roughness=disbInput["roughness"];
	halfDir=Normalize[viewDir+lightDir];
	nol=Clip[Dot[normalDir,lightDir],{0,1}];
	noh=Clip[Dot[normalDir,halfDir],{0,1}];
	voh=Clip[Dot[viewDir,halfDir],{0,1}];
	nov=Clip[Dot[normalDir,viewDir],{0,1}];
	disbFunc[<|"\[Theta]"->\[Theta],"viewDir"->viewDir,"lightDir"->lightDir,"normalDir"->normalDir,
		"halfDir"->halfDir,"roughness"->roughness,
		"nol"->nol,"noh"->noh,"voh"->voh,"nov"->nov|>]
];


ClearAll[gParamVaryDirectionDisbs];
gParamVaryDirectionDisbs[disbInput_,\[Theta]_]:=Module[
	{viewDir,lightDir,normalDir},
	viewDir=Normalize[disbInput["viewDir"][<|"\[Theta]"->\[Theta]|>]];
	lightDir=Normalize[disbInput["lightDir"][<|"\[Theta]"->\[Theta]|>]];
	normalDir=Normalize[disbInput["normalDir"][<|"\[Theta]"->\[Theta]|>]];
	gParamVaryingDisbShared[disbInput,\[Theta],viewDir,lightDir,normalDir]
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


ClearAll[gParamSGVaryDirDisb];
gParamSGVaryDirDisb[disbInput_,\[Theta]_]:=Module[
	{roughness,viewDir,lightDir,normalDir,halfDir,sgFunc,
		sgValue},
	roughness=disbInput["roughness"];
	viewDir=Normalize[disbInput["viewDir"][<|"\[Theta]"->\[Theta]|>]];
	lightDir=Normalize[disbInput["lightDir"][<|"\[Theta]"->\[Theta]|>]];
	normalDir=Normalize[disbInput["normalDir"][<|"\[Theta]"->\[Theta]|>]];
	halfDir=Normalize[viewDir+lightDir];
	sgFunc=disbInput["sgFunc"];
	sgValue=sgFunc[<|"\[Theta]"->\[Theta],"roughness"->roughness,"lightDir"->lightDir,
			"viewDir"->viewDir,"normalDir"->normalDir,"halfDir"->halfDir|>];
	
	sgValue
];


ClearAll[gParamGroundShading];
gParamGroundShading[shadingInput_,\[Theta]_]:=Module[
	{inputKeys,shadingFunc,lightCenter,lightRadius,shadingPos,normalDir,
		shadingDist,lightDir,viewDir,
		roughness,nol,
		shadedValue},
	inputKeys=Keys[shadingInput];
	shadingFunc=shadingInput["shadingFunc"];
	lightCenter=shadingInput["lightCenter"];
	lightRadius=shadingInput["lightRadius"];
	roughness=If[MemberQ[inputKeys,"roughness"],shadingInput["roughness"],NaN];
	viewDir=Normalize[If[MemberQ[inputKeys,"viewDir"],shadingInput["viewDir"],NaN]];
	shadingPos={\[Theta]-\[Pi],0};
	normalDir=Normalize[{0,1}];
	shadingDist=Norm[lightCenter-shadingPos];
	lightDir=Normalize[lightCenter-shadingPos];
	nol=Dot[normalDir,lightDir];
	shadedValue=shadingFunc[<|"lightCenter"->lightCenter,"lightRadius"->lightRadius,
		"lightDir"->lightDir,"shadingPos"->shadingPos,"shadingDist"->shadingDist,
		"normalDir"->normalDir,"viewDir"->viewDir,"roughness"->roughness,"nol"->nol|>];
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


ClearAll[gParamSGGroundShading];
gParamSGGroundShading[input_,\[Theta]_]:=Module[
	{inputKeys,sgLightFunc,sgLight,sgShadingFunc,sgShading,viewDir,lightDir,
		lightCenter,lightRadius,lightIntensity,shadingPos,shadingDist,
		normalDir,roughness,sgNDF,sgClampedCos},
	inputKeys=Keys[input];
	sgLightFunc=input["sgLightFunc"];
	sgShadingFunc=input["sgShadingFunc"];
	lightCenter=input["lightCenter"];
	lightRadius=input["lightRadius"];
	lightIntensity=input["lightIntensity"];
	roughness=If[MemberQ[inputKeys,"roughness"],input["roughness"],NaN];
	viewDir=Normalize[If[MemberQ[inputKeys,"viewDir"],input["viewDir"],NaN]];
	shadingPos={\[Theta]-\[Pi],0};
	normalDir=Normalize[{0,1}];
	lightDir=Normalize[lightCenter-shadingPos];
	shadingDist=Norm[lightCenter-shadingPos];
	sgLight=sgLightFunc[<|"lightCenter"->lightCenter,"lightRadius"->lightRadius,
		"lightIntensity"->lightIntensity,"shadingPos"->shadingPos|>];
	sgNDF=If[MemberQ[inputKeys,"sgNDFFunc"],
				input["sgNDFFunc"][<|"roughness"->roughness,"lightDir"->lightDir,
							"viewDir"->viewDir,"normalDir"->normalDir|>],
				{p,\[Lambda],\[Mu]}];
	sgClampedCos=sgClampedCosine[normalDir,sgLight[[1]]];
	sgShading=sgShadingFunc[<|"sgLight"->sgLight,"sgClampedCos"->sgClampedCos,
				"sgNDF"->sgNDF,"lightRadius"->lightRadius,"shadingDist"->shadingDist|>];
	{shadingPos[[1]],sgShading}
];


(*
circles: {center,radius}
dirLines: {start,direction vector}
axisExtent: plot range
*)
gParamPlot[inputs_,imageSize_:Tiny]:=Module[
	{
		inputKeys,collectFunc,
		plotList,plotStyles,plotLabels,
		axisExtent
	},
    inputKeys=Keys[inputs];    
	plotList={};
	plotStyles={};
	plotLabels={};
	
	collectFunc[keyName_,paramFunc_]:={
		If[MemberQ[inputKeys,keyName],
			elements=inputs[[keyName]];
			For[i=1,i<=Length[elements],i++,
				element=elements[[i]];
				AppendTo[plotList,paramFunc[element,\[Theta]]];
				elementKeys=Keys[element];
				(*AppendTo[plotStyles,If[MemberQ[elementKeys,"color"],element["color"],Brown]];*)
				tmpColor=If[MemberQ[elementKeys,"color"],element["color"],LightBrown];
				tmpThickness=If[MemberQ[elementKeys,"thickness"],element["thickness"],0.01];
				AppendTo[plotStyles,{tmpColor,Thickness[tmpThickness]}];
				AppendTo[plotLabels,If[MemberQ[elementKeys,"label"],element["label"],""]];
			];
		];
	};
	
	(*append circles*)
	collectFunc["circles",gParamCircle];
	(*append dir lines*)
	collectFunc["lines",gParamLine];
	(*append simple distributions*)
	collectFunc["basicDisbs",gParamBasicDisb];
	(*append shading distributions, direction(view or light or normal) varying with \[Theta]*)
	collectFunc["varyDirectionDisbs",gParamVaryDirectionDisbs];
	(*append SG distributions*)
	collectFunc["sgDisbs",gParamSGDisb];
	(*append SG function distributions*)
	collectFunc["sgFuncDisbs",gParamSGFuncDisb];
	(*append SG varying direction distributions*)
	collectFunc["sgVaryDirDisbs",gParamSGVaryDirDisb];
	(*append grounding shading*)
	collectFunc["groundShadings",gParamGroundShading];
	(*append SG point lights*)
	collectFunc["sgPointLights",gParamSGPointLight];
	(*append SG point lights*)
	collectFunc["sgGroundShading",gParamSGGroundShading];

	axisExtent=If[MemberQ[inputKeys,"axisExtent"],inputs[["axisExtent"]],5];
	ParametricPlot[plotList,
		{\[Theta],0,2 \[Pi]},
		PlotStyle->plotStyles,
		PlotLegends->plotLabels,
		PlotRange->{{-axisExtent,axisExtent},{-axisExtent,axisExtent}},
		AspectRatio->1,
		ImageSize->imageSize]
];


End[];


EndPackage[];
