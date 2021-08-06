(* ::Package:: *)

BeginPackage["gPlots`"];
SetDirectory[FileNameJoin@{ParentDirectory[NotebookDirectory[]],"Shared"}];
Needs["sgCommon`"];
Needs["gUtils`"];
Needs["gBRDF`"];
ResetDirectory[];


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
	startPos+(0.1*length/2\[Pi])*\[Theta]*Normalize[dirVector]
];


ClearAll[gParamRect];
gParamRect[input_,\[Theta]_]:=Module[
	{centerPt,majorAxis,minorAxis,majorSize,minorSize,
		btmLeft,btmRight,topLeft,topRight},
	
	centerPt=input["center"];
	majorAxis=Normalize[input["majorAxis"]];
	minorAxis=Normalize[input["minorAxis"]];
	majorSize=input["majorSize"];
	minorSize=input["minorSize"];
	
	btmLeft=centerPt-majorSize/2 majorAxis-minorSize/2 minorAxis;
	btmRight=centerPt-majorSize/2 majorAxis+minorSize/2 minorAxis;
	topLeft=centerPt+majorSize/2 majorAxis-minorSize/2 minorAxis;
	topRight=centerPt+majorSize/2 majorAxis+minorSize/2 minorAxis;
	
	{
		btmLeft+minorSize*minorAxis*0.1*(\[Theta]/2\[Pi]),
		btmRight+majorSize*majorAxis*0.1*(\[Theta]/2\[Pi]),
		topRight-minorSize*minorAxis*0.1*(\[Theta]/2\[Pi]),
		topLeft-majorSize*majorAxis*0.1*(\[Theta]/2\[Pi])
	}
];


ClearAll[gParamCone];
gParamCone[coneInput_,\[Theta]_]:=Module[
	{origin,radius,cone,coneCenterTheta,coneDeltaTheta,
		arcMinTheta,arcMaxTheta,arcRange,rescaledTheta},
	origin=coneInput["origin"];
	radius=coneInput["radius"];
	cone=coneInput["cone"];
	coneCenterTheta=cone[[1]];
	coneDeltaTheta=cone[[2]];
	arcMinTheta=coneCenterTheta-coneDeltaTheta;
	arcMaxTheta=coneCenterTheta+coneDeltaTheta;
	arcRange=arcMaxTheta-arcMinTheta;
	rescaledTheta=Rescale[\[Theta],{0,2\[Pi]},{arcMinTheta,arcMaxTheta}];
	
	If[arcMinTheta>\[Pi],arcMinTheta=arcMinTheta-2\[Pi]];
	If[arcMaxTheta>\[Pi],arcMaxTheta=arcMaxTheta-2\[Pi]];
	
	{
		(origin+radius*{Cos[rescaledTheta],Sin[rescaledTheta]}),
		origin+(0.1*radius/2\[Pi])*\[Theta]*FromPolarCoordinates[{1,arcMinTheta}],
		origin+(0.1*radius/2\[Pi])*\[Theta]*FromPolarCoordinates[{1,arcMaxTheta}]
	}
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


ClearAll[gParamLobes];
gParamLobes[lobeInput_,\[Theta]_]:=Module[
	{lobeOrigin,inputKeys,viewDir,lightDir,normalDir,halfDir,roughness,
		nol,noh,voh,nov,lobeFunc,
		lightIntensity,shadingPos,
		(*point light related*)
		lightCenter,lightRadius,lightFadeStart,lightFadeEnd},
	inputKeys=Keys[lobeInput];
	(*mandatory inputs*)
	lobeOrigin=lobeInput["origin"];
	shadingPos=lobeOrigin;
	lobeFunc=lobeInput["lobeFunc"];
	(*optional inputs*)
	viewDir=If[MemberQ[inputKeys,"viewDir"],
		Normalize[lobeInput["viewDir"][<|"\[Theta]"->\[Theta]|>]],NaN];
	lightDir=If[MemberQ[inputKeys,"lightDir"],
		Normalize[lobeInput["lightDir"][<|"\[Theta]"->\[Theta]|>]],NaN];
	normalDir=If[MemberQ[inputKeys,"normalDir"],
		Normalize[lobeInput["normalDir"][<|"\[Theta]"->\[Theta]|>]],NaN];
	halfDir=Normalize[viewDir+lightDir];
	roughness=If[MemberQ[inputKeys,"roughness"],lobeInput["roughness"],NaN];
	lightIntensity=If[MemberQ[inputKeys,"lightIntensity"],lobeInput["lightIntensity"],NaN];
	lightCenter=If[MemberQ[inputKeys,"lightCenter"],lobeInput["lightCenter"],NaN];
	lightRadius=If[MemberQ[inputKeys,"lightRadius"],lobeInput["lightRadius"],NaN];
	lightFadeStart=If[MemberQ[inputKeys,"lightFadeStart"],lobeInput["lightFadeStart"],NaN];
	lightFadeEnd=If[MemberQ[inputKeys,"lightFadeEnd"],lobeInput["lightFadeEnd"],NaN];
	nol=Clip[Dot[normalDir,lightDir],{0,1}];
	noh=Clip[Dot[normalDir,halfDir],{0,1}];
	voh=Clip[Dot[viewDir,halfDir],{0,1}];
	nov=Clip[Dot[normalDir,viewDir],{0,1}];
	
	lobeFunc[<|"\[Theta]"->\[Theta],"viewDir"->viewDir,"lightDir"->lightDir,"normalDir"->normalDir,
		"halfDir"->halfDir,"roughness"->roughness,"lightIntensity"->lightIntensity,
		"shadingPos"->shadingPos,"lightCenter"->lightCenter,"lightRadius"->lightRadius,
		"lightFadeStart"->lightFadeStart,"lightFadeEnd"->lightFadeEnd,
		"nol"->nol,"noh"->noh,"voh"->voh,"nov"->nov|>]
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
	{inputKeys,sgFunc,sg,lightCenter,lightRadius,lightFadeStart,lightFadeEnd,
		lightIntensity,shadingPos},
	inputKeys=Keys[input];
	sgFunc=input["sgFunc"];
	lightCenter=input["lightCenter"];
	lightRadius=If[MemberQ[inputKeys,"lightRadius"],input["lightRadius"],NaN];
	lightFadeStart=If[MemberQ[inputKeys,"lightFadeStart"],input["lightFadeStart"],NaN];
	lightFadeEnd=If[MemberQ[inputKeys,"lightFadeEnd"],input["lightFadeEnd"],NaN];
	lightIntensity=input["lightIntensity"];
	shadingPos=input["shadingPos"];
	sg=sgFunc[<|"lightCenter"->lightCenter,"lightRadius"->lightRadius,
		"lightFadeStart"->lightFadeStart,"lightFadeEnd"->lightFadeEnd,
		"lightIntensity"->lightIntensity,"shadingPos"->shadingPos|>];
	gParamSGCommon[shadingPos,sg,\[Theta]]
];


ClearAll[gParamSGGroundShading];
gParamSGGroundShading[input_,\[Theta]_]:=Module[
	{inputKeys,sgLightFunc,sgLight,sgShadingFunc,sgShading,viewDir,lightDir,
		lightCenter,lightRadius,lightIntensity,groundRangeX,shadingPos,shadingDist,
		lightFadeStart,lightFadeEnd,normalDir,roughness,sgNDF,sgClampedCos,
		lightEnergyPercent},
	inputKeys=Keys[input];
	sgLightFunc=input["sgLightFunc"];
	sgShadingFunc=input["sgShadingFunc"];
	lightCenter=input["lightCenter"];
	lightEnergyPercent=If[MemberQ[inputKeys,"lightEnergyPercent"],
							input["lightEnergyPercent"],1];
	lightRadius=If[MemberQ[inputKeys,"lightRadius"],input["lightRadius"],NaN];
	lightFadeStart=If[MemberQ[inputKeys,"lightFadeStart"],input["lightFadeStart"],NaN];
	lightFadeEnd=If[MemberQ[inputKeys,"lightFadeEnd"],input["lightFadeEnd"],NaN];
	lightIntensity=input["lightIntensity"];
	roughness=If[MemberQ[inputKeys,"roughness"],input["roughness"],NaN];
	viewDir=Normalize[If[MemberQ[inputKeys,"viewDir"],input["viewDir"],NaN]];
	groundRangeX=If[MemberQ[inputKeys,"groundRangeX"],input["groundRangeX"],{-\[Pi],\[Pi]}];
	shadingPos={gRemap[\[Theta],{0,2\[Pi]},groundRangeX],0};
	normalDir=Normalize[{0,1}];
	lightDir=Normalize[lightCenter-shadingPos];
	shadingDist=Norm[lightCenter-shadingPos];
	sgLight=sgLightFunc[<|"lightCenter"->lightCenter,"lightRadius"->lightRadius,
		"lightFadeStart"->lightFadeStart,"lightFadeEnd"->lightFadeEnd,
		"lightIntensity"->lightIntensity,"shadingPos"->shadingPos|>];
	sgNDF=If[MemberQ[inputKeys,"sgNDFFunc"],
				input["sgNDFFunc"][<|"roughness"->roughness,"lightDir"->lightDir,
							"viewDir"->viewDir,"normalDir"->normalDir|>],
				{NaN,NaN,NaN}];
				
	sgClampedCos=sgClampedCosine[normalDir,sgLight[[1]]];
	sgShading=sgShadingFunc[<|"sgLight"->sgLight,"lightEnergyPercent"->lightEnergyPercent,
				"sgClampedCos"->sgClampedCos,"sgNDF"->sgNDF,
				"lightRadius"->lightRadius,"shadingDist"->shadingDist,
				"viewDir"->viewDir,"normalDir"->normalDir,"lightDir"->lightDir,
				"roughness"->roughness|>];
	
	{shadingPos[[1]],sgShading}
];


ClearAll[gParamSGGroundShadingWithWalls];
gParamSGGroundShadingWithWalls[input_,\[Theta]_]:=Module[
	{inputKeys,sgLightFunc,sgLight,sgShadingFunc,sgShading,eyePoint,viewDir,lightDir,
		lightCenter,lightRadius,lightIntensity,shadingPos,shadingDist,
		lightFadeStart,lightFadeEnd,normalDir,roughness,sgNDF,sgClampedCos,
		lightEnergyPercent,oldLightSg2D,oldLightSg3D,newLightSg3D,newLightSg2D,
		wallXRange,wallHeights,bentCone,coneTheta2D,coneAperture,coneCapAxis,coneCap,
		energyPercent,areaPercent,sgPercent,sgPercentStrategy},
	inputKeys=Keys[input];
	sgLightFunc=input["sgLightFunc"];
	sgShadingFunc=input["sgShadingFunc"];
	lightCenter=input["lightCenter"];
	lightEnergyPercent=If[MemberQ[inputKeys,"lightEnergyPercent"],
							input["lightEnergyPercent"],1];
	lightRadius=If[MemberQ[inputKeys,"lightRadius"],input["lightRadius"],NaN];
	lightFadeStart=If[MemberQ[inputKeys,"lightFadeStart"],input["lightFadeStart"],NaN];
	lightFadeEnd=If[MemberQ[inputKeys,"lightFadeEnd"],input["lightFadeEnd"],NaN];
	lightIntensity=input["lightIntensity"];
	roughness=If[MemberQ[inputKeys,"roughness"],input["roughness"],NaN];
	wallXRange=input["wallXRange"];
	wallHeights=input["wallHeights"];
	shadingPos={gRemap[\[Theta],{0,2\[Pi]},wallXRange],0};
	eyePoint=If[MemberQ[inputKeys,"eyePoint"],input["eyePoint"],NaN];
	viewDir=Normalize[eyePoint-shadingPos];
	normalDir=Normalize[{0,1}];
	lightDir=Normalize[lightCenter-shadingPos];
	shadingDist=Norm[lightCenter-shadingPos];
	
	(*cone cutting sg light*)
	oldLightSg2D=sgSphereLight[lightCenter,lightFadeStart,
			lightFadeEnd,lightIntensity,shadingPos];
	oldLightSg3D=sgTo3D[oldLightSg2D];
	bentCone=gCreateCone[shadingPos,
		{wallXRange[[1]],wallHeights[[1]]},{wallXRange[[2]],wallHeights[[2]]}];
	coneTheta2D=bentCone[[1]];
	coneAperture=bentCone[[2]];
	coneCapAxis={Cos[coneTheta2D],0,Sin[coneTheta2D]};
	coneCap={coneCapAxis,coneAperture};
	
	sgPercentStrategy=If[MemberQ[inputKeys,"sgPercentStrategy"],
		input["sgPercentStrategy"],1];
	energyPercent=sgCapIntsEnergyPercent[oldLightSg3D,coneCap];
	areaPercent=sgCapIntsAreaPercent[oldLightSg3D,coneCap];
	sgPercent=Which[
		sgPercentStrategy==2,energyPercent,
		sgPercentStrategy==3,areaPercent,
		True,1];
		
	newLightSg2D={oldLightSg2D[[1]],oldLightSg2D[[2]],energyPercent*oldLightSg2D[[3]]};
	 
	(*calculate new sg light*)
	sgLight=sgLightFunc[<|"lightCenter"->lightCenter,"lightRadius"->lightRadius,
		"lightFadeStart"->lightFadeStart,"lightFadeEnd"->lightFadeEnd,
		"lightIntensity"->lightIntensity,"shadingPos"->shadingPos|>];
	sgNDF=If[MemberQ[inputKeys,"sgNDFFunc"],
				input["sgNDFFunc"][<|"roughness"->roughness,"lightDir"->lightDir,
							"viewDir"->viewDir,"normalDir"->normalDir|>],
				{NaN,NaN,NaN}];
				
	sgClampedCos=sgClampedCosine[normalDir,sgLight[[1]]];
	sgShading=sgShadingFunc[<|"sgLight"->sgLight,"lightEnergyPercent"->lightEnergyPercent,
				"sgClampedCos"->sgClampedCos,"sgNDF"->sgNDF,
				"lightRadius"->lightRadius,"shadingDist"->shadingDist|>];
	
	{shadingPos[[1]],sgShading*sgPercent}
];


ClearAll[gParamSGRightWallShading];
gParamSGRightWallShading[input_,\[Theta]_]:=Module[
	{inputKeys,sgLightFunc,sgLight,sgShadingFunc,sgShading,eyePoint,viewDir,lightDir,
		lightCenter,lightRadius,lightIntensity,shadingPos,shadingDist,
		lightFadeStart,lightFadeEnd,normalDir,roughness,sgNDF,sgClampedCos,
		lightEnergyPercent,oldLightSg2D,oldLightSg3D,newLightSg3D,newLightSg2D,
		wallXRange,wallHeights,bentCone,coneTheta2D,coneAperture,coneCapAxis,coneCap,
		energyPercent,areaPercent,sgPercent,sgPercentStrategy},
	inputKeys=Keys[input];
	sgLightFunc=input["sgLightFunc"];
	sgShadingFunc=input["sgShadingFunc"];
	lightCenter=input["lightCenter"];
	bentCone=input["representCone"];
	lightEnergyPercent=If[MemberQ[inputKeys,"lightEnergyPercent"],
							input["lightEnergyPercent"],1];
	lightRadius=If[MemberQ[inputKeys,"lightRadius"],input["lightRadius"],NaN];
	lightFadeStart=If[MemberQ[inputKeys,"lightFadeStart"],input["lightFadeStart"],NaN];
	lightFadeEnd=If[MemberQ[inputKeys,"lightFadeEnd"],input["lightFadeEnd"],NaN];
	lightIntensity=input["lightIntensity"];
	roughness=If[MemberQ[inputKeys,"roughness"],input["roughness"],NaN];
	wallXRange=input["wallXRange"];
	wallHeights=input["wallHeights"];
	shadingPos={wallXRange[[2]],gRemap[\[Theta],{0,2\[Pi]},{0,wallHeights[[2]]}]};
	eyePoint=If[MemberQ[inputKeys,"eyePoint"],input["eyePoint"],NaN];
	viewDir=Normalize[eyePoint-shadingPos];
	normalDir=Normalize[{-1,0}];
	lightDir=Normalize[lightCenter-shadingPos];
	shadingDist=Norm[lightCenter-shadingPos];
	
	(*cone cutting sg light*)
	oldLightSg2D=sgSphereLight[lightCenter,lightFadeStart,
			lightFadeEnd,lightIntensity,shadingPos];
	oldLightSg3D=sgTo3D[oldLightSg2D];
	If[!MemberQ[inputKeys,"representCone"],
		bentCone=gCreateCone[shadingPos,{wallXRange[[1]],wallHeights[[1]]},
					{wallXRange[[2]],wallHeights[[2]]+1}]];
	coneTheta2D=bentCone[[1]];
	coneAperture=bentCone[[2]];
	coneCapAxis={Cos[coneTheta2D],0,Sin[coneTheta2D]};
	coneCap={coneCapAxis,coneAperture};
	
	sgPercentStrategy=If[MemberQ[inputKeys,"sgPercentStrategy"],
		input["sgPercentStrategy"],1];
	energyPercent=sgCapIntsEnergyPercent[oldLightSg3D,coneCap];
	areaPercent=sgCapIntsAreaPercent[oldLightSg3D,coneCap];
	sgPercent=Which[
		sgPercentStrategy==2,energyPercent,
		sgPercentStrategy==3,areaPercent,
		True,1];
		
	newLightSg2D={oldLightSg2D[[1]],oldLightSg2D[[2]],energyPercent*oldLightSg2D[[3]]};
	 
	(*calculate new sg light*)
	sgLight=sgLightFunc[<|"lightCenter"->lightCenter,"lightRadius"->lightRadius,
		"lightFadeStart"->lightFadeStart,"lightFadeEnd"->lightFadeEnd,
		"lightIntensity"->lightIntensity,"shadingPos"->shadingPos|>];
	sgNDF=If[MemberQ[inputKeys,"sgNDFFunc"],
				input["sgNDFFunc"][<|"roughness"->roughness,"lightDir"->lightDir,
							"viewDir"->viewDir,"normalDir"->normalDir|>],
				{NaN,NaN,NaN}];
				
	sgClampedCos=sgClampedCosine[normalDir,sgLight[[1]]];
	sgShading=sgShadingFunc[<|"sgLight"->sgLight,"lightEnergyPercent"->lightEnergyPercent,
				"sgClampedCos"->sgClampedCos,"sgNDF"->sgNDF,
				"lightRadius"->lightRadius,"shadingDist"->shadingDist|>];
	
	{wallXRange[[2]]-sgShading*sgPercent,shadingPos[[2]]}
];


ClearAll[gParamGgxPDF2];
gParamGgxPDF2[input_,\[Theta]_]:=Module[
	{center,m,viewDir},
	center=input[["center"]];
	m=input[["roughness"]];
	viewDir=input[["viewDir"]];
	
	center+gPlotGgxPdf2D[m,viewDir,\[Theta]]
]; 


(*
circles: {center,radius}
dirLines: {start,direction vector}
axisExtent: plot range
*)
gParamPlot[inputs_,imageSize_:Tiny]:=Module[
	{
		inputKeys,collectFunc,complexFunc,customPlots,
		plotList,plotStyles,plotLabels,axisExtent,finalPlots,
		wallElement1,wallStyle1,wallLabel1,
		wallElement2,wallStyle2,wallLabel2,
		p1,w1,w2
	},	
    inputKeys=Keys[inputs];    
	plotList={};
	plotStyles={};
	plotLabels={};
	
	collectFunc[keyName_,paramFunc_]:=Block[
		{elements,element,evaluated,elementKeys,tmpColor,tmpThickness},
		If[MemberQ[inputKeys,keyName],
			elements=inputs[[keyName]];
			For[i=1,i<=Length[elements],i++,
				element=elements[[i]];
				evaluated:=paramFunc[element,\[Theta]];
				(*AppendTo[plotList,paramFunc[element,\[Theta]]];*)
				elementKeys=Keys[element];
				tmpColor=If[MemberQ[elementKeys,"color"],element["color"],LightBrown];
				tmpThickness=If[MemberQ[elementKeys,"thickness"],element["thickness"],0.01];
				
				AppendTo[plotList,evaluated];
				AppendTo[plotStyles,{tmpColor,Thickness[tmpThickness]}];
				AppendTo[plotLabels,If[MemberQ[elementKeys,"label"],element["label"],""]];	
			];
		];
	];

	complexFunc[keyName_,paramFunc_]:=Block[
		{elements,element,elementKeys,tmpColor,tmpThickness,tmpPlot},
		If[MemberQ[inputKeys,keyName],
			elements=inputs[[keyName]];
			For[i=1,i<=Length[elements],i++,
				element=elements[[i]];
				elementKeys=Keys[element];
				tmpColor=If[MemberQ[elementKeys,"color"],element["color"],LightBrown];
				tmpThickness=If[MemberQ[elementKeys,"thickness"],element["thickness"],0.01];
				
				tmpPlot=ParametricPlot[
					paramFunc[element,\[Theta]],
					{\[Theta],0,2\[Pi]},
					PlotStyle->{tmpColor,Thickness[tmpThickness]},
					PlotLegends->If[MemberQ[elementKeys,"label"],element["label"],""],
					PlotRange->{{-axisExtent,axisExtent},{-axisExtent,axisExtent}},
					AspectRatio->1,
					ImageSize->imageSize];
				AppendTo[finalPlots,tmpPlot]
			];
		];
	];
	
	(*append circles*)
	collectFunc["circles",gParamCircle];
	(*append dir lines*)
	collectFunc["lines",gParamLine];
	(*append rectangles*)
	collectFunc["rects",gParamRect];
	(*append cones*)
	collectFunc["cones",gParamCone];
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
	collectFunc["sgGroundShading",gParamSGGroundShading];
	(*append lobes, direction(view or light or normal) might varying with \[Theta]*)
	collectFunc["lobes",gParamLobes];
	(*append GGX PDF 2D*)
	collectFunc["ggxPDF2",gParamGgxPDF2];

	axisExtent=If[MemberQ[inputKeys,"axisExtent"],inputs[["axisExtent"]],5];
	p1=ParametricPlot[
		plotList,
		{\[Theta],0,2\[Pi]},
		PlotStyle->plotStyles,
		PlotLegends->plotLabels,
		PlotRange->{{-axisExtent,axisExtent},{-axisExtent,axisExtent}},
		AspectRatio->1,
		ImageSize->imageSize];
	finalPlots={};
	AppendTo[finalPlots,p1];
	
	(*ground shading with walls*)
	complexFunc["sgGroundShadingWithWalls",gParamSGGroundShadingWithWalls];
	(*wall shading*)
	complexFunc["sgRightWallShading",gParamSGRightWallShading];
	
	customPlots=If[MemberQ[inputKeys,"customPlots"],inputs["customPlots"],{}];
	AppendTo[finalPlots,customPlots];
	
	(*general plot*)
	Show[finalPlots]
];


End[];


EndPackage[];
