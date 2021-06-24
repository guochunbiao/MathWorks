(* ::Package:: *)

BeginPackage["gPlots3D`"];
Needs["sgCommon`"];
Needs["gUtils`"];
Needs["gBRDF`"];
Needs["gPlots`"];
Needs["gSphericalCap`"];


ClearAll[gParamPlot3D];
gParamPlot3D::usage="function{gParamPlot3D}";


Begin["`Private`"];


ClearAll[gParamLine3D];
gParamLine3D[input_,globalInput_,x_,y_,z_,\[Phi]_,\[Theta]_]:=Module[
	{startPos,dirVector,length},
	startPos=input["startPos"];
	dirVector=input["dirVec"];
	length=input["length"];
	
	startPos+(length/\[Pi])*\[Theta]*Normalize[dirVector]
];


ClearAll[gParamSpherLine];
gParamSpherLine[input_,globalInput_,x_,y_,z_,\[Phi]_,\[Theta]_]:=Module[
	{inputKeys,c,r,zbias,
		startPolars,endPolars,spherePts,
		i,\[Theta]1,\[Theta]2,\[Phi]1,\[Phi]2,spherLines,phiFactor,thetaFactor,currPhi,currTheta},
	inputKeys=Keys[input];
	c=input["spherCenter"];
	r=input["spherRadius"];
	zbias=If[MemberQ[inputKeys,"zbias"],input["zbias"],0];
	startPolars=input["startPts"];
	endPolars=input["endPts"];
	
	spherePts={};
	
	Assert[Length[startPolars]==Length[endPolars]];
	For[i=1,i<=Length[startPolars],i++,
		\[Theta]1=startPolars[[i]][[1]];
		\[Phi]1=startPolars[[i]][[2]];
		\[Theta]2=endPolars[[i]][[1]];
		\[Phi]2=endPolars[[i]][[2]];
		
		Assert[\[Theta]1<=\[Theta]2];
		Assert[\[Phi]1<=\[Phi]2];
		
		currPhi=If[\[Phi]1<0,\[Phi]-\[Pi],\[Phi]];
		currPhi=Clip[currPhi,{\[Phi]1,\[Phi]2}];
		currTheta=Clip[\[Theta],{\[Theta]1,\[Theta]2}];
		
		AppendTo[spherePts,
			c+r*{Cos[currPhi]*Sin[currTheta],Sin[currPhi]*Sin[currTheta],Cos[currTheta]}
				*(1+zbias*2^-6)]
	];
	
	spherePts
];


ClearAll[gParamSphere];
gParamSphere[input_,globalInput_,x_,y_,z_,\[Phi]_,\[Theta]_]:=Module[
	{inputKeys,c,r,zbias},
	inputKeys=Keys[input];
	c=input["center"];
	r=input["radius"];
	zbias=If[MemberQ[inputKeys,"zbias"],input["zbias"],0];
	
	c+r*{Cos[\[Phi]]*Sin[\[Theta]],Sin[\[Phi]]*Sin[\[Theta]],Cos[\[Theta]]}*(1+zbias*2^-6)
];


ClearAll[gParamSpherRange];
gParamSpherRange[input_,globalInput_,x_,y_,z_,\[Phi]_,\[Theta]_]:=Module[
	{inputKeys,c,r,spherePt,axisDir,minTheta,maxTheta,minPhi,maxPhi,vis,vis2,zbias,
		rebasedSpherePt,rebasedPolarPt,rebasedTheta,rebasedPhi},
	inputKeys=Keys[input];
	c=input["center"];
	r=input["radius"];
	axisDir=input["axisDir"];
	zbias=If[MemberQ[inputKeys,"zbias"],input["zbias"],0];
	
	minTheta=input["rangeTheta"][[1]];
	maxTheta=input["rangeTheta"][[2]];
	minPhi=input["rangePhi"][[1]];
	maxPhi=input["rangePhi"][[2]];
	
	spherePt={Cos[\[Phi]]*Sin[\[Theta]],Sin[\[Phi]]*Sin[\[Theta]],Cos[\[Theta]]};	
	vis=(minTheta<=\[Theta]<=maxTheta&&minPhi<=\[Phi]<=maxPhi);
	vis2=If[minPhi<0,minTheta<=\[Theta]<=maxTheta&&(minPhi+2\[Pi])<=\[Phi]<=2\[Pi],0];
	
	c+Boole[vis||vis2]*r*spherePt*(1+zbias*2^-6)
];


ClearAll[gLocalSpherCapVis];
gLocalSpherCapVis[spherCapInput_,x_,y_,z_,\[Phi]_,\[Theta]_]:=Module[
	{coneDir,coneAperture},
	coneDir=Normalize[spherCapInput["coneDir"]];
	coneAperture=spherCapInput["coneAperture"];
	
	gSpherCapVis[coneDir,coneAperture,\[Phi],\[Theta]]
];


ClearAll[gParamSpherCap];
gParamSpherCap[input_,globalInput_,x_,y_,z_,\[Phi]_,\[Theta]_]:=Module[
	{inputKeys,center,radius,zbias,coneDir,coneAperture,spherePt,deltaCos},
	inputKeys=Keys[input];
	center=input["center"];
	radius=input["radius"];
	zbias=If[MemberQ[inputKeys,"zbias"],input["zbias"],0];
	
	spherePt={Cos[\[Phi]]*Sin[\[Theta]],Sin[\[Phi]]*Sin[\[Theta]],Cos[\[Theta]]};
	center+(radius*gLocalSpherCapVis[input,x,y,z,\[Phi],\[Theta]])*spherePt*(1+zbias*2^-6)
];


ClearAll[gLocalSpherCapBorderVis];
gLocalSpherCapBorderVis[spherCapInput_,x_,y_,z_,\[Phi]_,\[Theta]_]:=Module[
	{coneDir,coneAperture},
	coneDir=Normalize[spherCapInput["coneDir"]];
	coneAperture=spherCapInput["coneAperture"];
	
	gSpherCapBorderVis[coneDir,coneAperture,\[Phi],\[Theta]]
];


ClearAll[gParamSpherCapBorder];
gParamSpherCapBorder[input_,globalInput_,x_,y_,z_,\[Phi]_,\[Theta]_]:=Module[
	{inputKeys,center,radius,zbias,coneDir,coneAperture,spherePt,deltaCos},
	inputKeys=Keys[input];
	center=input["center"];
	radius=input["radius"];
	zbias=If[MemberQ[inputKeys,"zbias"],input["zbias"],0];
	
	spherePt={Cos[\[Phi]]*Sin[\[Theta]],Sin[\[Phi]]*Sin[\[Theta]],Cos[\[Theta]]};
	coneDir=Normalize[input["coneDir"]];
	coneAperture=input["coneAperture"];
	
	center+(radius*gLocalSpherCapBorderVis[input,x,y,z,\[Phi],\[Theta]])*spherePt*(1+zbias*2^-6)
];


ClearAll[gParamSpherSeg];
gParamSpherSeg[input_,globalInput_,x_,y_,z_,\[Phi]_,\[Theta]_]:=Module[
	{inputKeys,center,radius,zbias,segAxis,segH1,segH2,sphereVec},
	inputKeys=Keys[input];
	center=input["center"];
	radius=input["radius"];
	zbias=If[MemberQ[inputKeys,"zbias"],input["zbias"],0];
	segAxis=Normalize[input["segAxis"]];
	segH1=input["segH1"];
	segH2=input["segH2"];
	
	On[Assert];
	Assert[segH1>=0&&segH2<=2&&segH1<segH2];
	
	sphereVec={Cos[\[Phi]]*Sin[\[Theta]],Sin[\[Phi]]*Sin[\[Theta]],Cos[\[Theta]]};
	center+(radius*gSpherSegmentVis[segAxis,segH1,segH2,\[Phi],\[Theta]])*sphereVec*(1+zbias*2^-6)
];


ClearAll[gParamSpherCapInts];
gParamSpherCapInts[input_,globalInput_,x_,y_,z_,\[Phi]_,\[Theta]_]:=Module[
	{inputKeys,capIndex1,capIndex2,zbias,spherCapInputs,spherCapNum,
		capInput1,capInput2,capVis1,capVis2,
		capCenter,capRadius,spherePt},
	inputKeys=Keys[input];
	
	capIndex1=input["spherCapPair"][[1]];
	capIndex2=input["spherCapPair"][[2]];
	zbias=If[MemberQ[inputKeys,"zbias"],input["zbias"],0];
	
	spherCapInputs=globalInput[["spherCaps"]];
	spherCapNum=Length[spherCapInputs];
	On[Assert];
	Assert[capIndex1<=spherCapNum && capIndex2<=spherCapNum];
	
	capInput1=spherCapInputs[[capIndex1]];
	capInput2=spherCapInputs[[capIndex2]];
	capCenter=capInput1[["center"]];
	capRadius=capInput1[["radius"]];
	Assert[capCenter==capInput2[["center"]]];
	Assert[capRadius==capInput2[["radius"]]];
	
	capVis1=gLocalSpherCapVis[capInput1,x,y,z,\[Phi],\[Theta]];
	capVis2=gLocalSpherCapVis[capInput2,x,y,z,\[Phi],\[Theta]];
	
	spherePt={Cos[\[Phi]]*Sin[\[Theta]],Sin[\[Phi]]*Sin[\[Theta]],Cos[\[Theta]]};
	capCenter+(capRadius*capVis1*capVis2)*spherePt*(1+zbias*2^-6)
];


ClearAll[gLocalSpherCapVis2];
gLocalSpherCapVis2[cap_,\[Phi]_,\[Theta]_]:=Module[
	{},

	gSpherCapVis[cap[[1]],cap[[2]],\[Phi],\[Theta]]
];


ClearAll[gParamSpherCapInts2];
gParamSpherCapInts2[input_,globalInput_,x_,y_,z_,\[Phi]_,\[Theta]_]:=Module[
	{inputKeys,capIndex1,capIndex2,zbias,
		cap1,cap2,capVis1,capVis2,
		capCenter,capRadius,spherePt},
	inputKeys=Keys[input];
	
	capCenter=input[["center"]];
	capRadius=input[["radius"]];
	cap1=input["cap1"];
	cap2=input["cap2"];
	zbias=If[MemberQ[inputKeys,"zbias"],input["zbias"],0];
	
	capVis1=gLocalSpherCapVis2[cap1,\[Phi],\[Theta]];
	capVis2=gLocalSpherCapVis2[cap2,\[Phi],\[Theta]];
	
	spherePt={Cos[\[Phi]]*Sin[\[Theta]],Sin[\[Phi]]*Sin[\[Theta]],Cos[\[Theta]]};
	capCenter+(capRadius*capVis1*capVis2)*spherePt*(1+zbias*2^-6)
];


ClearAll[gParamGgxPDF3];
gParamGgxPDF3[input_,globalInput_,x_,y_,z_,\[Theta]_,\[Phi]_]:=Module[
	{center,m,viewDir},
	center=input[["center"]];
	m=input[["roughness"]];
	viewDir=input[["viewDir"]];

	center+gPlotGgxPdf3D[m,viewDir,\[Theta],\[Phi]]
];


ClearAll[gMultiColorFunctionBackup];
gMultiColorFunctionBackup/:(h:(Plot|Plot3D|ParametricPlot|ParametricPlot3D))[
	{fs__},before___,gMultiColorFunctionBackup[cf__],after___]:=
		Show[h[#1,before,
			ColorFunction->#2[[1]],
			PlotStyle->#2[[2]],
			(*BoundaryStyle\[Rule]Green,*)
			PlotPoints->#2[[3]],
			Mesh->#2[[4]],
			after]&@@@Transpose[{{fs},cf}]];


gParamPlot3D[inputs_,imageSize_:Tiny]:=Module[
	{
		inputKeys,collectFunc,plotCmds,
		plotList,thetaOnlyList,plotStyles,plotLabels,
		colorFuncList,opacityList,thicknessList,plotPtsList,meshTypeList,
		axisExtent,projSettings,viewPoint,viewProj
	},
	inputKeys=Keys[inputs];
	plotList={};
	plotStyles={};
	plotLabels={};
	colorFuncList={};
	opacityList={};
	thicknessList={};
	plotPtsList={};
	meshTypeList={};
	thetaOnlyList={};
	
	collectFunc[keyName_,paramFunc_,bThetaOnly_:False]:=Block[
		{elements,element,evaluated,elementKeys,
			tmpColorFunc,tmpOpacity,tmpThickness,tmpPlotPts,tmpMeshType},
		
		If[MemberQ[inputKeys,keyName],
			elements=inputs[[keyName]];
			For[i=1,i<=Length[elements],i++,
				element=elements[[i]];
				evaluated=paramFunc[element,inputs,x,y,z,\[Phi],\[Theta]];
				(*AppendTo[plotList,paramFunc[element,\[Phi],\[Theta]]];*)
				elementKeys=Keys[element];
				tmpColorFunc=If[MemberQ[elementKeys,"colorFunc"],
					element["colorFunc"],Cyan];
				tmpOpacity=If[MemberQ[elementKeys,"opacity"],element["opacity"],1];
				tmpThickness=If[MemberQ[elementKeys,"thickness"],element["thickness"],0.01];
				tmpPlotPts=If[MemberQ[elementKeys,"plotPts"],element["plotPts"],20];
				tmpMeshType=If[MemberQ[elementKeys,"mesh"],element["mesh"],Full];
				
				AppendTo[colorFuncList,tmpColorFunc];
				AppendTo[opacityList,tmpOpacity];
				AppendTo[thicknessList,tmpThickness];
				AppendTo[plotPtsList,tmpPlotPts];
				AppendTo[meshTypeList,tmpMeshType];
				
				AppendTo[thetaOnlyList,bThetaOnly];
				AppendTo[plotList,evaluated];
				AppendTo[plotStyles,{tmpColorFunc,{Opacity[tmpOpacity],Thickness[tmpThickness]},
							tmpPlotPts,tmpMeshType}];
				AppendTo[plotLabels,If[MemberQ[elementKeys,"label"],element["label"],""]];
			];
		];
	];
	
	(*append lines*)
	collectFunc["lines",gParamLine3D,True];
	(*append spheres*)
	collectFunc["spheres",gParamSphere];
	(*append spherical caps*)
	collectFunc["spherCaps",gParamSpherCap];
	(*append spherical cap borders*)
	collectFunc["spherCapBorders",gParamSpherCapBorder];
	(*append intersections of spherical caps*)
	collectFunc["spherCapInts",gParamSpherCapInts];
	collectFunc["spherCapInts2",gParamSpherCapInts2];
	(*append spherical segments*)
	collectFunc["spherSegs",gParamSpherSeg];
	(*append spherical ranges*)
	collectFunc["spherRanges",gParamSpherRange];
	(*append spherical lines*)
	collectFunc["spherLines",gParamSpherLine];
	(*append GGX PDF 3d*)
	collectFunc["ggxPDF3",gParamGgxPDF3];
	
	axisExtent=If[MemberQ[inputKeys,"axisExtent"],inputs[["axisExtent"]],5];
	projSettings=If[MemberQ[inputKeys,"viewPoint"],
			{inputs[["viewPoint"]],"Orthographic"},{{1.3,-2.4,2},"Perspective"}];
	viewPoint=projSettings[[1]];
	viewProj=projSettings[[2]];
(*	ParametricPlot3D[#1,
		{\[Phi],0,2\[Pi]},{\[Theta],0,\[Pi]},
		gMultiColorFunctionBackup[#2],
		ColorFunctionScaling\[Rule]False,
		PlotRange->{{-axisExtent,axisExtent},{-axisExtent,axisExtent},{-axisExtent,axisExtent}},
		AspectRatio->1,
		Lighting->{"Ambient",White},
		ViewPoint->viewPoint,
		ViewProjection->viewProj,
		ImageSize->imageSize]&[plotList,plotStyles]*)
	
	plotCmds={};	
	For[i=1,i<=Length[plotList],i++,
		If[thetaOnlyList[[i]],
			AppendTo[
				plotCmds,
				ParametricPlot3D[plotList[[i]],
					{\[Theta],0,\[Pi]},
					ColorFunction->colorFuncList[[i]],
					PlotStyle->{Opacity[opacityList[[i]]],Thickness[thicknessList[[i]]]},
					(*BoundaryStyle\[Rule]Green,*)
					PlotPoints->plotPtsList[[i]],
					Mesh->meshTypeList[[i]],
					ColorFunctionScaling->False,
					PlotRange->{{-axisExtent,axisExtent},
						{-axisExtent,axisExtent},{-axisExtent,axisExtent}},
					AspectRatio->1,
					Lighting->{"Ambient",White},
					ViewPoint->viewPoint,
					ViewProjection->viewProj,
					ImageSize->imageSize]
			],
			AppendTo[
				plotCmds,
				ParametricPlot3D[plotList[[i]],
					{\[Phi],0,2\[Pi]},{\[Theta],0,\[Pi]},
					ColorFunction->colorFuncList[[i]],
					PlotStyle->{Opacity[opacityList[[i]]],Thickness[thicknessList[[i]]]},
					(*BoundaryStyle\[Rule]Green,*)
					PlotPoints->plotPtsList[[i]],
					Mesh->meshTypeList[[i]],
					ColorFunctionScaling->False,
					PlotRange->{{-axisExtent,axisExtent},
						{-axisExtent,axisExtent},{-axisExtent,axisExtent}},
					AspectRatio->1,
					Lighting->{"Ambient",White},
					ViewPoint->viewPoint,
					ViewProjection->viewProj,
					ImageSize->imageSize]
			]
		];
	];
(*	For[i=1,i\[LessEqual]Length[plotList],i++,
		AppendTo[
			plotCmds,
			ParametricPlot3D[plotList[[i]],
				{\[Phi],0,2\[Pi]},{\[Theta],0,\[Pi]},
				ColorFunction\[Rule]Function[Green],
				PlotStyle->{Opacity[1],Thickness[0.02]},
				PlotPoints\[Rule]20,
				ViewPoint->viewPoint,
				ViewProjection->viewProj,
				ImageSize->imageSize]
		];
	];
	For[i=1,i\[LessEqual]Length[plotList2],i++,
		AppendTo[
			plotCmds,
			ParametricPlot3D[plotList2[[i]],
				{\[Theta],0,\[Pi]},
				ColorFunction\[Rule]Function[Blue],
				PlotStyle->{Opacity[1],Thickness[0.02]},
				PlotPoints\[Rule]20,
				ViewPoint->viewPoint,
				ViewProjection->viewProj,
				ImageSize->imageSize]
		];
	];*)
	
	Show[plotCmds]
];


End[];


EndPackage[];
