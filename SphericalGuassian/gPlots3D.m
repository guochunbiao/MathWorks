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


ClearAll[gParamSphere];
gParamSphere[input_,globalInput_,x_,y_,z_,\[Phi]_,\[Theta]_]:=Module[
	{c,r},
	c=input["center"];
	r=input["radius"];
	
	c+r*{Cos[\[Phi]]*Sin[\[Theta]],Sin[\[Phi]]*Sin[\[Theta]],Cos[\[Theta]]}
];


ClearAll[gSpherCapVis2];
gSpherCapVis2[spherCapInput_,x_,y_,z_,\[Phi]_,\[Theta]_]:=Module[
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
	center+(radius*gSpherCapVis2[input,x,y,z,\[Phi],\[Theta]])*spherePt*(1+zbias*2^-6)
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
	
	capVis1=gSpherCapVis2[capInput1,x,y,z,\[Phi],\[Theta]];
	capVis2=gSpherCapVis2[capInput2,x,y,z,\[Phi],\[Theta]];
	
	spherePt={Cos[\[Phi]]*Sin[\[Theta]],Sin[\[Phi]]*Sin[\[Theta]],Cos[\[Theta]]};
	capCenter+(capRadius*capVis1*capVis2)*spherePt*(1+zbias*2^-6)
];


ClearAll[gMultiColorFunction];
gMultiColorFunction/:(h:(Plot|Plot3D|ParametricPlot|ParametricPlot3D))[
	{fs__},before___,gMultiColorFunction[cf__],after___]:=
		Show[h[#1,before,
			ColorFunction->#2[[1]],
			PlotStyle->#2[[2]],
			PlotPoints->#2[[3]],
			Mesh->#2[[4]],
			after]&@@@Transpose[{{fs},cf}]];


gParamPlot3D[inputs_,imageSize_:Tiny]:=Module[
	{
		inputKeys,collectFunc,
		plotList,colorFuncs,plotLabels,
		axisExtent,viewPoint
	},
	inputKeys=Keys[inputs];
	plotList={};
	colorFuncs={};
	plotLabels={};
	
	collectFunc[keyName_,paramFunc_]:=Block[
		{elements,element,evaluated,elementKeys,
			tmpColorFunc,tmpOpacity,tmpPlotPts,tmpMeshType},
		
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
				tmpPlotPts=If[MemberQ[elementKeys,"plotPts"],element["plotPts"],20];
				tmpMeshType=If[MemberQ[elementKeys,"mesh"],element["mesh"],Full];
				
				AppendTo[plotList,evaluated];
				AppendTo[colorFuncs,{tmpColorFunc,Opacity[tmpOpacity],tmpPlotPts,tmpMeshType}];
				AppendTo[plotLabels,If[MemberQ[elementKeys,"label"],element["label"],""]];
			];
		];
	];
	
	(*append lines*)
	collectFunc["lines",gParamLine3D];
	(*append spheres*)
	collectFunc["spheres",gParamSphere];
	(*apppend spherical caps*)
	collectFunc["spherCaps",gParamSpherCap];
	(*apppend intersections of spherical caps*)
	collectFunc["spherCapInts",gParamSpherCapInts];
	
	axisExtent=If[MemberQ[inputKeys,"axisExtent"],inputs[["axisExtent"]],5];	
	viewPoint=If[MemberQ[inputKeys,"viewPoint"],inputs[["viewPoint"]],{1.3,-2.4,2}];	
	ParametricPlot3D[#1,
		{\[Phi],0,2\[Pi]},{\[Theta],0,\[Pi]},
		gMultiColorFunction[#2],
		ColorFunctionScaling->False,
		PlotRange->{{-axisExtent,axisExtent},{-axisExtent,axisExtent},{-axisExtent,axisExtent}},
		AspectRatio->1,
		Lighting->{"Ambient",White},
		ViewPoint->viewPoint,
		ImageSize->imageSize]&[plotList,colorFuncs]
];


End[];


EndPackage[];
