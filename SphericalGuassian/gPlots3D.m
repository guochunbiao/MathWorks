(* ::Package:: *)

BeginPackage["gPlots3D`"];
Needs["sgCommon`"];
Needs["gUtils`"];
Needs["gBRDF`"];
Needs["gPlots`"];
Needs["gSphericalCap`"];


ClearAll[gParamPlot3D];
gParamPlot3D::usage="function{gParamPlot3D}";
gParamSphere::usage="aaa";


Begin["`Private`"];


ClearAll[gParamSphere];
gParamSphere[input_,\[Phi]_,\[Theta]_]:=Module[
	{c,r},
	c=input["center"];
	r=input["radius"];
	
	c+r*{Cos[\[Phi]]*Sin[\[Theta]],Sin[\[Phi]]*Sin[\[Theta]],Cos[\[Theta]]}
];


ClearAll[gMultiColorFunction];
gMultiColorFunction/:(h:(Plot|Plot3D|ParametricPlot|ParametricPlot3D))[
	{fs__},before___,gMultiColorFunction[cf__],after___]:=
		Show[h[#1,before,
			ColorFunction->#2[[1]],
			PlotStyle->#2[[2]],
			after]&@@@Transpose[{{fs},cf}]];


gParamPlot3D[inputs_,plotPts_:20]:=Module[
	{
		inputKeys,collectFunc,
		plotList,colorFuncs,plotLabels,
		axisExtent
	},
	inputKeys=Keys[inputs];
	plotList={};
	colorFuncs={};
	plotLabels={};
	
	collectFunc[keyName_,paramFunc_]:=Block[
		{elements,element,evaluated,elementKeys,
			tmpColorFunc,tmpOpacity},
		
		If[MemberQ[inputKeys,keyName],
			elements=inputs[[keyName]];
			For[i=1,i<=Length[elements],i++,
				element=elements[[i]];
				evaluated=paramFunc[element,\[Phi],\[Theta]];
				(*AppendTo[plotList,paramFunc[element,\[Phi],\[Theta]]];*)
				elementKeys=Keys[element];
				tmpColorFunc=If[MemberQ[elementKeys,"colorFunc"],
					element["colorFunc"],Function[{\[Phi],\[Theta]},Cyan]];
				tmpOpacity=If[MemberQ[elementKeys,"opacity"],element["opacity"],1];
				
				AppendTo[plotList,evaluated];
				AppendTo[colorFuncs,{tmpColorFunc,Opacity[tmpOpacity]}];
				AppendTo[plotLabels,If[MemberQ[elementKeys,"label"],element["label"],""]];
			];
		];
	];
	
	(*append spheres*)
	collectFunc["spheres",gParamSphere];
	
	axisExtent=If[MemberQ[inputKeys,"axisExtent"],inputs[["axisExtent"]],5];	
	ParametricPlot3D[#1,
		{\[Phi],0,2\[Pi]},{\[Theta],0,\[Pi]},
		gMultiColorFunction[#2],
		ColorFunctionScaling->False,
		PlotRange->{{-axisExtent,axisExtent},{-axisExtent,axisExtent},{-axisExtent,axisExtent}},
		PlotPoints->plotPts,
		Mesh->None,
		AspectRatio->1]&[plotList,colorFuncs]
];


End[];


EndPackage[];
