(* ::Package:: *)

BeginPackage["gPlots`"];


ClearAll[gParamCircle];
gParamCircle::usage="function[gParamCircle]";
ClearAll[gParamPlot];
gParamPlot::usage="function{gParamPlot}";


Begin["`Private`"];
On[Assert];


gParamCircle[c_,r_,\[Theta]_]:=c+r*{Cos[\[Theta]],Sin[\[Theta]]};


(*
param1:circles
param2:plot range
*)
gParamPlot:=Module[
	{plotList},
	plotList={};
	(*looping circles*)
	Do[plotList=Append[plotList,gParamCircle[circle[[1]],circle[[2]],\[Theta]]],
	{circle,#circles}];
	ParametricPlot[plotList,{\[Theta],0,2 \[Pi]},
	PlotRange->{{-#axisExtent,#axisExtent},{-#axisExtent,#axisExtent}},
	AspectRatio->1]
]&;


End[];


EndPackage[];
