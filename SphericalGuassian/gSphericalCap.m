(* ::Package:: *)

BeginPackage["gSphericalCap`"];


gSphericalCap::usage="gSphericalCap";


Begin["`Private`"];


ClearAll[gSpherCap];


gSpherCap[axisDir_,apertureAngle_]:=Module[
	{axisPolar,axisPhi,axisTheta},
	axisPolar=ToPolarCoordinates[axisDir];
	axisPhi=axisPolar[[1]];
	axisTheta=axisTheta[[2]];
	
	{axisPhi,axisTheta,apertureAngle}
];


End[];


EndPackage[];
