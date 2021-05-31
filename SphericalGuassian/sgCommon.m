(* ::Package:: *)

BeginPackage["sgCommon`"];


ClearAll[sgVector];
sgVector::usage="function{sgVector}";
ClearAll[sgPolar];
sgPolar::usage="function{sgPolar}";
ClearAll[sgIntegral];
sgIntegral::usage="function[sgIntegral]";
ClearAll[sgFindMinLambda];
sgFindMinLambda::usage="function[sgFindMinLambda]";
ClearAll[sgMinLambda];
sgMinLambda=4.20341;
ClearAll[sgRawFa];
sgRawFa::usage="All-Frequency.";
ClearAll[sgFa];
sgFa::usage="function[sgFa]";
ClearAll[sgInvFa];
sgInvFa::usage="function[sgInvFa]";


Begin["`Private`"];
On[Assert];


sgVector[v_,{p_,\[Lambda]_,\[Mu]_}]:=Module[
{CosTheta},
CosTheta=Dot[v,p];
Assert[CosTheta>=0];
Assert[\[Lambda]>=sgMinLambda];
\[Mu]*Exp[\[Lambda]*(CosTheta-1)]];


sgPolar[\[Theta]_,\[Lambda]_,\[Mu]_]:=Module[
{},
Assert[-\[Pi]/2<=\[Theta]<=\[Pi]/2];
Assert[\[Lambda]>=sgMinLambda];
\[Mu]*Exp[\[Lambda]*(Cos[\[Theta]]-1)]];


sgIntegral[\[Lambda]_,\[Mu]_:1]:=\[Mu]*2\[Pi]*(1-Exp[-2\[Lambda]])/\[Lambda];


sgFindMinLambda[\[Epsilon]_]:=Module[
{tmp0,tmp1},
tmp0=Quiet@Solve[(sgPolar[\[Pi]/2,\[Lambda],1]/sgIntegral[\[Lambda],1])==\[Epsilon] && \[Lambda]>0,\[Lambda]];
tmp1=tmp0[[All,1,2]];
tmp1[[1]]
];


sgRawFa[\[Lambda]_,\[Epsilon]_]=-2\[Pi]*Log[\[Epsilon]/\[Lambda]];


sgFa[\[Lambda]_]:=sgRawFa[\[Lambda],0.1];


sgInvFa=InverseFunction[sgFa];


End[];


EndPackage[];
