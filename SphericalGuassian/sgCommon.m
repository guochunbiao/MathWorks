(* ::Package:: *)

BeginPackage["sgCommon`"];


ClearAll[sgVector];
sgVector::usage="function{sgVector}";
ClearAll[sgPolar];
sgPolar::usage="function{sgPolar}";
ClearAll[sgPolar2];
sgPolar2::usage="function{sgPolar2}";
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
ClearAll[sgPointLightOld];
sgPointLightOld::usage="function[sgPointLightOld]";
ClearAll[sgPointLightNew];
sgPointLightNew::usage="function[sgPointLightNew]";


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


sgPolar2[\[Theta]_,{p_,\[Lambda]_,\[Mu]_}]:=sgPolar[\[Theta],\[Lambda],\[Mu]];


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


sgPointLightOld[lightCenter_,lightRadius_,lightIntensity_,shadingPos_]:=Module[
	{d,p,\[Lambda],\[Mu]},
	d=Norm[lightCenter-shadingPos];
	p=(lightCenter-shadingPos)/d;
	\[Lambda]=sgInvFa[2\[Pi]*lightRadius^2/d^2];
	\[Mu]=lightIntensity/d^2;
	{p,\[Lambda],\[Mu]}
];


sgPointLightQuadratic[t_,a_,b_,c_]=a*t^2+b*t+c;
sgPointLightNew[lightCenter_,lightRadius_,lightIntensity_,shadingPos_]:=Module[
    {fitParams,d,distPercent,p,\[Lambda],\[Mu]},
    fitParams={
		{0.791375,-1.89057,1.0872,6.66654,-10.8249,6.76642},
		{-0.340146,0.0665636,0.236471,3.79125,0.618155,1.38885},
		{0.385139,-0.87285,0.485253,1.80957,0.308354,2.7173},
		{0.213239,-0.506706,0.291679,1.83615,2.49729,1.51728},
		{0.128335,-0.316743,0.187358,2.15074,3.59944,0.786432}};
	d=Norm[lightCenter-shadingPos];
	distPercent=d/lightRadius;
	p=Normalize[lightCenter-shadingPos];
	Assert[IntegerQ[lightRadius] && 1<=lightRadius<=5];
	\[Lambda]=sgPointLightQuadratic[distPercent,fitParams[[lightRadius]][[4]],
			fitParams[[lightRadius]][[5]],fitParams[[lightRadius]][[6]]];
	\[Mu]=lightIntensity*sgPointLightQuadratic[distPercent,fitParams[[lightRadius]][[1]],
			fitParams[[lightRadius]][[2]],fitParams[[lightRadius]][[3]]];
	{p,\[Lambda],\[Mu]}
];


End[];


EndPackage[];
