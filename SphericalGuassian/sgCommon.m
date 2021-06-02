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
ClearAll[sgIntegral2];
sgIntegral2::usage="function[sgIntegral2]";
ClearAll[sgFindMinLambda];
sgFindMinLambda::usage="function[sgFindMinLambda]";
ClearAll[sgMinLambda];
sgMinLambda=4.20341;
ClearAll[sgDot];
sgDot::usage="function[sgDot]";
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
ClearAll[sgClampedCosine];
sgClampedCosine::usage="function[sgClampedCosine]";
ClearAll[sgDiffuseLighting];
sgDiffuseLighting::usage="function[sgDiffuseLighting]";
ClearAll[sgRawNDF];
sgRawNDF::usage="function[sgRawNDF]";
ClearAll[sgRawNDFConvHalfVec];
sgRawNDFConvHalfVec::usage="function[sgRawNDFConvHalfVec]";
ClearAll[sgNDF];
sgNDF::usage="function[sgNDF]";
ClearAll[sgNDFConvLight];
sgNDFConvLight::usage="function[sgNDFConvLight]";


Begin["`Private`"];
On[Assert];


sgVector[v_,{p_,\[Lambda]_,\[Mu]_}]:=Module[
{CosTheta},
CosTheta=Dot[v,p];
(*
Assert[CosTheta>=0];
Assert[\[Lambda]>=sgMinLambda];
*)
If[CosTheta<0||\[Lambda]<sgMinLambda,0,\[Mu]*Exp[\[Lambda]*(CosTheta-1)]]
];


sgPolar[\[Theta]_,\[Lambda]_,\[Mu]_]:=Module[
{},
Assert[-\[Pi]/2<=\[Theta]<=\[Pi]/2];
Assert[\[Lambda]>=sgMinLambda];
\[Mu]*Exp[\[Lambda]*(Cos[\[Theta]]-1)]];


sgPolar2[\[Theta]_,{p_,\[Lambda]_,\[Mu]_}]:=sgPolar[\[Theta],\[Lambda],\[Mu]];


sgIntegral[\[Lambda]_,\[Mu]_:1]:=\[Mu]*2\[Pi]*(1-Exp[-2\[Lambda]])/\[Lambda];


sgIntegral2[{p_,\[Lambda]_,\[Mu]_}]:=sgIntegral[\[Lambda],\[Mu]];


sgFindMinLambda[\[Epsilon]_]:=Module[
{tmp0,tmp1},
tmp0=Quiet@Solve[(sgPolar[\[Pi]/2,\[Lambda],1]/sgIntegral[\[Lambda],1])==\[Epsilon] && \[Lambda]>0,\[Lambda]];
tmp1=tmp0[[All,1,2]];
tmp1[[1]]
];


sgDot[{p1_,\[Lambda]1_,\[Mu]1_},{p2_,\[Lambda]2_,\[Mu]2_}]:=
	4\[Pi]*\[Mu]1*\[Mu]2*Sinh[Norm[p1*\[Lambda]1+p2*\[Lambda]2]]/(Exp[\[Lambda]1+\[Lambda]2]*Norm[p1*\[Lambda]1+p2*\[Lambda]2]);


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


sgPointLightNew[lightCenter_,lightRadius_,lightIntensity_,shadingPos_]:=Module[
    {fitParams,d,distPercent,p,\[Lambda],\[Mu],sgPointLightQuadratic},
    fitParams={
		{0.791375,-1.89057,1.0872,6.66654,-10.8249,6.76642},
		{-0.340146,0.0665636,0.236471,3.79125,0.618155,1.38885},
		{0.385139,-0.87285,0.485253,1.80957,0.308354,2.7173},
		{0.213239,-0.506706,0.291679,1.83615,2.49729,1.51728},
		{0.128335,-0.316743,0.187358,2.15074,3.59944,0.786432}};
	
	sgPointLightQuadratic[t_,a_,b_,c_]:=a*t^2+b*t+c;
	
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


sgClampedCosine[noramlDir_,lightDir_]:={lightDir,2.01906,1.077094};


sgDiffuseLighting[{p1_,\[Lambda]1_,\[Mu]1_},{p2_,\[Lambda]2_,\[Mu]2_},diffuseCol_:1]:=
					(diffuseCol/\[Pi])*sgDot[{p1,\[Lambda]1,\[Mu]1},{p2,\[Lambda]2,\[Mu]2}];


(*Raw(no warping) NDF*)
sgRawNDF[roughness_,normalDir_]:=Module[
	{p,\[Lambda],\[Mu]},
	p=normalDir;
	\[Lambda]=2/roughness^2;
	\[Mu]=1/(\[Pi]*roughness^2);
	{p,\[Lambda],\[Mu]}
];


sgRawNDFConvHalfVec[lightRadius_,shadingDist_,halfVec_,rawSgNdf_]:=Module[
	{},
	If[shadingDist>=lightRadius,0,sgDot[halfVec,rawSgNdf]]
];


(*Warped GGX NDF*)
sgNDF[roughness_,lightDir_,viewDir_,normalDir_]:=Module[
	{halfDir,reflectDir,jacobian,p,\[Lambda],\[Mu]},
	halfDir=Normalize[(lightDir+viewDir)/2];
	reflectDir=2Dot[viewDir,normalDir]*normalDir-viewDir;
	jacobian=Max[4Dot[halfDir,viewDir],0.001];
	p=reflectDir;
	\[Lambda]=(2/roughness^2)/jacobian;
	\[Mu]=1/(\[Pi]*roughness^2);
	{p,\[Lambda],\[Mu]}
];


sgNDFConvLight[lightRadius_,shadingDist_,sglight_,sgndf_]:=Module[
	{},
	If[shadingDist>=lightRadius,0,sgDot[sglight,sgndf]]
];


End[];


EndPackage[];
