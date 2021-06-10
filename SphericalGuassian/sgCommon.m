(* ::Package:: *)

BeginPackage["sgCommon`"];
Needs["gSphericalCap`"];


ClearAll[sgVector,sgPolar,sgPolar2,sgIntegral,sgIntegral2,sgFindMinLambda,sgMinLambda,sgDot,
		 sgRawFa,sgFa,sgInvFa,sgPointLightOld,sgPointLightNew,sgClampedCosine,sgDiffuseLighting,
		 sgRawNDF,sgRawNDFConvHalfVec,sgNDF,sgNDFConvLight,asgVector,asgPolar,
		 solveSgArea,sgArea,solveSgAsCap,sgAsCap,sgAreaCapInts,sgEnergy];
sgVector::usage="function{sgVector}";
sgPolar::usage="function{sgPolar}";
sgPolar2::usage="function{sgPolar2}";
sgIntegral::usage="function[sgIntegral]";
sgIntegral2::usage="function[sgIntegral2]";
sgFindMinLambda::usage="function[sgFindMinLambda]";
sgMinLambda=4.60517;
sgDot::usage="function[sgDot]";
sgRawFa::usage="All-Frequency.";
sgFa::usage="function[sgFa]";
sgInvFa::usage="function[sgInvFa]";
sgPointLightOld::usage="function[sgPointLightOld]";
sgPointLightNew::usage="function[sgPointLightNew]"; 
sgClampedCosine::usage="function[sgClampedCosine]";
sgDiffuseLighting::usage="function[sgDiffuseLighting]";
sgRawNDF::usage="function[sgRawNDF]";
sgRawNDFConvHalfVec::usage="function[sgRawNDFConvHalfVec]";
sgNDF::usage="function[sgNDF]";
sgNDFConvLight::usage="function[sgNDFConvLight]";
asgVector::usage="function[asgVector]";
asgPolar::usage="function[asgPolar]";
solveSgArea::usage="solveSgArea";
sgArea::usage="sgArea";
solveSgAsCap::usage="solveSgAsCap";
sgAsCap::usage="sgAsCap";
sgAreaCapInts::usage="sgAreaCapInts";
sgEnergy::usage="sgEnergy";


Begin["`Private`"];
On[Assert];


sgVector[v_,{p_,\[Lambda]_,\[Mu]_}]:=Module[
{CosTheta},
CosTheta=Dot[Normalize[v],Normalize[p]];
(*
Assert[CosTheta>=0];
Assert[\[Lambda]>=sgMinLambda];
*)
(*If[CosTheta<0(*||\[Lambda]<sgMinLambda*),0,\[Mu]*Exp[\[Lambda]*(CosTheta-1)]]*)
\[Mu]*Exp[\[Lambda]*(CosTheta-1)]
];


(*
	z:lobe axis
	x:tagent axis
	y:bi-tangent axis
	\[Lambda]:bandwidth for x-axis
	\[Mu]:bandwidth for y-axis
	c:lobe amplitude
*)
asgVector[v_,{lobeAxis_,tangentAxis_,bitangentAxis_},
	{xBandwidth_,yBandwidth_},amplitude_]:=Module[
	{z,x,y,\[Lambda],\[Mu],c,Svz},
	z=Normalize[lobeAxis];
	x=Normalize[tangentAxis];
	y=Normalize[bitangentAxis];
	\[Lambda]=xBandwidth;
	\[Mu]=yBandwidth;
	c=amplitude;
	
	Svz=Max[Dot[v,z],0];
	c*Svz*Exp[-\[Lambda]*(Dot[v,x])^2-\[Mu]*(Dot[v,y])^2]
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
	{sol1,sol2,min1,min2},
	(*energy*)
	sol1=Quiet@Solve[(sgPolar[\[Pi]/2,\[Lambda],1]/sgIntegral[\[Lambda],1])==\[Epsilon] && \[Lambda]>0,\[Lambda]];
	min1=sol1[[All,1,2]][[1]];
	(*area*)
	sol2=Quiet@Solve[-2\[Pi]*Log[0.01]/\[Lambda]==2\[Pi],\[Lambda]];
	min2=sol2[[All,1,2]][[1]];

	Max[min1,min2]
];


sgDot[{p1_,\[Lambda]1_,\[Mu]1_},{p2_,\[Lambda]2_,\[Mu]2_}]:=
	4\[Pi]*\[Mu]1*\[Mu]2*Sinh[Norm[p1*\[Lambda]1+p2*\[Lambda]2]]/(Exp[\[Lambda]1+\[Lambda]2]*Norm[p1*\[Lambda]1+p2*\[Lambda]2]);


sgRawFa[\[Lambda]_,\[Epsilon]_]=-2\[Pi]*Log[\[Epsilon]/\[Lambda]];


sgFa[\[Lambda]_]:=sgRawFa[\[Lambda],0.1];


sgInvFa=InverseFunction[sgFa];


sgPointLightOld[lightCenter_,lightRadius_,lightIntensity_,shadingPos_]:=Module[
	{d,p,\[Lambda],\[Mu]},\[AliasDelimiter]
	d=Norm[lightCenter-shadingPos];
	p=(lightCenter-shadingPos)/d;
	\[Lambda]=sgInvFa[2\[Pi]*lightRadius^2/d^2];
	\[Mu]=lightIntensity/d^2;
	{p,\[Lambda],\[Mu]}
];


sgPointLightNew[lightCenter_,lightRadius_,lightIntensity_,shadingPos_]:=Module[
    {fitParams,d,distPercent,p,\[Lambda],\[Mu],sgPointLightQuadratic,tmpMu},
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
	tmpMu=lightIntensity*sgPointLightQuadratic[distPercent,fitParams[[lightRadius]][[1]],
			fitParams[[lightRadius]][[2]],fitParams[[lightRadius]][[3]]];
	\[Mu]=If[distPercent>=1,0,tmpMu];
	
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



(*\[Piecewise]	4 \[Pi]	2 \[Lambda]+Log[\[Epsilon]]\[LessEqual]0
-((2 \[Pi] Log[\[Epsilon]])/\[Lambda])	2 \[Lambda]+Log[\[Epsilon]]>0
0	True

*)
solveSgArea[\[Lambda]_,\[Epsilon]_:0.01]:=Module[
	{\[Mu],reg,sol},
	\[Mu]=1;
	
	reg=ParametricRegion[{Cos[\[Phi]]*Sin[\[Theta]],Sin[\[Phi]]*Sin[\[Theta]],Cos[\[Theta]]},{{\[Phi],0,2 \[Pi]},{\[Theta],0,\[Pi]}}];
	sol=Integrate[Boole[\[Mu]*Exp[\[Lambda]*(z-1)]>=\[Epsilon]],{x,y,z}\[Element]reg,Assumptions->{\[Lambda]>0,\[Mu]>0,\[Epsilon]>0}];
	sol
];



sgArea[\[Lambda]_,\[Epsilon]_:0.01]:=Module[
	{},
	
	On[Assert];
	Assert[2\[Lambda]+Log[\[Epsilon]]>0];
	
	-2\[Pi]*Log[\[Epsilon]]/\[Lambda]
];



solveSgAsCap[\[Lambda]_,\[Epsilon]_:0.01]:=Module[
	{area,sol},
	
	area=sgArea[\[Lambda],\[Epsilon]];
	sol=Assuming[apertAngle>0,FullSimplify@Solve[gSpherCapArea[apertAngle]==area,apertAngle]];
	sol
];



sgAsCap[sg_]:=Module[
	{p,\[Lambda],\[Mu],apertAngle},
	p=sg[[1]];
	\[Lambda]=sg[[2]];
	\[Mu]=sg[[3]];
	
	apertAngle=ArcCos[1-sgMinLambda/\[Lambda]];
	{p,apertAngle}
];



sgEnergy[sg_,\[Epsilon]_:0.01]:=Module[
	{p,\[Lambda],\[Mu],reg,sol},
	p=sg[[1]];
	\[Lambda]=sg[[2]];
	\[Mu]=sg[[3]];
	
	reg=ParametricRegion[{Cos[\[Phi]]*Sin[\[Theta]],Sin[\[Phi]]*Sin[\[Theta]],Cos[\[Theta]]},{{\[Phi],0,2 \[Pi]},{\[Theta],0,\[Pi]}}];
	sol=Integrate[\[Mu]*Exp[\[Lambda]*(z-1)],
			{x,y,z}\[Element]reg,Assumptions->{\[Lambda]>0,\[Mu]>0,\[Epsilon]>0}];
	sol
];



sgAreaCapInts[sg_,spherCap_,\[Epsilon]_:0.01]:=Module[
	{p,\[Lambda],\[Mu],capAxis,capApert,reg,sol},
	p=sg[[1]];
	\[Lambda]=sg[[2]];
	\[Mu]=sg[[3]];
	capAxis=spherCap[[1]];
	capApert=spherCap[[2]];
	{}
];


End[];


EndPackage[];
