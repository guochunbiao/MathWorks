(* ::Package:: *)

BeginPackage["sgCommon`"];
<<gSphericalCap.m;
Needs["gSphericalCap`"];
Needs["gUtils`"];
Needs["gBRDF`"];


ClearAll[sgVector,sgPolar,sgPolar2,sgIntegral,sgIntegral2,sgFindMinLambda,sgMinLambda,sgDot,
		 sgRawFa,sgFa,sgInvFa,sgPointLightOld,sgPointLightNew,sgClampedCosine,sgDiffuseLighting,
		 sgRawNDF,sgRawNDFConvHalfVec,sgNDF,sgNDFConvLight,asgVector,asgPolar,sgTo3D,sgTo2D,
		 solveSgArea,sgArea,solveSgAsCap,sgAsCap,sgCapIntsArea,sgSolveEnergy,sgEnergy,
		 asgArea,sgCapIntsEnergy,sgSolveEnergyInRange,sgEnergyInRange,sgCapIntsAsNewSG,
		 sgSolveAvgEnergyTheta,sgAvgEnergyTheta,sgCapIntsMaxEnergyTheta,sgEnergyCentroidTheta,
		 sgCapsIntsEnergyCentroidTheta,sgCapIntsAsNewSGDeprecated,sgProductIntegral,
		 sgNDFProdIntegrateLight,sgCapIntsEnergyPercent,sgMaxLambda,sgSphereLight,
		 sgCapIntsAreaPercent,sgProduct,sgShading,sgSolveOneBounce1,sgSolveOneBounce2,
		 sgCalcEnergyPercent,sgReflectLight,sgRepresentLambda];
sgVector::usage="function{sgVector}";
sgPolar::usage="function{sgPolar}";
sgPolar2::usage="function{sgPolar2}";
sgIntegral::usage="function[sgIntegral]";
sgIntegral2::usage="function[sgIntegral2]";
sgFindMinLambda::usage="function[sgFindMinLambda]";
sgMinLambda=4.60517;
sgMaxLambda=50;
sgDot::usage="function[sgDot]";
sgProduct::usage="sgProduct";
sgProductIntegral::usage="sgProductIntegral";
sgRawFa::usage="All-Frequency.";
sgFa::usage="function[sgFa]";
sgInvFa::usage="function[sgInvFa]";
sgTo3D::usage="sgTo3D";
sgTo2D::usage="sgTo2D";
sgPointLightOld::usage="function[sgPointLightOld]";
sgPointLightNew::usage="function[sgPointLightNew]"; 
sgClampedCosine::usage="function[sgClampedCosine]";
sgDiffuseLighting::usage="function[sgDiffuseLighting]";
sgRawNDF::usage="function[sgRawNDF]";
sgRawNDFConvHalfVec::usage="function[sgRawNDFConvHalfVec]";
sgNDF::usage="function[sgNDF]";
sgNDFConvLight::usage="function[sgNDFConvLight]";
sgNDFProdIntegrateLight::uasge="sgNDFProdIntegrateLight";
asgVector::usage="function[asgVector]";
asgPolar::usage="function[asgPolar]";
solveSgArea::usage="solveSgArea";
sgArea::usage="sgArea";
asgArea::usage="asgArea";
solveSgAsCap::usage="solveSgAsCap";
sgAsCap::usage="sgAsCap";
sgCapIntsArea::usage="sgCapIntsArea";
sgSolveEnergy::usage="sgSolveEnergy";
sgEnergy::usage="sgEnergy";
sgSolveEnergyInRange::usage="sgSolveEnergyInRange";
sgEnergyInRange::usage="sgEnergyInRange";
sgCalcEnergyPercent::usage="sgCalcEnergyPercent";
sgCapIntsEnergy::usage="sgCapIntsEnergy";
sgCapIntsAsNewSG::usage="sgCapIntsAsNewSG";
sgSolveAvgEnergyTheta::usage="sgSolveAvgEnergyTheta";
sgAvgEnergyTheta::usage="sgAvgEnergyTheta";
sgCapIntsMaxEnergyTheta::usage="sgCapIntsMaxEnergyTheta";
sgEnergyCentroidTheta::usage="sgEnergyCentroidTheta";
sgCapsIntsEnergyCentroidTheta::usage="sgCapsIntsEnergyCentroidTheta";
sgCapIntsAsNewSGDeprecated::usage="sgCapIntsAsNewSGDeprecated";
sgCapIntsEnergyPercent::usage="sgCapIntsEnergyPercent";
sgSphereLight::usage="sgSphereLight";
sgReflectLight::usage="sgReflectLight";
sgCapIntsAreaPercent::usage="sgCapIntsAreaPercent";
sgShading::usage="sgShading";
sgSolveOneBounce1::usage="sgSolveOneBounce1";
sgSolveOneBounce2::usage="sgSolveOneBounce2";
sgRepresentLambda::usage="sgRepresentLambda";


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
(*Assert[-\[Pi]/2<=\[Theta]<=\[Pi]/2];
Assert[\[Lambda]>=sgMinLambda];*)
\[Mu]*Exp[\[Lambda]*(Cos[\[Theta]]-1)]];


sgPolar2[\[Theta]_,{p_,\[Lambda]_,\[Mu]_}]:=sgPolar[\[Theta],\[Lambda],\[Mu]];


sgIntegral[\[Lambda]_,\[Mu]_:1]:=\[Mu]*2\[Pi]*(1-Exp[-2\[Lambda]])/\[Lambda];


sgIntegral2[{p_,\[Lambda]_,\[Mu]_}]:=sgIntegral[\[Lambda],\[Mu]];


sgFindMinLambda[\[Epsilon]_]:=Module[
	{sol1,sol2,min1,min2},
	(*energy*)
	sol1=Quiet@Solve[(sgPolar[\[Pi]/2,\[Lambda],1]/sgPolar[0,\[Lambda],1])==\[Epsilon] && \[Lambda]>0,\[Lambda]];
	min1=sol1[[All,1,2]][[1]];
	(*area*)
	sol2=Quiet@Solve[-2\[Pi]*Log[\[Epsilon]/\[Lambda]]==2\[Pi],\[Lambda]];
	min2=sol2[[All,1,2]][[1]];

	Max[min1,min2]
];


sgDot[{p1_,\[Lambda]1_,\[Mu]1_},{p2_,\[Lambda]2_,\[Mu]2_}]:=
	4\[Pi]*\[Mu]1*\[Mu]2*Sinh[Norm[p1*\[Lambda]1+p2*\[Lambda]2]]/(Exp[\[Lambda]1+\[Lambda]2]*Norm[p1*\[Lambda]1+p2*\[Lambda]2]);


sgProduct[{p1_,\[Lambda]1_,\[Mu]1_},{p2_,\[Lambda]2_,\[Mu]2_}]:=Module[
	{p3,\[Lambda]3,\[Mu]3},
	
	\[Lambda]3=(\[Lambda]1+\[Lambda]2)-(\[Lambda]1*\[Lambda]2/(\[Lambda]1+\[Lambda]2))*(1-Dot[p1,p2]);
	p3=(\[Lambda]1*p1+\[Lambda]2*p2)/\[Lambda]3;
	\[Mu]3=Exp[\[Lambda]3-(\[Lambda]1+\[Lambda]2)];
	
	{p3,\[Lambda]3,\[Mu]3}
];


sgProductIntegral[{inP1_,\[Lambda]1_,\[Mu]1_},{inP2_,\[Lambda]2_,\[Mu]2_}]:=Module[
	{p1,p2,\[Lambda]3,c1,c2},
	p1=Normalize[inP1];
	p2=Normalize[inP2];
	
	c1=\[Lambda]1*\[Lambda]2/(\[Lambda]1+\[Lambda]2);
	c2=Dot[p1,p2];	
	\[Lambda]3=\[Lambda]1+\[Lambda]2-c1*(1-c2);
	
	\[Mu]1*\[Mu]2*(2\[Pi])/\[Lambda]3 Exp[c1*(c2-1)]
];


sgRawFa[\[Lambda]_,\[Epsilon]_]:=-2\[Pi]*Log[\[Epsilon]/\[Lambda]];


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


sgSphereLight[lightCenter_,lightFadeStart_,lightFadeEnd_,lightIntensity_,shadingPos_]:=Module[
	{shandingDist,lambdaFactor,p,\[Lambda],\[Mu]},
	shandingDist=Norm[lightCenter-shadingPos];
	shandingDist=Clip[shandingDist,{lightFadeStart,lightFadeEnd}];
	lambdaFactor=(shandingDist-lightFadeStart)/(lightFadeEnd-lightFadeStart);
	lambdaFactor=Clip[lambdaFactor,{0,1}];
		
	p=Normalize[lightCenter-shadingPos];
	\[Lambda]=gLerp[sgMinLambda,sgMaxLambda,lambdaFactor];
	\[Mu]=lightIntensity;
	
	{p,\[Lambda],\[Mu]}
];


sgReflectLight[repCenter_,inRepNormal_,repRadius_,shadingPt_,
	lightPt_,lightFadeStart_,lightFadeEnd_,lightIntensity_,roughness_]:=Module[
	{
		repNormal,repSG,shadingDist,repLightSG,repNDF,repPeakEnergy,
		repLightDirUnNormed,repLightDir,repViewDirUnNormed,repViewDir,repThetas,
		shadingDistToDisk,lightDistToDisk,shadingProjPt,lightProjPt,
		repSgCenter,repSgLambda,energyPercent,
		tangentView,tangentLight
	},
	
	repNormal=Normalize[inRepNormal];
	
	shadingDist=Norm[shadingPt-repCenter];
	repLightDirUnNormed=lightPt-repCenter;
	repLightDir=Normalize[repLightDirUnNormed];
	repViewDirUnNormed=shadingPt-repCenter;
	repViewDir=Normalize[repViewDirUnNormed];
	
    shadingDistToDisk=Dot[repViewDirUnNormed,repNormal];
    lightDistToDisk=Dot[repLightDirUnNormed,repNormal];
    shadingProjPt=shadingPt-repNormal*shadingDistToDisk;
    lightProjPt=lightPt-repNormal*lightDistToDisk;
    repSgCenter=gLerp[shadingProjPt,lightProjPt,shadingDistToDisk/(shadingDistToDisk+lightDistToDisk)];
	
	repLightSG=sgSphereLight[lightPt,lightFadeStart,lightFadeEnd,lightIntensity,shadingPt];
	repNDF=sgNDF[roughness,repLightDir,repViewDir,repNormal];
	repPeakEnergy=sgNDFConvLight[0,0,repLightSG,repNDF];
	
	tangentView=Tan[ArcCos[Dot[repViewDir,repNormal]]];
	tangentLight=Tan[ArcCos[Dot[repLightDir,repNormal]]];
	
	repSgLambda=sgRepresentLambda[tangentLight,tangentView];
	
	repThetas=gCalcRepresentThetas[repCenter,repRadius,shadingDist,repSgCenter,repSgLambda];
	energyPercent=sgCalcEnergyPercent[{repNormal,repSgLambda,1},repThetas];
	repSG={repNormal,repSgLambda,repPeakEnergy*energyPercent};
	
	{repSgCenter,repSG}
];


sgTo2D[sg_]:=Module[
	{p,\[Lambda],\[Mu],p2d},
	
	p=sg[[1]];
	\[Lambda]=sg[[2]];
	\[Mu]=sg[[3]];
	
	Assert[Length[p]==3];
	p2d={p[[1]],p[[3]]};
	
	{p2d,\[Lambda],\[Mu]}
];


sgTo3D[sg_]:=Module[
	{p,\[Lambda],\[Mu],p3d},
	
	p=sg[[1]];
	\[Lambda]=sg[[2]];
	\[Mu]=sg[[3]];
	
	Assert[Length[p]==2];
	p3d={p[[1]],0,p[[2]]};
	
	{p3d,\[Lambda],\[Mu]}
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
	(*If[shadingDist>=lightRadius,0,sgDot[halfVec,rawSgNdf]]*)
	sgDot[halfVec,rawSgNdf]
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
	(*If[shadingDist>=lightRadius,0,sgDot[sglight,sgndf]]*)
	sgDot[sglight,sgndf]
];


sgShading[sglight_,sgndf_,roughness_,viewDir_,normalDir_,lightDir_]:=Module[
	{normal,light,view,half,NoL,NoV,NoH,VoH,Vis,F},
	normal=Normalize[normalDir];
	light=Normalize[lightDir];
	view=Normalize[viewDir];
	half=Normalize[view+light];
	NoL=Clip[Dot[normal,light],{0,1}];
	NoV=Dot[normal,view];
	NoH=Dot[normal,half];
	VoH=Dot[view,half];
	
	Vis=gVisSmith[roughness,NoL,NoV];
	F=gFresnelOrigin[VoH];
	
	sgNDFConvLight[NaN,NaN,sglight,sgndf]*Vis*F
];


sgNDFProdIntegrateLight[lightRadius_,shadingDist_,sglight_,sgndf_]:=Module[
	{},
	If[shadingDist>=lightRadius,0,Abs[sgProductIntegral[sglight,sgndf]]]
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
	
(*	On[Assert];
	Assert[2\[Lambda]+Log[\[Epsilon]]>0];*)
	
	-2\[Pi]*Log[\[Epsilon]]/\[Lambda]
];



asgArea[\[Lambda]_,\[Mu]_]:=Module[
	{},
	
	\[Pi]/Sqrt[\[Lambda]*\[Mu]]
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
	
	apertAngle=Re[ArcCos[1-sgMinLambda/\[Lambda]]];
	{p,apertAngle}
];



sgSolveEnergy[sg_,\[Epsilon]_:0.01]:=Module[
	{p,\[Lambda],\[Mu],reg,sol},
	p=sg[[1]];
	\[Lambda]=sg[[2]];
	\[Mu]=sg[[3]];
	
	reg=ParametricRegion[{Cos[\[Phi]]*Sin[\[Theta]],Sin[\[Phi]]*Sin[\[Theta]],Cos[\[Theta]]},{{\[Phi],0,2\[Pi]},{\[Theta],0,\[Pi]}}];
	sol=Integrate[\[Mu]*Exp[\[Lambda]*(z-1)],
			{x,y,z}\[Element]reg,Assumptions->{\[Lambda]>0,\[Mu]>0,\[Epsilon]>0}];
	sol
];



sgEnergy[sg_]:=Module[
	{p,\[Lambda],\[Mu]},
	p=sg[[1]];
	\[Lambda]=sg[[2]];
	\[Mu]=sg[[3]];
	
	\[Mu] (2\[Pi])/\[Lambda] (1-E^(-2\[Lambda]))
];


sgSolveEnergyInRange[sg_]:=Module[
	{p,\[Lambda],\[Mu],reg,sol},
	p=sg[[1]];
	\[Lambda]=sg[[2]];
	\[Mu]=sg[[3]];
	
	(*Fast way: Integrate[\[Mu]*Exp[\[Lambda]*(z-1)],{z,z1,z2}, Assumptions\[Rule]{z1>z2}]*)
	(*The following is a slow way*)
	reg=ParametricRegion[{Cos[\[Phi]]*Sin[\[Theta]],Sin[\[Phi]]*Sin[\[Theta]],Cos[\[Theta]]},{{\[Phi],0,2\[Pi]},{\[Theta],\[Pi]/8,\[Pi]/7}}];
	sol=Integrate[\[Mu]*Exp[\[Lambda]*(z-1)],{x,y,z}\[Element]reg,Assumptions->{\[Lambda]>0,\[Mu]>0}];
	sol
];


sgEnergyInRange[sg_,\[Theta]1_,\[Theta]2_,\[Phi]1_,\[Phi]2_]:=Module[
	{p,\[Lambda],\[Mu]},
	p=sg[[1]];
	\[Lambda]=sg[[2]];
	\[Mu]=sg[[3]];
	
	Assert[\[Theta]1<=\[Theta]2];
	Assert[\[Phi]1<=\[Phi]2];
	(\[Mu]/\[Lambda])*(Exp[\[Lambda](Cos[\[Theta]1]-1)]-Exp[\[Lambda](Cos[\[Theta]2]-1)])*(\[Phi]2-\[Phi]1)
];


sgCalcEnergyPercent[sg_,{thetaLowerMin_,thetaLowerMax_,thetaUpper_}]:=Module[
	{totalEnergy,upperEnergy,lowerEnergy,partialEnergy},
	
	totalEnergy=sgEnergy[sg];
	upperEnergy=sgEnergyInRange[sg,0,thetaUpper,0,2\[Pi]];
	Assert[upperEnergy>=0];
	lowerEnergy=sgEnergyInRange[sg,thetaLowerMin,thetaLowerMax,0,\[Pi]];
	Assert[lowerEnergy>=0];
	partialEnergy=lowerEnergy+upperEnergy;
	Assert[partialEnergy<=totalEnergy];
	
	partialEnergy/totalEnergy
];


sgSolveAvgEnergyTheta[sg_,z1_,z2_]:=Module[
	{p,\[Lambda],\[Mu],reg,avgEnergy,sol},
	p=sg[[1]];
	\[Lambda]=sg[[2]];
	\[Mu]=sg[[3]];
	
	avgEnergy=Integrate[\[Mu]*Exp[\[Lambda]*(z-1)],{z,z1,z2}]/(z2-z1);
	sol=Solve[avgEnergy==\[Mu]*Exp[\[Lambda]*(x-1)],x];
	
	sol
];


sgAvgEnergyTheta[sg_,\[Theta]1_,\[Theta]2_]:=Module[
	{p,\[Lambda],\[Mu],avgEnergy,avgZ,avgTheta},
	p=sg[[1]];
	\[Lambda]=sg[[2]];
	\[Mu]=sg[[3]];
	
	Assert[\[Theta]1<\[Theta]2];
	
	avgEnergy=sgEnergyInRange[sg,\[Theta]1,\[Theta]2,0,1]/(\[Theta]2-\[Theta]1);
	avgZ=1+Log[avgEnergy/\[Mu]]/\[Lambda];
	avgTheta=Re[ArcCos[avgZ]];
	
	avgTheta
];


sgCapIntsArea[sg_,spherCap_]:=Module[
	{sgCap},
	sgCap=sgAsCap[sg];
	
	gSpherCapIntsArea[sgCap,spherCap]
];


sgCapIntsEnergy[sg_,spherCap_]:=Module[
	{sgCap,areaInts,hemiAperture,quaterRange,energy0,energy1},
	
	sgCap=sgAsCap[sg];
	
	areaInts=gCapIntsAreas[sgCap[[1]],sgCap[[2]],spherCap[[1]],spherCap[[2]]];
	hemiAperture=areaInts[[1]];
	quaterRange=areaInts[[2]];
	
	energy0=sgEnergyInRange[sg,0,hemiAperture,0,2\[Pi]];
	energy1=sgEnergyInRange[sg,quaterRange[[1]],quaterRange[[2]],quaterRange[[3]],quaterRange[[4]]];

	energy0+energy1
];


sgCapIntsEnergyPercent[sg_,spherCap_]:=Module[
	{partEnergy,totalEnergy},
	
	partEnergy=sgCapIntsEnergy[sg,spherCap];
	totalEnergy=sgEnergy[sg];
	
	Assert[0<=partEnergy<=totalEnergy];
	partEnergy/totalEnergy
];


sgCapIntsAreaPercent[sg_,spherCap_]:=Module[
	{partArea,totalArea},
	
	partArea=sgCapIntsArea[sg,spherCap];
	totalArea=sgArea[sg[[2]]];
	
	Clip[partArea/totalArea,{0,1}]
];


sgCapIntsMaxEnergyTheta[sg_,spherCap_]:=Module[
	{sgCap,edgeInfo,edgePsiL,edgePsiR,minTheta},
	
	sgCap=sgAsCap[sg];
	
	edgeInfo=gSpherCapIntsEdgeInfo[sgCap[[1]],sgCap[[2]],spherCap[[1]],spherCap[[2]]];
	edgePsiL=edgeInfo[[2]];
	edgePsiR=edgeInfo[[3]];
	
	minTheta=Min[Abs[edgePsiL],Abs[edgePsiR]];
	
	If[edgePsiL*edgePsiR<0,0,minTheta]
];


sgEnergyCentroidTheta[sg_,edgePsiL_,edgePsiR_]:=Module[
	{\[Lambda]0,\[Mu]0,minTheta,maxTheta,apexZ1,bNeedTopPart,energyTop2,energyBtm2,
		\[Mu],\[Lambda],z1,z2,z,btmPartTheta,topPartTheta,centroidTheta},
	\[Lambda]0=sg[[2]];
	\[Mu]0=sg[[3]];
	
	minTheta=Min[Abs[edgePsiL],Abs[edgePsiR]];
	maxTheta=Max[Abs[edgePsiL],Abs[edgePsiR]];
	
	bNeedTopPart=(edgePsiL*edgePsiR<0);
	apexZ1=If[Abs[minTheta-maxTheta]<0.001,0,
		Integrate[z*\[Mu]*Exp[\[Lambda]*(z-1)],{z,z1,z2}]/
			Integrate[\[Mu]*Exp[\[Lambda]*(z-1)],{z,z1,z2}]/.{
				\[Mu]->\[Mu]0,\[Lambda]->\[Lambda]0,z1->Cos[minTheta],z2->Cos[maxTheta]}];
	btmPartTheta=Re[ArcCos[apexZ1]];
	
	energyTop2=sgEnergyInRange[sg,0,minTheta,0,2\[Pi]];
	energyBtm2=sgEnergyInRange[sg,minTheta,maxTheta,0,\[Pi]];
	topPartTheta=gLerp[0,btmPartTheta,energyBtm2/(energyTop2+energyBtm2)];
	
	centroidTheta=If[bNeedTopPart,topPartTheta,btmPartTheta];
	
	centroidTheta
];


sgCapsIntsEnergyCentroidTheta[sg_,spherCap_]:=Module[
	{sgCap,edgeInfo,edgePsiL,edgePsiR},
	sgCap=sgAsCap[sg];
	
	edgeInfo=gSpherCapIntsEdgeInfo[sgCap[[1]],sgCap[[2]],spherCap[[1]],spherCap[[2]]];
	edgePsiL=edgeInfo[[2]];
	edgePsiR=edgeInfo[[3]];
	
	sgEnergyCentroidTheta[sg,edgePsiL,edgePsiR]
];


sgCapIntsAsNewSGDeprecated[sg_,spherCap_,strategy_:1]:=Module[
	{\[Mu]0,sgCap,intsArea,intsEnergy,sol,p,\[Lambda],\[Mu]},
	\[Mu]0=sg[[3]];
	sgCap=sgAsCap[sg];
	
	intsArea=sgCapIntsArea[sg,spherCap];
	intsEnergy=sgCapIntsEnergy[sg,spherCap];
	
	p=gApproxCapIntsCentroid[sgCap[[1]],sgCap[[2]],spherCap[[1]],spherCap[[2]]];
	sol=Solve[sgArea[\[Lambda],0.1]==intsArea&&sgEnergy[{p,\[Lambda],\[Mu]}]==intsEnergy,{\[Lambda],\[Mu]}];
	
	\[Lambda]=sol[[All, 1, 2]][[1]];
	\[Mu]=sol[[All, 2, 2]][[1]];
	
	{p,\[Lambda],\[Mu]}
];


sgCapIntsAsNewSG[sg_,spherCap_]:=Module[
	{\[Lambda]0,\[Mu]0,sgCap,intsArea,intsEnergy,p1,p2,sol1,sol2,sol,p,\[Lambda],\[Mu],
	maxEnergyTheta,energyCentroidTheta,apexEnergy,apexTheta,newSgAxis,
	axis1,axis2,axisDot,\[Psi]1,\[Psi]2,factor,centroidAxis,centroidPt,edgeInfo,edgePsiL,edgePsiR},
	axis1=Normalize[sg[[1]]];
	axis2=Normalize[spherCap[[1]]];
	\[Lambda]0=sg[[2]];
	\[Mu]0=sg[[3]];
	sgCap=sgAsCap[sg];
	
	edgeInfo=gSpherCapIntsEdgeInfo[sgCap[[1]],sgCap[[2]],spherCap[[1]],spherCap[[2]]];
	edgePsiL=edgeInfo[[2]];
	edgePsiR=edgeInfo[[3]];
	
	intsArea=sgCapIntsArea[sg,spherCap];
	intsEnergy=sgCapIntsEnergy[sg,spherCap];

	maxEnergyTheta=sgCapIntsMaxEnergyTheta[sg,spherCap];
	energyCentroidTheta=sgCapsIntsEnergyCentroidTheta[sg,spherCap];

	(*sol2=Solve[sgArea[\[Lambda]]\[Equal]intsArea&&sgPolar[maxEnergyTheta,\[Lambda]0,\[Mu]0]\[Equal]sgPolar[0,\[Lambda],\[Mu]],{\[Lambda],\[Mu]}];*)
	sol2=Solve[sgArea[\[Lambda]]==intsArea&&sgEnergy[{p,\[Lambda],\[Mu]}]==intsEnergy,{\[Lambda],\[Mu]}];
	\[Lambda]=sol2[[All, 1, 2]][[1]];
	\[Mu]=sol2[[All, 2, 2]][[1]];
	
	energyCentroidTheta=Min[Abs[energyCentroidTheta],Abs[edgePsiL]];
	energyCentroidTheta=Min[Abs[energyCentroidTheta],Abs[edgePsiR]];

	\[Psi]1=0;
	axisDot=Dot[axis1,axis2];
	\[Psi]2=0+Re[ArcCos[axisDot]];
	factor=Abs[energyCentroidTheta-\[Psi]1]/Abs[\[Psi]2-\[Psi]1];
	centroidAxis=axis1*(1-factor)+axis2*factor;
	centroidPt=Normalize[centroidAxis];

	{centroidPt,\[Lambda],\[Mu]}//N
];


sgSolveOneBounce1[thetaL_,distOffset_]:=Module[
	{ 
	  pG(*ground point which accept reflectance*),
	  pL(*light point*),
	  pR(*represent point at wall which generate reflectance*),
	  dG(*Norm[pG-pR]*),
	  dL(*Norm[pL-pR]*),
	  \[Theta]V(*View: ArcCos[pG-pR]*),
	  \[Theta]VW(*Warped \[Theta] for \[Theta]V*),
	  \[Theta]L(*Light: ArcCos[pL-pR]*),
	  \[Theta]LW(*Warped \[Theta] for \[Theta]L*),
	  sD(*Distance from represent point(wall point)*),
	  aX(*Sin[\[Theta]L]*),
	  \[Phi],
	  \[Phi]W,
	  detL(*Jacobian determinant of light vector*),
	  detH(*Jacobian determinant of half vector*),
	  a1,a2,\[Theta]0,sol
	},
	
	\[Theta]L=thetaL;
	sD=distOffset;
	
	aX=Sin[\[Theta]L];
	\[Theta]LW=ArcTan[Tan[\[Theta]L]*aX/(aX+sD)];
	\[Phi]W=\[Phi];
	detL=Det[D[{\[Theta]LW,\[Phi]W},{{\[Theta]L,\[Phi]}}]];
	
	gPrint["Jacobian of Light SG(Method 1)"];
	Print[TraditionalForm@FullSimplify@detL];
	detL=(sD Sin[\[Theta]L]+Sec[\[Theta]L] (sD+Sin[\[Theta]L]) Tan[\[Theta]L])/(sD^2+2 sD Sin[\[Theta]L]+Tan[\[Theta]L]^2);
	detH=(sD Sin[\[Theta]V]+Sec[\[Theta]V] (sD+Sin[\[Theta]V]) Tan[\[Theta]V])/(sD^2+2 sD Sin[\[Theta]V]+Tan[\[Theta]V]^2);
	
(*	sol=Quiet@FindMinimum[
	  {
		NIntegrate[Abs[detL-sgPolar[ArcTan[sD],a1+a2/Tan[\[Theta]L],1]],{sD,0,1},{\[Theta]L,0.1,\[Pi]/4},
		PrecisionGoal\[Rule]2,AccuracyGoal\[Rule]2, Method->"QuasiMonteCarlo",MaxRecursion->1]
      },
	  {a1,50},{a2,1}
	];*)
	gPrint["Fit jacobian of light vector: a1->-1.322,a2->1.618"];
	
(*	sol=FindMinimum[
		{
			NIntegrate[Abs[detL*detH-sgPolar[ArcTan[sD],a1+a2/(0.01+Tan[\[Theta]L]*Tan[\[Theta]V]),1]],
				{sD,0,1},{\[Theta]L,0.1,\[Pi]/4},{\[Theta]V,0.1,\[Pi]/4},
				PrecisionGoal\[Rule]2,AccuracyGoal\[Rule]2, Method\[Rule]"QuasiMonteCarlo",MaxRecursion\[Rule]1]
        },
		{a1,50},{a2,1},{a3,1}
	];
	Print[sol];*)
	gPrint["Fit jacobian multiplication: a1->49.467,a2->-1.234,a3->72.114"];
	
	Blank[]
];


sgSolveOneBounce2[\[Theta]L_,sD_]:=Module[
	{ 
	  theta2,theta1,ax,t,phi2,phi1,detH,detH2,detH3,detH6,sol,a,b,thetaV,thetaL
	},
	
	theta2=ArcTan[Tan[theta1]*ax/(ax+t)];
	phi2=phi1;
	detH=Det[D[{theta2,phi2},{{theta1,phi1}}]];
	
	detH2=detH/.{ax->Sin[theta1]};
	detH3=FullSimplify@detH2;
	
	gPrint["Jacobian of Light SG(Method 2)"];
	Print[detH3/.{theta1->\[Theta]L,t->sD}];
	
	(*sol=Quiet@FindMinimum[
	{
	NIntegrate[Abs[detH3-sgPolar[ArcTan[t],a+b/Tan[theta1],1]],
	{t,0,1},{theta1,0.1,\[Pi]/4},
	PrecisionGoal\[Rule]2,AccuracyGoal\[Rule]2, Method\[Rule]"QuasiMonteCarlo",MaxRecursion\[Rule]1]
      },
	 {a,50},{b,1}
	];
	Print[sol];*)
	gPrint["Fit jacobian of light vector: a1->-1.638,a2->2.936"];
	
	(*TODO: Copy to a note book and run the fitting*)
(*	ClearAll[thetaV,thetaL,t,a,b,detH6];
	detH6=(Sec[thetaV] (t+Sin[thetaV]) Tan[thetaV])/(t^2+2 t Sin[thetaV]+Tan[thetaV]^2)*(Sec[thetaL] (t+Sin[thetaL]) Tan[thetaL])/(t^2+2 t Sin[thetaL]+Tan[thetaL]^2);
	(*TODO: Copy to a note book and run the fitting*)
	sol=FindMinimum[
	{
		NIntegrate[Abs[detH6-sgPolar[ArcSin[t],a1+a2/(a3+Tan[thetaV]*Tan[thetaL]),1]],
		{t,0,1},{thetaV,0.1,\[Pi]/4},{thetaL,0.1,\[Pi]/4},
		PrecisionGoal\[Rule]1,AccuracyGoal\[Rule]1, Method\[Rule]"QuasiMonteCarlo",MaxRecursion\[Rule]1]
      },
	 {a1,50},{a2,1},{a3,1}
	];
	Print[sol];
	ClearAll[thetaV,thetaL,t,a,b,detH6];*)
	gPrint["Fit jacobian multiplication: a1->-2.102,a2->3.116"];
	
	Blank[]
];


sgRepresentLambda[tangentLight_,tangentView_]:=Module[
	{},
	
	Max[-2.102+3.116/(0.01+tangentLight*tangentView),sgMinLambda]
];


End[];


EndPackage[];