(* ::Package:: *)

BeginPackage["gSphericalCap`"];


ClearAll[gSpherCap,gSolveCapIntsPoints,gSpherCapVis,gSpherCapIntsVis,gSpherCapRegion,
		gSolveSpherCapIntsCentroid,gSolveSpherCapArea,gSpherCapIntsArea,gSpherCapVis2,
		gSpherCapIntsVis2,gSpherCapIntsVis3,gSpherCapIntsAreaError,gSpherSegmentVis,gSpherCapIntsEdgeInfo,
		gSpherCapIntsBaseRefs,gSpherCapIntsRegionVis,gApproxCapIntsCentroid];
gSphericalCap::usage="gSphericalCap";
gSolveCapIntsPoints::usage="gSolveCapIntsPoints";
gSpherCapVis::usage="gSpherCapVis";
gSpherCapVis2::usage="gSpherCapVis2";
gSpherCapIntsVis::usage="gSpherCapIntsVis";
gSpherCapIntsVis2::usage="gSpherCapIntsVis2";
gSpherCapIntsVis3::usage="gSpherCapIntsVis3";
gSpherCapRegion::usage="gSpherCapRegion";
gSolveSpherCapIntsCentroid::usage="gSolveSpherCapIntsCentroid";
gApproxCapIntsCentroid::usage="gApproxCapIntsCentroid";
gSpherCapArea::usage="gSpherCapArea";
gSpherCapIntsArea::usage="gSpherCapIntsArea";
gSpherCapIntsAreaError::usage="gSpherCapIntsAreaError";
gSpherCapIntsEdgeInfo::usage="gSpherCapIntsEdgeInfo";
gSpherCapIntsBaseRefs::usage="gSpherCapIntsBaseRefs";
gSpherCapIntsRegionVis::usage="gSpherCapIntsRegionVis";


Begin["`Private`"];


gSpherCap[axisDir_,apertureAngle_]:=Module[
	{axisPolar,axisPhi,axisTheta},
	axisPolar=ToPolarCoordinates[axisDir];
	axisPhi=axisPolar[[1]];
	axisTheta=axisTheta[[2]];
	
	{axisPhi,axisTheta,apertureAngle}
];


gSpherCapArea[coneAperture_]:=Module[
	{},
	
	2\[Pi]*(1-Cos[coneAperture])
]


(*https://math.stackexchange.com/questions/14036/intersection-of-spherical-caps?rq=1*)
gSolveCapIntsPoints[inN1_,\[Theta]1_,inN2_,\[Theta]2_]:=Module[
    {n1,n2,x,y,z,tmp1,tmp2},
    n1=Normalize[inN1];
    n2=Normalize[inN2];

    tmp1=N@Solve[x^2+y^2+z^2==1&&Dot[{x,y,z},n1]==Cos[\[Theta]1]&&Dot[{x,y,z},n2]==Cos[\[Theta]2],{x,y,z},Reals];
    tmp2=Transpose[tmp1];
    {tmp2[[All,1,2]],tmp2[[All,2,2]]}
];


gSpherCapVis[coneDir_,coneAperture_,\[Phi]_,\[Theta]_]:=Module[
	{spherePt,deltaCos},
	spherePt={Cos[\[Phi]]*Sin[\[Theta]],Sin[\[Phi]]*Sin[\[Theta]],Cos[\[Theta]]};
	deltaCos=Dot[spherePt,Normalize[coneDir]];
	
	Boole[deltaCos>=Cos[coneAperture]]
];


gSpherCapVis2[coneDir_,coneAperture_,spherePt_]:=Module[
	{deltaCos},
	deltaCos=Dot[spherePt,Normalize[coneDir]];
	
	Boole[deltaCos>=Cos[coneAperture]]
];


gSpherCapIntsVis[coneDir1_,coneAperture1_,coneDir2_,coneAperture2_,\[Phi]_,\[Theta]_]:=Module[
	{vis1,vis2},
	vis1=gSpherCapVis[coneDir1,coneAperture1,\[Phi],\[Theta]];
	vis2=gSpherCapVis[coneDir2,coneAperture2,\[Phi],\[Theta]];
	
	vis1*vis2
];


gSpherCapIntsVis2[coneDir1_,coneAperture1_,coneDir2_,coneAperture2_,spherePt_]:=Module[
	{vis1,vis2},
	vis1=gSpherCapVis2[coneDir1,coneAperture1,spherePt];
	vis2=gSpherCapVis2[coneDir2,coneAperture2,spherePt];
	
	vis1*vis2
];


gSpherCapIntsVis3[coneAperture1_,coneDirCosDot_,coneApertureRatio_,\[Phi]_,\[Theta]_]:=Module[
	{vis1,vis2,coneDir1,coneDir2,coneAperture2},
	coneDir1={0,0,1};
	coneDir2={Sin[coneDirCosDot],0,Cos[coneDirCosDot]};
	coneAperture2=coneAperture1*coneApertureRatio;
	
	vis1=gSpherCapVis[coneDir1,coneAperture1,\[Phi],\[Theta]];
	vis2=gSpherCapVis[coneDir2,coneAperture2,\[Phi],\[Theta]];
	
	vis1*vis2
];


gSpherSegmentVis[segAxis_,segH1_,segH2_,\[Phi]_,\[Theta]_]:=Module[
	{spherePt,sphereVec,h},
	
	spherePt={Cos[\[Phi]]*Sin[\[Theta]],Sin[\[Phi]]*Sin[\[Theta]],Cos[\[Theta]]};
	sphereVec=Normalize[spherePt];
	h=Dot[sphereVec,Normalize[segAxis]];
	
	Boole[segH1<=h<=segH2]
];


gSpherCapRegion[coneDir_,coneAperture_,\[Phi]_,\[Theta]_]:=Module[
	{spherePt,vis},
	
	spherePt={Cos[\[Phi]]*Sin[\[Theta]],Sin[\[Phi]]*Sin[\[Theta]],Cos[\[Theta]]};
	vis=gSpherCapVis[coneDir,coneAperture,\[Phi],\[Theta]];
	
	ParametricRegion[spherePt*vis,{{\[Phi],0,2\[Pi]},{\[Theta],0,\[Pi]}}]
];


gSolveSpherCapIntsCentroid[inN1_,\[Theta]1_,inN2_,\[Theta]2_]:=Module[
	{n1,n2,area,cx,cy,cz},
	n1=Normalize[inN1];
	n2=Normalize[inN2];
	area=NIntegrate[gSpherCapIntsVis[n1,\[Theta]1,n2,\[Theta]2,\[Phi],\[Theta]],{\[Phi],0,2\[Pi]},{\[Theta],0,\[Pi]},
			PrecisionGoal->2,AccuracyGoal->2, Method->"QuasiMonteCarlo",MaxRecursion->2];
	cx=NIntegrate[Cos[\[Phi]]*Sin[\[Theta]]*gSpherCapIntsVis[n1,\[Theta]1,n2,\[Theta]2,\[Phi],\[Theta]],
			{\[Phi],0,2\[Pi]},{\[Theta],0,\[Pi]},
			PrecisionGoal->2,AccuracyGoal->2, Method->"QuasiMonteCarlo",MaxRecursion->2]/area;
	cy=NIntegrate[Sin[\[Phi]]*Sin[\[Theta]]*gSpherCapIntsVis[n1,\[Theta]1,n2,\[Theta]2,\[Phi],\[Theta]],
			{\[Phi],0,2\[Pi]},{\[Theta],0,\[Pi]},
			PrecisionGoal->2,AccuracyGoal->2, Method->"QuasiMonteCarlo",MaxRecursion->2]/area;
	cz=NIntegrate[Cos[\[Theta]]*gSpherCapIntsVis[n1,\[Theta]1,n2,\[Theta]2,\[Phi],\[Theta]],
			{\[Phi],0,2\[Pi]},{\[Theta],0,\[Pi]},
			PrecisionGoal->2,AccuracyGoal->2, Method->"QuasiMonteCarlo",MaxRecursion->2]/area;
			
	Normalize[{cx,cy,cz}]
];


gApproxCapIntsCentroid[inN1_,\[Theta]1_,inN2_,\[Theta]2_]:=Module[
	{axis1,axis2,axisDot,\[Psi]1,\[Psi]2,axisPsiRange,edgeInfo,
		edgePsiL,edgePsiR,centroidPsi,factor,centroidAxis,centroidPt},
	axis1=Normalize[inN1];
	axis2=Normalize[inN2];
	edgeInfo=gSpherCapIntsEdgeInfo[inN1,\[Theta]1,inN2,\[Theta]2];
	edgePsiL=edgeInfo[[2]];
	edgePsiR=edgeInfo[[3]];
	
	\[Psi]1=0;
	axisDot=Dot[axis1,axis2];
	\[Psi]2=0+ArcCos[axisDot];
	centroidPsi=(edgePsiL+edgePsiR)/2;
	
	factor=Abs[centroidPsi-\[Psi]1]/Abs[\[Psi]2-\[Psi]1];
	centroidAxis=axis1*(1-factor)+axis2*factor;
	centroidPt=Normalize[centroidAxis];
	
	centroidPt
];


gSpherCapIntsArea[spherCap1_,spherCap2_]:=Module[
	{axis1,axis2,apert1,apert2,\[Psi]1,\[Psi]2,\[Omega]1,\[Omega]2,\[Psi]d,area1,pfunc,area2},
	axis1=spherCap1[[1]];
	apert1=spherCap1[[2]];
	axis2=spherCap2[[1]];
	apert2=spherCap2[[2]];
	
	\[Psi]1=Min[apert1,apert2];
	\[Psi]2=Max[apert1,apert2];
	\[Omega]1=Normalize[axis1];
	\[Omega]2=Normalize[axis2];
	\[Psi]d=ArcCos[Dot[\[Omega]1,\[Omega]2]];
	
	area1=2\[Pi]*(1-Cos[\[Psi]1]);
	pfunc[x_]:=3x^2-2x^3;
	area2=area1*pfunc[(\[Psi]1+\[Psi]2-\[Psi]d)/(2\[Psi]1)];
	
	Which[(\[Psi]2-\[Psi]1)>=\[Psi]d,area1,(\[Psi]1+\[Psi]2)<=\[Psi]d,0,True,area2]
];


gSpherCapIntsAreaError[spherCap1_,spherCap2_]:=Module[
	{data1,data2},
	data1=gSpherCapIntsArea[spherCap1,spherCap2];
	data2=NIntegrate[gSpherCapIntsVis2[
					spherCap1[[1]],spherCap1[[2]],spherCap2[[1]],spherCap2[[2]],{x,y,z}],
			{x,y,z}\[Element]Sphere[],PrecisionGoal->2,AccuracyGoal->2,MaxRecursion->2];
	
	Abs[data1-data2]
];


(*returns: 1.apex point covered or not?  2.left edge point. 3.right edge point*)
gSpherCapIntsEdgeInfo[capAxis1_,capApert1_,capAxis2_,capApert2_]:=Module[
	{axis1,axis2,edgeAngleL,edgeAngleR,axisDot,\[Psi]1,\[Psi]1L,\[Psi]1R,\[Psi]2,\[Psi]2L,\[Psi]2R,apex1Covered},
	axis1=Normalize[capAxis1];
	axis2=Normalize[capAxis2];

	axisDot=Dot[axis1,axis2];

	\[Psi]1=0;
	\[Psi]1L=\[Psi]1-capApert1;
	\[Psi]1R=\[Psi]1+capApert1;
	\[Psi]2=0+ArcCos[axisDot];
	\[Psi]2L=\[Psi]2-capApert2;
	\[Psi]2R=\[Psi]2+capApert2;

	edgeAngleL=Max[\[Psi]1L,\[Psi]2L];
	edgeAngleR=Min[\[Psi]1R,\[Psi]2R];
	
	On[Assert];
	Assert[edgeAngleL<edgeAngleR];

	apex1Covered=Boole[\[Psi]2L<0&&\[Psi]2R>0];
	(*returns: 1.apex point covered or not?  2.left edge point. 3.right edge point*)
	{apex1Covered,edgeAngleL,edgeAngleR}
];


gSpherCapIntsBaseRefs[capAxis1_,capApert1_,capAxis2_,capApert2_]:=Module[
	{axis1,axis2,edgeAngleL,edgeAngleR,axisDot,\[Psi]1,\[Psi]1L,\[Psi]1R,\[Psi]2,\[Psi]2L,\[Psi]2R,apex1Covered,
	baseHemiApert,baseQuaterCenter,baseQuaterApert},
	axis1=Normalize[capAxis1];
	axis2=Normalize[capAxis2];

	axisDot=Dot[axis1,axis2];

	\[Psi]1=0;
	\[Psi]1L=\[Psi]1-capApert1;
	\[Psi]1R=\[Psi]1+capApert1;
	\[Psi]2=0+ArcCos[axisDot];
	\[Psi]2L=\[Psi]2-capApert2;
	\[Psi]2R=\[Psi]2+capApert2;

	edgeAngleL=Max[\[Psi]1L,\[Psi]2L];
	edgeAngleR=Min[\[Psi]1R,\[Psi]2R];
	
	On[Assert];
	Assert[edgeAngleL<edgeAngleR];

	apex1Covered=(\[Psi]2L<0&&\[Psi]2R>0);
	baseHemiApert=If[apex1Covered,Min[Abs[edgeAngleL],Abs[edgeAngleR]],0];
    baseQuaterApert=If[apex1Covered,0,Abs[edgeAngleR-edgeAngleL]/2];
    baseQuaterCenter=(edgeAngleR+edgeAngleL)/2;
    {baseHemiApert,baseQuaterCenter,baseQuaterApert}
];


gSpherCapIntsRegionVis[capAxis1_,capApert1_,capAxis2_,capApert2_,\[Phi]_,\[Theta]_]:=Module[
	{visHemiA1,visHemiA2,visHemiB1,visHemiB2,visQuaterA1,visQuaterA2,
	  spherePt,axis1,axis2,edgeAngleL,edgeAngleR,axisDot,\[Psi]1,\[Psi]1L,\[Psi]1R,\[Psi]2,\[Psi]2L,\[Psi]2R,
	  apex1Covered},
	spherePt={Cos[\[Phi]]*Sin[\[Theta]],Sin[\[Phi]]*Sin[\[Theta]],Cos[\[Theta]]};
	axis1=Normalize[capAxis1];
	axis2=Normalize[capAxis2];

	axisDot=Dot[axis1,axis2];

	\[Psi]1=0;
	\[Psi]1L=\[Psi]1-capApert1;
	\[Psi]1R=\[Psi]1+capApert1;
	\[Psi]2=0+ArcCos[axisDot];
	\[Psi]2L=\[Psi]2-capApert2;
	\[Psi]2R=\[Psi]2+capApert2;

	edgeAngleL=Max[\[Psi]1L,\[Psi]2L];
	edgeAngleR=Min[\[Psi]1R,\[Psi]2R];
	
	On[Assert];
	Assert[edgeAngleL<edgeAngleR];

	apex1Covered=(\[Psi]2L<0&&\[Psi]2R>0);
	
	{}
];


End[];


EndPackage[];
