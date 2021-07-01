(* ::Package:: *)

BeginPackage["gSphericalCap`"];
Needs["sgCommon`"];
Needs["gUtils`"];
Needs["gBRDF`"];


ClearAll[gSpherCap,gSolveCapIntsPoints,gSpherCapVis,gSpherCapIntsVis,gSpherCapRegion,
		gSolveSpherCapIntsCentroid,gSolveSpherCapArea,gSpherCapIntsArea,gSpherCapVis2,
		gSpherCapIntsVis2,gSpherCapIntsVis3,gSpherCapIntsAreaError,gSpherSegmentVis,gSpherCapIntsEdgeInfo,
		gSpherCapIntsBaseRefs,gApproxCapIntsCentroid,gCapIntsIntegralFlags,
		gCapIntsQuaterPart,gCapIntsHemiPart,gCapIntsFullPart,gCapIntsAreas,gSpherCapBorderVis,
		gCalcRepresentThetas];
gSphericalCap::usage="gSphericalCap";
gSolveCapIntsPoints::usage="gSolveCapIntsPoints";
gSpherCapVis::usage="gSpherCapVis";
gSpherCapVis2::usage="gSpherCapVis2";
gSpherCapBorderVis::usage="gSpherCapBorderVis";
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
gCapIntsIntegralFlags::usage="gCapIntsIntegralFlags";
gCapIntsQuaterPart::usage="gCapIntsQuaterPart";
gCapIntsHemiPart::usage="gCapIntsHemiPart";
gCapIntsFullPart::usage="gCapIntsFullPart";
gCapIntsAreas::usage="gCapIntsAreas";
gCalcRepresentThetas::usage="gCalcRepresentThetas";


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


gSpherCapBorderVis[coneDir_,coneAperture_,\[Phi]_,\[Theta]_]:=Module[
	{spherePt,deltaCos,border0,border1},
	spherePt={Cos[\[Phi]]*Sin[\[Theta]],Sin[\[Phi]]*Sin[\[Theta]],Cos[\[Theta]]};
	deltaCos=Dot[spherePt,Normalize[coneDir]];
	
	border0=Clip[coneAperture-\[Pi]/90,{0,\[Pi]}];
	border1=Clip[coneAperture+\[Pi]/90,{0,\[Pi]}];
	
	Boole[Cos[border1]<=deltaCos<=Cos[border0]]
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
	\[Psi]2=0+Re[ArcCos[axisDot]];
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
	\[Psi]d=Re[ArcCos[Dot[\[Omega]1,\[Omega]2]]];
	
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
	{axis1,axis2,edgeAngleL,edgeAngleR,axisDot,a1,a1L,a1R,a2,a2L,a2R,apex1Covered},
	axis1=Normalize[capAxis1];
	axis2=Normalize[capAxis2];

	axisDot=Dot[axis1,axis2];

	a1=0;
	a1L=a1-capApert1;
	a1R=a1+capApert1;
	a2=0+Re[ArcCos[axisDot]];
	a2L=a2-capApert2;
	a2R=a2+capApert2;

	edgeAngleL=Max[a1L,a2L];
	edgeAngleR=Min[a1R,a2R];
	
	(*Assert[edgeAngleL\[LessEqual]edgeAngleR];*)

	apex1Covered=Boole[a2L<0&&a2R>0];
	(*returns: 1.apex point covered or not?  2.left edge point. 3.right edge point*)
	{apex1Covered,edgeAngleL,edgeAngleR}
];


gSpherCapIntsBaseRefs[capAxis1_,capApert1_,capAxis2_,capApert2_]:=Module[
	{axis1,axis2,edgeAngleL,edgeAngleR,axisDot,a1,a1L,a1R,a2,a2L,a2R,apex1Covered,
	baseHemiApert,baseQuaterCenter,baseQuaterApert},
	axis1=Normalize[capAxis1];
	axis2=Normalize[capAxis2];

	axisDot=Dot[axis1,axis2];

	a1=0;
	a1L=a1-capApert1;
	a1R=a1+capApert1;
	a2=0+Re[ArcCos[axisDot]];
	a2L=a2-capApert2;
	a2R=a2+capApert2;

	edgeAngleL=Max[a1L,a2L];
	edgeAngleR=Min[a1R,a2R];
	
	On[Assert];
	Assert[edgeAngleL<edgeAngleR];

	apex1Covered=(a2L<0&&a2R>0);
	baseHemiApert=If[apex1Covered,Min[Abs[edgeAngleL],Abs[edgeAngleR]],0];
    baseQuaterApert=If[apex1Covered,0,Abs[edgeAngleR-edgeAngleL]/2];
    baseQuaterCenter=(edgeAngleR+edgeAngleL)/2;
    {baseHemiApert,baseQuaterCenter,baseQuaterApert}
];


gCapIntsIntegralFlags[capAxis1_,capApert1_,capAxis2_,capApert2_]:=Module[
	{axis1,axis2,edgeAngleL,edgeAngleR,axisDot,a1,a1L,a1R,a2,a2L,a2R,ad,
		apex1Covered,baseHemiApert,baseQuaterCenter,
		part2MinTheta,part2MaxTheta,part2MinPhi,part2MaxPhi,
		zeroFlag,fullFlag,hemiFlag,quaterFlag},
	axis1=Normalize[capAxis1];
	axis2=Normalize[capAxis2];

	axisDot=Dot[axis1,axis2];

	a1=0;
	a1L=a1-capApert1;
	a1R=a1+capApert1;
	a2=0+Re[ArcCos[axisDot]];
	a2L=a2-capApert2;
	a2R=a2+capApert2;
	ad=Re[ArcCos[Dot[axis1,axis2]]];
	
	zeroFlag=If[(a2-a1)>=(capApert1+capApert2),1,0];
	fullFlag=If[(a2-a1)<=(capApert2-capApert1),1,0];
	hemiFlag=If[fullFlag==0&&(a2-a1)<=capApert2,1,0];
	quaterFlag=If[zeroFlag==0&&(a2-a1)>=capApert2,1,0];

	{zeroFlag,fullFlag,hemiFlag,quaterFlag}
];


gCapIntsQuaterPart[capAxis1_,capApert1_,capAxis2_,capApert2_]:=Module[
	{axis1,axis2,axisDot,integralFlags,quaterFlag,thetaRadius,
		fullPhiRadiusFlag,cap2PhiRadius,tmpPhiRatio1,tmpPhiRadius1,tmpPhiRatio2,tmpPhiRadius2,
		a1,a1L,a1R,a2,a2L,a2R,ad,
		phiFactor,phiFactor2,phiRadius,minPhi,maxPhi,minTheta,maxTheta},
	integralFlags=gCapIntsIntegralFlags[capAxis1,capApert1,capAxis2,capApert2];
	axis1=Normalize[capAxis1];
	axis2=Normalize[capAxis2];

	axisDot=Dot[axis1,axis2];
	
	a1=0;
	a1L=a1-capApert1;
	a1R=a1+capApert1;
	a2=0+Re[ArcCos[axisDot]];
	a2L=a2-capApert2;
	a2R=a2+capApert2;
	
	minTheta=Max[a1L,a2L];
	maxTheta=Min[a1R,a2R];
	
	quaterFlag=integralFlags[[4]];
	
	(*the apex of cap2 is fully inside the hemisphere of cap1*)
	fullPhiRadiusFlag=(a2-a1)<=capApert1;
	tmpPhiRatio1=Clip[(a2-a1)/capApert1,{0,1}];
	tmpPhiRadius1=gLerp[capApert2,(\[Pi]/2)*Clip[capApert2/capApert1,{0,1}],tmpPhiRatio1];
	
	(*the apex of cap2 is outside the hemisphere of cap1*)
	tmpPhiRatio2=Clip[(a2-a1-capApert1)/capApert2,{0,1}];
	tmpPhiRadius2=gLerp[capApert2,0,tmpPhiRatio2];
	
	cap2PhiRadius=If[fullPhiRadiusFlag,tmpPhiRadius1,tmpPhiRadius2];
	
(*	phiFactor=(a2-a1-capApert1)/capApert2;
(* (a2-a1-capApert1)\[Equal](capApert2)	\[Rule]0*)
	phiFactor=Clip[phiFactor,{0,1}];
	phiRadius=capApert2*(1-phiFactor);
	phiFactor2=(capApert1-(a2-a1))/capApert1;
	phiFactor2=Clip[phiFactor2,{0,1}];
	phiRadius=phiRadius+(\[Pi]/4)*(phiFactor2);*)
	
	minPhi=-cap2PhiRadius;
	maxPhi=cap2PhiRadius;
	
	If[quaterFlag==1,{minTheta,maxTheta,minPhi,maxPhi},{0,0,0,0}]
]


gCapIntsHemiPart[capAxis1_,capApert1_,capAxis2_,capApert2_]:=Module[
	{axis1,axis2,axisDot,integralFlags,fullFlag,hemiFlag,thetaRadius,
		fullPhiRadiusFlag,cap2PhiRadius,tmpPhiRatio1,tmpPhiRadius1,tmpPhiRatio2,tmpPhiRadius2,
		a1,a1L,a1R,a2,a2L,a2R,ad,radiusPart1,
		phiFactor,phiFactor2,phiRadius,
		minPhiPart2,maxPhiPart2,minThetaPart2,maxThetaPart2,
		edgeAngleL,edgeAngleR},
	integralFlags=gCapIntsIntegralFlags[capAxis1,capApert1,capAxis2,capApert2];
	axis1=Normalize[capAxis1];
	axis2=Normalize[capAxis2];

	axisDot=Dot[axis1,axis2];
	
	a1=0;
	a1L=a1-capApert1;
	a1R=a1+capApert1;
	a2=0+Re[ArcCos[axisDot]];
	a2L=a2-capApert2;
	a2R=a2+capApert2;
	
	edgeAngleL=Max[a1L,a2L];
	edgeAngleR=Min[a1R,a2R];
	
	fullFlag=integralFlags[[2]];
	hemiFlag=integralFlags[[3]];
	radiusPart1:=Min[Abs[edgeAngleL],Abs[edgeAngleR]];
	
	minThetaPart2=radiusPart1;
	maxThetaPart2:=Max[Abs[edgeAngleL],Abs[edgeAngleR]];
	
	cap2PhiRadius=If[fullPhiRadiusFlag,tmpPhiRadius1,tmpPhiRadius2];
	
	If[hemiFlag==1,{radiusPart1,minThetaPart2,maxThetaPart2,-\[Pi]/2,\[Pi]/2},{0,0,0,0,0}]
]


gCapIntsFullPart[capAxis1_,capApert1_,capAxis2_,capApert2_]:=Module[
	{integralFlags,fullFlag},
	integralFlags=gCapIntsIntegralFlags[capAxis1,capApert1,capAxis2,capApert2];
	fullFlag=integralFlags[[2]];
	
	fullFlag==1
]


gCapIntsAreas[capAxis1_,capApert1_,capAxis2_,capApert2_]:=Module[
	{integralFlags,zeroFlag,fullFlag,hemiFlag,quaterFlag,
		quaterParts,hemiParts,hemiAperture,quaterRange},
	integralFlags=gCapIntsIntegralFlags[capAxis1,capApert1,capAxis2,capApert2];
	zeroFlag=integralFlags[[1]];
	fullFlag=integralFlags[[2]];
	hemiFlag=integralFlags[[3]];
	quaterFlag=integralFlags[[4]];
	
	quaterParts=gCapIntsQuaterPart[capAxis1,capApert1,capAxis2,capApert2];
	hemiParts=gCapIntsHemiPart[capAxis1,capApert1,capAxis2,capApert2];
	
	hemiAperture=Which[fullFlag==1,capApert1,True,hemiParts[[1]]];
	quaterRange=If[hemiAperture==0,quaterParts,
		{hemiParts[[2]],hemiParts[[3]],hemiParts[[4]],hemiParts[[5]]}];
	
	{hemiAperture,quaterRange}
];


(*
	shadingDist:the distance between shading point and represent point
	representLambda: the lambda of represent SG
*)
gCalcRepresentThetas[representPt_,diskRadius_,shadingDist_,sgCenter_,sgLambda_]:=Module[
	{
		representDist,distEdgeMax,distEdgeMin,
		thetaMax,thetaLowerMin,thetaLowerMax,thetaUpper
	},
		
	(*representDist: the distance between SG center and represent point*)
	representDist=Norm[sgCenter-representPt];
	distEdgeMax=diskRadius+representDist;
	distEdgeMin=diskRadius-representDist;
	
	(*Solve[(sgPolar[thetaMax,\[Lambda],1]/sgPolar[0,\[Lambda],1])\[Equal]0.01 && \[Lambda]\[GreaterEqual]sgMinLambda&&thetaMax\[Element]Reals,thetaMax]*)
	thetaMax=ArcCos[1-sgMinLambda/sgLambda];
	If[representDist>=diskRadius,
		
		thetaUpper=0;
		thetaLowerMin=ArcTan[Abs[distEdgeMin]/shadingDist];
		thetaLowerMax=ArcTan[distEdgeMax/shadingDist],
		
		thetaUpper=ArcTan[distEdgeMin/shadingDist];
		thetaLowerMin=thetaUpper;
		thetaLowerMax=ArcTan[distEdgeMax/shadingDist]
	];
	
	{thetaLowerMin,thetaLowerMax,thetaUpper}
];


End[];


EndPackage[];
