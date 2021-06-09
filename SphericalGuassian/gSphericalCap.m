(* ::Package:: *)

BeginPackage["gSphericalCap`"];


ClearAll[gSpherCap];
gSphericalCap::usage="gSphericalCap";
ClearAll[gSolveCapIntsPoints];
gSolveCapIntsPoints::usage="gSolveCapIntsPoints";
ClearAll[gSpherCapVis];
gSpherCapVis::usage="gSpherCapVis";
ClearAll[gSpherCapIntsVis];
gSpherCapIntsVis::usage="gSpherCapIntsVis";
ClearAll[gSpherCapRegion];
gSpherCapRegion::usage="gSpherCapRegion";
ClearAll[gSpherCapIntsCentroid];
gSpherCapIntsCentroid::usage="gSpherCapIntsCentroid";


Begin["`Private`"];


gSpherCap[axisDir_,apertureAngle_]:=Module[
	{axisPolar,axisPhi,axisTheta},
	axisPolar=ToPolarCoordinates[axisDir];
	axisPhi=axisPolar[[1]];
	axisTheta=axisTheta[[2]];
	
	{axisPhi,axisTheta,apertureAngle}
];


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


gSpherCapIntsVis[coneDir1_,coneAperture1_,coneDir2_,coneAperture2_,\[Phi]_,\[Theta]_]:=Module[
	{vis1,vis2},
	vis1=gSpherCapVis[coneDir1,coneAperture1,\[Phi],\[Theta]];
	vis2=gSpherCapVis[coneDir2,coneAperture2,\[Phi],\[Theta]];
	
	vis1*vis2
];


gSpherCapRegion[coneDir_,coneAperture_,\[Phi]_,\[Theta]_]:=Module[
	{spherePt,vis},
	
	spherePt={Cos[\[Phi]]*Sin[\[Theta]],Sin[\[Phi]]*Sin[\[Theta]],Cos[\[Theta]]};
	vis=gSpherCapVis[coneDir,coneAperture,\[Phi],\[Theta]];
	
	ParametricRegion[spherePt*vis,{{\[Phi],0,2\[Pi]},{\[Theta],0,\[Pi]}}]
];


gSpherCapIntsCentroid[inN1_,\[Theta]1_,inN2_,\[Theta]2_]:=Module[
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





End[];


EndPackage[];
