(* ::Package:: *)

SetDirectory[NotebookDirectory[]];
BeginPackage["gBlochSphere`"];
Needs["gUtils`"];


ClearAll[blInitOffset,blCirclePrec,blTransFuncXZ,blTransFuncXY,blTransFuncYZ,
	blCalcPoint,blCalcPointEx,blCalcAngle,blCalcAngleEx,blCalcTheta,blCalcPhi,
	blBasicXYCircle,blBasicXZCircle];
blInitOffset=0.1\[Pi];
blCirclePrec=\[Pi]/120;
blTransFuncXZ=Composition@@{RotationTransform[-blInitOffset,{0,0,1}],
						RotationTransform[{{0,0,1},{0,1,0}}]};
blTransFuncXY=Composition@@{RotationTransform[blInitOffset,{0,1,0}],
						RotationTransform[{{0,0,1},{0,0,1}}]};
blTransFuncYZ=RotationTransform[{{0,0,1},{1,0,0}}];
blCalcPoint::usage="blCalcPoint";
blCalcPointEx::usage="blCalcPointEx";
blCalcAngle::usage="blCalcAngle";
blCalcAngleEx::usage="blCalcAngleEx";
blCalcTheta::usage="blCalcTheta";
blCalcPhi::usage="blCalcPhi";
blBasicXYCircle::usage="blBasicXYCircle";
blBasicXZCircle::usage="blBasicYZCircle";


Begin["`Private`"];


blCalcAngle[oldPt_]:=Module[
	{p1A,dirFlag1,dir1,radius1A,radius1B,p1B,
	p2A,dirFlag2,dir2,radius2A,radius2B,p2B,
	newThetaA,newPhiA,
	newTheta,newPhi},
	
	(*x-y circle*)
	p1A={0,oldPt[[2]],oldPt[[3]]};
	dirFlag1=If[oldPt[[1]]>0,1,-1];
	dir1=RotationTransform[blInitOffset,{0,1,0}][{1,0,0}]*dirFlag1;
	radius1A=Sqrt[1-oldPt[[3]]^2];
	radius1B=Sqrt[radius1A^2-oldPt[[2]]^2];
	p1B=p1A+Normalize[dir1]*radius1B;
	newThetaA=ArcCos[Dot[Normalize[p1B-{0,0,0}],{0,0,1}]];
	newTheta=If[p1B[[1]]>=0,newThetaA,2\[Pi]-newThetaA];

	(*x-z circle*)
	p2A={0,oldPt[[2]],oldPt[[3]]};
	dirFlag2=If[oldPt[[1]]>0,1,-1];
	dir2=RotationTransform[-blInitOffset,{0,0,1}][{1,0,0}]*dirFlag2;
	radius2A=Sqrt[1-oldPt[[3]]^2];
	radius2B=Sqrt[radius2A^2-oldPt[[2]]^2];
	p2B=p2A+Normalize[dir2]*radius2B;
	newPhiA=ArcCos[Dot[Normalize@{p2B[[1]],p2B[[2]],0},{1,0,0}]];
	newPhi=If[p2B[[2]]>0,newPhiA,2\[Pi]-newPhiA];
	
	{newTheta,newPhi}
];


blCalcAngleEx[oldTheta_,oldPhi_]:=Module[
	{oldPt},
	oldPt={Cos[oldPhi]*Sin[oldTheta],Sin[oldPhi]*Sin[oldTheta],Cos[oldTheta]};
	
	blCalcAngle[oldPt]
];


blCalcPoint[oldPt_]:=Module[
	{newTheta,newPhi,newPt},
	
	{newTheta,newPhi}=blCalcAngle[oldPt];
	newPt={Cos[newPhi]*Sin[newTheta],Sin[newPhi]*Sin[newTheta],Cos[newTheta]};
	
	newPt
];


blCalcPointEx[oldTheta_,oldPhi_]:=Module[
	{oldPt},
	oldPt={Cos[oldPhi]*Sin[oldTheta],Sin[oldPhi]*Sin[oldTheta],Cos[oldTheta]};
	
	blCalcPoint[oldPt]
];


blCalcTheta[oldTheta_,oldPhi_]:=Module[
	{},
	
	blCalcAngleEx[oldTheta,oldPhi][[1]]
]


blCalcPhi[oldTheta_,oldPhi_]:=Module[
	{},
	
	blCalcAngleEx[oldTheta,oldPhi][[2]]
]


blBasicXYCircle[\[Theta]_]:=Module[
	{z,r,\[Phi]0,\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4,circlePts},
	
	(*z coordinate*)
	z=Cos[\[Theta]];
	(*radius*)
	r=Sqrt[1-z^2];
	
	\[Phi]0=blCalcPhi[\[Theta],0];(*5.969*)
	\[Phi]1=blCalcPhi[\[Theta],\[Pi]/2];(*1.57*)
	\[Phi]2=blCalcPhi[\[Theta],\[Pi]];(*2.827*)
	\[Phi]3=blCalcPhi[\[Theta],3\[Pi]/2];(*4.7123*)
	\[Phi]4=blCalcPhi[\[Theta],2\[Pi]];(*5.969*)
	
	{
		(*0--\[Pi]/2*)
		{Line[Table[blTransFuncXY@{r*Cos[\[Phi]],r*Sin[\[Phi]],z},{\[Phi],\[Phi]0,2\[Pi],blCirclePrec}]]},
		{Line[Table[blTransFuncXY@{r*Cos[\[Phi]],r*Sin[\[Phi]],z},{\[Phi],0,\[Phi]1,blCirclePrec}]]},
		(*\[Pi]/2--\[Pi]*)
		{Dashed,Line[Table[blTransFuncXY@{r*Cos[\[Phi]],r*Sin[\[Phi]],z},{\[Phi],\[Phi]1,\[Phi]2,blCirclePrec}]]},
		(*\[Pi]--3\[Pi]/2*)
		{Dashed,Line[Table[blTransFuncXY@{r*Cos[\[Phi]],r*Sin[\[Phi]],z},{\[Phi],\[Phi]2,\[Phi]3,blCirclePrec}]]},
		(*3\[Pi]/2--2\[Pi]*)
		{Line[Table[blTransFuncXY@{r*Cos[\[Phi]],r*Sin[\[Phi]],z},{\[Phi],\[Phi]3,2\[Pi],blCirclePrec}]]}
	}
];


blBasicXZCircle[]:=Module[
	{\[Phi],\[Theta]0,\[Theta]1,\[Theta]2,\[Theta]3,\[Theta]4},
	
	\[Phi]=0;
	\[Theta]0=blCalcTheta[0,\[Phi]];(**)
	\[Theta]1=blCalcTheta[\[Pi]/2,\[Phi]];(**)
	\[Theta]2=blCalcTheta[\[Pi],\[Phi]];(**)
	\[Theta]3=blCalcTheta[3\[Pi]/2,\[Phi]];(**)
	\[Theta]4=blCalcTheta[2\[Pi],\[Phi]];(**)

	{
		(*0--\[Pi]/2*)
		{Line[Table[blTransFuncXZ@{Sin[\[Theta]],-Cos[\[Theta]],0},{\[Theta],0,\[Theta]1,blCirclePrec}]]},
		(*\[Pi]/2--\[Pi]*)
		{Line[Table[blTransFuncXZ@{Sin[\[Theta]],-Cos[\[Theta]],0},{\[Theta],\[Theta]1,\[Theta]2,blCirclePrec}]]},
		(*\[Pi]--3\[Pi]/2*)
		{Dashed,Line[Table[blTransFuncXZ@{Sin[\[Theta]],-Cos[\[Theta]],0},{\[Theta],\[Theta]2,\[Theta]3,blCirclePrec}]]},
		(*3\[Pi]/2--2\[Pi]*)
		{Dashed,Line[Table[blTransFuncXZ@{Sin[\[Theta]],-Cos[\[Theta]],0},{\[Theta],\[Theta]3,2\[Pi],blCirclePrec}]]}
	}
];


End[];


EndPackage[];
