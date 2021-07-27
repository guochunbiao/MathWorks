(* ::Package:: *)

SetDirectory[NotebookDirectory[]];
BeginPackage["gBlochSphere`"];
Needs["gUtils`"];
Needs["gTexStyles`"];
Needs["gPlots3DEx`"];


ClearAll[blInitOffset,blDefaultThickness,blCirclePrec,blTransFuncXY,(*blTransFuncYZ,*)
	blCalcPoint,blCalcPointEx,blCalcAngle,blCalcAngleEx,blCalcTheta,blCalcPhi,
	blBasicXYCircle,blBasicXZCircle,blBasicYZCircle,blSphereArrow,blSphereAxes,
	blWholeSphere,blHemiSphere,
	(*paper related*)
	blPaperSphere01,blPaperSphere02,
	blPaperIntsDisk01,blPaperIntsDisk02,blPaperIntsDisk03,blPaperIntsDisk04,blPaperIntsDisk05,
	blPaperIntsDisk06,blPaperIntsDisk07
	];
blInitOffset=0.1\[Pi];
blDefaultThickness=1.5;
blCirclePrec=\[Pi]/120;
blTransFuncXY=Composition@@{RotationTransform[blInitOffset,{0,1,0}],
						RotationTransform[{{0,0,1},{0,0,1}}]};
(*blTransFuncYZ=RotationTransform[{{0,0,1},{1,0,0}}];*)
blCalcPoint::usage="blCalcPoint";
blCalcPointEx::usage="blCalcPointEx";
blCalcAngle::usage="blCalcAngle";
blCalcAngleEx::usage="blCalcAngleEx";
blCalcTheta::usage="blCalcTheta";
blCalcPhi::usage="blCalcPhi";
blBasicXYCircle::usage="blBasicXYCircle";
blBasicXZCircle::usage="blBasicYZCircle";
blBasicYZCircle::usage="blBasicYZCircle";
blSphereArrow::usage="blSphereArrow";
blSphereAxes::usage="blSphereAxes";
blWholeSphere::usage="blWholeSphere";
blHemiSphere::usage="blHemiSphere";
blPaperSphere01::usage="blPaperSphere01";
blPaperSphere02::usage="blPaperSphere02";
blPaperIntsDisk01::usage="blPaperIntsDisk01";
blPaperIntsDisk02::usage="blPaperIntsDisk02";
blPaperIntsDisk03::usage="blPaperIntsDisk03";
blPaperIntsDisk04::usage="blPaperIntsDisk04";
blPaperIntsDisk05::usage="blPaperIntsDisk05";
blPaperIntsDisk06::usage="blPaperIntsDisk06";
blPaperIntsDisk07::usage="blPaperIntsDisk07";


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
		{AbsoluteThickness[blDefaultThickness],
			Line[Table[blTransFuncXY@{r*Cos[\[Phi]],r*Sin[\[Phi]],z},{\[Phi],\[Phi]0,2\[Pi],blCirclePrec}]]},
		{AbsoluteThickness[blDefaultThickness],
			Line[Table[blTransFuncXY@{r*Cos[\[Phi]],r*Sin[\[Phi]],z},{\[Phi],0,\[Phi]1,blCirclePrec}]]},
		(*\[Pi]/2--\[Pi]*)
		{AbsoluteThickness[blDefaultThickness],Dashed,
			Line[Table[blTransFuncXY@{r*Cos[\[Phi]],r*Sin[\[Phi]],z},{\[Phi],\[Phi]1,\[Phi]2,blCirclePrec}]]},
		(*\[Pi]--3\[Pi]/2*)
		{AbsoluteThickness[blDefaultThickness],Dashed,
			Line[Table[blTransFuncXY@{r*Cos[\[Phi]],r*Sin[\[Phi]],z},{\[Phi],\[Phi]2,\[Phi]3,blCirclePrec}]]},
		(*3\[Pi]/2--2\[Pi]*)
		{AbsoluteThickness[blDefaultThickness],
			Line[Table[blTransFuncXY@{r*Cos[\[Phi]],r*Sin[\[Phi]],z},{\[Phi],\[Phi]3,2\[Pi],blCirclePrec}]]}
	}
];


blBasicXZCircle[y_,turning_:-0.1\[Pi],bHemi_:False]:=Module[
	{\[Phi],r,\[Theta]0,\[Theta]1,\[Theta]2,\[Theta]3,\[Theta]4},
	
	(*radius*)
	r=Sqrt[1-y^2];
	
	\[Phi]=0;
	\[Theta]0=blCalcTheta[0,\[Phi]];(**)
	\[Theta]1=blCalcTheta[\[Pi]/2,\[Phi]];(**)
	\[Theta]2=blCalcTheta[\[Pi],\[Phi]];(**)
	\[Theta]3=blCalcTheta[3\[Pi]/2,\[Phi]];(**)
	\[Theta]4=blCalcTheta[2\[Pi],\[Phi]];(**)

	blTransFuncXZ=Composition@@{RotationTransform[turning,{0,0,1}],
						RotationTransform[{{0,0,1},{0,1,0}}]};
	{
		(*0--\[Pi]/2*)
		{AbsoluteThickness[blDefaultThickness],
			Line[Table[blTransFuncXZ@{r*Sin[\[Theta]],-r*Cos[\[Theta]],y},{\[Theta],0,\[Theta]1,blCirclePrec}]]},
		(*\[Pi]/2--\[Pi]*)
		If[bHemi,Nothing,{AbsoluteThickness[blDefaultThickness],
			Line[Table[blTransFuncXZ@{r*Sin[\[Theta]],-r*Cos[\[Theta]],y},{\[Theta],\[Theta]1,\[Theta]2,blCirclePrec}]]}],
		(*\[Pi]--3\[Pi]/2*)
		If[bHemi,Nothing,{AbsoluteThickness[blDefaultThickness],Dashed,
			Line[Table[blTransFuncXZ@{r*Sin[\[Theta]],-r*Cos[\[Theta]],y},{\[Theta],\[Theta]2,\[Theta]3,blCirclePrec}]]}],
		(*3\[Pi]/2--2\[Pi]*)
		{AbsoluteThickness[blDefaultThickness],Dashed,
			Line[Table[blTransFuncXZ@{r*Sin[\[Theta]],-r*Cos[\[Theta]],y},{\[Theta],\[Theta]3,2\[Pi],blCirclePrec}]]}
	}
];


blBasicYZCircle[bHemi_:False]:=Module[
	{},
	
	{
		(*0--\[Pi]/2*)
		{AbsoluteThickness[blDefaultThickness],
			Line[Table[{0,Cos[\[Theta]],Sin[\[Theta]]},{\[Theta],0,\[Pi]/2,blCirclePrec}]]},
		(*\[Pi]/2--\[Pi]*)
		{AbsoluteThickness[blDefaultThickness],
			Line[Table[{0,Cos[\[Theta]],Sin[\[Theta]]},{\[Theta],\[Pi]/2,\[Pi],blCirclePrec}]]},
		(*\[Pi]--3\[Pi]/2*)
		If[bHemi,Nothing,{AbsoluteThickness[blDefaultThickness],
			Line[Table[{0,Cos[\[Theta]],Sin[\[Theta]]},{\[Theta],\[Pi],3\[Pi]/2,blCirclePrec}]]}],
		(*3\[Pi]/2--2\[Pi]*)
		If[bHemi,Nothing,{AbsoluteThickness[blDefaultThickness],
			Line[Table[{0,Cos[\[Theta]],Sin[\[Theta]]},{\[Theta],3\[Pi]/2,2\[Pi],blCirclePrec}]]}]
	}
];


blSphereArrow[inDir_,length_]:=Module[
	{originPt,oldDir,newDir,oldPt,newPt},
	
	originPt={0,0,0};
	oldDir=Normalize@inDir;
	oldPt=originPt+oldDir;
	newPt=blCalcPoint[oldPt];
	newDir=Normalize[newPt-originPt];
	
	newPt=originPt+newDir*length;
	
	{AbsoluteThickness[blDefaultThickness],Arrowheads[{{.03,1,texArrowHead}}],Arrow[{originPt,newPt}]}
];


blSphereAxes[size_]:=Module[
	{},
	
	blSphereArrow[#[[1]],#[[2]]]&/@{{{0,0,1},size},{{0,1,0},size},{{1,0,0},size}}
];


blWholeSphere[axisSize_:1.3]:=Module[
	{},
	
	{
	(*x-y circle*)
	blBasicXYCircle[\[Pi]/2],
	(*x-z circle*)
	blBasicXZCircle[0],
	(*y-z circle*)
	blBasicYZCircle[],

	(*sphere center at origin point*)
	{{Black,PointSize[Large],Point[{0,0,0}]}},

	(*axes*)
	blSphereAxes[axisSize]
	}
];


blHemiSphere[axisSize_:1.3]:=Module[
	{},
	
	{
	(*x-y circle*)
	blBasicXYCircle[\[Pi]/2],
	(*x-z circle*)
	blBasicXZCircle[0,0,True],
	(*y-z circle*)
	blBasicYZCircle[True],

	(*sphere center at origin point*)
	{{Black,PointSize[Large],Point[{0,0,0}]}},

	(*axes*)
	blSphereAxes[axisSize]
	}
];


blPaperSphere01[]:=Module[
	{},
	
	Graphics3D[{
		(*x-y circle*)
		blBasicXYCircle[\[Pi]/2],
		(*x-z circle*)
		blBasicXZCircle[0,0,True],
		(*y-z circle*)
		blBasicYZCircle[True],
		
		(*axes*)
		blSphereAxes[1.3],

		(*sphere center at origin point*)
		{{Black,PointSize[Large],Point[{0,0,0}]}}
	}]
];


blPaperSphere02[color_,opacity_]:=Module[
	{},
	
	{
	Graphics3D[{
		(*x-y circle*)
		blBasicXYCircle[\[Pi]/2],
		(*x-z circle*)
		blBasicXZCircle[0,0,True],
		(*y-z circle*)
		blBasicYZCircle[True],
		
		(*axes*)
		blSphereAxes[1.3],

		(*sphere center at origin point*)
		{{Black,PointSize[Large],Point[{0,0,0}]}}
		}],
	
	(*colored sphere*)
	pltSphere3D[ <|"center"->{0,0,0},"radius"->1,"plotPts"->20,
		"mesh"->None,"opacity"->opacity,"colorFunc"->Function[{x,y,z},color]|>]
	}
];


blPaperIntsDisk01[inCenter_,inNormal_,radius_]:=Module[
	{c,n,r,outReg,inReg,x,y,z},
	
	c=(*blCalcPoint@*)Normalize[inCenter-{0,0,0}];
	n=Normalize@inNormal;
	r=radius;
	
	(*part of disk that outside the sphere*)
	outReg=DiscretizeRegion[ImplicitRegion[
		{
			Dot[{x,y,z}-c,n]==0&&
			Norm[{x,y,z}-c]<=radius&&
			Norm[{x,y,z}-{0,0,0}]>1
		},{x,y,z}]];
	
	(*part of disk that inside the sphere*)
	inReg=DiscretizeRegion[ImplicitRegion[
		{
			Dot[{x,y,z}-c,n]==0&&
			Norm[{x,y,z}-c]<=radius&&
			Norm[{x,y,z}-{0,0,0}]<=1
		},{x,y,z}]];
		
	{
		(*disk center point*)
		Graphics3D[{{Blue,PointSize[Large],Point[c]}}],
		(*part of disk that outside the sphere*)
		RegionPlot3D[outReg,BoundaryStyle->None,Mesh->None,PlotStyle->{Opacity[0.2],Blue}],
		(*part of disk that inside the sphere*)
		RegionPlot3D[inReg,BoundaryStyle->None,Mesh->None,PlotStyle->{Opacity[0.2],Red}]
	}
];


blPaperIntsDisk02[inCenter_,inNormalEuler_,radius_]:=Module[
	{c,nlTheta,nlPhi,nlFunc},
	
	c=(*blCalcPoint@*)Normalize[inCenter-{0,0,0}];
	nlTheta=inNormalEuler[[1]];
	nlPhi=inNormalEuler[[2]];
	
	nlFunc[\[Theta]_,\[Phi]_]:={Sin[\[Theta]]*Cos[\[Phi]],Sin[\[Theta]]*Sin[\[Phi]],Cos[\[Theta]]};
		
	{
		ParametricPlot3D[(RotationMatrix[{{0,0,1},nlFunc[nlTheta,nlPhi]}].
			(r*({Sin[\[Pi]/2]*Cos[\[Phi]],Sin[\[Pi]/2]Sin[\[Phi]],Cos[\[Pi]/2]})))+c,
			{r,0,radius},{\[Phi],0,2\[Pi]},
			BoundaryStyle->None,Mesh->None,(*PlotPoints\[Rule]20,*)
			PlotStyle->{Opacity[0.2],Blue}]
	}
];


blPaperIntsDisk03[inCenter_,inNormal_,radius_]:=Module[
	{c,n,r,sol1,sol2,intsPt1,intsPt2,x,y,z},
	
	c=(*blCalcPoint@*)Normalize[inCenter-{0,0,0}];
	n=Normalize@inNormal;
	r=radius;
	
	sol1=FindInstance[
		(*on sphere*)
		x^2+y^2+z^2==1&&
		(*on disk*)
		Norm[{x,y,z}-c]==r&&
		Dot[{x,y,z}-c,n]==0,
		{x,y,z},Reals,2];
	sol2=Transpose@sol1;
	intsPt1=sol2[[All,1,2]];
	intsPt2=sol2[[All,2,2]];
		
	{
		(*intersection points*)
		Graphics3D[{{Black,PointSize[Large],Point[{intsPt1,intsPt2}]}}],
		(*disk center point*)
		Graphics3D[{{Blue,PointSize[Large],Point[c]}}],
		(*outside the sphere*)
		pltArc3D[<|"center"->c,"normal"->n,"radius"->r,
			"edgePt0"->intsPt2,"edgePt1"->intsPt1|>],
		(*inside the sphere*)
		pltArc3D[<|"center"->c,"normal"->n,"radius"->r,"style"->Dashed,
			"edgePt0"->intsPt1,"edgePt1"->intsPt2|>]
	}
];


blPaperIntsDisk04[inCenter_,inNormal_,radius_]:=Module[
	{c,n,r,sol1,sol2,intsPt1,intsPt2,x,y,z},
	
	c=(*blCalcPoint@*)Normalize[inCenter-{0,0,0}];
	n=Normalize@inNormal;
	r=radius;
	
	sol1=FindInstance[
		(*on sphere*)
		x^2+y^2+z^2==1&&
		(*on disk*)
		Norm[{x,y,z}-c]==r&&
		Dot[{x,y,z}-c,n]==0,
		{x,y,z},Reals,2];
	sol2=Transpose@sol1;
	intsPt1=sol2[[All,1,2]];
	intsPt2=sol2[[All,2,2]];
		
	{
		(*intersection points*)
		(*Graphics3D[{{Black,PointSize[Large],Point[{intsPt1,intsPt2}]}}],*)
		(*disk center point*)
		Graphics3D[{{Blue,PointSize[Large],Point[c]}}],
		(*outside the sphere*)
		pltArc3D[<|"center"->c,"normal"->n,"radius"->r,
			"edgePt0"->intsPt2,"edgePt1"->intsPt1|>],
		(*inside the sphere*)
		pltArc3D[<|"center"->c,"normal"->n,"radius"->r,"style"->Dashed,
			"edgePt0"->intsPt1,"edgePt1"->intsPt2|>]
	}
];


blPaperIntsDisk05[inCenter_,inNormal_,radius_,intsPt1_,intsPt2_]:=Module[
	{c,n,r},
	
	c=(*blCalcPoint@*)Normalize[inCenter-{0,0,0}];
	n=Normalize@inNormal;
	r=radius;
		
	{
		(*intersection points*)
		(*Graphics3D[{{Black,PointSize[Large],Point[{intsPt1,intsPt2}]}}],*)
		(*disk center point*)
		Graphics3D[{{Blue,PointSize[Large],Point[c]}}],
		(*outside the sphere*)
		pltArc3D[<|"center"->c,"normal"->n,"radius"->r,
			"edgePt0"->intsPt2,"edgePt1"->intsPt1|>],
		(*inside the sphere*)
		pltArc3D[<|"center"->c,"normal"->n,"radius"->r,"style"->Dashed,
			"edgePt0"->intsPt1,"edgePt1"->intsPt2|>]
	}
];


blPaperIntsDisk06[inCenter_,inNormal_,radius_]:=Module[
	{c,n,r,sol1,sol2,intsPt1,intsPt2,x,y,z,
		diskMajorAxis,diskMinorAxis,
		diskMajorEdge0,diskMajorEdge1,diskMinorEdge0,diskMinorEdge1},
	
	c=(*blCalcPoint@*)Normalize[inCenter-{0,0,0}];
	n=Normalize@inNormal;
	r=radius;
	
	sol1=FindInstance[
		(*on sphere*)
		x^2+y^2+z^2==1&&
		(*on disk*)
		Norm[{x,y,z}-c]==r&&
		Dot[{x,y,z}-c,n]==0,
		{x,y,z},Reals,2];
	sol2=Transpose@sol1;
	intsPt1=sol2[[All,1,2]];
	intsPt2=sol2[[All,2,2]];
	
	diskMajorAxis=Normalize@Cross[c,n];
	diskMajorEdge0=c+diskMajorAxis*r;
	diskMajorEdge1=c-diskMajorAxis*r;
	diskMinorAxis=Normalize@Cross[diskMajorAxis,n];
	diskMinorEdge0=c+diskMinorAxis*r;
	diskMinorEdge1=c-diskMinorAxis*r;
		
	{
		(*sphere center*)
		pltPoint3D[<|"pos"->{0,0,0},"size"->0.01,"color"->Black|>],
		(*disk edge points*)
		pltPoint3D[<|"pos"->diskMajorEdge0,"size"->0.01,"color"->Black|>],
		pltPoint3D[<|"pos"->diskMajorEdge1,"size"->0.01,"color"->Black|>],
		pltPoint3D[<|"pos"->diskMinorEdge0,"size"->0.01,"color"->Black|>],
		pltPoint3D[<|"pos"->diskMinorEdge1,"size"->0.01,"color"->Black|>],
		(*edge projection lines*)
		pltLine3D[<|"points"->{{0,0,0},diskMajorEdge0},"color"->Black,"style"->Dashed|>],
		pltLine3D[<|"points"->{{0,0,0},diskMajorEdge1},"color"->Black,"style"->Dashed|>],
		pltLine3D[<|"points"->{{0,0,0},diskMinorEdge0},"color"->Black,"style"->Dashed|>],
		pltLine3D[<|"points"->{{0,0,0},diskMinorEdge1},"color"->Black,"style"->Dashed|>],
		(*sphere*)
		pltSphere3D[ <|"center"->{0,0,0},"radius"->1,"plotPts"->100,"opacity"->0.3,
		"lighting"->Automatic,"colorFunc"->Function[{x,y,z,\[Theta],\[Phi]},LightBlue]|>],
		(*disk center*)
		(*pltPoint3D[<|"pos"->c,"size"->0.01,"color"->Black|>],*)
		(*disk*)
		pltDisk3D[<|"center"->c,"normal"->n,"radius"->r,"opacity"->0.3,
			"colorFunc"->Function[{x,y,z},Green]|>],
		(*intersection points*)
		(*Graphics3D[{{Black,PointSize[Large],Point[{intsPt1,intsPt2}]}}],*)
		(*outside the sphere*)
		pltArc3D[<|"center"->c,"normal"->n,"radius"->r,"color"->Green,
			"edgePt0"->intsPt1,"edgePt1"->intsPt2|>],
		(*inside the sphere*)
		pltArc3D[<|"center"->c,"normal"->n,"radius"->r,"style"->Dashed,"color"->Green,
			"edgePt0"->intsPt2,"edgePt1"->intsPt1|>],
			
		(*ints boundary*)
		pltDiskProjBoundary3D[<|"diskCenter"->c,"diskNormal"->n,"diskRadius"->r,
			"color"->Red|>]
		(*ints area*)
		(*,pltDiskProjArea3D[<|"diskCenter"\[Rule]c,"diskNormal"\[Rule]n,"diskRadius"\[Rule]r,
			"opacity"\[Rule]0.3,"colorFunc"\[Rule]Function[{x,y,z},Red],"zbias"\[Rule]1|>]*)
	}
];


blPaperIntsDisk07[inCenter_,inNormal_,radius_,calcPointPercent_]:=Module[
	{c,n,r,sol1,sol2,intsPt1,intsPt2,x,y,z,
		diskMajorAxis,diskMinorAxis,calcPoint,
		diskMajorEdge0,diskMajorEdge1,diskMinorEdge0,diskMinorEdge1,
		viewDir0,viewDir1},
	
	c=(*blCalcPoint@*)Normalize[inCenter-{0,0,0}];
	n=Normalize@inNormal;
	r=radius;
	
(*	sol1=FindInstance[
		(*on sphere*)
		x^2+y^2+z^2==1&&
		(*on disk*)
		Norm[{x,y,z}-c]==r&&
		Dot[{x,y,z}-c,n]==0,
		{x,y,z},Reals,2];
	sol2=Transpose@sol1;
	intsPt1=sol2[[All,1,2]];
	intsPt2=sol2[[All,2,2]];*)
	
	diskMajorAxis=Normalize@Cross[c,n];
	diskMajorEdge0=c+diskMajorAxis*r;
	diskMajorEdge1=c-diskMajorAxis*r;
	diskMinorAxis=Normalize@Cross[diskMajorAxis,n];
	diskMinorEdge0=c+diskMinorAxis*r;
	diskMinorEdge1=c-diskMinorAxis*r;
	
	calcPoint=c-diskMinorAxis*r*calcPointPercent;
	
	viewDir0=Normalize[{0,0,0}-c];
	viewDir1=Normalize[{0,0,0}-calcPoint];

	{
		(*sphere center*)
		pltPoint3D[<|"pos"->{0,0,0},"size"->0.01,"color"->Black|>],
		(*disk edge points*)
(*		pltPoint3D[<|"pos"->diskMajorEdge0,"size"->0.01,"color"->Black|>],
		pltPoint3D[<|"pos"->diskMajorEdge1,"size"->0.01,"color"->Black|>],*)
		pltPoint3D[<|"pos"->diskMinorEdge0,"size"->0.01,"color"->Black|>],
		pltPoint3D[<|"pos"->diskMinorEdge1,"size"->0.01,"color"->Black|>],
		(*disk major axes*)
		pltLine3D[<|"points"->{c,diskMinorEdge0},"color"->Black,"style"->Dashed|>],
		pltLine3D[<|"points"->{c,diskMinorEdge1},"color"->Black|>],
		(*disk calc point*)
		pltPoint3D[<|"pos"->calcPoint,"size"->0.01,"color"->Black|>],
		
		(*edge projection lines*)
(*		pltLine3D[<|"points"\[Rule]{{0,0,0},diskMajorEdge0},"color"\[Rule]Black,"style"\[Rule]Dashed|>],
		pltLine3D[<|"points"\[Rule]{{0,0,0},diskMajorEdge1},"color"\[Rule]Black,"style"\[Rule]Dashed|>],
		pltLine3D[<|"points"\[Rule]{{0,0,0},diskMinorEdge0},"color"\[Rule]Black,"style"\[Rule]Dashed|>],
		pltLine3D[<|"points"\[Rule]{{0,0,0},diskMinorEdge1},"color"\[Rule]Black,"style"\[Rule]Dashed|>],*)
		(*sphere*)
		pltSphere3D[ <|"center"->{0,0,0},"radius"->1,"plotPts"->100,"opacity"->0.3,
		"lighting"->Automatic,"colorFunc"->Function[{x,y,z,\[Theta],\[Phi]},LightBlue]|>],
		(*disk center*)
		pltPoint3D[<|"pos"->c,"size"->0.01,"color"->Black|>],
		(*disk*)
		pltDisk3D[<|"center"->c,"normal"->n,"radius"->r,"opacity"->0.3,
			"colorFunc"->Function[{x,y,z},Green]|>],
		(*intersection points*)
		(*Graphics3D[{{Black,PointSize[Large],Point[{intsPt1,intsPt2}]}}],*)
		(*outside the sphere*)
(*		pltArc3D[<|"center"->c,"normal"->n,"radius"->r,"color"->Green,
			"edgePt0"->intsPt1,"edgePt1"->intsPt2|>],
		(*inside the sphere*)
		pltArc3D[<|"center"->c,"normal"->n,"radius"->r,"style"->Dashed,"color"->Green,
			"edgePt0"->intsPt2,"edgePt1"->intsPt1|>],*)
			
		(*view dir 0*)
		pltArrow3D[<|"origin"->c,"dir"->viewDir0,"length"->1|>],
		(*view dir 1*)
		pltArrow3D[<|"origin"->calcPoint,"dir"->viewDir1,"length"->1|>],
		pltLine3D[<|"points"->{{0,0,0},-viewDir1*(Norm[calcPoint]-1)},
			"color"->Black,"style"->Dashed|>],
		(*delta theta*)
		pltArc3DEx[<|"center"->{0,0,0},"normal"->{0,1,0},"radius"->0.2,
			"dir0"->-viewDir0,"dir1"->-viewDir1,
			"thickness"->2,"style"->Dashed,"color"->Blue|>],
		Graphics3D[{Text[Style["d\[Theta]",Medium],{0,0,0}+Normalize[-viewDir0-viewDir1]*0.25]}]
			
		(*ints boundary*)
(*		pltDiskProjBoundary3D[<|"diskCenter"->c,"diskNormal"->n,"diskRadius"->r,
			"color"->Red|>]*)
		(*ints area*)
		(*,pltDiskProjArea3D[<|"diskCenter"\[Rule]c,"diskNormal"\[Rule]n,"diskRadius"\[Rule]r,
			"opacity"\[Rule]0.3,"colorFunc"\[Rule]Function[{x,y,z},Red],"zbias"\[Rule]1|>]*)
	}
];


End[];


EndPackage[];
