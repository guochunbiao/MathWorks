(* ::Package:: *)

BeginPackage["gPlots3DEx`"];
Needs["sgCommon`"];
Needs["gUtils`"];
Needs["gBRDF`"];
Needs["gPlots`"];
Needs["gSphericalCap`"];
Needs["gBlochSphere`"];
Needs["gTexStyles`"];


ClearAll[showProps3D,pltRect3D,pltArrow3D,pltPoint3D,pltDisk3D,pltCircle3D,pltSphere3D,
	pltLine3D,pltArc3D,pltDiskProjBoundary3D,pltDiskProjArea3D];
showProps3D::usage="showProps3D";
pltRect3D::usage="pltRect3D";
pltArrow3D::usage="pltArrow3D";
pltPoint3D::usage="pltPoint3D";
pltDisk3D::usage="pltDisk3D";
pltCircle3D::usage="pltCircle3D";
pltSphere3D::usage="pltSphere3D";
pltLine3D::usage="pltLine3D";
pltArc3D::usage="pltArc3D";
pltDiskProjBoundary3D::usage="pltDiskProjBoundary3D";
pltDiskProjArea3D::usage="pltDiskProjArea3D";


Begin["`Private`"];


showProps3D[range_,imageSize_:Tiny]:=Module[
	{},
	
	{
		Axes->True,
		AxesLabel->{"X","Y","Z"},
		Boxed->False,
		PlotRange->{{-range,range},{-range,range},{-range,range}},
		ImageSize->imageSize
	}
];


pltLine3D[input_]:=Module[
	{points,thickness,style,color},
	
	points=gAssocData[input,"points"];
	
	thickness=gAssocDataOpt[input,"thickness",1.5];
	style=gAssocDataOpt[input,"style",Nothing];
	color=gAssocDataOpt[input,"color",Black];
	
	Graphics3D[{AbsoluteThickness[thickness],style,color,Line[points]}]
];


pltRect3D[input_]:=Module[
	{center,normal,majorAxisAssit,majorAxis,minorAxis,majorRadius,minorRadius,
		colorFunc,meshType,plotPts,opacity,thickness,
		rMat},
	
	center=gAssocData[input,"center"];
	normal=Normalize@gAssocData[input,"normal"];
	majorAxisAssit=Normalize@gAssocData[input,"majorAxis"];
	Assert[Abs@Dot[majorAxisAssit,normal]<0.999,"pltRect3D"];
	minorAxis=Normalize@Cross[normal,majorAxisAssit];
	majorAxis=Normalize@Cross[minorAxis,normal];
	majorRadius=gAssocData[input,"majorRadius"];
	minorRadius=gAssocData[input,"minorRadius"];
	
	colorFunc=gAssocDataOpt[input,"colorFunc",Function[{x,y,z},Cyan]];
	meshType=gAssocDataOpt[input,"mesh",None];
	plotPts=gAssocDataOpt[input,"plotPts",20];
	opacity=gAssocDataOpt[input,"opacity",1];
	thickness=gAssocDataOpt[input,"thickness",0.01];
	
	rMat=RotationMatrix[{{0,0,1},normal}].RotationMatrix[{{0,1,0},majorAxis}];
	
	ParametricPlot3D[
		rMat.{w,h,0}+center,
		{w,-minorRadius,minorRadius},{h,-majorRadius,majorRadius},
	    Mesh->meshType,
		PlotPoints->plotPts,
		Lighting->{"Ambient",White},
		PlotStyle->{Opacity[opacity],Thickness[thickness]},
		ColorFunction->colorFunc,
		ColorFunctionScaling->False]
];


pltArrow3D[input_]:=Module[
	{origin,dir,length,thickness},
	
	origin=gAssocData[input,"origin"];
	dir=Normalize@gAssocData[input,"dir"];
	length=gAssocData[input,"length"];
	
	thickness=gAssocDataOpt[input,"thickness",1.5];
	
	Graphics3D[{
		AbsoluteThickness[thickness],
		Arrowheads[{{.03,1,texArrowHead}}],Arrow[{origin,origin+dir*length}]}]
];


pltPoint3D[input_]:=Module[
	{pos,size,color},

	pos=gAssocData[input,"pos"];
	size=gAssocData[input,"size"];
	color=gAssocData[input,"color"];
	
	Graphics3D[{{color,PointSize[size],Point[pos]}}]
];


pltDisk3D[input_]:=Module[
	{center,normal,radius,rotMat,
		colorFunc,meshType,plotPts,opacity},
	
	center=gAssocData[input,"center"];
	normal=Normalize@gAssocData[input,"normal"];
	radius=gAssocData[input,"radius"];
	
	colorFunc=gAssocDataOpt[input,"colorFunc",Function[{x,y,z},Cyan]];
	meshType=gAssocDataOpt[input,"mesh",None];
	plotPts=gAssocDataOpt[input,"plotPts",20];
	opacity=gAssocDataOpt[input,"opacity",1];
	
	rotMat=RotationMatrix[{{0,0,1},normal}];
	
	ParametricPlot3D[rotMat.(r*({Sin[\[Pi]/2]*Cos[\[Phi]],Sin[\[Pi]/2]Sin[\[Phi]],Cos[\[Pi]/2]}))+center,
		{r,0,radius},{\[Phi],0,2\[Pi]},
		BoundaryStyle->None,
		Mesh->meshType,
		PlotPoints->plotPts,
		Lighting->{"Ambient",White},
		PlotStyle->{Opacity[opacity]},
		ColorFunction->colorFunc,
		ColorFunctionScaling->False]
];


pltCircle3D[input_]:=Module[
	{center,normal,radius,rotMat,
		thickness,style,color},
	
	center=gAssocData[input,"center"];
	normal=Normalize@gAssocData[input,"normal"];
	radius=gAssocData[input,"radius"];
	
	thickness=gAssocDataOpt[input,"thickness",1.5];
	style=gAssocDataOpt[input,"style",Nothing];
	color=gAssocDataOpt[input,"color",Black];
	
	rotMat=RotationMatrix[{{0,0,1},normal}];

	Graphics3D[{AbsoluteThickness[thickness],style,color,
		Line[Table[
			rotMat.(radius*({Sin[\[Pi]/2]*Cos[\[Phi]],Sin[\[Pi]/2]Sin[\[Phi]],Cos[\[Pi]/2]}))+center,
			{\[Phi],0,2\[Pi],blCirclePrec}]]
		}]
(*	ParametricPlot3D[rotMat.(radius*({Sin[\[Pi]/2]*Cos[\[Phi]],Sin[\[Pi]/2]Sin[\[Phi]],Cos[\[Pi]/2]}))+center,
		{\[Phi],0,2\[Pi]},
		BoundaryStyle->None,
		Mesh->meshType,
		PlotPoints->plotPts,
		Lighting->{"Ambient",White},
		PlotStyle->{Opacity[opacity]},
		ColorFunction->colorFunc,
		ColorFunctionScaling->False]*)
];


ClearAll[calcPhiOnCircle3D];
calcPhiOnCircle3D[circCenter_,inCircNormal_,circRadius_,testPt_]:=Module[
	{circNormal,rotTo2D,rotPt,projPt,\[Phi]},
	
	circNormal=Normalize@inCircNormal;
	Assert[Abs@(Norm[testPt-circCenter]-circRadius)<0.001,"calcPhiOnCircle3D"];
	
	rotTo2D=RotationMatrix[{circNormal,{0,0,1}}];
	rotPt=rotTo2D.(testPt-circCenter);
	Assert[Abs[rotPt[[3]]]<0.001,"calcPhiOnCircle3D"];
	
	projPt={rotPt[[1]],rotPt[[2]]};
	\[Phi]=ToPolarCoordinates[projPt][[2]];
	
	\[Phi]
];


pltArc3D[input_]:=Module[
	{center,normal,radius,startPt,endPt,
		rotTo3D,\[Phi]1,\[Phi]2,
		thickness,style,color,pltRanges},
	
	center=gAssocData[input,"center"];
	normal=Normalize@gAssocData[input,"normal"];
	radius=gAssocData[input,"radius"];
	startPt=gAssocData[input,"edgePt0"];
	endPt=gAssocData[input,"edgePt1"];
	
	thickness=gAssocDataOpt[input,"thickness",1.5];
	style=gAssocDataOpt[input,"style",Nothing];
	color=gAssocDataOpt[input,"color",Black];
	
	\[Phi]1=calcPhiOnCircle3D[center,normal,radius,startPt];
	\[Phi]2=calcPhiOnCircle3D[center,normal,radius,endPt];
	
	rotTo3D=RotationMatrix[{{0,0,1},normal}];
	pltRanges=If[\[Phi]1<=\[Phi]2,{{\[Phi]1,\[Phi]2}},{{-\[Pi],\[Phi]2},{\[Phi]1,\[Pi]+0.001}}];
	{
	Graphics3D[{AbsoluteThickness[thickness],style,color,
		Line[Table[
			rotTo3D.(radius*({Sin[\[Pi]/2]*Cos[\[Phi]],Sin[\[Pi]/2]Sin[\[Phi]],Cos[\[Pi]/2]}))+center,
			{\[Phi],#[[1]],#[[2]],blCirclePrec}]]
		}]
	}&/@pltRanges
];


pltSphere3D[input_]:=Module[
	{c,r,zbias,
		colorFunc,meshType,plotPts,opacity,lighting},
	
	c=gAssocData[input,"center"];
	r=gAssocData[input,"radius"];
	
	zbias=gAssocDataOpt[input,"zbias",0];
	colorFunc=gAssocDataOpt[input,"colorFunc",Function[{x,y,z},Cyan]];
	meshType=gAssocDataOpt[input,"mesh",None];
	plotPts=gAssocDataOpt[input,"plotPts",20];
	opacity=gAssocDataOpt[input,"opacity",1];
	lighting=gAssocDataOpt[input,"lighting",{"Ambient",White}];
	
	ParametricPlot3D[
		c+r*{Cos[\[Phi]]*Sin[\[Theta]],Sin[\[Phi]]*Sin[\[Theta]],Cos[\[Theta]]}*(1+zbias*2^-6),
		{\[Theta],0,\[Pi]},{\[Phi],0,2\[Pi]},
		BoundaryStyle->None,
		Mesh->meshType,
		PlotPoints->plotPts,
		Lighting->lighting,
		PlotStyle->{Opacity[opacity]},
		ColorFunction->colorFunc,
		ColorFunctionScaling->False]
];


pltDiskProjBoundary3D[input_]:=Module[
	{sphereCenter,sphereRadius,diskCenter,diskNormal,diskRadius,
		thickness,style,color,
		diskRotMat,calcProjPt,projPts},
	sphereCenter={0,0,0};
	sphereRadius=1;
	
	diskCenter=gAssocData[input,"diskCenter"];
	diskNormal=Normalize@gAssocData[input,"diskNormal"];
	diskRadius=gAssocData[input,"diskRadius"];
	
	thickness=gAssocDataOpt[input,"thickness",1.5];
	style=gAssocDataOpt[input,"style",Nothing];
	color=gAssocDataOpt[input,"color",Black];
	
	diskRotMat=RotationMatrix[{{0,0,1},diskNormal}];
	
	calcProjPt[\[Phi]_]:=Module[
		{circlePt,lineDir,lineStartPt,lineEndPt,,projPt,lineSegRegion,sphereRegion,x,y,z,t,sol},
		circlePt=diskRotMat.(diskRadius*({Sin[\[Pi]/2]*Cos[\[Phi]],Sin[\[Pi]/2]Sin[\[Phi]],Cos[\[Pi]/2]}))+diskCenter;
		lineDir=Normalize[circlePt-sphereCenter];
		lineStartPt=sphereCenter;
		lineEndPt=circlePt+lineDir*sphereRadius*2;
		
		sphereRegion=ImplicitRegion[{x^2+y^2+z^2==sphereRadius^2},{x,y,z}];
		lineSegRegion=ParametricRegion[(t*lineStartPt+(1-t)*lineEndPt),{{t,0,1}}];
		
		sol=Quiet@Solve[{t\[Element]sphereRegion,t\[Element]lineSegRegion},{t}];
		projPt=sol[[All,1,2]][[1]];
		
		projPt
	];
	
	projPts=Table[calcProjPt[\[Phi]],{\[Phi],0,2\[Pi],blCirclePrec}];
	
	Graphics3D[{AbsoluteThickness[thickness],style,color,Line[projPts]}]
];


pltDiskProjArea3D[input_]:=Module[
	{sphereCenter,sphereRadius,diskCenter,diskNormal,diskRadius,zbias,colorFunc,opacity,
		reflViewDir,reflNormal,
		majorAxis,minorAxis,majorSize,minorSize,
		region,x,y,z},
	sphereCenter={0,0,0};
	sphereRadius=1;
	
	diskCenter=gAssocData[input,"diskCenter"];
	diskNormal=Normalize@gAssocData[input,"diskNormal"];
	diskRadius=gAssocData[input,"diskRadius"];
	
	zbias=gAssocDataOpt[input,"zbias",0];
	colorFunc=gAssocDataOpt[input,"colorFunc",Function[{x,y,z},Black]];
	opacity=gAssocDataOpt[input,"opacity",1];
	
	reflViewDir=Normalize[sphereCenter-diskCenter];
	If[Dot[diskNormal,reflViewDir]<0,reflNormal=-diskNormal,
		reflNormal=diskNormal,reflNormal=diskNormal];
	majorAxis=Cross[reflViewDir,reflNormal];
	minorAxis=Cross[reflViewDir,majorAxis];
	majorSize=diskRadius;
	minorSize=Dot[reflNormal,reflViewDir]*diskRadius;
	(*Assert[minorSize\[GreaterEqual]0,With[{a=minorSize},a]];*)
	
	region=DiscretizeRegion@ImplicitRegion[
	   Module[{flag1,flag2,flag3,transPt,diff,prod1,prod2,prod3,rayDir,intsPt},
	   (*zbias*)
		flag1=(x-sphereCenter[[1]])^2+(y-sphereCenter[[2]])^2+(z-sphereCenter[[3]])^2==
				(sphereRadius*(1+zbias*2^-6))^2;	
		(*point on hemi-sphere*)
		flag2=(z-sphereCenter[[3]]>=0);
		
		(*https://www.rosettacode.org/wiki/Find_the_intersection_of_a_line_with_a_plane#C.2B.2B*)
		(*c++ code example*)
		rayDir={x,y,z};
		diff={x,y,z}-diskCenter;
		prod1=Dot[diff,diskNormal];
		prod2=Dot[rayDir,diskNormal];
		prod3=prod1/prod2;
		intsPt={x,y,z}-rayDir*prod3;
		If[
			prod2==0,
			flag3=False,
			flag3=Norm[intsPt-diskCenter]<=diskRadius,
			flag3=Norm[intsPt-diskCenter]<=diskRadius
		];
		
	    flag1&&flag2&&flag3
	   ],
	 {x,y,z}];
	
	{
		Graphics3D[{
			AbsoluteThickness[1],Black,Arrowheads[{{.02,1,texArrowHead}}],
				Arrow[{diskCenter,diskCenter+majorAxis*majorSize}],
			AbsoluteThickness[1],Black,Arrowheads[{{.02,1,texArrowHead}}],
				Arrow[{diskCenter,diskCenter+minorAxis*minorSize}]
		}],
		RegionPlot3D[region,ColorFunction->colorFunc,PlotStyle->{Opacity[opacity]}]
	}
];


End[];


EndPackage[];
