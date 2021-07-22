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
	pltLine3D];
showProps3D::usage="showProps3D";
pltRect3D::usage="pltRect3D";
pltArrow3D::usage="pltArrow3D";
pltPoint3D::usage="pltPoint3D";
pltDisk3D::usage="pltDisk3D";
pltCircle3D::usage="pltCircle3D";
pltSphere3D::usage="pltSphere3D";
pltLine3D::usage="pltLine3D";


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
		colorFunc,meshType,plotPts,opacity},
	
	center=gAssocData[input,"center"];
	normal=Normalize@gAssocData[input,"normal"];
	radius=gAssocData[input,"radius"];
	
	colorFunc=gAssocDataOpt[input,"colorFunc",Function[{x,y,z},Cyan]];
	meshType=gAssocDataOpt[input,"mesh",None];
	plotPts=gAssocDataOpt[input,"plotPts",20];
	opacity=gAssocDataOpt[input,"opacity",1];
	
	rotMat=RotationMatrix[{{0,0,1},normal}];
	
	ParametricPlot3D[rotMat.(radius*({Sin[\[Pi]/2]*Cos[\[Phi]],Sin[\[Pi]/2]Sin[\[Phi]],Cos[\[Pi]/2]}))+center,
		{\[Phi],0,2\[Pi]},
		BoundaryStyle->None,
		Mesh->meshType,
		PlotPoints->plotPts,
		Lighting->{"Ambient",White},
		PlotStyle->{Opacity[opacity]},
		ColorFunction->colorFunc,
		ColorFunctionScaling->False]
];


pltSphere3D[input_]:=Module[
	{c,r,zbias,
		colorFunc,meshType,plotPts,opacity},
	
	c=gAssocData[input,"center"];
	r=gAssocData[input,"radius"];
	
	zbias=gAssocDataOpt[input,"zbias",0];
	colorFunc=gAssocDataOpt[input,"colorFunc",Function[{x,y,z},Cyan]];
	meshType=gAssocDataOpt[input,"mesh",None];
	plotPts=gAssocDataOpt[input,"plotPts",20];
	opacity=gAssocDataOpt[input,"opacity",1];
	
	ParametricPlot3D[
		c+r*{Cos[\[Phi]]*Sin[\[Theta]],Sin[\[Phi]]*Sin[\[Theta]],Cos[\[Theta]]}*(1+zbias*2^-6),
		{\[Theta],0,\[Pi]},{\[Phi],0,2\[Pi]},
		BoundaryStyle->None,
		Mesh->meshType,
		PlotPoints->plotPts,
		Lighting->{"Ambient",White},
		PlotStyle->{Opacity[opacity]},
		ColorFunction->colorFunc,
		ColorFunctionScaling->False]
];


End[];


EndPackage[];
