(* ::Package:: *)

BeginPackage["gPlots3DEx`"];
Needs["sgCommon`"];
Needs["gUtils`"];
Needs["gBRDF`"];
Needs["gPlots`"];
Needs["gSphericalCap`"];
Needs["gBlochSphere`"];
Needs["gTexStyles`"];


ClearAll[showProps3D,pltRect3D,pltArrow3D,pltPoint3D];
showProps3D::usage="showProps3D";
pltRect3D::usage="pltRect3D";
pltArrow3D::usage="pltArrow3D";
pltPoint3D::usage="pltPoint3D";


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


pltRect3D[input_]:=Module[
	{center,normal,majorAxis,majorRadius,minorRadius,
		colorFunc,meshType,
		rMat},
	
	center=gAssocData[input,"center"];
	normal=Normalize@gAssocData[input,"normal"];
	majorAxis=Normalize@gAssocData[input,"majorAxis"];
	majorRadius=gAssocData[input,"majorRadius"];
	minorRadius=gAssocData[input,"minorRadius"];
	
	colorFunc=gAssocDataOpt[input,"colorFunc",Function[{x,y,z},Cyan]];
	meshType=gAssocDataOpt[input,"mesh",None];
	
	rMat=RotationMatrix[{{0,0,1},normal}].RotationMatrix[{{0,1,0},majorAxis}];
	
	ParametricPlot3D[
		rMat.{w,h,0}+center,
		{w,-minorRadius,minorRadius},{h,-majorRadius,majorRadius},
	    Mesh->meshType,
		Lighting->{"Ambient",White},
		ColorFunction->colorFunc,
		ColorFunctionScaling->False]
];


pltArrow3D[input_]:=Module[
	{origin,dir,length},
	
	origin=gAssocData[input,"origin"];
	dir=Normalize@gAssocData[input,"dir"];
	length=gAssocData[input,"length"];
	
	Graphics3D[{
		AbsoluteThickness[blDefaultThickness],
		Arrowheads[{{.03,1,texArrowHead}}],Arrow[{origin,origin+dir*length}]}]
];


pltPoint3D[input_]:=Module[
	{pos,size,color},

	pos=gAssocData[input,"pos"];
	size=gAssocData[input,"size"];
	color=gAssocData[input,"color"];
	
	Graphics3D[{{color,PointSize[size],Point[pos]}}]
];


End[];


EndPackage[];
