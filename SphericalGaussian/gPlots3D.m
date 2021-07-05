(* ::Package:: *)

BeginPackage["gPlots3D`"];
Needs["sgCommon`"];
Needs["gUtils`"];
Needs["gBRDF`"];
Needs["gPlots`"];
Needs["gSphericalCap`"];


ClearAll[gParamPlot3D];
gParamPlot3D::usage="function{gParamPlot3D}";


Begin["`Private`"];


ClearAll[gParamLine3D];
gParamLine3D[input_,globalInput_,x_,y_,z_,\[Phi]_,\[Theta]_]:=Module[
	{startPos,dirVector,length},
	startPos=input["startPos"];
	dirVector=input["dirVec"];
	length=input["length"];
	
	startPos+(length/\[Pi])*\[Theta]*Normalize[dirVector]
];


(*RegionPlot3D*)
ClearAll[gParamDisk3D];
gParamDisk3D[input_,globalInput_,x_,y_,z_]:=Module[
	{center,radius,normalAxis,region},
	
	center=input["center"];
	radius=input["radius"];
	normalAxis=Normalize[input["normalAxis"]];
	
	region=DiscretizeRegion@ImplicitRegion[
		{
			Dot[{x,y,z}-center,normalAxis]==0&&
			Norm[{x,y,z}-center]<=radius
		},
		{x,y,z}];
	
	region
];


(*RegionPlot3D*)
ClearAll[gParamRect];
gParamRect[input_,globalInput_,x_,y_,z_]:=Module[
	{centerPt,normalAxis,majorAxis,minorAxis,majorRadius,minorRadius,
		region},
	
	centerPt=input["center"];
	normalAxis=Normalize[input["normalAxis"]];
	majorAxis=Normalize[input["majorAxis"]];
	Assert[Dot[normalAxis,majorAxis]==0];
	minorAxis=Normalize@Cross[normalAxis,majorAxis];
	majorRadius=input["majorRadius"];
	minorRadius=input["minorRadius"];
	
	region=DiscretizeRegion@ImplicitRegion[
		{
			Dot[{x,y,z}-centerPt,normalAxis]==0&&
			Abs@Dot[({x,y,z}-centerPt),majorAxis]<=majorRadius&&
            Abs@Dot[({x,y,z}-centerPt),minorAxis]<=minorRadius
        },
		{x,y,z}];
		
	region
];


(*projection of rectangle onto sphere*)
(*https://mathematica.stackexchange.com/questions/83550/projection-of-triangles-onto-a-sphere*)
(*https://mathematica.stackexchange.com/questions/78705/plot-a-partition-of-the-sphere-given-vertices-of-polygons*)
ClearAll[gParamProjRect];
gParamProjRect[input_,globalInput_,x_,y_,z_]:=Module[
	{inputKeys,rectInput,rectCenter,rectNormal,rectMajorAxis,rectMinorAxis,
		rectMajorRadius,rectMinorRadius,rectLT,rectRT,rectLB,rectRB,
		sphCenter,sphRadius,p1,p2,p3,p4,
		projFlag,zbias,region},
		
	inputKeys=Keys[input];
	
	(*rectangle*)
	rectInput=input["rect"];
	rectCenter=rectInput[[1]];
	rectNormal=Normalize[rectInput[[2]]];
	rectMajorAxis=Normalize[rectInput[[3]]];
	Assert[Dot[rectNormal,rectMajorAxis]==0];
	rectMinorAxis=Normalize@Cross[rectNormal,rectMajorAxis];
	rectMajorRadius=rectInput[[4]];
	rectMinorRadius=rectInput[[5]];
	
	{rectLT,rectRT,rectLB,rectRB}=gCalcRectCorners[rectInput];
	
	(*sphere*)
	sphCenter=input["sphCenter"];
	sphRadius=input["sphRadius"];
	
	{p1,p2,p3,p4}={
			Normalize[rectLT-sphCenter],
			Normalize[rectRT-sphCenter],
			Normalize[rectRB-sphCenter],
			Normalize[rectLB-sphCenter]
		};
		
	(*project rectangle onto sphere*)
	zbias=If[MemberQ[inputKeys,"zbias"],input["zbias"],0];
	region=DiscretizeRegion@ImplicitRegion[
		{
			(x-sphCenter[[1]])^2+(y-sphCenter[[2]])^2+(z-sphCenter[[3]])^2==
				(sphRadius*(1+zbias*2^-6))^2&&
	   	 projFlag*Normalize[{x,y,z}-sphCenter].Cross[p1,p2]>0&&
	   	 projFlag*Normalize[{x,y,z}-sphCenter].Cross[p2,p3]>0&&
	   	 projFlag*Normalize[{x,y,z}-sphCenter].Cross[p3,p4]>0&&
	   	 projFlag*Normalize[{x,y,z}-sphCenter].Cross[p4,p1]>0
	   },
	   {x,y,z}];

	region
];


ClearAll[gParamProjDisk];
gParamProjDisk[input_,globalInput_,x_,y_,z_]:=Module[
	{inputKeys,diskInput,diskCenter,diskNormal,diskRadius,
		diskTangentZ,diskBiTangentZ,diskTangent,diskBiTangent,diskPlane,
		sphCenter,sphRadius,sphPlane,
		projFlag,zbias,region,transFunc},
		
	inputKeys=Keys[input];
	
	(*disk*)
	diskInput=input["disk"];
	diskCenter=diskInput[[1]];
	diskNormal=Normalize[diskInput[[2]]];
	diskRadius=diskInput[[3]];
	
	diskTangentZ=Solve[Dot[diskNormal,{1,0,x}]==0,x][[All,1,2]][[1]];
	diskTangent=Normalize[{1,0,diskTangentZ}];
	diskBiTangentZ=Solve[Dot[diskNormal,{0,1,x}]==0,x][[All,1,2]][[1]];
	diskBiTangent=Normalize[{0,1,diskBiTangentZ}];
	diskPlane=InfinitePlane[diskCenter,{diskTangent,diskBiTangent}];
	
	(*sphere*)
	sphCenter=input["sphCenter"];
	sphRadius=input["sphRadius"];
	
	(*transformation function from point to disk*)
	(*https://mathematica.stackexchange.com/questions/63259/compose-many-geometric-transformations-for-3d-graphics*)
	transFunc=Composition@@{RotationTransform[
		ArcCos@Dot[{0,0,1},diskNormal],{0,0,1}],TranslationTransform[-diskCenter]};
		
	(*project rectangle onto sphere*)
	zbias=If[MemberQ[inputKeys,"zbias"],input["zbias"],0];
	region=DiscretizeRegion@ImplicitRegion[
	   Block[
	   {flag1,flag2,flag3,transPt,diff,prod1,prod2,prod3,rayDir,intsPt},
		
		(*zbias*)
		flag1=(x-sphCenter[[1]])^2+(y-sphCenter[[2]])^2+(z-sphCenter[[3]])^2==
				(sphRadius*(1+zbias*2^-6))^2;	
		(*point on hemi-sphere*)
		flag2=(z-sphCenter[[3]]>=0);
		(*sphere points can be projected onto disk*)
		(*transformed point on disk space*)
		(*transPt=transFunc[{x,y,z}];
		transPt[[3]]=0;
		flag3=((Norm@transPt)\[LessEqual]diskRadius);*)
		
		(*https://www.rosettacode.org/wiki/Find_the_intersection_of_a_line_with_a_plane#C.2B.2B*)
		(*c++ code example*)
		(*rayDir={0,0,1};*)
		rayDir={x,y,z};
		diff={x,y,z}-diskCenter;
		prod1=Dot[diff,diskNormal];
		prod2=Dot[rayDir,diskNormal];
		If[
			prod2==0,flag3=False,
			prod3=prod1/prod2;
			intsPt={x,y,z}-rayDir*prod3;
			flag3=Norm[intsPt-diskCenter]<=diskRadius
		];
	   
	    flag1&&flag2&&flag3
	   ],
	   {x,y,z}];

	region
];


ClearAll[gParamSpherLine];
gParamSpherLine[input_,globalInput_,x_,y_,z_,\[Phi]_,\[Theta]_]:=Module[
	{inputKeys,c,r,zbias,
		startPolars,endPolars,spherePts,
		i,\[Theta]1,\[Theta]2,\[Phi]1,\[Phi]2,spherLines,phiFactor,thetaFactor,currPhi,currTheta},
	inputKeys=Keys[input];
	c=input["spherCenter"];
	r=input["spherRadius"];
	zbias=If[MemberQ[inputKeys,"zbias"],input["zbias"],0];
	startPolars=input["startPts"];
	endPolars=input["endPts"];
	
	spherePts={};
	
	Assert[Length[startPolars]==Length[endPolars]];
	For[i=1,i<=Length[startPolars],i++,
		\[Theta]1=startPolars[[i]][[1]];
		\[Phi]1=startPolars[[i]][[2]];
		\[Theta]2=endPolars[[i]][[1]];
		\[Phi]2=endPolars[[i]][[2]];
		
		Assert[\[Theta]1<=\[Theta]2];
		Assert[\[Phi]1<=\[Phi]2];
		
		currPhi=If[\[Phi]1<0,\[Phi]-\[Pi],\[Phi]];
		currPhi=Clip[currPhi,{\[Phi]1,\[Phi]2}];
		currTheta=Clip[\[Theta],{\[Theta]1,\[Theta]2}];
		
		AppendTo[spherePts,
			c+r*{Cos[currPhi]*Sin[currTheta],Sin[currPhi]*Sin[currTheta],Cos[currTheta]}
				*(1+zbias*2^-6)]
	];
	
	spherePts
];


ClearAll[gParamSphere];
gParamSphere[input_,globalInput_,x_,y_,z_,\[Phi]_,\[Theta]_]:=Module[
	{inputKeys,c,r,zbias},
	inputKeys=Keys[input];
	c=input["center"];
	r=input["radius"];
	zbias=If[MemberQ[inputKeys,"zbias"],input["zbias"],0];
	
	c+r*{Cos[\[Phi]]*Sin[\[Theta]],Sin[\[Phi]]*Sin[\[Theta]],Cos[\[Theta]]}*(1+zbias*2^-6)
];


ClearAll[gParamSpherRange];
gParamSpherRange[input_,globalInput_,x_,y_,z_,\[Phi]_,\[Theta]_]:=Module[
	{inputKeys,c,r,spherePt,axisDir,minTheta,maxTheta,minPhi,maxPhi,vis,vis2,zbias,
		rebasedSpherePt,rebasedPolarPt,rebasedTheta,rebasedPhi},
	inputKeys=Keys[input];
	c=input["center"];
	r=input["radius"];
	axisDir=input["axisDir"];
	zbias=If[MemberQ[inputKeys,"zbias"],input["zbias"],0];
	
	minTheta=input["rangeTheta"][[1]];
	maxTheta=input["rangeTheta"][[2]];
	minPhi=input["rangePhi"][[1]];
	maxPhi=input["rangePhi"][[2]];
	
	spherePt={Cos[\[Phi]]*Sin[\[Theta]],Sin[\[Phi]]*Sin[\[Theta]],Cos[\[Theta]]};	
	vis=(minTheta<=\[Theta]<=maxTheta&&minPhi<=\[Phi]<=maxPhi);
	vis2=If[minPhi<0,minTheta<=\[Theta]<=maxTheta&&(minPhi+2\[Pi])<=\[Phi]<=2\[Pi],0];
	
	c+Boole[vis||vis2]*r*spherePt*(1+zbias*2^-6)
];


ClearAll[gLocalSpherCapVis];
gLocalSpherCapVis[spherCapInput_,x_,y_,z_,\[Phi]_,\[Theta]_]:=Module[
	{coneDir,coneAperture},
	coneDir=Normalize[spherCapInput["coneDir"]];
	coneAperture=spherCapInput["coneAperture"];
	
	gSpherCapVis[coneDir,coneAperture,\[Phi],\[Theta]]
];


ClearAll[gParamSpherCap];
gParamSpherCap[input_,globalInput_,x_,y_,z_,\[Phi]_,\[Theta]_]:=Module[
	{inputKeys,center,radius,zbias,coneDir,coneAperture,spherePt,deltaCos},
	inputKeys=Keys[input];
	center=input["center"];
	radius=input["radius"];
	zbias=If[MemberQ[inputKeys,"zbias"],input["zbias"],0];
	
	spherePt={Cos[\[Phi]]*Sin[\[Theta]],Sin[\[Phi]]*Sin[\[Theta]],Cos[\[Theta]]};
	center+(radius*gLocalSpherCapVis[input,x,y,z,\[Phi],\[Theta]])*spherePt*(1+zbias*2^-6)
];


ClearAll[gLocalSpherCapBorderVis];
gLocalSpherCapBorderVis[spherCapInput_,x_,y_,z_,\[Phi]_,\[Theta]_]:=Module[
	{coneDir,coneAperture},
	coneDir=Normalize[spherCapInput["coneDir"]];
	coneAperture=spherCapInput["coneAperture"];
	
	gSpherCapBorderVis[coneDir,coneAperture,\[Phi],\[Theta]]
];


ClearAll[gParamSpherCapBorder];
gParamSpherCapBorder[input_,globalInput_,x_,y_,z_,\[Phi]_,\[Theta]_]:=Module[
	{inputKeys,center,radius,zbias,coneDir,coneAperture,spherePt,deltaCos},
	inputKeys=Keys[input];
	center=input["center"];
	radius=input["radius"];
	zbias=If[MemberQ[inputKeys,"zbias"],input["zbias"],0];
	
	spherePt={Cos[\[Phi]]*Sin[\[Theta]],Sin[\[Phi]]*Sin[\[Theta]],Cos[\[Theta]]};
	coneDir=Normalize[input["coneDir"]];
	coneAperture=input["coneAperture"];
	
	center+(radius*gLocalSpherCapBorderVis[input,x,y,z,\[Phi],\[Theta]])*spherePt*(1+zbias*2^-6)
];


ClearAll[gParamSpherSeg];
gParamSpherSeg[input_,globalInput_,x_,y_,z_,\[Phi]_,\[Theta]_]:=Module[
	{inputKeys,center,radius,zbias,segAxis,segH1,segH2,sphereVec},
	inputKeys=Keys[input];
	center=input["center"];
	radius=input["radius"];
	zbias=If[MemberQ[inputKeys,"zbias"],input["zbias"],0];
	segAxis=Normalize[input["segAxis"]];
	segH1=input["segH1"];
	segH2=input["segH2"];
	
	On[Assert];
	Assert[segH1>=0&&segH2<=2&&segH1<segH2];
	
	sphereVec={Cos[\[Phi]]*Sin[\[Theta]],Sin[\[Phi]]*Sin[\[Theta]],Cos[\[Theta]]};
	center+(radius*gSpherSegmentVis[segAxis,segH1,segH2,\[Phi],\[Theta]])*sphereVec*(1+zbias*2^-6)
];


ClearAll[gParamSpherCapInts];
gParamSpherCapInts[input_,globalInput_,x_,y_,z_,\[Phi]_,\[Theta]_]:=Module[
	{inputKeys,capIndex1,capIndex2,zbias,spherCapInputs,spherCapNum,
		capInput1,capInput2,capVis1,capVis2,
		capCenter,capRadius,spherePt},
	inputKeys=Keys[input];
	
	capIndex1=input["spherCapPair"][[1]];
	capIndex2=input["spherCapPair"][[2]];
	zbias=If[MemberQ[inputKeys,"zbias"],input["zbias"],0];
	
	spherCapInputs=globalInput[["spherCaps"]];
	spherCapNum=Length[spherCapInputs];
	On[Assert];
	Assert[capIndex1<=spherCapNum && capIndex2<=spherCapNum];
	
	capInput1=spherCapInputs[[capIndex1]];
	capInput2=spherCapInputs[[capIndex2]];
	capCenter=capInput1[["center"]];
	capRadius=capInput1[["radius"]];
	Assert[capCenter==capInput2[["center"]]];
	Assert[capRadius==capInput2[["radius"]]];
	
	capVis1=gLocalSpherCapVis[capInput1,x,y,z,\[Phi],\[Theta]];
	capVis2=gLocalSpherCapVis[capInput2,x,y,z,\[Phi],\[Theta]];
	
	spherePt={Cos[\[Phi]]*Sin[\[Theta]],Sin[\[Phi]]*Sin[\[Theta]],Cos[\[Theta]]};
	capCenter+(capRadius*capVis1*capVis2)*spherePt*(1+zbias*2^-6)
];


ClearAll[gLocalSpherCapVis2];
gLocalSpherCapVis2[cap_,\[Phi]_,\[Theta]_]:=Module[
	{},

	gSpherCapVis[cap[[1]],cap[[2]],\[Phi],\[Theta]]
];


ClearAll[gParamSpherCapInts2];
gParamSpherCapInts2[input_,globalInput_,x_,y_,z_,\[Phi]_,\[Theta]_]:=Module[
	{inputKeys,capIndex1,capIndex2,zbias,
		cap1,cap2,capVis1,capVis2,
		capCenter,capRadius,spherePt},
	inputKeys=Keys[input];
	
	capCenter=input[["center"]];
	capRadius=input[["radius"]];
	cap1=input["cap1"];
	cap2=input["cap2"];
	zbias=If[MemberQ[inputKeys,"zbias"],input["zbias"],0];
	
	capVis1=gLocalSpherCapVis2[cap1,\[Phi],\[Theta]];
	capVis2=gLocalSpherCapVis2[cap2,\[Phi],\[Theta]];
	
	spherePt={Cos[\[Phi]]*Sin[\[Theta]],Sin[\[Phi]]*Sin[\[Theta]],Cos[\[Theta]]};
	capCenter+(capRadius*capVis1*capVis2)*spherePt*(1+zbias*2^-6)
];


ClearAll[gParamGgxPDF3];
gParamGgxPDF3[input_,globalInput_,x_,y_,z_,\[Theta]_,\[Phi]_]:=Module[
	{center,m,viewDir},
	center=input[["center"]];
	m=input[["roughness"]];
	viewDir=input[["viewDir"]];

	center+gPlotGgxPdf3D[m,viewDir,\[Theta],\[Phi]]
];


ClearAll[gMultiColorFunctionBackup];
gMultiColorFunctionBackup/:(h:(Plot|Plot3D|ParametricPlot|ParametricPlot3D))[
	{fs__},before___,gMultiColorFunctionBackup[cf__],after___]:=
		Show[h[#1,before,
			ColorFunction->#2[[1]],
			PlotStyle->#2[[2]],
			(*BoundaryStyle\[Rule]Green,*)
			PlotPoints->#2[[3]],
			Mesh->#2[[4]],
			after]&@@@Transpose[{{fs},cf}]];


gParamPlot3D[inputs_,imageSize_:Tiny]:=Module[
	{
		inputKeys,collectFunc,plotCmds,
		plotList,plotListTypes,plotStyles,plotLabels,
		colorFuncList,opacityList,thicknessList,plotPtsList,meshTypeList,
		axisExtent,projSettings,viewPoint,viewProj,
		tagPlot
	},
	inputKeys=Keys[inputs];
	plotList={};
	plotStyles={};
	plotLabels={};
	colorFuncList={};
	opacityList={};
	thicknessList={};
	plotPtsList={};
	meshTypeList={};
	(*
		Enumeration for plot types
		1. ParametricPlot3D(theta, phi)
		2. ParametricPlot3D(theta)
		3. RegionPlot3D
	 *)
	plotListTypes={};
	
	collectFunc[keyName_,paramFunc_,plotType_:1]:=Block[
		{elements,element,evaluated,elementKeys,
			tmpColorFunc,tmpOpacity,tmpThickness,tmpPlotPts,tmpMeshType},
		
		If[MemberQ[inputKeys,keyName],
			elements=inputs[[keyName]];
			For[i=1,i<=Length[elements],i++,
				element=elements[[i]];
				evaluated=Which[
					plotType==1||plotType==2,
					paramFunc[element,inputs,x,y,z,\[Phi],\[Theta]],
					plotType==3,
					paramFunc[element,inputs,x,y,z]
					];
				(*AppendTo[plotList,paramFunc[element,\[Phi],\[Theta]]];*)
				elementKeys=Keys[element];
				tmpColorFunc=If[MemberQ[elementKeys,"colorFunc"],
					element["colorFunc"],Cyan];
				tmpOpacity=If[MemberQ[elementKeys,"opacity"],element["opacity"],1];
				tmpThickness=If[MemberQ[elementKeys,"thickness"],element["thickness"],0.01];
				tmpPlotPts=If[MemberQ[elementKeys,"plotPts"],element["plotPts"],10];
				tmpMeshType=If[MemberQ[elementKeys,"mesh"],element["mesh"],Full];
				
				AppendTo[colorFuncList,tmpColorFunc];
				AppendTo[opacityList,tmpOpacity];
				AppendTo[thicknessList,tmpThickness];
				AppendTo[plotPtsList,tmpPlotPts];
				AppendTo[meshTypeList,tmpMeshType];
				
				AppendTo[plotListTypes,plotType];
				AppendTo[plotList,evaluated];
				AppendTo[plotStyles,{tmpColorFunc,{Opacity[tmpOpacity],Thickness[tmpThickness]},
							tmpPlotPts,tmpMeshType}];
				AppendTo[plotLabels,If[MemberQ[elementKeys,"label"],element["label"],""]];
			];
		];
	];
	
	(*append lines*)
	collectFunc["lines",gParamLine3D,2];
	(*append circles*)
	collectFunc["disks",gParamDisk3D,3];
	(*append rectangles*)
	collectFunc["rects",gParamRect,3];
	(*append projection of rectangles onto sphere*)
	collectFunc["projRects",gParamProjRect,3];
	(*append projection of disks onto sphere*)
	collectFunc["projDisks",gParamProjDisk,3];
	(*append spheres*)
	collectFunc["spheres",gParamSphere];
	(*append spherical caps*)
	collectFunc["spherCaps",gParamSpherCap];
	(*append spherical cap borders*)
	collectFunc["spherCapBorders",gParamSpherCapBorder];
	(*append intersections of spherical caps*)
	collectFunc["spherCapInts",gParamSpherCapInts];
	collectFunc["spherCapInts2",gParamSpherCapInts2];
	(*append spherical segments*)
	collectFunc["spherSegs",gParamSpherSeg];
	(*append spherical ranges*)
	collectFunc["spherRanges",gParamSpherRange];
	(*append spherical lines*)
	collectFunc["spherLines",gParamSpherLine];
	(*append GGX PDF 3d*)
	collectFunc["ggxPDF3",gParamGgxPDF3];
	
	axisExtent=If[MemberQ[inputKeys,"axisExtent"],inputs[["axisExtent"]],5];
	projSettings=If[MemberQ[inputKeys,"viewPoint"],
			{inputs[["viewPoint"]],"Orthographic"},{{1.3,-2.4,2},"Perspective"}];
	viewPoint=projSettings[[1]];
	viewProj=projSettings[[2]];
	
	tagPlot/:(h:(Plot3D|ParametricPlot3D|RegionPlot3D))[
	before___,tagPlot[i_],after___]:=
		h[before,
		ColorFunction->colorFuncList[[i]],
		PlotStyle->{Opacity[opacityList[[i]]],Thickness[thicknessList[[i]]]},
		PlotPoints->plotPtsList[[i]],
		PlotLegends->plotLabels[[i]],
		Mesh->meshTypeList[[i]],
		ColorFunctionScaling->False,
		PlotRange->{{-axisExtent,axisExtent},{-axisExtent,axisExtent},{-axisExtent,axisExtent}},
		Axes->True,
		AspectRatio->1,
		AxesLabel->{"X","Y","Z"},
		PlotTheme->"Detailed",
		Lighting->{"Ambient",White},
		ViewPoint->viewPoint,
		ViewProjection->viewProj,
		ImageSize->imageSize];
	
	plotCmds={};	
	For[i=1,i<=Length[plotList],i++,
		Which[
			plotListTypes[[i]]==1,
			AppendTo[plotCmds,ParametricPlot3D[plotList[[i]],{\[Phi],0,2\[Pi]},{\[Theta],0,\[Pi]},tagPlot[i]]],
			plotListTypes[[i]]==2,
			AppendTo[plotCmds,ParametricPlot3D[plotList[[i]],{\[Theta],0,\[Pi]},tagPlot[i]]],
			plotListTypes[[i]]==3,
			AppendTo[plotCmds,RegionPlot3D[plotList[[i]],tagPlot[i]]]
		];
	];
	
	Show[plotCmds]
];


End[];


EndPackage[];
