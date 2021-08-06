(* ::Package:: *)

BeginPackage["gUtils`"];
SetDirectory[FileNameJoin@{ParentDirectory[NotebookDirectory[]],"Shared"}];
(*Needs[]*)
ResetDirectory[];


ClearAll[gStructRules,gPrint,gPrintFunc,gEvalFunc,gCreateCone,gLerp,gRemap,gClampPhi,
	gCalcRectCorners,gReflectVector,gAssocData,gAssocDataOpt,gCircIntsRectPts,
	gCircIntsRectArea,gCircIntsRectAreaDebug,gCalcPlaneTangents,gCalcRect2DEdgePts];
gPrint::usage="Print messages";
gPrintFunc::usage="Print a function";
gEvalFunc::usage="Evaluate a funciton";
gQuickFinMin::usage="Quick find minimum";
gCreateCone::usgae="Create a cone";
gLerp::usage="gLerp";
gRemap::usage="gRemap";
gClampPhi::usage="gClampPhi";
gCalcRectCorners::usage="gCalcRectCorners";
gReflectVector::usage="gReflectVector";
gAssocData::usage="gAssocData";
gAssocDataOpt::usage="gAssocDataOpt";
gCircIntsRectPts::usage="gCircIntsRectPts";
gCircIntsRectArea::usage="gCircIntsRectArea";
gCircIntsRectAreaDebug::usage="gCircIntsRectAreaDebug";
gCalcPlaneTangents::usage="gCalcPlaneTangents";
gCalcRect2DEdgePts::usage="gCalcRect2DEdgePts";


SetAttributes[gStructRules,HoldAll]
gStructRules[rules_,expr_]:=First@PreemptProtect@Internal`InheritedBlock[
	{Rule,RuleDelayed},
	SetAttributes[{Rule,RuleDelayed},HoldFirst];
	Hold[expr]/.rules];


Begin["`Private`"];


gPrint[msgs__]:=Module[
{narg,text},
narg=Length[List[msgs]];
text="";
For[i=1,i<=narg,i++,text=StringJoin[text," ",ToString[List[msgs][[i]]]]];
       Print[Style[text,FontSize->18,Background->LightBlue]];
];


gPrintFunc[name_,func_]:=Module[
{ret},
ret=Print[Style[
StringJoin[ToString[name],"=",ToString[TraditionalForm[func]]],
FontSize->18,Background->LightBlue]];
ret
];


gEvalFunc[name_,func_]:=Print[Style[
StringJoin[ToString[name],"=",ToString[func]], FontSize->18,Background->LightBlue]];


gCreateCone[originPt_,refPt0_,refPt1_]:=Module[
	{dirAngle,apertureAngle,refTheta0,refTheta1,minTheta,maxTheta},
	refTheta0=ToPolarCoordinates[refPt0-originPt][[2]];
	refTheta1=ToPolarCoordinates[refPt1-originPt][[2]];
	minTheta=Min[refTheta0,refTheta1];
	If[minTheta<0,minTheta=minTheta+2\[Pi]];
	maxTheta=Max[refTheta0,refTheta1];
	If[maxTheta<0,maxTheta=maxTheta+2\[Pi]];
	dirAngle=(maxTheta+minTheta)/2;
	apertureAngle=(maxTheta-minTheta)/2;
	
	{dirAngle,apertureAngle}
];


gLerp[from_,to_,factor_]:=Module[
	{t,interpolated},
	t=Clip[factor,{0,1}];
	
	interpolated=from+(to-from)*t;
	interpolated
];


gRemap[x_,oldRange_,newRange_]:=Module[
	{os,oe,ns,ne,factor,nx},
	os=oldRange[[1]];
	oe=oldRange[[2]];
	ns=newRange[[1]];
	ne=newRange[[2]];
	
	factor=(x-os)/(oe-os);
	nx=ns+factor*(ne-ns);
	
	nx
];


gClampPhi[inPhi_]:=Module[
	{outPhi},
	
	outPhi=If[inPhi<0,inPhi+2\[Pi],inPhi];
	outPhi
];


gCalcRectCorners[rectInput_]:=Module[
	{rectCenter,rectNormal,rectMajorAxis,rectMinorAxis,rectMajorRadius,rectMinorRadius,
		leftTop,rightTop,leftBtm,rightBtm},
	
	rectCenter=rectInput[[1]];
	rectNormal=Normalize[rectInput[[2]]];
	rectMajorAxis=Normalize[rectInput[[3]]];
	Assert[Dot[rectNormal,rectMajorAxis]==0];
	rectMinorAxis=Normalize@Cross[rectNormal,rectMajorAxis];
	rectMajorRadius=rectInput[[4]];
	rectMinorRadius=rectInput[[5]];
	
	leftTop=rectCenter-rectMinorRadius*rectMinorAxis+rectMajorAxis*rectMajorRadius;
	rightTop=rectCenter+rectMinorRadius*rectMinorAxis+rectMajorAxis*rectMajorRadius;
	leftBtm=rectCenter-rectMinorRadius*rectMinorAxis-rectMajorAxis*rectMajorRadius;
	rightBtm=rectCenter+rectMinorRadius*rectMinorAxis-rectMajorAxis*rectMajorRadius;
	
	{leftTop,rightTop,leftBtm,rightBtm}
];


gReflectVector[viewOrLight_,half_]:=Module[
	{o,h,i},
	
	o=Normalize@viewOrLight;
	h=Normalize@half;
	i=Normalize[2Dot[o,h]*h-o];
	
	i
];


gAssocData[assoc_,key_]:=Module[
	{},
	
	Assert[MemberQ[Keys[assoc],key]];
	assoc[key]
];


gAssocDataOpt[assoc_,key_,default_]:=Module[
	{},
	
	If[MemberQ[Keys[assoc],key],assoc[key],default]
];


gCalcPlaneTangents[normal_,inMajorAxisAssit_]:=Module[
	{majorAxisAssit,majorAxis,minorAxis},

	majorAxisAssit=Normalize@inMajorAxisAssit;
	minorAxis=Normalize@Cross[normal,majorAxisAssit];
	majorAxis=Normalize@Cross[minorAxis,normal];
	
	{majorAxis,minorAxis}
];


gCircIntsRectAreaDebug[circCenter_,circRadius_,
	rectCenter_,inRectMajorAxis_,inRectMinorAxis_,rectMajorRadius_,rectMinorRadius_]:=Module[
	{rCenter,rLeft,rRight,rTop,rBottom,rMajorAxis,rMinorAxis,
	 cDistX,cDistY,cCenter,cLeft,cRight,cTop,cBottom,
	 xOverlap,yOverlap,area,
	 newLeft,newRight,newTop,newBottom},
	 
	 rMajorAxis=Normalize@inRectMajorAxis;
	 rMinorAxis=Normalize@inRectMinorAxis;
	
	(*assuming major axis of rectangle is left-right*)
	(*setting to the center of rectangle*)
	rCenter={0,0};
	rLeft=-rectMajorRadius;
	rRight=rectMajorRadius;
	rTop=rectMinorRadius;
	rBottom=-rectMinorRadius;
	
	cDistX=Dot[circCenter-rectCenter,rMajorAxis];
	cDistY=Dot[circCenter-rectCenter,rMinorAxis];
	cLeft=cDistX-circRadius;
	cRight=cDistX+circRadius;
	cTop=cDistY+circRadius;
	cBottom=cDistY-circRadius;
	
	(*restore to the absolute position*)
	newLeft=Max[rLeft,cLeft];
	newRight=Min[rRight,cRight];
	newTop=Min[rTop,cTop];
	newBottom=Max[rBottom,cBottom];
	
	If[newLeft>newRight,newLeft=newRight];
	If[newBottom>newTop,newBottom=newTop];
	
	{newLeft,newRight,newBottom,newTop}
];


gCircIntsRectArea[circCenter_,circRadius_,
	rectCenter_,inRectMajorAxis_,inRectMinorAxis_,rectMajorRadius_,rectMinorRadius_]:=Module[
	{rCenter,rLeft,rRight,rTop,rBottom,rMajorAxis,rMinorAxis,
	 cDistX,cDistY,cCenter,cLeft,cRight,cTop,cBottom,
	 xOverlap,yOverlap,area},
	 
	 rMajorAxis=Normalize@inRectMajorAxis;
	 rMinorAxis=Normalize@inRectMinorAxis;
	
	(*assuming major axis of rectangle is left-right*)
	rCenter={0,0};
	rLeft=-rectMajorRadius;
	rRight=rectMajorRadius;
	rTop=rectMinorRadius;
	rBottom=-rectMinorRadius;
	
	cDistX=Dot[circCenter-rectCenter,rMajorAxis];
	cDistY=Dot[circCenter-rectCenter,rMinorAxis];
	cLeft=cDistX-circRadius;
	cRight=cDistX+circRadius;
	cTop=cDistY+circRadius;
	cBottom=cDistY-circRadius;
	
	(*https://math.stackexchange.com/questions/99565/simplest-way-to-calculate-the-intersect-area-of-two-rectangles*)
	xOverlap=Max[0,Min[rRight,cRight]-Max[rLeft,cLeft]];
	yOverlap=Max[0,Min[rTop,cTop]-Max[rBottom,cBottom]];
	area=xOverlap*yOverlap;
	
	area
];


gCircIntsRectPts[circCenter_,circRadius_,
	rectCenter_,inRectMajorAxis_,inRectMinorAxis_,rectMajorRadius_,rectMinorRadius_]:=Module[
	{rCenter,rLeft,rRight,rTop,rBottom,rMajorAxis,rMinorAxis,
	 cDistX,cDistY,cCenter,cLeft,cRight,cTop,cBottom,
	 iLeftTop,iRightTop,iRightBtm,iLeftBtm},
	 
	 rMajorAxis=Normalize@inRectMajorAxis;
	 rMinorAxis=Normalize@inRectMinorAxis;
	
	(*assuming major axis of rectangle is left-right*)
	rCenter={0,0};
	rLeft=-rectMajorRadius;
	rRight=rectMajorRadius;
	rTop=rectMinorRadius;
	rBottom=-rectMinorRadius;
	
	cDistX=Dot[circCenter-rectCenter,rMajorAxis];
	cDistY=Dot[circCenter-rectCenter,rMinorAxis];
	cLeft=cDistX-circRadius;
	cRight=cDistX+circRadius;
	cTop=cDistY+circRadius;
	cBottom=cDistY-circRadius;
	
	(*https://math.stackexchange.com/questions/99565/simplest-way-to-calculate-the-intersect-area-of-two-rectangles*)
	iLeftTop=rectCenter+rMajorAxis*Max[rLeft,cLeft]+rMinorAxis*Min[rTop,cTop];
	iRightTop=rectCenter+rMajorAxis*Min[rRight,cRight]+rMinorAxis*Min[rTop,cTop];
	iRightBtm=rectCenter+rMajorAxis*Min[rRight,cRight]+rMinorAxis*Max[rBottom,cBottom];
	iLeftBtm=rectCenter+rMajorAxis*Max[rLeft,cLeft]+rMinorAxis*Max[rBottom,cBottom];
	
	{iLeftTop,iRightTop,iRightBtm,iLeftBtm,iLeftTop}
];


gCalcRect2DEdgePts[center_,inMajorAxis_,majorRadius_,minorRadius_]:=Module[
	{majorAxis,minorAxis,pointLeft,pointRight,pointLT,pointRT,pointRB,pointLB},
	
	majorAxis=Normalize@inMajorAxis;
	minorAxis={-majorAxis[[2]],majorAxis[[1]]};
	
	pointLeft=center-majorAxis*majorRadius;
	pointRight=center+majorAxis*majorRadius;
	pointLT=pointLeft+minorAxis*minorRadius;
	pointLB=pointLeft-minorAxis*minorRadius;
	pointRT=pointRight+minorAxis*minorRadius;
	pointRB=pointRight-minorAxis*minorRadius;
	
	{pointLT,pointRT,pointRB,pointLB}
];


End[];


EndPackage[];
