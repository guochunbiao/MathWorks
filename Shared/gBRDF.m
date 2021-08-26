(* ::Package:: *)

BeginPackage["gBRDF`"];
SetDirectory[FileNameJoin@{ParentDirectory[NotebookDirectory[]],"Shared"}];
Needs["gUtils`"];
ResetDirectory[];


ClearAll[gPointLightFallOff,gPhongNDF,gDGGX,gDGGX2,gVisSmith,gFresnelOrigin,gBrdfFunc,
	gSolveSamplingHalfDir,gSamplingHalfDir,gSamplingLightDir,gSamplingLightDir2D,
	gPlotGgxPdf3D,gPlotGgxPdf2D,gCalcGgxPeakOnPlane,gCalcGgxPeakForLight,
	gIntegrateDiskLighting,gCalcPeakPoint,gCalcProjPoint,gIntegrateDiskLightingEx,
	gIntegrateIntsDiffuse,gIntegrateIntsDiffuseEx,gReflectDiffuse,gCircIntsRectAngles,
	gCircIntsRectIntegralRegion,gBrdfIntegralOverRange,gIntegrateDiskDiffuse];
gPointLightFallOff::usage="function{gPointLightFallOff}";
gPhongNDF::usage="function[gPhongNDF]";
gDGGX::usage="function[gDGGX]";
gDGGX2::usage="function[gDGGX2]";
gVisSmith::usage="function[gVisSmithG]";
gFresnelOrigin::usage="function[gVisSmithG]";
gBrdfFunc::usage="function[gBrdfFunc]";
gSolveSamplingHalfDir::usage="gSolveSamplingHalfDir";
gSamplingHalfDir::usage="gSamplingHalfDir";
gSamplingLightDir::usage="gSamplingLightDir";
gSamplingLightDir2D::usage="gSamplingLightDir2D";
gPlotGgxPdf3D::usage="gPlotGgxPdf3D";
gPlotGgxPdf2D::usage="gPlotGgxPdf2D";
gCalcGgxPeakOnPlane::usage="Deprecated, use gCalcPeakPoint";
gCalcGgxPeakForLight::usage="Deprecated, use gCalcPeakPoint";
gIntegrateDiskLighting::usage="gIntegrateDiskLighting";
gIntegrateDiskLightingEx::usage="gIntegrateDiskLightingEx";
gCalcPeakPoint::usage="gCalcPeakPoint";
gCalcProjPoint::usage="gCalcProjPoint";
gIntegrateIntsDiffuse::usage="gIntegrateIntsDiffuse";
gIntegrateIntsDiffuseEx::usage="gIntegrateIntsDiffuseEx";
gReflectDiffuse::usage="gReflectDiffuse";
gCircIntsRectAngles::usage="gCircIntsRectAngles";
gCircIntsRectIntegralRegion::usage="gCircIntsRectIntegralRegion";
gBrdfIntegralOverRange::usage="gBrdfIntegralOverRange";
gIntegrateDiskDiffuse::usage="gIntegrateDiskDiffuse";


Begin["`Private`"];


(*https://neil3d.github.io/assets/pdf/s2013_pbs_epic_notes_v2.pdf Page 12*)
gPointLightFallOff[radius_,dist_]:=(Clip[(1-(dist/radius)^4),{0,1}])^2/(dist^2+1);


(*Blinn Phong NDF*)
gPhongNDF[m_,noh_]:=Module[
	{a2,tmpN},
	a2=m*m;
	tmpN=2/a2-2;
	(*tmpN=Max[tmpN,0.001];
	(tmpN+2)/(2 \[Pi])*Max[(noh^tmpN),0]*)
	(tmpN+2)/(2 \[Pi])*(noh^tmpN)
];


gDGGX[m_,NoH_]:=m^2/(\[Pi]*((NoH^2*(m^2-1))+1)^2);


gDGGX2[m_,\[Theta]_,\[Phi]_]:=m^2/(\[Pi]*((Cos[\[Theta]]^2*(m^2-1))+1)^2);


ClearAll[gVisSmithG];
gVisSmithG[m_,x_]:=x+Sqrt[m^2+(1-m^2)*x^2];
gVisSmith[m_,NoL_,NoV_]:=1/(gVisSmithG[m,NoL]*gVisSmithG[m,NoV]);


ClearAll[gFresnelOriginN];
gFresnelOriginN[SpecularColor_]:=(1+Sqrt[Max[SpecularColor,0.99]])/(1-Max[SpecularColor,0.99]);
ClearAll[gFresnelOriginG];
gFresnelOriginG[VoH_,SpecularColor_]:=Sqrt[gFresnelOriginN[SpecularColor]^2+VoH^2-1];
gFresnelOrigin[VoH_,SpecularColor_:0.99]:=Module[
	{f1,f2},
	0.5*((gFresnelOriginG[VoH,SpecularColor]-VoH)/(gFresnelOriginG[VoH,SpecularColor]+VoH))^2*(
	1+(((
	gFresnelOriginG[VoH,SpecularColor]+VoH)*VoH-1)/((
	gFresnelOriginG[VoH,SpecularColor]-VoH)*VoH+1))^2)
];


(*D*G*F/(4*NoL*NoV)=D*Vis*F*)
gBrdfFunc[m_,normalDir_,lightDir_,viewDir_,specularColor_:0.99]:=Module[
	{normal,light,view,half,NoL,NoV,NoH,VoH,D,Vis,F},
	normal=Normalize[normalDir];
	light=Normalize[lightDir];
	view=Normalize[viewDir];
	half=Normalize[view+light];
	NoL=Clip[Dot[normal,light],{0,1}];
	NoV=Dot[normal,view];
	NoH=Dot[normal,half];
	VoH=Dot[view,half];

	D=gDGGX[m,NoH];
	Vis=gVisSmith[m,NoL,NoV];
	F=gFresnelOrigin[VoH,specularColor];
	
	(*D*G*F/(4*NoL*NoV)=D*Vis*F*)
	D*Vis*F
];


(*https://agraphicsguy.wordpress.com/2015/11/01/sampling-microfacet-brdf/*)
(*https://www.tobias-franke.eu/log/2014/03/30/notes_on_importance_sampling.html*)
gSolveSamplingHalfDir[m_,\[Theta]_,\[Phi]_,\[Epsilon]1_,\[Epsilon]2_]:=Module[
	{halfVectorGGX,solidAngleGGX,spherCoordGGX,intsGGXPhi,
		intsGGXTheta,cdfGGX1,cdfGGX2,cdfGGXTheta,pdfSpherical,pdfCartesian,pdfJacobian},
	
	(*GGX - half vector space*)
	halfVectorGGX[m1_,\[Theta]1_,\[Phi]1_]:=gDGGX2[m,\[Theta],\[Phi]];
	gEvalFunc["halfVectorGGX",TraditionalForm@halfVectorGGX[m,\[Theta],\[Phi]]];
	(*pdf respecting solid angle - solid angle space*)
	solidAngleGGX[m1_,\[Theta]1_,\[Phi]1_]:=halfVectorGGX[m1,\[Theta]1,\[Phi]1]*Cos[\[Theta]1];
	gEvalFunc["solidAngleGGX",TraditionalForm@solidAngleGGX[m,\[Theta],\[Phi]]];
	(*solid angle to spherical coordinate*)
	spherCoordGGX[m1_,\[Theta]1_,\[Phi]1_]:=solidAngleGGX[m1,\[Theta]1,\[Phi]1]*Sin[\[Theta]1];
	gEvalFunc["spherCoordGGX",TraditionalForm@spherCoordGGX[m,\[Theta],\[Phi]]];
	(*integrating over \[Phi]*)
	intsGGXPhi[m1_,\[Theta]1_]:=Integrate[spherCoordGGX[m1,\[Theta]1,\[Phi]],{\[Phi],0,2\[Pi]}];
	gEvalFunc["Integrate Marginal Phi",TraditionalForm@intsGGXPhi[m,\[Theta]]];
	(*integrating over \[Theta]*)
	intsGGXTheta[m1_,\[Phi]1_]:=Integrate[spherCoordGGX[m1,\[Theta],\[Phi]1],{\[Theta],0,\[Pi]/2}];
	gEvalFunc["Integrate Marginal Theta",TraditionalForm@intsGGXTheta[m,\[Phi]]];
	(*CDF*)
	cdfGGX1[m1_,\[Theta]1_]:=m1^2/(Cos[\[Theta]1]^2(m1^2-1)^2+(m1^2-1))-1/(m1^2-1);
	(*cdfGGXTheta[m1_,\[Theta]1_]=Integrate[intsGGXPhi[m1,x],{x,0,\[Theta]1}];*)
	cdfGGX2[m1_,\[Theta]1_]:=1/(1-m1^2+m1^2 /Sin[\[Theta]1]^2);
	gEvalFunc["integrateOverHemisphere",Simplify@cdfGGX1[m,\[Pi]/2]];
	(*Validate the error of solve result*)
	(*Maximize[{Abs[cdfGGX1[m,\[Theta]]-cdfGGX2[m,\[Theta]]],0.01\[LessEqual]m\[LessEqual]0.99&&0\[LessEqual]\[Theta]\[LessEqual]\[Pi]},{m,\[Theta]}];*)
	gEvalFunc["cdfGGX1",TraditionalForm@cdfGGX1[m,\[Theta]]];
	gEvalFunc["cdfGGX2",TraditionalForm@cdfGGX2[m,\[Theta]]];
	
	pdfJacobian[\[Theta]1_]:=Sin[\[Theta]1];
	pdfSpherical[m1_,\[Theta]1_,\[Phi]1_]:=spherCoordGGX[m1,\[Theta]1,\[Phi]1];
	pdfCartesian[m1_,\[Theta]1_,\[Phi]1_]:=pdfSpherical[m1,\[Theta]1,\[Phi]1]/pdfJacobian[\[Theta]1];
	
	{Solve[\[Epsilon]1==cdfGGX1[m,\[Theta]],\[Theta]],Solve[\[Epsilon]2==1/(2\[Pi]) \[Phi],\[Phi]],pdfCartesian[m,\[Theta],\[Phi]]}
];


(*sampling half vector*)
gSamplingHalfDir[m_,\[Epsilon]1_,\[Epsilon]2_]:=Module[
	{\[Theta],\[Phi],pdfCartesian},
	
	\[Theta]=ArcCos[Sqrt[(1-\[Epsilon]1)/(\[Epsilon]1*(m^2-1)+1)]];
	\[Phi]=2\[Pi]*\[Epsilon]2;
	pdfCartesian=gDGGX2[m,\[Theta],\[Phi]]*Cos[\[Theta]];
	
	{{Cos[\[Phi]]*Sin[\[Theta]],Sin[\[Phi]]*Sin[\[Theta]],Cos[\[Theta]]},pdfCartesian}
];


gSamplingLightDir[m_,inViewDir_,\[Epsilon]1_,\[Epsilon]2_]:=Module[
	{halfSampling,halfDir,pdfHalfDirCCS,viewDir,normalDir,lightDir,
		pdfJacobian,pdfLightDirCCS,reflectance},
	
	viewDir=Normalize[inViewDir];
	normalDir={0,0,1};
	
	halfSampling=gSamplingHalfDir[m,\[Epsilon]1,\[Epsilon]2];
	halfDir=halfSampling[[1]];
	pdfHalfDirCCS=halfSampling[[2]];
	
	lightDir=2*Dot[viewDir,halfDir]*halfDir-viewDir;
	
	pdfJacobian=4*Dot[viewDir,halfDir];
	pdfLightDirCCS=pdfHalfDirCCS/pdfJacobian;
	
	reflectance=gBrdfFunc[m,normalDir,lightDir,viewDir]/pdfLightDirCCS;
	
	{lightDir,pdfLightDirCCS,reflectance}
];


gSamplingLightDir2D[m_,inViewDir_,\[Epsilon]1_,\[Epsilon]2_]:=Module[
	{viewDir3D,lightDirSample,lightDir3D,lightDir2D},
	
	viewDir3D=Normalize[{inViewDir[[1]],0,inViewDir[[2]]}];
	lightDirSample=gSamplingLightDir[m,viewDir3D,\[Epsilon]1,\[Epsilon]2];
	lightDir3D=lightDirSample[[1]];
	lightDir2D=Normalize[{lightDir3D[[1]],lightDir3D[[3]]}];
	
	lightDir2D
];


gPlotGgxPdf3D[roughness_,inViewDir_,\[Theta]_,\[Phi]_]:=Module[
	{m,viewDir,\[Epsilon]1,\[Epsilon]2,halfSample,halfDir,pdfHalfCCS,
		lightSample,lightDir,pdfLightCCS},
		
	m=roughness;
	viewDir=Normalize[inViewDir];
	
	\[Epsilon]1=\[Theta]/\[Pi];
	\[Epsilon]2=\[Phi]/(2\[Pi]);

	halfSample=gSamplingHalfDir[m,\[Epsilon]1,\[Epsilon]2];
	halfDir=halfSample[[1]];
	pdfHalfCCS=halfSample[[2]];

	lightSample=gSamplingLightDir[m,viewDir,\[Epsilon]1,\[Epsilon]2];
	lightDir=lightSample[[1]];
	pdfLightCCS=lightSample[[2]];

	(*halfDir*pdfHalfCCS*)
	If[!ValueQ[lightSample]||lightDir[[3]]<0,pdfLightCCS=0];
	lightDir*pdfLightCCS
];


gPlotGgxPdf2D[roughness_,inViewDir_,\[Theta]_]:=Module[
	{viewDir3D,lightDir3D1,lightDir3D2},

	viewDir3D={inViewDir[[1]],0,inViewDir[[2]]};
	lightDir3D1=gPlotGgxPdf3D[roughness,viewDir3D,\[Theta],0];
	lightDir3D2=gPlotGgxPdf3D[roughness,viewDir3D,\[Theta],\[Pi]];

	{
		{lightDir3D1[[1]],lightDir3D1[[3]]},
		{lightDir3D2[[1]],lightDir3D2[[3]]}
	}
];


(*Deprecated, use gCalcPeakPoint*)
gCalcGgxPeakOnPlane[planePt_,planeNormal_,lightPt_,viewPt_]:=Module[
	{normalDir,tmpLightDir,tmpViewDir,lightCosine,viewCosine,
		lightVerticalDist,viewVerticalDist,lightProjPt,viewProjPt,lerpFactor,peakPt},
	
	normalDir=Normalize[planeNormal];
	tmpLightDir=Normalize[lightPt-planePt];
	tmpViewDir=Normalize[viewPt-planePt];
	
	lightCosine=Dot[planeNormal, tmpLightDir];
	viewCosine=Dot[planeNormal,tmpViewDir];
	Assert[lightCosine>=0&&viewCosine>=0];
	
	lightVerticalDist=Norm[lightPt-planePt]*lightCosine;
	viewVerticalDist=Norm[viewPt-planePt]*viewCosine;
	
	lightProjPt=lightPt-normalDir*lightVerticalDist;
	viewProjPt=viewPt-normalDir*viewVerticalDist;
	
	lerpFactor=lightVerticalDist/(lightVerticalDist+viewVerticalDist);
	peakPt=gLerp[lightProjPt,viewProjPt,lerpFactor];
	
	peakPt
];


(*Deprecated, use gCalcPeakPoint*)
gCalcGgxPeakForLight[peakPt_,planeNormal_,viewPt_]:=Module[
	{halfDir,viewDir,lightDir},
	
	halfDir=Normalize[planeNormal];
	viewDir=Normalize[viewPt-peakPt];
	
	lightDir=2*Dot[viewDir,halfDir]*halfDir-viewDir;
	
	lightDir
];


gIntegrateDiskLighting[shadingPt_,diskPt_,diskNormal_,diskRadius_,
	lightDir_,lightIntensity_,roughness_]:=Module[
	{m,n,l,nol,dr,k,percentSum,lighting,drCos,drCos2,approxSinThetaA2},
	
	m=roughness;
	n=Normalize[diskNormal];
	l=Normalize[lightDir];
	nol=Dot[n,l];
	dr=diskRadius/Norm[diskPt-shadingPt];
	
	(*17-OB_02_LeftMinorAxis-GGX.nb*)
	(*percentSum=Min[dr/Sqrt[1+dr^2],(3.672 m)/(1+nol)];*)
	(*17-OB_07_ShadingPart5-FitIntegral*)
	k=(0.288*nol)/m^2-0.673;
	drCos=dr*nol;
	drCos2=drCos*drCos;
	approxSinThetaA2=drCos2/(1+drCos2);
	percentSum=(1-Exp[-k*approxSinThetaA2])/(2 k);
	lighting=percentSum*gDGGX[m,1]*lightIntensity;
	
	lighting
];


gBrdfIntegralOverRange[nol_,m_,range_]:=Module[
	{k,drCos,drCos2,approxSinThetaA2,approxSinThetaB2},
	
	k=(0.288*nol)/m^2-0.673;
	drCos=range[[1]]*nol;
	drCos2=drCos*drCos;
	approxSinThetaA2=drCos2/(1+drCos2);
	
	drCos=range[[2]]*nol;
	drCos2=drCos*drCos;
	approxSinThetaB2=drCos2/(1+drCos2);
	
	(Exp[-k*approxSinThetaA2]-Exp[-k*approxSinThetaB2])/(2 k)
];


gIntegrateDiskLightingEx[shadingPt_,diskPt_,diskNormal_,diskRadius_,
	lightDir_,lightIntensity_,roughness_,range0_,range1_]:=Module[
	{m,n,l,nol,dr,k,percentSum,lighting,drCos,drCos2,
		s1,s2},
	
	m=roughness;
	n=Normalize[diskNormal];
	l=Normalize[lightDir];
	nol=Dot[n,l];
	dr=diskRadius/Norm[diskPt-shadingPt];
	
	(*17-OB_02_LeftMinorAxis-GGX.nb*)
	(*percentSum=Min[dr/Sqrt[1+dr^2],(3.672 m)/(1+nol)];*)
	(*17-OB_07_ShadingPart5-FitIntegral*)
	(*k=(0.288*nol)/m^2-0.673;
	drCos=dr*nol;
	drCos2=drCos*drCos;
	approxSinThetaA2=drCos2/(1+drCos2);*)
	s1=gBrdfIntegralOverRange[nol,m,range0];
	s2=gBrdfIntegralOverRange[nol,m,range1];
	percentSum=s1+s2;
	lighting=percentSum*gDGGX[m,1]*lightIntensity;
	
	lighting
];


gCalcPeakPoint[planeNormal_,planePt_,inLightDir_,viewPt_]:=Module[
	{normalDir,lightDir,halfDir,viewDir,
		viewProjDist,viewProjPt,viewPeakDist,peakPt,tmp1},
	
	normalDir=Normalize@planeNormal;
	lightDir=Normalize@inLightDir;
	halfDir=normalDir;
	viewDir=gReflectVector[lightDir,halfDir];
	
	viewProjDist=Dot[Normalize[viewPt-planePt],normalDir]*Norm[viewPt-planePt];
	(*viewProjPt=viewPt-normalDir*viewProjDist;*)
	
	If[Dot[lightDir,halfDir]<0.001,Return[{100,100,100}]];
	
	viewPeakDist=viewProjDist/Dot[lightDir,halfDir];
	peakPt=viewPt-viewDir*viewPeakDist;
	
	tmp1=Abs@Dot[peakPt-planePt,normalDir];
	Assert[tmp1<0.001,"gCalcPeakPoint"];
	
	peakPt
];


gCalcProjPoint[planeNormal_,planePt_,shadingPt_]:=Module[
	{normalDir,dir0,projDir,projDist,projPt},
	
	normalDir=Normalize@planeNormal;
	
	dir0=planePt-shadingPt;
	projDir=Dot[dir0,-normalDir];
	projDist=Norm[projDir];
	projPt=shadingPt-normalDir*projDist;
	
	projPt
];


gIntegrateIntsDiffuse[{drMin_,drMax_,sumPhi_}]:=Module[
	{approxD1,approxD2,approxD},
	
	Assert[0<=drMin<=drMax];
	Assert[0<=sumPhi<=2\[Pi]];
	(*17-OB_08_ShadingPart6-DiffuseRefl.nb*)
	(*approxD=dr^2/(1+dr^2)*2/3 \[Pi];*)
	approxD=sumPhi/3 (1/(1+drMin^2)-1/(1+drMax^2));
	
	approxD
];


gIntegrateIntsDiffuseEx[\[Theta]r1_,\[Theta]r2_,\[Phi]r1_,\[Phi]r2_,circRadius_]:=Module[
	{intsRangeA,intsRangeB,minThetaR,maxThetaR,sumPhi,intsA,intsB},
	
	Assert[\[Theta]r1<=\[Theta]r2<=circRadius];
	Assert[\[Phi]r1<=\[Phi]r2<=circRadius];
	sumPhi=(\[Phi]r2-\[Phi]r1)/(2*circRadius) \[Pi];
	
	minThetaR=Min[Abs@\[Theta]r1,Abs@\[Theta]r2];
	maxThetaR=Max[Abs@\[Theta]r1,Abs@\[Theta]r2];
	
	intsRangeA={1,0,0};
	intsRangeB={1,0,0};
	intsRangeA=If[\[Theta]r1*\[Theta]r2<0,{0,minThetaR,2\[Pi]},{minThetaR,maxThetaR,sumPhi}];
	intsRangeB=If[\[Theta]r1*\[Theta]r2<0,{minThetaR,maxThetaR,sumPhi},{0,0,0}];
	
	intsA=gIntegrateIntsDiffuse[intsRangeA];
	intsB=gIntegrateIntsDiffuse[intsRangeB];
	(*Print[{intsRangeA,intsRangeB,intsA,intsB}];*)
	
	intsA + intsB
];


gCircIntsRectAngles[circCenter_,circRadius_,
	rectCenter_,inRectMajorAxis_,inRectMinorAxis_,rectMajorRadius_,rectMinorRadius_]:=Module[
	{rLeft,rRight,rTop,rBottom,rMajorAxis,rMinorAxis,
	 cDistX,cDistY,cCenter,cLeft,cRight,cTop,cBottom,
	 xOverlap,yOverlap,area,
	 newLeft,newRight,newTop,newBottom,intsWidth,intsHeight,
	 \[Theta]r1,\[Theta]r2,\[Phi]r1,\[Phi]r2,\[Theta]Axis,\[Phi]Axis},
	 
	 rMajorAxis=Normalize@inRectMajorAxis;
	 rMinorAxis=Normalize@inRectMinorAxis;
	
	(*assuming major axis of rectangle is left-right*)
	(*setting to the center of rectangle*)
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
	newLeft=Max[rLeft,cLeft]-cDistX;
	newRight=Min[rRight,cRight]-cDistX;
	newTop=Min[rTop,cTop]-cDistY;
	newBottom=Max[rBottom,cBottom]-cDistY;
	
	If[newLeft>newRight,newLeft=newRight];
	If[newBottom>newTop,newBottom=newTop];
	
	intsWidth=newRight-newLeft;
	intsHeight=newTop-newBottom;
(*	Assert[0\[LessEqual]intsWidth\[LessEqual]2*circRadius];
	Assert[0\[LessEqual]intsHeight\[LessEqual]2*circRadius];*)
	
(*	If[intsWidth\[GreaterEqual]intsHeight,
		{\[Theta]r1=newBottom;\[Theta]r2=newTop;\[Phi]r1=newLeft;\[Phi]r2=newRight;
			\[Theta]Axis=rMinorAxis;\[Phi]Axis=rMajorAxis;\[Theta]Multi=-1;\[Phi]Multi=1;},
		{\[Theta]r1=newLeft;\[Theta]r2=newRight;\[Phi]r1=newBottom;\[Phi]r2=newTop;
			\[Theta]Axis=rMajorAxis;\[Phi]Axis=rMinorAxis;\[Theta]Multi=1;\[Phi]Multi=-1;}
		];*)
	
	(*{newLeft,newRight,newBottom,newTop,rMajorAxis,rMinorAxis,1,-1}*)
	(*{newBottom,newTop,newLeft,newRight,rMinorAxis,rMajorAxis,-1,1}*)
	If[intsWidth>=intsHeight,
		{newBottom,newTop,newLeft,newRight,rMinorAxis,rMajorAxis},
		{newLeft,newRight,newBottom,newTop,rMajorAxis,rMinorAxis}]
];


(*ClearAll[gInnerFindNearestPt];
gInnerFindNearestPt[pt1_,pt2_,pt3_,pt4_]:=Module[
	{tmp,dist1,dist2,dist3,dist4},
	
	dist1=Abs@pt1[[1]]+Abs@pt1[[2]];
	dist2=Abs@pt2[[1]]+Abs@pt2[[2]];
	dist3=Abs@pt3[[1]]+Abs@pt3[[2]];
	dist4=Abs@pt4[[1]]+Abs@pt4[[2]];
	
	tmp=If[dist1<dist2,{pt1,dist1},{pt2,dist2}];
	tmp=If[tmp[[2]]<dist3,tmp,{pt3,dist3}];
	tmp=If[tmp[[2]]<dist4,tmp,{pt4,dist4}];
	
	tmp[[1]]
];
ClearAll[gInnerFindFarestPt];
gInnerFindFarestPt[pt1_,pt2_,pt3_,pt4_]:=Module[
	{tmp,dist1,dist2,dist3,dist4},
	
	dist1=Abs@pt1[[1]]+Abs@pt1[[2]];
	dist2=Abs@pt2[[1]]+Abs@pt2[[2]];
	dist3=Abs@pt3[[1]]+Abs@pt3[[2]];
	dist4=Abs@pt4[[1]]+Abs@pt4[[2]];
	
	tmp=If[dist1>dist2,{pt1,dist1},{pt2,dist2}];
	tmp=If[tmp[[2]]>dist3,tmp,{pt3,dist3}];
	tmp=If[tmp[[2]]>dist4,tmp,{pt4,dist4}];
	
	tmp[[1]]
];
gCircIntsRectIntegralRegion[circCenter_,circRadius_,
	rectCenter_,inRectMajorAxis_,inRectMinorAxis_,rectMajorRadius_,rectMinorRadius_]:=Module[
	{circOrigin,rLeft,rRight,rTop,rBottom,rMajorAxis,rMinorAxis,
	 cDistX,cDistZ,cCenter,cLeft,cRight,cTop,cBottom,
	 xOverlap,yOverlap,area,
	 newLeft,newRight,newTop,newBottom,intsWidth,intsHeight,
	 \[Theta]r1,\[Theta]r2,\[Phi]r1,\[Phi]r2,\[Theta]Axis,\[Phi]Axis,
	 newLT,newRT,newRB,newLB,ltDist,rtDist,rbDist,lbDist,
	 nearEdgePt,farEdgePt},
	 
	 rMajorAxis=Normalize@inRectMajorAxis;
	 rMinorAxis=Normalize@inRectMinorAxis;
	
	(*assuming major axis of rectangle is left-right*)
	(*setting to the center of rectangle*)
	circOrigin={0,0,0};
	
	cLeft=-circRadius;
	cRight=circRadius;
	cTop=circRadius;
	cBottom=-circRadius;
	
	cDistX=Dot[rectCenter-circCenter,rMajorAxis];
	cDistZ=Dot[rectCenter-circCenter,rMinorAxis];
	(*rectOrigin={cDistX,0,cDistZ};*)
	rLeft=cDistX-rectMajorRadius;
	rRight=cDistX+rectMajorRadius;
	rTop=cDistZ+rectMinorRadius;
	rBottom=cDistZ-rectMinorRadius;
	(*Print[{cDistX,cDistZ,rLeft,rRight,rBottom,rTop}];*)
	
	(*origin at circle center*)
	newLeft=Max[rLeft,cLeft];
	newRight=Min[rRight,cRight];
	newTop=Min[rTop,cTop];
	newBottom=Max[rBottom,cBottom];
	
	(*Print[{newLeft,newRight,newBottom,newTop}];*)
	
	(*calculate distances to circle center*)
	
	If[newLeft>newRight,newLeft=newRight];
	If[newBottom>newTop,newBottom=newTop];
	
	intsWidth=newRight-newLeft;
	intsHeight=newTop-newBottom;
	
	newLT={newLeft,newTop};
	newRT={newRight,newTop};
	newRB={newRight,newBottom};
	newLB={newLeft,newBottom};
	
	
	
	
	(*{newLeft,newRight,newBottom,newTop,rMajorAxis,rMinorAxis,1,-1}*)
	(*{newBottom,newTop,newLeft,newRight,rMinorAxis,rMajorAxis,-1,1}*)
	If[intsWidth>=intsHeight,
		{newBottom,newTop,newLeft,newRight,rMinorAxis,rMajorAxis},
		{newLeft,newRight,newBottom,newTop,rMajorAxis,rMinorAxis}]
];*)


gReflectDiffuse[
	(*shading point*)
	shadingPt_,samplingRadius_,
	(*reflection rectangle*)
	planeCenter_,inPlaneNormal_,inPlaneAssistMajorAxis_,
		planeMajorRadius_,planeMinorRadius_,
	(*light*)
	inLightDir_,lightIntensity_
	]:=Module[
	{planeNormal,planeAssistMajorAxis,lightDir,planeMajorAxis,planeMinorAxis,
		shadingDist,projPt,dr,integratedDiffuse,planeNoL,
		intsArea,intsPercent},
	
	planeNormal=Normalize@inPlaneNormal;
	planeAssistMajorAxis=Normalize@inPlaneAssistMajorAxis;
	lightDir=Normalize@inLightDir;
	
	{planeMajorAxis,planeMinorAxis}=gCalcPlaneTangents[planeNormal,planeAssistMajorAxis];
	
	projPt=gCalcProjPoint[planeNormal,planeCenter,shadingPt];
	
	shadingDist=Norm[projPt-shadingPt];
	dr=samplingRadius/shadingDist;
	integratedDiffuse=gIntegrateDiskDiffuse[dr];
	
	planeNoL=Max[Dot[planeNormal,lightDir],0];
	
	intsArea=gCircIntsRectArea[projPt,samplingRadius,planeCenter,
		planeMajorAxis,planeMinorAxis,planeMajorRadius,planeMinorRadius];
	intsPercent=Min[1,intsArea/(4*samplingRadius*samplingRadius)];
	
	integratedDiffuse * planeNoL * lightIntensity * intsPercent
];


gIntegrateDiskDiffuse[dr_]:=Module[
	{},
	
	(2\[Pi])/3 (1-1/(1+dr^2))
];


End[];


EndPackage[];
