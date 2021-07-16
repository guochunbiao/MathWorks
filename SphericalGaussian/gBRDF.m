(* ::Package:: *)

SetDirectory[NotebookDirectory[]];
BeginPackage["gBRDF`"];
Needs["gUtils`"];


ClearAll[gPointLightFallOff,gPhongNDF,gDGGX,gDGGX2,gVisSmith,gFresnelOrigin,gBrdfFunc,
	gSolveSamplingHalfDir,gSamplingHalfDir,gSamplingLightDir,gSamplingLightDir2D,
	gPlotGgxPdf3D,gPlotGgxPdf2D,gCalcGgxPeakOnPlane,gCalcGgxPeakForLight];
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
gCalcGgxPeakOnPlane::usage="gCalcGgxPeakOnPlane";
gCalcGgxPeakForLight::usage="gCalcGgxPeakForLight";


Begin["`Private`"];
On[Assert];


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


gCalcGgxPeakForLight[peakPt_,planeNormal_,viewPt_]:=Module[
	{halfDir,viewDir,lightDir},
	
	halfDir=Normalize[planeNormal];
	viewDir=Normalize[viewPt-peakPt];
	
	lightDir=2*Dot[viewDir,halfDir]*halfDir-viewDir;
	
	lightDir
];


End[];


EndPackage[];
