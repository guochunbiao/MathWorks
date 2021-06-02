(* ::Package:: *)

BeginPackage["gBRDF`"];


ClearAll[gPointLightFallOff];
gPointLightFallOff::usage="function{gPointLightFallOff}";
ClearAll[gPhongNDF];
gPhongNDF::usage="function[gPhongNDF]";
ClearAll[gDGGX];
gDGGX::usage="function[gDGGX]";
ClearAll[gVisSmith];
gVisSmith::usage="function[gVisSmithG]";
ClearAll[gFresnelOrigin];
gFresnelOrigin::usage="function[gVisSmithG]";
ClearAll[gBrdfFunc];
gBrdfFunc::usage="function[gBrdfFunc]";


Begin["`Private`"];
On[Assert];


(*https://neil3d.github.io/assets/pdf/s2013_pbs_epic_notes_v2.pdf Page 12*)
gPointLightFallOff[radius_,dist_]:=(Clip[(1-(dist/radius)^4),{0,1}])^2/(dist^2+1);


(*Blinn Phong NDF*)
gPhongNDF[m_,noh_]:=Module[
	{a2,tmpN},
	a2=m*m;
	tmpN=2/a2-2;
	tmpN=Max[tmpN,0.001];
	(tmpN+2)/(2 \[Pi])*Max[(noh^tmpN),0]
];


gDGGX[m_,NoH_]=m^2/(\[Pi]*((NoH^2*(m^2-1))+1)^2);


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


ClearAll[gBrdfFunc];
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
	D*Vis*F
];


End[];


EndPackage[];
