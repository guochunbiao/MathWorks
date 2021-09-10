(* ::Package:: *)

BeginPackage["pbrtPath`"];
SetDirectory[FileNameJoin@{ParentDirectory[NotebookDirectory[]],"Shared"}];
Needs["gUtils`"];
Needs["pbrtPathLog`"];
Needs["gPlots3DEx`"];
ResetDirectory[];


ClearAll[pbrtRayMaxDist,pbrtLoadScene,pbrtRasterToCamera,pbrtGetCameraSample,
	pbrtRayDifferential,pbrtTriUVs,pbrtTriNormalRHS,pbrtTriIntersect,pbrtSceneIntersect,
	pbrtBsdf,pbrtIsBlack,pbrtGet2D,pbrtComputeScatteringFunctions,pbrtUniformSampleTri,
	pbrtTriArea,pbrtTriSample,pbrtShapeSample,pbrtLightL,pbrtLightSampleLi,pbrtLightPdfLi,
	pbrtInteractionSpawnRay,pbrtShapePdf,pbrtBsdfWorldToLocalLHS,pbrtBsdfLocalToWorldRHS,
	pbrtLambertianReflectionF,pbrtBsdfF,pbrtBxdfPdf,pbrtBsdfPdf,pbrtBxdfF,pbrtBxdfSampleF,
	pbrtCosineSampleHemisphereLHS,pbrtPowerHeuristic,pbrtBsdfSampleF,pbrtSurfaceInteractionLe,
	pbrtEstimateDirect,pbrtUniformSampleOneLight,pbrtPathIntegratorLi,pbrtSamplerIntegratorRender,
	pbrtRenderTile,pbrtPlotOriginScene,pbrtPlot3DOptions,pbrtValidateSinglePixel,
	pbrtValidateTilePixels,pbrtGetTestPixelColor,pbrtPathIntegratorLiBounce,pbrtPlotPath,
	pbrtConcentricSampleDisk,pbrtLightSampleLe,pbrtCosineHemispherePdf,pbrtCoordinateSystemRHS,
	pbrtSwithRHS];
pbrtRayMaxDist=10000;
pbrtLoadScene::usage="pbrtLoadScene";
pbrtRasterToCamera::usage="pbrtRasterToCamera";
pbrtGetCameraSample::usage="pbrtGetCameraSample";
pbrtRayDifferential::usage="pbrtRayDifferential";
pbrtTriUVs::usage="pbrtTriUVs";
pbrtTriNormalRHS::usage="pbrtTriNormalRHS";
pbrtTriIntersect::usage="pbrtTriIntersect";
pbrtSceneIntersect::usage="pbrtSceneIntersect";
pbrtBsdf::usage="pbrtBsdf";
pbrtIsBlack::usage="pbrtIsBlack";
pbrtGet2D::usage="pbrtGet2D";
pbrtComputeScatteringFunctions::usage="pbrtComputeScatteringFunctions";
pbrtUniformSampleTri::usage="pbrtUniformSampleTri";
pbrtTriArea::usage="pbrtTriArea";
pbrtTriSample::usage="pbrtTriSample";
pbrtShapeSample::usage="pbrtShapeSample";
pbrtLightL::usage="pbrtLightL";
pbrtLightSampleLi::usage="pbrtLightSampleLi";
pbrtInteractionSpawnRay::usage="pbrtInteractionSpawnRay";
pbrtLightPdfLi::usage="pbrtLightPdfLi";
pbrtShapePdf::usage="pbrtShapePdf";
pbrtBsdfWorldToLocalLHS::usage="pbrtBsdfWorldToLocalLHS";
pbrtBsdfLocalToWorldRHS::usage="pbrtBsdfLocalToWorldRHS";
pbrtLambertianReflectionF::usage="pbrtLambertianReflectionF";
pbrtBsdfF::usage="pbrtBsdfF";
pbrtBxdfPdf::usage="pbrtBxdfPdf";
pbrtBsdfPdf::usage="pbrtBsdfPdf";
pbrtBxdfF::usage="pbrtBxdfF";
pbrtBxdfSampleF::usage="pbrtBxdfSampleF";
pbrtCosineSampleHemisphereLHS::usage="pbrtCosineSampleHemisphereLHS";
pbrtPowerHeuristic::usage="pbrtPowerHeuristic";
pbrtBsdfSampleF::usage="pbrtBsdfSampleF";
pbrtSurfaceInteractionLe::usage="pbrtSurfaceInteractionLe";
pbrtEstimateDirect::usage="pbrtEstimateDirect";
pbrtUniformSampleOneLight::usage="pbrtUniformSampleOneLight";
pbrtPathIntegratorLi::usage="pbrtPathIntegratorLi";
pbrtSamplerIntegratorRender::usage="pbrtSamplerIntegratorRender";
pbrtPrintPixelLog::usage="pbrtPrintPixelLog";
pbrtPlot3DOptions::usage="pbrtPlot3DOptions";
pbrtPlotOriginScene::usage="pbrtPlotOriginScene";
pbrtRenderTile::usage="pbrtRenderTile";
pbrtValidateSinglePixel::usage="pbrtValidateSinglePixel";
pbrtValidateTilePixels::usage="pbrtValidateTilePixels";
pbrtPathIntegratorLiBounce::usage="pbrtPathIntegratorLiBounce";
pbrtPlotPath::usage="pbrtPlotPath";
pbrtConcentricSampleDisk::usage="pbrtConcentricSampleDisk";
pbrtLightSampleLe::usage="pbrtLightSampleLe";
pbrtCosineHemispherePdf::usage="pbrtCosineHemispherePdf";
pbrtCoordinateSystemRHS::usage="pbrtCoordinateSystemRHS";
pbrtSwithRHS::usage="pbrtSwithRHS";


Begin["`Private`"];


(*GlobalSampler::Get2D \[Rule] FixedSampler::SampleDimension*)
pbrtGet2D[]:={0.6,0.6};


(*SamplerIntegrator::Render*)
pbrtSamplerIntegratorRender[pixel_,scene_,maxDepth_:1]:=Module[
	{l,camRayo,camRayd,pixelLog},
	(*logging*)
	pixelLog=<|"pixel"->pixel|>;

	camRayo=gAssocData[scene,"eyePt"];
	camRayd=pbrtRayDifferential[pixel,scene];
	pbrtLogInitPixel[pixel,{camRayo,camRayd}];

	(*PathIntegrator::Li*)
	l=pbrtPathIntegratorLi[{camRayo,camRayd},scene,maxDepth];
	pbrtLogPixelLi[l];

	l
];


(*PathIntegrator::Li*)
pbrtPathIntegratorLi[{camRayo_,camRayd_},scene_,maxDepth_:1]:=Module[
	{bounces,l,nextBounce,rayo,rayd,beta,bounceL},
	
	l={0,0,0};
	rayo=camRayo;
	rayd=camRayd;
	beta={1,1,1};
	For[bounces=0,bounces<maxDepth,bounces++,
			bounceL=pbrtPathIntegratorLiBounce[{rayo,rayd,beta},scene,bounces];
			l+=bounceL;
		
			nextBounce=pbrtCurrBounce["nextBounce"];
			rayo=nextBounce["next_rayo"];
			rayd=nextBounce["next_rayd"];
			beta=nextBounce["next_beta"];
		];
	
	l
];


(*PathIntegrator::Li --> for (bounces = 0;; ++bounces)*)
pbrtPathIntegratorLiBounce[{rayo_,rayd_,beta_},scene_,bounceIndex_]:=Module[
	{l,le,ld,isect1,isectWithBsdf,nextwo,nextwi,nextf,nextpdf,
		nextSamples,nextRayo,nextRayd,nextBeta,endLabel},
	
	l={0,0,0};
	pbrtLogStartBounce[bounceIndex,rayo,rayd];

	isect1=pbrtSceneIntersect[{rayo,rayd},scene];
	le={0,0,0};
	If[ToString@isect1!="NaN",
		le=If[bounceIndex>0,{0,0,0},pbrtSurfaceInteractionLe[isect1,-rayd]]];
	pbrtLogBounceLe[le,beta];
	l+=beta*le;

	isectWithBsdf=If[ToString@isect1=="NaN","NaN",pbrtComputeScatteringFunctions[isect1]];
	ld=If[ToString@isectWithBsdf=="NaN",
			{0,0,0},
			pbrtUniformSampleOneLight[isectWithBsdf,scene]];
	pbrtLogBounceLd[ld,beta];	
	ld*=beta;
	l+=ld;
	
	If[ToString@isectWithBsdf=="NaN",Goto[endLabel]];
	(*next bounce info*)
	nextwo=-rayd;
	nextSamples=pbrtGet2D[];
	{nextf,nextwi,nextpdf}=pbrtBsdfSampleF[isectWithBsdf["bsdf"],nextwo,nextSamples];
	{nextRayo,nextRayd}=pbrtInteractionSpawnRay[isectWithBsdf,nextwi];
	nextBeta=beta*nextf*Abs@Dot[nextwi,isectWithBsdf["n"]]/nextpdf;
	pbrtLogNextBounce[nextRayo,nextRayd,nextBeta];
	
	pbrtLogEndBounce[l];
	
	Label[endLabel];
	l
];


(*UniformSampleOneLight*)
pbrtUniformSampleOneLight[isect_,scene_]:=Module[
	{lights,lightIndex,light,lightPdf,uLight,uScattering,li,bounceLog},
	lights=gAssocData[scene,"lights"];
	lightIndex=1;
	lightPdf=1;

	Assert[1<=lightIndex<=Length[lights]];
	light=lights[[lightIndex]];

	uLight=pbrtGet2D[];
	uScattering=pbrtGet2D[];

	li=pbrtEstimateDirect[isect,uScattering,light,uLight,False,False,scene];
	li
];


(*EstimateDirect*)
pbrtEstimateDirect[it_,uScattering_,light_,uLight_,
		handleMedia_,specular_,scene_]:=Module[
{li,wi,lightPdf,vis,lightLi,bsdf,f1,f2,scatteringPdf,weight,ld,ld2,
	lightIsect,ray,foundSurfaceInteraction,tr,endLabel,
	fLight,fBsdf},
	Assert[handleMedia==False];
	Assert[specular==False];
	(*Print[{it,uScattering,light,uLight,handleMedia,specular}];*)

	lightLi=pbrtLightSampleLi[light,it,uLight];
	li=lightLi["li"];
	wi=lightLi["wi"];
	lightPdf=lightLi["pdf"];

	bsdf=it["bsdf"];

	(* f1:sampling the light *)
	{fLight,scatteringPdf}=If[pbrtIsBlack[li]||lightPdf==0,
	{{0,0,0},0},
	{pbrtBsdfF[bsdf,it["wo"],wi],pbrtBsdfPdf[bsdf,it["wo"],wi]}];

	f1=fLight*Abs@Dot[wi,it["n"]];

	weight=pbrtPowerHeuristic[1,lightPdf,1,scatteringPdf];
	ld={0,0,0};
	If[!pbrtIsBlack[f1],ld+=f1*li*weight/lightPdf];
	pbrtLogMisLight[it["wo"],wi,lightPdf,scatteringPdf,li,fLight,ld];

	(* f2:sampling the bxdf *)
	{fBsdf,wi,scatteringPdf}=pbrtBsdfSampleF[bsdf,it["wo"],uScattering];
	f2=fBsdf*Abs@Dot[wi,it["n"]];

	Assert[!pbrtIsBlack[f2]&&scatteringPdf>0];
	weight=1;
 
    ld2={0,0,0};
	tr=1;
	lightPdf=pbrtLightPdfLi[light,it,wi];
	If[lightPdf==0,Goto[endLabel]];

	weight=pbrtPowerHeuristic[1,scatteringPdf,1,lightPdf];

	(*offset origin a little bit*)
	ray={it["p"]+wi*0.01,wi};
	lightIsect=pbrtSceneIntersect[ray,scene];
	foundSurfaceInteraction=If[ToString@lightIsect=="NaN",False,True];
	Assert[foundSurfaceInteraction];

	li=pbrtSurfaceInteractionLe[lightIsect,-wi];
	ld2=f2*li*tr*weight/scatteringPdf;

	Label[endLabel];
	pbrtLogMisBsdf[it["wo"],wi,lightPdf,scatteringPdf,li,fBsdf,tr,ld2];
	ld+ld2
];


(*PowerHeuristic*)
pbrtPowerHeuristic[nf_,fPdf_,ng_,gPdf_]:=Module[
	{f,g},

	f=nf*fPdf;
	g=ng*gPdf;

	If[f==0&&g==0,Return[0]];

	f*f/(f*f+g*g)
];


(*SurfaceInteraction::Le*)
pbrtSurfaceInteractionLe[isect_,rayd_]:=Module[
	{},

	If[!isect["isLight"],Return[{0,0,0}]];
	
	pbrtLightL[isect["n"],rayd,isect["material"]]
];


(*CosineSampleHemisphere*)
pbrtCosineSampleHemisphereLHS[u_]:=Module[
	{d,z},

	d=pbrtConcentricSampleDisk[u];
	z=Sqrt@Max[0,1-d[[1]]*d[[1]]-d[[2]]*d[[2]]];

	(*local space, dont need RHS*)
	{d[[1]],d[[2]],z}
];


(*LambertianReflection::f*)
pbrtBxdfF[bxdf_,wo_,wi_]:=Module[
	{},

	bxdf["R"]/\[Pi]
];


(*BxDF::Sample_f*)
pbrtBxdfSampleF[bxdf_,wo_,u_]:=Module[
	{wi,pdf,f},
	
	wi=pbrtCosineSampleHemisphereLHS[u];
	(*RHS*)
	If[wo[[2]]<0,wi[[2]]*=-1];
	pdf=pbrtBxdfPdf[bxdf,wo,wi];

	f=pbrtBxdfF[bxdf,wo,wi];
	{f,wi,pdf}
];


(*BSDF::Sample_f*)
pbrtBsdfSampleF[bsdf_,woWorld_,u_]:=Module[
	{uRemapped,wi,wo,bxdf,f,wiWorld,pdf,ng,reflect},
	uRemapped=u;
	wo=pbrtBsdfWorldToLocalLHS[bsdf,woWorld];

	bxdf=bsdf["bxdfs0"];
	{f,wi,pdf}=pbrtBxdfSampleF[bxdf,wo,uRemapped];
	wiWorld=pbrtBsdfLocalToWorldRHS[bsdf,wi];
	ng=bsdf["ng"];

	reflect=Dot[wiWorld,ng]*Dot[woWorld,ng]>0;
	f=0;
	f+=pbrtBxdfF[bxdf,wo,wi];

	{f,wiWorld,pdf}//N
];


(*BxDF::Pdf*)
pbrtBxdfPdf[bxdf_,wo_,wi_]:=Module[
	{},

	(*local space,dont need RHS*)
	If[wo[[3]]*wi[[3]]>0,Abs[wi[[3]]]/\[Pi],0]
];


(*BSDF::Pdf*)
pbrtBsdfPdf[bsdf_,woWorld_,wiWorld_]:=Module[
	{wo,wi,pdf,bxdf,matchingComps},
	wi=pbrtBsdfWorldToLocalLHS[bsdf,wiWorld];
	wo=pbrtBsdfWorldToLocalLHS[bsdf,woWorld];
	If[wo[[3]]==0,Return[0]];

	pdf=0;
	bxdf=bsdf["bxdfs0"];
	pdf+=pbrtBxdfPdf[bxdf,wo,wi];

	matchingComps=1;
	pdf/matchingComps
];



(*LambertianReflection::f*)
pbrtLambertianReflectionF[bxdf_,wo_,wi_]:=Module[
	{r},
	
	r=bxdf["R"];
	r/\[Pi]
];


(*BSDF::f*)
pbrtBsdfF[bsdf_,woW_,wiW_]:=Module[
	{wi,wo,f,bxdf},
	wi=pbrtBsdfWorldToLocalLHS[bsdf,wiW];
	wo=pbrtBsdfWorldToLocalLHS[bsdf,woW];
	If[wo[[3]]==0,Return[0]];

	f={0,0,0};
	bxdf=bsdf["bxdfs0"];
	f+=pbrtLambertianReflectionF[bxdf,wo,wi];

	f
];


(*Interaction::SpawnRay*)
pbrtInteractionSpawnRay[ref_,d_]:=Module[
	{o},

	o=ref["p"];
	{o,d}
];


(*BSDF::WorldToLocal*)
pbrtBsdfWorldToLocalLHS[bsdf_,v_]:=Module[
	{ss,ts,ns},

	ss=bsdf["ss"];
	ts=bsdf["ts"];
	ns=bsdf["ns"];

	(*local space dont need RHS*)
	{Dot[v,ss],Dot[v,ts],Dot[v,ns]}
];


(*BSDF::LocalToWorld*)
pbrtBsdfLocalToWorldRHS[bsdf_,v_]:=Module[
	{ss,ts,ns,x,y,z},

	ss=bsdf["ss"];
	ts=bsdf["ts"];
	ns=bsdf["ns"];

	x=ss[[1]]*v[[1]]+ts[[1]]*v[[2]]+ns[[1]]*v[[3]];
	y=ss[[2]]*v[[1]]+ts[[2]]*v[[2]]+ns[[2]]*v[[3]];
	z=ss[[3]]*v[[1]]+ts[[3]]*v[[2]]+ns[[3]]*v[[3]];
	(*ss,ts,ns already in RHS*)
	{x,y,z}
];


(*Shape::Pdf*)
pbrtShapePdf[tri_,ref_,wi_]:=Module[
	{rayo,rayd,intsPt,intsNormal,denom,pdf},

	{rayo,rayd}=pbrtInteractionSpawnRay[ref,wi];
	{intsPt,intsNormal}=pbrtTriIntersect[{rayo,rayd},tri];
	If[ToString@intsPt=="NaN",Return[0]];

	denom=Abs@Dot[intsNormal,-wi]*pbrtTriArea[tri];
	If[denom==0,Return[0]];

	(*Convert light sample weight to solid angle measure*)
	pdf=Norm[ref["p"]-intsPt]^2/denom;

	pdf
];


(*DiffuseAreaLight::Pdf_Li*)
pbrtLightPdfLi[light_,ref_,wi_]:=Module[
	{tri},

	(*Print[{light,ref,wi}];*)
	tri=light["tri"];
	pbrtShapePdf[tri,ref,wi]
];


(*Triangle::Area*)
pbrtTriArea[tri_]:=Module[
	{p0,p1,p2,area},
	p0=tri[[1]];
	p1=tri[[2]];
	p2=tri[[3]];
	area=0.5*Norm@Cross[p2-p0,p1-p0];
	area
];


(*DiffuseAreaLight::L*)
pbrtLightL[surfNormal_,w_,lightMat_]:=Module[
	{kd},

	kd=lightMat["Kd"];
	If[Dot[surfNormal,w]>0,kd,{0,0,0}]
];


(*DiffuseAreaLight::Sample_Li*)
pbrtLightSampleLi[light_,ref_,u_]:=Module[
	{pShape,wi,vis,l,endLabel},

	l={0,0,0};
	wi={0,0,0};
	pShape=pbrtShapeSample[light,ref,u];
	If[pShape["pdf"]==0||Norm[pShape["p"]-ref["p"]]<0.0000001,Goto[endLabel]];

	wi=Normalize[pShape["p"]-ref["p"]];
	vis={};
	l=pbrtLightL[pShape["n"],-wi,light["material"]];

	Label[endLabel];
	<|"li"->l,"wi"->wi,"pdf"->pShape["pdf"]|>
];


(*DiffuseAreaLight::Sample_Le*)
pbrtLightSampleLe[light_,u1_,u2_]:=Module[
	{tri,pShape,lightNormal,pdfPos,wLhs,wRhs,pdfDir,v1,v2,ray,l},
	
	tri=gAssocData[light,"tri"];
	pShape=pbrtTriSample[tri,u1];
	pdfPos=pShape["pdf"];
	lightNormal=pShape["n"];
	
	wLhs=pbrtCosineSampleHemisphereLHS[u2];
	pdfDir=pbrtCosineHemispherePdf[wLhs[[3]]];
	
	{v1,v2}=pbrtCoordinateSystemRHS[lightNormal];
	wRhs=wLhs[[1]]*v1+wLhs[[2]]*v2+wLhs[[3]]*lightNormal;
	
	ray=pbrtInteractionSpawnRay[pShape,wRhs];
	l=pbrtLightL[pShape["n"],wRhs,light["material"]];
	
	<|"l"->l,"photonRay"->ray,"lightNormal"->lightNormal,
		"pdfPos"->pdfPos,"pdfDir"->pdfDir|>
];


(*CoordinateSystem*)
pbrtCoordinateSystemRHS[v1RHS_]:=Module[
	{v1LHS,v2LHS,v3LHS,v2RHS,v3RHS},
	v1LHS=pbrtSwithRHS[v1RHS];
	
	v2LHS=If[
		Abs@v1LHS[[1]]>Abs@v1LHS[[2]],
		{-v1LHS[[3]],0,v1LHS[[1]]}/Sqrt[v1LHS[[1]]*v1LHS[[1]]+v1LHS[[3]]*v1LHS[[3]]],
		{0,v1LHS[[3]],-v1LHS[[2]]}/Sqrt[v1LHS[[2]]*v1LHS[[2]]+v1LHS[[3]]*v1LHS[[3]]]];
	v3LHS=Cross[v1LHS,v2LHS];
	
	v2RHS=pbrtSwithRHS[v2LHS];
	v3RHS=pbrtSwithRHS[v3LHS];
	
	{v2RHS,v3RHS}
];


pbrtSwithRHS[lhsOrRhs_]:=Module[
	{rhsOrLhs},
	
	rhsOrLhs={lhsOrRhs[[1]],lhsOrRhs[[3]],lhsOrRhs[[2]]};
	rhsOrLhs
];


(*CosineHemispherePdf*)
pbrtCosineHemispherePdf[cosTheta_]:=Module[
	{},
	
	cosTheta * 1/\[Pi]
];


(*Triangle::Sample*)
pbrtTriSample[tri_,u_]:=Module[
	{b,p0,p1,p2,itP,itN,itError,pAbsSum,gamma6,pdf},

	b=pbrtUniformSampleTri[u];
	p0=tri[[1]];
	p1=tri[[2]];
	p2=tri[[3]];

	itP=b[[1]]*p0+b[[2]]*p1+(1-b[[1]]-b[[2]])*p2;
	itN=Normalize@Cross[p2-p0,p1-p0];

	pAbsSum=Abs[b[[1]]*p0]+Abs[b[[2]]*p1]+Abs[(1-b[[1]]-b[[2]])*p2];
	gamma6=3.57628011*10^-7;
	itError=gamma6*pAbsSum;

	pdf=1/pbrtTriArea[tri];

	<|"p"->itP,"n"->itN,"error"->itError,"pdf"->pdf|>
];


(*Shape::Sample*)
pbrtShapeSample[light_,si_,u_]:=Module[
	{tri, intr,wi,distSquared,pdf,denom},

	tri=gAssocData[light,"tri"];
	intr=pbrtTriSample[tri,u];
	wi=intr["p"]-si["p"];
	Assert[Norm[wi]>0];
	wi=Normalize@wi;

	(*P838 & P875*)
	(*Convert from area measure to solid angle measure.*)
	(*https://computergraphics.stackexchange.com/questions/8032/how-can-we-convert-a-probability-density-according-to-solid-angle-to-a-density-a*)
	distSquared=Norm[intr["p"]-si["p"]]^2;
	denom=Abs@Dot[intr["n"],-wi];
	pdf=If[denom<0.0000001,0,intr["pdf"]*distSquared/denom];
	intr["pdf"]=pdf;

	intr
];


(*UniformSampleTriangle*)
pbrtUniformSampleTri[u_]:=Module[
	{su0},

	su0=Sqrt[u[[1]]];

	{1-su0,u[[2]]*su0}
];


(*MatteMaterial::ComputeScatteringFunctions*)
pbrtComputeScatteringFunctions[si_]:=Module[
	{bsdf,material,r,sig,newsi},
	bsdf=pbrtBsdf[si,1];

	material=si["material"];
	r=material["Kd"];
	sig=material["sigma"];
	sig=Clip[sig,{0,90}];

	(*not black*)
	Assert[r[[1]]!=0||r[[2]]!=0||r[[3]]!=0];
	Assert[sig==0];

	bsdf["bxdfs0"]=<|"type"->"LambertianReflection","R"->r|>;

	newsi=si;
	newsi["bsdf"]=bsdf;

	newsi
];


(*ConcentricSampleDisk*)
pbrtConcentricSampleDisk[u_]:=Module[
	{uOffset,theta,r},
	
	uOffset=2*u-{1,1};
	If[uOffset=={0,0},Return[{0,0}]];
	
	{r,theta}=If[Abs[uOffset[[1]]]>Abs[uOffset[[2]]],
		{uOffset[[1]],\[Pi]/4*uOffset[[2]]/uOffset[[1]]},
		{uOffset[[2]],\[Pi]/2-\[Pi]/4*uOffset[[1]]/uOffset[[2]]}];
	
	r*{Cos[theta],Sin[theta]}
];


(*CoefficientSpectrum::IsBlack*)
pbrtIsBlack[f_]:=Module[
	{},

	If[f[[1]]==0&&f[[2]]==0&&f[[3]]==0,True,False]
];


(*BSDF::BSDF*)
pbrtBsdf[si_,eta_]:=Module[
	{ns,ss,ts},

	ns=si["n"];
	ss=Normalize@si["dpdu"];
	ts=Cross[ss,ns];

	<|"eta"->eta,"ns"->ns,"ng"->si["n"],"ss"->ss,"ts"->ts|>
];


(*Triangle::Intersect*)
pbrtTriIntersect[ray_,tri_]:=Module[
	{rayo,rayd,R,L,d,curve,t,sol,solT,pt,norm},
	rayo=ray[[1]];
	rayd=ray[[2]];

	R=Triangle[tri];
	L=Line[{rayo,rayo+rayd*pbrtRayMaxDist}];
	d=RegionDistance[R];
	curve=(1-t) L[[1,1]]+t L[[1,2]];
	
	(*https://mathematica.stackexchange.com/questions/187688/how-to-find-the-cells-of-a-region-that-intersect-a-line*)
	sol=Quiet@Check[FindRoot[d[curve]==0,{t,0,1},Method->"Secant"],
						"NaN",{FindRoot::lstol,FindRoot::jsing,FindRoot::cvmit}];

	If[ToString@sol!="NaN"&&0.00001<=sol[[1,2]]<=1,pt=curve/.sol,pt="NaN"];
	norm=pbrtTriNormalRHS[tri];

	{pt,norm[[1]]}
];


(*Scene::Intersect*)
pbrtSceneIntersect[ray_,scene_]:=Module[
	{prims,lights,
		rayo,rayd,prim,light,tri,dist,triInts,intsNormal,minDist,minIntsPt,
		minPrim,minTri,minNormal,minIsLight,dpdu,dpdv,material,i},
	rayo=ray[[1]];
	rayd=ray[[2]];
	prims=gAssocData[scene,"prims"];
	lights=gAssocData[scene,"lights"];

	minDist=pbrtRayMaxDist ;
	minPrim="NaN";
	minTri="NaN";
	minIntsPt="NaN";
	minIsLight=False;

	(*testing geometry primitives*)
	For[i=1,i<=Length[prims],i++,
		prim=prims[[i]];
		tri=gAssocData[prim,"tri"];
		{triInts,intsNormal}=pbrtTriIntersect[ray,tri];
		If[ToString@triInts=="NaN",Continue[]];

		Assert[Length[triInts]==3];
		dist=Norm[triInts-rayo];
		If[dist>=minDist,Continue[]];

		minPrim=prim;
		minDist=dist;
		minTri=tri;
		minIntsPt=triInts;
		minIsLight=False;
	];

	(*testing light primitives*)
	For[i=1,i<=Length[lights],i++,
		light=lights[[i]];
		tri=gAssocData[light,"tri"];
		{triInts,intsNormal}=pbrtTriIntersect[ray,tri];
		If[ToString@triInts=="NaN",Continue[]];

		Assert[Length[triInts]==3];
		dist=Norm[triInts-rayo];
		If[dist>=minDist,Continue[]];

		minPrim=light;
		minDist=dist;
		minTri=tri;
		minIntsPt=triInts;
		minIsLight=True;
	];

	If[ToString@minPrim=="NaN",Return["NaN"]];

	{minNormal,dpdu,dpdv}=pbrtTriNormalRHS[minTri];
	material=gAssocData[minPrim,"material"];

	(*result*)
	<|"p"->minIntsPt,"n"->minNormal,"wo"->-rayd,"isLight"->minIsLight,
		"dpdu"->dpdu,"dpdv"->dpdv,
		"tri"->minTri,"material"->material|>
];


(*Triangle::GetUVs*)
pbrtTriUVs[]:=Module[
	{},

	{{0,0},{1,0},{1,1}}
];


pbrtTriNormalRHS[tri_]:=Module[
{dp02,dp12,n,uv,duv02,duv12,determinant,degenerateUV,invdet,
	dpdu,dpdv},
	dp02=tri[[1]]-tri[[3]];
	dp12=tri[[2]]-tri[[3]];
	n=Normalize@Cross[dp12,dp02];

	(*calculate dpdu, dpdv*)
	uv=pbrtTriUVs[];
	duv02=uv[[1]]-uv[[3]];
	duv12=uv[[2]]-uv[[3]];
	determinant=duv02[[1]]*duv12[[2]]-duv02[[2]]*duv12[[1]];
	degenerateUV=determinant<0.000001;
	Assert(!degenerateUV);

	invdet=1/determinant;
	dpdu=(duv12[[2]]*dp02-duv02[[2]]*dp12)*invdet;
	dpdv=(-duv12[[1]]*dp02+duv02[[1]]*dp12)*invdet;

	{n,dpdu,dpdv}
];


(*ProjectiveCamera::RasterToCamera*)
pbrtRasterToCamera[rasterPt_,scene_]:=Module[
	{worldPt,m,x,y,z,xp,yp,zp,wp,dirRHS},
	m=gAssocData[scene,"rasterToCam"];

	x=rasterPt[[1]];
	y=rasterPt[[2]];
	z=0;

    xp=m[[1]][[1]]*x+m[[1]][[2]]*y+m[[1]][[3]]*z+m[[1]][[4]];
	yp=m[[2]][[1]]*x+m[[2]][[2]]*y+m[[2]][[3]]*z+m[[2]][[4]];
	zp=m[[3]][[1]]*x+m[[3]][[2]]*y+m[[3]][[3]]*z+m[[3]][[4]];
	wp=m[[4]][[1]]*x+m[[4]][[2]]*y+m[[4]][[3]]*z+m[[4]][[4]];

	dirRHS={xp,zp,yp}/wp;
	Normalize@dirRHS
];


(*Sampler::GetCameraSample*)
pbrtGetCameraSample[x_,y_]:=Module[
	{dim,startIdx,sampValue,data},
	dim=2;
	data={};
	sampValue=pbrtGet2D[];
	(*For[i=0,i<dim,i++,AppendTo[data,dataArray[[startIdx+i]]]];*)
	data={x,y}+sampValue;
	
	data
];


(*PerspectiveCamera::GenerateRayDifferential*)
pbrtRayDifferential[pixel_,scene_]:=Module[
	{px,py,sampRasterPt,sampDir},
	px=pixel[[1]];
	py=pixel[[2]];

	sampRasterPt=pbrtGetCameraSample[px,py];
	sampDir=pbrtRasterToCamera[sampRasterPt,scene];

	Normalize@sampDir
];


pbrtRenderTile[tileMin_,tileMax_,scene_,colorMul_:1]:=Module[
	{resx,resy,i,j,imgTable,l},
	resx=gAssocData[scene,"resx"];
	resy=gAssocData[scene,"resy"];
	
	imgTable=Table[1,resx,resy];

	For[i=1,i<=resx,i++,
		For[j=1,j<=resy,j++,
			imgTable[[i]][[j]]=RGBColor[0,0,0];
		]
	];

	For[i=tileMin[[1]]+1,i<=tileMax[[1]]+1,i++,
		For[j=tileMin[[2]]+1,j<=tileMax[[2]]+1,j++,
			l=pbrtSamplerIntegratorRender[{i,j},scene]*colorMul;
            imgTable[[j]][[i]]=RGBColor@l;
		];
	];
	
	imgTable
];


pbrtLoadScene[file_]:=Module[
	{
		(*scene*)
		scene,imgSettings,resx,resy,
		(*camera*)
		camera,eyePt,lookPt,upDir,fov,camBoundMin,camBoundMax,rasterToCam,
		(*lights*)
		lights,
		(*primitives*)
		prims
	},
	
	(*scene*)
	scene=ToExpression[Import[file]];
	imgSettings=gAssocData[scene,"image"][[1]];
	resx=gAssocData[imgSettings,"resx"];
	resy=gAssocData[imgSettings,"resy"];
	Assert[resx>0&&resy>0];
	
	(*camera*)
	camera=gAssocData[scene,"camera"][[1]];
	eyePt=gAssocData[camera,"eyePt"];
	lookPt=gAssocData[camera,"lookPt"];
	upDir=gAssocData[camera,"upDir"];
	fov=gAssocData[camera,"fov"];
	fov=fov/180 \[Pi];
	camBoundMin=gAssocData[camera,"boundMin"];
	camBoundMax=gAssocData[camera,"boundMax"];
	rasterToCam=gAssocData[camera,"rasterToCam"];
	
	(*lights*)
	lights=gAssocData[scene,"lights"];
	(*primitives*)
	prims=gAssocData[scene,"prims"];
	
	<|
		(*scene*)
		"resx"->resx,"resy"->resy,
		(*camera*)
		"eyePt"->eyePt,"lookPt"->lookPt,"upDir"->upDir,"fov"->fov,
		"camBoundMin"->camBoundMin,"camBoundMax"->camBoundMax,"rasterToCam"->rasterToCam,
		(*lights*)
		"lights"->lights,
		(*primitives*)
		"prims"->prims
	|>
];





pbrtPlot3DOptions[scene_]:=Module[
	{eyePt,lookPt,upDir,fov},
	eyePt=gAssocData[scene,"eyePt"];
	lookPt=gAssocData[scene,"lookPt"];
	upDir=gAssocData[scene,"upDir"];
	fov=gAssocData[scene,"fov"];
	
	{
		Lighting->{{"Ambient",White}},
		Boxed->False, Axes->False,AxesLabel->{"X","Y","Z"},
		ViewVector->{eyePt,lookPt},ViewVertical->upDir,ViewAngle->fov
	}
];


pbrtPlotOriginScene[scene_,opacity_:1]:=Module[
	{lights,prims,graphList,createPolyGraphs},
	lights=gAssocData[scene,"lights"];
	prims=gAssocData[scene,"prims"];
	graphList={};

	createPolyGraphs[elems_]:=Module[
		{i,elem,tri,mat,Kd,sigma},

		For[i=1,i<=Length[elems],i++,
			elem=elems[[i]];
			tri=gAssocData[elem,"tri"];
			mat=gAssocData[elem,"material"];
			Kd=gAssocData[mat,"Kd"];
			sigma=gAssocData[mat,"sigma"];
			AppendTo[graphList,RGBColor[Kd]];
			AppendTo[graphList,Opacity[opacity]];
			AppendTo[graphList,Polygon[tri]]
		];
	];
	
	createPolyGraphs[lights];
	createPolyGraphs[prims];
	
	Graphics3D[graphList]
];


pbrtPlotPath[scene_,pathLog_]:=Module[
	{pathArrows,pathArrowPlots,i},
	
	{pathArrows}=pbrtExtractBounceArrows[pathLog];
	pathArrowPlots={};
	For[i=1,i<=Length@pathArrows,i++,
		AppendTo[pathArrowPlots,pltArrow3D@pathArrows[[i]]]];
	
	Print[pathArrows];
	Show[{
			pbrtPlotOriginScene[scene,0.1],
			pathArrowPlots
		 },
		 pbrtPlot3DOptions[scene]]
];


pbrtGetTestPixelColor[rowIndex_,colIndex_,scene_,testData_]:=Module[
	{resx,resy,idx,x,y,z},
	resx=gAssocData[scene,"resx"];
	resy=gAssocData[scene,"resy"];
	
	idx=rowIndex*resy*3+colIndex*3;
	x=testData[[idx+1]];
	y=testData[[idx+2]];
	z=testData[[idx+3]];
	
	{x,y,z}
];


pbrtValidateSinglePixel[i_,j_,scene_,testData_]:=Module[
	{pixelA,pixelB,test},

	pixelA=pbrtSamplerIntegratorRender[{i,j},scene];
	pixelB=pbrtGetTestPixelColor[i,j,scene,testData];

	(*Print[pixelA];
	Print[pixelB];*)
	test=If[gColorEquals[pixelA,pixelB],True,False];

	Assert[test==True||test==False];
	{test,pixelA,pixelB}
];


pbrtValidateTilePixels[tileMin_,tileMax_,scene_,testData_]:=Module[
	{i,j,ret,testOK,renderedPixel,testPixel},

	(*debugData={};*)
	ret=<|"total"->0,"ok"->0,"fail"->0,"black"->0|>;
	For[i=tileMin[[1]],i<tileMax[[1]],i++,
		For[j=tileMin[[2]],j<tileMax[[2]],j++,
			ret["total"]+=1;
			{testOK,renderedPixel,testPixel}=pbrtValidateSinglePixel[i,j,scene,testData];
			If[testOK,ret["ok"]+=1,ret["fail"]+=1];
			If[!testOK,ret["failedPixel"]={i,j}];
			If[pbrtIsBlack[renderedPixel],ret["black"]+=1];
			(*If[!pbrtIsBlack[renderedColor],Print[{i,j,}]]*)
			(*AppendTo[debugData,testOK];*)
		];
	];

	ret
	(*debugData*)
];


End[];


EndPackage[];
