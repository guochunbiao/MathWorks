(* ::Package:: *)

BeginPackage["gBRDF`"];


ClearAll[gPointLightFallOff];
gPointLightFallOff::usage="function{gPointLightFallOff}";


Begin["`Private`"];
On[Assert];


(*https://neil3d.github.io/assets/pdf/s2013_pbs_epic_notes_v2.pdf Page 12*)
gPointLightFallOff[radius_,dist_]:=(Clip[(1-(dist/radius)^4),{0,1}])^2/(dist^2+1);


End[];


EndPackage[];
