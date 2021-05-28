(* ::Package:: *)

BeginPackage["gUtils`"]
Begin["`Private`"]

ClearAll[gPrint2];
gPrint2[msg_,f_,p1_]=f[p1];

ClearAll[testF123];
testF123[x_]=x*2;

End[ ]
EndPackage[ ]
