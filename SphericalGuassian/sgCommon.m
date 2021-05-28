(* ::Package:: *)

BeginPackage["sgCommon`"];
Begin["`Private`"];

ClearAll[sgFunc];
sgFunc[v_,{p_,\[Lambda]_,\[Mu]_}]:=\[Mu]*Exp[\[Lambda]*(Dot[v,p]-1)];

End[ ];
EndPackage[ ];
