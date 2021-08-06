(* ::Package:: *)

BeginPackage["gTexStyles`"];
SetDirectory[FileNameJoin@{ParentDirectory[NotebookDirectory[]],"Shared"}];
Needs["gUtils`"];
ResetDirectory[];


ClearAll[texArrowHead];
texArrowHead::usage="texArrowHead";


Begin["`Private`"];


(*@see gTexStyleExamples.nb*)
texArrowHead=Flatten[Cases[Show[Graph[{1\[DirectedEdge]2},
	EdgeShapeFunction->"ShortFilledArrow"]],
	Arrowheads[a_]:>Cases[a,_Graphics,Infinity,1],Infinity,1]][[1]];


End[];


EndPackage[];
