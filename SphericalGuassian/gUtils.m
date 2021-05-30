(* ::Package:: *)

BeginPackage["gUtils`"];


gPrint::usage="Print a message";
gPrintFunc::usage="Print a function";
gEvalFunc::usage="Evaluate a funciton";


Begin["`Private`"];


ClearAll[gPrint];
gPrint[msg_]:=Module[
{},
Print[Style[msg,FontSize->18,Background->LightBlue]]
];


ClearAll[gPrintFunc];
gPrintFunc[name_,func_]:=Module[
{ret},
ret=Print[Style[
StringJoin[ToString[name],"=",ToString[TraditionalForm[func]]],
FontSize->18,Background->LightBlue]];
ret
];


ClearAll[gEvalFunc];
gEvalFunc[name_,func_]:=Print[Style[
StringJoin[ToString[name],"=",ToString[func]],
FontSize->18,Background->LightBlue]];


End[];


EndPackage[];
