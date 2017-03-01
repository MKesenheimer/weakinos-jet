(* ::Package:: *)

(*
generates the Fortran code for
p p -> weakino weakino jet in the MSSM
last modified January 2016
*)


Clear["Global`*"]
SetDirectory[NotebookDirectory[]];
<< FeynArts`
<< FeynArtsAdd`
<< FormCalc`
<< FormCalcAdd`
ClearProcess[]
<<"!rm *.frm"

time1 = SessionTime[]


name = "gg_g";
process = {V[5], V[5]} -> {V[5]};
Print[process]


(*Options*)
SetOptions[InsertFields, Model -> "SMQCD",
		   LastSelections -> {}];

SetOptions[Paint, PaintLevel -> {Classes}, ColumnsXRows -> {4, 5}, AutoEdit -> False];

(*Reduce tensor to scalar integrals and choose regularisazation scheme*)
(*D = dimensional regularization (default),*)
(*4 = constrained differential renormalization,*)
(*0 = keeps the whole amplitude D-dimensional*)
SetOptions[CalcFeynAmp, Dimension->D];
(*Note: There is currently a bug in FormCalc which does not allow to compile*)
(*the generated code with PaVeReduce\[Rule]True set.*)
(*One has to replace "Derivative(1)(IGram)(MS2)" with "(-1/(MS2**2))"*)

(*Save the Diagrams*)
$PaintSE = MkDir["Diagrams"];
DoPaint[diags_, type_, opt___] := Paint[diags, opt,
  DisplayFunction -> (Export[ToFileName[$PaintSE, name <> "_" <> type <> ".pdf"], #]&)]


Print["Born"]

tops = CreateTopologies[0, 2 -> 1];
ins = InsertFields[tops, process];

DoPaint[ins, "born"];
born = CalcFeynAmp[CreateFeynAmp[ins]];

Print["born = "];
Print[born];


Print["Vertices"]

top = CreateTopologies[1, 2 -> 1, TrianglesOnly];
ins = InsertFields[top, process];

DoPaint[ins, "vert"];

vert = CalcFeynAmp[CreateFeynAmp[ins], FermionChains->Chiral];
Print["vert = "];
Print[vert];

vertUV = UVDivergentPart[vert];

Print["vertUV = "];
Print[vertUV];


(*Calculate dZgg3*)
abbr = Abbr[];
subexpr = Subexpr[];
rules = Join[subexpr, abbr];

gggb = Plus@@born//.rules//FullSimplify
gggv = Plus@@vertUV//.rules//FullSimplify
dZgg3 = -gggv/gggb//FullSimplify

Export["dZgg3_SM.wdx",dZgg3,"WDX"];


Print["time used: ", SessionTime[] - time1]
Exit[];
