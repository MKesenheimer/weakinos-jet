(* ::Package:: *)

(* uutoneuneug.m
this is supposed to be a mathematica/formcalc script used to generate born-level amplitudes for
the up + upbar --> neutralino + neutralino + gluon process   *)


Clear["Global`*"]
SetDirectory[NotebookDirectory[]];
<< FeynArts`
<< FormCalc`
ClearProcess[]

time1 = SessionsTime[]


process = {F[3, {1}], -F[3, {1}]} -> {F[11, {1}], F[11, {1}]};
name =  uubar;


(*Neglect Masses (URL)*)
Neglect[ME] = Neglect[ME2] = 0;
(*Neglect[MQU] = Neglect[MQD] = 0;*)
Neglect[MU] = Neglect[MU2] = 0;
Neglect[MC] = Neglect[MC2] = 0;
(*Neglect[MT] = Neglect[MT2] = 0;*)
Neglect[MD] = Neglect[MD2] = 0;
Neglect[MS] = Neglect[MS2] = 0;
(*Neglect[MB] = Neglect[MB2] = 0;*)

(*Diagonale CKM Matrix*)
CKM = IndexDelta;
CKMC = IndexDelta;


(* some options *)
SetOptions[InsertFields, Model -> "MSSMCT",
           Restrictions -> {NoLightFHCoupling}(*No Fermion-Higgs coupling*),
		   ExcludeParticles -> {}];

SetOptions[Paint, PaintLevel -> {Classes}, ColumnsXRows -> {4, 5}, AutoEdit -> False];
SetOptions[CalcFeynAmp, Dimension -> D];
$PaintSE = MkDir["Diagrams"];
DoPaint[diags_, type_, opt___] := Paint[diags, opt,
  DisplayFunction -> (Export[ToFileName[$PaintSE, name <> "." <> type <> ".pdf"], #]&)]


Print["Born"]

tops = CreateTopologies[0, 2 -> 2];
ins = InsertFields[tops, process];
DoPaint[ins, "born"];
born = CalcFeynAmp[CreateFeynAmp[ins]];
born = born//.{Alfa2->0}


amps = {born};
{born} = Abbreviate[amps, 6, Preprocess -> OnSize[100, Simplify, 500, DenCollect]];

col = ColourME[All, born];

abbr = OptimizeAbbr[Abbr[]]
subexpr = OptimizeAbbr[Subexpr[]]//.{Alfa2->0}


dir = SetupCodeDir[name <> ".fortran", Drivers -> name <> ".drivers"];
WriteSquaredME[born,{}, col, abbr, subexpr, dir];

Print["time used: ", SessionTime[] - time1]
Exit[];












