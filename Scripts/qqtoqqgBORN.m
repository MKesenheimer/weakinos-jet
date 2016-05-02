(* ::Package:: *)

(*  	qqbneuIchaJ.m
		generates the Fortran code for
		q q-bar -> neu cha in the MSSM
		last modified Oct 2014

Note: the QED contributions are not taken into account. To plug
the QED part back in, comment out the parts in DiagramSelect that
eliminate a V[1|3]...
*)


Clear["Global`*"]
SetDirectory[NotebookDirectory[]];
<< FeynArts`
<< FormCalc`
ClearProcess[]

time1 = SessionTime[]


	process = {F[3, {1}], -F[3, {1}]} -> {F[3,{1}], -F[3,{1}], V[5]};
	name = uubar;
	


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


(*Options*)
SetOptions[InsertFields, Model -> "SMQCD", Restrictions -> {NoLightFHCoupling}, ExcludeParticles ->{}]

SetOptions[Paint, PaintLevel -> {Classes}, ColumnsXRows -> {4, 5}, AutoEdit -> False];

(*Reduce tensor to scalar integrals and choose regularization scheme*)
(*D = dimensional regularization (default),*)
(*4 = constrained differential renormalization,*)
(*0 = keeps the whole amplitude D-dimensional*)
SetOptions[CalcFeynAmp,Dimension->D];
(*Note: There is currently a bug in FormCalc which does not allow to compile*)
(*the generated code with PaVeReduce\[Rule]True set.*)
(*One has to replace "Derivative(1)(IGram)(MS2)" with "(-1/(MS2**2))"*)

(*Save the Diagrams*)
$PaintSE = MkDir["Diagrams"];
DoPaint[diags_, type_, opt___] := Paint[diags, opt,
  DisplayFunction -> (Export[ToFileName[$PaintSE, name <> "." <> type <> ".pdf"], #]&)]

(*SUSY Counterterm*)
GSY = GS;
ELY = EL(*+dZe1y*);


Print["Born"]

tops = CreateTopologies[0, 2 -> 3];
ins = InsertFields[tops, process];
DoPaint[ins, "born"];
born = CalcFeynAmp[CreateFeynAmp[ins]];
born = born//.{Alfa2->0}


amps = {born(* BORN ONLY, self, vert, box*)};
{born(* BORN ONLY, self, vert, box*)} = Abbreviate[amps, 6, Preprocess -> OnSize[100, Simplify, 500, DenCollect]];

col = ColourME[All, born];

abbr = OptimizeAbbr[Abbr[]]
subexpr = OptimizeAbbr[Subexpr[]]//.{Alfa2->0}


dir = SetupCodeDir[name <> ".fortran", Drivers -> name <> ".drivers"];
WriteSquaredME[born,{}, col, abbr, subexpr, dir];
(*neue Syntax durch ge\[ADoubleDot]nderte Datei "FormCalc84.m" und "FormCalc.m" im FormCalc Installationsordner.*)
(*Nun ist es m\[ODoubleDot]glich die RenConst. zu berechnen ohne, dass sofort der Fortran Code erzeugt wird.*)
(*Auf die berechneten RenConst. k\[ODoubleDot]nnen weitere Operationen angewendet werden. Danach werden*)
(*die RenConst. mit der Funktion "WriteRenConst[]" in Fortran Code umgewandelt.*)
renConst = CalcRenConst[amps];(*ge\[ADoubleDot]nderte Funktion, berechnet die RenConst, wandelt sie aber nicht sofort in Fortran Code um.*)
renConst = renConst//.{Alfa -> 0}//FullSimplify (*consider just strong corrections*)
Export["renConst.wdx",renConst,"WDX"];
WriteRenConst[renConst,dir];(*Neue Funktion! Hinzugef\[UDoubleDot]gt in "FormCalc84.m" und "FormCalc.m"*)


Print["time used: ", SessionTime[] - time1]
Exit[];
