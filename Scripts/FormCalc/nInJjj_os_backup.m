(* ::Package:: *)

(*  	neu1neu2+jet.m
		generates the Fortran code for
		p p -> chi chi jet in the MSSM
		last modified July 2016

This version introduces particle widths on the Amp level, which is much simpler than
introducing them on the FeynAmp level.
*)


Clear["Global`*"]
SetDirectory[NotebookDirectory[]];
<< FeynArts`
<< FormCalc`
ClearProcess[]
<<"!rm *.frm"

time1 = SessionTime[]


(*rewrite the FieldMemberQ function to allow checks with leg numbers*)
FieldMatchExtQ[{_. (fi:P$Generic)[___],exti1_,exti2_},{_. fi_,extj1_,extj2_} ] := And[MatchQ[exti1,extj1],MatchQ[exti2,extj2]]
FieldMatchExtQ[{_. (fi:P$Generic)[i_, ___],exti1_,exti2_},{_. fi_[j_],extj1_,extj2_} ] := And[MatchQ[i, j],MatchQ[exti1,extj1],MatchQ[exti2,extj2]]
FieldMatchExtQ[{_. (fi:P$Generic)[i__],exti1_,exti2_},{_. fi_[j_, {r__}],extj1_,extj2_} ] :=
  And[MatchQ[{i}, {j, {r, ___}}],MatchQ[exti1,extj1],MatchQ[exti2,extj2]]
FieldMatchExtQ[{_. fi:P$Generic,exti1_,exti2_},{_. fi_,extj1_,extj2_}] := And[MatchQ[exti1,extj1],MatchQ[exti2,extj2]]
FieldMatchExtQ[{Rev[fi__],exti1_,exti2_},{Mix[fi__],extj1_,extj2_}] := And[MatchQ[exti1,extj1],MatchQ[exti2,extj2]]
FieldMatchExtQ[{Mix[fi__],exti1_,exti2_},{Rev[fi__],extj1_,extj2_}] := And[MatchQ[exti1,extj1],MatchQ[exti2,extj2]]
FieldMatchExtQ[{fi1_,exti1_,exti2_},{fi2_,extj1_,extj2_}] := And[MatchQ[fi1, fi2],MatchQ[exti1,extj1],MatchQ[exti2,extj2]]

FieldMemberExtQ[ li_, fi_ ] := !VectorQ[List@@ li, !FieldMatchExtQ[#, fi]&]


(*Functions to check for a given particle in s- or t-channel*)
(*now with additional functions which can be used to check for an explicit coupling to external fields.*)
Fext[ _[Incoming][f_, t_, ___],{n_}] := {f, t, {{Fx[n]}}}
Fext[ _[Outgoing][f_, t_, ___],{n_}] := {f, t, {{-Fx[n]}}}
Fext[ _[f_, t_, ___], {n_} ] := {f, t, {{Fi[n]}}}
Fsel[ftop_][v_] := Level[Select[ftop, MemberQ[#, v]&], {4}, Fs]
Ftel[ftop_][v_] := Level[Select[ftop, MemberQ[#, v]&], {4}, Ft]
Attributes[Fs] = Attributes[Ft] = {Orderless};
Fs[Fx[i_Integer],Fx[j_Integer], Fi[n_]] := (Fv[i,j,n]=1;Fi[n] = 1; Seq[])
Fs[-Fx[i_Integer],-Fx[j_Integer], Fi[n_]] := (Fv[i,j,n]=-1;Fi[n] = -1; Seq[])
Fs[1,1, Fi[n_]] := (Fv[i,j,n]=1;Fi[n] = 1; Seq[])
Fs[-1,-1, Fi[n_]] := (Fv[i,j,n]=-1;Fi[n] = -1; Seq[])
Ft[Fx[i_Integer],-Fx[j_Integer],Fi[n_]] := (Fv[i,j,n]=0;Fi[n] = 0; Seq[])
Ft[1,-1,Fi[n_]] := (Fv[i,j,n]=0;Fi[n] = 0; Seq[])

STChannelFields[ top:P$Topology ] := STChannelFields[top] =
Block[ {Fi, Fx,Fv, ttop = ToTree[top]},
  FixedPoint[Evaluate, Ft@@@
     FixedPoint[Evaluate,
       Fsel[MapIndexed[Fext, ttop]]/@ Vertices[ttop]]];
  Flatten/@ Transpose[Cases[ DownValues[Fi], _[_[_[n_]], i_] :>
    ReplacePart[{{}, {}}, Field[n], 2 - Abs[i]] ]]]

STChannelFieldsExt[ top:P$Topology ] := STChannelFieldsExt[top] =
Block[ {Fi,Fx,Fv, ttop = ToTree[top]},
  FixedPoint[Evaluate, Ft@@@
     FixedPoint[Evaluate,
       Fsel[MapIndexed[Fext, ttop]]/@ Vertices[ttop]]];
  ReplaceAll[Transpose[Cases[DownValues[Fv], _[_[_[l_,m_,n_]], i_] :>
    ReplacePart[{{}, {}}, {Field[n],l,m}, 2 - Abs[i]]]],{}->Sequence[]]]

SChannelQ[ fi_ ][ gr_, top_, ___ ] :=
  FieldMemberQ[STChannelFields[top][[1]] /. List@@ gr, fi]
TChannelQ[ fi_ ][ gr_, top_, ___ ] :=
  FieldMemberQ[STChannelFields[top][[2]] /. List@@ gr, fi]

(*All diagrams without the ones with the fields in s- or t-channel*)
NotSChannelQ[ fi_ ][ gr_, top_, ___ ] :=
  Not[FieldMemberQ[STChannelFields[top][[1]] /. List@@ gr, fi]]
NotTChannelQ[ fi_ ][ gr_, top_, ___ ] :=
  Not[FieldMemberQ[STChannelFields[top][[2]] /. List@@ gr, fi]]

(*The same as SChannelQ, but with an additional rule that checks the external fields which couple to the s- or t-channel fields.*)
Attributes[SChannelExtQ] = Attributes[TChannelExtQ] = {Orderless};
SChannelExtQ[ fi_,ext1_,ext2_][ gr_, top_, ___ ] :=
 FieldMemberExtQ[STChannelFieldsExt[top][[1]] /. List@@ gr, {fi,ext1,ext2}]
TChannelExtQ[ fi_,ext1_,ext2_ ][ gr_, top_, ___ ] :=
  FieldMemberExtQ[STChannelFieldsExt[top][[2]] /. List@@ gr, {fi,ext1,ext2}]

(*Same as above.*)
Attributes[NotSChannelExtQ] = Attributes[NotTChannelExtQ] = {Orderless};
NotSChannelExtQ[ fi_,ext1_,ext2_][ gr_, top_, ___ ] :=
 Not[FieldMemberExtQ[STChannelFieldsExt[top][[1]] /. List@@ gr, {fi,ext1,ext2}]]
NotTChannelExtQ[ fi_,ext1_,ext2_ ][ gr_, top_, ___ ] :=
  Not[FieldMemberExtQ[STChannelFieldsExt[top][[2]] /. List@@ gr, {fi,ext1,ext2}]]


(*You can now load the script with the command $ MathKernel -script nInJjj.m "d" "dbar" "n1" "n2" "d" "dbar"*)
Print[$CommandLine]
If[$CommandLine[[2]] === "-script",
	(p[1] = ToString[$CommandLine[[4]]];
	 p[2] = ToString[$CommandLine[[5]]];
	 p[3] = ToString[$CommandLine[[6]]];
	 p[4] = ToString[$CommandLine[[7]]];
	 p[5] = ToString[$CommandLine[[8]]];
	 p[6] = ToString[$CommandLine[[9]]];),
	(*Else*)
	(p[1] = "qubar";
	 p[2] = "qd";
	 p[3] = "nI";
	 p[4] = "nJ";
	 p[5] = "qd";
	 p[6] = "qubar";)
]

CalcProcess = p[1]<>p[2]<>"_"<>p[3]<>p[4]<>p[5]<>p[6];
name = CalcProcess;
Print[CalcProcess]

For[i=1, i<7, i++,
If[p[i] === "qu", P[i] = F[3],
If[p[i] === "qubar", P[i] = -F[3],
If[p[i] === "qd", P[i] = F[4],
If[p[i] === "qdbar", P[i] = -F[4],
If[p[i] === "nI", P[i] = F[11],
If[p[i] === "nJ", P[i] = F[11],
If[p[i] === "xI-", P[i] = F[12],
If[p[i] === "xI+", P[i] = -F[12],
If[p[i] === "xJ-", P[i] = F[12],
If[p[i] === "xJ+", P[i] = -F[12],
If[p[i] === "g", P[i] = V[5],

If[p[i] === "u", P[i] = F[3,{1}],
If[p[i] === "ubar", P[i] = -F[3,{1}],
If[p[i] === "c", P[i] = F[3,{2}],
If[p[i] === "cbar", P[i] = -F[3,{2}],
If[p[i] === "t", P[i] = F[3,{3}],
If[p[i] === "tbar", P[i] = -F[3,{3}],

If[p[i] === "d", P[i] = F[4,{1}],
If[p[i] === "dbar", P[i] = -F[4,{1}],
If[p[i] === "s", P[i] = F[4,{2}],
If[p[i] === "sbar", P[i] = -F[4,{2}],
If[p[i] === "b", P[i] = F[4,{3}],
If[p[i] === "bbar", P[i] = -F[4,{3}],

If[p[i] === "n1", P[i] = F[11,{1}],
If[p[i] === "n2", P[i] = F[11,{2}],
If[p[i] === "n3", P[i] = F[11,{3}],
If[p[i] === "n4", P[i] = F[11,{4}],

If[p[i] === "x1-", P[i] = F[12,{1}],
If[p[i] === "x1+", P[i] = -F[12,{1}],
If[p[i] === "x2-", P[i] = F[12,{2}],
If[p[i] === "x2+", P[i] = -F[12,{2}]
]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]
]

process = {P[1], P[2]} -> {P[3], P[4], P[5], P[6]};
Print[process]


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
SetOptions[InsertFields, Model -> "MSSMCT",
           Restrictions -> {NoLightFHCoupling}(*No Fermion-Higgs coupling*),
           (*Exclude Top, Higgs, Neutrinos, massive Leptons, Sneutrinos, Sleptons*)
		   ExcludeParticles -> {S[1|2|3|4|5|6|11|12], F[1|2](*, V[1|3]*)},
		   (*No internal Weakinos*)
		   LastSelections->{!F[11],!F[12]}];

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
  DisplayFunction -> (Export[ToFileName[$PaintSE, name <> "_" <> type <> ".pdf"], #]&)]

(*Coupling order of amplitude*)
(*Note: all diagrams are calculated, but after the feynman amplitude is generated*)
(*only the amplitudes with specified order of the coupling constants survive.*)
(*The diagrams that are generated are therefore not reliable any more.*)
(*Check results with coupl_order.nb*)
OrderEL = 2;
OrderGS = 2;
PowerOf[GS][x_Integer] := 0/;x>OrderGS
PowerOf[GS][OrderGS] := 1
PowerOf[EL][x_Integer] := 0/;x>OrderEL
PowerOf[EL][OrderEL] := 1

(*helpful function to extract parts of a Feynman amplitude*)
Component[Amp_,n_]:=Replace[Amp,Amp[_][x__]:>{x}][[n]]


Print["On-Shell Diagrams"]

tops = CreateTopologies[0, 2 -> 4];
(*DoPaint[tops, "tops"];*)
(*ins = InsertFields[tops, process, InsertionLevel -> {Particles}];*)
ins = InsertFields[tops, process];

(*Extract the on-shell diagrams*)
(*Select Diagrams with Squarks*)
ins = DiagramSelect[ins,(FieldPointMemberQ[FieldPoints[##],FieldPoint[_][S[_,{_,_,_}],_,_]]||
							FieldPointMemberQ[FieldPoints[##],FieldPoint[_][_,_,S[_,{_,_,_}]]]||
							FieldPointMemberQ[FieldPoints[##],FieldPoint[_][_,S[_,{_,_,_}],_]])&];
DoPaint[ins, "realOS"];

(*Select Diagrams with possible double on-shell resonances*)
insL35L46 = DiagramSelect[ins,(SChannelExtQ[S[_,{1,_,_}],3,5][##] && SChannelExtQ[S[_,{1,_,_}],4,6][##])&];
(*DoPaint[insL35L46, "realOS_L35L46", PaintLevel -> {Particles}];*)
DoPaint[insL35L46, "realOS_L35L46"];

(*Select Diagrams with possible double on-shell resonances*)
insL35R46 = DiagramSelect[ins,(SChannelExtQ[S[_,{1,_,_}],3,5][##] && SChannelExtQ[S[_,{2,_,_}],4,6][##])&];
DoPaint[insL35R46, "realOS_L35R46", PaintLevel -> {Particles}];

(*Select Diagrams with possible double on-shell resonances*)
insR35L46 = DiagramSelect[ins,(SChannelExtQ[S[_,{2,_,_}],3,5][##] && SChannelExtQ[S[_,{1,_,_}],4,6][##])&];
DoPaint[insR35L46, "realOS_R35L46", PaintLevel -> {Particles}];

(*Select Diagrams with possible double on-shell resonances*)
insR35R46 = DiagramSelect[ins,(SChannelExtQ[S[_,{2,_,_}],3,5][##] && SChannelExtQ[S[_,{2,_,_}],4,6][##])&];
DoPaint[insR35R46, "realOS_R35R46", PaintLevel -> {Particles}];

(*Select Diagrams with possible double on-shell resonances*)
insL36L45 = DiagramSelect[ins,(SChannelExtQ[S[_,{1,_,_}],3,6][##] && SChannelExtQ[S[_,{1,_,_}],4,5][##])&];
DoPaint[insL36L45, "realOS_L36L45", PaintLevel -> {Particles}];

(*Select Diagrams with possible double on-shell resonances*)
insL36R45 = DiagramSelect[ins,(SChannelExtQ[S[_,{1,_,_}],3,6][##] && SChannelExtQ[S[_,{2,_,_}],4,5][##])&];
DoPaint[insL36R45, "realOS_L36R45", PaintLevel -> {Particles}];

(*Select Diagrams with possible double on-shell resonances*)
insR36L45 = DiagramSelect[ins,(SChannelExtQ[S[_,{2,_,_}],3,6][##] && SChannelExtQ[S[_,{1,_,_}],4,5][##])&];
DoPaint[insR36L45, "realOS_R36L45", PaintLevel -> {Particles}];

(*Select Diagrams with possible double on-shell resonances*)
insR36R45 = DiagramSelect[ins,(SChannelExtQ[S[_,{2,_,_}],3,6][##] && SChannelExtQ[S[_,{2,_,_}],4,5][##])&];
DoPaint[insR36R45, "realOS_R36R45", PaintLevel -> {Particles}];

(*Check the abrreviations and subexpressions, no WREG should occur here up to now*)
Abbr[];
Subexpr[];


(*now, generate the amplitudes and insert the particle widths*)
widths={MZ2->MZ2-I WZ MZ, MW2->MW2-I WW MW, MSf2[sfe_,n1_,n2_]:>MSf2[sfe,n1,n2]-I (WSf[sfe,n1,n2]+WREG) MSf[sfe,n1,n2]};

real = CalcFeynAmp[CreateFeynAmp[insL35L46]/.{EL->EL PowerOf[EL], GS->GS PowerOf[GS]}];
real = real//.{PowerOf[a_]^x_:>PowerOf[a][x]};
real = real//.{PowerOf[a_]:>PowerOf[a][1]};
realL35L46 = real/.{Den[x_,y_]:>Den[x/.widths,y/.widths]}

real = CalcFeynAmp[CreateFeynAmp[insL35R46]/.{EL->EL PowerOf[EL], GS->GS PowerOf[GS]}];
real = real//.{PowerOf[a_]^x_:>PowerOf[a][x]};
real = real//.{PowerOf[a_]:>PowerOf[a][1]};
realL35R46 = real/.{Den[x_,y_]:>Den[x/.widths,y/.widths]}

real = CalcFeynAmp[CreateFeynAmp[insR35L46]/.{EL->EL PowerOf[EL], GS->GS PowerOf[GS]}];
real = real//.{PowerOf[a_]^x_:>PowerOf[a][x]};
real = real//.{PowerOf[a_]:>PowerOf[a][1]};
realR35L46 = real/.{Den[x_,y_]:>Den[x/.widths,y/.widths]}

real = CalcFeynAmp[CreateFeynAmp[insR35R46]/.{EL->EL PowerOf[EL], GS->GS PowerOf[GS]}];
real = real//.{PowerOf[a_]^x_:>PowerOf[a][x]};
real = real//.{PowerOf[a_]:>PowerOf[a][1]};
realR35R46 = real/.{Den[x_,y_]:>Den[x/.widths,y/.widths]}

real = CalcFeynAmp[CreateFeynAmp[insL36L45]/.{EL->EL PowerOf[EL], GS->GS PowerOf[GS]}];
real = real//.{PowerOf[a_]^x_:>PowerOf[a][x]};
real = real//.{PowerOf[a_]:>PowerOf[a][1]};
realL36L45 = real/.{Den[x_,y_]:>Den[x/.widths,y/.widths]}

real = CalcFeynAmp[CreateFeynAmp[insL36R45]/.{EL->EL PowerOf[EL], GS->GS PowerOf[GS]}];
real = real//.{PowerOf[a_]^x_:>PowerOf[a][x]};
real = real//.{PowerOf[a_]:>PowerOf[a][1]};
realL36R45 = real/.{Den[x_,y_]:>Den[x/.widths,y/.widths]}

real = CalcFeynAmp[CreateFeynAmp[insR36L45]/.{EL->EL PowerOf[EL], GS->GS PowerOf[GS]}];
real = real//.{PowerOf[a_]^x_:>PowerOf[a][x]};
real = real//.{PowerOf[a_]:>PowerOf[a][1]};
realR36L45 = real/.{Den[x_,y_]:>Den[x/.widths,y/.widths]}

real = CalcFeynAmp[CreateFeynAmp[insR36R45]/.{EL->EL PowerOf[EL], GS->GS PowerOf[GS]}];
real = real//.{PowerOf[a_]^x_:>PowerOf[a][x]};
real = real//.{PowerOf[a_]:>PowerOf[a][1]};
realR36R45 = real/.{Den[x_,y_]:>Den[x/.widths,y/.widths]}


Print["Non resonant Diagrams"]

tops = CreateTopologies[0, 2 -> 4];
ins = InsertFields[tops, process];

(*Select the diagrams without on-shell divergences*)
insNR = DiagramSelect[ins,(Not[SChannelExtQ[S[_],3,5][##] && SChannelExtQ[S[_],4,6][##]] &&
                            Not[SChannelExtQ[S[_],3,6][##] && SChannelExtQ[S[_],4,5][##]])&];
DoPaint[insNR, "realNR"];

(*insert the particle widths*)
(*widths={MZ2->MZ2-I WZ MZ, MW2->MW2-I WW MW};*)
widths={MZ2->MZ2-I WZ MZ, MW2->MW2-I WW MW, MSf2[sfe_,n1_,n2_]:>MSf2[sfe,n1,n2]-I WSf[sfe,n1,n2] MSf[sfe,n1,n2]};

realNR = CalcFeynAmp[CreateFeynAmp[insNR]/.{EL->EL PowerOf[EL], GS->GS PowerOf[GS]}];
realNR = realNR//.{PowerOf[a_]^x_:>PowerOf[a][x]};
realNR = realNR//.{PowerOf[a_]:>PowerOf[a][1]};
realNR = realNR/.{Den[x_,y_]:>Den[x/.widths,y/.widths]}


(*Write files for on-shell resonant reals, L35L46*)
amps = {realL35L46};
{realOS} = Abbreviate[amps, 6, Preprocess -> OnSize[100, Simplify, 500, DenCollect]];

col = ColourME[All, realOS];

abbr = OptimizeAbbr[Abbr[]];
subexpr = OptimizeAbbr[Subexpr[]];

dir = SetupCodeDir[name<>"_realOS_L35L46", Drivers -> name <> "_drivers"];
WriteSquaredME[realOS, {}, col, abbr, subexpr, dir];


(*Write files for on-shell resonant reals, L35R46*)
amps = {realL35R46};
{realOS} = Abbreviate[amps, 6, Preprocess -> OnSize[100, Simplify, 500, DenCollect]];

col = ColourME[All, realOS];

abbr = OptimizeAbbr[Abbr[]];
subexpr = OptimizeAbbr[Subexpr[]];

dir = SetupCodeDir[name<>"_realOS_L35R46", Drivers -> name <> "_drivers"];
WriteSquaredME[realOS, {}, col, abbr, subexpr, dir];


(*Write files for on-shell resonant reals, R35L46*)
amps = {realR35L46};
{realOS} = Abbreviate[amps, 6, Preprocess -> OnSize[100, Simplify, 500, DenCollect]];

col = ColourME[All, realOS];

abbr = OptimizeAbbr[Abbr[]];
subexpr = OptimizeAbbr[Subexpr[]];

dir = SetupCodeDir[name<>"_realOS_R35L46", Drivers -> name <> "_drivers"];
WriteSquaredME[realOS, {}, col, abbr, subexpr, dir];


(*Write files for on-shell resonant reals, R35R46*)
amps = {realR35R46};
{realOS} = Abbreviate[amps, 6, Preprocess -> OnSize[100, Simplify, 500, DenCollect]];

col = ColourME[All, realOS];

abbr = OptimizeAbbr[Abbr[]];
subexpr = OptimizeAbbr[Subexpr[]];

dir = SetupCodeDir[name<>"_realOS_R35R46", Drivers -> name <> "_drivers"];
WriteSquaredME[realOS, {}, col, abbr, subexpr, dir];


(*Write files for on-shell resonant reals, L36L45*)
amps = {realL36L45};
{realOS} = Abbreviate[amps, 6, Preprocess -> OnSize[100, Simplify, 500, DenCollect]];

col = ColourME[All, realOS];

abbr = OptimizeAbbr[Abbr[]];
subexpr = OptimizeAbbr[Subexpr[]];

dir = SetupCodeDir[name<>"_realOS_L36L45", Drivers -> name <> "_drivers"];
WriteSquaredME[realOS, {}, col, abbr, subexpr, dir];


(*Write files for on-shell resonant reals, L36R45*)
amps = {realL36R45};
{realOS} = Abbreviate[amps, 6, Preprocess -> OnSize[100, Simplify, 500, DenCollect]];

col = ColourME[All, realOS];

abbr = OptimizeAbbr[Abbr[]];
subexpr = OptimizeAbbr[Subexpr[]];

dir = SetupCodeDir[name<>"_realOS_L36R45", Drivers -> name <> "_drivers"];
WriteSquaredME[realOS, {}, col, abbr, subexpr, dir];


(*Write files for on-shell resonant reals, R36L45*)
amps = {realR36L45};
{realOS} = Abbreviate[amps, 6, Preprocess -> OnSize[100, Simplify, 500, DenCollect]];

col = ColourME[All, realOS];

abbr = OptimizeAbbr[Abbr[]];
subexpr = OptimizeAbbr[Subexpr[]];

dir = SetupCodeDir[name<>"_realOS_R36L45", Drivers -> name <> "_drivers"];
WriteSquaredME[realOS, {}, col, abbr, subexpr, dir];


(*Write files for on-shell resonant reals, R36R45*)
amps = {realR36R45};
{realOS} = Abbreviate[amps, 6, Preprocess -> OnSize[100, Simplify, 500, DenCollect]];

col = ColourME[All, realOS];

abbr = OptimizeAbbr[Abbr[]];
subexpr = OptimizeAbbr[Subexpr[]];

dir = SetupCodeDir[name<>"_realOS_R36R45", Drivers -> name <> "_drivers"];
WriteSquaredME[realOS, {}, col, abbr, subexpr, dir];


(*Combine the amplitudes again, but this time the resonant diagrams are regulated*)
real = Combine[realNR,realL35L46,realL35R46,realR35L46,realR35R46, realL36L45,realL36R45,realR36L45,realR36R45];

(*Write real files with inserted regulator for on-shell diagrams*)
amps = {real};
{real} = Abbreviate[amps, 6, Preprocess -> OnSize[100, Simplify, 500, DenCollect]];

col = ColourME[All, real];

abbr = OptimizeAbbr[Abbr[]];
subexpr = OptimizeAbbr[Subexpr[]];

dir = SetupCodeDir[name<>"_real", Drivers -> name <> "_drivers"];
WriteSquaredME[real, {}, col, abbr, subexpr, dir];


(*Write files of non resonant reals*)
amps = {realNR};
{real} = Abbreviate[amps, 6, Preprocess -> OnSize[100, Simplify, 500, DenCollect]];

col = ColourME[All, real];

abbr = OptimizeAbbr[Abbr[]];
subexpr = OptimizeAbbr[Subexpr[]];

dir = SetupCodeDir[name<>"_realNR", Drivers -> name <> "_drivers"];
WriteSquaredME[real, {}, col, abbr, subexpr, dir];


Print["time used: ", SessionTime[] - time1]
Exit[];
