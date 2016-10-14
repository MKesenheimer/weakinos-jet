(* ::Package:: *)

(*      neu1neu2+jet.m
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


(*count the arguments of a function*)
ClearAll[countArgs];
SetAttributes[countArgs,{(*HoldAll,*)Listable}];
countArgs[f_Symbol]:=With[{dv=DownValues[f]},countArgs[dv]];
countArgs[Verbatim[HoldPattern][HoldPattern[f_Symbol[args___]]]:>_]:=countArgs[f[args]];
countArgs[f_[Except[_Optional|_OptionsPattern|Verbatim[Pattern][_,_OptionsPattern]],rest___]]:={1,0,0}+countArgs[f[rest]];
countArgs[f_[o__Optional,rest___]]:={0,Length[HoldComplete[o]],0}+countArgs[f[rest]];
countArgs[f_[_OptionsPattern|Verbatim[Pattern][_,_OptionsPattern]]]:={0,0,1};
countArgs[f_[]]:={0,0,0};

countArgs[f[x,1]]
countArgs[f[1,2,3]]


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
    (p[1] = "qdbar";
     p[2] = "qd";
     p[3] = "nI";
     p[4] = "nJ";
     p[5] = "qdbar";
     p[6] = "qd";)
]

CalcProcess = p[1]<>p[2]<>"_"<>p[3]<>p[4]<>p[5]<>p[6];
name = CalcProcess;
Print[CalcProcess]

IOGluon = False;
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
If[p[i] === "g", (IOGluon = True; P[i] = V[5]),

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
If[IOGluon,
          (*no internal Weakinos*)
          lastsel = {!F[11],!F[12]};
          (*else*),
          (*no internal Weakinos, but internal gluons or gluinos required*)
          lastsel = {!F[11],!F[12],V[5]|F[15]};
]
SetOptions[InsertFields, Model -> "MSSMCT",
           (*No Fermion-Higgs coupling*)
           Restrictions -> {NoLightFHCoupling},
           (*Exclude Top, Higgs, Neutrinos, massive Leptons, Sneutrinos, Sleptons*)
           ExcludeParticles -> {S[1|2|3|4|5|6|11|12], F[1|2]},
           LastSelections -> lastsel];

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
(*Get the object that gets replaced in a replacement list {a\[Rule]4} => a*)
getReplacementHead[f_->_]:=f


Print["Non resonant Diagrams"]

tops = CreateTopologies[0, 2 -> 4];
topNR = TopologyList[Sequence@@Delete[Level[tops,1],{{104},{119},{213},{214},{106},{220},{122},{219},{165},{177},{120},{169},{181},{125},{138},{153},{123},{142},{157},{127}}]];
insNR = InsertFields[topNR, process];
DoPaint[insNR, "realNR"];

(*insert the particle widths*)
widths={MZ2->MZ2-I WZ MZ, MW2->MW2-I WW MW, MSf2[sfe_,n1_,n2_]:>MSf2[sfe,n1,n2]-I WSf[sfe,n1,n2] MSf[sfe,n1,n2], MGl2->MGl2-I MGl WGl};

realNR = CalcFeynAmp[CreateFeynAmp[insNR](*/.{EL->EL PowerOf[EL], GS->GS PowerOf[GS]}*)(*, InvSimplify -> False*)];
(*realNR = realNR//.{PowerOf[a_]^x_:>PowerOf[a][x]};
realNR = realNR//.{PowerOf[a_]:>PowerOf[a][1]};*)
realNR = realNR/.{Den[x_,y_]:>Den[x,y/.widths]}


Print["On-Shell Diagrams"]

top356=TopologyList[Topology[1][Propagator[Incoming][Vertex[1][1],Vertex[3][7]],
    Propagator[Incoming][Vertex[1][2],Vertex[3][8]],
    Propagator[Outgoing][Vertex[1][3],Vertex[3][9]],
    Propagator[Outgoing][Vertex[1][4],Vertex[3][7]],
    Propagator[Outgoing][Vertex[1][5],Vertex[3][9]],
    Propagator[Outgoing][Vertex[1][6],Vertex[3][10]],
    Propagator[Internal][Vertex[3][8],Vertex[3][10]],
    Propagator[Internal][Vertex[3][9],Vertex[3][10]],
    Propagator[Internal][Vertex[3][7],Vertex[3][8]]],
    Topology[1][Propagator[Incoming][Vertex[1][1],Vertex[3][7]],
    Propagator[Incoming][Vertex[1][2],Vertex[3][8]],
    Propagator[Outgoing][Vertex[1][3],Vertex[3][9]],
    Propagator[Outgoing][Vertex[1][4],Vertex[3][8]],
    Propagator[Outgoing][Vertex[1][5],Vertex[3][9]],
    Propagator[Outgoing][Vertex[1][6],Vertex[3][10]],
    Propagator[Internal][Vertex[3][7],Vertex[3][10]],
    Propagator[Internal][Vertex[3][9],Vertex[3][10]],
    Propagator[Internal][Vertex[3][7],Vertex[3][8]]],
    Topology[1][Propagator[Incoming][Vertex[1][1],Vertex[3][7]],
    Propagator[Incoming][Vertex[1][2],Vertex[3][7]],
    Propagator[Outgoing][Vertex[1][3],Vertex[3][8]],
    Propagator[Outgoing][Vertex[1][4],Vertex[3][9]],
    Propagator[Outgoing][Vertex[1][5],Vertex[3][8]],
    Propagator[Outgoing][Vertex[1][6],Vertex[3][10]],
    Propagator[Internal][Vertex[3][9],Vertex[3][10]],
    Propagator[Internal][Vertex[3][8],Vertex[3][10]],
    Propagator[Internal][Vertex[3][7],Vertex[3][9]]]];

top365=TopologyList[Topology[1][Propagator[Incoming][Vertex[1][1],Vertex[3][7]],
    Propagator[Incoming][Vertex[1][2],Vertex[3][8]],
    Propagator[Outgoing][Vertex[1][3],Vertex[3][9]],
    Propagator[Outgoing][Vertex[1][4],Vertex[3][7]],
    Propagator[Outgoing][Vertex[1][5],Vertex[3][10]],
    Propagator[Outgoing][Vertex[1][6],Vertex[3][9]],
    Propagator[Internal][Vertex[3][8],Vertex[3][10]],
    Propagator[Internal][Vertex[3][9],Vertex[3][10]],
    Propagator[Internal][Vertex[3][7],Vertex[3][8]]],
    Topology[1][Propagator[Incoming][Vertex[1][1],Vertex[3][7]],
    Propagator[Incoming][Vertex[1][2],Vertex[3][8]],
    Propagator[Outgoing][Vertex[1][3],Vertex[3][9]],
    Propagator[Outgoing][Vertex[1][4],Vertex[3][8]],
    Propagator[Outgoing][Vertex[1][5],Vertex[3][10]],
    Propagator[Outgoing][Vertex[1][6],Vertex[3][9]],
    Propagator[Internal][Vertex[3][7],Vertex[3][10]],
    Propagator[Internal][Vertex[3][9],Vertex[3][10]],
    Propagator[Internal][Vertex[3][7],Vertex[3][8]]],
    Topology[1][Propagator[Incoming][Vertex[1][1],Vertex[3][7]],
    Propagator[Incoming][Vertex[1][2],Vertex[3][7]],
    Propagator[Outgoing][Vertex[1][3],Vertex[3][8]],
    Propagator[Outgoing][Vertex[1][4],Vertex[3][9]],
    Propagator[Outgoing][Vertex[1][5],Vertex[3][10]],
    Propagator[Outgoing][Vertex[1][6],Vertex[3][8]],
    Propagator[Internal][Vertex[3][9],Vertex[3][10]],
    Propagator[Internal][Vertex[3][8],Vertex[3][10]],
    Propagator[Internal][Vertex[3][7],Vertex[3][9]]]];

top456=TopologyList[Topology[1][Propagator[Incoming][Vertex[1][1],Vertex[3][7]],
    Propagator[Incoming][Vertex[1][2],Vertex[3][8]],
    Propagator[Outgoing][Vertex[1][3],Vertex[3][7]],
    Propagator[Outgoing][Vertex[1][4],Vertex[3][9]],
    Propagator[Outgoing][Vertex[1][5],Vertex[3][9]],
    Propagator[Outgoing][Vertex[1][6],Vertex[3][10]],
    Propagator[Internal][Vertex[3][8],Vertex[3][10]],
    Propagator[Internal][Vertex[3][9],Vertex[3][10]],
    Propagator[Internal][Vertex[3][7],Vertex[3][8]]],
    Topology[1][Propagator[Incoming][Vertex[1][1],Vertex[3][7]],
    Propagator[Incoming][Vertex[1][2],Vertex[3][8]],
    Propagator[Outgoing][Vertex[1][3],Vertex[3][8]],
    Propagator[Outgoing][Vertex[1][4],Vertex[3][9]],
    Propagator[Outgoing][Vertex[1][5],Vertex[3][9]],
    Propagator[Outgoing][Vertex[1][6],Vertex[3][10]],
    Propagator[Internal][Vertex[3][7],Vertex[3][10]],
    Propagator[Internal][Vertex[3][9],Vertex[3][10]],
    Propagator[Internal][Vertex[3][7],Vertex[3][8]]],
    Topology[1][Propagator[Incoming][Vertex[1][1],Vertex[3][7]],
    Propagator[Incoming][Vertex[1][2],Vertex[3][7]],
    Propagator[Outgoing][Vertex[1][3],Vertex[3][8]],
    Propagator[Outgoing][Vertex[1][4],Vertex[3][9]],
    Propagator[Outgoing][Vertex[1][5],Vertex[3][9]],
    Propagator[Outgoing][Vertex[1][6],Vertex[3][10]],
    Propagator[Internal][Vertex[3][8],Vertex[3][10]],
    Propagator[Internal][Vertex[3][9],Vertex[3][10]],
    Propagator[Internal][Vertex[3][7],Vertex[3][8]]]];

top465=TopologyList[Topology[1][Propagator[Incoming][Vertex[1][1],Vertex[3][7]],
    Propagator[Incoming][Vertex[1][2],Vertex[3][8]],
    Propagator[Outgoing][Vertex[1][3],Vertex[3][7]],
    Propagator[Outgoing][Vertex[1][4],Vertex[3][9]],
    Propagator[Outgoing][Vertex[1][5],Vertex[3][10]],
    Propagator[Outgoing][Vertex[1][6],Vertex[3][9]],
    Propagator[Internal][Vertex[3][8],Vertex[3][10]],
    Propagator[Internal][Vertex[3][9],Vertex[3][10]],
    Propagator[Internal][Vertex[3][7],Vertex[3][8]]],
    Topology[1][Propagator[Incoming][Vertex[1][1],Vertex[3][7]],
    Propagator[Incoming][Vertex[1][2],Vertex[3][8]],
    Propagator[Outgoing][Vertex[1][3],Vertex[3][8]],
    Propagator[Outgoing][Vertex[1][4],Vertex[3][9]],
    Propagator[Outgoing][Vertex[1][5],Vertex[3][10]],
    Propagator[Outgoing][Vertex[1][6],Vertex[3][9]],
    Propagator[Internal][Vertex[3][7],Vertex[3][10]],
    Propagator[Internal][Vertex[3][9],Vertex[3][10]],
    Propagator[Internal][Vertex[3][7],Vertex[3][8]]],
    Topology[1][Propagator[Incoming][Vertex[1][1],Vertex[3][7]],
    Propagator[Incoming][Vertex[1][2],Vertex[3][7]],
    Propagator[Outgoing][Vertex[1][3],Vertex[3][8]],
    Propagator[Outgoing][Vertex[1][4],Vertex[3][9]],
    Propagator[Outgoing][Vertex[1][5],Vertex[3][10]],
    Propagator[Outgoing][Vertex[1][6],Vertex[3][9]],
    Propagator[Internal][Vertex[3][8],Vertex[3][10]],
    Propagator[Internal][Vertex[3][9],Vertex[3][10]],
    Propagator[Internal][Vertex[3][7],Vertex[3][8]]]];

ins356 = InsertFields[top356, process];
DoPaint[ins356, "realOS_356"];
ins365 = InsertFields[top365, process];
DoPaint[ins356, "realOS_365"];
ins456 = InsertFields[top456, process];
DoPaint[ins356, "realOS_456"];
ins465 = InsertFields[top465, process];
DoPaint[ins356, "realOS_465"];


(*widths and regulator replacement rules*)
widths = {MZ2->MZ2-I WZ MZ, MW2->MW2-I WW MW, MSf2[sfe_,n1_,n2_]:>MSf2[sfe,n1,n2]-I WSf[sfe,n1,n2] MSf[sfe,n1,n2], MGl2->MGl2-I MGl WGl};
reg1 = {Den[sijk_,MGl2-I MGl WGl]:>Den[sijk,MGl2-I MGl (WGl+WREG1)]};
reg2 = {Den[sij_,MSf2[cij_,tij_,gij_]-I MSf[cij_,tij_,gij_] WSf[cij_,tij_,gij_]]Den[skl_,MSf2[ckl_,tkl_,gkl_]-I MSf[ckl_,tkl_,gkl_] WSf[ckl_,tkl_,gkl_]]:>((Den[sij,MSf2[cij,tij,gij]-I MSf[cij,tij,gij] (WSf[cij,tij,gij]+WREG2)]^-1+Den[skl,MSf2[ckl,tkl,gkl]-I MSf[ckl,tkl,gkl] (WSf[ckl,tkl,gkl]+WREG2)]^-1)^-1)*(Den[sij,MSf2[cij,tij,gij]-I MSf[cij,tij,gij] WSf[cij,tij,gij]]+Den[skl,MSf2[ckl,tkl,gkl]-I MSf[ckl,tkl,gkl] WSf[ckl,tkl,gkl]])};

(*generate amplitudes*)
real356 = CalcFeynAmp[CreateFeynAmp[ins356](*/.{EL->EL PowerOf[EL], GS->GS PowerOf[GS]}*)(*, InvSimplify -> False*)]; (*uncomment ", InvSimplify -> False" for Mac OS X*)
real365 = CalcFeynAmp[CreateFeynAmp[ins365](*/.{EL->EL PowerOf[EL], GS->GS PowerOf[GS]}*)(*, InvSimplify -> False*)];
real456 = CalcFeynAmp[CreateFeynAmp[ins456](*/.{EL->EL PowerOf[EL], GS->GS PowerOf[GS]}*)(*, InvSimplify -> False*)]; 
real465 = CalcFeynAmp[CreateFeynAmp[ins465](*/.{EL->EL PowerOf[EL], GS->GS PowerOf[GS]}*)(*, InvSimplify -> False*)];
(*real = real//.{PowerOf[a_]^x_:>PowerOf[a][x]};
real = real//.{PowerOf[a_]:>PowerOf[a][1]};*)
(*insert widths*)
real356 = real356/.{Den[x_,y_]:>Den[x,y/.widths]};
real365 = real365/.{Den[x_,y_]:>Den[x,y/.widths]};
real456 = real456/.{Den[x_,y_]:>Den[x,y/.widths]};
real465 = real465/.{Den[x_,y_]:>Den[x,y/.widths]};
(*insert the regulator*)
real356 = real356/.reg1
real365 = real365/.reg1
real456 = real456/.reg1
real465 = real465/.reg1


Print["Non resonant Diagrams (gluino single poles removed)"]

(*Test, plot all diagrams*)
tops = CreateTopologies[0, 2 -> 4];
ins = InsertFields[tops, process];
DoPaint[ins, "realAll"];

topsNR = TopologyList[Sequence@@Delete[
           Level[tops,1],
          {{104},{119},{213},{214},
           {106},{220},{122},{219},
           {165},{177},{120},{169},
           {181},{125},{138},{153},
           {123},{142},{157},{127}}]];
insNR = InsertFields[topsNR, process];
DoPaint[insNR, "realNR"];

(*insert the particle widths*)
widths={MZ2->MZ2-I WZ MZ, MW2->MW2-I WW MW, MSf2[sfe_,n1_,n2_]:>MSf2[sfe,n1,n2]-I WSf[sfe,n1,n2] MSf[sfe,n1,n2], MGl2->MGl2-I MGl WGl};

realNR = CalcFeynAmp[CreateFeynAmp[insNR](*/.{EL->EL PowerOf[EL], GS->GS PowerOf[GS]}*)(*, InvSimplify -> False*)];
(*realNR = realNR//.{PowerOf[a_]^x_:>PowerOf[a][x]};
realNR = realNR//.{PowerOf[a_]:>PowerOf[a][1]};*)
realNR = realNR/.{Den[x_,y_]:>Den[x,y/.widths]}


Print["time used: ", SessionTime[] - time1]
Exit[];
