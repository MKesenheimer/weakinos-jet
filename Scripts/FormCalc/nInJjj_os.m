(* ::Package:: *)

(*
generates the Fortran code for
p p -> weakino weakino jet jet in the MSSM
last modified November 2016

Note: Generating the non-resonant amplitudes might be version dependent.
To be on the sure side use FeynArts 3.9 and FormCal 8.4.
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


(*Process dependent input parameters and flags*)
(*You can now load the script with the command $ MathKernel -script nInJjj_os.m "qu" "qubar" "nI" "nJ" "qd" "qdbar"*)
Print[$CommandLine]
If[$CommandLine[[2]] === "-script",
  (p[1] = ToString[$CommandLine[[4]]];
   p[2] = ToString[$CommandLine[[5]]];
   p[3] = ToString[$CommandLine[[6]]];
   p[4] = ToString[$CommandLine[[7]]];
   p[5] = ToString[$CommandLine[[8]]];
   p[6] = ToString[$CommandLine[[9]]];),
  (*Else*)
  (p[1] = "qu";
   p[2] = "qubar";
   p[3] = "nI";
   p[4] = "nJ";
   p[5] = "qd";
   p[6] = "qdbar";)
]

CalcProcess = p[1]<>p[2]<>"_"<>p[3]<>p[4]<>p[5]<>p[6];
name = CalcProcess;
Print[CalcProcess]

isIOGluon = False;
For[i=1, i<=6, i++,
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

If[p[i] === "g", (isIOGluon = True; P[i] = V[5]),
If[p[i] === "gam", P[i] = V[1],
If[p[i] === "Z", P[i] = V[2],
If[p[i] === "W+", P[i] = V[3],
If[p[i] === "W-", P[i] = -V[3],

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
]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]
]

process = {P[1], P[2]} -> {P[3], P[4], P[5], P[6]};
Print["Process: ", process]

(*Check if it is a process with gluino single resonances,*)
(*gluino resonances can only occur in processes with same type*)
(*of quark in initial or final state*)
isGluinoRes = And[P[1] === -P[2], P[5] === -P[6], Not[P[1] === V[5]], Not[P[5] === V[5]] ];

(*Check if it is a process with squark double resonances in two distinct pairs of legs (mandelstam s_ij and s_kl).*)
(*These types of resonances can only occur in processes with no gluon in the final state.*)
isSquarkRes1 = And[Not[P[5] === V[5]], Not[P[6] === V[5]]];

(*Check if it is a process with squark double resonances in three legs (mandelstam s_ijk)*)
(*These types of resonances can only occur in processes with only one gluon in the final state.*)
(*Note: These resonances are not critical since they can only occur for very small jet momementa.*)
(*With a sufficient high cut on the transverse jet momenta they vanish, set isSquarkRes2 to false..*)
(*isSquarkRes2 = Or[And[Not[P[5] === V[5]], P[6] === V[5]], And[P[5] === V[5], Not[P[6] === V[5]]]];*)
isSquarkRes2 = False;

(*Print Flags*)
Print["isIOGluon: ", isIOGluon]
Print["isGluinoRes: ", isGluinoRes]
Print["isSquarkRes1: ", isSquarkRes1]
Print["isSquarkRes2: ", isSquarkRes2]

(*Quit[]*)


(*Options and parameters*)
If[isIOGluon,
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
  ExcludeParticles -> {S[11|12], F[1|2]},
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
(*Note: all possible diagrams are calculated, but after the feynman amplitude is generated*)
(*only the amplitudes with specified order of the coupling constants survive.*)
(*Check results with coupl_order.nb*)
OrderEL = 2;
OrderGS = 2;
PowerOf[GS][x_Integer] := 0/;x>OrderGS
PowerOf[GS][OrderGS] := 1
PowerOf[EL][x_Integer] := 0/;x>OrderEL
PowerOf[EL][OrderEL] := 1

(*Neglect Masses (URL)*)
Neglect[MU] = Neglect[MU2] = 0;
Neglect[MC] = Neglect[MC2] = 0;
Neglect[MD] = Neglect[MD2] = 0;
Neglect[MS] = Neglect[MS2] = 0;
Neglect[MUC] = Neglect[MU2C] = 0;
Neglect[MCC] = Neglect[MC2C] = 0;
Neglect[MDC] = Neglect[MD2C] = 0;
Neglect[MSC] = Neglect[MS2C] = 0;
Neglect[_Mf] = Neglect[_Mf2] = 0;
Neglect[_MfC] = Neglect[_Mf2C] = 0;
(*Neglect[MB] = Neglect[MB2] = 0;
Neglect[MT] = Neglect[MT2] = 0;*)


If[isSquarkRes1,
  Print["On-shell squark double resonances type 1"];

  (*On-shell topologies built by hand*)
  top3546 =  TopologyList[
    Topology[1][Propagator[Incoming][Vertex[1][1],Vertex[4][7]],
    Propagator[Incoming][Vertex[1][2],Vertex[4][7]],
    Propagator[Outgoing][Vertex[1][3],Vertex[3][8]],
    Propagator[Outgoing][Vertex[1][4],Vertex[3][9]],
    Propagator[Outgoing][Vertex[1][5],Vertex[3][8]],
    Propagator[Outgoing][Vertex[1][6],Vertex[3][9]],
    Propagator[Internal][Vertex[3][8],Vertex[4][7]],
    Propagator[Internal][Vertex[3][9],Vertex[4][7]]],
    Topology[1][Propagator[Incoming][Vertex[1][1],Vertex[3][7]],
    Propagator[Incoming][Vertex[1][2],Vertex[3][7]],
    Propagator[Outgoing][Vertex[1][3],Vertex[3][8]],
    Propagator[Outgoing][Vertex[1][4],Vertex[3][9]],
    Propagator[Outgoing][Vertex[1][5],Vertex[3][8]],
    Propagator[Outgoing][Vertex[1][6],Vertex[3][9]],
    Propagator[Internal][Vertex[3][8],Vertex[3][10]],
    Propagator[Internal][Vertex[3][9],Vertex[3][10]],
    Propagator[Internal][Vertex[3][7],Vertex[3][10]]],
    Topology[1][Propagator[Incoming][Vertex[1][1],Vertex[3][7]],
    Propagator[Incoming][Vertex[1][2],Vertex[3][8]],
    Propagator[Outgoing][Vertex[1][3],Vertex[3][9]],
    Propagator[Outgoing][Vertex[1][4],Vertex[3][10]],
    Propagator[Outgoing][Vertex[1][5],Vertex[3][9]],
    Propagator[Outgoing][Vertex[1][6],Vertex[3][10]],
    Propagator[Internal][Vertex[3][7],Vertex[3][9]],
    Propagator[Internal][Vertex[3][8],Vertex[3][10]],
    Propagator[Internal][Vertex[3][7],Vertex[3][8]]],
    Topology[1][Propagator[Incoming][Vertex[1][1],Vertex[3][7]],
    Propagator[Incoming][Vertex[1][2],Vertex[3][8]],
    Propagator[Outgoing][Vertex[1][3],Vertex[3][9]],
    Propagator[Outgoing][Vertex[1][4],Vertex[3][10]],
    Propagator[Outgoing][Vertex[1][5],Vertex[3][9]],
    Propagator[Outgoing][Vertex[1][6],Vertex[3][10]],
    Propagator[Internal][Vertex[3][8],Vertex[3][9]],
    Propagator[Internal][Vertex[3][7],Vertex[3][10]],
    Propagator[Internal][Vertex[3][7],Vertex[3][8]]]];

  top3645 = TopologyList[Topology[1][Propagator[Incoming][Vertex[1][1],Vertex[4][7]],
    Propagator[Incoming][Vertex[1][2],Vertex[4][7]],
    Propagator[Outgoing][Vertex[1][3],Vertex[3][8]],
    Propagator[Outgoing][Vertex[1][4],Vertex[3][9]],
    Propagator[Outgoing][Vertex[1][5],Vertex[3][9]],
    Propagator[Outgoing][Vertex[1][6],Vertex[3][8]],
    Propagator[Internal][Vertex[3][8],Vertex[4][7]],
    Propagator[Internal][Vertex[3][9],Vertex[4][7]]],
    Topology[1][Propagator[Incoming][Vertex[1][1],Vertex[3][7]],
    Propagator[Incoming][Vertex[1][2],Vertex[3][8]],
    Propagator[Outgoing][Vertex[1][3],Vertex[3][9]],
    Propagator[Outgoing][Vertex[1][4],Vertex[3][10]],
    Propagator[Outgoing][Vertex[1][5],Vertex[3][10]],
    Propagator[Outgoing][Vertex[1][6],Vertex[3][9]],
    Propagator[Internal][Vertex[3][8],Vertex[3][9]],
    Propagator[Internal][Vertex[3][7],Vertex[3][10]],
    Propagator[Internal][Vertex[3][7],Vertex[3][8]]],
    Topology[1][Propagator[Incoming][Vertex[1][1],Vertex[3][7]],
    Propagator[Incoming][Vertex[1][2],Vertex[3][7]],
    Propagator[Outgoing][Vertex[1][3],Vertex[3][8]],
    Propagator[Outgoing][Vertex[1][4],Vertex[3][9]],
    Propagator[Outgoing][Vertex[1][5],Vertex[3][9]],
    Propagator[Outgoing][Vertex[1][6],Vertex[3][8]],
    Propagator[Internal][Vertex[3][8],Vertex[3][10]],
    Propagator[Internal][Vertex[3][9],Vertex[3][10]],
    Propagator[Internal][Vertex[3][7],Vertex[3][10]]],
    Topology[1][Propagator[Incoming][Vertex[1][1],Vertex[3][7]],
    Propagator[Incoming][Vertex[1][2],Vertex[3][8]],
    Propagator[Outgoing][Vertex[1][3],Vertex[3][9]],
    Propagator[Outgoing][Vertex[1][4],Vertex[3][10]],
    Propagator[Outgoing][Vertex[1][5],Vertex[3][10]],
    Propagator[Outgoing][Vertex[1][6],Vertex[3][9]],
    Propagator[Internal][Vertex[3][7],Vertex[3][9]],
    Propagator[Internal][Vertex[3][8],Vertex[3][10]],
    Propagator[Internal][Vertex[3][7],Vertex[3][8]]]];

  ins3546 = InsertFields[top3546, process];
  DoPaint[ins3546, "realOS_3546_Sq1"];
  ins3645 = InsertFields[top3645, process];
  DoPaint[ins3645, "realOS_3645_Sq1"];

  (*widths and regulator replacement rules*)
  widths = {MZ2->MZ2-I WZ MZ, MW2->MW2-I WW MW, MSf2[sfe_,n1_,n2_]:>MSf2[sfe,n1,n2]-I WSf[sfe,n1,n2] MSf[sfe,n1,n2], MGl2->MGl2-I MGl WGl};
  reg2 = {Den[sij_,MSf2[cij_,tij_,gij_]-I MSf[cij_,tij_,gij_] WSf[cij_,tij_,gij_]]Den[skl_,MSf2[ckl_,tkl_,gkl_]-I MSf[ckl_,tkl_,gkl_] WSf[ckl_,tkl_,gkl_]]:>((Den[sij,MSf2[cij,tij,gij]-I MSf[cij,tij,gij] (WSf[cij,tij,gij]+WREG2)]^-1+Den[skl,MSf2[ckl,tkl,gkl]-I MSf[ckl,tkl,gkl] (WSf[ckl,tkl,gkl]+WREG2)]^-1)^-1)*(Den[sij,MSf2[cij,tij,gij]-I MSf[cij,tij,gij] WSf[cij,tij,gij]]+Den[skl,MSf2[ckl,tkl,gkl]-I MSf[ckl,tkl,gkl] WSf[ckl,tkl,gkl]])};
  
  (*generate amplitudes*)
  real3546Sq1 = CalcFeynAmp[CreateFeynAmp[ins3546](*/.{EL->EL PowerOf[EL], GS->GS PowerOf[GS]}*)(*, InvSimplify -> False*)]; (*uncomment ", InvSimplify -> False" for Mac OS X*)
  real3645Sq1 = CalcFeynAmp[CreateFeynAmp[ins3645](*/.{EL->EL PowerOf[EL], GS->GS PowerOf[GS]}*)(*, InvSimplify -> False*)];
  (*real = real/.{PowerOf[a_]^x_:>PowerOf[a][x]};
  real = real/.{PowerOf[a_]:>PowerOf[a][1]};*)
  
  (*insert widths*)
  real3546Sq1 = real3546Sq1/.{Den[x_,y_]:>Den[x,y/.widths]};
  real3645Sq1 = real3645Sq1/.{Den[x_,y_]:>Den[x,y/.widths]};
  (*insert the regulator*)
  real3546Sq1 = real3546Sq1/.reg2;
  real3645Sq1 = real3645Sq1/.reg2;

  (*Print the results*)
  (*Print["real3546Sq1 = "];
  Print[real3546Sq1];
  Print["real3645Sq1 = "];
  Print[real3645Sq1];*)

  (*generate the os-amplitudes with sums which are evaluated externally*)
  (*insert an additional IndexDelta if the sum over sfermion indices occurs only once*)
  (*we sum over two sfermion indices in the external fortran program*)
  (*Hack: If an IndexDelta[Sfe7, Sfe8] is inserted FormCalc gets automatically rid of it after simplifying the summed expression.*)
  (*=> We have to insert the place holder IndexDelta[Sq1, Sq2] which is not known by FormCalc.*)
  Clear[AdditionalDelta];
  AdditionalDelta[args__] := IndexDelta[Sq1,Sq2] /;
    (! FreeQ[{args}, SumOver[Index[Sfermion, 7], 2]] && FreeQ[{args}, SumOver[Index[Sfermion, 8], 2]]) ||
    (FreeQ[{args}, SumOver[Index[Sfermion, 8], 2]] && ! FreeQ[{args}, SumOver[Index[Sfermion, 7], 2]]);
  _AdditionalDelta = 1;
  
  (*generate amplitudes*)
  famps3546 = CreateFeynAmp[ins3546];
  famps3546 = MultiplyDiagrams[AdditionalDelta][famps3546];
  real3546Sq1ExtSum = CalcFeynAmp[famps3546(*/.{EL->EL PowerOf[EL], GS->GS PowerOf[GS]}*)(*, InvSimplify -> False*)]; (*uncomment ", InvSimplify -> False" for Mac OS X*)
  famps3645 = CreateFeynAmp[ins3645];
  famps3645 = MultiplyDiagrams[AdditionalDelta][famps3645];
  real3645Sq1ExtSum = CalcFeynAmp[famps3645(*/.{EL->EL PowerOf[EL], GS->GS PowerOf[GS]}*)(*, InvSimplify -> False*)];
  (*real = real/.{PowerOf[a_]^x_:>PowerOf[a][x]};
  real = real/.{PowerOf[a_]:>PowerOf[a][1]};*)
  
  (*insert widths*)
  real3546Sq1ExtSum = real3546Sq1ExtSum/.{Den[x_,y_]:>Den[x,y/.widths]};
  real3645Sq1ExtSum = real3645Sq1ExtSum/.{Den[x_,y_]:>Den[x,y/.widths]};
  (*insert the regulator*)
  real3546Sq1ExtSum = real3546Sq1ExtSum/.reg2;
  real3645Sq1ExtSum = real3645Sq1ExtSum/.reg2;
  
  (*set the sfermion index with the external fortran program (leave it open here)*)
  (*first replace possible occurrences of a double sum*)
  real3546Sq1ExtSum = real3546Sq1ExtSum/.{SumOver[Sfe7,i_] SumOver[Sfe8,i_]:>SumOver[Sfe7,i,External] SumOver[Sfe8,i,External]};
  real3645Sq1ExtSum = real3645Sq1ExtSum/.{SumOver[Sfe7,i_] SumOver[Sfe8,i_]:>SumOver[Sfe7,i,External] SumOver[Sfe8,i,External]};
  (*then replace single sums with a double sum*)
  real3546Sq1ExtSum = real3546Sq1ExtSum/.{SumOver[Sfe7,i_]:>SumOver[Sfe7,i,External] SumOver[Sfe8,i,External]};
  real3546Sq1ExtSum = real3546Sq1ExtSum/.{SumOver[Sfe8,i_]:>SumOver[Sfe7,i,External] SumOver[Sfe8,i,External]};
  real3645Sq1ExtSum = real3645Sq1ExtSum/.{SumOver[Sfe7,i_]:>SumOver[Sfe7,i,External] SumOver[Sfe8,i,External]};
  real3645Sq1ExtSum = real3645Sq1ExtSum/.{SumOver[Sfe8,i_]:>SumOver[Sfe7,i,External] SumOver[Sfe8,i,External]};

  (*Compress the FormCalc amplitude*)
  real3546Sq1ExtSum = AmpSimplify[real3546Sq1ExtSum];
  real3645Sq1ExtSum = AmpSimplify[real3645Sq1ExtSum];

  (*finally replace the place holder, this is fine now since the sums over Sfe7 and Sfe8 are carried out externally*)
  real3546Sq1ExtSum = real3546Sq1ExtSum/.{Sq1->Sfe7,Sq2->Sfe8};
  real3645Sq1ExtSum = real3645Sq1ExtSum/.{Sq1->Sfe7,Sq2->Sfe8};

  (*Print the results*)
  Print["real3546Sq1ExtSum = "];
  Print[real3546Sq1ExtSum];
  Print["real3645Sq1ExtSum = "];
  Print[real3645Sq1ExtSum];
]


If[isGluinoRes,
  Print["On-shell gluino single resonances"];

  (*On-shell topologies built by hand*)
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
  DoPaint[ins356, "realOS_356_Gl"];
  ins365 = InsertFields[top365, process];
  DoPaint[ins365, "realOS_365_Gl"];
  ins456 = InsertFields[top456, process];
  DoPaint[ins456, "realOS_456_Gl"];
  ins465 = InsertFields[top465, process];
  DoPaint[ins465, "realOS_465_Gl"];

  (*widths and regulator replacement rules*)
  widths = {MZ2->MZ2-I WZ MZ, MW2->MW2-I WW MW, MSf2[sfe_,n1_,n2_]:>MSf2[sfe,n1,n2]-I WSf[sfe,n1,n2] MSf[sfe,n1,n2], MGl2->MGl2-I MGl WGl};
  reg1 = {Den[sijk_,MGl2-I MGl WGl]:>Den[sijk,MGl2-I MGl (WGl+WREG1)]};

  (*generate amplitudes*)
  real356Gl = CalcFeynAmp[CreateFeynAmp[ins356](*/.{EL->EL PowerOf[EL], GS->GS PowerOf[GS]}*)(*, InvSimplify -> False*)]; (*uncomment ", InvSimplify -> False" for Mac OS X*)
  real365Gl = CalcFeynAmp[CreateFeynAmp[ins365](*/.{EL->EL PowerOf[EL], GS->GS PowerOf[GS]}*)(*, InvSimplify -> False*)];
  real456Gl = CalcFeynAmp[CreateFeynAmp[ins456](*/.{EL->EL PowerOf[EL], GS->GS PowerOf[GS]}*)(*, InvSimplify -> False*)]; 
  real465Gl = CalcFeynAmp[CreateFeynAmp[ins465](*/.{EL->EL PowerOf[EL], GS->GS PowerOf[GS]}*)(*, InvSimplify -> False*)];
  (*real = real/.{PowerOf[a_]^x_:>PowerOf[a][x]};
  real = real/.{PowerOf[a_]:>PowerOf[a][1]};*)
  (*insert widths*)
  real356Gl = real356Gl/.{Den[x_,y_]:>Den[x,y/.widths]};
  real365Gl = real365Gl/.{Den[x_,y_]:>Den[x,y/.widths]};
  real456Gl = real456Gl/.{Den[x_,y_]:>Den[x,y/.widths]};
  real465Gl = real465Gl/.{Den[x_,y_]:>Den[x,y/.widths]};
  (*insert the regulator*)
  real356Gl = real356Gl/.reg1;
  real365Gl = real365Gl/.reg1;
  real456Gl = real456Gl/.reg1;
  real465Gl = real465Gl/.reg1;

  (*set the sfermion index in the external fortran program (leave it open here)*)
  real356GlExtSum = real356Gl/.{SumOver[Sfe7,i_]:>SumOver[Sfe7,i,External]};
  real365GlExtSum = real365Gl/.{SumOver[Sfe7,i_]:>SumOver[Sfe7,i,External]};
  real456GlExtSum = real456Gl/.{SumOver[Sfe7,i_]:>SumOver[Sfe7,i,External]};
  real465GlExtSum = real465Gl/.{SumOver[Sfe7,i_]:>SumOver[Sfe7,i,External]};

  Print[real356GlExtSum];
  Print[real365GlExtSum];
  Print[real456GlExtSum];
  Print[real465GlExtSum];
]


If[isSquarkRes2,
  Print["On-shell squark double resonances type 2"];

  (*On-shell topologies built by hand*)
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
  DoPaint[ins356, "realOS_356_Sq2"];
  ins365 = InsertFields[top365, process];
  DoPaint[ins365, "realOS_365_Sq2"];
  ins456 = InsertFields[top456, process];
  DoPaint[ins456, "realOS_456_Sq2"];
  ins465 = InsertFields[top465, process];
  DoPaint[ins465, "realOS_465_Sq2"];

  (*widths and regulator replacement rules*)
  widths = {MZ2->MZ2-I WZ MZ, MW2->MW2-I WW MW, MSf2[sfe_,n1_,n2_]:>MSf2[sfe,n1,n2]-I WSf[sfe,n1,n2] MSf[sfe,n1,n2], MGl2->MGl2-I MGl WGl};
  reg2 = {Den[sij_,MSf2[cij_,tij_,gij_]-I MSf[cij_,tij_,gij_] WSf[cij_,tij_,gij_]]Den[skl_,MSf2[ckl_,tkl_,gkl_]-I MSf[ckl_,tkl_,gkl_] WSf[ckl_,tkl_,gkl_]]:>((Den[sij,MSf2[cij,tij,gij]-I MSf[cij,tij,gij] (WSf[cij,tij,gij]+WREG2)]^-1+Den[skl,MSf2[ckl,tkl,gkl]-I MSf[ckl,tkl,gkl] (WSf[ckl,tkl,gkl]+WREG2)]^-1)^-1)*(Den[sij,MSf2[cij,tij,gij]-I MSf[cij,tij,gij] WSf[cij,tij,gij]]+Den[skl,MSf2[ckl,tkl,gkl]-I MSf[ckl,tkl,gkl] WSf[ckl,tkl,gkl]])};
  
  (*generate amplitudes*)
  real356Sq2 = CalcFeynAmp[CreateFeynAmp[ins356](*/.{EL->EL PowerOf[EL], GS->GS PowerOf[GS]}*)(*, InvSimplify -> False*)]; (*uncomment ", InvSimplify -> False" for Mac OS X*)
  real365Sq2 = CalcFeynAmp[CreateFeynAmp[ins365](*/.{EL->EL PowerOf[EL], GS->GS PowerOf[GS]}*)(*, InvSimplify -> False*)];
  real456Sq2 = CalcFeynAmp[CreateFeynAmp[ins456](*/.{EL->EL PowerOf[EL], GS->GS PowerOf[GS]}*)(*, InvSimplify -> False*)]; 
  real465Sq2 = CalcFeynAmp[CreateFeynAmp[ins465](*/.{EL->EL PowerOf[EL], GS->GS PowerOf[GS]}*)(*, InvSimplify -> False*)];
  (*real = real/.{PowerOf[a_]^x_:>PowerOf[a][x]};
  real = real/.{PowerOf[a_]:>PowerOf[a][1]};*)
  (*insert widths*)
  real356Sq2 = real356Sq2/.{Den[x_,y_]:>Den[x,y/.widths]};
  real365Sq2 = real365Sq2/.{Den[x_,y_]:>Den[x,y/.widths]};
  real456Sq2 = real456Sq2/.{Den[x_,y_]:>Den[x,y/.widths]};
  real465Sq2 = real465Sq2/.{Den[x_,y_]:>Den[x,y/.widths]};
  (*insert the regulator*)
  real356Sq2 = real356Sq2/.reg2;
  real365Sq2 = real365Sq2/.reg2;
  real456Sq2 = real456Sq2/.reg2;
  real465Sq2 = real465Sq2/.reg2;

  (*set the sfermion index in the external fortran program (leave it open here)*)
  real356Sq2ExtSum = real356Sq2/.{SumOver[Sfe7,i_]:>SumOver[Sfe7,i,External], SumOver[Sfe8,i_]:>SumOver[Sfe8,i,External]};
  real365Sq2ExtSum = real365Sq2/.{SumOver[Sfe7,i_]:>SumOver[Sfe7,i,External], SumOver[Sfe8,i_]:>SumOver[Sfe8,i,External]};
  real456Sq2ExtSum = real456Sq2/.{SumOver[Sfe7,i_]:>SumOver[Sfe7,i,External], SumOver[Sfe8,i_]:>SumOver[Sfe8,i,External]};
  real465Sq2ExtSum = real465Sq2/.{SumOver[Sfe7,i_]:>SumOver[Sfe7,i,External], SumOver[Sfe8,i_]:>SumOver[Sfe8,i,External]};

  Print[real356Sq2ExtSum];
  Print[real365Sq2ExtSum];
  Print[real456Sq2ExtSum];
  Print[real465Sq2ExtSum];
]


Print["Non resonant Diagrams (squark double and possible gluino single poles removed)"]
tops = CreateTopologies[0, 2 -> 4];
insNR = InsertFields[tops, process];
DoPaint[insNR, "realAll"];

If[Or[isGluinoRes, isSquarkRes2],
  (*remove diagrams respective topologies with gluino single resonances*)
  (*Note: this might be version dependent, so be careful! In case of doubts use FeynArts 3.9 and FormCal 8.4*)
  tops = TopologyList[Sequence@@Delete[
           Level[tops,1],
           {{104},{119},{213},{214},
           {106},{220},{122},{219},
           {165},{177},{120},{169},
           {181},{125},{138},{153},
           {123},{142},{157},{127}}]];
  insNR = InsertFields[tops, process];
]

If[isSquarkRes1,
  (*remove diagrams with squark double resonances*)
  insNR = DiagramSelect[insNR,(Not[SChannelExtQ[S[_],3,5][##] && SChannelExtQ[S[_],4,6][##]] &&
                               Not[SChannelExtQ[S[_],3,6][##] && SChannelExtQ[S[_],4,5][##]])&];
]
DoPaint[insNR, "realNR"];

(*insert the particle widths*)
widths={MZ2->MZ2-I WZ MZ, MW2->MW2-I WW MW, MSf2[sfe_,n1_,n2_]:>MSf2[sfe,n1,n2]-I WSf[sfe,n1,n2] MSf[sfe,n1,n2], MGl2->MGl2-I MGl WGl};

realNR = CalcFeynAmp[CreateFeynAmp[insNR](*/.{EL->EL PowerOf[EL], GS->GS PowerOf[GS]}*)(*, InvSimplify -> False*)];
(*realNR = realNR/.{PowerOf[a_]^x_:>PowerOf[a][x]};
realNR = realNR/.{PowerOf[a_]:>PowerOf[a][1]};*)
realNR = realNR/.{Den[x_,y_]:>Den[x,y/.widths]}


If[And[isSquarkRes1, Not[Component[real3546Sq1ExtSum,1]===0]],
  (*Write files for on-shell resonant reals, OS3546*)
  amps = {real3546Sq1ExtSum};
  {realOS} = Abbreviate[amps, 6, Preprocess -> OnSize[100, Simplify, 500, DenCollect]];

  col = ColourME[All, realOS];

  abbr = OptimizeAbbr[Abbr[]];
  subexpr = OptimizeAbbr[Subexpr[]];

  (*fortran can't handle arrays with dimensionality greater than 7*)
  (*apply back the subexpressions with number of arguments greater than 6*)
  subexpr6 = Table[If[(CountArgs[SubstitutionHead[subexpr[[i]]]]/.{}->Sequence[])[[1]]>6,subexpr[[i]]],
               {i,1,Length[subexpr]}]/.Null->Sequence[];
  realOS = realOS//.subexpr6;
  abbr = abbr//.subexpr6;

  (*delete the subexpressions with number of arguments greater than 6 from subexpr list*)
  subexpr = Table[If[(CountArgs[SubstitutionHead[subexpr[[i]]]]/.{}->Sequence[])[[1]]<=6,subexpr[[i]]],
              {i,1,Length[subexpr]}]/.Null->Sequence[];
  subexpr = subexpr//.subexpr6;

  dir = SetupCodeDir[name<>"_realOS_3546_Sq1Sq2", Drivers -> name <> "_drivers"];
  WriteSquaredME[realOS, {}, col, abbr, subexpr, dir];
]


If[And[isSquarkRes1, Not[Component[real3645Sq1ExtSum,1]===0]],
  (*Write files for on-shell resonant reals, OS3645*)
  amps = {real3645Sq1ExtSum};
  {realOS} = Abbreviate[amps, 6, Preprocess -> OnSize[100, Simplify, 500, DenCollect]];

  col = ColourME[All, realOS];

  abbr = OptimizeAbbr[Abbr[]];
  subexpr = OptimizeAbbr[Subexpr[]];

  (*fortran can't handle arrays with dimensionality greater than 7*)
  (*apply back the subexpressions with number of arguments greater than 6*)
  subexpr6 = Table[If[(CountArgs[SubstitutionHead[subexpr[[i]]]]/.{}->Sequence[])[[1]]>6,subexpr[[i]]],
               {i,1,Length[subexpr]}]/.Null->Sequence[];
  realOS = realOS//.subexpr6;
  abbr = abbr//.subexpr6;

  (*delete the subexpressions with number of arguments greater than 6 from subexpr list*)
  subexpr = Table[If[(CountArgs[SubstitutionHead[subexpr[[i]]]]/.{}->Sequence[])[[1]]<=6,subexpr[[i]]],
              {i,1,Length[subexpr]}]/.Null->Sequence[];
  subexpr = subexpr//.subexpr6;

  dir = SetupCodeDir[name<>"_realOS_3645_Sq1Sq2", Drivers -> name <> "_drivers"];
  WriteSquaredME[realOS, {}, col, abbr, subexpr, dir];
]


If[And[isGluinoRes, Not[Component[real356GlExtSum,1]===0]],
  (*Write files for on-shell resonant reals, OS356*)
  amps = {real356GlExtSum};
  {realOS} = Abbreviate[amps, 6, Preprocess -> OnSize[100, Simplify, 500, DenCollect]];

  col = ColourME[All, realOS];

  abbr = OptimizeAbbr[Abbr[]];
  subexpr = OptimizeAbbr[Subexpr[]];

  (*fortran can't handle arrays with dimensionality greater than 7*)
  (*apply back the subexpressions with number of arguments greater than 6*)
  subexpr6 = Table[If[(CountArgs[SubstitutionHead[subexpr[[i]]]]/.{}->Sequence[])[[1]]>6,subexpr[[i]]],
               {i,1,Length[subexpr]}]/.Null->Sequence[];
  realOS = realOS//.subexpr6;
  abbr = abbr//.subexpr6;

  (*delete the subexpressions with number of arguments greater than 6 from subexpr list*)
  subexpr = Table[If[(CountArgs[SubstitutionHead[subexpr[[i]]]]/.{}->Sequence[])[[1]]<=6,subexpr[[i]]],
              {i,1,Length[subexpr]}]/.Null->Sequence[];
  subexpr = subexpr//.subexpr6;

  dir = SetupCodeDir[name<>"_realOS_356_Sq1", Drivers -> name <> "_drivers"];
  WriteSquaredME[realOS, {}, col, abbr, subexpr, dir];
]


If[And[isGluinoRes, Not[Component[real365GlExtSum,1]===0]],
  (*Write files for on-shell resonant reals, OS365*)
  amps = {real365GlExtSum};
  {realOS} = Abbreviate[amps, 6, Preprocess -> OnSize[100, Simplify, 500, DenCollect]];

  col = ColourME[All, realOS];

  abbr = OptimizeAbbr[Abbr[]];
  subexpr = OptimizeAbbr[Subexpr[]];

  (*fortran can't handle arrays with dimensionality greater than 7*)
  (*apply back the subexpressions with number of arguments greater than 6*)
  subexpr6 = Table[If[(CountArgs[SubstitutionHead[subexpr[[i]]]]/.{}->Sequence[])[[1]]>6,subexpr[[i]]],
               {i,1,Length[subexpr]}]/.Null->Sequence[];
  realOS = realOS//.subexpr6;
  abbr = abbr//.subexpr6;

  (*delete the subexpressions with number of arguments greater than 6 from subexpr list*)
  subexpr = Table[If[(CountArgs[SubstitutionHead[subexpr[[i]]]]/.{}->Sequence[])[[1]]<=6,subexpr[[i]]],
              {i,1,Length[subexpr]}]/.Null->Sequence[];
  subexpr = subexpr//.subexpr6;

  dir = SetupCodeDir[name<>"_realOS_365_Sq1", Drivers -> name <> "_drivers"];
  WriteSquaredME[realOS, {}, col, abbr, subexpr, dir];
]


If[And[isGluinoRes, Not[Component[real456GlExtSum,1]===0]],
  (*Write files for on-shell resonant reals, OS456*)
  amps = {real456GlExtSum};
  {realOS} = Abbreviate[amps, 6, Preprocess -> OnSize[100, Simplify, 500, DenCollect]];

  col = ColourME[All, realOS];

  abbr = OptimizeAbbr[Abbr[]];
  subexpr = OptimizeAbbr[Subexpr[]];

  (*fortran can't handle arrays with dimensionality greater than 7*)
  (*apply back the subexpressions with number of arguments greater than 6*)
  subexpr6 = Table[If[(CountArgs[SubstitutionHead[subexpr[[i]]]]/.{}->Sequence[])[[1]]>6,subexpr[[i]]],
               {i,1,Length[subexpr]}]/.Null->Sequence[];
  realOS = realOS//.subexpr6;
  abbr = abbr//.subexpr6;

  (*delete the subexpressions with number of arguments greater than 6 from subexpr list*)
  subexpr = Table[If[(CountArgs[SubstitutionHead[subexpr[[i]]]]/.{}->Sequence[])[[1]]<=6,subexpr[[i]]],
              {i,1,Length[subexpr]}]/.Null->Sequence[];
  subexpr = subexpr//.subexpr6;

  dir = SetupCodeDir[name<>"_realOS_456_Sq1", Drivers -> name <> "_drivers"];
  WriteSquaredME[realOS, {}, col, abbr, subexpr, dir];
]


If[And[isGluinoRes, Not[Component[real465GlExtSum,1]===0]],
  (*Write files for on-shell resonant reals, OS465*)
  amps = {real465GlExtSum};
  {realOS} = Abbreviate[amps, 6, Preprocess -> OnSize[100, Simplify, 500, DenCollect]];

  col = ColourME[All, realOS];

  abbr = OptimizeAbbr[Abbr[]];
  subexpr = OptimizeAbbr[Subexpr[]];

  (*fortran can't handle arrays with dimensionality greater than 7*)
  (*apply back the subexpressions with number of arguments greater than 6*)
  subexpr6 = Table[If[(CountArgs[SubstitutionHead[subexpr[[i]]]]/.{}->Sequence[])[[1]]>6,subexpr[[i]]],
               {i,1,Length[subexpr]}]/.Null->Sequence[];
  realOS = realOS//.subexpr6;
  abbr = abbr//.subexpr6;

  (*delete the subexpressions with number of arguments greater than 6 from subexpr list*)
  subexpr = Table[If[(CountArgs[SubstitutionHead[subexpr[[i]]]]/.{}->Sequence[])[[1]]<=6,subexpr[[i]]],
              {i,1,Length[subexpr]}]/.Null->Sequence[];
  subexpr = subexpr//.subexpr6;

  dir = SetupCodeDir[name<>"_realOS_465_Sq1", Drivers -> name <> "_drivers"];
  WriteSquaredME[realOS, {}, col, abbr, subexpr, dir];
]


If[And[isSquarkRes2, Not[Component[real356Sq2ExtSum,1]===0]],
  (*Write files for on-shell resonant reals, OS356*)
  amps = {real356Sq2ExtSum};
  {realOS} = Abbreviate[amps, 6, Preprocess -> OnSize[100, Simplify, 500, DenCollect]];

  col = ColourME[All, realOS];

  abbr = OptimizeAbbr[Abbr[]];
  subexpr = OptimizeAbbr[Subexpr[]];

  (*fortran can't handle arrays with dimensionality greater than 7*)
  (*apply back the subexpressions with number of arguments greater than 6*)
  subexpr6 = Table[If[(CountArgs[SubstitutionHead[subexpr[[i]]]]/.{}->Sequence[])[[1]]>6,subexpr[[i]]],
               {i,1,Length[subexpr]}]/.Null->Sequence[];
  realOS = realOS//.subexpr6;
  abbr = abbr//.subexpr6;

  (*delete the subexpressions with number of arguments greater than 6 from subexpr list*)
  subexpr = Table[If[(CountArgs[SubstitutionHead[subexpr[[i]]]]/.{}->Sequence[])[[1]]<=6,subexpr[[i]]],
              {i,1,Length[subexpr]}]/.Null->Sequence[];
  subexpr = subexpr//.subexpr6;

  dir = SetupCodeDir[name<>"_realOS_356_Sq1Sq2", Drivers -> name <> "_drivers"];
  WriteSquaredME[realOS, {}, col, abbr, subexpr, dir];
]


If[And[isSquarkRes2, Not[Component[real365Sq2ExtSum,1]===0]],
  (*Write files for on-shell resonant reals, OS365*)
  amps = {real365Sq2ExtSum};
  {realOS} = Abbreviate[amps, 6, Preprocess -> OnSize[100, Simplify, 500, DenCollect]];

  col = ColourME[All, realOS];

  abbr = OptimizeAbbr[Abbr[]];
  subexpr = OptimizeAbbr[Subexpr[]];

  (*fortran can't handle arrays with dimensionality greater than 7*)
  (*apply back the subexpressions with number of arguments greater than 6*)
  subexpr6 = Table[If[(CountArgs[SubstitutionHead[subexpr[[i]]]]/.{}->Sequence[])[[1]]>6,subexpr[[i]]],
               {i,1,Length[subexpr]}]/.Null->Sequence[];
  realOS = realOS//.subexpr6;
  abbr = abbr//.subexpr6;

  (*delete the subexpressions with number of arguments greater than 6 from subexpr list*)
  subexpr = Table[If[(CountArgs[SubstitutionHead[subexpr[[i]]]]/.{}->Sequence[])[[1]]<=6,subexpr[[i]]],
              {i,1,Length[subexpr]}]/.Null->Sequence[];
  subexpr = subexpr//.subexpr6;

  dir = SetupCodeDir[name<>"_realOS_365_Sq1Sq2", Drivers -> name <> "_drivers"];
  WriteSquaredME[realOS, {}, col, abbr, subexpr, dir];
]


If[And[isSquarkRes2, Not[Component[real456Sq2ExtSum,1]===0]],
  (*Write files for on-shell resonant reals, OS456*)
  amps = {real456Sq2ExtSum};
  {realOS} = Abbreviate[amps, 6, Preprocess -> OnSize[100, Simplify, 500, DenCollect]];

  col = ColourME[All, realOS];

  abbr = OptimizeAbbr[Abbr[]];
  subexpr = OptimizeAbbr[Subexpr[]];

  (*fortran can't handle arrays with dimensionality greater than 7*)
  (*apply back the subexpressions with number of arguments greater than 6*)
  subexpr6 = Table[If[(CountArgs[SubstitutionHead[subexpr[[i]]]]/.{}->Sequence[])[[1]]>6,subexpr[[i]]],
               {i,1,Length[subexpr]}]/.Null->Sequence[];
  realOS = realOS//.subexpr6;
  abbr = abbr//.subexpr6;

  (*delete the subexpressions with number of arguments greater than 6 from subexpr list*)
  subexpr = Table[If[(CountArgs[SubstitutionHead[subexpr[[i]]]]/.{}->Sequence[])[[1]]<=6,subexpr[[i]]],
              {i,1,Length[subexpr]}]/.Null->Sequence[];
  subexpr = subexpr//.subexpr6;

  dir = SetupCodeDir[name<>"_realOS_456_Sq1Sq2", Drivers -> name <> "_drivers"];
  WriteSquaredME[realOS, {}, col, abbr, subexpr, dir];
]


If[And[isSquarkRes2, Not[Component[real465Sq2ExtSum,1]===0]],
  (*Write files for on-shell resonant reals, OS465*)
  amps = {real465Sq2ExtSum};
  {realOS} = Abbreviate[amps, 6, Preprocess -> OnSize[100, Simplify, 500, DenCollect]];

  col = ColourME[All, realOS];

  abbr = OptimizeAbbr[Abbr[]];
  subexpr = OptimizeAbbr[Subexpr[]];

  (*fortran can't handle arrays with dimensionality greater than 7*)
  (*apply back the subexpressions with number of arguments greater than 6*)
  subexpr6 = Table[If[(CountArgs[SubstitutionHead[subexpr[[i]]]]/.{}->Sequence[])[[1]]>6,subexpr[[i]]],
               {i,1,Length[subexpr]}]/.Null->Sequence[];
  realOS = realOS//.subexpr6;
  abbr = abbr//.subexpr6;

  (*delete the subexpressions with number of arguments greater than 6 from subexpr list*)
  subexpr = Table[If[(CountArgs[SubstitutionHead[subexpr[[i]]]]/.{}->Sequence[])[[1]]<=6,subexpr[[i]]],
              {i,1,Length[subexpr]}]/.Null->Sequence[];
  subexpr = subexpr//.subexpr6;

  dir = SetupCodeDir[name<>"_realOS_465_Sq1Sq2", Drivers -> name <> "_drivers"];
  WriteSquaredME[realOS, {}, col, abbr, subexpr, dir];
]


(*Combine the amplitudes again, but this time the resonant diagrams are regulated*)
(*Use here the regulated on shell amplitudes with summation over the sfermion indices*)
real = realNR;
If[isSquarkRes1,
  real = Combine[real,real3546Sq1,real3645Sq1];
]
If[isGluinoRes,
  real = Combine[real,real356Gl,real365Gl,real456Gl,real465Gl];
]
If[isSquarkRes2,
  real = Combine[real,real356Sq2,real365Sq2,real456Sq2,real465Sq2];
]

(*Write real files with inserted regulator for on-shell diagrams*)
amps = {real};
{real} = Abbreviate[amps, 6, Preprocess -> OnSize[100, Simplify, 500, DenCollect]];

col = ColourME[All, real];

abbr = OptimizeAbbr[Abbr[]];
subexpr = OptimizeAbbr[Subexpr[]];

(*fortran can't handle arrays with dimensionality greater than 7*)
(*apply back the subexpressions with number of arguments greater than 6*)
subexpr6 = Table[If[(CountArgs[SubstitutionHead[subexpr[[i]]]]/.{}->Sequence[])[[1]]>6,subexpr[[i]]],
       {i,1,Length[subexpr]}]/.Null->Sequence[];
real = real//.subexpr6;
abbr = abbr//.subexpr6;

(*delete the subexpressions with number of arguments greater than 6 from subexpr list*)
subexpr = Table[If[(CountArgs[SubstitutionHead[subexpr[[i]]]]/.{}->Sequence[])[[1]]<=6,subexpr[[i]]],
       {i,1,Length[subexpr]}]/.Null->Sequence[];
subexpr = subexpr//.subexpr6;

dir = SetupCodeDir[name<>"_real", Drivers -> name <> "_drivers"];
WriteSquaredME[real, {}, col, abbr, subexpr, dir];


Print["time used: ", SessionTime[] - time1]
Exit[];
