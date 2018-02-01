(* ::Package:: *)

(*
generates the Fortran code for
p p -> weakino weakino jet in the MSSM
last modified 2017 by Matthias Kesenheimer
*)


Clear["Global`*"]
SetDirectory[NotebookDirectory[]];
<< FeynArts`
<< FeynArtsAdd`
<< FormCalcCMS`
<< FormCalcAdd`
ClearProcess[]
<<"!rm *.frm"

time1 = SessionTime[]


(*You can now load the script with the command $ MathKernel -script xIxJj.m "qd" "qdbar" "xI+" "xJ-" "g"*)
Print[$CommandLine]
If[$CommandLine[[2]] === "-script",
	(p[1] = ToString[$CommandLine[[4]]];
	 p[2] = ToString[$CommandLine[[5]]];
	 p[3] = ToString[$CommandLine[[6]]];
	 p[4] = ToString[$CommandLine[[7]]];
	 p[5] = ToString[$CommandLine[[8]]];),
	(*Else*)
	(p[1] = "qubar";
	 p[2] = "g";
	 p[3] = "xI+";
	 p[4] = "xJ-";
	 p[5] = "qubar";)
]

CalcProcess = p[1]<>p[2]<>"_"<>p[3]<>p[4]<>p[5];
name = StringReplace[CalcProcess, {"+" -> "", "-" -> ""}];
Print[CalcProcess]

For[i=1, i<=5, i++,
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

process = {P[1], P[2]} -> {P[3], P[4], P[5]};
Print[process]


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

(*particle widths (complex mass scheme)*)
Sq[MGl] = MGl2 - I MGl WGl;
Sq[MSf[a__]] = MSf2[a] - I MSf[a] WSf[a];
(*Sq[MZ] = MZ2 - I MZ WZ;
Sq[MW] = MW2 - I MW WW;*)
widths = {MZ2 -> MZ2 - I MZ WZ, MW2 -> MW2 - I MW WW};

(*real widths*)
Scan[ (RealQ[#] = True)&, {WGl, _WSf, WW, WZ}];

(*Test*)
Re[MSf[Sfe3,3,Gen3]^2]
Conjugate[MSf[Sfe3,3,Gen3]^2]


(*Options*)
SetOptions[InsertFields, Model -> "MSSMCTPOWHEG_dZgg3", InsertionLevel->{Classes},
           (*No Fermion-Higgs coupling*)
           Restrictions -> {NoLightFHCoupling},
           (*Exclude Neutrinos, massive Leptons, Sneutrinos, Sleptons*)
           ExcludeParticles -> {S[11|12], F[1|2]},
           (*no internal Weakinos*)
           LastSelections -> {!F[11],!F[12]}];

SetOptions[Paint, PaintLevel -> {Classes}, ColumnsXRows -> {4, 5}, AutoEdit -> False];

(*Reduce tensor to scalar integrals and choose regularisazation scheme*)
(*D = dimensional regularization (default),*)
(*4 = constrained differential renormalization,*)
(*0 = keeps the whole amplitude D-dimensional*)
SetOptions[CalcFeynAmp,Dimension->D];

(*Save the Diagrams*)
$PaintSE = MkDir["Diagrams_"<>name];
DoPaint[diags_, type_, opt___] := Paint[diags, opt,
  DisplayFunction -> (Export[ToFileName[$PaintSE, name <> "_" <> type <> ".pdf"], #]&)];

(*faster code generation without boxes and pentagons, could be used for debugging*)
$FastCode = False;
(*Generate only diagrams without calculating anything*)
$DiagramsOnly = False;

(*complexify the arguments of the loop functions (required for the use with collier)*)
cmplx = {0->dcmplx[0], MZ->MZC, MW->MWC, MZ2->MZ2C, MW2->MW2C, MU->MUC, MC->MCC, MT->MTC, MD->MDC, MS->MSC, MB->MBC, MU2->MU2C, MC2->MC2C, MT2->MT2C, MD2->MD2C, MS2->MS2C, MB2->MB2C, 
       Mf[i_,j_]:>MfC[i,j], Mf2[i_,j_]:>Mf2C[i,j], MNeu[i_]:>MNeuC[i], MNeu2[i_]:>MNeu2C[i], MCha[i_]:>MChaC[i], Cha2[i_]:>MCha2C[i], MSf[i_,j_,k_]:>MSfC[i,j,k], MSf2[i_,j_,k_]:>MSf2C[i,j,k],
       MGl->MGlC, MGl2->MGl2C, Mh0->Mh0C, MHH->MHHC, MA0->MA0C, MHp->MHpC, Mh02->Mh02C, MHH2->MHH2C, MA02->MA02C, MHp2->MHp2C};


Print["Born"];

tops = CreateTopologies[0, 2 -> 3];
ins = InsertFields[tops, process];
(*exclude fermion higgs couplings. top pdfs = 0 \[Rule] no external tops \[Rule] no internal tops \[Rule] no fermion higgs coupling*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][_. F[3|4,_], _. F[3|4,_], _. S[n_/;n<=6]]])& ];

DoPaint[ins, "born"];

If[Not[$DiagramsOnly],
  born = CalcFeynAmp[CreateFeynAmp[ins]];
  born = born/.{Den[p2_,m2_]:>Den[p2,m2/.widths]};
  born = born//.{Alfa2->0};

  Print["born = "];
  Print[born];
];


(* Write files *)
If[Not[$DiagramsOnly],
  amps = {born};
  {born} = Abbreviate[amps, 6, Preprocess -> OnSize[100, Simplify, 500, DenCollect]];
  
  col = ColourME[All,born];
  
  abbr = OptimizeAbbr[Abbr[]];
  subexpr = OptimizeAbbr[Subexpr[]]//.{Alfa2->0};
  
  (*fortran can\.b4t handle arrays with dimensionality greater than 7*)
  (*apply back the subexpressions with number of arguments greater than 6*)
  subexpr6 = Table[If[(CountArgs[SubstitutionHead[subexpr[[i]]]]/.{}->Sequence[])[[1]]>6,subexpr[[i]]],
         {i,1,Length[subexpr]}]/.Null->Sequence[];
  born = born//.subexpr6;
  abbr = abbr//.subexpr6;
  amps = {born};
  
  (*delete the subexpressions with number of arguments greater than 6 from subexpr list*)
  subexpr = Table[If[(CountArgs[SubstitutionHead[subexpr[[i]]]]/.{}->Sequence[])[[1]]<=6,subexpr[[i]]],
         {i,1,Length[subexpr]}]/.Null->Sequence[];
  subexpr = subexpr//.subexpr6;
  
  dir = SetupCodeDir[name <> "_born", Drivers -> name <> "_drivers"];
  WriteSquaredME[born, {}, col, abbr, subexpr, dir];
];


Print["time used: ", SessionTime[] - time1];
Exit[];
