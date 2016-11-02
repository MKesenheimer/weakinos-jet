(* ::Package:: *)

(*
generates the Fortran code for
p p -> weakino weakino jet jet in the MSSM
last modified July 2016
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
	(p[1] = "g";
	 p[2] = "qu";
	 p[3] = "nI";
	 p[4] = "nJ";
	 p[5] = "g";
	 p[6] = "qu";)
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
SetOptions[CalcFeynAmp, Dimension->D];

(*Save the Diagrams*)
$PaintSE = MkDir["Diagrams"];
DoPaint[diags_, type_, opt___] := Paint[diags, opt,
  DisplayFunction -> (Export[ToFileName[$PaintSE, name <> "_" <> type <> ".pdf"], #]&)]

(*Set Options for Abbreviations*)
(*SetOptions[Abbreviate,MinLeafCount\[Rule]100];*)

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


Print["Reals"]

tops = CreateTopologies[0, 2 -> 4];
ins = InsertFields[tops, process]

(*DEBUG*)
(*ins = DiagramExtract[ins,2,4] (*MadGraph: 17,18,49,50*)*)
(*ins = DiagramExtract[ins,10,14] (*Mad: 31,32,63,64*)*)
(*ins = DiagramSelect[ins,(MemberQ[FieldPoints[##],FieldPoint[_][V[5,{_}],V[5,{_}],V[5,{_}]]]) &]; (*MadGraph: *)*)
(*ins = DiagramSelect[ins, MemberQ[#, Field[_] -> S[_,{_,_,_}] ]&];*)

DoPaint[ins, "real"];
(*Quit[]*)

(*sort the amplitude by powers of the coupling constants*)
(*Uncomment the option ",InvSimplify\[Rule]False" for Mac OS X => *)
(*FormCalc fails to generate the Form code in function InvSimplify*)
real = CalcFeynAmp[CreateFeynAmp[ins](*/.{EL->EL PowerOf[EL], GS->GS PowerOf[GS]}*), InvSimplify->False];
(*Export["real."<>name<>".wdx",real,"WDX"]*)
(*apply max coupling rules*)
(*real = real//.{PowerOf[a_]^x_:>PowerOf[a][x]};
real = real//.{PowerOf[a_]:>PowerOf[a][1]};*)
(*Export["real0."<>name<>".wdx",real,"WDX"]*)

(*insert the particle widths*)
widths = {MZ2->MZ2-I WZ MZ, MW2->MW2-I WW MW, MSf2[sfe_,n1_,n2_]:>MSf2[sfe,n1,n2]-I WSf[sfe,n1,n2] MSf[sfe,n1,n2], MGl2->MGl2-I MGl WGl};
real = real/.{Den[x_,y_]:>Den[x,y/.widths]}


(*Write files of non resonant reals*)
amps = {real};
{real} = Abbreviate[amps, 6, Preprocess -> OnSize[100, Simplify, 500, DenCollect]];

col = ColourME[All, real];

abbr = OptimizeAbbr[Abbr[]]
subexpr = OptimizeAbbr[Subexpr[]]

(*fortran can't handle arrays with dimensionality greater than 7*)
(*apply back the subexpressions with number of arguments greater than 6*)
subexpr6 = Table[If[(countArgs[getReplacementHead[subexpr[[i]]]]/.{}->Sequence[])[[1]]>6,subexpr[[i]]],
       {i,1,Length[subexpr]}]/.Null->Sequence[];
real = real//.subexpr6;
abbr = abbr//.subexpr6;

(*delete the subexpressions with number of arguments greater than 6 from subexpr list*)
subexpr = Table[If[(countArgs[getReplacementHead[subexpr[[i]]]]/.{}->Sequence[])[[1]]<=6,subexpr[[i]]],
       {i,1,Length[subexpr]}]/.Null->Sequence[];
subexpr = subexpr//.subexpr6;

(*
Export["real.wdx",real,"WDX"];
Export["abbr.wdx",abbr,"WDX"];
Export["subexpr.wdx",subexpr,"WDX"];
*)

dir = SetupCodeDir[name<>"_real", Drivers -> name <> "_drivers"];
WriteSquaredME[real, {}, col, abbr, subexpr, dir];


Print["time used: ", SessionTime[] - time1]
Exit[];
