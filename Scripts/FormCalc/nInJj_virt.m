(* ::Package:: *)

(*
generates the Fortran code for
p p -> weakino weakino jet in the MSSM
last modified December 2016
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


(*You can now load the script with the command $ MathKernel -script nInJj_virt.m "qd" "qdbar" "nI" "nJ" "g"*)
Print[$CommandLine]
If[$CommandLine[[2]] === "-script",
	(p[1] = ToString[$CommandLine[[4]]];
	 p[2] = ToString[$CommandLine[[5]]];
	 p[3] = ToString[$CommandLine[[6]]];
	 p[4] = ToString[$CommandLine[[7]]];
	 p[5] = ToString[$CommandLine[[8]]];),
	(*Else*)
	(p[1] = "qd";
	 p[2] = "qdbar";
	 p[3] = "nI";
	 p[4] = "nJ";
	 p[5] = "g";)
]

CalcProcess = p[1]<>p[2]<>"_"<>p[3]<>p[4]<>p[5];
name = CalcProcess;
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
Neglect[ME] = Neglect[ME2] = 0;
Neglect[MU] = Neglect[MU2] = 0;
Neglect[MC] = Neglect[MC2] = 0;
Neglect[MD] = Neglect[MD2] = 0;
Neglect[MS] = Neglect[MS2] = 0;

(*Diagonale CKM Matrix*)
CKM = IndexDelta;
CKMC = IndexDelta;


(*Options*)
SetOptions[InsertFields, Model -> "MSSMCTPOWHEG", InsertionLevel->{Classes},
           (*No Fermion-Higgs coupling*)
           Restrictions -> {NoLightFHCoupling},
           (*Exclude Top, Higgs, Neutrinos, massive Leptons, Sneutrinos, Sleptons*)
		   (*ExcludeParticles -> {S[1|2|3|4|5|6|11|12], F[1|2]},*)
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

widths={MZ2->MZ2-I WZ MZ, MW2->MW2-I WW MW, MSf2[sfe_,n1_,n2_]:>MSf2[sfe,n1,n2]-I WSf[sfe,n1,n2] MSf[sfe,n1,n2], MGl2->MGl2-I MGl WGl};
(*insertion of widths via complex masses. Theses masses are set in the external fortran code*)
(*widths={MZ2->MZ2c, MW2->MW2c, MSf2[sfe_,n1_,n2_]:>MSf2c[sfe,n1,n2], MGl2->MGl2c};*)


Print["Born"]

tops = CreateTopologies[0, 2 -> 3];
ins = InsertFields[tops, process];

DoPaint[ins, "born"];

born = CalcFeynAmp[CreateFeynAmp[ins]];

born = born/.{Den[x_,y_]:>Den[x,y/.widths]};
born = born//.{Alfa2->0};


Print["Counter Terms"]

top = CreateCTTopologies[1, 2 -> 3, ExcludeTopologies -> {TadpoleCTs, WFCorrectionCTs}]; (*Exclude Tadpole- and Self-Energy CT on external legs*)
ins = InsertFields[top, process];

(*All Counter Terms*)
DoPaint[ins, "counterAll"];
counterAll = CreateFeynAmp[ins];

(*ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][_,_,V[1|3]]]) &]; (*Diagram does not contain W/gam*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][_,_,S[1|2|3|4|5|6]]]) &]; (*Diagram does not contain Goldstone/Higgs*)*)

(*Self Energy Counter Terms*)
insSelf = DiagramSelect[ins,(FieldMemberQ[FieldPoints[##],FieldPoint[_][_, _]]) &];
(*insSelf = DiagramDelete[insSelf, 14, 16];(*Delete CT for EW Corrections*)*)
DoPaint[insSelf, "counterSelf"];
counterSelf = CreateFeynAmp[insSelf];

(*Vertex Correction Counter Terms*)
insVert = DiagramSelect[ins,FieldMemberQ[FieldPoints[##],FieldPoint[1][_, _, _]] &];
(*insVert = DiagramDelete[insVert, 7, 8];(*Delete CT for EW Corrections*)*)
DoPaint[insVert, "counterVert"];
counterVert = CreateFeynAmp[insVert];


Print["Self Energies"]

top = CreateTopologies[1, 2 -> 3, SelfEnergiesOnly];
ins = InsertFields[top, process];

ins = DiagramSelect[ins,(FieldMemberQ[LoopFields[##],F[15,{_}]] || FieldMemberQ[LoopFields[##], S[13,{_,_,_}]] || 
		FieldMemberQ[LoopFields[##], S[14,{_,_,_}]] || FieldMemberQ[LoopFields[##], V[5,{_}]]) &]; (*Diagram contains g, sg, sq*)

ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][V[1],_,_]]) &]; (*Diagram does not contain gam*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][_,V[1],_]]) &]; (*Diagram does not contain gam*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][_,_,V[1]]]) &]; (*Diagram does not contain gam*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][V[3],_,_]]) &]; (*Diagram does not contain W*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][_,V[3],_]]) &]; (*Diagram does not contain W*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][_,_,V[3]]]) &]; (*Diagram does not contain W*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][S[n_/;n<=6],_,_]]) &]; (*Diagram does not contain Higgs/Goldstone*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][_,S[n_/;n<=6],_]]) &]; (*Diagram does not contain Higgs/Goldstone*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][_,_,S[n_/;n<=6]]]) &]; (*Diagram does not contain Higgs/Goldstone*)
(*ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][_,_,V[2]]]) &]; (*Diagram does not contain Z*)*)

ins = DiagramSelect[ins,FreeQ[LoopFields[##], U[_]] &]; (*Loop does not contain a ghost*)
(*ins = DiagramSelect[ins,FreeQ[LoopFields[##], V[1|2|3]] &]; (*Loop does not contain W/Z/gam*)*)
(*ins = DiagramSelect[ins,FreeQ[LoopFields[##], S[1|2|3|4|5|6]] &]; (*Loop does not contain a Higgs/Goldstone*)*)

ins = DiagramSelect[ins,FreeQ[FieldPoints[##],FieldPoint[_][V[5,_],V[5,_],V[5,_]]] &]; (*Diagram does not contain 3-gluon coupling*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][-S[13|14,_],S[13|14,_],V[2]]]) &]; (*Diagram does not contain Z-squark-squark coupling*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][S[13|14,_],-S[13|14,_],V[2]]]) &]; (*Diagram does not contain Z-squark-squark coupling*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][-S[13|14,_],S[13|14,_],V[2],V[2]]]) &]; (*Diagram does not contain Z-Z-squark-squark coupling*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][S[13|14,_],-S[13|14,_],V[2],V[2]]]) &]; (*Diagram does not contain Z-Z-squark-squark coupling*)
ins = DiagramSelect[ins,FreeQ[FieldPoints[##],FieldPoint[_][-S[13|14,_],S[13|14,_],V[2],V[5,_]]] &]; (*Diagram does not contain 3-gluon coupling*)

DoPaint[ins, "self"];

self = CalcFeynAmp[CreateFeynAmp[ins], counterSelf];
self = self/.{Den[x_,y_]:>Den[x,y/.widths]};
self = self//.{Alfa2->0};


Print["Vertices"]

top = CreateTopologies[1, 2 -> 3, TrianglesOnly];
ins = InsertFields[top, process];

(*All Vertices*)
(*DoPaint[ins, "vertAll"];*)

(*ins = DiagramSelect[ins,(MemberQ[LoopFields[##], V[5,{_}]] || MemberQ[LoopFields[##], F[15,{_}]]) &];(*Loop contains a g or sg*)*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][V[1],_,_]]) &]; (*Diagram does not contain gam*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][_,V[1],_]]) &]; (*Diagram does not contain gam*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][_,_,V[1]]]) &]; (*Diagram does not contain gam*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][V[3],_,_]]) &]; (*Diagram does not contain W*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][_,V[3],_]]) &]; (*Diagram does not contain W*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][_,_,V[3]]]) &]; (*Diagram does not contain W*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][-V[3],_,_]]) &]; (*Diagram does not contain W*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][_,-V[3],_]]) &]; (*Diagram does not contain W*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][_,_,-V[3]]]) &]; (*Diagram does not contain W*)

ins = DiagramSelect[ins,FreeQ[LoopFields[##], V[n_/;n<=3]] &]; (*Loop does not contain W/Z/gam*)
ins = DiagramSelect[ins,FreeQ[LoopFields[##], S[n_/;n<=6]] &]; (*Loop does not contain a Higgs/Goldstone*)

ins = DiagramSelect[ins,FreeQ[FieldPoints[##],FieldPoint[_][-S[13|14,_],S[13|14,_],V[2],V[5,_]]] &]; (*Diagram does not contain Z-gluon-squark-squark coupling*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][-S[13|14,_],S[13|14,_],V[2],V[2]]]) &]; (*Diagram does not contain Z-Z-squark-squark coupling*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][S[13|14,_],-S[13|14,_],V[2],V[2]]]) &]; (*Diagram does not contain Z-Z-squark-squark coupling*)

insrg = DiagramSelect[ins, If[Length[LoopFields[##]]==3, ((MemberQ[#, Field[6] -> V[5,_]] ||\[NonBreakingSpace]MemberQ[#, Field[6] -> F[15,_]]) &&
		MemberQ[FieldPoints[##],FieldPoint[_][V[5, _], V[5, _], V[5, _]]]) || MemberQ[#, Field[7] -> V[5,_]],False] &];(*Diagram with a zero color trace*)
insrew = DiagramSelect[ins, If[Length[LoopFields[##]]==3, FreeQ[LoopFields[##], V[5, _]] && FreeQ[LoopFields[##], F[15, _]] &&
	FreeQ[#, Field[6] -> V[5, _]] && (MemberQ[#, Field[6] -> V[2]] || MemberQ[#, Field[7] -> V[2]]),False] &];(*Diagram with a Z-q-q coupling and only EW correction*)

ins = DiagramComplement[ins,insrg,insrew];

(*ins = DiagramExtract[ins,133..146];
(*ins = Reap[DiagramSelect[ins,FreeQ[Sow[FieldPoints[##]],FieldPoint[_][V[2],V[5,_],S[13,_],-S[13,_]]] &]]; (*Diagram does not contain 3-gluon coupling*)
Export["./test.wdx",ins[[2]],"WDX"];*)*)
DoPaint[ins, "vert"];

vert = CalcFeynAmp[CreateFeynAmp[ins], counterVert];
vert = vert/.{Den[x_,y_]:>Den[x,y/.widths]};
vert = vert//.{Alfa2->0};


Print["Boxes"]

top = CreateTopologies[1, 2 -> 3, BoxesOnly];
ins = InsertFields[top, process];

ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][V[1],_,_]]) &]; (*Diagram does not contain gam*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][_,V[1],_]]) &]; (*Diagram does not contain gam*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][_,_,V[1]]]) &]; (*Diagram does not contain gam*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][V[3],_,_]]) &]; (*Diagram does not contain W*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][_,V[3],_]]) &]; (*Diagram does not contain W*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][_,_,V[3]]]) &]; (*Diagram does not contain W*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][-V[3],_,_]]) &]; (*Diagram does not contain W*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][_,-V[3],_]]) &]; (*Diagram does not contain W*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][_,_,-V[3]]]) &]; (*Diagram does not contain W*)

ins = DiagramSelect[ins,FreeQ[LoopFields[##], V[1|2|3]] &]; (*Loop does not contain W/Z/gam*)

(*ins = Reap[DiagramSelect[ins, Sow[(FreeQ[FieldPoints[##],FieldPoint[_][F[3|4, _], -F[3|4, _], V[2]]] && FreeQ[FieldPoints[##],FieldPoint[_][-F[3|4, _], F[3|4, _], V[2]]])
		|| FreeQ[LoopFields[##], V[5, _]]] &]];
Export["./test.wdx",ins[[2]],"WDX"];*)
ins = DiagramSelect[ins, If[(MemberQ[FieldPoints[##],FieldPoint[_][F[3|4, _], -F[3|4, _], V[2]]] || MemberQ[FieldPoints[##],FieldPoint[_][-F[3|4, _], F[3|4, _], V[2]]]),
  MemberQ[LoopFields[##], V[5, _]], True] &];(*remove EW diagrams with Z-q-q  couplings*)

DoPaint[ins, "box"];

box = CalcFeynAmp[CreateFeynAmp[ins]];
box = box/.{Den[x_,y_]:>Den[x,y/.widths]};
box = box//.{Alfa2->0};

(*
insbox1 = DiagramExtract[ins,1..20];
insbox2 = DiagramExtract[ins,21..40];
insbox3 = DiagramExtract[ins,41..60];
insbox4 = DiagramComplement[ins,insbox1,insbox2,insbox3];

box1 = CalcFeynAmp[CreateFeynAmp[insbox1]];
box1 = box1/.{Den[x_,y_]:>Den[x,y/.widths]};
box1 = box1//.{Alfa2->0}

box2 = CalcFeynAmp[CreateFeynAmp[insbox2]];
box2 = box2/.{Den[x_,y_]:>Den[x,y/.widths]};
box2 = box2//.{Alfa2->0}

box3 = CalcFeynAmp[CreateFeynAmp[insbox3]];
box3 = box3/.{Den[x_,y_]:>Den[x,y/.widths]};
box3 = box3//.{Alfa2->0}

box4 = CalcFeynAmp[CreateFeynAmp[insbox4]];
box4 = box4/.{Den[x_,y_]:>Den[x,y/.widths]};
box4 = box4//.{Alfa2->0}
*)


Print["Pentagons"]

top = CreateTopologies[1, 2 -> 3, PentagonsOnly];
ins = InsertFields[top, process];

ins = DiagramSelect[ins,(MemberQ[LoopFields[##], V[5,{_}]] || MemberQ[LoopFields[##], F[15,{_}]]) &];(*Loop contains a g or gluino*)

DoPaint[ins, "pent"];

pent = CalcFeynAmp[CreateFeynAmp[ins]];
pent = pent/.{Den[x_,y_]:>Den[x,y/.widths]};
pent = pent//.{Alfa2->0};


(* Write files *)
amps = {born,self,vert,box,pent};
{born,self,vert,box,pent} = Abbreviate[amps, 6, Preprocess -> OnSize[100, Simplify, 500, DenCollect]];

col = ColourME[All,born];

abbr = OptimizeAbbr[Abbr[]]
subexpr = OptimizeAbbr[Subexpr[]]

(*fortran can't handle arrays with dimensionality greater than 7*)
(*apply back the subexpressions with number of arguments greater than 6*)
subexpr6 = Table[If[(CountArgs[SubstitutionHead[subexpr[[i]]]]/.{}->Sequence[])[[1]]>6,subexpr[[i]]],
       {i,1,Length[subexpr]}]/.Null->Sequence[];
born = born//.subexpr6;
self = self//.subexpr6;
vert = vert//.subexpr6;
box  = box//.subexpr6;
pent = pent//.subexpr6;
abbr = abbr//.subexpr6;

(*delete the subexpressions with number of arguments greater than 6 from subexpr list*)
subexpr = Table[If[(CountArgs[SubstitutionHead[subexpr[[i]]]]/.{}->Sequence[])[[1]]<=6,subexpr[[i]]],
       {i,1,Length[subexpr]}]/.Null->Sequence[];
subexpr = subexpr//.subexpr6;

dir = SetupCodeDir[name <> "_virt", Drivers -> name <> "_drivers"];
WriteSquaredME[born, {self,vert,box,pent}, col, abbr, subexpr, dir];


(*Calculate RenConsts (do this only if needed)*)
(*
InsertFieldsHook[tops_,f1_->f2_]:=InsertFields[tops,f1->f2,ExcludeFieldPoints -> {
	FieldPoint[_][S[n_/;n<=6], _, _],(*Exclude Higgs*)
	FieldPoint[_][S[n_/;n<=6], _, _, _],
	FieldPoint[_][V[n_/;n<5], _, _],(*Exclude Photon, Z, W*)
	FieldPoint[_][V[n_/;n<5], _, _, _],
	FieldPoint[_][F[11|12], _, _],(*Exclude Neutralino+Chargino*)
	FieldPoint[_][F[11|12], _, _, _]}];

WriteRenConst[amps,dir];
*)


Print["time used: ", SessionTime[] - time1]
Exit[];
