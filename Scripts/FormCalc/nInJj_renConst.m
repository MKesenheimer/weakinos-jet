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
	(p[1] = "qdbar";
	 p[2] = "qd";
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
(*Neglect[MB] = Neglect[MB2] = 0;
Neglect[MT] = Neglect[MT2] = 0;*)

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

(*widths={MZ2->MZ2-I WZ MZ, MW2->MW2-I WW MW, MSf2[sfe_,n1_,n2_]:>MSf2[sfe,n1,n2]-I WSf[sfe,n1,n2] MSf[sfe,n1,n2], MGl2->MGl2-I MGl WGl};*)
(*insertion of widths via complex masses. Theses masses are set in the external fortran code*)
widths={MZ2->MZ2W, MW2->MW2W, MSf2[sfe_,n1_,n2_]:>MSf2W[sfe,n1,n2], MGl2->MGl2W};
(*special case of ren consts: the kinematical variables only consist of masses, particle width should not added to these masses*)
cmplx={0->dcmplx[0], MZ->MZC, MW->MWC, MZ2->MZ2C, MW2->MW2C, MU->MUC, MC->MCC, MT->MTC, MD->MDC, MS->MSC, MB->MBC, MU2->MU2C, MC2->MC2C, MT2->MT2C, MD2->MD2C, MS2->MS2C, MB2->MB2C, 
       Mf2[i_,j_]:>Mf2C[i,j], MNeu[i_]:>MNeuC[i], MNeu2[i_]:>MNeu2C[i], MCha[i_]:>MChaC[i], Cha2[i_]:>MCha2C[i], MSf[i_,j_,k_]:>MSfC[i,j,k], MSf2[i_,j_,k_]:>MSf2C[i,j,k],
       MGl->MGlC, MGl2->MGl2C, Mh0->Mh0C, MHH->MHHC, MA0->MA0C, MHp->MHpC, Mh02->Mh02C, MHH2->MHH2C, MA02->MA02C, MHp2->MHp2C};


Print["Counter Terms"]

top = CreateCTTopologies[1, 2 -> 3, ExcludeTopologies -> {TadpoleCTs, WFCorrectionCTs}]; (*Exclude Tadpole- and Self-Energy CT on external legs*)
ins = InsertFields[top, process];

(*exclude fermion higgs couplings*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][_. F[3|4,_], _. F[3|4,_], S[n_/;n<=6]]])& ];

(*Self Energy Counter Terms*)
insSelf = DiagramSelect[ins, (FieldMemberQ[FieldPoints[##],FieldPoint[_][_, _]]) &];
DoPaint[insSelf, "counterSelf"];
counterSelf = CreateFeynAmp[insSelf];

(*Vertex Correction Counter Terms*)
insVert = DiagramSelect[ins, (FieldMemberQ[FieldPoints[##],FieldPoint[1][_, _, _]]) &];
DoPaint[insVert, "counterVert"];
counterVert = CreateFeynAmp[insVert];


Print["Self Energies"]

top = CreateTopologies[1, 2 -> 3, SelfEnergiesOnly];
ins = InsertFields[top, process];

(*exclude fermion higgs couplings*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][_. F[3|4,_], _. F[3|4,_], S[n_/;n<=6]]])& ];

ins = DiagramSelect[ins,(FieldMemberQ[LoopFields[##], _. F[15,_]] || FieldMemberQ[LoopFields[##], _. S[13|14,_]] || FieldMemberQ[LoopFields[##], V[5,_]]) &]; (*Loop contains g, sguark, q, gluino*)
ins = DiagramSelect[ins,(FreeQ[LoopFields[##], V[1|2|3]] && FreeQ[LoopFields[##], S[n_/;n<=6]]) &]; (*Loop does not contain Z, W, gam, Higgs/Goldstone*)

(*delete diagrams with two ew particles that couple to the loop*)
insDel = DiagramSelect[ins,(FieldMemberQ[LoopFields[##],_. S[13|14,_]])& ]; (*only squark loops*)
insDel = DiagramSelect[insDel,(FieldMemberQ[FieldPoints[##],FieldPoint[_][_. S[13|14,_],_. S[13|14,_],V[1|2|3|5,___]]])& ]; (*vector couples to the loop*)
insDel = DiagramSelect[insDel,(FieldMemberQ[FieldPoints[##],FieldPoint[_][_. F[11|12,_],_. F[11|12,_],V[1|2|3]]] || FieldMemberQ[FieldPoints[##],FieldPoint[_][_. F[11|12,_],_. F[11|12,_],S[n_/;n<=6]]])& ]; (*ew vector couples to the weakino*)
ins = DiagramComplement[ins,insDel];

ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][_. S[13|14,_],_. S[13|14,_],V[1|2|3],V[1|2|3|5,___]]]) &]; (*Diagram does not contain V-V-squark-squark coupling*)

DoPaint[ins, "self"];

self = CalcFeynAmp[CreateFeynAmp[ins], counterSelf];


Print["Vertices"]

top = CreateTopologies[1, 2 -> 3, TrianglesOnly];
ins = InsertFields[top, process];

ins = DiagramSelect[ins,(FieldMemberQ[LoopFields[##], _. F[15,_]] || FieldMemberQ[LoopFields[##], _. S[13|14,_]] || FieldMemberQ[LoopFields[##], V[5,_]] || FieldMemberQ[LoopFields[##], _. F[3|4,_]]) &]; (*Loop contains g, sguark, q, gluino*)
ins = DiagramSelect[ins,(FreeQ[LoopFields[##], V[1|2|3]] && FreeQ[LoopFields[##], S[n_/;n<=6]]) &]; (*Loop does not contain Z, W, gam, Higgs/Goldstone*)

(*delete diagrams with two ew particles that couple to the loop*)
insDel = DiagramSelect[ins,(FieldMatchQ[LoopFields[##],{_. S[13|14,_],_. S[13|14,_],_. S[13|14,_]}] || FieldMatchQ[LoopFields[##],{_. F[3|4,_],_. F[3|4,_],_. F[3|4,_]}])&]; (*only squark or quark loops*)
insDel = DiagramSelect[insDel,(FieldMemberQ[FieldPoints[##],FieldPoint[_][_. S[13|14,_],_. S[13|14,_],V[1|2|3]]] || FieldMemberQ[FieldPoints[##],FieldPoint[_][_. F[3|4,_],_. F[3|4,_],V[1|2|3]]])& ]; (* ew vector couples to the loop*)
insDel = DiagramSelect[insDel,(FieldMemberQ[FieldPoints[##],FieldPoint[_][_. F[11|12,_],_. F[11|12,_],V[1|2|3]]] || FieldMemberQ[FieldPoints[##],FieldPoint[_][_. F[11|12,_],_. F[11|12,_],S[n_/;n<=6]]])& ]; (*ew vector couples to the weakinos*)
(*insDel = DiagramSelect[insDel, NotSChannelQ[V[5,_]]]; (*no s-channel gluons in the diagrams that we want to delete*)*)
insDel = DiagramSelect[insDel, FreeQ[#, Field[n_/;n>5] -> V[5,_]]& ];  (*no internal gluon lines in the diagrams that we want to delete*)
ins = DiagramComplement[ins,insDel];

(*same for ew scalars*)
insDel = DiagramSelect[ins,(FieldMatchQ[LoopFields[##],{_. S[13|14,_],_. S[13|14,_],_. S[13|14,_]}] || FieldMatchQ[LoopFields[##],{_. F[3|4,_],_. F[3|4,_],_. F[3|4,_]}])&]; (*only squark or quark loops*)
insDel = DiagramSelect[insDel,(FieldMemberQ[FieldPoints[##],FieldPoint[_][S[n_/;n<=6],_. S[13|14,_],_. S[13|14,_]]] || FieldMemberQ[FieldPoints[##],FieldPoint[_][_. F[3|4,_],_. F[3|4,_],S[n_/;n<=6]]])& ]; (* ew vector couples to the loop*)
insDel = DiagramSelect[insDel,(FieldMemberQ[FieldPoints[##],FieldPoint[_][_. F[11|12,_],_. F[11|12,_],S[n_/;n<=6]]])& ]; (*ew vector couples to the weakinos*)
(*insDel = DiagramSelect[insDel, NotSChannelQ[V[5,_]]]; (*no s-channel gluons in the diagrams that we want to delete*)*)
insDel = DiagramSelect[insDel, FreeQ[#, Field[n_/;n>5] -> V[5,_]]& ];  (*no internal gluon lines in the diagrams that we want to delete*)
ins = DiagramComplement[ins,insDel];

ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][_. S[13|14,_],_. S[13|14,_],V[1|2|3],V[1|2|3|5,___]]]) &]; (*Diagram does not contain V-V-squark-squark coupling*)
ins = DiagramSelect[ins,(FreeQ[FieldPoints[##],FieldPoint[_][S[1|2|3|4|5|6],S[1|2|3|4|5|6],_. S[13|14,_],_. S[13|14,_]]]) &]; (*Diagram does not contain higgs-higgs-squark-squark coupling*)

(*delete diagrams with ew particle couplings in the loop and ew vector in propagator*)
insDel = DiagramSelect[ins,(FieldMatchQ[LoopFields[##],{_. S[13|14,_],_. F[3|4,_],_. F[3|4,_]}] || FieldMatchQ[LoopFields[##],{_. F[3|4,_],_. S[13|14,_],_. S[13|14,_]}])&]; (*only squark or quark loops*)
insDel = DiagramSelect[insDel,(FieldMemberQ[FieldPoints[##],FieldPoint[_][_,_,V[1|2|3|5,___]]])&]; (*diagram contains ew vector or has zero color trace*)
ins = DiagramComplement[ins,insDel];

(*delete higgs to external fermion coupling (no external tops)*)
insDel = DiagramSelect[ins,(FieldMemberQ[FieldPoints[##],FieldPoint[_][S[n_/;n<=6],_,_]] || FieldMemberQ[FieldPoints[##],FieldPoint[_][_,_,S[n_/;n<=6]]])&];
insDel = DiagramSelect[insDel,(Not[FieldMatchQ[LoopFields[##],{_. F[3|4,_],_. F[3|4,_],_. F[3|4,_]}]])&]; (*no pure quark loops*)
insDel = DiagramSelect[insDel,(Not[FieldMatchQ[LoopFields[##],{_. S[13|14,_],_. S[13|14,_],_. S[13|14,_]}]])&]; (*no pure squark loops*)
insDel = DiagramSelect[insDel,(Not[FieldMatchQ[LoopFields[##],{_. S[13|14,_],_. S[13|14,_]}]])&]; (*no pure squark loops*)
insDel = DiagramSelect[insDel,(Not[FieldMatchQ[LoopFields[##],{_. F[15,_],_. S[13|14,_],_. S[13|14,_]}]])&]; (*no gluino squark squark loop*)
ins = DiagramComplement[ins,insDel];

DoPaint[ins, "vert"];

vert = CalcFeynAmp[CreateFeynAmp[ins], counterVert];


(* Write files *)
amps = {self,vert};
{self,vert} = Abbreviate[amps, 6, Preprocess -> OnSize[100, Simplify, 500, DenCollect]];

dir = SetupCodeDir[name <> "_virt", Drivers -> name <> "_drivers"];

(*Calculate RenConsts*)
InsertFieldsHook[tops_,f1_->f2_]:=InsertFields[tops,f1->f2,ExcludeFieldPoints -> {
    FieldPoint[_][S[n_/;n<=6], _, _],(*Exclude Higgs*)
    FieldPoint[_][S[n_/;n<=6], _, _, _],
    FieldPoint[_][V[n_/;n<5], _, _],(*Exclude Photon, Z, W*)
    FieldPoint[_][V[n_/;n<5], _, _, _],
    FieldPoint[_][F[11|12], _, _],(*Exclude Neutralino+Chargino*)
    FieldPoint[_][F[11|12], _, _, _]}];

ren = CalcRenConst[amps];
ren = ren/.{c_/MD:>c/(MD+MR),c_/MU:>c/(MU+MR),c_/MS:>c/(MS+MR),c_/MC:>c/(MC+MR),c_/MB:>c/(MB+MR)}; (*regularize 1/0 terms*)
ren = ren/.{c_/MD2:>c/(MD2+MR2),c_/MU2:>c/(MU2+MR2),c_/MS2:>c/(MS2+MR2),c_/MC2:>c/(MC2+MR2),c_/MB2:>c/(MB2+MR2)}; (*regularize 1/0 terms*)
ren = ren/.{Den[x_,y_]:>Den[x,y/.widths/.cmplx]};
ren = ren/.{A0[m1_]:>A0[m1/.widths/.cmplx]};
ren = ren/.{A0i[aai_,m1_]:>A0i[aai,m1/.widths/.cmplx]};
ren = ren/.{B0i[bbi_,p1_,m1_,m2_]:>B0i[bbi,p1/.cmplx,m1/.widths/.cmplx,m2/.widths/.cmplx]};
ren = ren/.{C0i[cci_,p1_,p2_,p3_,m1_,m2_,m3_]:>C0i[cci,p1/.cmplx,p2/.cmplx,p3/.cmplx,m1/.widths/.cmplx,m2/.widths/.cmplx,m3/.widths/.cmplx]};
ren = ren/.{D0i[ddi_,p1_,p2_,p3_,p4_,p5_,p6_,m1_,m2_,m3_,m4_]:>D0i[ddi,p1/.cmplx,p2/.cmplx,p3/.cmplx,p4/.cmplx,p5/.cmplx,p6/.cmplx,m1/.widths/.cmplx,m2/.widths/.cmplx,m3/.widths/.cmplx,m4/.widths/.cmplx]};
ren = ren/.{E0i[eei_,p1_,p2_,p3_,p4_,p5_,p6_,p7_,p8_,p9_,p10_,m1_,m2_,m3_,m4_,m5_]:>E0i[eei,p1/.cmplx,p2/.cmplx,p3/.cmplx,p4/.cmplx,p5/.cmplx,p6/.cmplx,p7/.cmplx,p8/.cmplx,p9/.cmplx,p10/.cmplx,m1/.widths/.cmplx,m2/.widths/.cmplx,m3/.widths/.cmplx,m4/.widths/.cmplx,m5/.widths/.cmplx]};

ren = ren/.{RenConst[0]->0};
(*
(*cast real arguments into complex arguments*)
$Assumptions=_\[Element]Reals;
ren = ren/.{A0[m1_]:>A0[Cmplx[m1]]};
ren = ren/.{A0i[aai_,m1_]:>A0i[aai,Cmplx[m1]]};
ren = ren/.{B0i[bbi_,p1_,m1_,m2_]:>B0i[bbi,Cmplx[p1],Cmplx[m1],Cmplx[m2]]};
ren = ren/.{C0i[cci_,p1_,p2_,p3_,m1_,m2_,m3_]:>C0i[cci,Cmplx[p1],Cmplx[p2],Cmplx[p3],Cmplx[m1],Cmplx[m2],Cmplx[m3]]};
ren = ren/.{D0i[ddi_,p1_,p2_,p3_,p4_,p5_,p6_,m1_,m2_,m3_,m4_]:>D0i[ddi,Cmplx[p1],Cmplx[p2],Cmplx[p3],Cmplx[p4],Cmplx[p5],Cmplx[p6],Cmplx[m1],Cmplx[m2],Cmplx[m3],Cmplx[m4]]};
ren = ren/.{E0i[eei_,p1_,p2_,p3_,p4_,p5_,p6_,p7_,p8_,p9_,p10_,m1_,m2_,m3_,m4_,m5_]:>E0i[eei,Cmplx[p1],Cmplx[p2],Cmplx[p3],Cmplx[p4],Cmplx[p5],Cmplx[p6],Cmplx[p7],Cmplx[p8],Cmplx[p9],Cmplx[p10],Cmplx[m1],Cmplx[m2],Cmplx[m3],Cmplx[m4],Cmplx[m5]]};
$Assumptions=True;
*)

Print["renConsts = "];
Print[ren];

WriteRenConst[ren,dir];


Print["time used: ", SessionTime[] - time1]
Exit[];
