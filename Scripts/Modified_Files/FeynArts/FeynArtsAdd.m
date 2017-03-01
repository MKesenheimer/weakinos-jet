(* ::Package:: *)

(*
This package defines new functions for FeynArts.
FeynArts V3.9 or higher is required.
Please copy FeynArtsAdd.m to your
local FeynArts installation:
$<FEYNARTS-PATH>/FeynArtsAdd.m

Copied and modified on the basis of FeynArts39.m with thanks to Thomas 
Hahn.
*)

BeginPackage["FeynArts`"]

FieldMatchExtQ::usage = ""

FieldMemberExtQ::usage = ""

Fext::usage = ""

Fsel::usage = ""

Ftel::usage = ""

Fs::usage = ""

Ft::usage = ""

STChannelFields::usage = ""

STChannelFieldsExt::usage = ""

SChannelQ::usage = ""

TChannelQ::usage = ""

NotSChannelQ::usage = ""

NotTChannelQ::usage = ""

SChannelExtQ::usage = ""

TChannelExtQ::usage = ""

NotSChannelExtQ::usage = ""

NotTChannelExtQ::usage = ""


Begin["`Private`"]

Print[];
Print["FeynArtsAdd 1.0 (16 Dec 2016)"];
Print["by Matthias Kesenheimer, with thanks to Thomas Hahn"];


(*Functions to check for a given particle in s- or t-channel*)
(*now with additional functions which can be used to check for an explicit coupling to external fields.*)

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


End[]
EndPackage[]
