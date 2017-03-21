* SpinCorrelatedSum.frm
* the FORM part of the SpinCorrelatedSum function
* this file is part of FormCalc
* last modified 7 Dec 16 th
* Copied and modified on the basis of PolarizationSum.frm with thanks to
* Thomas Hahn.
*
* Form Manual
* https://www.nikhef.nl/~form/maindir/documentation/reference/online/online.html

#procedure Prepare
#call Square
#call eiei

#call ConstBracket
.sort

collect mulM;

id Conjugate([f]?LOOPINT(?x)) = Conjugate([f](?x));
also Conjugate(?x) = mulM(Conjugate(?x));

moduleoption polyfun=mulM;
.sort

#call Factor(mulM)

b mulM;
.sort
keep brackets;

argument mulM;
toPolynomial;
endargument;

toPolynomial;

.sort
#endprocedure

***********************************************************************

#procedure SpinCorrSum(i, m, d)
b e`i', ec`i', z`i', zc`i', Pol;
.sort
d `d';
keep brackets;

id e`i' = ET`i'(?);
id ec`i' = ETC`i'(?);
id z`i' = ET`i'(?);
id zc`i' = ETC`i'(?);
mul DF;

#if `m' == "0"
* massless case

#if "`GaugeTerms'" != "Off"
id DF * ET`i'([mu]?) * ETC`i'([nu]?) = 
     (- d_([mu], [be])
      - (d_(k`i', [mu]) * d_(k`i', [be]) 
     	- (eta`i'.k`i')*(d_(eta`i', [mu])*d_(k`i', [be]) 
     		+ d_(eta`i', [be])*d_(k`i', [mu])))
       /((eta`i'.k`i')^2) ) * 
     (- d_([al], [nu])
      - (d_(k`i', [al]) * d_(k`i', [nu]) 
     	- (eta`i'.k`i')*(d_(eta`i', [al])*d_(k`i', [nu]) 
     		+ d_(eta`i', [nu])*d_(k`i', [al])))
       /((eta`i'.k`i')^2) );
#else
id DF * ET`i'([mu]?) * ETC`i'([nu]?) = d_([mu], [be])*d_([al], [nu]);
#endif

* The eta are gauge-dependent vectors.
* Their appearance in the result is supposed to alert
* the user to the presence of gauge-dependent terms.
* The eta must fulfill eta.eta = e.eta = 0 and k.eta != 0.
* Instead of imposing eta.eta = 0 one can add
* - d_(k`i', [mu])*d_(k`i', [nu])*(eta`i'.eta`i')/(eta`i'.k`i')^2

#if "`GaugeTerms'" != "Off"
also DF * Pol(e`i', [mu]?, [nu]?) * Pol(ec`i', [ro]?, [si]?) =
  (d_([mu], [ro])*d_([nu], [si]) +
  d_([mu], [si])*d_([nu], [ro]) -
  d_([mu], [nu])*d_([ro], [si])) * 
  (- d_([al], [be])
      - (d_(k`i', [al]) * d_(k`i', [be]) 
     	- (eta`i'.k`i')*(d_(eta`i', [al])*d_(k`i', [be]) 
     		+ d_(eta`i', [be])*d_(k`i', [al])))
       /((eta`i'.k`i')^2) );
     
also DF = (- d_([al], [be])
      - (d_(k`i', [al]) * d_(k`i', [be]) 
     	- (eta`i'.k`i')*(d_(eta`i', [al])*d_(k`i', [be]) 
     		+ d_(eta`i', [be])*d_(k`i', [al])))
       /((eta`i'.k`i')^2) );
#else
also DF * Pol(e`i', [mu]?, [nu]?) * Pol(ec`i', [ro]?, [si]?) =
  (d_([mu], [ro])*d_([nu], [si]) +
  d_([mu], [si])*d_([nu], [ro]) -
  d_([mu], [nu])*d_([ro], [si])) * 
  (-d_([al], [be])) * (`Dim' - 2)/`Dim';
  
also DF = -d_([al], [be]) * (`Dim' - 2)/`Dim';
#endif

#else
* massive case

id DF * ET`i'([mu]?) * ETC`i'([nu]?) = (-d_([mu], [be]) +
  k`i'([mu])*k`i'([be])/(`m')^2) * (-d_([al], [nu]) +
  k`i'([al])*k`i'([nu])/(`m')^2);

also DF = (-d_([al], [be]) + k`i'([al])*k`i'([be])/(`m')^2);

#call Square
#endif

.sort
d `Dim';

#call eiei
#call eiki
#call kikj
#call Neglect
#endprocedure

***********************************************************************

#procedure Emit(m)
.sort

id D = Dminus4 + 4;

contract;

* Cycling the momenta at this point (rather than in DotSimplify)
* is necessary to obtain all possible terms coming from the
* cancellation of an etaM.kN before sending the etaM to zero.
#do i = {`MomRange'}
#ifdef `k`i''
id k`i' = `k`i'';
.sort;
#endif
#enddo

#if "`GaugeTerms'" == "False"
#do i = 1, `Legs'
id eta`i' = 0;
#enddo
.sort;

* UPDATE: With a new definition of the polarization sums the eta vectors
* do not vanish in the final results.
* only for the massless case, when all polarization vectors have been 
* set to zero, we must multiply the amplitude with an additional factor
* to account for the degrees of freedom (GaugeTerms = False for the 
* massive case is meaningless).
*#if `m' == "0"
*multiply (`Dim' - 2)/`Dim';
*#endif

#endif

* we get rid of the eta remainder terms with a gauge choice eta = (1,0,0,0)
* #if "`GaugeTerms'" == "LightGauge"
#do i = 1, `Legs'
id eta`i'.al = al0;
id eta`i'.be = be0;
id eta`i'([al]) = al0;
id eta`i'([be]) = be0;
#do j = 1, `Legs'
id eta`i'.k`j' = k`j'0;
id 1/eta`i'.k`j' = 1/k`j'0;
id k`j'.eta`i' = k`j'0;
id 1/k`j'.eta`i' = 1/k`j'0;
#enddo
#enddo
.sort;
* #endif

#call EtaSubst

id D = Dminus4Eps + 4;

#call DotSimplify
#call Abbreviate

b SumOver, Conjugate, Den;
.sort

collect mulM;
makeinteger mulM;

argument mulM;
toPolynomial;
endargument;

b Conjugate, mulM;
.sort
keep brackets;

toPolynomial;

.sort

#write "%X"

b SumOver, Den;
print +s;
.end
#endprocedure

***********************************************************************

#procedure ReplaceOpenLorentzIndices
.sort

s deltaalbe
#do i = 1, `Legs'
s eta`i'al, eta`i'be;
s k`i'al, k`i'be;
#enddo
v al, be;

#do i = 1, `Legs'
*#if "`GaugeTerms'" == "LightGauge"
id eta`i'([al]) = al0;
id eta`i'([be]) = be0;
*#else
*id eta`i'([al]) = eta`i'al;
*id eta`i'([be]) = eta`i'be;
*#endif
id k`i'([al]) = k`i'al;
id k`i'([be]) = k`i'be;
id d_([al],[be]) = deltaalbe;
id e_(k1?,k2?,k3?,[al]) = e_(k1,k2,k3,al);
id e_(k1?,k2?,k3?,[be]) = e_(k1,k2,k3,be);
id e_(k1?,k2?,[al],[be]) = e_(k1,k2,al,be);
#enddo

.sort;

#endprocedure

***********************************************************************

#define LoopInt "A0, A00, B0, B1, B00, B11, B001, B111, A0i, B0i, C0i, D0i, E0i, F0i"

cf SumOver, Den, Conjugate, Mat, `LoopInt';
s D, Dminus4, Dminus4Eps, `Invariants';

nt GA;
f GF;
s DF, TAG, ETAG, QTAG, al0, be0, deltaal0, deltabe0;
t ET, ETC, Pol;
#do i = 1, `Legs'
s k`i'0;
t ET`i';
t ETC`i';
#enddo
cf TMP, ABB;
auto s ARG;
set LOOPINT: `LoopInt';
set INVS: `Invariants';

i [mu], [nu], [ro], [si], [i], [al], [be];
v [p1], [p2];
s [s1], [s2], [x], [y], [n];
t [t];
cf [f];

extrasymbols array subM;
cf abbM, fermM, dotM, addM, mulM, powM;

