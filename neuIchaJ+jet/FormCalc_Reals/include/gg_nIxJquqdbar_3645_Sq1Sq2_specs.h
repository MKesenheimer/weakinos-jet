#if 0
* specs.h
* process specifications
* generated by FormCalc 9.4 (7 Jun 2016) on 23-Oct-2017 10:54
#endif

#undef FCVERSION
#define FCVERSION "FormCalc 9.4 (7 Jun 2016)"

#undef PROCNAME
#define PROCNAME "V5iV5iF11iF12iF3iiFb4ii"

#undef SQUAREDME_FUNC
#define SQUAREDME_FUNC SquaredME

#undef KIN
#define KIN "2to4.F"

#undef IDENTICALFACTOR
#define IDENTICALFACTOR 1

#undef Compose
#define Compose(f,c,A,B,C,D,E,F) c(c(c(c(c(f(1,A,1),f(2,B,1)),f(3,C,2)),f(4,D,2)),f(5,E,2)),f(6,F,2))

#undef Generic
#define Generic(f,c) Compose(f,c,PHOTON,PHOTON,FERMION,FERMION,FERMION,FERMION)

#undef Anti
#define Anti(f,c) Compose(f,c,1,1,1,1,1,-1)

#undef Mass
#define Mass(f,c) Compose(f,c,0,0,MNeu(Neu(3)),MCha(Cha(4)),Mf(3,Gen(5)),Mf(4,Gen(6)))

#undef Charge
#define Charge(f,c) Compose(f,c,0,0,0,-1,2/3.D0,1/3.D0)

#undef ColorCharge
#define ColorCharge(f,c) Compose(f,c,Sqrt(3.D0),Sqrt(3.D0),0,0,2/Sqrt(3.D0),-2/Sqrt(3.D0))

