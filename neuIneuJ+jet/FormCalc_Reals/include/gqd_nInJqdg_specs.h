#if 0
* specs.h
* process specifications
* generated by FormCalc 9.4 (7 Jun 2016) on 19-Dec-2016 15:39
#endif

#undef FCVERSION
#define FCVERSION "FormCalc 9.4 (7 Jun 2016)"

#undef PROCNAME
#define PROCNAME "V5iF4iiF11iF11iF4iiV5i"

#undef SQUAREDME_FUNC
#define SQUAREDME_FUNC SquaredME

#undef KIN
#define KIN "2to4.F"

#undef IDENTICALFACTOR
#define IDENTICALFACTOR 2

#undef Compose
#define Compose(f,c,A,B,C,D,E,F) c(c(c(c(c(f(1,A,1),f(2,B,1)),f(3,C,2)),f(4,D,2)),f(5,E,2)),f(6,F,2))

#undef Generic
#define Generic(f,c) Compose(f,c,PHOTON,FERMION,FERMION,FERMION,FERMION,PHOTON)

#undef Anti
#define Anti(f,c) Compose(f,c,1,1,1,1,1,1)

#undef Mass
#define Mass(f,c) Compose(f,c,0,Mf(4,Gen(2)),MNeu(Neu(3)),MNeu(Neu(4)),Mf(4,Gen(5)),0)

#undef Charge
#define Charge(f,c) Compose(f,c,0,-1/3.D0,0,0,-1/3.D0,0)

#undef ColorCharge
#define ColorCharge(f,c) Compose(f,c,Sqrt(3.D0),2/Sqrt(3.D0),0,0,2/Sqrt(3.D0),Sqrt(3.D0))

