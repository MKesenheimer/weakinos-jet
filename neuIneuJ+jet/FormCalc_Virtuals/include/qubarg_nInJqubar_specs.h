#if 0
* specs.h
* process specifications
* generated by FormCalc 9.4 (7 Jun 2016) on 2-Mar-2017 7:42
#endif

#undef FCVERSION
#define FCVERSION "FormCalc 9.4 (7 Jun 2016)"

#undef PROCNAME
#define PROCNAME "Fb3iiV5iF11iF11iFb3ii"

#undef SQUAREDME_FUNC
#define SQUAREDME_FUNC SquaredME

#undef KIN
#define KIN "2to3.F"

#undef IDENTICALFACTOR
#define IDENTICALFACTOR 2

#undef Compose
#define Compose(f,c,A,B,C,D,E) c(c(c(c(f(1,A,1),f(2,B,1)),f(3,C,2)),f(4,D,2)),f(5,E,2))

#undef Generic
#define Generic(f,c) Compose(f,c,FERMION,PHOTON,FERMION,FERMION,FERMION)

#undef Anti
#define Anti(f,c) Compose(f,c,-1,1,1,1,-1)

#undef Mass
#define Mass(f,c) Compose(f,c,Mf(3,Gen(1)),0,MNeu(Neu(3)),MNeu(Neu(4)),Mf(3,Gen(5)))

#undef Charge
#define Charge(f,c) Compose(f,c,-2/3.D0,0,0,0,-2/3.D0)

#undef ColorCharge
#define ColorCharge(f,c) Compose(f,c,-2/Sqrt(3.D0),Sqrt(3.D0),0,0,-2/Sqrt(3.D0))

