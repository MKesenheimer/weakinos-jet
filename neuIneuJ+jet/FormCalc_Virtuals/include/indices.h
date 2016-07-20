c############### indices.h #############################################
c last modified by MK, date 18.07.2016

#ifndef INDICES_H
#define INDICES_H

c process definitions
        integer legs
        parameter (legs=6)
        integer Neu(legs), Cha(legs), Gen(legs)
        common/cindices/ Neu, Cha, Gen
#endif
c############### end indices.h #########################################