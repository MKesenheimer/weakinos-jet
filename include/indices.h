c############### indices.h #############################################
c last modified by MK, date 18.07.2016
#ifndef INDICES_H
#define INDICES_H

#define EXTLEGS 6

c process definitions
        integer Neu(EXTLEGS), Cha(EXTLEGS), Gen(EXTLEGS)
        integer Sfe6, Sfe7
        common/cindices/ Neu, Cha, Gen, Sfe6, Sfe7
#endif
c############### end indices.h #########################################