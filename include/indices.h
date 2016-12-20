c############### indices.h #############################################
c last modified by MK, date 19.12.2016
#define EXTLEGS 6

c process definitions
        integer Neu(EXTLEGS), Cha(EXTLEGS), Gen(EXTLEGS)
        common/cindices/ Neu, Cha, Gen
        
        integer Sq1, Sq2
        common/cindices/ Sq1, Sq2
c############### end indices.h #########################################