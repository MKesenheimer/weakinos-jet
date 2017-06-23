#!/bin/bash

for f in $(ls r_*.f); do
   #sed -i -e "s/CALL SWITCHMOM(P1,P,IC(1,IPROC),JC,NEXTERNAL)/!CALL SWITCHMOM(P1,P,IC(1,IPROC),JC,NEXTERNAL)/g" $f
   sed -i -e "s/(P,NHEL(1,IHEL),JC(1))/(P1,NHEL(1,IHEL),JC(1))/g" $f
done