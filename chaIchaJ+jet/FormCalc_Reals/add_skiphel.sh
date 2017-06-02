#!/bin/bash

for x in $(ls *_SquaredME.F); do
    echo $x
    proc=${x:0:${#x}-12}

cat > vars.txt << EOM
* -------- MK: added -----------------------------------------------------------
#include "skiphel_vars.h"
* -------- MK: added -----------------------------------------------------------
EOM

    gsed -i 's/data MatSUN \/NaN(1)\//TAG/g' $x
    gsed -i '/TAG/r vars.txt' $x
    gsed -i "s/TAG/data MatSUN \/NaN(1)\//g" $x

    gsed -i 's/data MatSUN \/NaN(4)\//TAG/g' $x
    gsed -i '/TAG/r vars.txt' $x
    gsed -i "s/TAG/data MatSUN \/NaN(4)\//g" $x

    gsed -i 's/data MatSUN \/NaN(9)\//TAG/g' $x
    gsed -i '/TAG/r vars.txt' $x
    gsed -i "s/TAG/data MatSUN \/NaN(9)\//g" $x

    cat > func.txt << EOM
* -------- MK: added -----------------------------------------------------------
#if NSKIP > 0
          if(.not.skiphel(h)) then
            call ${proc}_SquaredMEHel(HelInd(SIMD,res(HelAll(1),h)), flags)
          else
            res(HelAll(1),h) = 0D0
#if DEBUG >= 1
            print*,"skipping helicity ",h
#endif
          endif
          ! determine which hel-amplitudes to skip during the initialisation phase
          if(initskip) then
            if(res(HelAll(1),h).eq.0D0) mskip(h) = mskip(h) + 1
            if(mskip(h).ge.NSKIP) skiphel(h) = .true.
            if(minit.ge.ninit) initskip = .false.
            minit = minit + 1
          endif
#else
          call ${proc}_SquaredMEHel(HelInd(SIMD,res(HelAll(1),h)), flags)
#endif
#if DEBUG >= 2
          print*,h,res(HelAll(1),h)
#endif
* -------- MK: added -----------------------------------------------------------

EOM

    gsed -i "s/call ${proc}_SquaredMEHel(HelInd(SIMD,res(HelAll(1),h)), flags)/TAG/g" $x
    gsed -i '/TAG/r func.txt' $x
    gsed -i "s/TAG//g" $x
done

rm vars.txt
rm func.txt
rm -f *.F-e
