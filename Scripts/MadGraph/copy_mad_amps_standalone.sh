#!/bin/bash

rm -rf madME
mkdir madME

#final states for which the amplitudes were generated
final1="n1n2"
#new final states name
final2="nInJ"

#generate the lines we want to inject
echo "C" > addfinalstates.txt
echo "C FINAL STATES" >> addfinalstates.txt
echo "C" >> addfinalstates.txt
echo "#include \"finalstate.h\"" >> addfinalstates.txt

echo "      if(final1.eq.final2) then ! equal final states" > addfinalstatescheck.txt
echo "        ANS(IPROC)=ANS(IPROC)/DBLE(IDEN(IPROC)*2)" >> addfinalstatescheck.txt
echo "      else" >> addfinalstatescheck.txt
echo "        ANS(IPROC)=ANS(IPROC)/DBLE(IDEN(IPROC))" >> addfinalstatescheck.txt
echo "      endif" >> addfinalstatescheck.txt

#echo "C" > addsubtraction.txt
#echo "C VARIABLES TO APPLY THE DIAGRAM SUBTRACTION SCHEMES" >> addsubtraction.txt
#echo "C" >> addsubtraction.txt
#echo "#include \"dsubtraction.h\"" >> addsubtraction.txt
#echo "C" >> addsubtraction.txt
#echo "C GLOBAL VARIABLES" >> addsubtraction.txt

for dir in $(ls -d */); do
    if [ "$dir" == "madME/" ]; then
      continue
    fi

    # copy and rename the files to madME
    dir=$(echo $dir | sed -e 's:/::g')
    # process name
    proc=$(echo $dir | sed -e 's:P0_::g')
    proc=$(echo $proc | sed -e "s:${final1}:${final2}:g")
    #to upper case
    uproc=$(echo "$proc" | tr '[:lower:]' '[:upper:]')

    filename=r_${proc}.f
    filenameps=${proc}.ps
    echo $filename
    cp $dir/matrix.f madME/$filename
    cp $dir/matrix.ps madME/$filenameps

    # make the layout prettier
    x=madME/$filename
    sed -i -e 's/      )/)/g' $x
    sed -i -e 's/     )/)/g' $x
    sed -i -e 's/    )/)/g' $x
    sed -i -e 's/   )/)/g' $x
    sed -i -e 's/  )/)/g' $x
    sed -i -e 's/ )/)/g' $x
    sed -i -e 's/      (/(/g' $x
    sed -i -e 's/     (/(/g' $x
    sed -i -e 's/    (/(/g' $x
    sed -i -e 's/   (/(/g' $x
    sed -i -e 's/  (/(/g' $x
    sed -i -e 's/ (/(/g' $x
    sed -i -e 's/      ,/,/g' $x
    sed -i -e 's/     ,/,/g' $x
    sed -i -e 's/    ,/,/g' $x
    sed -i -e 's/   ,/,/g' $x
    sed -i -e 's/  ,/,/g' $x
    sed -i -e 's/ ,/,/g' $x
    # replace the line breaks "       \n     &     "
    # convert the file into hex code | truncate newlines | delete "       \n     &     " | convert it back to ascii
    xxd -p $x | tr -d \\n | sed 's/202020202020200a2020202020262020202020//g' | xxd -r -p > temp
    mv temp $x

    # now apply multiple changes
    # add process name
    sed -i -e "s/MATRIX/MATRIX_${uproc}/g" $x
    # add arguments
    #sed -i -e "s/(P1,ANS)/(P1,ANS,WDTI)/g" $x
    #sed -i -e "s/(P,NHEL,IC)/(P,NHEL,IC,WDTI)/g" $x
    #sed -i -e "s/(P,NHEL(1,IHEL),JC(1))/(P,NHEL(1,IHEL),JC(1),WDTI)/g" $x
    
    # add finalstats.h
    sed -i -e '/PARAMETER(THEL=NCOMB\*NCROSS)/r addfinalstates.txt' $x
    
    #remove unnecessary variables
    sed -i -e "s/      REAL     xran1//g" $x
    sed -i -e "s/      EXTERNAL xran1//g" $x
    #sed -i -e "s/      CALL SWITCHMOM(P1,P,IC(1,IPROC),JC,NEXTERNAL)//g" $x
    
    # rename common blocks
    sed -i -e "s/to_amps/to_Ramps_${proc}/g" $x
    #sed -i -e "s/P,NHEL(1,IHEL)/P1,NHEL(1,IHEL)/g" $x

    # add finastates-check (if equal final states divide amp by 2)
    sed -i -e "s/ANS(IPROC)=ANS(IPROC)\/DBLE(IDEN(IPROC))/marker/g" $x
    sed -i -e '/marker/r addfinalstatescheck.txt' $x
    sed -i -e 's/marker//g' $x
    
    # add dsubtraction.h
    #sed -i -e "s/C GLOBAL VARIABLES/marker/g" $x
    #sed -i -e '/marker/r addsubtraction.txt' $x
    #sed -i -e 's/marker//g' $x

    # rename to prevent clashes with FormCalc
    sed -i -e 's/MB1/MBL/g' $x
    sed -i -e 's/MB2/MBR/g' $x

    # rename to generalize
    sed -i -e 's/MN1/MNI/g' $x
    sed -i -e 's/MN2/MNJ/g' $x

    sed -i -e 's/GDLN1M/GDLNIM/g' $x
    sed -i -e 's/GDLN2M/GDLNJM/g' $x
    sed -i -e 's/GDLN1P/GDLNIP/g' $x
    sed -i -e 's/GDLN2P/GDLNJP/g' $x

    sed -i -e 's/GDRN1M/GDRNIM/g' $x
    sed -i -e 's/GDRN2M/GDRNJM/g' $x
    sed -i -e 's/GDRN1P/GDRNIP/g' $x
    sed -i -e 's/GDRN2P/GDRNJP/g' $x

    sed -i -e 's/GULN1M/GULNIM/g' $x
    sed -i -e 's/GULN2M/GULNJM/g' $x
    sed -i -e 's/GULN1P/GULNIP/g' $x
    sed -i -e 's/GULN2P/GULNJP/g' $x

    sed -i -e 's/GURN1M/GURNIM/g' $x
    sed -i -e 's/GURN2M/GURNJM/g' $x
    sed -i -e 's/GURN1P/GURNIP/g' $x
    sed -i -e 's/GURN2P/GURNJP/g' $x

    sed -i -e 's/GZN12/GZNIJ/g' $x

    sed -i -e 's/GH1N12/GH1NIJ/g' $x
    sed -i -e 's/GH2N12/GH2NIJ/g' $x
    sed -i -e 's/GH3N12/GH3NIJ/g' $x

    #remove junk files
    find . -type f -name '*.f-e'  -exec rm -f '{}' \;
done

# generate maxamps.inc file
echo "      integer    maxamps, maxflow" > madME/maxamps.inc
echo "      parameter (maxamps=6000,maxflow=  10)" >> madME/maxamps.inc


#Warnings
echo ""
echo "Check the generated files for errors and for line length violations! "

#clean up
rm addfinalstates.txt addfinalstatescheck.txt addsubtraction.txt