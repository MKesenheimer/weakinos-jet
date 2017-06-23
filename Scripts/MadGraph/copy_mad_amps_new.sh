#!/bin/bash

rm -rf madME
mkdir madME

#generate the lines we want to inject
echo "C" > addfinalstates.txt
echo "C FINAL STATES" >> addfinalstates.txt
echo "#include \"finalstate.h\"" >> addfinalstates.txt

echo "      if(final1.eq.final2) then ! equal final states" > addfinalstatescheck.txt
echo "        ANS(IPROC)=ANS(IPROC)/DBLE(IDEN(IPROC)*2)" >> addfinalstatescheck.txt
echo "      else" >> addfinalstatescheck.txt
echo "        ANS(IPROC)=ANS(IPROC)/DBLE(IDEN(IPROC))" >> addfinalstatescheck.txt
echo "      endif" >> addfinalstatescheck.txt

echo "      if(final1.eq.final2) then ! equal final states" > addfinalstatescheck2.txt
echo "        ANS(IPROC+1)=ANS(IPROC+1)/DBLE(IDEN(IPROC)*2)" >> addfinalstatescheck2.txt
echo "      else" >> addfinalstatescheck2.txt
echo "        ANS(IPROC+1)=ANS(IPROC+1)/DBLE(IDEN(IPROC))" >> addfinalstatescheck2.txt
echo "      endif" >> addfinalstatescheck2.txt

for filename in $(ls *.f); do
    if [ "$dir" == "madME/" ]; then
      continue
    fi

    # copy and rename the files to madME
    echo $filename
    cp $filename madME/

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
    xxd -p $x | tr -d \\n | sed 's/2020202020200a2020202020262020202020//g' | xxd -r -p > temp
    mv temp $x

    #now apply multiple changes
    sed -i -e '/PARAMETER(THEL=NCOMB\*NCROSS)/r addfinalstates.txt' $x

    sed -i -e "s/ANS(IPROC)=ANS(IPROC)\/DBLE(IDEN(IPROC))/marker/g" $x
    sed -i -e '/marker/r addfinalstatescheck.txt' $x
    sed -i -e 's/marker//g' $x

    sed -i -e "s/ANS(IPROC+1)=ANS(IPROC+1)\/DBLE(IDEN(IPROC))/marker/g" $x
    sed -i -e '/marker/r addfinalstatescheck2.txt' $x
    sed -i -e 's/marker//g' $x

    sed -i -e 's/MB1/MBL/g' $x
    sed -i -e 's/MB2/MBR/g' $x
    sed -i -e 's/WB1/WBL/g' $x
    sed -i -e 's/WB2/WBR/g' $x
    
    sed -i -e 's/MT1/MTL/g' $x
    sed -i -e 's/MT2/MTR/g' $x
    sed -i -e 's/WT1/WTL/g' $x
    sed -i -e 's/WT2/WTR/g' $x

    sed -i -e 's/MN1/MNI/g' $x
    sed -i -e 's/MN2/MNJ/g' $x
    sed -i -e 's/MX1/MXI/g' $x
    sed -i -e 's/MX2/MXJ/g' $x

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

    sed -i -e 's/GB1N1P/GB1NIP/g' $x
    sed -i -e 's/GB2N1P/GB2NIP/g' $x
    sed -i -e 's/GB1N1M/GB1NIM/g' $x
    sed -i -e 's/GB2N1M/GB2NIM/g' $x

    sed -i -e 's/GB1N2P/GB1NJP/g' $x
    sed -i -e 's/GB2N2P/GB2NJP/g' $x
    sed -i -e 's/GB1N2M/GB1NJM/g' $x
    sed -i -e 's/GB2N2M/GB2NJM/g' $x

    sed -i -e 's/GT1N1P/GT1NIP/g' $x
    sed -i -e 's/GT2N1P/GT2NIP/g' $x
    sed -i -e 's/GT1N1M/GT1NIM/g' $x
    sed -i -e 's/GT2N1M/GT2NIM/g' $x

    sed -i -e 's/GT1N2P/GT1NJP/g' $x
    sed -i -e 's/GT2N2P/GT2NJP/g' $x
    sed -i -e 's/GT1N2M/GT1NJM/g' $x
    sed -i -e 's/GT2N2M/GT2NJM/g' $x

    sed -i -e 's/GZN12/GZNIJ/g' $x
    sed -i -e 's/GZX12/GZXIJ/g' $x
    sed -i -e 's/GWN1X2/GWNIXJ/g' $x
    sed -i -e 's/GWX2N1/GWXJNI/g' $x
    sed -i -e 's/GZX11/GZXII/g' $x

    sed -i -e 's/GH1N12/GH1NIJ/g' $x
    sed -i -e 's/GH2N12/GH2NIJ/g' $x
    sed -i -e 's/GH3N12/GH3NIJ/g' $x

    sed -i -e 's/GDLX1M/GDLXIM/g' $x
    sed -i -e 's/GDLX2M/GDLXJM/g' $x
    sed -i -e 's/GDLX1P/GDLXIP/g' $x
    sed -i -e 's/GDLX2P/GDLXJP/g' $x

    sed -i -e 's/GDRX1M/GDRXIM/g' $x
    sed -i -e 's/GDRX2M/GDRXJM/g' $x
    sed -i -e 's/GDRX1P/GDRXIP/g' $x
    sed -i -e 's/GDRX2P/GDRXJP/g' $x

    sed -i -e 's/GULX1M/GULXIM/g' $x
    sed -i -e 's/GULX2M/GULXJM/g' $x
    sed -i -e 's/GULX1P/GULXIP/g' $x
    sed -i -e 's/GULX2P/GULXJP/g' $x

    sed -i -e 's/GURX1M/GURXIM/g' $x
    sed -i -e 's/GURX2M/GURXJM/g' $x
    sed -i -e 's/GURX1P/GURXIP/g' $x
    sed -i -e 's/GURX2P/GURXJP/g' $x

    sed -i -e 's/GT1X1P/GT1XIP/g' $x
    sed -i -e 's/GT2X1P/GT2XIP/g' $x
    sed -i -e 's/GT1X1M/GT1XIM/g' $x
    sed -i -e 's/GT2X1M/GT2XIM/g' $x

    sed -i -e 's/GT1X2P/GT1XJP/g' $x
    sed -i -e 's/GT2X2P/GT2XJP/g' $x
    sed -i -e 's/GT1X2M/GT1XJM/g' $x
    sed -i -e 's/GT2X2M/GT2XJM/g' $x

    sed -i -e 's/GH1X12/GH1XIJ/g' $x
    sed -i -e 's/GH2X12/GH2XIJ/g' $x
    sed -i -e 's/GH3X12/GH3XIJ/g' $x

    sed -i -e 's/GH1X11/GH1XII/g' $x
    sed -i -e 's/GH2X11/GH2XII/g' $x
    sed -i -e 's/GH3X11/GH3XII/g' $x
    
    sed -i -e 's/GHN1X2/GHNIXJ/g' $x
    sed -i -e 's/GHX2N1/GHXJNI/g' $x
    
    sed -i -e 's/GB1X1P/GB1XIP/g' $x
    sed -i -e 's/GB1X2P/GB1XJP/g' $x
    sed -i -e 's/GB2X1P/GB2XIP/g' $x
    sed -i -e 's/GB2X2P/GB2XJP/g' $x
    
    sed -i -e 's/GB1X1M/GB1XIM/g' $x
    sed -i -e 's/GB1X2M/GB1XJM/g' $x
    sed -i -e 's/GB2X1M/GB2XIM/g' $x
    sed -i -e 's/GB2X2M/GB2XJM/g' $x
    
    sed -i -e 's/GT1X1P/GT1XIP/g' $x
    sed -i -e 's/GT1X2P/GT1XJP/g' $x
    sed -i -e 's/GT2X1P/GT2XIP/g' $x
    sed -i -e 's/GT2X2P/GT2XJP/g' $x
    
    sed -i -e 's/GT1X1M/GT1XIM/g' $x
    sed -i -e 's/GT1X2M/GT1XJM/g' $x
    sed -i -e 's/GT2X1M/GT2XIM/g' $x
    sed -i -e 's/GT2X2M/GT2XJM/g' $x

    #remove junk files
    find . -type f -name '*.f-e'  -exec rm -f '{}' \;
done

#Warnings
echo ""
echo "Check the generated files for errors and for line length violations! "

#clean up
rm addfinalstates.txt addfinalstatescheck.txt addfinalstatescheck2.txt

# copy all relevant Born-Amplitudes to madME/Born
cd madME
mkdir Real
mv realmtrx_*.f ../leshouches_R_*.inc ../sreal_proc.f Real
mkdir Born
mv b_cl_*.f sborn_cl_*.f ../leshouches_*.inc ../sborn_proc.f Born

