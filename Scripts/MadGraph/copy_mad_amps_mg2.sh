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

for filename in $(ls b_*.f -o sborn_*.f); do
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

#Warnings
echo ""
echo "Check the generated files for errors and for line length violations! "

#clean up
rm addfinalstates.txt addfinalstatescheck.txt addfinalstatescheck2.txt

# copy all relevant Born-Amplitudes to madME/Born
cd madME
mkdir Born
cp b_cl_001_001.f b_cl_008_003.f b_sf_002_008.f b_sf_006_007.f b_sf_022_006.f sborn_cl_005.f b_cl_001_002.f b_cl_021_001.f b_sf_002_009.f b_sf_006_008.f b_sf_022_007.f sborn_cl_006.f b_cl_001_003.f b_cl_021_002.f b_sf_003_004.f b_sf_006_009.f b_sf_022_008.f sborn_cl_007.f b_cl_002_001.f b_cl_021_003.f b_sf_003_005.f b_sf_007_004.f b_sf_022_009.f sborn_cl_008.f b_cl_002_002.f b_cl_022_001.f b_sf_003_006.f b_sf_007_005.f b_sf_023_004.f sborn_cl_011.f b_cl_002_003.f b_cl_022_002.f b_sf_003_007.f b_sf_007_006.f b_sf_023_005.f sborn_cl_021.f b_cl_003_001.f b_cl_022_003.f b_sf_003_008.f b_sf_007_007.f b_sf_023_006.f sborn_cl_022.f b_cl_003_002.f b_cl_023_001.f b_sf_003_009.f b_sf_007_008.f b_sf_023_007.f sborn_cl_023.f b_cl_003_003.f b_cl_023_002.f b_sf_004_004.f b_sf_007_009.f b_sf_023_008.f sborn_cl_024.f b_cl_004_001.f b_cl_023_003.f b_sf_004_005.f b_sf_008_004.f b_sf_023_009.f b_cl_004_002.f b_cl_024_001.f b_sf_004_006.f b_sf_008_005.f b_sf_024_004.f sborn_sf_001.f b_cl_004_003.f b_cl_024_002.f b_sf_004_007.f b_sf_008_006.f b_sf_024_005.f sborn_sf_002.f b_cl_005_001.f b_cl_024_003.f b_sf_004_008.f b_sf_008_007.f b_sf_024_006.f sborn_sf_003.f b_cl_005_002.f b_sf_001_004.f b_sf_004_009.f b_sf_008_008.f b_sf_024_007.f sborn_sf_004.f b_cl_005_003.f b_sf_001_005.f b_sf_005_004.f b_sf_008_009.f b_sf_024_008.f sborn_sf_005.f b_cl_006_001.f b_sf_001_006.f b_sf_005_005.f b_sf_021_004.f b_sf_024_009.f sborn_sf_006.f b_cl_006_002.f b_sf_001_007.f b_sf_005_006.f b_sf_021_005.f sborn_sf_007.f b_cl_006_003.f b_sf_001_008.f b_sf_005_007.f b_sf_021_006.f sborn_sf_008.f b_cl_007_001.f b_sf_001_009.f b_sf_005_008.f b_sf_021_007.f sborn_sf_011.f b_cl_007_002.f b_sf_002_004.f b_sf_005_009.f b_sf_021_008.f sborn_cl_001.f sborn_sf_021.f b_cl_007_003.f b_sf_002_005.f b_sf_006_004.f b_sf_021_009.f sborn_cl_002.f sborn_sf_022.f b_cl_008_001.f b_sf_002_006.f b_sf_006_005.f b_sf_022_004.f sborn_cl_003.f sborn_sf_023.f b_cl_008_002.f b_sf_002_007.f b_sf_006_006.f b_sf_022_005.f sborn_cl_004.f sborn_sf_024.f Born