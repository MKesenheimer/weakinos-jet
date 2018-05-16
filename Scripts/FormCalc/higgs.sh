#!/bin/sh
# Copyright (C) Matthias Kesenheimer - All Rights Reserved
# Written by Matthias Kesenheimer <m.kesenheimer@gmx.net>, 2017

for x in $(ls *.F); do
    echo $x
    # make higgs masses complex
    gsed -i 's/Mh02/Mh02C/g' $x
    gsed -i 's/MHH2/MHH2C/g' $x
    gsed -i 's/MA02/MA02C/g' $x
    gsed -i 's/MHp2/MHp2C/g' $x
done

rm -f *.F-e