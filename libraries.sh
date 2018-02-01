#!/bin/bash
# Copyright (C) Matthias Kesenheimer - All Rights Reserved
# Written by Matthias Kesenheimer <m.kesenheimer@gmx.net>, 2017

# this is a handy script to backup and restore previously compiled
# libraries to your home directoy. Compilation of these libraries
# takes a long time and usually must be done only once.
# Examples:
# ./libraries.sh backup neuIneuJ+jet -> backup copy the compiled libraries from neuIneuJ+jet to your home directory
# ./libraries.sh copy neuIneuJ+jet -> copy the libraries from your home directory to the project directory neuIneuJ+jet

if [[ "$1" == "copy" ]]; then
  echo "cp ~/gfortran/*.a ./Tools/"
  cp ~/gfortran/*.a ./Tools/
  echo "cp ~/gfortran/FormCalc_$2/formcalc_reals.a ./$2/FormCalc_Reals"
  cp ~/gfortran/FormCalc_$2/formcalc_reals.a ./$2/FormCalc_Reals
  echo "cp ~/gfortran/FormCalc_$2/formcalc_virtuals.a ./$2/FormCalc_Virtuals"
  cp ~/gfortran/FormCalc_$2/formcalc_virtuals.a ./$2/FormCalc_Virtuals
elif [[ "$1" == "copy-all" ]]; then
  echo "cp ~/gfortran/*.a ./Tools/"
  cp ~/gfortran/*.a ./Tools/
  for x in "neuIneuJ+jet" "neuIchaJ+jet" "chaIchaJ+jet"; do
    echo "cp ~/gfortran/FormCalc_$x/formcalc_reals.a ./$x/FormCalc_Reals"
    cp ~/gfortran/FormCalc_$x/formcalc_reals.a ./$x/FormCalc_Reals
    echo "cp ~/gfortran/FormCalc_$x/formcalc_virtuals.a ./$x/FormCalc_Virtuals"
    cp ~/gfortran/FormCalc_$x/formcalc_virtuals.a ./$x/FormCalc_Virtuals
  done
elif [[ "$1" == "backup" ]]; then
  echo "cp ./$2/FormCalc_Reals/formcalc_reals.a ~/gfortran/FormCalc_$2/"
  cp ./$2/FormCalc_Reals/formcalc_reals.a ~/gfortran/FormCalc_$2/
  echo "cp ./$2/FormCalc_Virtuals/formcalc_virtuals.a ~/gfortran/FormCalc_$2/"
  cp ./$2/FormCalc_Virtuals/formcalc_virtuals.a ~/gfortran/FormCalc_$2/
elif [[ "$1" == "backup-all" ]]; then
  cp ./Tools/*.a ~/gfortran/
  for x in "neuIneuJ+jet" "neuIchaJ+jet" "chaIchaJ+jet"; do
    echo "cp ./$x/FormCalc_Reals/formcalc_reals.a ~/gfortran/FormCalc_$x/"
    cp ./$x/FormCalc_Reals/formcalc_reals.a ~/gfortran/FormCalc_$x/
    echo "cp ./$x/FormCalc_Virtuals/formcalc_virtuals.a ~/gfortran/FormCalc_$x/"
    cp ./$x/FormCalc_Virtuals/formcalc_virtuals.a ~/gfortran/FormCalc_$x/
  done
elif [[ "$1" == "delete" ]]; then
  echo "rm -f ./Tools/*.a"
  rm -f ./Tools/*.a
  echo "rm -f ./$2/FormCalc_Reals/formcalc_reals.a"
  rm -f ./$2/FormCalc_Reals/formcalc_reals.a
  echo "rm -f ./$2/FormCalc_Virtuals/formcalc_virtuals.a"
  rm -f ./$2/FormCalc_Virtuals/formcalc_virtuals.a
else
  echo "usage: ./libraries.sh copy|copy-all|backup|backup-all|delete <process dir>"
  exit -1
fi
