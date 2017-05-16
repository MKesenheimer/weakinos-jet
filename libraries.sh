#!/bin/bash

if [[ "$2" == "" ]]; then
  echo "usage: ./libraries.sh copy|backup|delete <process dir>"
  exit -1
fi


if [[ "$1" == "copy" ]]; then
  cp ~/gfortran/*.a ./Tools/
  cp ~/gfortran/FormCalc_$2/formcalc_reals.a ./$2/FormCalc_Reals
  cp ~/gfortran/FormCalc_$2/formcalc_virtuals.a ./$2/FormCalc_Virtuals
elif [[ "$1" == "backup" ]]; then
  #cp ./Tools/*.a ~/gfortran/
  cp ./$2/FormCalc_Reals/formcalc_reals.a ~/gfortran/FormCalc_$2/
  cp ./$2/FormCalc_Virtuals/formcalc_virtuals.a ~/gfortran/FormCalc_$2/
elif [[ "$1" == "delete" ]]; then
  rm -f ./Tools/*.a
  rm -f ./$2/FormCalc_Reals/formcalc_reals.a
  rm -f ./$2/FormCalc_Virtuals/formcalc_virtuals.a
else
  echo "usage: ./libraries.sh copy|backup|delete <process dir>"
  exit -1
fi
