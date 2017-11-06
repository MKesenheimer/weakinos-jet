#!/bin/bash

# This scripts provides a faster and simpler way to compile all sub programs at once.

make pastegnudata

if [[ $1 != -j* ]]; then
  J=-j4
elif [[ $1 == -j* ]]; then
  J=$1
fi

if [[ "$1" == "libs" ]] || [[ "$2" == "libs" ]]; then
  cd ./chaIchaJ+jet && make $J libs && make $J all && make $J lhef_analysis main-PYTHIA-lhef
  cd ../neuIchaJ+jet && make $J libs && make $J all && make $J lhef_analysis main-PYTHIA-lhef
  cd ../neuIneuJ+jet && make $J libs && make $J all && make $J lhef_analysis main-PYTHIA-lhef
elif [[ $1 == -j* ]]; then
  cd ./chaIchaJ+jet && make $J all && make $J lhef_analysis main-PYTHIA-lhef
  cd ../neuIchaJ+jet && make $J all && make $J lhef_analysis main-PYTHIA-lhef
  cd ../neuIneuJ+jet && make $J all && make $J lhef_analysis main-PYTHIA-lhef
else
  cd ./chaIchaJ+jet && make $@
  cd ../neuIchaJ+jet && make $@
  cd ../neuIneuJ+jet && make $@
fi
