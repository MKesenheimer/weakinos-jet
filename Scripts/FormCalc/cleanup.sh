#!/bin/bash

if [ "$1" == "" ]; then
  rm -r ../../neuIneuJ+jet/FormCalc_Virtuals/include/* ../../neuIneuJ+jet/FormCalc_Virtuals/squaredME/*
  rm -r ../../neuIneuJ+jet/FormCalc_Virtuals/RenConst/*
else
  rm -r ../../neuIneuJ+jet/FormCalc_Virtuals/include/"$1"* ../../neuIneuJ+jet/FormCalc_Virtuals/squaredME/"$1"*
  rm -r ../../neuIneuJ+jet/FormCalc_Virtuals/RenConst/"$1"*
fi
#$(ls -1 -d */)
