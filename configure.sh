#!/bin/sh
# calls the LoopTools and SLHAlib configure scripts

WORKINGDIR=$(pwd)
LT=$WORKINGDIR/Tools/LoopTools-2.12
SLHA=$WORKINGDIR/Tools/SLHALib-2.2
PYTHIA=$WORKINGDIR/Tools/pythia8215
COLLIER=$WORKINGDIR/Tools/COLLIER-1.1

COMPILER=$1
if [ "$1" = "" ]; then
  COMPILER=gfortran
fi

#cd $PYTHIA && ./configure --prefix-lib=$WORKINGDIR/Tools --cxx-common='-O2 -fomit-frame-pointer -ffast-math -Wall -m64 -stdlib=libstdc++ -mmacosx-version-min=10.6 -Qunused-arguments -g -ansi -pedantic -W -Wall -Wshadow -fPIC'
cd $PYTHIA && ./configure --prefix-lib=$WORKINGDIR/Tools
cd $LT && ./configure FC=$COMPILER
cd $SLHA && ./configure FC=$COMPILER
mkdir -p $COLLIER/build
cd $COLLIER/build && cmake -DCMAKE_Fortran_COMPILER=$COMPILER -Dstatic=ON ..