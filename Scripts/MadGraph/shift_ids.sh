#!/bin/bash
shift=114

rm -rf renamed
mkdir renamed
for f in $(ls leshouches_R_*.inc); do
   id=$(echo "$f" | sed -n "s/leshouches_R_\([0-9]*\).inc/\1/p")
   newid=$((10#$id+10#$shift))
   echo "leshouches_R_$id.inc -> leshouches_R_$newid.inc"
   cp $f renamed/leshouches_R_$newid.inc
done

cp sreal_proc.f renamed/sreal_proc.f
for f in $(ls realmtrx_*.f); do
   id=$(echo "$f" | sed -n "s/realmtrx_\([0-9]*\).f/\1/p")
   newid=$((10#$id+10#$shift))
   echo "realmtrx_$id.f -> realmtrx_$newid.f"
   cp $f renamed/realmtrx_$newid.f
   
   sed -i -e "s/_$id/_$newid/g" renamed/realmtrx_$newid.f
   sed -i -e "s/_$id/_$newid/g" renamed/sreal_proc.f
   sed -i -e "s/2$id/2$newid/g" renamed/sreal_proc.f
done