#!/bin/bash

if ([[ $# -ne 2 ]] && [[ $# -ne 3 ]]); then
  echo "Usage: ./canceljob.sh <name> <end>"
  echo "Usage: ./canceljob.sh <name> <start> <end>"
  exit 0
fi

if [[ $# -eq 3 ]]; then
  name=$1
  nstart=$2
  nend=$3
elif [[ $# -eq 2 ]]; then
  name=$1
  nstart=1
  nend=$2
fi

for i in `seq $nstart 1 $nend`; do
  echo "cancel job $name$i"
  mjobctl -c $name$i
done
