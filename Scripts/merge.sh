#!/bin/bash
# Copyright (C) Matthias Kesenheimer - All Rights Reserved
# Written by Matthias Kesenheimer <m.kesenheimer@gmx.net>, 2017

# Usage: ./merge.sh <directory> [<grid iteration>]
WORKINGDIR=${PWD}
RUNDIR=$WORKINGDIR/$1

# grid iteration
if [ "$2" = "" ]; then
  IT=4
else
  IT=$2
fi

# combined results for stage 2
echo ""
echo "Combined results for stage 2:"
rm -f $RUNDIR/pwg-st2-combined-stat.dat
$WORKINGDIR/merge-pwg-stat $(ls $RUNDIR/pwg-st2-*-stat.dat) > $RUNDIR/pwg-st2-combined-stat.dat
cat $RUNDIR/pwg-st2-combined-stat.dat

# merge the event files
if [ "$(ls $RUNDIR/pwgevents-*.lhe 2>/dev/null)" != "" ]; then
  echo ""
  echo "Merging event files..."
  zcat -f $RUNDIR/pwgevents-*.lhe | grep -v "/LesHouchesEvents" > $RUNDIR/pwgevents.lhe
  echo "</LesHouchesEvents>" >> $RUNDIR/pwgevents.lhe
fi
#if [ -e "$RUNDIR/pwgevents.lhe" ]; then
#  echo "merged event files succesfully, deleting old event files..."
#  find $RUNDIR -type f -name "pwgevents-*" -exec rm -f '{}' \;
#fi
# merge the NLO top files
echo ""
echo "Merging top files..."

rm -f $RUNDIR/pwg-NLO.top
if [ "$(ls $RUNDIR/pwg-*-NLO.top 2>/dev/null)" != "" ]; then
  echo "-> pwg-NLO.top"
  cd $RUNDIR && ../merge-data 1 $(ls $RUNDIR/pwg-*-NLO.top) && mv fort.12 pwg-NLO.top
fi

rm -f $RUNDIR/pwgpwhgalone.top
if [ "$(ls $RUNDIR/pwgpwhgalone*.top 2>/dev/null)" != "" ]; then
  echo "-> pwgpwhgalone.top"
  cd $RUNDIR && ../merge-data 1 $(ls $RUNDIR/pwgpwhgalone*.top) && mv fort.12 pwgpwhgalone.top
fi

rm -f $RUNDIR/pwg-xg$IT-btlgrid.top
if [ "$(ls $RUNDIR/pwg-xg$IT-*-btlgrid.top 2>/dev/null)" != "" ]; then
  echo "-> pwg-xg$IT-btlgrid.top"
  cd $RUNDIR && ../merge-data 1 $(ls $RUNDIR/pwg-xg$IT-*-btlgrid.top) && mv fort.12 pwg-xg$IT-btlgrid.top
fi

rm -f $RUNDIR/pwg-xg$IT-osresgrid.top
if [ "$(ls $RUNDIR/pwg-xg$IT-*-osresgrid.top 2>/dev/null)" != "" ]; then
  echo "-> pwg-xg$IT-osresgrid.top"
  cd $RUNDIR && ../merge-data 1 $(ls $RUNDIR/pwg-xg$IT-*-osresgrid.top) && mv fort.12 pwg-xg$IT-osresgrid.top
fi

rm -f $RUNDIR/pwg-xg$IT-rmngrid.top
if [ "$(ls $RUNDIR/pwg-xg$IT-*-rmngrid.top 2>/dev/null)" != "" ]; then
  echo "-> pwg-xg$IT-rmngrid.top"
  cd $RUNDIR && ../merge-data 1 $(ls $RUNDIR/pwg-xg$IT-*-rmngrid.top) && mv fort.12 pwg-xg$IT-rmngrid.top
fi
echo "done."
