#!/bin/bash
# Usage: ./merge.sh <directory>
WORKINGDIR=${PWD}
RUNDIR=$WORKINGDIR/$1
# grid iteration
IT=4

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
  #for f in $(ls $RUNDIR/pwgevents-*.lhe); do
  #  echo $f
  zcat -f $RUNDIR/pwgevents-*.lhe | grep -v "/LesHouchesEvents" > $RUNDIR/pwgevents.lhe
  echo "</LesHouchesEvents>" >> $RUNDIR/pwgevents.lhe
  #done
fi
#if [ -e "$RUNDIR/pwgevents.lhe" ]; then
#  echo "merged event files succesfully, deleting old event files..."
#  find $RUNDIR -type f -name "pwgevents-*" -exec rm -f '{}' \;
#fi
# merge the NLO top files
echo ""
echo "Merging top files..."
rm -f $RUNDIR/pwg-NLO.top
cd $RUNDIR && ../merge-data 1 $(ls $RUNDIR/pwg-*-NLO.top) && mv fort.12 pwg-NLO.top

rm -f $RUNDIR/pwgpwhgalone.top
cd $RUNDIR && ../merge-data 1 $(ls $RUNDIR/pwgpwhgalone*.top) && mv fort.12 pwgpwhgalone.top

rm -f $RUNDIR/pwg-xg$IT-btlgrid.top
cd $RUNDIR && ../merge-data 1 $(ls $RUNDIR/pwg-xg$IT-*-btlgrid.top) && mv fort.12 pwg-xg$IT-btlgrid.top

rm -f $RUNDIR/pwg-xg$IT-osresgrid.top
cd $RUNDIR && ../merge-data 1 $(ls $RUNDIR/pwg-xg$IT-*-osresgrid.top) && mv fort.12 pwg-xg$IT-osresgrid.top

echo "done."
