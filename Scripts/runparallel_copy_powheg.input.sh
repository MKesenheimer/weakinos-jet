#!/bin/bash
#
# Examples:
#
# $ ./runparallel.sh -d testrun_1 -e pwhg_main_nixj
# -> runs pwhg_main_nixj in testrun_1 on 4 cores (default)
#
#
# $ ./runparallel.sh -c -d testrun_1 -e pwhg_main_nixj --itmx1 4 \
# --itmx2 4 --itmx1osres 6 --itmx2osres 8 --ncall1 2000 --ncall2 2000 \
# --ncall1osres 20000 --ncall2osres 20000
# -> runs pwhg_main_nixj in testrun_1 on 4 cores and overwrites some powheg
#    parameters in powheg.input
#
#
# $ ./runparallel.sh -g -c -e pwhg_main_nixj -d run_wevents --genevents > log_wevents
# -> copies the folder testrun_clean (and renames it to run_wevents),
#    generates events (nubound and nevents in powheg.input must be greater than zero,
#    or use --nevents and --nubound to set the numbers)
#
#
# $ softpoint.x sugra --m0=125 --m12=200 --a0=-300 --tanBeta=10 > ./testrun_clean/input.slha 
# $ ./runparallel.sh -g -c -e pwhg_main_nixj --lopdf 10042 --slha input.slha -d testrun_1
# -> use softpoint (must be installed separately) to generate a slha input file which
#    is then processed with powheg.
#    Copies the folder testrun_clean (and renames it to testrun_1) and proceeds to
#    calculate the LO cross section on 4 cores.


# Functions
function info {
   echo "Automatically runs POWHEG in parallel mode"
}

function usage {
cat <<EOM
Usage: $(basename $0) [OPTIONS]
Mandatory arguments:
  -d, --directory <name>   specify the subdirectory relative to working
                           directory where to execute the program and where 
                           to look for powheg.input
  -e, --executable <name>  the relative path of the executable
  
Optional arguments:
  -h, --help               print this help message
  -i, --info               show informations
  -p, --parallel <n>       number of parallel jobs to submit
  --ncall1 <n>             overwrite the parameter ncall1 in powheg.input
  --ncall2 <n>             overwrite the parameter ncall2 in powheg.input
  --ncall1osres <n>        overwrite the parameter ncall1osres in powheg.input
  --ncall2osres <n>        overwrite the parameter ncall1osres in powheg.input
  --itmx1 <n>              overwrite the parameter itmx1 in powheg.input
  --itmx2 <n>              overwrite the parameter itmx2 in powheg.input
  --itmx1osres <n>         overwrite the parameter itmx1osres in powheg.input
  --itmx2osres <n>         overwrite the parameter itmx2osres in powheg.input
  -c, --clean              clean directory before running POWHEG
  -o, --offset <n>         supply an offset for extracting the seeds from seeds.dat
  --genevents              generate events
  --nubound <n>            upper bound number
  --nevents <n>            number of events
  --usemsub                use the submitting system msub
  --usecondor              use condor (experimental)
  -s, --slha <name>        name of the slha file you want to use
  --lhapath <path,rd>      path of LHAPDF (in case condor can't access the file 
                           system and needs a local copy of the LHA grid files)
                           if "--lhapath rd" the current run directory will be used
  --lopdf <n>              only LO calculation with LO pdf and LHA number n
  -g, --genfolder          generate a new run directory with default input files 
                           (the directory "testrun_clean" is needed)
  --fin1 <n>               PDG number of first final particle
  --fin2 <n>               PDG number of second final particle
  --dec1 <n>               PDG number of first decay particle
  --dec2 <n>               PDG number of second decay particle
  --dec3 <n>               PDG number of third decay particle
  --dec4 <n>               PDG number of fourth decay particle
  --mur <n>                set renscfact
  --muf <n>                set facscfact
  --ewi <n>                set the regulator ewi
  --merge                  merge the event files and delete the old ones
  --checklimits            check the soft and collinear limits during the NLO run
  --w[i] <n>               the walltime in seconds of the i-th stage (f.e. -w1 60)
  --fakevirt               use fake virtuals in all calculations
  --name <name>            job is identified with a name for msub
  --time                   meassure execution time
EOM
   exit 0
}

function overwrite_powheg_var {
  sed -i -e "s/$1 /#$1 /g" powheg.input
  echo "$1 $2" >> powheg.input
}

function comment_powheg_var {
  sed -i -e "s/$1 /#$1 /g" powheg.input
}

# make sure grep uses no other arguments (for example via aliases)
function read_powheg_var {
  grep "$1" powheg.input | sed 's/[^0-9]//g' | head -1
}

function checksum {
  if type md5 1>/dev/null 2>/dev/null; then
    md5 "$@"
  elif type md5sum 1>/dev/null 2>/dev/null; then
    md5sum "$@"
  elif type sha1sum 1>/dev/null 2>/dev/null; then
    sha1sum "$@"
  elif type sha256sum 1>/dev/null 2>/dev/null; then
    sha256sum "$@"
  else
    echo $(date +%s)
  fi
}

# if there are no arguments print help
if [ $# -lt 1 ] 
  then usage
fi

#default values
JOBS=4
CLEAN=false
NSEEDOFFSET=0
NICENESS=10
USEMSUB=false
USECONDOR=false
GENEVENTS=false
GENFOLGDER=false
MERGE=false
ARG1=""
CHECKLIM=false
WALLTIME1=3600
WALLTIME2=43200
WALLTIME3=1800
WALLTIME4=43200
FAKEVIRT=false
NAME=""
LHAPATH1=""
TIME=""

# go through the options
while [[ $# -gt 0 ]]; do
KEY="$1"
case $KEY in
    -h|--help)
        usage
        shift # past argument
        ;;
    -i|--info)
        info
        shift
        ;;
    -d|--directory)
        RUNDIR="$2"
        shift
        shift
        ;;
    -e|--executable)
        EXE="$2"
        shift
        shift
        ;;
    -p|--parallel)
        JOBS="$2"
        shift
        shift
        ;;
    -o|--offset)
        NSEEDOFFSET="$2"
        shift
        shift
        ;;
    -c|--clean)
        CLEAN=true
        shift
        ;;
    --ncall1)
        NCALL1="$2"
        shift
        shift
        ;;
    --ncall2)
        NCALL2="$2"
        shift
        shift
        ;;
    --ncall1osres)
        NCALL1OSRES="$2"
        shift
        shift
        ;;
    --ncall2osres)
        NCALL2OSRES="$2"
        shift
        shift
        ;;
    --itmx1)
        ITMX1="$2"
        shift
        shift
        ;;
    --itmx2)
        ITMX2="$2"
        shift
        shift
        ;;
    --itmx1osres)
        ITMX1OSRES="$2"
        shift
        shift
        ;;
    --itmx2osres)
        ITMX2OSRES="$2"
        shift
        shift
        ;;
    -s|--slha)
        SLHA="$2"
        shift
        shift
        ;;
    --lhapath)
        LHAPATH1="$2"
        shift
        shift
        ;;
    --lopdf)
        LOPDF="$2"
        shift
        shift
        ;;
    --genevents)
        GENEVENTS=true
        shift
        ;;
    --nubound)
        GENEVENTS=true
        NUBOUND="$2"
        shift
        shift
        ;;
    --nevents)
        GENEVENTS=true
        NEVENTS="$2"
        shift
        shift
        ;;
    --fin1)
        FIN1="$2"
        shift
        shift
        ;;
    --fin2)
        FIN2="$2"
        shift
        shift
        ;;
    --dec1)
        DEC1="$2"
        shift
        shift
        ;;
    --dec2)
        DEC2="$2"
        shift
        shift
        ;;
    --dec3)
        DEC3="$2"
        shift
        shift
        ;;
    --dec4)
        DEC4="$2"
        shift
        shift
        ;;
    --muf)
        FACSCFACT="$2"
        shift
        shift
        ;;
    --mur)
        RENSCFACT="$2"
        shift
        shift
        ;;
    --ewi)
        EWI="$2"
        shift
        shift
        ;;
    --usemsub)
        USEMSUB=true
        shift
        ;;
     --usecondor)
        USECONDOR=true
        shift
        ;;
     --w1)
        WALLTIME1="$2"
        shift
        shift
        ;;
     --w2)
        WALLTIME2="$2"
        shift
        shift
        ;;
     --w3)
        WALLTIME3="$2"
        shift
        shift
        ;;
     --w4)
        WALLTIME4="$2"
        shift
        shift
        ;;
     --name)
        NAME="$2"
        shift
        shift
        ;;
    --merge)
        MERGE=true
        shift
        ;;
    -g|--genfolder)
        GENFOLGDER=true
        shift
        ;;
    --checklimits)
        CHECKLIM=true
        shift
        ;;
    --fakevirt)
        FAKEVIRT=true
        shift
        ;;
    --time)
        TIME=time
        shift
        ;;
    *)
        usage    # unknown option
        ;;
esac
done

# check if RUNDIR is set
if [ "$RUNDIR" == "" ]; then
   echo "Error: no directory specified."
   usage
fi

# directories
WORKINGDIR=${PWD}
RUNDIR=$WORKINGDIR/$RUNDIR
EXEPATH=$WORKINGDIR/$EXE

#generate a new run directory
if [ "$GENFOLGDER" = true ]; then
   # if the folder already exists
   if [ -d "$RUNDIR" ]; then
      echo "Warning: old directory $RUNDIR will be deleted." 
      read -n1 -r -p "Type [y] to continue... " KEY
      if [ "$KEY" = 'y' ]; then
         rm -r $RUNDIR
      else
         echo "Script stopped."
         exit 0
      fi  
   fi
   if [ ! -d "testrun_clean" ]; then
      echo ""
      echo "Error: directory 'testrun_clean' does not exist."
      exit 0
   fi
   cp -r testrun_clean/ $RUNDIR
   cp $RUNDIR/powheg_clean.input $RUNDIR/powheg.input
fi

# check if EXE is set
if [ "$EXE" == "" ]; then
   echo "Error: no executable specified."
   usage
fi

# generate an individual identifier for every run
IDENT=$(echo -n "$RUNDIR" | checksum | cut -c1-8)
if [ "$NAME" == "" ]; then
   NAME=$(basename ${RUNDIR})
else
   NAME=${NAME}
fi
echo ""
echo "Identifier is $IDENT"
echo "Name is $NAME"

# change into the directory where to execute pwhg_main
cd $RUNDIR

# clean up the directory
if [ "$CLEAN" = true ]; then
   find $RUNDIR ! \( -name '*.slha' -o -name '*.input' -o -name 'pwgseeds.dat' -o -name '*.LHgrid' \) -type f -exec rm -f {} +
   cp $RUNDIR/powheg_clean.input $RUNDIR/powheg.input
fi
#exit 0

# append to powheg.input
echo "" >> powheg.input
echo "" >> powheg.input
echo "# Modified by runparallel.sh:" >> powheg.input

#default parameters in powheg.input
overwrite_powheg_var "use-old-grid" 1
overwrite_powheg_var "use-old-ubound" 1
comment_powheg_var "iseed"
overwrite_powheg_var "manyseeds" 1
overwrite_powheg_var "testplots" 1
overwrite_powheg_var "softtest" 0
overwrite_powheg_var "colltest" 0

# sed magic
if [ "$NCALL1" != "" ]; then
   overwrite_powheg_var "ncall1" $NCALL1
fi  

if [ "$NCALL2" != "" ]; then
   overwrite_powheg_var "ncall2" $NCALL2
fi

if [ "$NCALL1OSRES" != "" ]; then
   overwrite_powheg_var "ncall1osres" $NCALL1OSRES
fi  

if [ "$NCALL2OSRES" != "" ]; then
   overwrite_powheg_var "ncall2osres" $NCALL2OSRES
fi

if [ "$ITMX1" != "" ]; then
   overwrite_powheg_var "itmx1" $ITMX1
fi  

if [ "$ITMX2" != "" ]; then
   overwrite_powheg_var "itmx2" $ITMX2
fi

if [ "$ITMX1OSRES" != "" ]; then
   overwrite_powheg_var "itmx1osres" $ITMX1OSRES
fi  

if [ "$ITMX2OSRES" != "" ]; then
   overwrite_powheg_var "itmx2osres" $ITMX2OSRES
fi

if [ "$SLHA" != "" ]; then
   overwrite_powheg_var "SLHA" \'"$SLHA"\'
fi

if [ "$LOPDF" != "" ]; then
   overwrite_powheg_var "lhans1" $LOPDF
   overwrite_powheg_var "lhans2" $LOPDF
   overwrite_powheg_var "bornonly" 1
   overwrite_powheg_var "LOevents" 1
fi

if [ "$FIN1" != "" ]; then
   overwrite_powheg_var "fin1" "$FIN1"
fi

if [ "$FIN2" != "" ]; then
   overwrite_powheg_var "fin2" "$FIN2"
fi

if [ "$DEC1" != "" ]; then
   overwrite_powheg_var "dec1" "$DEC1"
fi

if [ "$DEC2" != "" ]; then
   overwrite_powheg_var "dec2" "$DEC2"
fi

if [ "$DEC3" != "" ]; then
   overwrite_powheg_var "dec3" "$DEC3"
fi

if [ "$DEC4" != "" ]; then
   overwrite_powheg_var "dec4" "$DEC4"
fi

if [ "$RENSCFACT" != "" ]; then
   overwrite_powheg_var "renscfact" "$RENSCFACT"
fi

if [ "$FACSCFACT" != "" ]; then
   overwrite_powheg_var "facscfact" "$FACSCFACT"
fi

if [ "$EWI" != "" ]; then
   overwrite_powheg_var "ewi" "$EWI"
fi

# back up the old parameters
NEVENTSOLD=$(read_powheg_var "numevts")
NUBOUNDOLD=$(read_powheg_var "nubound")

# generate the scripts to start the POWHEG-main executable

# STEP 1a
echo "" >> $RUNDIR/powheg.input
echo "#Stage 1a: Generating Grids, iteration 1" >> $RUNDIR/powheg.input
overwrite_powheg_var "fakevirtuals" 1
overwrite_powheg_var "parallelstage" 1
overwrite_powheg_var "xgriditeration" 1
cp $RUNDIR/powheg.input $RUNDIR/st1a-powheg.input
cp $RUNDIR/pwgseeds.dat $RUNDIR/st1a-seeds.dat 

# at least \$1 or \$ARG1 is defined (msub sets ARG1)
cat <<EOM > $WORKINGDIR/run_st1a_${IDENT}.sh
#!/bin/bash
cd $RUNDIR
#cp $RUNDIR/st1a-powheg.input $RUNDIR/powheg.input
$EXEPATH < <(printf "%s\n%s\n" "st1a" "\$1" "\$ARG1")
EOM
chmod +x $WORKINGDIR/run_st1a_${IDENT}.sh

# STEP 1b
echo "" >> $RUNDIR/powheg.input
echo "#Stage 1b: Generating Grids, iteration 2" >> $RUNDIR/powheg.input
overwrite_powheg_var "fakevirtuals" 1
overwrite_powheg_var "parallelstage" 1
overwrite_powheg_var "xgriditeration" 2
cp $RUNDIR/powheg.input $RUNDIR/st1b-powheg.input
cp $RUNDIR/pwgseeds.dat $RUNDIR/st1b-seeds.dat 

cat <<EOM > $WORKINGDIR/run_st1b_${IDENT}.sh
#!/bin/bash
cd $RUNDIR
#cp $RUNDIR/st1b-powheg.input $RUNDIR/powheg.input
$EXEPATH < <(printf "%s\n%s\n" "st1b" "\$1" "\$ARG1")
EOM
chmod +x $WORKINGDIR/run_st1b_${IDENT}.sh

# STEP 2
echo "" >> $RUNDIR/powheg.input
echo "#Stage 2: NLO run" >> $RUNDIR/powheg.input
if [ "$FAKEVIRT" = false ]; then
  overwrite_powheg_var "fakevirtuals" 0
fi
#overwrite_powheg_var "xgriditeration" 1
overwrite_powheg_var "parallelstage" 2
overwrite_powheg_var "numevts" 0
overwrite_powheg_var "nubound" 0
if [ "$CHECKLIM" = true ]; then
  overwrite_powheg_var "softtest" 1
  overwrite_powheg_var "colltest" 1
fi
cp $RUNDIR/powheg.input $RUNDIR/st2-powheg.input
cp $RUNDIR/pwgseeds.dat $RUNDIR/st2-seeds.dat 

cat <<EOM > $WORKINGDIR/run_st2_${IDENT}.sh
#!/bin/bash
cd $RUNDIR
#cp $RUNDIR/st2-powheg.input $RUNDIR/powheg.input
$EXEPATH < <(printf "%s\n%s\n" "st2" "\$1" "\$ARG1")
EOM
chmod +x $WORKINGDIR/run_st2_${IDENT}.sh

# if the user wants to generate events
if [ "$GENEVENTS" = true ]; then

# STEP 3
echo "" >> $RUNDIR/powheg.input
echo "#Stage 3: Upper bound" >> $RUNDIR/powheg.input
if [ "$NUBOUND" != "" ]; then
  overwrite_powheg_var "nubound" $NUBOUND
else
  overwrite_powheg_var "nubound" $NUBOUNDOLD
fi
overwrite_powheg_var "parallelstage" 3
cp $RUNDIR/powheg.input $RUNDIR/st3-powheg.input
cp $RUNDIR/pwgseeds.dat $RUNDIR/st3-seeds.dat 

cat <<EOM > $WORKINGDIR/run_st3_${IDENT}.sh
#!/bin/bash
cd $RUNDIR
#cp $RUNDIR/st3-powheg.input $RUNDIR/powheg.input
$EXEPATH < <(printf "%s\n%s\n" "st3" "\$1" "\$ARG1")
EOM
chmod +x $WORKINGDIR/run_st3_${IDENT}.sh

# STEP 4
echo "" >> $RUNDIR/powheg.input
echo "#Stage 4: Events" >> $RUNDIR/powheg.input
if [ "$NEVENTS" != "" ]; then
  overwrite_powheg_var "numevts" $NEVENTS
else
  overwrite_powheg_var "numevts" $NEVENTSOLD
fi
overwrite_powheg_var "parallelstage" 4
cp $RUNDIR/powheg.input $RUNDIR/st4-powheg.input
cp $RUNDIR/pwgseeds.dat $RUNDIR/st4-seeds.dat 

cat <<EOM > $WORKINGDIR/run_st4_${IDENT}.sh
#!/bin/bash
cd $RUNDIR
#cp $RUNDIR/st4-powheg.input $RUNDIR/powheg.input
$EXEPATH < <(printf "%s\n%s\n" "st4" "\$1" "\$ARG1")
EOM
chmod +x $WORKINGDIR/run_st4_${IDENT}.sh
fi

# additional environmental variables
if [ "$LHAPATH1" = "rd" ]; then
  LHAPATH=$RUNDIR
  export LHAPATH
elif [ "$LHAPATH1" != "" ]; then
  LHAPATH=$LHAPATH1
  export LHAPATH
fi

# now remove the powheg.input file
rm $RUNDIR/powheg.input

# generate and run the run.sh script
if [ "$USEMSUB" = false ] && [ "$USECONDOR" = false ]; then
cat <<EOM > $WORKINGDIR/run_${IDENT}.sh
#!/bin/bash
echo ""
echo "Stage 1a: Generating Grids, iteration 1"
echo "  starting $JOBS job(s)..."
for i in \`seq 1 $JOBS\`; do
  NSEED=\$((\$i+$NSEEDOFFSET))
  echo "  job \$i with nseed \$NSEED"
  nohup $TIME nice -n $NICENESS $WORKINGDIR/run_st1a_${IDENT}.sh \$NSEED > $RUNDIR/powheg_st1a_\${NSEED}.output 2>&1 &
done
for job in \`jobs -p\`; do
    wait \$job
    echo "  job with pid=\$job finished"
done

echo ""
echo "Stage 1b: Generating Grids, iteration 2"
echo "  starting $JOBS job(s)..."
for i in \`seq 1 $JOBS\`; do
   NSEED=\$((i+$NSEEDOFFSET))
   echo "  job \$i with nseed \$NSEED"
   nohup $TIME nice -n $NICENESS $WORKINGDIR/run_st1b_${IDENT}.sh \$NSEED > $RUNDIR/powheg_st1b_\${NSEED}.output 2>&1 &
done
for job in \`jobs -p\`; do
    wait \$job
    echo "  job with pid=\$job finished"
done

echo ""
echo "Stage 2: NLO run"
echo "  starting $JOBS job(s)..."
for i in \`seq 1 $JOBS\`; do
   NSEED=\$((i+$NSEEDOFFSET))
   echo "  job \$i with nseed \$NSEED"
   nohup $TIME nice -n $NICENESS $WORKINGDIR/run_st2_${IDENT}.sh \$NSEED > $RUNDIR/powheg_st2_\${NSEED}.output 2>&1 &
done
for job in \`jobs -p\`; do
    wait \$job
    echo "  job with pid=\$job finished"
done
EOM

if [ "$GENEVENTS" = true ]; then
cat <<EOM >> $WORKINGDIR/run_${IDENT}.sh
echo ""
echo "Stage 3: Upper bound"
echo "  starting $JOBS job(s)..."
for i in \`seq 1 $JOBS\`; do
   NSEED=\$((i+$NSEEDOFFSET))
   echo "  job \$i with nseed \$NSEED"
   nohup $TIME nice -n $NICENESS $WORKINGDIR/run_st3_${IDENT}.sh \$NSEED > $RUNDIR/powheg_st3_\${NSEED}.output 2>&1 &
done
for job in \`jobs -p\`; do
    wait \$job
    echo "  job with pid=\$job finished"
done

echo ""
echo "Stage 4: Events"
echo "  starting $JOBS job(s)..."
for i in \`seq 1 $JOBS\`; do
   NSEED=\$((i+$NSEEDOFFSET))
   echo "  job \$i with nseed \$NSEED"
   nohup $TIME nice -n $NICENESS $WORKINGDIR/run_st4_${IDENT}.sh \$NSEED > $RUNDIR/powheg_st4_\${NSEED}.output 2>&1 &
done
for job in \`jobs -p\`; do
    wait \$job
    echo "  job with pid=\$job finished"
done
EOM
fi #if GENEVENTS

if [ "$MERGE" = true ]; then
cat <<EOM >> $WORKINGDIR/run_${IDENT}.sh
# merge the event files
if [ "\$(ls $RUNDIR/pwgevents-*.lhe 2>/dev/null)" != "" ]; then
  zcat $RUNDIR/pwgevents-*.lhe | grep -v "/LesHouchesEvents" > $RUNDIR/pwgevents.lhe
  echo "</LesHouchesEvents>" >> $RUNDIR/pwgevents.lhe
fi
#if [ -e "$RUNDIR/pwgevents.lhe" ]; then
#  echo "merged event files succesfully, deleting old event files..."
#  find $RUNDIR -type f -name "pwgevents-*" -exec rm -f '{}' \;
#fi

# merge the NLO top files
rm -f $RUNDIR/pwg-NLO.top
cd $RUNDIR && ../merge-data 1 \$(ls $RUNDIR/pwg-*-NLO.top) && mv fort.12 pwg-NLO.top
rm -f $RUNDIR/pwgpwhgalone.top
cd $RUNDIR && ../merge-data 1 \$(ls $RUNDIR/pwgpwhgalone*.top) && mv fort.12 pwgpwhgalone.top
rm -f $RUNDIR/pwg-xg2-btlgrid.top
cd $RUNDIR && ../merge-data 1 \$(ls $RUNDIR/pwg-xg2-*-btlgrid.top) && mv fort.12 pwg-xg2-btlgrid.top
rm -f $RUNDIR/pwg-xg2-osresgrid.top
cd $RUNDIR && ../merge-data 1 \$(ls $RUNDIR/pwg-xg2-*-osresgrid.top) && mv fort.12 pwg-xg2-osresgrid.top

# combined results for stage 2
rm -f $RUNDIR/pwg-st2-combined-stat.dat
$WORKINGDIR/merge-pwg-stat \$(ls $RUNDIR/pwg-st2-*-stat.dat) > $RUNDIR/pwg-st2-combined-stat.dat
cat $RUNDIR/pwg-st2-combined-stat.dat
EOM
fi #if MERGE

chmod +x $WORKINGDIR/run_${IDENT}.sh
$WORKINGDIR/run_${IDENT}.sh

# if finished delete the old files
rm -f $WORKINGDIR/run_st1a_${IDENT}.sh
rm -f $WORKINGDIR/run_st1b_${IDENT}.sh
rm -f $WORKINGDIR/run_st2_${IDENT}.sh
rm -f $WORKINGDIR/run_st3_${IDENT}.sh
rm -f $WORKINGDIR/run_st4_${IDENT}.sh
rm -f $WORKINGDIR/run_${IDENT}.sh
#rm -f $RUNDIR/powheg_st*.input
fi

# additional environmental variables for msub and condor
ADDVAR=""
if [ "$LHAPATH1" = "rd" ]; then
  ADDVAR+=",LHAPATH=$RUNDIR"
elif [ "$LHAPATH1" != "" ]; then
  ADDVAR+=",LHAPATH=$LHAPATH1"
fi

# if the user wants to use msub:
if [ "$USEMSUB" = true ]; then
cat <<EOM > $WORKINGDIR/runmsub_${IDENT}.sh
#!/bin/bash
echo ""
echo "Stage 1a: Generating Grids, iteration 1"
echo "  submitting $JOBS job(s)..."
dependIDs1a=()
for i in \`seq 1 $JOBS\`; do
  NSEED=\$((\$i+$NSEEDOFFSET))
  job[\$i]=\$(msub -N ${NAME}_st1a_\${NSEED} -l walltime=$WALLTIME1 -v ARG1=\${NSEED}$ADDVAR -o $RUNDIR/powheg_st1a_\${NSEED}.output -e $RUNDIR/powheg_st1a_\${NSEED}.error $WORKINGDIR/run_st1a_${IDENT}.sh | grep -v -e '^$')
  echo "  job \$i with nseed \$NSEED and ID \${job[\$i]}"
  dependIDs1a[\$i]=\${job[\$i]}
done

echo ""
echo "Stage 1b: Generating Grids, iteration 2"
echo "  submitting $JOBS job(s)..."
dependIDs1b=()
for i in \`seq 1 $JOBS\`; do
  NSEED=\$((\$i+$NSEEDOFFSET))
  job[\$i]=\$(msub -N ${NAME}_st1b_\${NSEED} -l walltime=$WALLTIME1,depend=afterok:\${dependIDs1a[\$i]} -v ARG1=\${NSEED}$ADDVAR -o $RUNDIR/powheg_st1b_\${NSEED}.output -e $RUNDIR/powheg_st1b_\${NSEED}.error $WORKINGDIR/run_st1b_${IDENT}.sh | grep -v -e '^$')
  echo "  job \$i with nseed \$NSEED and ID \${job[\$i]}"
  dependIDs1b[\$i]=\${job[\$i]}
done

echo ""
echo "Stage 2: NLO run"
echo "  submitting $JOBS job(s)..."
dependIDs2=()
for i in \`seq 1 $JOBS\`; do
  NSEED=\$((\$i+$NSEEDOFFSET))
  job[\$i]=\$(msub -N ${NAME}_st2_\${NSEED} -l walltime=$WALLTIME2,depend=afterok:\${dependIDs1b[\$i]} -v ARG1=\${NSEED}$ADDVAR -o $RUNDIR/powheg_st2_\${NSEED}.output -e $RUNDIR/powheg_st2_\${NSEED}.error $WORKINGDIR/run_st2_${IDENT}.sh | grep -v -e '^$')
  echo "  job \$i with nseed \$NSEED and ID \${job[\$i]}"
  dependIDs2[\$i]=\${job[\$i]}
done

EOM
if [ "$GENEVENTS" = true ]; then
cat <<EOM >> $WORKINGDIR/runmsub_${IDENT}.sh
echo ""
echo "Stage 3: Upper bound"
echo "  submitting $JOBS job(s)..."
dependIDs3=()
for i in \`seq 1 $JOBS\`; do
  NSEED=\$((\$i+$NSEEDOFFSET))
  job[\$i]=\$(msub -N ${NAME}_st3_\${NSEED} -l walltime=$WALLTIME3,depend=afterok:\${dependIDs2[\$i]} -v ARG1=\${NSEED}$ADDVAR -o $RUNDIR/powheg_st3_\${NSEED}.output -e $RUNDIR/powheg_st3_\${NSEED}.error $WORKINGDIR/run_st3_${IDENT}.sh | grep -v -e '^$')
  echo "  job \$i with nseed \$NSEED and ID \${job[\$i]}"
  dependIDs3[\$i]=\${job[\$i]}
done

echo ""
echo "Stage 4: Events"
echo "  submitting $JOBS job(s)..."
dependIDs4=()
for i in \`seq 1 $JOBS\`; do
  NSEED=\$((\$i+$NSEEDOFFSET))
  job[\$i]=\$(msub -N ${NAME}_st4_\${NSEED} -l walltime=$WALLTIME4,depend=afterok:\${dependIDs3[\$i]} -v ARG1=\${NSEED}$ADDVAR -o $RUNDIR/powheg_st4_\${NSEED}.output -e $RUNDIR/powheg_st4_\${NSEED}.error $WORKINGDIR/run_st4_${IDENT}.sh | grep -v -e '^$')
  echo "  job \$i with nseed \$NSEED and ID \${job[\$i]}"
  dependIDs4[\$i]=\${job[\$i]}
done

EOM
fi #if GENEVENTS

chmod +x $WORKINGDIR/runmsub_${IDENT}.sh
$WORKINGDIR/runmsub_${IDENT}.sh
fi

# if the user wants to use condor:
if [ "$USECONDOR" = true ]; then
cat <<EOM > $WORKINGDIR/runcondor_${IDENT}.sh
#!/bin/bash
echo ""
echo "Stage 1a: Generating Grids, iteration 1"
echo "  submitting $JOBS job(s)..."
dependIDs1a=()
for i in \`seq 1 $JOBS\`; do
  NSEED=\$((\$i+$NSEEDOFFSET))
  job[\$i]=\$(condor_qsub -v ARG1=\${NSEED}$ADDVAR -o $RUNDIR/powheg_st1a_\${NSEED}.output -e $RUNDIR/powheg_st1a_\${NSEED}.error $WORKINGDIR/run_st1a_${IDENT}.sh | sed -r 's/.* ([0-9]*) .*/\1/g')
  echo "  job \$i with nseed \$NSEED and ID \${job[\$i]}"
  dependIDs1a[\$i]=\${job[\$i]}
done

echo ""
echo "Stage 1b: Generating Grids, iteration 2"
echo "  submitting $JOBS job(s)..."
dependIDs1b=()
for i in \`seq 1 $JOBS\`; do
  NSEED=\$((\$i+$NSEEDOFFSET))
  job[\$i]=\$(condor_qsub -hold_jid \${dependIDs1a[\$i]} -v ARG1=\${NSEED}$ADDVAR -o $RUNDIR/powheg_st1b_\${NSEED}.output -e $RUNDIR/powheg_st1b_\${NSEED}.error $WORKINGDIR/run_st1b_${IDENT}.sh | sed -r 's/.* ([0-9]*) .*/\1/g')
  echo "  job \$i with nseed \$NSEED and ID \${job[\$i]}"
  dependIDs1b[\$i]=\${job[\$i]}
done

echo ""
echo "Stage 2: NLO run"
echo "  submitting $JOBS job(s)..."
dependIDs2=()
for i in \`seq 1 $JOBS\`; do
  NSEED=\$((\$i+$NSEEDOFFSET))
  job[\$i]=\$(condor_qsub -hold_jid \${dependIDs1b[\$i]} -v ARG1=\${NSEED}$ADDVAR -o $RUNDIR/powheg_st2_\${NSEED}.output -e $RUNDIR/powheg_st2_\${NSEED}.error $WORKINGDIR/run_st2_${IDENT}.sh | sed -r 's/.* ([0-9]*) .*/\1/g')
  echo "  job \$i with nseed \$NSEED and ID \${job[\$i]}"
  dependIDs2[\$i]=\${job[\$i]}
done

EOM
if [ "$GENEVENTS" = true ]; then
cat <<EOM >> $WORKINGDIR/runcondor_${IDENT}.sh
echo ""
echo "Stage 3: Upper bound"
echo "  submitting $JOBS job(s)..."
dependIDs3=()
for i in \`seq 1 $JOBS\`; do
  NSEED=\$((\$i+$NSEEDOFFSET))
  job[\$i]=\$(condor_qsub -hold_jid \${dependIDs2[\$i]} -v ARG1=\${NSEED}$ADDVAR -o $RUNDIR/powheg_st3_\${NSEED}.output -e $RUNDIR/powheg_st3_\${NSEED}.error $WORKINGDIR/run_st3_${IDENT}.sh | sed -r 's/.* ([0-9]*) .*/\1/g')
  echo "  job \$i with nseed \$NSEED and ID \${job[\$i]}"
  dependIDs3[\$i]=\${job[\$i]}
done

echo ""
echo "Stage 4: Events"
echo "  submitting $JOBS job(s)..."
dependIDs4=()
for i in \`seq 1 $JOBS\`; do
  NSEED=\$((\$i+$NSEEDOFFSET))
  job[\$i]=\$(condor_qsub -hold_jid \${dependIDs3[\$i]} -v ARG1=\${NSEED}$ADDVAR -o $RUNDIR/powheg_st4_\${NSEED}.output -e $RUNDIR/powheg_st4_\${NSEED}.error $WORKINGDIR/run_st4_${IDENT}.sh | sed -r 's/.* ([0-9]*) .*/\1/g')
  echo "  job \$i with nseed \$NSEED and ID \${job[\$i]}"
  dependIDs4[\$i]=\${job[\$i]}
done

EOM
fi #if GENEVENTS

chmod +x $WORKINGDIR/runcondor_${IDENT}.sh
$WORKINGDIR/runcondor_${IDENT}.sh
fi
