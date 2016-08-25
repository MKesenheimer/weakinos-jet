#!/bin/bash
WORKINGDIR=${PWD}

########################################################################
#                       -*-  Preferences -*-                           #
########################################################################
# Steps for weakinos + jet:
# 1. generate and copy the born or virtual amplitudes, e.g. 
#     PROCDIR="neuIneuJ+jet/FormCalc_Virtuals"
#     NPART=5
#     PROCF="./proc_nInJj"
#     MSCRIPT="./nInJj.m"
#     TYPE="born"
#
# 2. generate and copy the amplitudes of purely non resonant diagrams:
#     PROCDIR="neuIneuJ+jet/FormCalc_Reals"
#     NPART=6
#     PROCF="./proc_nInJjj_nr"
#     MSCRIPT="./nInJjj.m"
#     TYPE="real"
#
# 3. Modify the part "on-shell subroutines" if needed (i.e. modify the
#     Squark indices Sfe6 and Sfe7).
#
# 4. generate and copy the amplitudes with possible on shell resonances
#     PROCDIR="neuIneuJ+jet/FormCalc_Reals"
#     NPART=6
#     PROCF="./proc_nInJjj_os"
#     MSCRIPT="./nInJjj_os.m"
#     TYPE="realOS"
#
# 5. copy the regulated real processes (no creation of the amplitudes is
#    needed, since the regulated reals were generated in step 3)
#     PROCDIR="neuIneuJ+jet/FormCalc_Reals"
#     NPART=6
#     PROCF="./proc_nInJjj_reg"
#     MSCRIPT=""   # leave this empty!
#     TYPE="real"
#
# process list files
# proc_nInJj      -> Born processes for p p > nI nJ j
# proc_nInJjj     -> all Real processes for p p > nI nJ j (real = nr + os)
# proc_nInJjj_nr  -> processes that don't have not resonant diagrams
# proc_nInJjj_os  -> processes with resonant diagrams
# proc_nInJjj_reg -> regulated real processes with resonant diagrams (os_reg, 
#                    the same as proc_nInJjj, but without channel identifiers)

# the name of the target process directory
PROCDIR="neuIneuJ+jet/FormCalc_Reals"
# where to copy the amplitudes to
DEST=${PWD}/../../${PROCDIR}
# number of particles (incoming + outgoing)
NPART=6
# process list file
PROCF="./proc_nInJjj_os"
# the name of Mathematica Scripts
MSCRIPT=""
# the type of the amplitudes (born, virt, real, realOS)
TYPE="realOS"

########################################################################
#       -*- usually no editing is required below this line -*-         #
########################################################################

# convert PDG numbers to particle names
# $1: PDG number
function pdg2names() {
    case $1 in
        1) echo "d" ;;
       -1) echo "dbar" ;;
        2) echo "u" ;; 
       -2) echo "ubar" ;;
        3) echo "s" ;;
       -3) echo "sbar" ;;
        4) echo "c" ;; 
       -4) echo "cbar" ;;
        5) echo "b" ;;
       -5) echo "bbar" ;;
        6) echo "t" ;; 
       -6) echo "tbar" ;;
        0) echo "g";;    
        1000022) echo "n1";;
        1000023) echo "n2";;
        1000025) echo "n3";;
        1000035) echo "n4";;
        1000024) echo "x1+";;
        1000037) echo "x2+";;
       -1000024) echo "x1-";;
       -1000037) echo "x2-";;
       *) echo $1;;
    esac
}

# rename variables in a file
# $1: filename
# $2: prefix of new variable
function rename() {
    sed -i -e "s/\t/        /g" $1
    sed -i -e "s/vars.h/${2}vars.h/g" $1
    sed -i -e "s/${TYPE}/${2}${TYPE}/g" $1
    sed -i -e "s/vert/${2}vert/g" $1
    sed -i -e "s/self/${2}self/g" $1
    sed -i -e "s/box/${2}box/g" $1
    sed -i -e "s/SquaredME/${2}SquaredME/g" $1
    sed -i -e "s/abbr0h/${2}abbr0h/g" $1
    sed -i -e "s/abbr1h/${2}abbr1h/g" $1
    sed -i -e "s/abbr0s/${2}abbr0s/g" $1
    sed -i -e "s/abbr0a/${2}abbr0a/g" $1
    sed -i -e "s/abbr1s/${2}abbr1s/g" $1
    sed -i -e "s/abbr1a/${2}abbr1a/g" $1    
    sed -i -e 's/#ifdef DEBUG/#ifdef DEBUGQ/g' $1
    # handling of general process indices
    sed -i -e "s/\<Gen1\>/Gen(1)/g" $1
    sed -i -e "s/\<Gen2\>/Gen(2)/g" $1
    sed -i -e "s/\<Gen3\>/Gen(3)/g" $1
    sed -i -e "s/\<Gen4\>/Gen(4)/g" $1
    sed -i -e "s/\<Gen5\>/Gen(5)/g" $1
    sed -i -e "s/\<Gen6\>/Gen(6)/g" $1
    sed -i -e "s/\<Neu1\>/Neu(1)/g" $1
    sed -i -e "s/\<Neu2\>/Neu(2)/g" $1
    sed -i -e "s/\<Neu3\>/Neu(3)/g" $1
    sed -i -e "s/\<Neu4\>/Neu(4)/g" $1
    sed -i -e "s/\<Neu5\>/Neu(5)/g" $1
    sed -i -e "s/\<Neu6\>/Neu(6)/g" $1
    sed -i -e "s/\<Cha1\>/Cha(1)/g" $1
    sed -i -e "s/\<Cha2\>/Cha(2)/g" $1
    sed -i -e "s/\<Cha3\>/Cha(3)/g" $1
    sed -i -e "s/\<Cha4\>/Cha(4)/g" $1
    sed -i -e "s/\<Cha5\>/Cha(5)/g" $1
    sed -i -e "s/\<Cha6\>/Cha(6)/g" $1
    # jede Zeile löschen, die #include "inline.h" enthält
    sed -i -e '/^#include "inline.h"/d' $1
}

# rename variables in include files
# $1: filename
function rename2() {
        sed -i -e "s/\t/        /g" $1
        sed -i -e "s/formfactors/${PRE2}_formfactors/g" $1
        # replace the indices \<Neu3\>, Neu4, Cha3, Cha4, ... with constants
        # max generation
        sed -i -e "s/\<Gen1\>/3/g" $1
        sed -i -e "s/\<Gen2\>/3/g" $1
        sed -i -e "s/\<Gen3\>/3/g" $1
        sed -i -e "s/\<Gen4\>/3/g" $1
        sed -i -e "s/\<Gen5\>/3/g" $1
        sed -i -e "s/\<Gen6\>/3/g" $1
        sed -i -e "s/\<Neu1\>/4/g" $1
        sed -i -e "s/\<Neu2\>/4/g" $1
        sed -i -e "s/\<Neu3\>/4/g" $1
        sed -i -e "s/\<Neu4\>/4/g" $1
        sed -i -e "s/\<Neu5\>/4/g" $1
        sed -i -e "s/\<Neu6\>/4/g" $1
        sed -i -e "s/\<Cha1\>/2/g" $1
        sed -i -e "s/\<Cha2\>/2/g" $1
        sed -i -e "s/\<Cha3\>/2/g" $1
        sed -i -e "s/\<Cha4\>/2/g" $1
        sed -i -e "s/\<Cha5\>/2/g" $1
        sed -i -e "s/\<Cha6\>/2/g" $1
}

# read in the process list
IFS=$' \r\n' command eval 'PART=($(cat $PROCF))'
unset IFS

#echo ${PART[@]}

# total number of processes
NPROC=0
for i in ${PART[@]}; do
    NPROC=$((NPROC+1))
done
# the on shell processes carry additional informations
# regarding the channels in the last column
if [[ $TYPE == "realOS" ]]; then
    NPROC=$((NPROC/(NPART+1)))
else
    NPROC=$((NPROC/NPART))
fi
echo "number of processes: $NPROC"

# build the process names and call the Mathematica script
echo 
echo "building the process names..."
CHANNELS=()
for i in `seq 0 1 $((NPROC-1))`; do
    PROC=""
    P=()
    if [[ $TYPE == "realOS" ]]; then
        for j in `seq 0 1 $NPART`; do
            if [[ $j -eq $NPART ]]; then
                # store the last entry in an array CHANNELS
                CHANNELS+=(${PART[$((j+i*(NPART+1)))]})
            else
                # every other entry goes into P
                P+=($(pdg2names ${PART[$((j+i*(NPART+1)))]}))
                PROC=$PROC${P[j]}
            fi
            if [[ $j -eq 1 ]]; then
                PROC=$PROC"_"
            fi
        done
    else
         CHANNELS+=("none")
        for j in `seq 0 1 $((NPART-1))`; do
            P+=($(pdg2names ${PART[$((j+i*NPART))]}))
            PROC=$PROC${P[j]}
            if [[ $j -eq 1 ]]; then
                PROC=$PROC"_"
            fi
        done
    fi
    #echo ${P[@]}
    echo
    echo "current process: $PROC"    
    
    if [[ $TYPE == "realOS" ]]; then
        IFS=$',\r\n' command eval 'LIST=(${CHANNELS[i]})'
        unset IFS
        echo "channels: ${LIST[@]}"
    fi
    
    # generate the Amplitudes
    # if MSCRIPT is not empty, call the MathKernel
    if [[ -n "$MSCRIPT" ]]; then
        echo "removing ${PROC}_${TYPE}"
        rm -rf ${PROC}_${TYPE}
        echo "calling Mathematica $MSCRIPT -script $MSCRIPT ${P[@]}"
        #/Applications/Mathematica.app/Contents/MacOS/MathKernel -script $MSCRIPT ${P[@]}
        MathKernel -script $MSCRIPT ${P[@]}
    fi
    PROCESSES+=($PROC)
done

# # commented: do this by hand
# #echo "Generating directories..."
# #mkdir ${DEST}/squaredME/
# 
# for i in `seq 0 1 $((NPROC-1))`; do
#     # if no on-shell resonant channels were defined CHANNEL has only one
#     # entry which is "none"
#     IFS=$',\r\n' command eval 'CHANNEL=(${CHANNELS[i]})'
#     unset IFS
#     NCHANNELS=0
#     MCHANNEL=()
#     for j in ${CHANNEL[@]}; do
#         # the mathematica identifiers don't need the left and right identifier, truncate them
#         MCHANNEL+=($(echo "${CHANNEL[NCHANNELS]}" | tr -d "lr"))
#         # count the different channels starting with zero
#         NCHANNELS=$((1+NCHANNELS))
#     done
#     MCHANNEL=($(echo "${MCHANNEL[@]}" | xargs -n1 | sort -u | xargs))
#     #echo "$NCHANNELS channels: ${CHANNEL[@]}"
#     #echo "Mathematica identifiers: ${MCHANNEL[@]}"
#     #exit
#     for j in ${MCHANNEL[@]}; do
#         if [[ $TYPE == "realOS" ]]; then
#             APPEND="_${j}"
#         fi
#         PRE1="${PROCESSES[i]}_${TYPE}${APPEND}"
#         PRE2="${PROCESSES[i]}${APPEND}"
#         
#         echo
#         echo "${PROCESSES[i]} ${TYPE}${APPEND}"
#         echo "PRE1 = $PRE1"
#         echo "PRE2 = $PRE2"
#         
#         #echo "cleaning old files..."
#         #echo $(ls ${DEST}/squaredME/${PRE2}*.F)
#         #echo $(ls ${DEST}/include/${PRE2}_vars.h)
#         #rm ${DEST}/squaredME/${PRE2}*.F
#         #rm ${DEST}/include/${PRE2}_vars.h
#         
#         echo "copying new files..."
#         mkdir -p ${DEST}/${PRE2}_squaredME/
#         cp ${WORKINGDIR}/${PRE1}/squaredme/*.F ${DEST}/${PRE2}_squaredME/
#         cp ${WORKINGDIR}/${PRE1}/squaredme/vars.h ${DEST}/${PRE2}_squaredME/${PRE2}_vars.h
#         
#         
#         echo "renaming files..."
#         echo "cd ${DEST}/${PRE2}_squaredME"
#         cd ${DEST}/${PRE2}_squaredME
#         for file in *.F; do
#             # rename files, append prefix
#             echo "$file -> ${PRE2}_${file}"
#             mv "$file" "${PRE2}_${file}"
#         done
#         for file in *.F; do
#             # rename the variables
#             echo "renaming variables in $file..."
#             rename $file "${PRE2}_"
#             if [[ $TYPE == "realOS" ]]; then
#                 sed -i -e "s/\<Sfe7\>/SfeSQ1/g" $file
#                 sed -i -e "s/\<Sfe8\>/SfeSQ2/g" $file
#             fi
#         done
#         rename2 ${PRE2}_vars.h
#         if [[ $TYPE == "realOS" ]]; then
#             sed -i -e "s/\<Sfe7\>/2/g" ${PRE2}_vars.h
#             sed -i -e "s/\<Sfe8\>/2/g" ${PRE2}_vars.h
#         fi
#         cp ${DEST}/${PRE2}_squaredME/*.F ${DEST}/squaredME/
#         cp ${DEST}/${PRE2}_squaredME/${PRE2}_vars.h ${DEST}/include
#         rm -rf ${DEST}/${PRE2}_squaredME
#     done
# done

# on-shell subroutines:
# modify this if needed.
# generate the subroutines to call the on-shell amplitudes
if [[ $TYPE == "realOS" ]]; then
    echo
    echo "generating on shell routines..."
    cd ${DEST}/squaredME
    for i in `seq 0 1 $((NPROC-1))`; do
        IFS=$',\r\n' command eval 'CHANNEL=(${CHANNELS[i]})'
        unset IFS
        PRE2="${PROCESSES[i]}_${TYPE}"
        
        echo "      subroutine ${PRE2}_squaredME(fc_result, ichan, helicities, flags)" > ${PRE2}_squaredME.F
        echo "        implicit none" >> ${PRE2}_squaredME.F
        echo "#include \"osres.h\"" >> ${PRE2}_squaredME.F
        echo "#include \"indices.h\"" >> ${PRE2}_squaredME.F
        echo "        integer*8 helicities" >> ${PRE2}_squaredME.F
        echo "        integer flags,i,ichan" >> ${PRE2}_squaredME.F
        echo "        double precision fc_result(2)" >> ${PRE2}_squaredME.F
        echo "        fc_result(:) = 0D0" >> ${PRE2}_squaredME.F
        echo "        ! sfeij and sfekl are defined in set_channel" >> ${PRE2}_squaredME.F
        echo "        SfeSQ1 = osres_sfeij" >> ${PRE2}_squaredME.F
        echo "        SFeSQ2 = osres_sfekl" >> ${PRE2}_squaredME.F
        NCHANNELS=0
        for j in ${CHANNEL[@]}; do
            # the mathematica identifiers don't need the left and right identifier, truncate them
            MCHANNEL+=($(echo "${CHANNEL[NCHANNELS]}" | tr -d "lr"))
            NCHANNELS=$((1+NCHANNELS))
        done
        for j in `seq 0 1 $((NCHANNELS-1))`; do
            PRE1="${PROCESSES[i]}_${MCHANNEL[j]}"
            echo "        if(ichan.eq.$((j+1))) then" >> ${PRE2}_squaredME.F
            echo "          call ${PRE1}_SquaredME(fc_result, helicities, flags)" >> ${PRE2}_squaredME.F
            echo "          goto 10" >> ${PRE2}_squaredME.F
            echo "        endif" >> ${PRE2}_squaredME.F
        done
        echo " 10     continue" >> ${PRE2}_squaredME.F
        echo "      end" >> ${PRE2}_squaredME.F
    done
    echo "done."
fi

echo
echo "new Makefile objects (without OS-amplitudes):"
# copy the output to your makefile
echo "cd ${DEST}"
cd ${DEST}
COUNTER=0
y=''
for x in $(ls squaredME/* | grep -v "3546" | grep -v "3645" | grep -v "realOS"); do
  x=${x##*/}
  let COUNTER=COUNTER+1
  y=$y"  "$(echo $x | sed -e 's/\.F/\.o/g')
  if [ $COUNTER -eq 3  ]; then
    COUNTER=0
     echo -e '\t'$y' \'
     y=''
  fi
done
echo -e '\t'$y
echo
echo "new Makefile objects (OS-amplitudes):"
# copy the output to your makefile
echo "cd ${DEST}"
cd ${DEST}
COUNTER=0
y=''
for x in $(ls squaredME/*realOS*.F squaredME/*3645* squaredME/*3546*); do
  x=${x##*/}
  let COUNTER=COUNTER+1
  y=$y"  "$(echo $x | sed -e 's/\.F/\.o/g')
  if [ $COUNTER -eq 3  ]; then
    COUNTER=0
     echo -e '\t'$y' \'
     y=''
  fi
done
echo -e '\t'$y

# clear junk
cd ${DEST}
find . -type f -name '*.F-e'  -exec rm -f '{}' \;
find . -type f -name '*.h-e'  -exec rm -f '{}' \;

# zurück an Anfang
cd ${WORKINGDIR}/

echo
echo "script ended succesfully."
