#!/bin/bash
WORKINGDIR=${PWD}

# Disclaimer: This script works for neutralino-/chargino-pair + jet 
# production.
# Even though this script is written as common as possible
# does not mean, that this script works for every arbitrary process.
# Keep that in mind when using this script.
# Although the main structur should be the same for every process, so it
# is possible to adapt it to any process.

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
# 2. generate and copy the amplitudes with purely non resonant diagrams:
#     PROCDIR="neuIneuJ+jet/FormCalc_Reals"
#     NPART=6
#     PROCF="./proc_nInJjj_nr"
#     MSCRIPT="./nInJjj.m"
#     TYPE="real"
#
# 3. If needed, modify the parts where squark indices get explicitly 
#     replaced. Possible lines are marked with "-> modify this if needed"
#
# 4. generate and copy the amplitudes with possible on shell resonances
#     PROCDIR="neuIneuJ+jet/FormCalc_Reals"
#     NPART=6
#     PROCF="./proc_nInJjj_os"
#     MSCRIPT="./nInJjj_os.m"
#     TYPE="realOS"
#
# 5. copy the regulated real processes (no creation of the amplitudes is
#    needed, since the regulated reals were generated in step 4)
#     PROCDIR="neuIneuJ+jet/FormCalc_Reals"
#     NPART=6
#     PROCF="./proc_nInJjj_reg"
#     MSCRIPT=""   # leave this empty!
#     TYPE="real"
#
# process list files
# proc_nInJj      -> Born processes for p p > nI nJ j
# proc_nInJjj     -> all Real processes for p p > nI nJ j (real = nr + os)
# proc_nInJjj_nr  -> processes that don't have resonant diagrams
# proc_nInJjj_os  -> processes with resonant diagrams
# proc_nInJjj_reg -> regulated real processes with resonant diagrams (same 
#                    as proc_nInJjj_os, but without channel identifiers)

# the name of the target process directory
PROCDIR="neuIneuJ+jet/FormCalc_Virtuals"

# where to copy the amplitudes to
DEST=${PWD}/../../${PROCDIR}

# number of particles (incoming + outgoing)
NPART=5

# process list file
PROCF="./proc_nInJj"
#PROCF="./proc_nInJjj_nr"
#PROCF="./proc_nInJjj_os"
#PROCF="./proc_nInJjj_reg"

# the name of Mathematica Scripts
#MSCRIPT="./nInJj.m"
MSCRIPT="./nInJj_virt_collier.m"
#MSCRIPT="./nInJjj.m"
#MSCRIPT="./nInJjj_os.m"
#MSCRIPT=""

# the type of the amplitudes (born, virt, real, realOS)
#TYPE="born"
TYPE="virt"
#TYPE="realOS"
#TYPE="real"

# the number of subchannels of realOS amplitudes (f.e. ll, lr, rl, rr)
NSQUARKSUBCHANNELS=4
NGLUINOSUBCHANNELS=2

# if needed: additional complex kinematics for virtual processes
# modify "# add complex mandelstams to *_SquaredME.F" too.
CKINVARS="SC, TC, T14C, UC, T24C, S34C"

########################################################################
#       -*- usually no editing is required below this line -*-         #
########################################################################

# does the script run on Linux or Mac?
UNAME=$(uname)
if [[ $UNAME == Darwin ]]; then
    # name of sed(gsed for Mac OSX)
    SED=gsed
    # name MathKernel
    MATHK=/Applications/Mathematica.app/Contents/MacOS/MathKernel
else
    # name of sed (sed for Linux)
    SED=sed
    # name MathKernel
    MATHK=MathKernel
fi

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
    $SED -i -e "s/\t/        /g" $1
    $SED -i -e "s/vars.h/${2}vars.h/g" $1
    $SED -i -e "s/specs.h/${2}specs.h/g" $1
    $SED -i -e "s/born/${2}born/g" $1
    $SED -i -e "s/virt/${2}virt/g" $1
    $SED -i -e "s/real/${2}real/g" $1
    $SED -i -e "s/vert/${2}vert/g" $1
    $SED -i -e "s/self/${2}self/g" $1
    $SED -i -e "s/box/${2}box/g" $1
    $SED -i -e "s/pent/${2}pent/g" $1
    $SED -i -e "s/SquaredME/${2}SquaredME/g" $1
    $SED -i -e "s/abbr0h/${2}abbr0h/g" $1
    $SED -i -e "s/abbr1h/${2}abbr1h/g" $1
    $SED -i -e "s/abbr0s/${2}abbr0s/g" $1
    $SED -i -e "s/abbr0a/${2}abbr0a/g" $1
    $SED -i -e "s/abbr1s/${2}abbr1s/g" $1
    $SED -i -e "s/abbr1a/${2}abbr1a/g" $1    
    $SED -i -e 's/#ifdef DEBUG/#ifdef DEBUGQ/g' $1
    # handling of general process indices
    for i in `seq 1 ${NPART}`; do
      $SED -i -e "s/\<Gen${i}\>/Gen(${i})/g" $1
      $SED -i -e "s/\<Neu${i}\>/Neu(${i})/g" $1
      $SED -i -e "s/\<Cha${i}\>/Cha(${i})/g" $1
    done
    # ändere die Namen der globalen Include Dateien
    #$SED -i -e '/^#include "inline.h"/d' $1
    $SED -i -e "s/const.h/${TYPE}_const.h/g" $1
    $SED -i -e "s/contains.h/${TYPE}_contains.h/g" $1
    $SED -i -e "s/decl.h/${TYPE}_decl.h/g" $1
    $SED -i -e "s/inline.h/${TYPE}_inline.h/g" $1
    $SED -i -e "s/types.h/${TYPE}_types.h/g" $1
    $SED -i -e "s/user.h/${TYPE}_user.h/g" $1
    $SED -i -e "s/util.h/${TYPE}_util.h/g" $1
}

# rename variables in vars.h include files
# $1: filename
function rename2() {
        $SED -i -e "s/decl.h/${TYPE}_decl.h/g" $1
        $SED -i -e "s/\t/        /g" $1
        $SED -i -e "s/varX/${PRE2}_varX/g" $1
        $SED -i -e "s/indices/${PRE2}_indices/g" $1
        $SED -i -e "s/formfactors/${PRE2}_formfactors/g" $1
        # replace the indices \<Neu3\>, Neu4, Cha3, Cha4, ... with constants
        # max generation
        for i in `seq 1 ${NPART}`; do
          $SED -i -e "s/\<Gen${i}\>/3/g" $1
          $SED -i -e "s/\<Neu${i}\>/4/g" $1
          $SED -i -e "s/\<Cha${i}\>/2/g" $1
        done
}

# rename variables in RenConst*.* files
# $1: filename
# $2: prefix of new variable
function rename3() {
        $SED -i -e "s/decl.h/virt_decl.h/g" $1
        $SED -i -e "s/inline.h/virt_inline.h/g" $1
        $SED -i -e "s/contains.h/virt_contains.h/g" $1
        $SED -i -e "s/subroutine RenConst/subroutine ${2}RenConst/g" $1
        $SED -i -e "s/\t/        /g" $1
}

# read in the process list and ignore what comes after #
IFS=$' \r\n' command eval 'PART=($(grep -v "^#" $PROCF | cut -f1 -d"#"))'
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
        IFS=$',;\r\n' command eval 'LIST=(${CHANNELS[i]})'
        unset IFS
        echo "channels: ${LIST[@]}"
    fi
    
    # generate the Amplitudes
    # if MSCRIPT is not empty, call the MathKernel
    if [[ -n "$MSCRIPT" ]]; then
        echo "removing ${PROC}_${TYPE}"
        rm -rf ${PROC}_${TYPE}
        echo "calling Mathematica $MSCRIPT -script $MSCRIPT ${P[@]}"
        $MATHK -script $MSCRIPT ${P[@]}
    fi
    PROCESSES+=($PROC)
done

# commented: do this by hand
#echo "Generating directories..."
#mkdir ${DEST}/squaredME/

for i in `seq 0 1 $((NPROC-1))`; do
    # if no on-shell resonant channels were defined CHANNEL has only one
    # entry which is "none"
    IFS=$',;\r\n' command eval 'CHANNEL=(${CHANNELS[i]})'
    unset IFS
    for j in ${CHANNEL[@]}; do
        if [[ $TYPE == "realOS" ]]; then
            APPEND="_${j}"
        fi
        PRE1="${PROCESSES[i]}_${TYPE}${APPEND}"
        PRE2="${PROCESSES[i]}${APPEND}"
        
        echo
        echo "${PROCESSES[i]} ${TYPE}${APPEND}"
        echo "PRE1 = $PRE1"
        echo "PRE2 = $PRE2"

        #echo "cleaning old files..."
        #echo $(ls ${DEST}/squaredME/${PRE2}*.F)
        #echo $(ls ${DEST}/include/${PRE2}_vars.h)
        #echo $(ls ${DEST}/include/${PRE2}_specs.h)
        #rm ${DEST}/squaredME/${PRE2}*.F
        #rm ${DEST}/include/${PRE2}_vars.h
        #rm ${DEST}/include/${PRE2}_specs.h
        
        echo "copying new files..."
        mkdir -p ${DEST}/${PRE2}_squaredME/
        cp ${WORKINGDIR}/${PRE1}/squaredme/*.F ${DEST}/${PRE2}_squaredME/
        cp ${WORKINGDIR}/${PRE1}/squaredme/vars.h ${DEST}/${PRE2}_squaredME/${PRE2}_vars.h
        cp ${WORKINGDIR}/${PRE1}/squaredme/specs.h ${DEST}/${PRE2}_squaredME/${PRE2}_specs.h
        mkdir -p ${DEST}/${PRE2}_RenConst/
        cp ${WORKINGDIR}/${PRE1}/renconst/RenConst* ${DEST}/${PRE2}_RenConst/

        echo "renaming files..."
        echo "cd ${DEST}/${PRE2}_squaredME"
        cd ${DEST}/${PRE2}_squaredME
        for file in *.F; do
            # rename files, append prefix
            echo "$file -> ${PRE2}_${file}"
            mv "$file" "${PRE2}_${file}"
        done

        if [[ $TYPE == "virt" ]]; then
            echo "cd ${DEST}/${PRE2}_RenConst"
            cd ${DEST}/${PRE2}_RenConst
            for file in *.F; do
                # rename files, append prefix
                echo "$file -> ${PRE2}_${file}"
                mv "$file" "${PRE2}_${file}"
            done
        fi

        echo "cd ${DEST}/${PRE2}_squaredME"
        cd ${DEST}/${PRE2}_squaredME
        for file in *.F; do
            # rename the variables
            echo "renaming variables in $file..."
            rename $file "${PRE2}_"
            # -> modify this if needed.
            # if the channel identifiers contain the open squark indice "Sq1"
            # rename Sfe7 with unassigned variable name
            if [[ $PRE2 == *"Sq1" ]] && [[ $TYPE == "realOS" ]]; then
                $SED -i -e "s/\<Sfe7\>/Sq1/g" $file
            fi
            # if the channel identifiers contain the open squark indices "Sq1Sq2"
            # rename Sfe7 and Sfe8 with unassigned variable names
            if [[ $PRE2 == *"Sq1Sq2" ]] && [[ $TYPE == "realOS" ]]; then
                $SED -i -e "s/\<Sfe7\>/Sq1/g" $file
                $SED -i -e "s/\<Sfe8\>/Sq2/g" $file
            fi
        done
        rename2 ${PRE2}_vars.h
        rename ${PRE2}_specs.h # don't change Gen1 to 3 but to Gen(1)
        # -> modify this if needed.
        # if the channel identifiers contain the open squark indice "Sq1"
        # rename Sfe7 with unassigned variable name
        if [[ $PRE2 == *"Sq1" ]] && [[ $TYPE == "realOS" ]]; then
            $SED -i -e "s/\<Sfe7\>/2/g" ${PRE2}_vars.h
        fi
        # if the channel identifiers contain the open squark indices "Sq1Sq2"
        # rename Sfe7 and Sfe8 with unassigned variable names
        if [[ $PRE2 == *"Sq1Sq2" ]] && [[ $TYPE == "realOS" ]]; then
            $SED -i -e "s/\<Sfe7\>/2/g" ${PRE2}_vars.h
            $SED -i -e "s/\<Sfe8\>/2/g" ${PRE2}_vars.h
        fi

        # modify virtual amplitudes
        if [[ $TYPE == "virt" ]]; then
            # add complex kinematical variables for virtual processes
            #$SED -i -e "s/#endif//g" ${PRE2}_vars.h
            echo "#ifndef vars2_h" >> ${PRE2}_vars.h
            echo "#define vars2_h" >> ${PRE2}_vars.h
            echo "#else" >> ${PRE2}_vars.h
            echo "        ComplexType ${CKINVARS}" >> ${PRE2}_vars.h
            echo "        common /${PRE2}_ckin/ ${CKINVARS}" >> ${PRE2}_vars.h
            echo "#endif" >> ${PRE2}_vars.h
            # add complex mandelstams to *_SquaredME.F
            $SED -i -e "s/* END INVARIANTS/        SC = dcmplx(S)\n* END INVARIANTS/g" ${PRE2}_SquaredME.F
            $SED -i -e "s/* END INVARIANTS/        TC = dcmplx(T)\n* END INVARIANTS/g" ${PRE2}_SquaredME.F
            $SED -i -e "s/* END INVARIANTS/        T14C = dcmplx(T14)\n* END INVARIANTS/g" ${PRE2}_SquaredME.F
            $SED -i -e "s/* END INVARIANTS/        UC = dcmplx(U)\n* END INVARIANTS/g" ${PRE2}_SquaredME.F
            $SED -i -e "s/* END INVARIANTS/        T24C = dcmplx(T24)\n* END INVARIANTS/g" ${PRE2}_SquaredME.F
            $SED -i -e "s/* END INVARIANTS/        S34C = dcmplx(S34)\n* END INVARIANTS/g" ${PRE2}_SquaredME.F
            # rename variables in RenConst*
            echo "cd ${DEST}/${PRE2}_RenConst"
            cd ${DEST}/${PRE2}_RenConst
            for file in *.F; do
                echo "renaming variables in $file..."
                rename3 $file "${PRE2}_"
                # insert new regulator parameter. Note: Lines in reversed order
                TAG="        implicit none"
                $SED -i -e "s/$TAG/$TAG\n        parameter (MR=1D-10, MR2=MR**2)/g" $file
                $SED -i -e "s/$TAG/$TAG\n        RealType MR,MR2/g" $file
                $SED -i -e "s/$TAG/$TAG\n\n        \! Regularize 1\/MD Terms/g" $file
            done
        fi

        # copy finished files to the common directory
        cp ${DEST}/${PRE2}_squaredME/*.F ${DEST}/squaredME/
        cp ${DEST}/${PRE2}_squaredME/${PRE2}_vars.h ${DEST}/include/
        cp ${DEST}/${PRE2}_squaredME/${PRE2}_specs.h ${DEST}/include/
        rm -rf ${DEST}/${PRE2}_squaredME/
        
        if [[ $TYPE == "virt" ]]; then
            cp ${DEST}/${PRE2}_RenConst/* ${DEST}/RenConst/
            rm -rf ${DEST}/${PRE2}_RenConst/
        fi
    done
done


# copying global include files
echo "copying global header files..."
mkdir -p ${DEST}/temp/
# use last process
cp ${WORKINGDIR}/${PRE1}/F/const.h ${DEST}/temp/${TYPE}_const.h
# append additional constant definition to const.h
echo -e "\tComplexType CNULL" >> ${DEST}/temp/${TYPE}_const.h
echo -e "\tparameter (CNULL = (0D0,0D0))" >> ${DEST}/temp/${TYPE}_const.h
cp ${WORKINGDIR}/${PRE1}/F/contains.h ${DEST}/temp/${TYPE}_contains.h
cp ${WORKINGDIR}/${PRE1}/F/decl.h ${DEST}/temp/${TYPE}_decl.h
cp ${WORKINGDIR}/${PRE1}/F/inline.h ${DEST}/temp/${TYPE}_inline.h
cp ${WORKINGDIR}/${PRE1}/F/types.h ${DEST}/temp/${TYPE}_types.h
cp ${WORKINGDIR}/${PRE1}/F/user.h ${DEST}/temp/${TYPE}_user.h
cp ${WORKINGDIR}/${PRE1}/F/util.h ${DEST}/temp/${TYPE}_util.h
# modify some of the copied files
cd ${DEST}/temp/
rename ${TYPE}_decl.h ""
$SED -i -e '/^#include "distrib.h"/d' ${TYPE}_decl.h
$SED -i -e '/^#include "extra.h"/d' ${TYPE}_decl.h
if [[ $TYPE == "real" ]] || [[ $TYPE == "realOS" ]]; then
    $SED -i -e '/^#include "RenConst.h"/d' ${TYPE}_decl.h
    $SED -i -e '/^#include "looptools.h"/d' ${TYPE}_decl.h
fi
$SED -i -e 's/#include "model_mssm.h"/#include "model_mssm.h"\n#include "model_sm.h"\n#include "osres.h"\n#include "indices.h"\n/g' ${TYPE}_user.h
$SED -i -e 's/#include "looptools.h"/#ifdef collier\n#include "lt_collier.h"\n#else\n#include "looptools.h"\n#endif/g' ${TYPE}_decl.h
#$SED -i -e 's/Sq(z_)/!Sq(z_)/g' ${TYPE}_inline.h
cp ${DEST}/temp/*.h ${DEST}/global/
rm -rf ${DEST}/temp/

# copy and execute script to finalize the header files
echo "modifying header files..."
cp ${WORKINGDIR}/finalize.sh ${DEST}/include
cd ${DEST}/include && ./finalize.sh


# on-shell subroutines:
# -> modify this if needed.
# generate the subroutines to call the on-shell amplitudes
if [[ $TYPE == "realOS" ]]; then
    echo
    echo "generating on shell routines..."
    cd ${DEST}/squaredME
    for i in `seq 0 1 $((NPROC-1))`; do
        IFS=$',;\r\n' command eval 'CHANNEL=(${CHANNELS[i]})'
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
        echo "        ! sfeij and sfekl get defined in subroutine set_channel" >> ${PRE2}_squaredME.F
        # track the number of processed channels
        ICHAN=0
        for j in ${CHANNEL[@]}; do
            PRE1="${PROCESSES[i]}_${j}"
            # -> modify this if needed.
            # if there are subchannels (for example different chiralities for squarks)
            # a different if statement is needed and the open indices have to be set
            if [[ $j == *"Sq1Sq2" ]]; then
                ICHAN=$((ICHAN+NSQUARKSUBCHANNELS))
                echo "        if(ichan.ge.$((ICHAN-NSQUARKSUBCHANNELS+1)) .and. ichan.le.$ICHAN) then" >> ${PRE2}_squaredME.F
                echo "          Sq1 = osres_sfeij" >> ${PRE2}_squaredME.F
                echo "          Sq2 = osres_sfekl" >> ${PRE2}_squaredME.F
            elif [[ $j == *"Sq1" ]]; then
                ICHAN=$((ICHAN+NGLUINOSUBCHANNELS))
                echo "        if(ichan.ge.$((ICHAN-NGLUINOSUBCHANNELS+1)) .and. ichan.le.$ICHAN) then" >> ${PRE2}_squaredME.F
                echo "          Sq1 = osres_sfeij" >> ${PRE2}_squaredME.F
            else
                ICHAN=$((ICHAN+1))
                echo "        if(ichan.eq.$ICHAN) then" >> ${PRE2}_squaredME.F
            fi
            echo "          call ${PRE1}_SquaredME(fc_result, helicities, flags)" >> ${PRE2}_squaredME.F
            echo "          goto 10" >> ${PRE2}_squaredME.F
            echo "        endif" >> ${PRE2}_squaredME.F
        done
        echo " 10     continue" >> ${PRE2}_squaredME.F
        echo "      end" >> ${PRE2}_squaredME.F
    done
    echo "done."
fi

# clear junk
cd ${DEST}
find . -type f -name '*.F-e'  -exec rm -f '{}' \;
find . -type f -name '*.h-e'  -exec rm -f '{}' \;

# zurück an Anfang
cd ${WORKINGDIR}/

echo
echo "script ended succesfully."
