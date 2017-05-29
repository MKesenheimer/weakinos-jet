#!/bin/bash
WORKINGDIR=${PWD}

./copy_form_amps.sh "chaIchaJ+jet/FormCalc_Reals" 6 "./proc_xIxJjj_nr" "./xIxJjj.m" "real"
./copy_form_amps.sh "chaIchaJ+jet/FormCalc_Reals" 6 "./proc_xIxJjj_os" "./xIxJjj_os.m" "realOS"
./copy_form_amps.sh "chaIchaJ+jet/FormCalc_Reals" 6 "./proc_xIxJjj_reg" "real"