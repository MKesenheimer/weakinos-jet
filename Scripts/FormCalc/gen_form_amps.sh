#!/bin/bash
# Copyright (C) Matthias Kesenheimer - All Rights Reserved
# Written by Matthias Kesenheimer <m.kesenheimer@gmx.net>, 2017

WORKINGDIR=${PWD}

./copy_form_amps.sh "neuIneuJ+jet/FormCalc_Reals" 6 "./proc_nInJjj_nr" "./nInJjj.m" "real"
./copy_form_amps.sh "neuIneuJ+jet/FormCalc_Reals" 6 "./proc_nInJjj_os" "./nInJjj_os.m" "realOS"
./copy_form_amps.sh "neuIneuJ+jet/FormCalc_Reals" 6 "./proc_nInJjj_reg" "real"

./copy_form_amps.sh "neuIchaJ+jet/FormCalc_Reals" 6 "./proc_nIxJjj_nr" "./nIxJjj.m" "real"
./copy_form_amps.sh "neuIchaJ+jet/FormCalc_Reals" 6 "./proc_nIxJjj_os" "./nIxJjj_os.m" "realOS"
./copy_form_amps.sh "neuIchaJ+jet/FormCalc_Reals" 6 "./proc_nIxJjj_reg" "real"

./copy_form_amps.sh "chaIchaJ+jet/FormCalc_Reals" 6 "./proc_xIxJjj_nr" "./xIxJjj.m" "real"
./copy_form_amps.sh "chaIchaJ+jet/FormCalc_Reals" 6 "./proc_xIxJjj_os" "./xIxJjj_os.m" "realOS"
./copy_form_amps.sh "chaIchaJ+jet/FormCalc_Reals" 6 "./proc_xIxJjj_reg" "real"
