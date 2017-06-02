c############### osres.h ###############################################
c last modified by MK, date 05.12.2015
c global definitions for treating the on-shell resonant diagrams

c for the case of weakino pair-production, there are eight resonances
c but if you have less resonances you can simply initialize the variables
c defined here in a different way.
c
c layout of double squark resonances
c
c              pi
c            /
c           /
c          /---- pj
c   ____  /   !
c  |    |/ sij=mij**2
c  | s  |     !
c  |____|\ skl=mkl**2
c         \
c          \---- pk
c           \
c            \
c              pl
c
c layout of single gluino resonances
c
c           pl
c          /
c   ____  /
c  |    |/ 
c  | s  |      !          pi 
c  |____|\ sijk=mijk**2  /
c     mijk\      pij    /
c          \------------
c           \           \
c            \           \
c             \           pj
c              pk
c
c definitions
        ! store the number of on-shell resonances here
        ! max. number of single resonances
        integer nosres1
        parameter (nosres1=8)
        ! max. number of double resonances
        integer nosres2
        parameter (nosres2=8)
        ! total number of double resonances
        integer nosres
        parameter (nosres=nosres1+nosres2)
        
        ! the width of the on-shell resonant particle (regulator)
        ! must be set in init_couplings.f
        ! regulator for single resonances
        double precision wreg1
        ! regulator for double resonances
        double precision wreg2


        ! variables which get defined in set_channel(flav,ichan)
        ! and are used to determine the on-shell divergence
        integer osres_i, osres_j, osres_k, osres_l
        integer osres_sfeij, osres_sfekl
        double precision osres_mi, osres_mj, osres_mk, osres_ml
        double precision osres_mijk
        double precision osres_wijk
        double precision osres_mij, osres_mkl
        double precision osres_wij, osres_wkl

        common/c_onshell/ osres_i, osres_j, osres_k, osres_l
        common/c_onshell/ osres_sfeij, osres_sfekl
        common/c_onshell/ osres_mi, osres_mj, osres_mk, osres_ml
        common/c_onshell/ osres_mijk
        common/c_onshell/ osres_wijk
        common/c_onshell/ osres_mij, osres_mkl
        common/c_onshell/ osres_wij, osres_wkl
        common/c_onshell/ wreg1, wreg2
c############### end osres.h ###########################################
