c############### osres.h ###############################################
c last modified by MK, date 05.12.2015
c global definitions for treating the on-shell resonant diagrams

c for the case of weakino pair-production, there are eight resonances
c but if you have less resonances you can simply initialize the variables
c defined here in a different way.
c              pi
c            /
c           /
c          /---- pj
c   ____  /    !
c  |    |/ sij = mij**2
c  | s  |      !
c  |____|\ skl = mkl**2
c         \
c          \---- pk
c           \
c            \
c              pl

c definitions
        ! store the number of on-shell resonances here
        integer cnosres ! nosres, constants
        parameter (cnosres=8)
        
        ! the width of the on-shell resonant particle (regulator)
        ! this must be set in init_couplings.f
        double precision wreg


        ! variables which get defined in set_channel(flav,ichan)
        ! and are used to determine the on-shell divergence
        integer osres_i, osres_j, osres_k, osres_l
        integer osres_sfeij, osres_sfekl
        double precision osres_mi, osres_mj, osres_mk, osres_ml
        double precision osres_mij, osres_mkl
        double precision osres_wij, osres_wkl

        common/c_onshell/ osres_i, osres_j, osres_k, osres_l
        common/c_onshell/ osres_sfeij, osres_sfekl
        common/c_onshell/ osres_mi, osres_mj, osres_mk, osres_ml
        common/c_onshell/ osres_mij, osres_mkl
        common/c_onshell/ osres_wij, osres_wkl
        common/c_onshell/ wreg

#if defined(DSUB_I) || defined(DSUB_II) || defined(DSUB_II_TEST)
#define WDLR wreg
#define WDRR wreg
#define WULR wreg
#define WURR wreg

#else
#define WDLR WDL
#define WDRR WDR
#define WULR WUL
#define WURR WUR
#endif

c############### end osres.h ###########################################
