########################################################################
#                          -*- Makefile -*-                            #
########################################################################
# General Conventions for Makefiles
SHELL = /bin/sh
.SUFFIXES:
.SUFFIXES: .c .f .F .cc .cpp .h .hh .inc .o .a
.DEFAULT_GOAL := all

########################################################################
## Flags

## Compiler and additional compiler Flags
FC  =  gfortran
CXX =  g++
CC  =  gcc
FCFLAGS  = -g
CXXFLAGS = -g
CFLAGS   = -g
LDFLAGS  = -ff2c -g

# recommended compiler flags
REC_FCFLAGS   = -fno-automatic -fno-range-check
REC_FCFLAGS  += -ffixed-line-length-none -lgfortran -DU77EXT=0 -DQuad=0
REC_FCFLAGS  += -ff2c -fno-second-underscore
REC_FCFLAGS  += $(FCFLAGS)
REC_CXXFLAGS  = -fomit-frame-pointer -ffast-math -Wall -m64
REC_CXXFLAGS += $(CXXFLAGS)
REC_CFLAGS    = -fomit-frame-pointer -ffast-math -Wall -m64
REC_CFLAGS   += -DNOUNDERSCORE=0 -DBIGENDIAN=0
REC_CFLAGS   += $(CFLAGS)

UNAME = $(shell uname)
ifeq ($(UNAME), Darwin)
  #Mac OSX
  REC_CFLAGS   += -stdlib=libstdc++ -mmacosx-version-min=10.6 -Qunused-arguments
  REC_CXXFLAGS += -stdlib=libstdc++ -mmacosx-version-min=10.6 -Qunused-arguments
endif

## PDF
## choose PDF: native,lhapdf
## LHAPDF package has to be installed separately
## wheter to link precompiled LHAPDF library statically or compile it 
## from source (static, none)
PDF = lhapdf
PDFSTATIC = none

## path to static lhapdf library
#STATICLIBSLHAPDF = /opt/lib/libLHAPDF.a

## choose analysis: none, default
ANALYSIS = default

## path to LHAPDF config executable
LHAPDF_CONFIG = lhapdf-config

## path to fastjet config executable
FASTJET_CONFIG = fastjet-config

## Pythia8
# if you want to use Pythia8 instead of Pythia6 uncomment the following
#USE_PYTHIA8 = yes
# path to pythia8-config executable
# (uncomment only if you want to use a locally installed Pythia8)
#PYTHIA8_CONFIG = pythia8-config

# (not necessary anymore)
## path to fortran and c++ libraries
# (use $locate libstdc++.a to find the library)
#ifeq ($(UNAME), Darwin)
#  #Mac OSX
#  LIBGFORTRANPATH = /usr/local/lib
#  LIBSTDCPPPATH   = /usr/local/lib
#else
#  #Linux 
#  LIBGFORTRANPATH = /usr/lib/gcc/x86_64-linux-gnu/4.8
#  LIBSTDCPPPATH   = /usr/lib/gcc/x86_64-linux-gnu/4.8
#endif

## warning for type-conversions -> basically useless, as those occur in
## too many places
#WARN  = -Wconversion -Wall -Wtabs -Wall -Wimplicit-interface
## -fbounds-check sometimes causes a weird error due to non-lazy
## evaluation of boolean in gfortran.
#WARN += -fbounds-check
## gfortran 4.4.1 optimized with -O3 yields erroneous results
## Use -O2 to be on the safe side
OPT = -O2

### generate directory build, if not yet existing
$(shell mkdir -p build)

### version control
GV = $(shell git rev-parse --short HEAD)
$(shell echo "      subroutine print_version" > ../version.f)
$(shell echo "        print*,'        git version $(GV)'" >> ../version.f)
$(shell echo "      end" >> ../version.f)

########################################################################
## Runtime flags

## Preprocessor
# it might be advisable to use the -ffree-line-length-none 
# or -ffixed-line-length-none options
CPP = -cpp

## For debugging uncomment the following
#DEBUG = -ggdb -pg -D DEBUG
## more verbose debugging
DEBUG = -ggdb -pg -D DEBUG -D DEBUGV
## If you wish to show all possible debug output
#DEBUG = -ggdb -pg -D DEBUG -D DEBUGV -D DEBUGQ -D CHECK

## More debugging flags
# Check UV finiteness in FormCalc Virtuals: -D CHECKUV
# Check if MadGraph and FormCalc born amplitudes are the same during 
# calculation and abort if they are not: -D CHECK_FORM_MAD
# Note: Use this only for testing born amplitudes, not for the final 
# program and not while checking real amplitudes!
USRFLAGS += -D CHECK_FORM_MAD

## Additional and mandatory flags
# Use MadGraph Borns (default): -D MAD_BORN 
# Use FormCalc Borns: -D FORM_BORN
#USRFLAGS += -D MAD_BORN
USRFLAGS += -D FORM_BORN

## Treatment of quark masses and squark widths
# The current version uses vanishing masses for the incoming quarks.
# Whether to neglect the squark widths or use the complex mass scheme
# (CMS) to regularize on-shell resonances (default).
USRFLAGS += -D NEGLECTBMASS
#USRFLAGS += -D NEGLECT_SWIDTH
#USRFLAGS += -D COMPLEXMASS
# If you want to change these flags, you probably have to change a lot
# for the FormCalc amplitudes. For example, the b-quark amplitudes are 
# currently calculated by crossing relations from d-quark amplitudes.
# Since FormCalc does not use squark widths, you have to introduce
# squark widths by hand.

## Subtraction schemes
# There are divergences originating from on-shell produced squark lines
# in the real processes. The resonant contribution can easily exceed 
# the full NLO corrections. Keeping it as part of the real corrections
# would spoil the predictivity of the NLO calculation. Therefore, these
# contributions have to be removed in a consistent way.
# There exist several methods to cope with them, the most relevant 
# being the following:
# - Diagram Removal type I, implemented with preprocessor flag -D DR_I
# - Diagram Removal type II, implemented with preprocessor flag -D DR_II
# - Diagram Subtraction, implemented with preprocessor flag -D DSUB_I,
#   no correction of the jacobian, which is in principal not ok, but 
#   could be used for testing the convergence.
# - Diagram Subtraction with corrected jacobian, flag -D DSUB_II.
#   With this flag defined, the real amplitude is split into two parts.
#   In Reals.f the matrix element |M_NR|**2 + 2 M_R* M_NR is calculated,
#   in Reals_osres.f the remaining terms and the on-shell counter terms
#   are added: |M_R|**2 - BW*corrfac*|M_R^OS|**2. The latter gets 
#   integrated seperately in sigosres.f and is included at a later point
#   to the main result.
#USRFLAGS += -D SLICING
#USRFLAGS += -D DR_I
#USRFLAGS += -D DR_II
#USRFLAGS += -D DSUB_I
#USRFLAGS += -D DSUB_II_TEST
#USRFLAGS += -D DSUB_II

########################################################################
#           -*- no editing is required below this line -*-             #
########################################################################

########################################################################
## Runtime flags which should not be changed (don't touch this unless
## you know what you do!) 
CHANNEL += -D NINJ_JET

########################################################################
## Paths

WORKINGDIR = $(shell pwd)/..
PWHGDIR    = $(WORKINGDIR)/..
SUBWORKINGDIR = $(shell pwd)

# directories
TOOLS   = $(WORKINGDIR)/Tools
REAL    = $(SUBWORKINGDIR)/MadGraph_Reals
VIRT    = $(SUBWORKINGDIR)/FormCalc_Virtuals
VIRTSUB = $(VIRT)/squaredME $(VIRT)/RenConsts
FORMREAL = $(SUBWORKINGDIR)/FormCalc_Reals
FORMREALSUB = $(FORMREAL)/squaredME
REALSUB  = $(REAL)/SubProcesses_nInJ+jet
MODIFIED = $(WORKINGDIR)/POWHEG_mod
SCRIPTS  = $(WORKINGDIR)/Scripts

FJ     = $(TOOLS)/fastjet
PY     = $(TOOLS)/pythia6425
PY8    = $(TOOLS)/pythia8215
PYF    = $(TOOLS)/pythiafortran
HER    = $(TOOLS)/herwig
LT     = $(TOOLS)/LoopTools-2.12
SLHA   = $(TOOLS)/SLHALib-2.2
DHELAS = $(TOOLS)/DHELAS
MADMODEL  = $(REAL)/MODEL

# includes
UINCLUDE    = $(WORKINGDIR)/include
SINCLUDE    = $(SUBWORKINGDIR)/include
PINCLUDE    = $(PWHGDIR)/include
VINCLUDE    = $(VIRT)/include
MDINCLUDE   = $(REAL)/include
SLHAINCLUDE = $(SLHA)/src
LTINCLUDE   = $(LT)/src/include
PY8INCLUDE  = $(PY8)/include
FRINCLUDE   = $(FORMREAL)/include

########################################################################
## search for the files and set paths

vpath %.f $(PWHGDIR) $(WORKINGDIR) $(TOOLS)/functions $(DHELAS) $(PY) $(PYF) $(HER)
vpath %.F $(WORKINGDIR) $(TOOLS)/FormCalcTools $(DHELAS)
vpath %.F $(VIRTSUB)
vpath %.F $(FORMREALSUB)
vpath %.F $(MADMODEL) $(REALSUB)
vpath %.f $(MADMODEL) $(REALSUB)
vpath %.f $(MODIFIED)
vpath %.cc $(FJ) $(PY8) $(PYF)
vpath %.o $(SUBWORKINGDIR)/build

########################################################################
## Source files

### PDFs ###
ifeq ("$(PDF)","lhapdf")
  PDFPACK       = lhapdfif.o
  LIBSLHAPDF    = -Wl,-rpath,$(shell $(LHAPDF_CONFIG) --libdir)		\
                  -L$(shell $(LHAPDF_CONFIG) --libdir) -lLHAPDF
ifeq  ("$(PDFSTATIC)","static")
  LIBSLHAPDF += $(STATICLIBSLHAPDF)
endif
  LIBS += $(LIBSLHAPDF) 
else
  PDFPACK = mlmpdfif.o hvqpdfpho.o
endif

### ANALYSIS ###
ifeq ("$(ANALYSIS)","default")
  USE_FASTJET  = yes
  PWHGANALYSIS = Analysis.o
endif
ifeq ("$(ANALYSIS)","none")
  PWHGANALYSIS = Analysis-dummy.o
endif

ifdef USE_FASTJET
  ifeq ($(FASTJET_CONFIG),)
    $(error cannot find fastjet-config (did you install fastjet?))
  endif
  LIBS += $(shell $(FASTJET_CONFIG) --libs --plugins )
  FJCXXFLAGS  += $(shell $(FASTJET_CONFIG) --cxxflags)
  PWHGANALYSIS+= fastjetfortran.o
endif

# the user choosed to use Pythia8
ifeq ("$(USE_PYTHIA8)","yes")
# the user choosed to use a locally installed Pythia8
ifdef PYTHIA8_CONFIG
  LIBS += $(shell $(PYTHIA8_CONFIG) --libs --plugins )
  PYCXXFLAGS  += $(shell $(PYTHIA8_CONFIG) --cxxflags)
else
  LIBS += $(TOOLS)/libpythia8.a
  PYCXXFLAGS += -I$(PY8INCLUDE)
endif
  PWHGANALYSIS+= pythiafortran.o
endif

### USER Files ###
USER = Born.o Born_phsp.o Born_lh.o Reals.o Reals_lh.o Virtuals.o 	\
       init_processes.o init_couplings.o Reals_osres.o 		\
       Real_osres_phsp.o phi1_2.o sigosres.o version.o FormCalc.o

### Functions in Tools/functions/
FUNC = funcprocess.o Print.o funcbasic.o off_to_on.o

### MadGraph mandatory files
MMADOBJ = couplings.o hardstop.o printout.o read_slha.o \
	set_general_couplings.o nexper.o

### Formcalc Files in Tools/FormcalcTools/###
FORMCALC = VecSet.o

### PWHG FILES in ../ ###
PWHG = pwhg_main.o pwhg_init.o btilde.o utils.o gen_Born_phsp.o	\
	test_Sudakov.o pt2maxreg.o maxrat.o Bornzerodamp.o		\
	random.o boostrot.o bra_ket_subroutines.o			\
	init_phys.o powheginput.o sigcollremn.o		\
	pwhg_bookhist-multi.o pwhg_analysis_driver.o checkmomzero.o	\
	setstrongcoupl.o integrator.o mwarn.o		\
	sigcollsoft.o validflav.o decidemerging.o          		\
	loadstorenlo.o setlocalscales.o ubprojections.o      	\
	mint_upb.o reshufflemoms.o opencount.o

# modified POWHEG-files
PWHG += cernroutines_mod.o find_regions_mod.o gen_real_phsp_mod.o	\
	sigborn_mod.o sigreal_mod.o sigsoftvirt_mod.o			\
	sigvirtual_mod.o bbinit_mod.o LesHouches_mod.o 			\
	LesHouchesreg_mod.o gen_index_mod.o lhefwrite_mod.o		\
	gen_radiation_mod.o sigremnants_mod.o pwhgreweight_mod.o 	\
	pdfcalls.o newunit_mod.o

### LHEF ###
# analyze LHEF output
LHEF = lhef_analysis.o boostrot.o random.o cernroutines_mod.o		\
     opencount.o powheginput.o pwhg_bookhist-multi.o 			\
     lhefread.o newunit_mod.o pwhg_analysis_driver.o 			\
     funcprocess.o init_couplings.o Print.o				\
     bra_ket_subroutines.o off_to_on.o funcbasic.o

### PYTHIA ###
# target to read event file, shower events with PYTHIA + analysis
# NOTE: pythia-6.4.25-decay.f is based on POWHEG-BOX/pythia-6.4.25.f,
# to which we applied a simple patch to enable slepton decays
# (for clarity a separate patch file is included which is not needed otherwise)
PYTHIA = boostrot.o powheginput.o init_couplings.o funcprocess.o 	\
	pwhg_bookhist-multi.o lhefread.o newunit_mod.o pdfdummies.o		\
	pwhg_analysis_driver.o random.o cernroutines_mod.o opencount.o	\
	bra_ket_subroutines.o off_to_on.o funcbasic.o Print.o
ifeq ("$(USE_PYTHIA8)","yes")
  # TODO:
  PYTHIA += main-PYTHIA_mod.o
else
  PYTHIA += main-PYTHIA.o setup-PYTHIA-lhef.o
  PYTHIA += pythia-6.4.25-decay.o
endif

### HERWIG ###
# this is work in progress
HERWIG = main-HERWIG.o setup-HERWIG-lhef.o herwig6510.o boostrot.o 	\
     opencount.o powheginput.o pwhg_bookhist-multi.o random.o		\
     lhefread.o newunit_mod.o pwhg_analysis_driver.o 			\
     funcprocess.o init_couplings.o cernroutines_mod.o			\
     bra_ket_subroutines.o off_to_on.o funcbasic.o

# MadGraph born amplitudes
MADOBJ += sborn_proc.o sreal_proc.o
MADOBJ += b_cl_001_001.o b_cl_001_002.o b_cl_001_003.o \
	 b_cl_002_001.o b_cl_002_002.o b_cl_002_003.o \
	 b_cl_003_001.o b_cl_003_002.o b_cl_003_003.o \
	 b_cl_004_001.o b_cl_004_002.o b_cl_004_003.o \
	 b_cl_005_001.o b_cl_005_002.o b_cl_005_003.o \
	 b_cl_006_001.o b_cl_006_002.o b_cl_006_003.o \
	 b_cl_007_001.o b_cl_007_002.o b_cl_007_003.o \
	 b_cl_008_001.o b_cl_008_002.o b_cl_008_003.o \
	 b_cl_021_001.o b_cl_021_002.o b_cl_021_003.o \
	 b_cl_022_001.o b_cl_022_002.o b_cl_022_003.o \
	 b_cl_023_001.o b_cl_023_002.o b_cl_023_003.o \
	 b_cl_024_001.o b_cl_024_002.o b_cl_024_003.o \
	 b_sf_001_004.o b_sf_001_005.o b_sf_001_006.o \
	 b_sf_001_007.o b_sf_001_008.o b_sf_001_009.o \
	 b_sf_002_004.o b_sf_002_005.o b_sf_002_006.o \
	 b_sf_002_007.o b_sf_002_008.o b_sf_002_009.o \
	 b_sf_003_004.o b_sf_003_005.o b_sf_003_006.o \
	 b_sf_003_007.o b_sf_003_008.o b_sf_003_009.o \
	 b_sf_004_004.o b_sf_004_005.o b_sf_004_006.o \
	 b_sf_004_007.o b_sf_004_008.o b_sf_004_009.o \
	 b_sf_005_004.o b_sf_005_005.o b_sf_005_006.o \
	 b_sf_005_007.o b_sf_005_008.o b_sf_005_009.o \
	 b_sf_006_004.o b_sf_006_005.o b_sf_006_006.o \
	 b_sf_006_007.o b_sf_006_008.o b_sf_006_009.o \
	 b_sf_007_004.o b_sf_007_005.o b_sf_007_006.o \
	 b_sf_007_007.o b_sf_007_008.o b_sf_007_009.o \
	 b_sf_008_004.o b_sf_008_005.o b_sf_008_006.o \
	 b_sf_008_007.o b_sf_008_008.o b_sf_008_009.o \
	 b_sf_021_004.o b_sf_021_005.o b_sf_021_006.o \
	 b_sf_021_007.o b_sf_021_008.o b_sf_021_009.o \
	 b_sf_022_004.o b_sf_022_005.o b_sf_022_006.o \
	 b_sf_022_007.o b_sf_022_008.o b_sf_022_009.o \
	 b_sf_023_004.o b_sf_023_005.o b_sf_023_006.o \
	 b_sf_023_007.o b_sf_023_008.o b_sf_023_009.o \
	 b_sf_024_004.o b_sf_024_005.o b_sf_024_006.o \
	 b_sf_024_007.o b_sf_024_008.o b_sf_024_009.o \
	 r_bb_nInJbb.o r_bbx_nInJbbx.o r_bbx_nInJccx.o \
	 r_bbx_nInJddx.o r_bbx_nInJgg.o r_bbx_nInJssx.o \
	 r_bbx_nInJuux.o r_bc_nInJcb.o r_bcx_nInJcxb.o \
	 r_bd_nInJdb.o r_bdx_nInJdxb.o r_bg_nInJbg.o \
	 r_bs_nInJsb.o r_bsx_nInJsxb.o r_bu_nInJub.o \
	 r_bux_nInJuxb.o r_bxb_nInJbbx.o r_bxb_nInJccx.o \
	 r_bxb_nInJddx.o r_bxb_nInJgg.o r_bxb_nInJssx.o \
	 r_bxb_nInJuux.o r_bxbx_nInJbxbx.o r_bxc_nInJcbx.o \
	 r_bxcx_nInJcxbx.o r_bxd_nInJdbx.o r_bxdx_nInJdxbx.o \
	 r_bxg_nInJbxg.o r_bxs_nInJsbx.o r_bxsx_nInJsxbx.o \
	 r_bxu_nInJubx.o r_bxux_nInJuxbx.o r_cb_nInJcb.o \
	 r_cbx_nInJcbx.o r_cc_nInJcc.o r_ccx_nInJbbx.o \
	 r_ccx_nInJccx.o r_ccx_nInJddx.o r_ccx_nInJgg.o \
	 r_ccx_nInJssx.o r_ccx_nInJuux.o r_cd_nInJdc.o \
	 r_cdx_nInJdxc.o r_cg_nInJcg.o r_cs_nInJcs.o \
	 r_csx_nInJcsx.o r_cu_nInJuc.o r_cux_nInJuxc.o \
	 r_cxb_nInJcxb.o r_cxbx_nInJcxbx.o r_cxc_nInJbbx.o \
	 r_cxc_nInJccx.o r_cxc_nInJddx.o r_cxc_nInJgg.o \
	 r_cxc_nInJssx.o r_cxc_nInJuux.o r_cxcx_nInJcxcx.o \
	 r_cxd_nInJdcx.o r_cxdx_nInJdxcx.o r_cxg_nInJcxg.o \
	 r_cxs_nInJcxs.o r_cxsx_nInJcxsx.o r_cxu_nInJucx.o \
	 r_cxux_nInJuxcx.o r_db_nInJdb.o r_dbx_nInJdbx.o \
	 r_dc_nInJdc.o r_dcx_nInJdcx.o r_dd_nInJdd.o \
	 r_ddx_nInJbbx.o r_ddx_nInJccx.o r_ddx_nInJddx.o \
	 r_ddx_nInJgg.o r_ddx_nInJssx.o r_ddx_nInJuux.o \
	 r_dg_nInJdg.o r_ds_nInJds.o r_dsx_nInJdsx.o \
	 r_du_nInJdu.o r_dux_nInJdux.o r_dxb_nInJdxb.o \
	 r_dxbx_nInJdxbx.o r_dxc_nInJdxc.o r_dxcx_nInJdxcx.o \
	 r_dxd_nInJbbx.o r_dxd_nInJccx.o r_dxd_nInJddx.o \
	 r_dxd_nInJgg.o r_dxd_nInJssx.o r_dxd_nInJuux.o \
	 r_dxdx_nInJdxdx.o r_dxg_nInJdxg.o r_dxs_nInJdxs.o \
	 r_dxsx_nInJdxsx.o r_dxu_nInJdxu.o r_dxux_nInJdxux.o \
	 r_gb_nInJbg.o r_gbx_nInJbxg.o r_gc_nInJcg.o \
	 r_gcx_nInJcxg.o r_gd_nInJdg.o r_gdx_nInJdxg.o \
	 r_gg_nInJbbx.o r_gg_nInJccx.o r_gg_nInJddx.o \
	 r_gg_nInJssx.o r_gg_nInJuux.o r_gs_nInJsg.o \
	 r_gsx_nInJsxg.o r_gu_nInJug.o r_gux_nInJuxg.o \
	 r_sb_nInJsb.o r_sbx_nInJsbx.o r_sc_nInJcs.o \
	 r_scx_nInJcxs.o r_sd_nInJds.o r_sdx_nInJdxs.o \
	 r_sg_nInJsg.o r_ss_nInJss.o r_ssx_nInJbbx.o \
	 r_ssx_nInJccx.o r_ssx_nInJddx.o r_ssx_nInJgg.o \
	 r_ssx_nInJssx.o r_ssx_nInJuux.o r_su_nInJus.o \
	 r_sux_nInJuxs.o r_sxb_nInJsxb.o r_sxbx_nInJsxbx.o \
	 r_sxc_nInJcsx.o r_sxcx_nInJcxsx.o r_sxd_nInJdsx.o \
	 r_sxdx_nInJdxsx.o r_sxg_nInJsxg.o r_sxs_nInJbbx.o \
	 r_sxs_nInJccx.o r_sxs_nInJddx.o r_sxs_nInJgg.o \
	 r_sxs_nInJssx.o r_sxs_nInJuux.o r_sxsx_nInJsxsx.o \
	 r_sxu_nInJusx.o r_sxux_nInJuxsx.o r_ub_nInJub.o \
	 r_ubx_nInJubx.o r_uc_nInJuc.o r_ucx_nInJucx.o \
	 r_ud_nInJdu.o r_udx_nInJdxu.o r_ug_nInJug.o \
	 r_us_nInJus.o r_usx_nInJusx.o r_uu_nInJuu.o \
	 r_uux_nInJbbx.o r_uux_nInJccx.o r_uux_nInJddx.o \
	 r_uux_nInJgg.o r_uux_nInJssx.o r_uux_nInJuux.o \
	 r_uxb_nInJuxb.o r_uxbx_nInJuxbx.o r_uxc_nInJuxc.o \
	 r_uxcx_nInJuxcx.o r_uxd_nInJdux.o r_uxdx_nInJdxux.o \
	 r_uxg_nInJuxg.o r_uxs_nInJuxs.o r_uxsx_nInJuxsx.o \
	 r_uxu_nInJbbx.o r_uxu_nInJccx.o r_uxu_nInJddx.o \
	 r_uxu_nInJgg.o r_uxu_nInJssx.o r_uxu_nInJuux.o \
	 r_uxux_nInJuxux.o sborn_cl_001.o sborn_cl_002.o \
	 sborn_cl_003.o sborn_cl_004.o sborn_cl_005.o \
	 sborn_cl_006.o sborn_cl_007.o sborn_cl_008.o \
	 sborn_cl_021.o sborn_cl_022.o sborn_cl_023.o \
	 sborn_cl_024.o \
	 sborn_sf_001.o sborn_sf_002.o sborn_sf_003.o \
	 sborn_sf_004.o sborn_sf_005.o sborn_sf_006.o \
	 sborn_sf_007.o sborn_sf_008.o sborn_sf_021.o \
	 sborn_sf_022.o sborn_sf_023.o sborn_sf_024.o

# on-shell subtraction scheme
MADOBJ +=

### Virtual Amplitudes in Formcalc_Virtuals/ ###
VIRTUALS += CalcRenConst.o qdbarg_nInJqdbar_abbr0a.o qdbarg_nInJqdbar_abbr0h.o \
	 qdbarg_nInJqdbar_abbr0s.o qdbarg_nInJqdbar_born.o qdbarg_nInJqdbar_born_Sfe6.o \
	 qdbarg_nInJqdbar_SquaredME.o qdg_nInJqd_abbr0a.o qdg_nInJqd_abbr0h.o \
	 qdg_nInJqd_abbr0s.o qdg_nInJqd_born.o qdg_nInJqd_born_Sfe6.o \
	 qdg_nInJqd_SquaredME.o qdqdbar_nInJg_abbr0a.o qdqdbar_nInJg_abbr0h.o \
	 qdqdbar_nInJg_abbr0s.o qdqdbar_nInJg_born.o qdqdbar_nInJg_born_Sfe6.o \
	 qdqdbar_nInJg_SquaredME.o qubarg_nInJqubar_abbr0a.o qubarg_nInJqubar_abbr0h.o \
	 qubarg_nInJqubar_abbr0s.o qubarg_nInJqubar_born.o qubarg_nInJqubar_born_Sfe6.o \
	 qubarg_nInJqubar_SquaredME.o qug_nInJqu_abbr0a.o qug_nInJqu_abbr0h.o \
	 qug_nInJqu_abbr0s.o qug_nInJqu_born.o qug_nInJqu_born_Sfe6.o \
	 qug_nInJqu_SquaredME.o ququbar_nInJg_abbr0a.o ququbar_nInJg_abbr0h.o \
	 ququbar_nInJg_abbr0s.o ququbar_nInJg_born.o ququbar_nInJg_born_Sfe6.o \
	 ququbar_nInJg_SquaredME.o


REALS += gg_nInJqdqdbar_abbr0a.o gg_nInJqdqdbar_abbr0h.o gg_nInJqdqdbar_abbr0s.o \
	 gg_nInJqdqdbar_real.o gg_nInJqdqdbar_real_Sfe7.o gg_nInJqdqdbar_SquaredME.o \
	 gg_nInJququbar_abbr0a.o gg_nInJququbar_abbr0h.o gg_nInJququbar_abbr0s.o \
	 gg_nInJququbar_real.o gg_nInJququbar_real_Sfe7.o gg_nInJququbar_SquaredME.o \
	 gqdbar_nInJqdbarg_abbr0a.o gqdbar_nInJqdbarg_abbr0h1.o gqdbar_nInJqdbarg_abbr0h2.o \
	 gqdbar_nInJqdbarg_abbr0s.o gqdbar_nInJqdbarg_real.o gqdbar_nInJqdbarg_real_Sfe7.o \
	 gqdbar_nInJqdbarg_SquaredME.o gqd_nInJqdg_abbr0a.o gqd_nInJqdg_abbr0h1.o \
	 gqd_nInJqdg_abbr0h2.o gqd_nInJqdg_abbr0s.o gqd_nInJqdg_real.o \
	 gqd_nInJqdg_real_Sfe7.o gqd_nInJqdg_SquaredME.o gqubar_nInJqubarg_abbr0a.o \
	 gqubar_nInJqubarg_abbr0h1.o gqubar_nInJqubarg_abbr0h2.o gqubar_nInJqubarg_abbr0s.o \
	 gqubar_nInJqubarg_real.o gqubar_nInJqubarg_real_Sfe7.o gqubar_nInJqubarg_SquaredME.o \
	 gqu_nInJqug_abbr0a.o gqu_nInJqug_abbr0h1.o gqu_nInJqug_abbr0h2.o \
	 gqu_nInJqug_abbr0s.o gqu_nInJqug_real.o gqu_nInJqug_real_Sfe7.o \
	 gqu_nInJqug_SquaredME.o qdbarqdbar_nInJqdbarqdbar_abbr0a.o qdbarqdbar_nInJqdbarqdbar_abbr0h.o \
	 qdbarqdbar_nInJqdbarqdbar_abbr0s.o qdbarqdbar_nInJqdbarqdbar_real.o qdbarqdbar_nInJqdbarqdbar_real_Sfe7.o \
	 qdbarqdbar_nInJqdbarqdbar_real_Sfe7_Sfe8.o qdbarqdbar_nInJqdbarqdbar_SquaredME.o qdbarqd_nInJgg_abbr0a.o \
	 qdbarqd_nInJgg_abbr0h1.o qdbarqd_nInJgg_abbr0h2.o qdbarqd_nInJgg_abbr0s.o \
	 qdbarqd_nInJgg_real.o qdbarqd_nInJgg_real_Sfe7.o qdbarqd_nInJgg_SquaredME.o \
	 qdbarqd_nInJqdbarqd_abbr0a.o qdbarqd_nInJqdbarqd_abbr0h.o qdbarqd_nInJqdbarqd_abbr0s.o \
	 qdbarqd_nInJqdbarqd_real.o qdbarqd_nInJqdbarqd_real_Sfe7.o qdbarqd_nInJqdbarqd_real_Sfe7_Sfe8.o \
	 qdbarqd_nInJqdbarqd_SquaredME.o qdbarqd_nInJqdqdbar_abbr0a.o qdbarqd_nInJqdqdbar_abbr0h.o \
	 qdbarqd_nInJqdqdbar_abbr0s.o qdbarqd_nInJqdqdbar_real.o qdbarqd_nInJqdqdbar_real_Sfe7.o \
	 qdbarqd_nInJqdqdbar_real_Sfe7_Sfe8.o qdbarqd_nInJqdqdbar_SquaredME.o qdbarqd_nInJququbar_abbr0a.o \
	 qdbarqd_nInJququbar_abbr0h.o qdbarqd_nInJququbar_abbr0s.o qdbarqd_nInJququbar_real.o \
	 qdbarqd_nInJququbar_real_Sfe7.o qdbarqd_nInJququbar_real_Sfe7_Sfe8.o qdbarqd_nInJququbar_SquaredME.o \
	 qdbarqubar_nInJqdbarqubar_abbr0a.o qdbarqubar_nInJqdbarqubar_abbr0h.o qdbarqubar_nInJqdbarqubar_abbr0s.o \
	 qdbarqubar_nInJqdbarqubar_real.o qdbarqubar_nInJqdbarqubar_real_Sfe7.o qdbarqubar_nInJqdbarqubar_real_Sfe7_Sfe8.o \
	 qdbarqubar_nInJqdbarqubar_SquaredME.o qdbarqubar_nInJqubarqdbar_abbr0a.o qdbarqubar_nInJqubarqdbar_abbr0h.o \
	 qdbarqubar_nInJqubarqdbar_abbr0s.o qdbarqubar_nInJqubarqdbar_real.o qdbarqubar_nInJqubarqdbar_real_Sfe7.o \
	 qdbarqubar_nInJqubarqdbar_real_Sfe7_Sfe8.o qdbarqubar_nInJqubarqdbar_SquaredME.o qdbarqu_nInJqdbarqu_abbr0a.o \
	 qdbarqu_nInJqdbarqu_abbr0h.o qdbarqu_nInJqdbarqu_abbr0s.o qdbarqu_nInJqdbarqu_real.o \
	 qdbarqu_nInJqdbarqu_real_Sfe7.o qdbarqu_nInJqdbarqu_real_Sfe7_Sfe8.o qdbarqu_nInJqdbarqu_SquaredME.o \
	 qdbarqu_nInJquqdbar_abbr0a.o qdbarqu_nInJquqdbar_abbr0h.o qdbarqu_nInJquqdbar_abbr0s.o \
	 qdbarqu_nInJquqdbar_real.o qdbarqu_nInJquqdbar_real_Sfe7.o qdbarqu_nInJquqdbar_real_Sfe7_Sfe8.o \
	 qdbarqu_nInJquqdbar_SquaredME.o qdqdbar_nInJgg_abbr0a.o qdqdbar_nInJgg_abbr0h1.o \
	 qdqdbar_nInJgg_abbr0h2.o qdqdbar_nInJgg_abbr0s.o qdqdbar_nInJgg_real.o \
	 qdqdbar_nInJgg_real_Sfe7.o qdqdbar_nInJgg_SquaredME.o qdqdbar_nInJqdbarqd_abbr0a.o \
	 qdqdbar_nInJqdbarqd_abbr0h.o qdqdbar_nInJqdbarqd_abbr0s.o qdqdbar_nInJqdbarqd_real.o \
	 qdqdbar_nInJqdbarqd_real_Sfe7.o qdqdbar_nInJqdbarqd_real_Sfe7_Sfe8.o qdqdbar_nInJqdbarqd_SquaredME.o \
	 qdqdbar_nInJqdqdbar_abbr0a.o qdqdbar_nInJqdqdbar_abbr0h.o qdqdbar_nInJqdqdbar_abbr0s.o \
	 qdqdbar_nInJqdqdbar_real.o qdqdbar_nInJqdqdbar_real_Sfe7.o qdqdbar_nInJqdqdbar_real_Sfe7_Sfe8.o \
	 qdqdbar_nInJqdqdbar_SquaredME.o qdqdbar_nInJququbar_abbr0a.o qdqdbar_nInJququbar_abbr0h.o \
	 qdqdbar_nInJququbar_abbr0s.o qdqdbar_nInJququbar_real.o qdqdbar_nInJququbar_real_Sfe7.o \
	 qdqdbar_nInJququbar_real_Sfe7_Sfe8.o qdqdbar_nInJququbar_SquaredME.o qdqd_nInJqdqd_abbr0a.o \
	 qdqd_nInJqdqd_abbr0h.o qdqd_nInJqdqd_abbr0s.o qdqd_nInJqdqd_real.o \
	 qdqd_nInJqdqd_real_Sfe7.o qdqd_nInJqdqd_real_Sfe7_Sfe8.o qdqd_nInJqdqd_SquaredME.o \
	 qdqubar_nInJqdqubar_abbr0a.o qdqubar_nInJqdqubar_abbr0h.o qdqubar_nInJqdqubar_abbr0s.o \
	 qdqubar_nInJqdqubar_real.o qdqubar_nInJqdqubar_real_Sfe7.o qdqubar_nInJqdqubar_real_Sfe7_Sfe8.o \
	 qdqubar_nInJqdqubar_SquaredME.o qdqubar_nInJqubarqd_abbr0a.o qdqubar_nInJqubarqd_abbr0h.o \
	 qdqubar_nInJqubarqd_abbr0s.o qdqubar_nInJqubarqd_real.o qdqubar_nInJqubarqd_real_Sfe7.o \
	 qdqubar_nInJqubarqd_real_Sfe7_Sfe8.o qdqubar_nInJqubarqd_SquaredME.o qdqu_nInJqdqu_abbr0a.o \
	 qdqu_nInJqdqu_abbr0h.o qdqu_nInJqdqu_abbr0s.o qdqu_nInJqdqu_real.o \
	 qdqu_nInJqdqu_real_Sfe7.o qdqu_nInJqdqu_real_Sfe7_Sfe8.o qdqu_nInJqdqu_SquaredME.o \
	 qdqu_nInJquqd_abbr0a.o qdqu_nInJquqd_abbr0h.o qdqu_nInJquqd_abbr0s.o \
	 qdqu_nInJquqd_real.o qdqu_nInJquqd_real_Sfe7.o qdqu_nInJquqd_real_Sfe7_Sfe8.o \
	 qdqu_nInJquqd_SquaredME.o qubarqdbar_nInJqdbarqubar_abbr0a.o qubarqdbar_nInJqdbarqubar_abbr0h.o \
	 qubarqdbar_nInJqdbarqubar_abbr0s.o qubarqdbar_nInJqdbarqubar_real.o qubarqdbar_nInJqdbarqubar_real_Sfe7.o \
	 qubarqdbar_nInJqdbarqubar_real_Sfe7_Sfe8.o qubarqdbar_nInJqdbarqubar_SquaredME.o qubarqdbar_nInJqubarqdbar_abbr0a.o \
	 qubarqdbar_nInJqubarqdbar_abbr0h.o qubarqdbar_nInJqubarqdbar_abbr0s.o qubarqdbar_nInJqubarqdbar_real.o \
	 qubarqdbar_nInJqubarqdbar_real_Sfe7.o qubarqdbar_nInJqubarqdbar_real_Sfe7_Sfe8.o qubarqdbar_nInJqubarqdbar_SquaredME.o \
	 qubarqd_nInJqdqubar_abbr0a.o qubarqd_nInJqdqubar_abbr0h.o qubarqd_nInJqdqubar_abbr0s.o \
	 qubarqd_nInJqdqubar_real.o qubarqd_nInJqdqubar_real_Sfe7.o qubarqd_nInJqdqubar_real_Sfe7_Sfe8.o \
	 qubarqd_nInJqdqubar_SquaredME.o qubarqd_nInJqubarqd_abbr0a.o qubarqd_nInJqubarqd_abbr0h.o \
	 qubarqd_nInJqubarqd_abbr0s.o qubarqd_nInJqubarqd_real.o qubarqd_nInJqubarqd_real_Sfe7.o \
	 qubarqd_nInJqubarqd_real_Sfe7_Sfe8.o qubarqd_nInJqubarqd_SquaredME.o qubarqubar_nInJqubarqubar_abbr0a.o \
	 qubarqubar_nInJqubarqubar_abbr0h.o qubarqubar_nInJqubarqubar_abbr0s.o qubarqubar_nInJqubarqubar_real.o \
	 qubarqubar_nInJqubarqubar_real_Sfe7.o qubarqubar_nInJqubarqubar_real_Sfe7_Sfe8.o qubarqubar_nInJqubarqubar_SquaredME.o \
	 qubarqu_nInJgg_abbr0a.o qubarqu_nInJgg_abbr0h1.o qubarqu_nInJgg_abbr0h2.o \
	 qubarqu_nInJgg_abbr0s.o qubarqu_nInJgg_real.o qubarqu_nInJgg_real_Sfe7.o \
	 qubarqu_nInJgg_SquaredME.o qubarqu_nInJqdqdbar_abbr0a.o qubarqu_nInJqdqdbar_abbr0h.o \
	 qubarqu_nInJqdqdbar_abbr0s.o qubarqu_nInJqdqdbar_real.o qubarqu_nInJqdqdbar_real_Sfe7.o \
	 qubarqu_nInJqdqdbar_real_Sfe7_Sfe8.o qubarqu_nInJqdqdbar_SquaredME.o qubarqu_nInJqubarqu_abbr0a.o \
	 qubarqu_nInJqubarqu_abbr0h.o qubarqu_nInJqubarqu_abbr0s.o qubarqu_nInJqubarqu_real.o \
	 qubarqu_nInJqubarqu_real_Sfe7.o qubarqu_nInJqubarqu_real_Sfe7_Sfe8.o qubarqu_nInJqubarqu_SquaredME.o \
	 qubarqu_nInJququbar_abbr0a.o qubarqu_nInJququbar_abbr0h.o qubarqu_nInJququbar_abbr0s.o \
	 qubarqu_nInJququbar_real.o qubarqu_nInJququbar_real_Sfe7.o qubarqu_nInJququbar_real_Sfe7_Sfe8.o \
	 qubarqu_nInJququbar_SquaredME.o quqdbar_nInJqdbarqu_abbr0a.o quqdbar_nInJqdbarqu_abbr0h.o \
	 quqdbar_nInJqdbarqu_abbr0s.o quqdbar_nInJqdbarqu_real.o quqdbar_nInJqdbarqu_real_Sfe7.o \
	 quqdbar_nInJqdbarqu_real_Sfe7_Sfe8.o quqdbar_nInJqdbarqu_SquaredME.o quqdbar_nInJquqdbar_abbr0a.o \
	 quqdbar_nInJquqdbar_abbr0h.o quqdbar_nInJquqdbar_abbr0s.o quqdbar_nInJquqdbar_real.o \
	 quqdbar_nInJquqdbar_real_Sfe7.o quqdbar_nInJquqdbar_real_Sfe7_Sfe8.o quqdbar_nInJquqdbar_SquaredME.o \
	 quqd_nInJqdqu_abbr0a.o quqd_nInJqdqu_abbr0h.o quqd_nInJqdqu_abbr0s.o \
	 quqd_nInJqdqu_real.o quqd_nInJqdqu_real_Sfe7.o quqd_nInJqdqu_real_Sfe7_Sfe8.o \
	 quqd_nInJqdqu_SquaredME.o quqd_nInJquqd_abbr0a.o quqd_nInJquqd_abbr0h.o \
	 quqd_nInJquqd_abbr0s.o quqd_nInJquqd_real.o quqd_nInJquqd_real_Sfe7.o \
	 quqd_nInJquqd_real_Sfe7_Sfe8.o quqd_nInJquqd_SquaredME.o ququbar_nInJgg_abbr0a.o \
	 ququbar_nInJgg_abbr0h1.o ququbar_nInJgg_abbr0h2.o ququbar_nInJgg_abbr0s.o \
	 ququbar_nInJgg_real.o ququbar_nInJgg_real_Sfe7.o ququbar_nInJgg_SquaredME.o \
	 ququbar_nInJqdqdbar_abbr0a.o ququbar_nInJqdqdbar_abbr0h.o ququbar_nInJqdqdbar_abbr0s.o \
	 ququbar_nInJqdqdbar_real.o ququbar_nInJqdqdbar_real_Sfe7.o ququbar_nInJqdqdbar_real_Sfe7_Sfe8.o \
	 ququbar_nInJqdqdbar_SquaredME.o ququbar_nInJqubarqu_abbr0a.o ququbar_nInJqubarqu_abbr0h.o \
	 ququbar_nInJqubarqu_abbr0s.o ququbar_nInJqubarqu_real.o ququbar_nInJqubarqu_real_Sfe7.o \
	 ququbar_nInJqubarqu_real_Sfe7_Sfe8.o ququbar_nInJqubarqu_SquaredME.o ququbar_nInJququbar_abbr0a.o \
	 ququbar_nInJququbar_abbr0h.o ququbar_nInJququbar_abbr0s.o ququbar_nInJququbar_real.o \
	 ququbar_nInJququbar_real_Sfe7.o ququbar_nInJququbar_real_Sfe7_Sfe8.o ququbar_nInJququbar_SquaredME.o \
	 ququ_nInJququ_abbr0a.o ququ_nInJququ_abbr0h.o ququ_nInJququ_abbr0s.o \
	 ququ_nInJququ_real.o ququ_nInJququ_real_Sfe7.o ququ_nInJququ_real_Sfe7_Sfe8.o \
	 ququ_nInJququ_SquaredME.o



SOURCESMAIN = $(PWHG) $(PDFPACK) $(USER) $(PWHGANALYSIS) $(FUNC) $(VIRTUALS) $(REALS) \
	$(FORMCALC) $(MMADOBJ) $(MADOBJ)

SOURCESLHEF = $(LHEF) $(PWHGANALYSIS) $(MMADOBJ)

SOURCESPYTHIA = $(PYTHIA) $(PWHGANALYSIS) $(MMADOBJ)

SOURCESHERWIG = $(HERWIG) $(PWHGANALYSIS) $(MMADOBJ)
          
# Additional FormCalc Files (maybe custom ones)
#VIRTUALS += BornEps_n1x1.o

########################################################################
## Libraries
### LibSLHA in Tools/SLHALib-2.2 ###
LIBS += $(TOOLS)/libSLHA.a

### LoopTools in Tools/Looptools-2.12 ###
LIBS += $(TOOLS)/libooptools.a

### DHELAS in Tools/DHELAS ###
LIBS += $(TOOLS)/libdhelas3.a

#not necessary anymore
#LIBS += $(LIBGFORTRANPATH)/libgfortranbegin.a
#LIBS += $(LIBGFORTRANPATH)/libgfortran.a
#LIBS += $(LIBSTDCPPPATH)/libstdc++.a

### Libraries for linking c++ code
LIBS += -ldl -lstdc++

########################################################################
## combine all flags, libraries and includes

ALL_FCFLAGS   = $(REC_FCFLAGS) $(OPT) $(WARN) $(CPP) $(DEBUG) $(USRFLAGS) $(CHANNEL)
ALL_FCFLAGS  += -I$(UINCLUDE) -I$(PINCLUDE) -I$(LTINCLUDE) -I$(MDINCLUDE)
ALL_FCFLAGS  += -I$(WORKINGDIR) -I$(VINCLUDE) -I$(SLHAINCLUDE)
ALL_FCFLAGS  += -I$(SINCLUDE) -I$(FRINCLUDE)

ALL_CFLAGS    = $(REC_CFLAGS) $(OPT)
ALL_CFLAGS   += -I$(LTINCLUDE)

ALL_CXXFLAGS  = $(REC_CXXFLAGS) $(OPT)

LINKER = $(CPPFLAGS) $(LIBS) $(LDFLAGS)

HEADERS  = $(wildcard *.h $(UINCLUDE)/*.h  $(PINCLUDE)/*.h $(VINCLUDE)/*.h)
HEADERS += $(wildcard *.h $(LTINCLUDE)/*.h $(SLHAINCLUDE)/*.h)
HEADERS += $(wildcard *.h $(MDINCLUDE)/*.h $(SINCLUDE)/*.h)
HEADERS += $(wildcard *.h $(FRINCLUDE)/*.h)
HEADERS += $(wildcard *.inc $(MDINCLUDE)/*.inc $(UINCLUDE)/*.inc)
     
########################################################################
## Rules, generate objects

%.o: %.f $(HEADERS)
	@echo "Compiling:" $<
	@$(FC) $(ALL_FCFLAGS) -c -o build/$@ $<

%.o: %.F $(HEADERS)
	@echo "Compiling:" $<
	@$(FC) $(ALL_FCFLAGS) -c -o build/$@ $<

pythiafortran.o: pythiafortran.cc
	@echo "Compiling:" $<
	@$(CXX) $(ALL_CXXFLAGS) $(PYCXXFLAGS) -c -o build/$@ $^

fastjetfortran.o: fastjetfortran.cc
	@echo "Compiling:" $<
	@$(CXX) $(ALL_CXXFLAGS) $(FJCXXFLAGS) -c -o build/$@ $^

%.o: %.c $(HEADERS)
	@echo "Compiling:" $<
	@$(CC) $(ALL_CFLAGS) -c -o build/$@ $<

########################################################################
## Rules, link
## type make -j4 [rule] to speed up the compilation
# TODO: Try: $(addprefix ./build/, $(SOURCESMAIN))

pwhg_main_ninj+jet: $(SOURCESMAIN)
	$(FC) $(ALL_FCFLAGS) $(patsubst %,build/%,$(SOURCESMAIN)) $(LINKER) -o $@

lhef_analysis:$(SOURCESLHEF)
	$(FC) $(ALL_FCFLAGS) $(patsubst %,build/%,$(SOURCESLHEF)) $(LINKER) -o $@ 

main-PYTHIA-lhef: $(SOURCESPYTHIA)
	$(FC) $(ALL_FCFLAGS) $(patsubst %,build/%,$(SOURCESPYTHIA)) $(LINKER) -o $@

# not implemented yet
main-HERWIG-lhef: $(SOURCESHERWIG)
	$(FC) $(ALL_FCFLAGS) $(patsubst %,build/%,$(SOURCESHERWIG)) $(LINKER) -o $@

merge-pwg-stat: 
	$(FC) $(TOOLS)/programs/merge-pwg-stat.f -o $@

merge-data: 
	$(FC) $(TOOLS)/programs/merge-data.f -o $@

copy-scripts:
	cp $(SCRIPTS)/runparallel.sh  $(SUBWORKINGDIR) && \
	cp $(SCRIPTS)/clean_rundir.sh $(SUBWORKINGDIR) && \
	cp $(SCRIPTS)/reweight.sh     $(SUBWORKINGDIR) && \
	cp $(SCRIPTS)/merge.sh        $(SUBWORKINGDIR)

all: pwhg_main_ninj+jet copy-scripts merge-pwg-stat merge-data lhef_analysis main-PYTHIA-lhef

clean:
	rm -f build/*.o pwhg_main_ninj+jet lhef_analysis main-PYTHIA-lhef merge-pwg-stat merge-data runparallel.sh clean_rundir.sh reweight.sh merge.sh

clean-results:
	find ./testrun ! \( -name '*.slha' -o -name '*.input' -o -name 'pwgseeds.dat' \) \
	-type f -exec rm -f {} +
	
clean-all: clean clean-results
	rm -f pwhg_main_ninj+jet lhef_analysis main-PYTHIA-lhef

do-pwhg do: pwhg_main_ninj+jet copy-scripts merge-pwg-stat merge-data
	cd testrun && ../pwhg_main_ninj+jet

do-lhef: lhef_analysis
	cd testrun && ../lhef_analysis
	
do-pythia: main-PYTHIA-lhef
	cd testrun && ../main-PYTHIA-lhef
	
do-all: do-pwhg do-lhef do-pythia
	
########################################################################
#                       -*- End of Makefile -*-                        #
########################################################################
