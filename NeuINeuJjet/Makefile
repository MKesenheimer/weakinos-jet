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
USRFLAGS += -D MAD_BORN

## Treatment of quark masses and squark widths
# The current version uses vanishing masses for the incoming quarks and
# zero widths for the internal squarks.
# For later software versions the possibility exists to make the
# calculations with finite b-quark mass or finite squark widths.
USRFLAGS += -D NEGLECTBMASS
USRFLAGS += -D NEGLECT_SWIDTH
# If you want to change these flags, you probably have to change a lot
# for the FormCalc amplitudes. For example, the b-quark amplitudes are 
# currently calculated by crossing relations from d-quark amplitudes.
# Since FormCalc does not use squark widths, you have to introduce
# squark widths by hand.

########################################################################
#           -*- no editing is required below this line -*-             #
########################################################################

########################################################################
## Runtime flags which should not be changed (don't touch this unless
## you know what you do!) 
CHANNEL += -D NIXJ

########################################################################
## Paths

WORKINGDIR = $(shell pwd)/..
PWHGDIR    = $(WORKINGDIR)/..
SUBWORKINGDIR = $(shell pwd)

# directories
TOOLS   = $(WORKINGDIR)/Tools
REAL    = $(SUBWORKINGDIR)/MadGraph_Reals
VIRT    = $(SUBWORKINGDIR)/FormCalc_Virtuals
VIRTSUB = $(VIRT)/udbar_n1x1_squaredme $(VIRT)/dubar_n1x1_squaredme
REALSUB  = $(REAL)/SubProcesses_nIxJ
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

########################################################################
## search for the files and set paths

vpath %.f $(PWHGDIR) $(WORKINGDIR) $(TOOLS)/functions $(DHELAS) $(PY) $(PYF) $(HER)
vpath %.F $(WORKINGDIR) $(TOOLS)/FormCalcTools $(DHELAS)
vpath %.F $(VIRT)/RenConsts $(VIRTSUB)
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
USER = Born.o Born_phsp.o Born_lh.o Reals.o Reals_lh.o Virtuals.o \
       init_processes.o init_couplings.o \
       phi1_2.o version.o

### Functions in Tools/functions/
FUNC = funcprocess.o Print.o funcbasic.o off_to_on.o

### MadGraph mandatory files
MMADOBJ = couplings.o hardstop.o printout.o read_slha.o \
	set_general_couplings.o 

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

# possible modified POWHEG-files
PWHG += cernroutines_mod.o find_regions.o gen_real_phsp.o	\
	sigborn.o sigreal.o sigsoftvirt.o			\
	sigvirtual.o bbinit.o LesHouches.o 		\
	LesHouchesreg.o gen_index.o lhefwrite.o		\
	gen_radiation.o sigremnants.o pwhgreweight.o \
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
  #PYTHIA += pythia6428.o
endif

### HERWIG ###
# this is work in progress
HERWIG = main-HERWIG.o setup-HERWIG-lhef.o herwig6510.o boostrot.o 	\
     opencount.o powheginput.o pwhg_bookhist-multi.o random.o		\
     lhefread.o newunit_mod.o pwhg_analysis_driver.o 			\
     funcprocess.o init_couplings.o cernroutines_mod.o			\
     bra_ket_subroutines.o off_to_on.o funcbasic.o

# MadGraph born amplitudes
MADOBJ += sborn_proc.o
MADOBJ += b_cl_001_001.o b_cl_001_002.o b_cl_001_003.o \
	b_cl_002_001.o b_cl_002_002.o b_cl_002_003.o \
	b_cl_003_001.o b_cl_003_002.o b_cl_003_003.o \
	b_cl_004_001.o b_cl_004_002.o b_cl_004_003.o \
	b_cl_005_001.o b_cl_005_002.o b_cl_005_003.o \
	b_cl_006_001.o b_cl_006_002.o b_cl_006_003.o \
	b_cl_007_001.o b_cl_007_002.o b_cl_007_003.o \
	b_cl_008_001.o b_cl_008_002.o b_cl_008_003.o \
	b_cl_009_001.o b_cl_009_002.o b_cl_009_003.o \
	b_cl_010_001.o b_cl_010_002.o b_cl_010_003.o \
	b_cl_011_001.o b_cl_011_002.o b_cl_011_003.o \
	b_cl_012_001.o b_cl_012_002.o b_cl_012_003.o \
	b_cl_013_001.o b_cl_013_002.o b_cl_013_003.o \
	b_cl_014_001.o b_cl_014_002.o b_cl_014_003.o \
	b_cl_015_001.o b_cl_015_002.o b_cl_015_003.o \
	b_cl_016_001.o b_cl_016_002.o b_cl_016_003.o \
	b_cl_017_001.o b_cl_017_002.o b_cl_017_003.o \
	b_cl_018_001.o b_cl_018_002.o b_cl_018_003.o \
	b_cl_019_001.o b_cl_019_002.o b_cl_019_003.o \
	b_cl_020_001.o b_cl_020_002.o b_cl_020_003.o \
	b_cl_021_001.o b_cl_021_002.o b_cl_021_003.o \
	b_cl_022_001.o b_cl_022_002.o b_cl_022_003.o \
	b_cl_023_001.o b_cl_023_002.o b_cl_023_003.o \
	b_cl_024_001.o b_cl_024_002.o b_cl_024_003.o \
	b_cl_025_001.o b_cl_025_002.o b_cl_025_003.o \
	b_cl_026_001.o b_cl_026_002.o b_cl_026_003.o \
	b_cl_027_001.o b_cl_027_002.o b_cl_027_003.o \
	b_cl_028_001.o b_cl_028_002.o b_cl_028_003.o \
	b_cl_029_001.o b_cl_029_002.o b_cl_029_003.o \
	b_cl_030_001.o b_cl_030_002.o b_cl_030_003.o \
	b_sf_001_004.o b_sf_001_005.o b_sf_001_006.o b_sf_001_007.o b_sf_001_008.o b_sf_001_009.o \
	b_sf_002_004.o b_sf_002_005.o b_sf_002_006.o b_sf_002_007.o b_sf_002_008.o b_sf_002_009.o \
	b_sf_003_004.o b_sf_003_005.o b_sf_003_006.o b_sf_003_007.o b_sf_003_008.o b_sf_003_009.o \
	b_sf_004_004.o b_sf_004_005.o b_sf_004_006.o b_sf_004_007.o b_sf_004_008.o b_sf_004_009.o \
	b_sf_005_004.o b_sf_005_005.o b_sf_005_006.o b_sf_005_007.o b_sf_005_008.o b_sf_005_009.o \
	b_sf_006_004.o b_sf_006_005.o b_sf_006_006.o b_sf_006_007.o b_sf_006_008.o b_sf_006_009.o \
	b_sf_007_004.o b_sf_007_005.o b_sf_007_006.o b_sf_007_007.o b_sf_007_008.o b_sf_007_009.o \
	b_sf_008_004.o b_sf_008_005.o b_sf_008_006.o b_sf_008_007.o b_sf_008_008.o b_sf_008_009.o \
	b_sf_009_004.o b_sf_009_005.o b_sf_009_006.o b_sf_009_007.o b_sf_009_008.o b_sf_009_009.o \
	b_sf_010_004.o b_sf_010_005.o b_sf_010_006.o b_sf_010_007.o b_sf_010_008.o b_sf_010_009.o \
	b_sf_011_004.o b_sf_011_005.o b_sf_011_006.o b_sf_011_007.o b_sf_011_008.o b_sf_011_009.o \
	b_sf_012_004.o b_sf_012_005.o b_sf_012_006.o b_sf_012_007.o b_sf_012_008.o b_sf_012_009.o \
	b_sf_013_004.o b_sf_013_005.o b_sf_013_006.o b_sf_013_007.o b_sf_013_008.o b_sf_013_009.o \
	b_sf_014_004.o b_sf_014_005.o b_sf_014_006.o b_sf_014_007.o b_sf_014_008.o b_sf_014_009.o \
	b_sf_015_004.o b_sf_015_005.o b_sf_015_006.o b_sf_015_007.o b_sf_015_008.o b_sf_015_009.o \
	b_sf_016_004.o b_sf_016_005.o b_sf_016_006.o b_sf_016_007.o b_sf_016_008.o b_sf_016_009.o \
	b_sf_017_004.o b_sf_017_005.o b_sf_017_006.o b_sf_017_007.o b_sf_017_008.o b_sf_017_009.o \
	b_sf_018_004.o b_sf_018_005.o b_sf_018_006.o b_sf_018_007.o b_sf_018_008.o b_sf_018_009.o \
	b_sf_019_004.o b_sf_019_005.o b_sf_019_006.o b_sf_019_007.o b_sf_019_008.o b_sf_019_009.o \
	b_sf_020_004.o b_sf_020_005.o b_sf_020_006.o b_sf_020_007.o b_sf_020_008.o b_sf_020_009.o \
	b_sf_021_004.o b_sf_021_005.o b_sf_021_006.o b_sf_021_007.o b_sf_021_008.o b_sf_021_009.o \
	b_sf_022_004.o b_sf_022_005.o b_sf_022_006.o b_sf_022_007.o b_sf_022_008.o b_sf_022_009.o \
	b_sf_023_004.o b_sf_023_005.o b_sf_023_006.o b_sf_023_007.o b_sf_023_008.o b_sf_023_009.o \
	b_sf_024_004.o b_sf_024_005.o b_sf_024_006.o b_sf_024_007.o b_sf_024_008.o b_sf_024_009.o \
	b_sf_025_004.o b_sf_025_005.o b_sf_025_006.o b_sf_025_007.o b_sf_025_008.o b_sf_025_009.o \
	b_sf_026_004.o b_sf_026_005.o b_sf_026_006.o b_sf_026_007.o b_sf_026_008.o b_sf_026_009.o \
	b_sf_027_004.o b_sf_027_005.o b_sf_027_006.o b_sf_027_007.o b_sf_027_008.o b_sf_027_009.o \
	b_sf_028_004.o b_sf_028_005.o b_sf_028_006.o b_sf_028_007.o b_sf_028_008.o b_sf_028_009.o \
	b_sf_029_004.o b_sf_029_005.o b_sf_029_006.o b_sf_029_007.o b_sf_029_008.o b_sf_029_009.o \
	b_sf_030_004.o b_sf_030_005.o b_sf_030_006.o b_sf_030_007.o b_sf_030_008.o b_sf_030_009.o 
	
MADOBJ += sborn_cl_001.o sborn_cl_002.o sborn_cl_003.o \
	sborn_cl_004.o sborn_cl_005.o sborn_cl_006.o \
	sborn_cl_007.o sborn_cl_008.o sborn_cl_009.o \
	sborn_cl_010.o sborn_cl_011.o sborn_cl_012.o \
	sborn_cl_013.o sborn_cl_014.o sborn_cl_015.o \
	sborn_cl_016.o sborn_cl_017.o sborn_cl_018.o \
	sborn_cl_019.o sborn_cl_020.o sborn_cl_021.o \
	sborn_cl_022.o sborn_cl_023.o sborn_cl_024.o \
	sborn_cl_025.o sborn_cl_026.o sborn_cl_027.o \
	sborn_cl_028.o sborn_cl_029.o sborn_cl_030.o \
	sborn_sf_001.o sborn_sf_002.o sborn_sf_003.o \
	sborn_sf_004.o sborn_sf_005.o sborn_sf_006.o \
	sborn_sf_007.o sborn_sf_008.o sborn_sf_009.o \
	sborn_sf_010.o sborn_sf_011.o sborn_sf_012.o \
	sborn_sf_013.o sborn_sf_014.o sborn_sf_015.o \
	sborn_sf_016.o sborn_sf_017.o sborn_sf_018.o \
	sborn_sf_019.o sborn_sf_020.o sborn_sf_021.o \
	sborn_sf_022.o sborn_sf_023.o sborn_sf_024.o \
	sborn_sf_025.o sborn_sf_026.o sborn_sf_027.o \
	sborn_sf_028.o sborn_sf_029.o sborn_sf_030.o \


	 


# real Amplitudes
MADOBJ +=

### Virtual Amplitudes in Formcalc_Virtuals/ ###
VIRTUALS += CalcRenConst.o udbar_n1x1_abbr0h.o udbar_n1x1_abbr0s.o	\
          udbar_n1x1_abbr1a.o udbar_n1x1_abbr1h.o udbar_n1x1_abbr1s.o	\
          udbar_n1x1_born.o udbar_n1x1_born_Sfe5.o			\
          udbar_n1x1_box_Sfe5.o udbar_n1x1_box_Sfe5_Sfe6.o		\
          udbar_n1x1_self_Sfe5.o udbar_n1x1_self_Sfe5_Sfe6.o		\
          udbar_n1x1_self_Sfe5_Sfe6_Sfe7.o				\
          udbar_n1x1_SquaredME.o udbar_n1x1_vert_Sfe5_Sfe6.o		\
          udbar_n1x1_vert.o udbar_n1x1_vert_Sfe5.o			\
          dubar_n1x1_abbr0h.o dubar_n1x1_abbr0s.o            		\
          dubar_n1x1_abbr1a.o dubar_n1x1_abbr1h.o dubar_n1x1_abbr1s.o	\
          dubar_n1x1_born.o dubar_n1x1_born_Sfe5.o			\
          dubar_n1x1_box_Sfe5.o dubar_n1x1_box_Sfe5_Sfe6.o		\
          dubar_n1x1_self_Sfe5.o dubar_n1x1_self_Sfe5_Sfe6.o		\
          dubar_n1x1_self_Sfe5_Sfe6_Sfe7.o				\
          dubar_n1x1_SquaredME.o dubar_n1x1_vert_Sfe5_Sfe6.o		\
          dubar_n1x1_vert.o dubar_n1x1_vert_Sfe5.o

SOURCESMAIN = $(PWHG) $(PDFPACK) $(USER) $(PWHGANALYSIS) $(FUNC) $(VIRTUALS)  \
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
ALL_FCFLAGS  += -I$(SINCLUDE)

ALL_CFLAGS    = $(REC_CFLAGS) $(OPT)
ALL_CFLAGS   += -I$(LTINCLUDE)

ALL_CXXFLAGS  = $(REC_CXXFLAGS) $(OPT)

LINKER = $(CPPFLAGS) $(LIBS) $(LDFLAGS)

HEADERS  = $(wildcard *.h $(UINCLUDE)/*.h  $(PINCLUDE)/*.h $(VINCLUDE)/*.h)
HEADERS += $(wildcard *.h $(LTINCLUDE)/*.h $(SLHAINCLUDE)/*.h)
HEADERS += $(wildcard *.h $(MDINCLUDE)/*.h $(SINCLUDE)/*.h)
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

pwhg_main_nixj: $(SOURCESMAIN)
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

all: pwhg_main_nixj copy-scripts merge-pwg-stat merge-data lhef_analysis main-PYTHIA-lhef

clean:
	rm -f build/*.o pwhg_main_nixj lhef_analysis main-PYTHIA-lhef merge-pwg-stat merge-data runparallel.sh clean_rundir.sh reweight.sh merge.sh

clean-results:
	find ./testrun ! \( -name '*.slha' -o -name '*.input' -o -name 'pwgseeds.dat' \) \
	-type f -exec rm -f {} +
	
clean-all: clean clean-results
	rm -f pwhg_main_nixj lhef_analysis main-PYTHIA-lhef

do-pwhg do: pwhg_main_nixj copy-scripts merge-pwg-stat merge-data
	cd testrun && ../pwhg_main_nixj

do-lhef: lhef_analysis
	cd testrun && ../lhef_analysis
	
do-pythia: main-PYTHIA-lhef
	cd testrun && ../main-PYTHIA-lhef
	
do-all: do-pwhg do-lhef do-pythia
	
########################################################################
#                       -*- End of Makefile -*-                        #
########################################################################
