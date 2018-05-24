########################################################################
#                          -*- Makefile -*-                            #
########################################################################
# Copyright (C) Matthias Kesenheimer - All Rights Reserved             #
# Written by Matthias Kesenheimer <m.kesenheimer@gmx.net>, 2017        #
########################################################################
#
# General Conventions for Makefiles
SHELL = /bin/sh
.SUFFIXES:
.SUFFIXES: .c .f .F .cc .cpp .h .hh .inc .o .a
.DEFAULT_GOAL := pwhg_main_ninj+jet

########################################################################
## Flags

## Compiler and additional compiler Flags
# use "./configure ifort" first
#FC  = ifort
# use "./configure gfortran" first
FC  = gfortran
CXX = g++
CC  = gcc

# check compiler version
ifeq ($(FC), gfortran)
  VERSIONGTEQ5 = $(shell expr `gfortran -dumpversion | cut -f1 -d.` \>= 5)
  #$(error $(shell expr `gfortran -dumpversion | cut -f1 -d.`))
  ifeq ($(VERSIONGTEQ5), 0)
    $(error gfortran compiler version >=5 required) 
  endif
endif

# p-flag to enable code profiling with gprof: gprof ./pwhg_main* gmon.out > analysis.txt
FCFLAGS  = -pg
CXXFLAGS = -g
CFLAGS   = -pg

# warnings, no -Wtabs, -Wunused
WARNINGS  = -Waliasing -Wampersand -Wconversion -Wsurprising -Wc-binding-type
WARNINGS += -Wintrinsics-std -Wintrinsic-shadow -Wline-truncation -Wtarget-lifetime
WARNINGS += -Wreal-q-constant 
  
# recommended compiler flags
ifeq ($(FC), ifort)
  LDFLAGS  = -pg
  REC_FCFLAGS   = -fpp -extend-source
  REC_FCFLAGS  += $(FCFLAGS)
else ifeq ($(FC), gfortran)
  LDFLAGS  = -ff2c -pg
  REC_FCFLAGS   = -fno-automatic -fno-range-check
  REC_FCFLAGS  += -ffixed-line-length-none -lgfortran -DU77EXT=0 -DQuad=0
  REC_FCFLAGS  += -ff2c -fno-second-underscore $(WARNINGS)
  REC_FCFLAGS  += $(FCFLAGS)
  #$(error $(REC_FCFLAGS))
endif
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

## choose analysis: none, default, decay
ANALYSIS = default

## PDF
## choose PDF: native,lhapdf
## LHAPDF package has to be installed separately
PDF = lhapdf
## path or name of LHAPDF config executable
LHAPDF_CONFIG = lhapdf-config
## static linking? yes, no
STATIC_LHAPDF = no

## FASTJET
## path or name of fastjet config executable
FASTJET_CONFIG = fastjet-config
## static linking? yes, no
STATIC_FJ = no

## choose LoopTools or Collier to evaluate loop integrals (Collier is slower
## but can handle particle widths). (looptools, collier)
LOOPINT = collier
USRFLAGS += -D$(LOOPINT)

## warning for type-conversions -> basically useless, as those occur in
## too many places
#WARN  = -Wconversion -Wall -Wtabs -Wall -Wimplicit-interface
## -fbounds-check sometimes causes a weird error due to non-lazy
## evaluation of boolean in gfortran.
#WARN += -fbounds-check
#WARN += -fcheck=all
## gfortran 4.4.1 optimized with -O3 yields erroneous results
## Use -O2 to be on the safe side
OPT = -O2

### generate directory build, if not yet existing
$(shell mkdir -p build)

### version control
#GV = $(shell git rev-parse --short HEAD)
#$(shell echo "      subroutine print_version" > ../version.f)
#$(shell echo "        print*,'        git version $(GV)'" >> ../version.f)
#$(shell echo "      end" >> ../version.f)

########################################################################
## Runtime flags

## Preprocessor
# it might be advisable to use the -ffree-line-length-none 
# or -ffixed-line-length-none options
CPP = -cpp

## For debugging uncomment the following, choose a number between 0 and 
# 4 to choose the level of verbosity (higher means more output).
LEVEL = 0
DEBUG = -ggdb -pg -DDEBUG=$(LEVEL)
# Runtime check
#DEBUG += -fcheck=all

## Subtraction schemes
# There are divergences originating from on-shell produced squark lines
# in the real processes. The resonant contribution can easily exceed 
# the full NLO corrections. Keeping it as part of the real corrections
# would spoil the predictivity of the NLO calculation. Therefore, these
# contributions have to be removed in a consistent way.
# There exist several methods to cope with them, the most relevant 
# being the following:
# - Diagram Removal type I, implemented with preprocessor flag -DDR_I
# - Diagram Removal type II, implemented with preprocessor flag -DDR_II
# - Diagram Subtraction, implemented with preprocessor flag -DDSUB_I,
#   with no separate integration.
# - Diagram Subtraction with corrected jacobian, flag -DDSUB_II.
#   With this flag defined, the real amplitude is split into two parts.
#   In Reals.f the matrix element |M_NR|**2 + 2 M_R* M_NR is calculated,
#   in Reals_osres.f the remaining terms and the on-shell counter terms
#   are added: |M_R|**2 - BW*corrfac*|M_R^OS|**2. The latter gets 
#   integrated separately in sigosres.f and is included at a later point
#   to the main result.
#USRFLAGS += -DDR_II
#USRFLAGS += -DDSUB_I
USRFLAGS += -DDSUB_II
# use the POWHEG modified files (yes, no)
POWMOD = yes

## SkipHel
# skip the calculation of formcalc helicity amplitudes if the amplitude
# is zero 'NSKIP'-times during the initialisation process.
USRFLAGS += -DNSKIP=50

########################################################################
#           -*- no editing is required below this line -*-             #
########################################################################

########################################################################
## Runtime flags which should not be changed (don't touch this unless
## you know what you do!) 
CHANNEL += -DNINJ_JET

########################################################################
## Paths

WORKINGDIR = $(shell pwd)/..
PWHGDIR    = $(WORKINGDIR)/..
SUBWORKINGDIR = $(shell pwd)

# important directories
TOOLS = $(WORKINGDIR)/Tools
MADREALS = $(SUBWORKINGDIR)/MadGraph
FORMREALS = $(SUBWORKINGDIR)/FormCalc_Reals
FORMVIRTS = $(SUBWORKINGDIR)/FormCalc_Virtuals
SCRIPTS = $(WORKINGDIR)/Scripts

# includes
UINCLUDE    = $(WORKINGDIR)/include
PINCLUDE    = $(PWHGDIR)/include
VINCLUDE    = $(FORMVIRTS)/include
RENINCLUDE  = $(FORMVIRTS)/RenConst
MDINCLUDE   = $(MADREALS)/include
PY8INCLUDE  = $(TOOLS)/pythia8215/include
SLHAINCLUDE = $(TOOLS)/SLHALib-2.2/src
FRINCLUDE   = $(FORMREALS)/include
FRGINCLUDE  = $(FORMREALS)/global
FVGINCLUDE  = $(FORMVIRTS)/global
CINCLUDE    = $(TOOLS)/collier/include
LTINCLUDE   = $(TOOLS)/LoopTools-2.12/src/include

# modules
ifeq ($(UNAME), Darwin)
  #Mac OSX
  CMODULES = $(TOOLS)/collier/modules/MacOSX_$(FC)
endif
ifeq ($(UNAME), Linux)
  #Linux
  CMODULES = $(TOOLS)/collier/modules/Linux_$(FC)
endif

########################################################################
## search for the files and set paths

vpath %.F $(WORKINGDIR)
vpath %.F $(TOOLS)/FormCalcTools
vpath %.F $(FORMVIRTS)/squaredME
vpath %.F $(FORMVIRTS)/RenConst
vpath %.F $(FORMREALS)/squaredME
vpath %.F $(TOOLS)/DHELAS
vpath %.f $(WORKINGDIR)
vpath %.f $(PWHGDIR)
vpath %.f $(MADREALS)/SubProcesses_nInJ+jet
vpath %.f $(WORKINGDIR)/POWHEG_mod
vpath %.f $(TOOLS)/pythia6425
vpath %.f $(TOOLS)/DHELAS
vpath %.f $(TOOLS)/MadGraph
vpath %.f $(TOOLS)/functions
vpath %.cc $(TOOLS)/fastjet
vpath %.cc $(TOOLS)/pythiafortran
vpath %.cc $(TOOLS)/pythia8215
vpath %.F90 $(TOOLS)/collier
vpath %.o $(SUBWORKINGDIR)/build

########################################################################
## Source files

### ANALYSIS ###
ifeq ("$(ANALYSIS)","default")
  USE_FASTJET  = yes
  PWHGANALYSIS = Analysis.o
endif
ifeq ("$(ANALYSIS)","none")
  PWHGANALYSIS = Analysis-dummy.o
endif
ifeq ("$(ANALYSIS)","decay")
  PWHGANALYSIS = Analysis-decay.o
endif

### PDFs ###
ifeq ("$(PDF)","lhapdf")
  ifeq ($(LHAPDF_CONFIG),)
    $(error cannot find lhapdf-config (did you install lhapdf?))
  endif
  PDFPACK = lhapdfif.o
  ifeq  ("$(STATIC_LHAPDF)","yes")
    LHAPATH = $(shell $(LHAPDF_CONFIG) --libdir)
    LIBS += $(LHAPATH)/libLHAPDF.a
  else
    LIBS += -Wl,-rpath,$(shell $(LHAPDF_CONFIG) --libdir) \
            $(shell $(LHAPDF_CONFIG) --libs)
  endif
else
  PDFPACK = mlmpdfif.o hvqpdfpho.o
endif

### FASTJET ###
ifdef USE_FASTJET
  ifeq ($(FASTJET_CONFIG),)
    $(error cannot find fastjet-config (did you install fastjet?))
  endif
  ifeq  ("$(STATIC_FJ)","yes")
    LIBS += $(shell $(FASTJET_CONFIG) --shared=no --libs --plugins)
  else
    LIBS += $(shell $(FASTJET_CONFIG) --libs --plugins)
  endif
  FJCXXFLAGS  += $(shell $(FASTJET_CONFIG) --cxxflags)
  PWHGANALYSIS+= fastjetfortran.o
endif

### USER Files ###
USER = Born.o Born_phsp.o Reals.o Virtuals.o 	\
       init_processes.o init_couplings.o Reals_osres.o 		\
       Real_osres_phsp.o phsp_routines.o sigosres.o version.o FormCalc.o \
       set_channel.o Histogram.o

### Functions in Tools/functions/
FUNC = funcprocess.o Print.o funcbasic.o off_to_on.o 
ifeq ("$(LOOPINT)","collier")
  FUNC += lt_collier_interface_complex_V4.o
endif

### MadGraph mandatory files
MADGRAPH = couplings.o hardstop.o printout.o read_slha.o \
	set_general_couplings.o nexper.o

### Formcalc Files in Tools/FormcalcTools/###
FORMCALC = VecSet.o

### PWHG FILES in ../ ###
PWHG = btilde.o utils.o gen_Born_phsp.o	\
	test_Sudakov.o pt2maxreg.o maxrat.o Bornzerodamp.o \
	random.o boostrot.o bra_ket_subroutines.o \
	init_phys.o powheginput.o sigcollremn.o \
	pwhg_bookhist-multi.o pwhg_analysis_driver.o checkmomzero.o \
	setstrongcoupl.o integrator.o mwarn.o \
	sigcollsoft.o validflav.o decidemerging.o \
	loadstorenlo.o setlocalscales.o multi_plot.o \
	mint_upb.o reshufflemoms.o opencount.o \
	lhefread.o pwhg_io_interface.o

# modified POWHEG-files
ifeq ("$(POWMOD)","no")
  PWHG += pwhg_init.o cernroutines.o find_regions.o gen_real_phsp.o\
	sigborn.o sigreal.o sigsoftvirt.o \
	sigvirtual.o bbinit.o LesHouches.o \
	LesHouchesreg.o gen_index.o lhefwrite.o \
	gen_radiation.o sigremnants.o pwhgreweight.o \
	pdfcalls.o newunit.o pwhg_main.o rwl_weightlists.o \
	ubprojections.o rwl_setup_param_weights.o
else
  PWHG += pwhg_init_mod.o cernroutines_mod.o find_regions_mod.o gen_real_phsp_mod.o \
	sigborn_mod.o sigreal_mod.o sigsoftvirt_mod.o \
	sigvirtual_mod.o bbinit_mod.o LesHouches_mod.o \
	LesHouchesreg_mod.o gen_index_mod.o lhefwrite_mod.o \
	gen_radiation_mod.o sigremnants_mod.o pwhgreweight_mod.o \
	pdfcalls_mod.o newunit_mod.o pwhg_main_mod.o rwl_weightlists_mod.o \
	ubprojections_mod.o rwl_setup_param_weights_mod.o rwl_setup_param_weights_user.o
endif

### LHEF ###
# analyze LHEF output
LHEF = lhef_analysis.o boostrot.o random.o cernroutines_mod.o \
	opencount.o powheginput.o pwhg_bookhist-multi.o \
	lhefread.o newunit_mod.o pwhg_analysis_driver.o \
	funcprocess.o init_couplings.o Print.o \
	bra_ket_subroutines.o off_to_on.o funcbasic.o \
	pwhg_io_interface.o rwl_weightlists_mod.o multi_plot.o 

### PYTHIA ###
# target to read event file, shower events with PYTHIA + analysis
# NOTE: pythia-6.4.25-decay.f is based on POWHEG-BOX/pythia-6.4.25.f,
# to which we applied a simple patch to enable slepton decays
# (for clarity a separate patch file is included which is not needed otherwise)
PYTHIA = boostrot.o powheginput.o init_couplings.o funcprocess.o 	\
	pwhg_bookhist-multi.o lhefread.o newunit_mod.o pdfdummies.o	\
	pwhg_analysis_driver.o random.o cernroutines_mod.o opencount.o	\
	bra_ket_subroutines.o off_to_on.o funcbasic.o Print.o \
	pwhg_io_interface.o rwl_weightlists_mod.o multi_plot.o 
PYTHIA += main-PYTHIA.o setup-PYTHIA-lhef.o
PYTHIA += pythia-6.4.25-decay.o

### Virtual Amplitudes in Formcalc_Virtuals/ ###
VIRTUALS_F := $(wildcard $(FORMVIRTS)/squaredME/*.F)
VIRTUALS := $(notdir $(VIRTUALS_F:.F=.o))

RENCONST_F := $(wildcard $(FORMVIRTS)/RenConst/*.F)
RENCONST := $(notdir $(RENCONST_F:.F=.o))
VIRTUALS += $(RENCONST)

### FormCalc Real Amplitudes ###
REALS_F := $(wildcard $(FORMREALS)/squaredME/*.F)
REALS := $(notdir $(REALS_F:.F=.o))

# MadGraph amplitudes ###
MREALS_F := $(wildcard $(MADREALS)/SubProcesses_nInJ+jet/*.f)
MREALS := $(notdir $(MREALS_F:.f=.o))

SOURCESMAIN = $(PWHG) $(PDFPACK) $(USER) $(PWHGANALYSIS) $(FUNC) \
	$(FORMCALC) $(MADGRAPH) $(MREALS)

SOURCESLHEF = $(LHEF) $(PWHGANALYSIS) $(MADGRAPH) set_channel.o

SOURCESPYTHIA = $(PYTHIA) $(PWHGANALYSIS) $(MADGRAPH) set_channel.o

########################################################################
## Libraries
### LibSLHA in Tools/SLHALib-2.2 ###
LIBS += $(TOOLS)/libSLHA.a

### DHELAS in Tools/DHELAS ###
LIBS += $(TOOLS)/libdhelas3.a

ifeq ("$(LOOPINT)","collier")
  ### COLLIER in Tools/COLLIER ###
  LIBS += $(TOOLS)/libcollier.a
else
  ### LoopTools in Tools/Looptools-2.12 ###
  LIBS += $(TOOLS)/libooptools.a
  #$(error $(LIBS))
endif

### Libraries for linking c++ code
LIBS += -ldl -lstdc++

### for compressing lhe files
LIBS += -lz

########################################################################
## combine all flags, libraries and includes

ALL_FCFLAGS   = $(REC_FCFLAGS) $(OPT) $(WARN) $(CPP) $(DEBUG) $(USRFLAGS) $(CHANNEL)
ALL_FCFLAGS  += -I$(UINCLUDE) -I$(PINCLUDE) -I$(MDINCLUDE)
ALL_FCFLAGS  += -I$(WORKINGDIR) -I$(VINCLUDE) -I$(SLHAINCLUDE)
ALL_FCFLAGS  += -I$(FRINCLUDE) -I$(FRGINCLUDE) -I$(FVGINCLUDE) -I$(RENINCLUDE)
ifeq ("$(LOOPINT)","collier")
  ALL_FCFLAGS  += -I$(CINCLUDE)
  ALL_FCFLAGS  += -J$(CMODULES)
else
  ALL_FCFLAGS  += -I$(LTINCLUDE)
endif

ALL_CFLAGS    = $(REC_CFLAGS) $(OPT)

ALL_CXXFLAGS  = $(REC_CXXFLAGS) $(OPT)

LINKER = $(CPPFLAGS) $(LIBS) $(LDFLAGS)

HEADERS  = $(wildcard *.h $(UINCLUDE)/*.h  $(PINCLUDE)/*.h)
HEADERS += $(wildcard *.h $(SLHAINCLUDE)/*.h)
HEADERS += $(wildcard *.h $(MDINCLUDE)/*.h)
HEADERS += $(wildcard *.h $(VINCLUDE)/*.h $(FVGINCLUDE)/*.h)
HEADERS += $(wildcard *.h $(FRINCLUDE)/*.h $(FRGINCLUDE)/*.h)
HEADERS += $(wildcard *.h $(RENINCLUDE)/*.h)
HEADERS += $(wildcard *.inc $(MDINCLUDE)/*.inc $(UINCLUDE)/*.inc)
ifeq ("$(LOOPINT)","collier")
  HEADERS += $(wildcard *.h $(CINCLUDE)/*.h)
  HEADERS += $(wildcard *.mod $(CMODULES)/*.mod)
else
  HEADERS += $(wildcard *.h $(LTINCLUDE)/*.h)
endif

########################################################################
## Rules, generate objects

./build/%.o: %.f $(HEADERS)
	@echo "Compiling:" $<
	@$(FC) $(ALL_FCFLAGS) -c -o $@ $<

./build/%.o: %.F $(HEADERS)
	@echo "Compiling:" $<
	@$(FC) $(ALL_FCFLAGS) -c -o $@ $<

./build/%.o: %.F90 $(HEADERS)
	@echo "Compiling:" $<
	@$(FC) $(ALL_FCFLAGS) -c -o $@ $<

./build/pythiafortran.o: pythiafortran.cc
	@echo "Compiling:" $<
	@$(CXX) $(ALL_CXXFLAGS) $(PYCXXFLAGS) -c -o $@ $^

./build/fastjetfortran.o: fastjetfortran.cc
	@echo "Compiling:" $<
	@$(CXX) $(ALL_CXXFLAGS) $(FJCXXFLAGS) -c -o $@ $^

./build/%.o: %.c $(HEADERS)
	@echo "Compiling:" $<
	@$(CC) $(ALL_CFLAGS) -c -o $@ $<

########################################################################
## Rules, link
## type make -j4 [rule] to speed up the compilation

# we don't need the loop routines for lhef and PYTHIA
# -> remove initialization of loop routines in init_couplings depending
# on which program we want to compile
pwhg-init_couplings:
	-rm ./build/init_couplings.o

analysis-init_couplings:
	-rm ./build/init_couplings.o
	$(eval ALL_FCFLAGS += -DANALYSIS)

formcalc_reals.a: $(addprefix ./build/, $(REALS))
	ar cru $@ $(patsubst %,build/%,$(REALS))
	mv $@ $(FORMREALS)

formcalc_virtuals.a: $(addprefix ./build/, $(VIRTUALS))
	ar cru $@ $(patsubst %,build/%,$(VIRTUALS))
	mv $@ $(FORMVIRTS)

libs: formcalc_reals.a formcalc_virtuals.a
reals: formcalc_reals.a
virtuals: formcalc_virtuals.a

pwhg_main_ninj+jet: pwhg-init_couplings $(addprefix ./build/, $(SOURCESMAIN))
	$(FC) $(ALL_FCFLAGS) $(patsubst %,build/%,$(SOURCESMAIN)) $(FORMREALS)/formcalc_reals.a $(FORMVIRTS)/formcalc_virtuals.a $(LINKER) -o $@

lhef_analysis: analysis-init_couplings $(addprefix ./build/, $(SOURCESLHEF))
	$(FC) $(ALL_FCFLAGS) $(patsubst %,build/%,$(SOURCESLHEF)) $(LINKER) -o $@ 

main-PYTHIA-lhef: analysis-init_couplings $(addprefix ./build/, $(SOURCESPYTHIA))
	$(FC) $(ALL_FCFLAGS) $(patsubst %,build/%,$(SOURCESPYTHIA)) $(LINKER) -o $@

merge-pwg-stat:
	$(FC) $(TOOLS)/programs/merge-pwg-stat.f -o $@

merge-data: 
	$(FC) $(TOOLS)/programs/merge-data.f -o $@

topintegrator:
	$(FC) $(TOOLS)/programs/topintegrator.f -o $@

copy-scripts:
	cp $(SCRIPTS)/runparallel.sh  $(SUBWORKINGDIR) && \
	cp $(SCRIPTS)/clean_rundir.sh $(SUBWORKINGDIR) && \
	cp $(SCRIPTS)/merge.sh        $(SUBWORKINGDIR)

all-libs: libs pwhg_main_ninj+jet copy-scripts merge-pwg-stat merge-data topintegrator

all: pwhg_main_ninj+jet copy-scripts merge-pwg-stat merge-data topintegrator

clean:
	rm -f build/*.o pwhg_main_ninj+jet lhef_analysis main-PYTHIA-lhef merge-pwg-stat merge-data topintegrator runparallel.sh clean_rundir.sh merge.sh

clean-results:
	find ./testrun ! \( -name '*.slha' -o -name '*.input' -o -name 'pwgseeds.dat' -o -name '*.xml' \) \
	-type f -exec rm -f {} +

clean-libs:
	rm -f $(FORMREALS)/formcalc_reals.a $(FORMVIRTS)/formcalc_virtuals.a

clean-all: clean clean-results clean-libs

do-pwhg do: pwhg_main_ninj+jet copy-scripts merge-pwg-stat merge-data topintegrator
	cd testrun && ../pwhg_main_ninj+jet

do-lhef: lhef_analysis
	cd testrun && ../lhef_analysis

do-pythia: main-PYTHIA-lhef
	cd testrun && ../main-PYTHIA-lhef

do-all: do-pwhg do-lhef do-pythia

########################################################################
#                       -*- End of Makefile -*-                        #
########################################################################
