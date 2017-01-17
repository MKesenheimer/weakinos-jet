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
# use "./configure ifort" first
#FC  = ifort
# use "./configure gfortran" first
FC  = gfortran
CXX = g++
CC  = gcc

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

## For debugging uncomment the following, choose a number between 0 and 
# 4 to choose the level of verbosity (higher means more output).
LEVEL = 0
DEBUG = -ggdb -pg -DDEBUG=$(LEVEL)

## More debugging flags
# Check UV finiteness in FormCalc Virtuals: -DCHECKUV
# Check if MadGraph and FormCalc born amplitudes are the same during 
# calculation and abort if they are not: -DCHECK_FORM_MAD
# Note: Use this only for testing born amplitudes, not for the final 
# program and not while checking real amplitudes!
#USRFLAGS += -DCHECK_FORM_MAD

## Additional and mandatory flags
# Use MadGraph Borns (default): -DMAD_BORN 
# Use FormCalc Borns: -DFORM_BORN
#USRFLAGS += -DMAD_BORN
USRFLAGS += -DFORM_BORN

## Treatment of quark masses and squark widths
# The current version uses vanishing masses for incoming quarks.
# if you wish to use finite bottom mass, comment the following line.
USRFLAGS += -DNEGLECTBMASS

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
#   no correction of the jacobian, which is in principal not ok, but 
#   could be used for testing the convergence.
# - Diagram Subtraction with corrected jacobian, flag -DDSUB_II.
#   With this flag defined, the real amplitude is split into two parts.
#   In Reals.f the matrix element |M_NR|**2 + 2 M_R* M_NR is calculated,
#   in Reals_osres.f the remaining terms and the on-shell counter terms
#   are added: |M_R|**2 - BW*corrfac*|M_R^OS|**2. The latter gets 
#   integrated seperately in sigosres.f and is included at a later point
#   to the main result.
#USRFLAGS += -DDSUB_II_TEST
USRFLAGS += -DDSUB_II

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
MADREALS = $(SUBWORKINGDIR)/MadGraph_Reals
FORMREALS = $(SUBWORKINGDIR)/FormCalc_Reals
FORMVIRTS = $(SUBWORKINGDIR)/FormCalc_Virtuals
SCRIPTS = $(WORKINGDIR)/Scripts

# includes
UINCLUDE    = $(WORKINGDIR)/include
SINCLUDE    = $(SUBWORKINGDIR)/include
PINCLUDE    = $(PWHGDIR)/include
VINCLUDE    = $(FORMVIRTS)/include
RENINCLUDE  = $(FORMVIRTS)/RenConst
MDINCLUDE   = $(MADREALS)/include
PY8INCLUDE  = $(TOOLS)/pythia8215/include
#LTINCLUDE   = $(TOOLS)/LoopTools-2.12/src/include
SLHAINCLUDE = $(TOOLS)/SLHALib-2.2/src
FRINCLUDE   = $(FORMREALS)/include
FRGINCLUDE  = $(FORMREALS)/global
FVGINCLUDE  = $(FORMVIRTS)/global
CINCLUDE    = $(TOOLS)/collier/include

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
USER = Born.o Born_phsp.o Reals.o Virtuals.o 	\
       init_processes.o init_couplings.o Reals_osres.o 		\
       Real_osres_phsp.o phi1_2.o sigosres.o version.o FormCalc.o \
       set_channel.o Histogram.o

### Functions in Tools/functions/
FUNC = funcprocess.o Print.o funcbasic.o off_to_on.o lt_collier_interface_complex.o

### MadGraph mandatory files
MADGRAPH = couplings.o hardstop.o printout.o read_slha.o \
	set_general_couplings.o nexper.o

### Formcalc Files in Tools/FormcalcTools/###
FORMCALC = VecSet.o

### PWHG FILES in ../ ###
PWHG = pwhg_init.o btilde.o utils.o gen_Born_phsp.o		\
	test_Sudakov.o pt2maxreg.o maxrat.o Bornzerodamp.o		\
	random.o boostrot.o bra_ket_subroutines.o			\
	init_phys.o powheginput.o sigcollremn.o				\
	pwhg_bookhist-multi.o pwhg_analysis_driver.o checkmomzero.o	\
	setstrongcoupl.o integrator.o mwarn.o				\
	sigcollsoft.o validflav.o decidemerging.o          		\
	loadstorenlo.o setlocalscales.o ubprojections.o      		\
	mint_upb.o reshufflemoms.o opencount.o \
	lhefread.o pwhg_io_interface.o rwl_weightlists.o rwl_setup_param_weights.o

# modified POWHEG-files
PWHG += cernroutines_mod.o find_regions_mod.o gen_real_phsp_mod.o	\
	sigborn_mod.o sigreal_mod.o sigsoftvirt_mod.o			\
	sigvirtual_mod.o bbinit_mod.o LesHouches_mod.o 			\
	LesHouchesreg_mod.o gen_index_mod.o lhefwrite_mod.o		\
	gen_radiation_mod.o sigremnants_mod.o pwhgreweight_mod.o 	\
	pdfcalls_mod.o newunit_mod.o pwhg_main_mod.o

### LHEF ###
# analyze LHEF output
LHEF = lhef_analysis.o boostrot.o random.o cernroutines_mod.o		\
	opencount.o powheginput.o pwhg_bookhist-multi.o 			\
	lhefread.o newunit_mod.o pwhg_analysis_driver.o 			\
	funcprocess.o init_couplings.o Print.o				\
	bra_ket_subroutines.o off_to_on.o funcbasic.o \
	pwhg_io_interface.o rwl_weightlists.o

### PYTHIA ###
# target to read event file, shower events with PYTHIA + analysis
# NOTE: pythia-6.4.25-decay.f is based on POWHEG-BOX/pythia-6.4.25.f,
# to which we applied a simple patch to enable slepton decays
# (for clarity a separate patch file is included which is not needed otherwise)
PYTHIA = boostrot.o powheginput.o init_couplings.o funcprocess.o 	\
	pwhg_bookhist-multi.o lhefread.o newunit_mod.o pdfdummies.o	\
	pwhg_analysis_driver.o random.o cernroutines_mod.o opencount.o	\
	bra_ket_subroutines.o off_to_on.o funcbasic.o Print.o \
	pwhg_io_interface.o rwl_weightlists.o
ifeq ("$(USE_PYTHIA8)","yes")
  # TODO:
  PYTHIA += main-PYTHIA_mod.o
else
  PYTHIA += main-PYTHIA.o setup-PYTHIA-lhef.o
  PYTHIA += pythia-6.4.25-decay.o
endif

### Virtual Amplitudes in Formcalc_Virtuals/ ###
VIRTUALS_F := $(wildcard $(FORMVIRTS)/squaredME/*.F)
VIRTUALS := $(notdir $(VIRTUALS_F:.F=.o))
VIRTUALS += RenConst.o RenConst1.o RenConst2.o

### FormCalc Real Amplitudes ###
REALS_F := $(wildcard $(FORMREALS)/squaredME/*.F)
REALS := $(notdir $(REALS_F:.F=.o))

# MadGraph amplitudes ###
MREALS_F := $(wildcard $(MADREALS)/SubProcesses_nInJ+jet/*.f)
MREALS := $(notdir $(MREALS_F:.f=.o))

SOURCESMAIN = $(PWHG) $(PDFPACK) $(USER) $(PWHGANALYSIS) $(FUNC) \
	$(FORMCALC) $(MADGRAPH) $(MREALS)

SOURCESLHEF = $(LHEF) $(PWHGANALYSIS) $(MADGRAPH) set_channel.o

SOURCESPYTHIA = $(PYTHIA) $(PWHGANALYSIS) $(MADGRAPH)

########################################################################
## Libraries
### LibSLHA in Tools/SLHALib-2.2 ###
LIBS += $(TOOLS)/libSLHA.a

### LoopTools in Tools/Looptools-2.12 ###
# is not used anymore
#LIBS += $(TOOLS)/libooptools.a

### DHELAS in Tools/DHELAS ###
LIBS += $(TOOLS)/libdhelas3.a

### COLLIER in Tools/COLLIER ###
LIBS += $(TOOLS)/libcollier.a

### Libraries for linking c++ code
LIBS += -ldl -lstdc++

### for compressing lhe files
LIBS += -lz

########################################################################
## combine all flags, libraries and includes

ALL_FCFLAGS   = $(REC_FCFLAGS) $(OPT) $(WARN) $(CPP) $(DEBUG) $(USRFLAGS) $(CHANNEL)
# -I$(LTINCLUDE)
ALL_FCFLAGS  += -I$(UINCLUDE) -I$(PINCLUDE) -I$(MDINCLUDE)
ALL_FCFLAGS  += -I$(SINCLUDE) -I$(WORKINGDIR) -I$(VINCLUDE) -I$(SLHAINCLUDE)
ALL_FCFLAGS  += -I$(FRINCLUDE) -I$(FRGINCLUDE) -I$(FVGINCLUDE) -I$(RENINCLUDE)
ALL_FCFLAGS  += -I$(CINCLUDE)

ALL_FCFLAGS  += -J$(CMODULES)

ALL_CFLAGS    = $(REC_CFLAGS) $(OPT)
#ALL_CFLAGS   += -I$(LTINCLUDE)

ALL_CXXFLAGS  = $(REC_CXXFLAGS) $(OPT)

LINKER = $(CPPFLAGS) $(LIBS) $(LDFLAGS)

# $(LTINCLUDE)/*.h
HEADERS  = $(wildcard *.h $(UINCLUDE)/*.h  $(PINCLUDE)/*.h)
HEADERS += $(wildcard *.h $(SLHAINCLUDE)/*.h)
HEADERS += $(wildcard *.h $(MDINCLUDE)/*.h $(SINCLUDE)/*.h)
HEADERS += $(wildcard *.h $(VINCLUDE)/*.h $(FVGINCLUDE)/*.h)
HEADERS += $(wildcard *.h $(FRINCLUDE)/*.h $(FRGINCLUDE)/*.h)
HEADERS += $(wildcard *.h $(RENINCLUDE)/*.h $(CINCLUDE)/*.h)
HEADERS += $(wildcard *.inc $(MDINCLUDE)/*.inc $(UINCLUDE)/*.inc)
HEADERS += $(wildcard *.mod $(CMODULES)/*.mod)

########################################################################
## Rules, generate objects

%.o: %.f $(HEADERS)
	@echo "Compiling:" $<
	@$(FC) $(ALL_FCFLAGS) -c -o build/$@ $<

%.o: %.F $(HEADERS)
	@echo "Compiling:" $<
	@$(FC) $(ALL_FCFLAGS) -c -o build/$@ $<

%.o: %.F90 $(HEADERS)
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

formcalc_reals.a: $(REALS)
	ar cru $@ $(patsubst %,build/%,$(REALS))
	mv $@ $(FORMREALS)
    
formcalc_virtuals.a: $(VIRTUALS)
	ar cru $@ $(patsubst %,build/%,$(VIRTUALS))
	mv $@ $(FORMVIRTS)

libs: formcalc_reals.a formcalc_virtuals.a

pwhg_main_ninj+jet: $(SOURCESMAIN)
	$(FC) $(ALL_FCFLAGS) $(patsubst %,build/%,$(SOURCESMAIN)) $(LINKER) $(FORMREALS)/formcalc_reals.a $(FORMVIRTS)/formcalc_virtuals.a -o $@

lhef_analysis:$(SOURCESLHEF)
	$(FC) $(ALL_FCFLAGS) $(patsubst %,build/%,$(SOURCESLHEF)) $(LINKER) -o $@ 

main-PYTHIA-lhef: $(SOURCESPYTHIA)
	$(FC) $(ALL_FCFLAGS) $(patsubst %,build/%,$(SOURCESPYTHIA)) $(LINKER) -o $@

merge-pwg-stat: 
	$(FC) $(TOOLS)/programs/merge-pwg-stat.f -o $@

merge-data: 
	$(FC) $(TOOLS)/programs/merge-data.f -o $@

copy-scripts:
	cp $(SCRIPTS)/runparallel.sh  $(SUBWORKINGDIR) && \
	cp $(SCRIPTS)/clean_rundir.sh $(SUBWORKINGDIR) && \
	cp $(SCRIPTS)/reweight.sh     $(SUBWORKINGDIR) && \
	cp $(SCRIPTS)/merge.sh        $(SUBWORKINGDIR)

all: libs pwhg_main_ninj+jet copy-scripts merge-pwg-stat merge-data lhef_analysis main-PYTHIA-lhef

clean:
	rm -f build/*.o pwhg_main_ninj+jet lhef_analysis main-PYTHIA-lhef merge-pwg-stat merge-data runparallel.sh clean_rundir.sh reweight.sh merge.sh

clean-results:
	find ./testrun ! \( -name '*.slha' -o -name '*.input' -o -name 'pwgseeds.dat' \) \
	-type f -exec rm -f {} +
    
clean-libs:
	rm -f $(FORMREALS)/formcalc_reals.a $(FORMVIRTS)/formcalc_virtuals.a

clean-all: clean clean-results clean-libs

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
