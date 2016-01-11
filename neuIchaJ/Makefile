########################################################################
#                          -*- Makefile -*-                            #
########################################################################
# General Conventions for Makefiles
SHELL = /bin/sh
.SUFFIXES:
.SUFFIXES: .c .f .F .cc .cpp .h .inc .o .a

########################################################################
## Flags

## Compiler and additional compiler Flags
FC  =  gfortran
CXX =  g++
CC  =  gcc
FCFLAGS  = -g
CXXFLAGS = -g
CFLAGS   = -g

# recommended compiler flags
REC_FCFLAGS   = -fno-automatic -fno-range-check
REC_FCFLAGS  += -ffixed-line-length-none -lgfortran -ff2c -DU77EXT=0 -DQuad=0
REC_FCFLAGS  += $(FCFLAGS)
REC_CXXFLAGS  = -fomit-frame-pointer -ffast-math -Wall -m64
REC_CXXFLAGS += $(CXFLAGS)
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
ANALYSIS = none

## path to LHAPDF config executable
LHAPDF_CONFIG = lhapdf-config

## path to fastjet config executable
FASTJET_CONFIG = fastjet-config

## path to fortran libraries
ifeq ($(UNAME), Darwin)
  #Mac OSX
  LIBGFORTRANPATH = /usr/local/lib
  LIBSTDCPPPATH   = /usr/local/lib
else
  #Linux
  LIBGFORTRANPATH = /usr/lib/gcc/x86_64-linux-gnu/4.8
  LIBSTDCPPPATH   = /usr/lib/gcc/x86_64-linux-gnu/4.8
endif

## warning for type-conversions -> basically useless, as those occur in
## too many places
#WARN  = -Wconversion -Wall -Wtabs -Wall -Wimplicit-interface
## -fbounds-check sometimes causes a weird error due to non-lazy
## evaluation of boolean in gfortran.
#WARN += -fbounds-check
## gfortran 4.4.1 optimized with -O3 yields erroneous results
## Use -O2 to be on the safe side
OPT = -O2

### generate directory build and run, if not yet existing
$(shell mkdir -p build)
$(shell mkdir -p run)

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

FJ     = $(TOOLS)/fastjet
PY     = $(TOOLS)/pythia
HER    = $(TOOLS)/herwig
LT     = $(TOOLS)/LoopTools-2.12
SLHA   = $(TOOLS)/SLHALib-2.2
DHELAS = $(TOOLS)/DHELAS
MADMODEL  = $(REAL)/MODEL

# includes
UINCLUDE    = $(WORKINGDIR)/include
PINCLUDE    = $(PWHGDIR)/include
VINCLUDE    = $(VIRT)/include
MDINCLUDE   = $(REAL)/include
SLHAINCLUDE = $(SLHA)/src
LTINCLUDE   = $(LT)/src/include

########################################################################
## search for the files and set paths

vpath %.f $(PWHGDIR) $(WORKINGDIR) $(TOOLS)/functions $(DHELAS) $(PY) $(HER)
vpath %.F $(WORKINGDIR) $(TOOLS)/FormCalcTools $(DHELAS)
vpath %.F $(VIRT)/RenConsts $(VIRTSUB)
vpath %.F $(MADMODEL) $(REALSUB)
vpath %.f $(MADMODEL) $(REALSUB)
vpath %.f $(MODIFIED)
vpath %.cc $(FJ)
vpath %.o $(SUBWORKINGDIR)/build

########################################################################
## Libraries
#not necessary anymore
#LIBS  += $(LIBGFORTRANPATH)/libgfortranbegin.a
#LIBS  += $(LIBGFORTRANPATH)/libgfortran.a
#LIBS  += $(LIBSTDCPPPATH)/libstdc++.a

### LibSLHA in Tools/SLHALib-2.2 ###
LIBS   += $(TOOLS)/libSLHA.a

### LoopTools in Tools/Looptools-2.12 ###
LIBS  += $(TOOLS)/libooptools.a

### DHELAS in Tools/DHELAS ###
LIBS  += $(TOOLS)/libdhelas3.a

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
  LIBS += $(shell $(FASTJET_CONFIG) --libs --plugins ) -lstdc++
  FJCXXFLAGS  += $(shell $(FASTJET_CONFIG) --cxxflags)
  PWHGANALYSIS+= fastjetfortran.o
  # fastjetsisconewrap.o fastjetktwrap.o
endif

### USER Files ###
USER = Born.o Born_phsp.o Born_lh.o Reals.o Reals_lh.o Virtuals.o \
       init_processes.o init_couplings.o

### Functions in Tools/functions/
FUNC = hep2pwg.o funcprocess.o Print.o funcbasic.o

### MadGraph Amplitudes
MADOBJ = couplings.o hardstop.o printout.o read_slha.o \
	set_general_couplings.o 

### Formcalc Files in Tools/FormcalcTools/###
FORMCALC = VecSet.o

### PWHG FILES in ../ ###
PWHG = pwhg_main.o pwhg_init.o btilde.o utils.o gen_Born_phsp.o	\
	test_Sudakov.o pt2maxreg.o maxrat.o Bornzerodamp.o		\
	random.o boostrot.o bra_ket_subroutines.o			\
	init_phys.o powheginput.o pdfcalls.o sigcollremn.o		\
	pwhg_bookhist-multi.o pwhg_analysis_driver.o checkmomzero.o	\
	setstrongcoupl.o integrator.o newunit.o mwarn.o		\
	sigcollsoft.o validflav.o decidemerging.o          		\
	loadstorenlo.o setlocalscales.o ubprojections.o      	\
	mint_upb.o reshufflemoms.o opencount.o
# possible modified POWHEG-files
PWHG += cernroutines_mod.o find_regions.o gen_real_phsp.o	\
	sigborn.o sigreal.o sigsoftvirt.o			\
	sigvirtual.o bbinit.o LesHouches.o 		\
	LesHouchesreg.o gen_index.o lhefwrite.o		\
	gen_radiation.o sigremnants.o pwhgreweight.o

### LHEF ###
# analyze LHEF output
LHEF = lhef_analysis.o boostrot.o random.o cernroutines.o		\
     opencount.o powheginput.o pwhg_bookhist-multi.o 			\
     lhefread.o newunit.o pwhg_analysis_driver.o 			\
     funcprocess.o init_couplings.o Print.o				\
     bra_ket_subroutines.o off_to_on.o funcbasic.o

### PYTHIA ###
# target to read event file, shower events with PYTHIA + analysis
# NOTE: pythia-6.4.25-decay.f is based on POWHEG-BOX/pythia-6.4.25.f,
# to which we applied a simple patch to enable slepton decays
# (for clarity a separate patch file is included which is not needed otherwise)
PYTHIA = main-PYTHIA.o setup-PYTHIA-lhef.o pythia6428.o	\
	boostrot.o powheginput.o init_couplings.o funcprocess.o 	\
	pwhg_bookhist-multi.o lhefread.o newunit.o pdfdummies.o		\
	pwhg_analysis_driver.o random.o cernroutines.o opencount.o	\
	bra_ket_subroutines.o off_to_on.o funcbasic.o Print.o

### HERWIG ###
# this is work in progress
HERWIG = main-HERWIG.o setup-HERWIG-lhef.o herwig6510.o boostrot.o 	\
     opencount.o powheginput.o pwhg_bookhist-multi.o random.o		\
     lhefread.o newunit.o pwhg_analysis_driver.o 			\
     funcprocess.o init_couplings.o cernroutines.o			\
     bra_ket_subroutines.o off_to_on.o funcbasic.o

# MadGraph born amplitudes
MADOBJ += sborn_proc.o
MADOBJ += b_cl_nIxJm_001_001.o  b_cl_nIxJm_003_001.o  b_sf_nIxJm_001_003.o  b_sf_nIxJm_003_003.o \
	b_cl_nIxJm_001_002.o  b_cl_nIxJm_003_002.o  b_sf_nIxJm_001_004.o  b_sf_nIxJm_003_004.o \
	b_cl_nIxJm_002_001.o  b_cl_nIxJm_004_001.o  b_sf_nIxJm_002_003.o  b_sf_nIxJm_004_003.o \
	b_cl_nIxJm_002_002.o  b_cl_nIxJm_004_002.o  b_sf_nIxJm_002_004.o  b_sf_nIxJm_004_004.o \
	b_cl_nIxJp_001_001.o  b_cl_nIxJp_003_001.o  b_sf_nIxJp_001_003.o  b_sf_nIxJp_003_003.o \
	b_cl_nIxJp_001_002.o  b_cl_nIxJp_003_002.o  b_sf_nIxJp_001_004.o  b_sf_nIxJp_003_004.o \
	b_cl_nIxJp_002_001.o  b_cl_nIxJp_004_001.o  b_sf_nIxJp_002_003.o  b_sf_nIxJp_004_003.o \
	b_cl_nIxJp_002_002.o  b_cl_nIxJp_004_002.o  b_sf_nIxJp_002_004.o  b_sf_nIxJp_004_004.o
MADOBJ += sborn_cl_nIxJm_001.o  sborn_cl_nIxJm_004.o  sborn_sf_nIxJm_002.o \
	sborn_cl_nIxJm_002.o  sborn_sf_nIxJm_003.o \
	sborn_cl_nIxJm_003.o  sborn_sf_nIxJm_001.o  sborn_sf_nIxJm_004.o \
	sborn_cl_nIxJp_001.o  sborn_cl_nIxJp_004.o  sborn_sf_nIxJp_002.o \
	sborn_cl_nIxJp_002.o  sborn_sf_nIxJp_003.o \
	sborn_cl_nIxJp_003.o  sborn_sf_nIxJp_001.o  sborn_sf_nIxJp_004.o

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
	$(FORMCALC) $(MADOBJ)

SOURCESLHEF = $(LHEF) $(PWHGANALYSIS) $(MADOBJ)

SOURCESPYTHIA = $(PYTHIA) $(PWHGANALYSIS)  $(MADOBJ)

SOURCESHERWIG = $(HERWIG) $(PWHGANALYSIS)  $(MADOBJ)
          
# Additional FormCalc Files (maybe custom ones)
#VIRTUALS += BornEps_n1x1.o

########################################################################
## combine all flags, libraries and includes

ALL_FCFLAGS   = $(REC_FCFLAGS) $(OPT) $(WARN) $(CPP) $(DEBUG) $(USRFLAGS)
ALL_FCFLAGS  += -I$(UINCLUDE) -I$(PINCLUDE) -I$(LTINCLUDE) -I$(MDINCLUDE)
ALL_FCFLAGS  += -I$(WORKINGDIR) -I$(VINCLUDE) -I$(SLHAINCLUDE)

ALL_CFLAGS    = $(REC_CFLAGS) $(OPT)
ALL_CFLAGS   += -I$(LTINCLUDE)

ALL_CXXFLAGS  = $(REC_CXXFLAGS) $(FJCXXFLAGS) $(OPT)

LINKER = $(CPPFLAGS) $(LIBS) $(LDFLAGS) 

HEADERS  = $(wildcard *.h $(UINCLUDE)/*.h  $(PINCLUDE)/*.h $(VINCLUDE)/*.h)
HEADERS += $(wildcard *.h $(LTINCLUDE)/*.h $(SLHAINCLUDE)/*.h)
HEADERS += $(wildcard *.h $(MDINCLUDE)/*.h)
HEADERS += $(wildcard *.inc $(MDINCLUDE)/*.inc $(UINCLUDE)/*.inc)
     
########################################################################
## Rules, generate objects

%.o: %.f $(HEADERS)
	@echo "Compiling:" $<
	@$(FC) $(ALL_FCFLAGS) -c -o build/$@ $<

%.o: %.F $(HEADERS)
	@echo "Compiling:" $<
	@$(FC) $(ALL_FCFLAGS) -c -o build/$@ $<

%.o: %.cc
	@echo "Compiling:" $<
	@$(CXX) $(ALL_CXXFLAGS) -c -o build/$@ $^

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

all: pwhg_main_nixj lhef_analysis main-PYTHIA-lhef

clean:
	rm -f build/*.o

clean-results:
	cd testrun && rm -f *.out *.lhe *.top *.gp *.eps *.dat FlavRegList	\
	bornequiv realequiv realequivregions pwhg_checklimits   	\
	virtequiv
	
clean-all: clean clean-results
	rm -f pwhg_main_nixj lhef_analysis main-PYTHIA-lhef

do-pwhg do: pwhg_main_nixj
	cd testrun && cp pwgseedsdat pwgseeds.dat && ../pwhg_main_nixj

do-lhef: lhef_analysis
	cd testrun && ../lhef_analysis
	
do-pythia: main-PYTHIA-lhef
	cd testrun && ../main-PYTHIA-lhef
	
do-all: do-pwhg do-lhef do-pythia
	
########################################################################
#                       -*- End of Makefile -*-                        #
########################################################################
