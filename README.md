## POWHEG-BOX-V2 Project Weakino Pair Production

### IMPORTANT NOTE

The code available was tested and developed for the POWHEG-BOX-V2 revision 3361. If you examine unexpected behavior 
or if you have problems compiling and running the code, please rewind or update your POWHEG-BOX-V2 version to the 
one it was developed for.
To do this, use

        $ svn info
        $ svn up -r3361

in your POWHEG-BOX-V2 main directory.

### Synopsis

More informations about how to set up the project properly on http://powhegbox.mib.infn.it/.

### Compiling

The first thing you should do is to compile the static libraries

* libdhelas3.a

* libcollier.a

* libSLHA.a

for your own operating system. To do this, call the configuration scripts in the main directory by simply typing

        $ ./configure [compiler]

where the optional parameter compiler is whether gfortran or ifort.

You can compile the libraries by typing

        $ make libdhelas3.a

        $ make libcollier.a

        $ make libSLHA.a

or short

        $ make libs

If you want to use your own libraries, copy them into ./Tools/ or provide
paths to the libraries in the Makefile.

Then, change into a desired process directory and type

        $ make -j4 libs

        $ make clean-results && make -j4 do

to compile the virtual and real matrix elements and run the program or use the script

        $ ./make-all.sh -j4 libs

to build the libraries and executables for all processes at once.

Important note for Mac OSX and probably for some Linux users, too:
In order to link the object files properly with newer compiler versions
it might be advisable to recompile all libraries using the -lstdc++ flag.

### Precompiler Flags

In the current version several C preprocessor (cpp) flags are implemented.
The preprocessor runs in traditional mode for gfortran. Any restrictions of the 
file-format, especially the limits on line length, apply for 
preprocessed output as well, so it might be advisable to use the 

        -ffree-line-length-none 

or 

        -ffixed-line-length-none

options (activated as default). If you want to change a preprocessor flag
it is imperative to run

        $ make clean

before recompiling the source code.

The flags

* FORM_BORN, MAD_BORN

* DR_I, DR_II, DSUB_I, DSUB_II

are mandatory, you should not clear them.

The preprocessor flags are used in such a way that runtime is saved. 
For example it is more costly to replace all preprocessor flags with

        if(flag) then
          ...
        endif

statements, since the program has to check these if-query frequently.
With the implemented flags the C preprocessor sorts out all unnecessary 
code.

Please refer to the Makefile for a detailed overview.

### Running

        $ make do

compiles the source and runs the program in ./testrun.

        $ make clean

removes all object files in ./build. This has no effect on the compiled program.

        $ make clean-results

removes the results in ./testrun.

        $ make clean-all

removes the results, the object files and the compiled programs.

        $ make clean-libs

removes the libraries in ./Tools.

All parameters are read from a single slha-file in ./testrun. Runtime variables, such as 
integration points, number of events to generate, etc. has to be specified in powheg.input.
If you want to change the Z-mass, Z-width or alpha_em you can do this in powheg.input, too.
Please refer to the provided manual.

### Scripts

We have added several helpful scripts, which could be used to generate results or clean 
old runs. The most important one is ./Scripts/runparallel.sh, which is used to run the 
POWHEG-BOX-V2 executable totally automated in parallel mode. 
Type
        $ ./runparallel.sh -h

to get an overview of the functionality of the script. This script works even with the MOAB 
and condor submitting systems.

### License

This project is open source. Please refer to the LICENSE file for a full overview.
