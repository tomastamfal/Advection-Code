#*********************************************************************************************************
#**                                     Developped by Tomas Tamfal 					**
#**                                       Version 0.1 January 2018	                               	**
#*********************************************************************************************************


#**********************************************************************************************************
#            Makefile_start: Compiles the code with different compilers and different parameters
#**********************************************************************************************************
SHELL = /bin/sh

.SUFFIXES:
.SUFFIXES: .F90 .o

#********** Compiler and options choice **********
FC = gfortran

#------- GFORTRAN ------
#FFLAGS = -O3 
FFLAGS = -O0 -Og -Wall -Wextra -pedantic -fimplicit-none -fcheck=all  -fbacktrace  
#FFLAGS = -O0 -Wunused-variable
#*********** Library linking ******************************
LDLIBS = #

#********** Choice of simulation parameters **********
#DIMENSION = ONE
DIMENSION = TWO

#FLUX = Fconst
#FLUX = Fcentraldiff
FLUX = Flim

#TVD = Minmod
TVD = new

#TIMESTEPS = FIXED
TIMESTEPS = VARIABLE

MESH = LIN_MESH
#MESH = LOG_MESH

#********** Object files **********
modules = parameters.o variables.o

objects = advection.o mesh.o initialsetup.o timestep.o output.o \
	 periodic.o reconstruct_x.o reconstruct_y.o fluxcalc_x.o \
	 fluxcalc_y.o riemann_x.o riemann_y.o 


target = ../run

#********** Commands **********
all: def $(target) cl

$(target): $(modules) $(objects)
	$(FC) $(FFLAGS) -o $@ $^ ${LDLIBS} 
	
%.o: %.F90
	$(FC) $(FFLAGS) -c  $<  ${LDLIBS}
	
%.o: choice.def

.PHONY: cl clean

cl:
	rm -f *~ *.o *.mod OUT choice.def
	
clean:
	rm -f *~ *.o *.mod *data fort* OUT
		
def:
	echo \#define $(DIMENSION) >> choice.def
	echo \#define $(FLUX) >> choice.def	
	echo \#define $(TIMESTEPS) >> choice.def
	echo \#define $(MESH) >> choice.def
	echo \#define $(TVD) >> choice.def








