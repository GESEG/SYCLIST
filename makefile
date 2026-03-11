# MAKEFILE pour SYCLIST
SRCDIR = .
EXEC = Syclist.e

FFLAGS = -frecursive -fopenmp #-fcheck=all
LIBS =  -lm

##############
## GFORTRAN ##
##############
COMPILER= gfortran

# Compilation rules
%.o: $(SRCDIR)/%.f95
	@echo "F90: $(COMPILER) -c $(FFLAGS) $(<)"
	@$(COMPILER) $(FFLAGS) -c $(SRCDIR)/$*.f95 -o $*.o

# Source files
SOURCES_F95 = formats.f95 \
parameters.f95 \
SYCLIST.f95

# Objects lists
OBJECTS=$(SOURCES_F95:%.f95=%.o)

# Compile and link the code
evolData_new: $(OBJECTS)
	@echo "==========LINKING========="
	@echo "Fortran options: $(FFLAGS) --> $(EXEC)"
	@$(COMPILER) $(FFLAGS) $(OBJECTS) $(LIBS) -o $(EXEC)

clean:
	rm -f *.o *.mod
