# MAKEFILE pour SYCLIST
SRCDIR = .
EXEC = Syclist.e

LIBS =  -lm

##############
## GFORTRAN ##
##############
COMPILER= gfortran

# Compilation rules
%.o: $(SRCDIR)/%.f95
	@echo "F90: $(COMPILER) -c $(FFLAGS) $(<)"
	@$(COMPILER) $(FFLAGS) -c $(SRCDIR)/$*.f90 -o $*.o

# Source files
SOURCES_F90 = formats.f95 \
paramters.f95 \
SYCLIST.f95

# Objects lists
SOURCES=$(SOURCES_F90:%.f90=%.o)

OBJECTS=$(SOURCES)

# Compile and link the code
evolData_new: $(OBJECTS)
	@echo "==========LINKING========="
	@echo "Fortran options: $(FFLAGS) --> $(EXEC)"
	@$(COMPILER) $(FFLAGS) $(OBJECTS) $(LIBS) -o $(EXEC)

clean:
	rm -f *.o *.mod
