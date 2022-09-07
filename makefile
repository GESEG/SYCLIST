# MAKEFILE pour SYCLIST
SRCDIR = .
EXEC = Syclist.e

LIBS =  -lm

##############
## GFORTRAN ##
##############
COMPILER= gfortran

# Source files
SOURCES_F95 = formats.f95 \
parameters.f95 \
SYCLIST.f95

# Objects lists
SOURCES=$(SOURCES_F95:%.f95=%.o)

OBJECTS=$(SOURCES) main.o
LIBOBJECTS=$(SOURCES) amuse_helpers.o

# Compilation rules
%.o: $(SRCDIR)/%.f95
	@echo "F90: $(COMPILER) -c $(FFLAGS) $(<)"
	@$(COMPILER) $(FFLAGS) -c $(SRCDIR)/$*.f95 -o $*.o


# Compile and link the code
evolData_new: $(OBJECTS)
	@echo "==========LINKING========="
	@echo "Fortran options: $(FFLAGS) --> $(EXEC)"
	@$(COMPILER) $(FFLAGS) $(OBJECTS) $(LIBS) -o $(EXEC)

libsyclist.a: $(LIBOBJECTS)
	ar crs $@ $^
	#@echo "==========LINKING========="
	#@echo "=========INTERFACE========"
	#@echo "Fortran options: $(FFLAGS) --> $(EXEC)"
	#@$(COMPILER) $(FFLAGS) -c $(OBJECTS) SYCLIST.f95 amuse_helpers.f95 $(LIBS) # -o $(EXEC)


clean:
	rm -f *.o *.mod
