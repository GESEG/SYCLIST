# MAKEFILE pour SYCLIST
SRCDIR = .
EXEC = Syclist.e

##############
## GFORTRAN ##
##############
COMPILER = gfortran

LIBS = -lm
FFLAGS = -frecursive #-fcheck=all

############
## OPENMP ##
############
# OPENMP_SYCLIST = auto | yes | no
#   auto : active OpenMP s'il est disponible (defaut)
#   yes  : impose OpenMP (erreur si indisponible)
#   no   : compile sans OpenMP
# Priorite : ligne de commande > variable d'environnement > "auto"
#   make OPENMP_SYCLIST=no
#   export OPENMP_SYCLIST=no   (dans ~/.bashrc ou ~/.bash_profile)
OPENMP_SYCLIST ?= auto

ifeq ($(filter $(OPENMP_SYCLIST),auto yes no),)
  $(error OPENMP_SYCLIST doit valoir auto, yes ou no (valeur actuelle : "$(OPENMP_SYCLIST)"))
endif

# Test : compile ET lie un petit programme utilisant omp_lib (marche sous Linux et macOS)
ifneq ($(OPENMP_SYCLIST),no)
  OMP_TEST := $(shell d=.omptest_$$$$; \
    printf 'program t\nuse omp_lib\nprint *, omp_get_max_threads()\nend program t\n' > $$d.f90 \
    && $(COMPILER) -fopenmp $$d.f90 -o $$d.out >/dev/null 2>&1 && echo yes; \
    rm -f $$d.f90 $$d.out)
  ifneq ($(strip $(OMP_TEST)),yes)
    ifeq ($(OPENMP_SYCLIST),yes)
      $(error OPENMP_SYCLIST=yes mais $(COMPILER) ne sait pas compiler avec -fopenmp)
    endif
    OPENMP_MODE = no
  else
    OPENMP_MODE = yes
  endif
else
  OPENMP_MODE = no
endif

ifeq ($(OPENMP_MODE),yes)
  FFLAGS += -fopenmp
endif

$(info Compilateur : $(COMPILER) | OpenMP : $(OPENMP_MODE))

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

# Recompile automatiquement tout si le mode OpenMP a change
.openmp_mode: FORCE
	@echo "$(OPENMP_MODE)" | cmp -s - $@ || echo "$(OPENMP_MODE)" > $@

$(OBJECTS): .openmp_mode

clean:
	rm -f *.o *.mod .openmp_mode

.PHONY: clean FORCE
FORCE:
