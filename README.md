# SYCLIST
Synthetic clusters, Isochrones and Stellar Tracks

The code has openmp capabilities. In case you want to use the parallel version, you need :
- to compile with gfortran ;
- to have an installed openmp working with gfotran.

To compile, use the makefile in the project. You can use the environment variable $OPENMP_SYCLIST
to compile with openmp, or to compile with :
make OPENMP_SYCLIST=yes

The makefile checks whether a workable openmp environment exists on your machine, and switch
to OPENMP_SYCLIST=no if not.