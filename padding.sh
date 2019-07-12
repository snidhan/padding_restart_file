#!/bin/bash
#ulimit -s unlimited
#ulimit -s 999999999
rm padding.x #*.vtk *.dat
#gfortran -O3 -mcmodel=medium -o a.out 3d_vtk.f90

# problem with -r8 -132 (can't see in paraview)
#ifort -O3 -r8 -132 -mcmodel=large -o a.out 3d_vtk.f90

#ifort -O3 -mcmodel=large -o a.out 3d_vtk.f90 -L/usr/local/LAPACK_KARU  -llapack -lblas
#ifort -O3 -mcmodel=medium -heap-arrays 100000 -o a.out 3d_vtk.f90 -L/usr/local/LAPACK_KARU  -llapack -lblas
ifort -O3 -heap-arrays 100000 -mcmodel=large -o padding.x padding.f90
./padding.x
