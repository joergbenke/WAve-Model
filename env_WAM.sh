#!/usr/bin/env bash

module purge

ml git
ml emacs

module load gcc/11.2.0-gcc-11.2.0
module load openmpi/4.1.2-gcc-11.2.0
module load netcdf-c/4.8.1-gcc-11.2.0 
module load netcdf-fortran/4.6.1-gcc-11.2.0
module load ncview/2.1.8-gcc-11.2.0

module list


