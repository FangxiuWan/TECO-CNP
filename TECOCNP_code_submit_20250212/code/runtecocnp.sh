#!/bin/bash

# -----------------------------------------------------------------------
# Compile and run the TECO-CNP biogeochemical model
# You need to install the dependences according to the README file
# -----------------------------------------------------------------------

# Step 1: Compile the Fortran source files
echo "Compiling Fortran source files..."
gfortran -ffree-form -c FileSize.f90 ParasModule.f90 SASpinUp.f90 LIMITATION.f90 NPUptakeDemand.f90 NPDynamic.f90 MCMC.f90 TECO_CNP_main.f90

# Step 2: Link the object files into an executable
echo "Linking object files..."
#gfortran -o teco_cnp.exe FileSize.o ParasModule.o SASpinUp.o LIMITATION.o NPUptakeDemand.o NPDynamic.o MCMC.o TECO_CNP_main.o -L/YourPath -llapacke -llapack -lrefblas
gfortran -o teco_cnp.exe FileSize.o ParasModule.o SASpinUp.o LIMITATION.o NPUptakeDemand.o NPDynamic.o MCMC.o TECO_CNP_main.o -L/home/wan_fx/usr/lib -llapacke -llapack -lrefblas

# Step 3: Execute the program with the provided arguments
# To run the TECO-CNP model, you can adjust the configuration by using different combinations of arguments. 
# Please refer to the definitions of the arguments in the `TECO-CNP_main.f90` file.
echo "Running the executable..."
./teco_cnp.exe 2021 2021 3 0 1 5 0

echo "Done."