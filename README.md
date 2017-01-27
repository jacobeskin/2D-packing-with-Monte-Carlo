# 2D-packing-with-Monte-Carlo
Description of this project is in the pdf file 2Dpacking.pdf. Programs are written in Fortran (F90 standard). The Mersenne 
twister 
module was NOT written by me, the authors are credited in the module source code file.


Compilation instructions. The compilation of the program happens by the means of the makefile called "makesqr". Example of how 
to compile:

jacob@jeskin:~/Tila2/final$ make -f makesqr

The following should happen:

gfortran -g -c mtfort90.f90
gfortran -g -c squares.f90
gfortran -g -c packing.f90
gfortran -g -o packing mtfort90.o squares.o packing.o 


Short run instructions:

The program name is just "packing", and it takes as an input only one number, that has to be bigger than 2, from the command 
line. An example of running the program is as follows:

jacob@jeskin:~/Tila2/final$ ./packing 10
Seed from /dev/urandom: 1036762744

 Initial footprint was          110
 Final footprint is           75

The program writes 3 files, InitConfig, FinalConfig and fpVSsim which contain instructions for Xgraph to plot the initial 
configuration, final configuration and the footprint as a function of simulation steps respectively. 

Example of drawing the initial configuration:

jacob@jeskin:~/Tila2/final$ xgraph -bb -tk InitConfig

And an example of plotting the file fpVSsim:

jacob@jeskin:~/Tila2/final$ xgraph  fpVSsim
