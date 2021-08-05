.POSIX:

FC=gfortran
FFLAGS=-o
FFLAGS2=-c -g -O0 -Wno-tabs# -Og  -O0 -Wtabs -Wall

.PHONY: all clean

main: build #clean
	echo "Building forpy is finished"

forpy.o:
	$(FC) $(FFLAGS2) ../src/forpy.f90

main.o : forpy.o
	$(FC) $(FFLAGS2) ../src/examples.f90 

build: main.o
	$(FC) $(FFLAGS) examples.exe examples.o forpy.o

#clean:
#	del *.o && del *.mod 
