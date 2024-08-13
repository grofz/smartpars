# Makefile is such a pain
#
.SUFFIXES:
.PHONY = all clean

fflags = -Og -g -Wall -Wextra -pedantic -std=f2018 -fimplicit-none -fcheck=all -fPIC
cc = gfortran $(fflags)

obj = \
			file2mat.o \
			smartpars.o \
			test_localpars.o \

main = test_main.o
exe = test.x

# not needed actually
src = $(obj:*.o=*.f90)
src += $(main:*.o=*.f90)

# default goal
all : $(exe)

$(exe) : $(obj) $(main)
	$(cc) -o $@ $(obj) $(main)

$(obj) $(main) : Makefile

# module dependencies

smartpars.o : file2mat.o

test_localpars.o : smartpars.o

$(main) : test_localpars.o

# default rules
%.o : %.f90
	$(cc) -c $< -o $@

# one phoney target
clean:
	-rm -f $(obj) $(main) *.mod $(exe)
