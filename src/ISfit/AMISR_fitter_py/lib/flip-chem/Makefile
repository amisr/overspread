# Solaris Make program
MAKE =  make

# GNU Fortran compiler
FC =  gfortran

# GNU C compiler
CC =  gcc

# Library directory
LIBDIR =

# Binary directory
BINDIR =

# ----------------------------------------------------------------------
#       End of site-dependent definitions
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
#       Print Command
# ----------------------------------------------------------------------

PRINT = lp

# ----------------------------------------------------------------------
#       Library directives
# ----------------------------------------------------------------------

LDLIBS = -lm

# ----------------------------------------------------------------------
#       Fortran compiler options
# ----------------------------------------------------------------------

#FFLAGS = -O -fPIC -Wall -Wuninitialized -fno-second-underscore
FFLAGS = -D_GNU_SOURCE -D_DARWIN -Wuninitialized -fno-second-underscore
#FFLAGS = -fno-automatic -finit-local-zero -O3 -ff77 -ffast-math -Wall -D_GNU_SOURCE -D_DARWIN -bundle -dynamiclib

# ----------------------------------------------------------------------
#       Link editor options
# ----------------------------------------------------------------------

LDFLAGS = -Xlinker

# ----------------------------------------------------------------------
#       Fortran source files
# ----------------------------------------------------------------------

SOURCES = FLIP-CHEM.f KEMPRN.f PESIMP.f Rates.f RSPRIM.f MSIS00.f

# ----------------------------------------------------------------------
#       Object files
# ----------------------------------------------------------------------
OBJECTS = $(SOURCES:%.f=%.o)

# ----------------------------------------------------------------------
#       Libraries and Programs
# ----------------------------------------------------------------------

all: clean libflip.a libflip.so.1

libflip.a: $(OBJECTS)
	libtool -static -o $@ $(OBJECTS)
	gcc -dynamiclib -all_load -flat_namespace -single_module -o libflip.dylib libflip.a /usr/local/Cellar/gcc/5.2.0/lib/gcc/5/libgfortran.dylib

libflip.so.1: $(OBJECTS)
	$(FC) -o $@ -shared $(OBJECTS)
	cp libgeo.so.1 $(LIBDIR)
	rm -f $(LIBDIR)/libflip.so
	ln -s $(LIBDIR)/libflip.so.1 $(LIBDIR)/libflip.so

clean:
	@echo "Cleaning ... "
	rm -f *# *~ *.o *.a
