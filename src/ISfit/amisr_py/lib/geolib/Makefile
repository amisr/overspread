# $Id: Makefile.gnu,v 1.13 2006/01/23 18:48:48 brideout Exp $
#
# ----------------------------------------------------------------------
#       Gnu makefile for libgeo (Distribution version)
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
#       Site-dependent definitions - change these as needed
# ----------------------------------------------------------------------

# Madrigal root directory
MADROOT =  /

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

LDLIBS = -L/Users/mnicolls/Documents/Work/ISfit/AMISR_fitter_py/lib/geolib -lgeo -lm

# ----------------------------------------------------------------------
#       Fortran compiler options
# ----------------------------------------------------------------------

#FFLAGS = -O -fPIC -Wall -Wuninitialized -fno-second-underscore
FFLAGS = -O -Wall -D_GNU_SOURCE -D_DARWIN -Wuninitialized -fno-second-underscore
#FFLAGS = -fno-automatic -finit-local-zero -O3 -ff77 -ffast-math -Wall -D_GNU_SOURCE -D_DARWIN -bundle -dynamiclib 

# ----------------------------------------------------------------------
#       Link editor options 
# ----------------------------------------------------------------------

LDFLAGS = -Xlinker

# ----------------------------------------------------------------------
#       Fortran source files
# ----------------------------------------------------------------------

SOURCES = carmel.f convrt.f coord.f csconv.f diplat.f dsf.f gaspct.f \
          gdmag.f gdran.f gdv.f gmet.f gtd7.f hfun.f integ.f invar.f \
          invlat.f iterat.f  lines.f lintra.f look.f milmag.f minv.f \
          mtran3.f point.f rfun.f rpcart.f sprod.f startr.f          \
          tnf.f vadd.f vctcnv.f vmag.f vsub.f geo-cgm.f              \
          calndr.f dater.f dates.f iday.f izlr.f idmyck.f jdater.f   \
          jday.f moname.f monum.f wkname.f T01_01.f Geopack_2005.f   \
	  sfc_convert_geo_coord.f cgm_to_altitude.f altitude_to_cgm.f \
	  rylm.f blkdat95.f isrim.f basis.f conduct.f irisub.f       \
          irifun.f cira.f igrf.f igrf11.f iritec.f iridreg.f         \
          geogcm01iface.f

# ----------------------------------------------------------------------
#       Object files
# ----------------------------------------------------------------------
OBJECTS = $(SOURCES:%.f=%.o)

# ----------------------------------------------------------------------
#       Libraries and Programs
# ----------------------------------------------------------------------

all: clean libgeo.a libgeo.so.1 testGeolib testGeolibs sfctest testISRIM verify

libgeo.a: $(OBJECTS)
	libtool -static -o $@ $(OBJECTS)
	gcc -dynamiclib -all_load -flat_namespace -single_module -o libgeo.dylib libgeo.a /usr/local/lib/libgfortran.dylib
#/sw/lib/libg2c.a

libgeo.so.1: $(OBJECTS)
	$(FC) -o $@ -shared $(OBJECTS)
	cp libgeo.so.1 $(LIBDIR)
	rm -f $(LIBDIR)/libgeo.so
	ln -s $(LIBDIR)/libgeo.so.1 $(LIBDIR)/libgeo.so

testGeolib: testGeolib.o
	$(LINK.f) testGeolib.o $(LDLIBS) -o $@
	cp testGeolib $(BINDIR)/testGeolib

testGeolibs: testGeolib.o
	$(LINK.f) testGeolib.o -static $(LDLIBS) -o $@
	cp testGeolibs $(BINDIR)/testGeolibs
	
sfctest: sfctest.o
	$(LINK.f) sfctest.o $(LDLIBS) -o $@
	
testISRIM: testISRIM.o
	$(LINK.f) testISRIM.o $(LDLIBS) -o $@

tgeogcm: tgeogcm.o
	$(LINK.f) tgeogcm.o $(LDLIBS) -o $@


# ----------------------------------------------------------------------
#       Utility
# ----------------------------------------------------------------------
 
verify:
	./testGeolib
	@cmp -s testGeolib.out testGeolib.out.rock && \
           echo 'OK - testGeolib.out and testGeolib.out.rock are the same' || \
        (cmp -s testGeolib.out testGeolib.out.rock || \
           echo 'Fail - testGeolib.out and testGeolib.out.rock are different')
	./testGeolibs
	@cmp -s testGeolib.out testGeolib.out.rock && \
           echo 'OK - testGeolib.out and testGeolib.out.rock are the same' || \
        (cmp -s testGeolib.out testGeolib.out.rock || \
           echo 'Fail - testGeolib.out and testGeolib.out.rock are different')

clean cleanup:
	rm -f testGeolib testGeolibs libgeo.a libgeo.so.1 sfctest testISRIM \
              core *.o *.BAK *.CKP *%
print listing:
	$(PRINT) Makefile $(SOURCES) testGeolib.f testGeolib.out.rock
