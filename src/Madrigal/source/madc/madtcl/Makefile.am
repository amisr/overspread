# $Id: Makefile.am,v 1.2 2009/01/15 21:12:04 brideout Exp $
#
# Makefile.am for madtclsh
#

AM_CFLAGS = -I../include

bin_PROGRAMS = madtclsh


# ----------------------------------------------------------------------
#       Madtclsh source files
# ----------------------------------------------------------------------

madtclsh_SOURCES = Isr.c  Mad.c  madTclAppInit.c

LDADD = -L../../madf/geolib -L../madrec \
        -lgeo -lmadrec $(TCL_LIB_SPEC) $(FLIBS)


