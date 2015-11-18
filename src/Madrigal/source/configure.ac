#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#
#  Main Madrigal configure.ac file
#
#  $Id: configure.ac,v 1.7 2009/01/23 14:48:22 brideout Exp $

AC_PREREQ(2.59)
AC_INIT([Madrigal], [2.5], [brideout@haystack.mit.edu])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])
AC_CONFIG_HEADER([config.h])
AC_CONFIG_MACRO_DIR([m4])

# Checks for programs.
AC_PROG_CC
AC_PROG_LIBTOOL
AC_PROG_F77

# Mix Fortran and C
AC_F77_LIBRARY_LDFLAGS
AC_F77_WRAPPERS

# Checks for libraries

# Need to run certain Macros from tcl.m4 in order to find tcl libraries, since
# not always named libtcl.* - this should define TCL_LIB_SPEC
SC_PATH_TCLCONFIG
SC_LOAD_TCLCONFIG
AC_SUBST([TCL_LIB_SPEC])

#AC_CHECK_LIB([freetype], [FT_New_Memory],,
#  [AC_MSG_ERROR([Please install freetype library before installing Madrigal])])

# Checks for header files.
AC_CHECK_HEADER([tcl.h],,
  [AC_MSG_ERROR([Required header tcl.h not found - please install before installing Madrigal])])

# Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_SIZEOF([long])
AM_CONDITIONAL([IS_32BIT], [test $ac_cv_sizeof_long = 4])

# Checks for library functions.

AC_CONFIG_FILES([
    Makefile
    madf/geolib/Makefile
    madf/applications/Makefile
    madc/madrec/Makefile
    madc/madtcl/Makefile
    madc/testF77/Makefile
])
AC_OUTPUT
