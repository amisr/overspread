#!/bin/sh
# The madtclsh path is longer than 32 characters. So, we take advantage
# of the fact that a backslash continues a comment line in tcl \
exec /opt/tcl/src/tcl8.0/unix/bin/tclsh8.0 "$0" ${1+"$@"}

##############################################################################

# Writes data statements appropriate for milmag.f from IGRF "*.DAT" files 
# found in specified directory

if { $argc < 1 } {
   puts "Usage: cofrm.tcl {IGRF directory}"
   exit
}

if {![file isdirectory $argv]} {
   puts "Usage: cofrm.tcl {IGRF directory}"
   exit
}

##########################################################################

proc readshc { fname } {
# @c reads IGRF model file, set global variables g and h
# @a fname IGRF model filename (e.g. DGRF45.DAT)
# @r tcl list of order of g and epoch
   global g h
   set fd [open $fname]
   set text [split [read $fd] \n]
   close $fd

   set i 0
   foreach line $text {
      switch [llength $line] {
         4 { 
	    set n [lindex $line 0]
	    set m [lindex $line 1]
	    set g($n,$m) [expr int([lindex $line 2])]
	    set h($n,$m) [expr int([lindex $line 3])]
         }
         3 { 
           set nmax [lindex $line 0]
           set epoch [lindex $line 2]
         }
         default {continue}
      }
   }
   list $nmax $epoch
}

##########################################################################

proc arrset { ari aro} {
# @c set aro to ari and clears ari.
# @a ari input array
# @a aro output array
   upvar 1 $ari ain
   foreach element [array names ain] {
      uplevel set ${aro}($element) $ain($element)
   }
   unset ain
}

##########################################################################

proc oline { line num } {
# @c fortran data statement output of IGRF coefficients
# @a line line content to ouput
# @a num data array index
   global iset
   upvar $num n
   switch [expr {$n%9}] {
      8 {
        incr n
        set n1 [expr {117-$n?$n+9:121}]
	puts "$line/"
	puts "      DATA (LG(I,${iset}),LGT(I,${iset}),I=[expr {$n+1}],$n1)"
      }
      0 {
	incr n
	puts "[string range $line 0 10]/[string range $line 12 end],"
      }
      default {
        puts -nonewline "$line"
        if {[incr n] < 121} { 
           puts ","
        } else {
           puts "/"
        }
      }
   }
}

##########################################################################

proc go {basef secf} {
# @c reads base and secular coefficient files, converts to Milmag format
# @c and outputs appropriate data statements to stdout.
# @a basef base coefficient filename.
# @a secf secular coefficient filename.
   global g h gh iset

   set extrap 0
   if {[regexp -nocase s\. $secf]} { set extrap 1 }

   set nm1 [readshc $basef]
   set epoch [lindex $nm1 1]
   set nm1 [lindex $nm1 0]
   arrset g g1
   arrset h h1
   set nm2 [readshc $secf]
   set epoche [lindex $nm2 1]
   arrset g g2
   arrset h h2

   if { $extrap } {
      set factor [expr {$epoche-$epoch}]
      foreach element [array names g1] {
         if {[info exists g2($element)]} {
	    set g2($element) [expr {$g1($element)+$factor*$g2($element)}]
	    set h2($element) [expr {$h1($element)+$factor*$h2($element)}]
         } else {
	    set g2($element) $g1($element)
	    set h2($element) $h1($element)
         }
      }
   }

   set iset [expr {int($epoch-1940)/5}]
   puts "      DATA TMSET(${iset})/${epoch}/, DNMAX(${iset})/11/"
   puts "      DATA (LG(I,${iset}),LGT(I,${iset}),I=1,9)"
   set i 0
   oline "     +     / 1, 10" i
   for {set m 0} {$m <= $nm1} {incr m} {
      for {set n 1} {$n <= $nm1} {incr n} {
         if {[info exists g1($n,$m)]} {
            set diff [expr {($g2($n,$m)-$g1($n,$m))*2}]
            oline [format "     +       $g1($n,$m), $diff"] i
         }
      }
      set m1 [expr {$m+1}]
      for {set n 1} {$n <= $nm1} {incr n} {
         if {[info exists h1($m1,$n)]} {
            set diff [expr {($h2($m1,$n)-$h1($m1,$n))*2}]
            oline [format "     +       $h1($m1,$n), $diff"] i
         }
      }
   }
}

##########################################################################

# Change to directory which contains IGRF coefficient files (see milmag.f
# for info on source of the coefficient files).
cd $argv
set datFiles [lsort [glob *.{DAT,dat}]]
set baseFiles [lrange $datFiles 0 [expr {[llength $datFiles]-2}]]
set secFiles [lrange $datFiles 1 end]
foreach basef $baseFiles secf $secFiles {
   go $basef $secf
}

exit
