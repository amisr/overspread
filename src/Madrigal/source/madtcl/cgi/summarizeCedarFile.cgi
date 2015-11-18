#!/bin/sh
# The madtclsh path is longer than 32 characters. So, we take advantage
# of the fact that a backslash continues a comment line in tcl \
exec MADROOT/bin/madtclsh "$0" ${1+"$@"}

# $Id: summarizeCedarFile.cgi,v 1.14 2008/07/29 15:47:05 brideout Exp $

# summarizeCedarFile prints a one line per record summary of a CEDAR
# file, followed by a summary of the contents of the file.  The file may
# be any of the 5 supported CEDAR formats (Madrigal, Blocked Binary, Cbf,
# Unblocked Binary or ASCII"), and may include any mixture of prologue,
# header and data records. The format of the file is determined
# automatically.

# summarizeCedarFile.cgi also now shows a link next to any individual record if 
# there exists a file with the form *ddddd*, where ddddd is a five digit record number
# that corresponds to the record number of the record (example: plot00005.gif would
# show up to the fifth record, since record numbers start at 1).  These files must
# be located in the plots/$fileName/records directory under the experiment directory.  More than
# one link will be shown if there is more than one file with the correct record number
# in its file name.  Note that this code will need to be changed to support files with
# more than 99,999 records.

lappend auto_path MADROOT/madtcllib
package require cgi
package require mtl

# Uncomment following statement to see errors in Web page
#cgi_debug -on

madrigal madrigal
set madroot [madrigal cget -madroot]
set madserver [madrigal cget -madserver]
set madservercgi [madrigal cget -madservercgi]
set madserverroot [madrigal cget -madserverroot]
set htmlstyle [madrigal cget -htmlstyle]
set htmlstyle [string trimleft [set htmlstyle] "<"]
set htmlstyle [string trimright [set htmlstyle] ">"]
regsub -nocase BODY $htmlstyle "" htmlstyle

# Get arguments
cgi_input "expName=1998/mlh/20jan98&fileName=mil980120g.003"
cgi_import expName
cgi_import fileName

set metadataDir $madroot/metadata
set exppath $madroot/experiments/$expName
set recpath $exppath/plots
set cgiPath http://$madserver/$madservercgi
set linkpath http://$madserver/$madserverroot/experiments/$expName
set madrigalFile [file join $exppath $fileName]

# Get parameter codes
# Note that we can't use env(MAD ROOT) because MAD ROOT would be processed
#   during installation
set env([string toupper madroot]) $madroot
cedarCode cedarCode

# Create madrec object for the input file. Specify file type 1 for automatic
# determination of the CEDAR file type
mad madin
$madin open 1 $madrigalFile

# Count azimuth and elevation scan plots
set nazscans 0
set azscans {}
catch {set azscans [glob [file join $expPath plot/azscan*]]}
foreach azscan $azscans {
    incr nazscans 1
}
set nelscans 0
set elscans {}
catch {set elscans [glob [file join $expPath plots elscan*]]}
foreach elscan $elscans {
    incr nelscans 1
}

# See if we should show links to plots/records
set hasRecordDir 0
if {[file isdirectory $recpath/$fileName/records] == 1} {
    set hasRecordDir 1
}


while {[set status [$madin getNextRecord]] == 0} {
    set krec [$madin get krec]
    if {$krec != 1002 && $krec != 1101} {
        continue
    }
    set kinst [$madin get kinst]
    set kindat [$madin get kindat]
    set modes {}
    set mdtypes {}
    set kinsts {}
    set kindats {}
    set nmodes {}
    set parcodes1d {}
    set parcodes2d {}
    set pl [expr int([expr 1000000.0*[$madin get parm1d 402]])]
    set modes [lappend modes $pl]
    set mdtype [expr int([$madin get parm1d 3325])]
    set mdtypes [lappend mdtypes $mdtype]
    set kinsts [lappend kinsts [$madin get kinst]]
    set kindats [lappend kindats [$madin get kindat]]
    set nmodes [lappend nmodes 0]
    set startIndex [$madin get startIndex]
    set endIndex startIndex
    set parcodes1d [append parcodes1d [$madin get parcodes1d]]
    set parcodes2d [append parcodes2d [$madin get parcodes2d]]
    if {$krec == 1002 || $krec == 1101} {
        break
    }
}

# set status flags depending on which parameters are present
if {$pl == 0} {
    set hasPl 0
} else {
    set hasPl 1
}
if {$mdtype == 0 || $pl == 0} {
    set hasMdtype 0
} else {
    set hasMdtype 1 
}



$madin close
$madin open 1 $madrigalFile

cgi_eval {

    cgi_http_head

    cgi_html {

        cgi_head {
            cgi_title "Summary of [file tail $madrigalFile]"
        }
    
        cgi_body $htmlstyle {
    
            cgi_center {
                cgi_h1 "Summary of [file tail $madrigalFile]"
		cgi_put "<table width='80%' border='1'><tr>"
		cgi_put "<td>Return to <a href='/$madserverroot'>Madrigal homepage</a></td>"
		cgi_put "<td><a href='/$madserverroot/wt_fileSummary.html'>Tutorial</a> on this page</td>"
		cgi_put "<td>Return to <a href='accessData.cgi'>access data</a> page</td>"
		cgi_put "</tr></table>"
            }

            h2 Contents
            cgi_li [cgi_url "Record Summary" #recordSummary]
            cgi_li [cgi_url "Parameter Summary" #parameterSummary]

            h2 "[cgi_anchor_name recordSummary] Record Summary"
	    if {$hasMdtype == 1} {
	        cgi_puts "Pulse types"
		cgi_puts " -- s: <i>single pulse</i>"
		cgi_puts " -- a: <i>alternating code</i>"
	        cgi_puts " -- b: <i>Barker code</i>"
		cgi_puts " -- m: <i>multipulse</i>"
		cgi_puts " -- r: <i>random</i>"
	        cgi_puts " -- c: <i>chirp</i>"
	    }
	    
            cgi_puts "<p>Select a record number to see the contents of that record</p>"

            cgi_preformatted {


                # Print a one line per record summary of the file
		# there are now three state variables to consider: hasRecordDir, hasPl, hasMdtype
		# note hasMdtype == 1 requires hasPl == 1, so only 6 possibilites
                if {$hasRecordDir == 0 && $hasPl == 0 && $hasMdtype == 0} {
                    cgi_p "record    Start Time          End Time         kinst krec"
                } elseif  {$hasRecordDir == 1 && $hasPl == 0 && $hasMdtype == 0} {
                    cgi_p "record    Start Time          End Time         kinst krec    record links"
                } elseif  {$hasRecordDir == 1 && $hasPl == 1 && $hasMdtype == 0} {
                    cgi_p "record    Start Time          End Time         kinst krec    plen  record links"
		} elseif  {$hasRecordDir == 1 && $hasMdtype == 1} {
                    cgi_p "record    Start Time          End Time         kinst krec    type  plen  record links"
		} elseif  {$hasRecordDir == 0 && $hasPl == 1 && $hasMdtype == 0} {
                    cgi_p "record    Start Time          End Time         kinst krec    plen" 
		} elseif  {$hasRecordDir == 0 && $hasPl == 1 && $hasMdtype == 1} {
                    cgi_p "record    Start Time          End Time         kinst krec    type  plen"   
                }
		
                set rec 0
                while {[set status [$madin getNextRecord]] == 0} {
                    incr rec
                    set recno [format %4d $rec]
		    set krec [$madin get krec]
                    set startTime [$madin get startTime]
                    set yr1 [lindex $startTime 0]
                    set mo1 [lindex $startTime 1]
                    set dy1 [lindex $startTime 2]
                    set hr1 [lindex $startTime 3]
                    set mn1 [lindex $startTime 4]
                    set sc1 [lindex $startTime 5]
                    set zero 0
                    if {[string length $hr1] == 1} {
                        set hr1 $zero$hr1
                    }
                    if {[string length $mn1] == 1} {
                        set mn1 $zero$mn1
                    }
                    if {[string length $sc1] == 1} {
                        set sc1 $zero$sc1
                    }
                    set endTime [$madin get endTime]
                    set yr2 [lindex $endTime 0]
                    set mo2 [lindex $endTime 1]
                    set dy2 [lindex $endTime 2]
                    set hr2 [lindex $endTime 3]
                    set mn2 [lindex $endTime 4]
                    set sc2 [lindex $endTime 5]
                    set zero 0
                    if {[string length $hr2] == 1} {
                        set hr2 $zero$hr2
                    }
                    if {[string length $mn2] == 1} {
                        set mn2 $zero$mn2
                    }
                    if {[string length $sc2] == 1} {
                        set sc2 $zero$sc2
                    }
		    
		    # set kind of record string
		    set krecStr "unknown"
		    set krec [expr int([$madin get krec])]
		    if {$krec == 2001 || $krec == 2101} {
		        set krecStr "catalog "
		    } elseif {$krec == 3002 || $krec == 3101} {
		        set krecStr "header  "
		    } elseif {$krec == 1002 || $krec == 1101} {
		        set krecStr "data    "
		    }
		    
		    # get optional parameters
		    if {$hasPl == 1 && ($krec == 1002 || $krec == 1101)} {
		        set pl [expr int([expr 1000000.0*[$madin get parm1d 402]])]
		    } elseif {$hasPl == 1} {
		        set pl 0
		    }
		    if {$hasMdtype == 1 && ($krec == 1002 || $krec == 1101)} {
			set mdtype [expr int([$madin get parm1d 3325])]
			if {$mdtype == 115} {
			    set mdStr "  s "
			} elseif {$mdtype == 97} {
			    set mdStr "  a "
			} elseif {$mdtype == 98} {
			    set mdStr "  b "
			} elseif {$mdtype == 99} {
			    set mdStr "  c "
			} elseif {$mdtype == 109} {
			    set mdStr "  m "
			} elseif {$mdtype == 114} {
			    set mdStr "  r "
			} else {
			    set mdStr "    "
			}
		    } elseif {$hasMdtype == 1} {
			set mdStr "    "
		    }

                    if {($krec != 1002 && $krec != 1101) || ($hasPl == 0 && $hasMdtype == 0)} {
                	set outLine [format "$mo1/$dy1/$yr1 $hr1:$mn1:$sc1  \
                             $mo2/$dy2/$yr2 $hr2:$mn2:$sc2%6i  %s" \
                             [$madin get kinst] \
                             $krecStr]
		    } elseif {$hasPl == 1 && $hasMdtype == 0} {
		        set outLine [format "$mo1/$dy1/$yr1 $hr1:$mn1:$sc1  \
                             $mo2/$dy2/$yr2 $hr2:$mn2:$sc2%6i  %s%6i" \
                             [$madin get kinst] \
                             $krecStr $pl]
		    } elseif {$hasPl == 1 && $hasMdtype == 1} {
		        set outLine [format "$mo1/$dy1/$yr1 $hr1:$mn1:$sc1  \
                             $mo2/$dy2/$yr2 $hr2:$mn2:$sc2%6i  %s%s%6i" \
                             [$madin get kinst] \
                             $krecStr $mdStr  $pl]
		    }
                    set qp1 [cgi_cgi_set fileName $madrigalFile]
                    set qp2 [cgi_cgi_set recno $recno]
                    set query "?$qp1&$qp2"
                    set cmd $cgiPath/printCedarRecord.cgi$query
                    
                    if {$hasRecordDir == 0} {
                        cgi_puts "[cgi_url $recno $cmd] $outLine"
                    } else {
                        # create appropriate links for this record to plots/records dir
                        set numStr [format %05d $rec]
                        set globStr $recpath/$fileName/records/*$numStr*
                        set fileList [glob -nocomplain $globStr]
                        # see if any files found
                        if { [llength $fileList] != 0 } {
                            set addedLink " "
                            foreach recFile $fileList {
                                # get new link name
                                set newlinkName [file tail $recFile]
                                set addedLink "$addedLink<a href=$linkpath/plots/$fileName/records/$newlinkName>"
                                set addedLink "$addedLink$newlinkName</a> "
				# create png link
				set extIndex [string last "." $newlinkName]
				set newFilename [string range $newlinkName  0 [expr $extIndex - 1]]
				set newFilename $newFilename.png
				set pngLink "<a href=getMadplot.py/$expName/plots/$fileName/records/$newFilename>Png version</a>"
                            }
                            cgi_puts "[cgi_url $recno $cmd] $outLine $addedLink $pngLink"
                        
                        } else {
                            cgi_puts "[cgi_url $recno $cmd] $outLine"
                        }
                    }
		    
		    set endIndex [$madin get endIndex]
		    
		    if {$krec != 1002 && $krec != 1101} {
                        continue
                    }

                    set pl [expr int([expr 1000000.0*[$madin get parm1d 402]])]
                    set newMode 1
                    set i 0
        	    foreach mode $modes {
                	if {$mode == $pl && \
        		    [lindex $kinsts $i]==[$madin get kinst] && \
                            [lindex $kindats $i]==[$madin get kindat]} {
        		set newMode 0
        		set nmode [lindex $nmodes $i]
        		set nmode [incr nmode 1]
        		set nmodes [lreplace $nmodes $i $i $nmode]
        	    }
        	    incr i 1
        	    }
        	    if {$newMode} {
        	        set modes [lappend modes $pl]
        	        set kinsts [lappend kinsts [$madin get kinst]]
        	        set kindats [lappend kindats [$madin get kindat]]
        	        set nmodes [lappend nmodes 1]
        	    }
  
                }
            }

            h2 "[cgi_anchor_name parameterSummary] Parameter Summary"

            cgi_preformatted {
                if {$nazscans > 0 || $nelscans > 0} {
                    cgi_p
                    cgi_puts "Number of azimuth scans: $nazscans"
                    cgi_puts "Number of elevation scans: $nelscans"
                }

                cgi_p
                cgi_puts "1D Parameters:"
                foreach parcode1d $parcodes1d {
                    set cedarCodeDescription [$cedarCode description $parcode1d]
                    cgi_puts [format "%8d %s" $parcode1d $cedarCodeDescription]
                }
                cgi_p
                cgi_puts "2D Parameters:"
                foreach parcode2d $parcodes2d {
                    set cedarCodeDescription [$cedarCode description $parcode2d]
                    cgi_puts [format "%8d %s" $parcode2d $cedarCodeDescription]
                }

                if {[llength $modes] > 0} {
                    cgi_p
                    cgi_puts "Radar Waveforms:"
                    set i 0
                    cgi_puts "     pl   kinst  kindat   n"
                    foreach mode $modes {
                        set qcal " "
	                cgi_puts [format "    %4d %4d     %4d  %4d" \
                              $mode \
                              [lindex $kinsts $i]   \
                              [lindex $kindats $i]   \
                              [lindex $nmodes $i]]
                        incr i 1
                    }
                }
            }
        }
    }
}

# Close file and delete madrec object
$madin close
$madin destroy
