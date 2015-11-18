#!/bin/sh
# The madtclsh path is longer than 32 characters. So, we take advantage
# of the fact that a backslash continues a comment line in tcl \
exec /Users/mnicolls/Documents/Work/Madrigal/bin/madtclsh "$0" ${1+"$@"}
 
lappend auto_path /Users/mnicolls/Documents/Work/Madrigal/madtcllib
package require cgi
package require mtl
 
# Usage: madPublish.cgi expPath fileName

# madPublish.cgi is a simple script to togle the 10th entry (previously called spare5)
# in fileTab.txt for a given Madrigal data file.
# The input parameters are expPath, the pathname of the experiment and
# fileName, the name of the Madrigal dataset

########################
# Start Execution Here #
########################
 
# Use cgi_input with arguments when running from command line
cgi_input "expPath=/opt/madrigal_250302/experiments/2002/son/06mar02&fileName=son020306g.001"
cgi_import expPath
cgi_import fileName
 
madExperiment experiment
set madroot [experiment cget -madroot]
set madserver [experiment cget -madserver]
set madservercgi [experiment cget -madservercgi]
set madserverroot [experiment cget -madserverroot]
set htmlstyle [experiment cget -htmlstyle]
 
# Determine if this access is from an internal site
# now determine this via python script
set isTrusted [exec $madroot/bin/isTrusted.py]
if {$isTrusted == 1} {
    set remoteAddress "internal"  
}  else {
    set remoteAddress "public"
}
  
if { $remoteAddress != "internal" } { 
    # illegal access, die
    exit 
} 
 
# Do the work: open the fileTab file and read all the entries checking each one.
# When the entry related to fileName is read, toggle the privateBit between 0 and 1.
# Close the filteTab file, reopen it for write and write out the new entries.

set expTabFile [ open "$expPath/fileTab.txt" "r"]
while { [ gets $expTabFile nextLine ] > 0 } {
    if {[string match "$fileName*" $nextLine]} {
        set fileTabParameters [ split $nextLine "," ]
        if { [ lindex $fileTabParameters 10 ] != 0 } {
            set fileTabParameters [ lreplace $fileTabParameters 10 10 "0" ]
        } else {
            set fileTabParameters [ lreplace $fileTabParameters 10 10 "1" ]
        }
        set nextLine [ join $fileTabParameters "," ]
    }
    lappend newfileTabParameters $nextLine
}
close $expTabFile
if { ![ catch { set expTabFile [ open "$expPath/fileTab.txt" "w" ] } returnMessage ] } {
    foreach nextLine $newfileTabParameters {
       puts $expTabFile $nextLine
    }
    close $expTabFile
    # Build the output Web Page, with a redirect back to the experiment listing
    cgi_eval {
        cgi_http_head {
            cgi_redirect $env(HTTP_REFERER) 
        }
        cgi_html {
            cgi_head {
                cgi_title "Madrigal dataset publisher"
                cgi_center {
                    cgi_h1 "Madrigal dataset publisher" 
                }
                cgi_center {
                    cgi_p "$expPath/$fileName published" 
                    cgi_p "Please use the back button to return to the experiment listing and reload" 
                }
            }
        }
    }
} else {
    # Build the output Web Page, with error message
    cgi_eval {
        cgi_http_head {
        }
        cgi_html {
            cgi_head {
                cgi_title "Madrigal dataset publisher"
                cgi_center {
                    cgi_h1 "Madrigal dataset publisher"
                }
                cgi_center {
                    cgi_p "A problem occured in updating the metadata:"
                    cgi_p "$returnMessage"
                    cgi_p "Please use the back button to return to the experiment listing."
                }
            }
        }
    }
}

exit

