#!/bin/sh
# The madtclsh path is longer than 32 characters. So, we take advantage
# of the fact that a backslash continues a comment line in tcl \
exec MADROOT/bin/madtclsh "$0" ${1+"$@"}

# $Id: printCedarRecord.cgi,v 1.3 2008/07/29 15:47:05 brideout Exp $

lappend auto_path MADROOT/madtcllib
package require cgi
package require mtl  

# Uncomment following statement to see errors in Web page
#cgi_debug -on

madrigal madrigal
set madroot [madrigal cget -madroot]
set htmlstyle [madrigal cget -htmlstyle]
set htmlstyle [string trimleft [set htmlstyle] "<"]
set htmlstyle [string trimright [set htmlstyle] ">"]
regsub -nocase BODY $htmlstyle "" htmlstyle

cgi_eval {
    cgi_input "fileName=/export/home/mad2000/experiments/1998/mlh/20jan98/mil980120g.003&recno=1"
    cgi_import fileName
    cgi_import recno

    if {[llength [cgi_import_list]] == 2} {
        cgi_html {
            cgi_head {
	      cgi_title "Record Contents"
            }
            cgi_body {
    
	        cgi_center {
	            cgi_h1 "Record Contents"
	        }

                cgi_preformatted {
                    catch {exec MADROOT/bin/printCedarRecords $fileName $recno $recno} result
                    cgi_puts $result 
                }
            }
        }

    } else {
	cgi_http_head
	cgi_html {
	    cgi_body $htmlstyle {
		cgi_p "Error: Must specify 2 parameters"
	    }
	}
    }

}

exit
