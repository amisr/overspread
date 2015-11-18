#!/bin/sh
# The madtclsh path is longer than 32 characters. So, we take advantage
# of the fact that a backslash continues a comment line in tcl \
exec MADROOT/bin/madtclsh "$0" ${1+"$@"}

# $Id: getMadfile.cgi,v 1.13 2008/11/07 16:15:44 brideout Exp $

lappend auto_path MADROOT/madtcllib
package require cgi
package require mtl

proc sendForm { fileName htmlstyle contact madserverroot} {
    # Send form to the browser.  This is done when the server is
    # called with a fileName argument but no fileType argument.

    cgi_http_head

    cgi_html {

	cgi_head {
            cgi_title "Madrigal Data File Server"
        }

	cgi_body $htmlstyle {
    
            cgi_center {        
		cgi_h1 "Madrigal Data File Server"
		cgi_put "<table width='80%' border='1'><tr>"
		cgi_put "<td>Return to <a href='/$madserverroot'>Madrigal homepage</a></td>"
		cgi_put "<td><a href='/$madserverroot/wt_browseExp.html'>Tutorial</a> on this page</td>"
		cgi_put "<td>Return to <a href='accessData.cgi'>access data</a> page</td>"
		cgi_put "</tr></table>"
            } 
            cgi_hr 
            cgi_h2 "Read This First" 
            cgi_p "Use of the Madrigal Database is generally subject to the
		   [cgi_url "CEDAR Database Rules-of-the-Road" \
		   http://cedarweb.hao.ucar.edu/catalog/Rules.html].
		   Prior permission to download a file is not required.
		   However, the user is required to establish early
		   contact with any organization whose data are
		   involved in the project to discuss the intended
		   usage. Data are often subject to limitations which
		   are not immediately evident to new users.  Before
		   they are formally submitted, draft copies of all
		   reports and publications must be sent to the contact
		   scientist at all data-supplying organizations along
		   with an offer of co-authorship to scientists who
		   have provided data. This offer may be declined.  The
		   Database and the organizations that contributed data
		   must be acknowledged in all reports and
		   publications, and whenever this data is
		   made available through another database. If you have any questions about
		   appropriate use of these data, contact $contact"
            cgi_br
            cgi_hr   
            cgi_form getMadfile {

		cgi_p "<INPUT TYPE=\"hidden\" NAME=\"fileName\" VALUE=\"$fileName\" SIZE=15>"    

		cgi_p "<H3>Translate [file tail $fileName] to the following CEDAR file type:</H3>"
                cgi_center {
                cgi_table {
			    cgi_table_data valign=top {
                    cgi_radio_button "fileType=0" checked
                    cgi_put "Madrigal"
                    cgi_br
                    cgi_radio_button "fileType=1"
                    cgi_put "Blocked Binary"
                    cgi_br
                    cgi_radio_button "fileType=2"
                    cgi_put "NCAR Binary (CBF)"
                    cgi_br
                    cgi_radio_button "fileType=3"
                    cgi_put "Unblocked Binary"
                    cgi_br
                    cgi_radio_button "fileType=4"
                    cgi_put "NCAR ASCII"
                    cgi_br
                }
                }
                    cgi_submit_button "=Download File"
                }
		cgi_p "</FORM>"
                cgi_p ""
	    }
	    cgi_hr
            cgi_h2 "File Formats"
	    cgi_p "The five CEDAR file types all use the CEDAR logical
		record format for the data in the file. All data are stored
		as 16-bit integers.  The four binary formats use 16-bit 2's
		complement binary integers and the ASCII
		format uses ASCII representations of the 16-bit integers.
		The data format is described in the
		[cgi_url "CEDAR Database Format document" \
		/$madserverroot/cedarFormat.pdf].
		The simplest file format is Unblocked binary, which is
		simply a sequence of binary records. The Blocked binary and
		NCAR CBF binary are sequential, tape-oriented formats and
		are described in the
		[cgi_url "CEDAR Database Format document" \
		/$madserverroot/cedarFormat.pdf].
		The [cgi_url "Madrigal format" \
		/$madserverroot/dev_madrigalFormat.html]
		is a random-access disk-oriented format which was designed
		for a Fortran environment. The NCAR ASCII format is an
		easy-to-use format which is widely used for submitting
		small datasets to NCAR, and for preparing catalog and
		header records. It is described in the
		[cgi_url "CEDAR Database Format document" \
		/$madserverroot/cedarFormat.pdf]."
		cgi_hr
	}
    }
}

proc sendFile { fileName fileType } {
    # Send file to the user.  This is done when the server is
    # called with a fileName argument and a fileType argument.

    global madroot
    global total done

    # Translate the file to the specified format
    set fileType [expr $fileType+20]

    set outFile [file join $madroot experiments stage [file tail $fileName]]
    translateCedarFile $fileName $outFile $fileType 
    
    # remove all old files from stage
    set oldFiles1 [split [glob [file join $madroot experiments stage * ] ] ]
    set now [clock seconds]
    foreach oldFile $oldFiles1 {
        file stat $oldFile statInfo
	if {[expr ($now - $statInfo(mtime))] > 1000 } {
	    file delete -force $oldFile
	}
    }

    # Send the data
    set encoding x-madrigal
    set description "MHR CEDAR file - fileType=$fileType"
    set filesize [file size $fileName]
    set filesize [file size $outFile]
    set total 0
    set chunk 8192
    fconfigure stdout -translation crlf
    puts "Content-Description: $description"
    puts "Content-Length: $filesize"
    puts "Content-Disposition: inline;fileName=[file tail $fileName]"
    puts "Content-Type: application/x-octet-stream,encoding=$encoding;name=[file tail $fileName]"
    puts ""
    flush stdout

    set fd [open $outFile]
    fconfigure $fd -translation binary
    fconfigure stdout -translation binary
    fcopy $fd stdout -command [list CopyMore $fd stdout $chunk] -size $chunk
    vwait done
    flush stdout
}

proc CopyMore {in out chunk bytes {error {}}} {
   global total done
   incr total $bytes
   if {([string length $error] != 0) || [eof $in]} {
      set done $total
      close $in
   } else {
      fcopy $in $out -command [list CopyMore $in $out $chunk] -size $chunk
   }
   return
}

########################
# Start Execution Here #
########################

madrigal madrigal
set contact [madrigal cget -contact]
set madroot [madrigal cget -madroot]
set madserverroot [madrigal cget -madserverroot]
set exproot [file join $madroot experiments]
set htmlstyle [madrigal cget -htmlstyle]
set htmlstyle [string trimleft [set htmlstyle] "<"]
set htmlstyle [string trimright [set htmlstyle] ">"]
regsub -nocase BODY $htmlstyle "" htmlstyle

# Uncomment following statement to see errors in Web page
#cgi_debug -on

# Get arguments
#cgi_input "fileName=/export/home/mad2000/experiments/1998/mlh/20jan98/mil980120g.008"
#cgi_input "fileName=/export/home/mad2000/experiments/1998/mlh/20jan98/mil980120g.003&fileType=4"
cgi_input
catch {cgi_import fileName}
catch {cgi_import fileType}

# File doesn't exist
if {[info exists fileName] && ![file exists $fileName]} {
    cgi_eval {
        cgi_http_head
        cgi_html {
	    cgi_body $htmlstyle {
                cgi_p "Error - $fileName does not exist<P>"
            }
        }
    }
   
# Don't have fileType argument. Display form.
} elseif {[info exists fileName] && ![info exists fileType]} {
    cgi_eval {
        sendForm $fileName $htmlstyle $contact $madserverroot
    }

# Have all arguments. Download file in appropriate format
} elseif {[info exists fileName] && [info exists fileType]} {
    cgi_eval {
        sendFile $fileName $fileType
    }

} else {
    cgi_eval {
        cgi_http_head
        cgi_html {
	    cgi_body $htmlstyle {
                cgi_p "Error - must specify file name as argument<P>"
            }
        }
    }
}
