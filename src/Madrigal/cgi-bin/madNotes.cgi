#!/bin/sh
# The madtclsh path is longer than 32 characters. So, we take advantage
# of the fact that a backslash continues a comment line in tcl \
exec /Users/mnicolls/Documents/Work/Madrigal/bin/madtclsh "$0" ${1+"$@"}
 
lappend auto_path /Users/mnicolls/Documents/Work/Madrigal/madtcllib
package require cgi
package require mtl
 
# Usage: madNotes.cgi expPath messageContent author referrer
# madNotes.cgi is a script to add text to notes.txt in a Madrigal experiment

########################
# Start Execution Here #
########################
 
# Use cgi_input with arguments when running from command line
cgi_input "expPath=&author=unset&messageContent=emptymessage&http_referer=none"
cgi_import expPath
cgi_import author
cgi_import messageContent
cgi_import http_referer

# reset the referrer pointer unless given expicitly
if { [ string length $http_referer ] == 0 } {
    set http_referer $env(HTTP_REFERER) 
}

madExperiment experiment
set madroot [experiment cget -madroot]
set madserver [experiment cget -madserver]
set madservercgi [experiment cget -madservercgi]
set madserverroot [experiment cget -madserverroot]
set htmlstyle [experiment cget -htmlstyle]
set htmlstyle [string trimleft [set htmlstyle] "<"]
set htmlstyle [string trimright [set htmlstyle] ">"]
set notesmanager [experiment cget -notesmanager]

# Check input and append to notes.txt if OK
set updated 0
set returnMessage ""
set messageContent [ string trim $messageContent ]
if { [ string length $messageContent ] != 0 && [ string first "@" $author ] > 0 } {
    # replace all carriage returns by html break     
    regsub -all "\n" $messageContent {<br>} messageContent 
    catch { exec echo "$messageContent<br>" >> $expPath/notes.txt } returnMessage
    catch { exec echo [exec date "+%e %B %Y, $author<br><br>"] >> $expPath/notes.txt  } returnMessage   
    catch { exec chmod a+rw $expPath/notes.txt } ReturnMessage
    # silently copy addition to the database manager (if specified)
    if { ![string match $notesmanager "0"] } {
        set mailMessage "/tmp/MADmail[ pid ][ exec date +%m%d%H%M%S ].tmp"
        set mailSubject "MADRIGAL note addition"
        catch { exec echo "Notification of note added to MADRIGAL" > $mailMessage }
        catch { exec echo " " >> $mailMessage }
        catch { exec echo "$expPath/notes.txt" >> $mailMessage }
        catch { exec echo " " >> $mailMessage }
        catch { exec echo "$messageContent" >> $mailMessage }
        catch { exec echo " " >> $mailMessage }
        catch { exec echo "Added: [exec date "+%e %B %Y by $author"]" >> $mailMessage }
        catch { exec echo "Additional information:" >> $mailMessage }
        if {[info exists env(REMOTE_HOST)]} {
            catch { exec echo "Client host: $env(REMOTE_HOST)" >> $mailMessage }
        }
        if {[info exists env(REMOTE_ADDR)]} {
            catch { exec echo "Client address: $env(REMOTE_ADDR)" >> $mailMessage }
        }
        if {[info exists env(REMOTE_USER)]} {
            catch { exec echo "Client username: $env(REMOTE_USER)" >> $mailMessage }
        }
        catch { exec /bin/mail -s $mailSubject $notesmanager < $mailMessage } returnMessage
        catch { exec rm $mailMessage }
    }
    set updated 1
}
 
if { !$updated } {
    # Build the output Web Page, with fill in form
    cgi_eval {
        cgi_title "Madrigal Notes System"
        cgi_body $htmlstyle {
            cgi_center {
                cgi_h2 "Madrigal Notes System"                     
                cgi_p "Please use the boxes below to enter your notes and email addess."
                cgi_form madNotes {
                    cgi_textarea messageContent rows=8 cols=70 
                    cgi_br
                    put "Email address:   "; cgi_text author size=40
                    cgi_p 
                    cgi_text http_referer size=120 type=hidden
                    cgi_text expPath size=60 type=hidden
                    cgi_submit_button "=Submit notes"
	            cgi_reset_button "Reset form"
                    cgi_p [cgi_url "Return to the experiment page" $http_referer]
                }
            }
        }
    }
} else {
    # Build the output Web Page, with a redirect back to the experiment listing
    cgi_eval {
        if {$http_referer != "None"} {
            cgi_http_head {
                 cgi_redirect $http_referer
            }
        }
        cgi_html {
            cgi_head {
                cgi_title "Madrigal Notes System"
            }
        }
    }
}

exit

