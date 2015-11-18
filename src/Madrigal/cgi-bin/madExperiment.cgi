#!/bin/sh
# The madtclsh path is longer than 32 characters. So, we take advantage
# of the fact that a backslash continues a comment line in tcl \
exec /Users/mnicolls/Documents/Work/Madrigal/bin/madtclsh "$0" ${1+"$@"}

# $Id: madExperiment.cgi,v 1.43 2008/10/10 19:36:30 brideout Exp $ 

lappend auto_path /Users/mnicolls/Documents/Work/Madrigal/madtcllib
package require cgi
package require mtl

# usage: madExperiment.cgi exp expTitle displayLevel

# madExperiment.cgi is a simple Madrigal experiment access program.
# madExperiment.cgi reads the metadata files in that experiment directory
# and the full Instrument and Data Type metadata files and then generates
# a page with links to:
    # CEDAR files in the experiment directory 
    # Html files in the experiment directory
    # Files named index.html in any subdirectory of the experiment directory. 
    
# madExperiment.cgi also checks for the existence of a plots/$filename/records directory
# in the experiment directory.  If found, it adds a note next to the File Summary link.
# Links to plots for individual records show up in summarizeCedarFile.cgi

# If this script is called from a trusted host (see trustedIPs in the Madrigal
# root directory) all data sets are displayed together with an option to
# toggle the status of individual data sets between public and private.
# For all other hosts, only data sets marked public will be displayed and links to 
# the index.html files in any subdirectories whose titles include the string
# "rivate" (matching Private and private) will be surpressed.

########################
# Start Execution Here #
########################

# Use cgi_input with arguments when running from command line
cgi_input "exp=1998/mlh/20jan98&expTitle=TestExpTitle&displayLevel=0"
cgi_import exp
# catch requests from older MADRIGAL installations which dont send the experiment title
if { [ catch { cgi_import expTitle } ] } {
   set expTitle ""
}
cgi_import displayLevel

madExperiment experiment
set madroot [experiment cget -madroot]
set madserver [experiment cget -madserver]
set madservercgi [experiment cget -madservercgi]
set madserverroot [experiment cget -madserverroot]
set madservercgiabs [experiment cget -madservercgiabs]
set htmlstyle [experiment cget -htmlstyle]
set htmlstyle [string trimleft [set htmlstyle] "<"]
set htmlstyle [string trimright [set htmlstyle] ">"]
regsub -nocase BODY $htmlstyle "" htmlstyle
set metadataDir [file join $madroot metadata]
set expPath [file join $madroot experiments $exp]
set cgiPath http://[file join $madserver $madservercgi]
set linkPath http://[file join $madserver $madserverroot experiments $exp]
set recpath $expPath/plots

# Determine if this access is from an internal site
# now determine this via python script
set isTrusted [exec $madroot/bin/isTrusted.py]
if {$isTrusted == 1} {
    set remoteAddress "internal"  
}  else {
    set remoteAddress "public"
}


experiment configure -expName $exp
if {[ catch { experiment read } ]} { }

set displayTypes(1) 1
set displayTypes(2) 1
set displayTypes(3) 1
set displayTypes(4) 1
if {$displayLevel == 0} {
    set displayTypes(3) 0
    set displayTypes(4) 0
}

# Get instrument table
madInstrument instruments
instruments read
instruments getEntries instrumentMnemonic instrumentName \
                       instrumentLatitude instrumentLongitude \
		       instrumentAltitude contactName contactAddress1 \
		       contactAddress2 contactAddress3 \
		       contactCity contactState contactPostalCode \
		       contactCountry contactTelephone contactEmail
foreach code [array names instrumentMnemonic] {
    #puts "$code $instrumentMnemonic($code) $instrumentName($code)"
}

# Get data type table
madDataType dataTypes
dataTypes read
dataTypes getEntries dataTypeDescription
foreach code [array names dataTypeDescription] {
    #puts "$code $dataTypeDescription($code)"
}

# See if we should show comment about plots/records
set hasRecordDir 0
if {[file isdirectory $recpath] == 1} {
    set hasRecordDir 1
}


# Get experiment
set files [experiment cget -files]

experiment getFileEntries experimentID fileDataType category spare1 hasCatalog \
                          hasHeader analysisDate analysisTime statusDesc privateBit
set fileNamesTmp {}
foreach dataFile [array names experimentID] {
    set fileNamesTmp [lappend fileName $dataFile]
    #puts [format "%14s %10.10d %4.4d %1.1d %8s %8s %6s %8s %6s %1.1d %1.1d" \
          $dataFile $experimentID($dataFile) $fileDataType($dataFile) \
          $category($dataFile) $spare1($dataFile) $hasCatalog($dataFile) \
          $hasHeader($dataFile) $analysisDate($dataFile) \
          $analysisTime($dataFile) $statusDesc($dataFile) $privateBit($dataFile)]
}
#puts "fileNames = $fileNames"
set fileNames [lsort  $fileNamesTmp ]

set data [experiment cget -data]
#puts $data
experiment getDataEntries dataExperimentID dataDataType
set days {}
foreach day [array names dataExperimentID] {
    set days [lappend days $day]
    #puts [format "%14s %10.10d %4.4d" \
        $day $dataExperimentID($day) $dataDataType($day)]
}
#puts "days = $days"

# Get the data type description for each Cedar file (n.b.:fileDataType=kindat)
foreach fileName $fileNames {
    set fileDataTypeDescription($fileName) "Unknown data type"
    catch {set fileDataTypeDescription($fileName)  \
            $dataTypeDescription($fileDataType($fileName))}
#    puts "$fileName $fileDataType($fileName) $fileDataTypeDescription($fileName)"
}

# Get the file category description and statusDescription for each Cedar file
set categoryDescriptions(1) "default file"
set categoryDescriptions(2) "variant file"
set categoryDescriptions(3) "history file"
set categoryDescriptions(4) "real-time file"
foreach fileName $fileNames {
    set fileCategoryDescription($fileName)  \
        $categoryDescriptions($category($fileName))
    if {[string length $statusDesc($fileName)] < 2} {
        set statusDescList($fileName) final
    } else {
        set statusDescList($fileName) $statusDesc($fileName)
    }
#    puts "$fileName $category($fileName) $fileCategoryDescription($fileName)"
}

# Read the experiment table
set id [experiment cget -id]
set url [experiment cget -url]
set name [experiment cget -name]
set siteID [experiment cget -siteID]
set startDate [experiment cget -startDate]
set startTime [experiment cget -startTime]
set endDate [experiment cget -endDate]
set endTime [experiment cget -endTime]
set instrumentCode [experiment cget -instrumentCode]
set expSecurityCode [experiment cget -securityCode]
#regsub madtoc $expURL madExperiment expURL
regsub madtoc $url madExperiment url

# Process experiment table information
set expInstrumentName "Unknown Instrument - Instrument code = $instrumentCode"
foreach code [array names instrumentMnemonic] {
    if {$code == $instrumentCode} {
        set expInstrumentName $instrumentName($code)
        break
    }
}
set year [string range $startDate 0 3]
set month [string range $startDate 4 5]
set day [string range $startDate 6 7]
set startDate $month/$day/$year
set hour [string range $startTime 0 1]
set minute [string range $startTime 2 3]
set second [string range $startTime 4 5]
set startTime $hour:$minute:$second
set year [string range $endDate 0 3]
set month [string range $endDate 4 5]
set day [string range $endDate 6 7]
set endDate $month/$day/$year
set hour [string range $endTime 0 1]
set minute [string range $endTime 2 3]
set second [string range $endTime 4 5]
set endTime $hour:$minute:$second

# Build the Experiment Web Page
# -----------------------------

cgi_eval {

    #cgi_debug -on

    cgi_http_head

    cgi_html {

        cgi_head {
            cgi_title "Madrigal Experiment $expPath"
        }
    
        cgi_body $htmlstyle {
            cgi_center {
                cgi_h1 $expInstrumentName
                cgi_h2 $expTitle
		cgi_put "<table width='80%' border='1'><tr>"
		cgi_put "<td>Return to <a href='/$madserverroot'>Madrigal homepage</a></td>"
		cgi_put "<td><a href='/$madserverroot/wt_expPage.html'>Tutorial</a> on this page</td>"
		cgi_put "<td>Return to <a href='accessData.cgi'>access data</a> page</td>"
		cgi_put "</tr></table>"
            }

            # Add Start, end date to page
            cgi_hr
            cgi_table cellpadding=5 {
                cgi_table_row {
                    cgi_th "Start Time: $startDate $startTime"
                    cgi_th "End Time: $endDate $endTime"
                }
            }
            cgi_hr

            # Add Madrigal files to page
            cgi_p [cgi_bold "CEDAR Format Datasets:"]
            cgi_bullet_list {
                # Sort by category
		# if no non-realtime files found, accept realtime
		set fileFound 0
                for {set j 1} {$j <= 4} {incr j} {
		    # check if non-realtime file found; otherwise, always display realtime
		    if {$j == 4} {
		        if {$fileFound == 0} {
			    set displayTypes(4) 1
			}
		    }
                    foreach fileName $fileNames {
                        if {$category($fileName) == $j && $displayTypes($j) == 1 && ( $privateBit($fileName) == 0 || $remoteAddress == "internal" ) } {
			    if {$j < 4} {
			        set fileFound 1
			    }
                            cgi_li "$fileName - $fileCategoryDescription($fileName) for $fileDataTypeDescription($fileName) - status: $statusDescList($fileName)"
                            cgi_bullet_list {
                                set qp1 [cgi_cgi_set expName $exp]
                                set qp2 [cgi_cgi_set fileName $fileName]
                                set query "?$qp1&$qp2"
				regsub -all " " $expTitle "+" cgiExpName
				
				# See if we should show a link to the catalog/Records
				if { $hasCatalog($fileName) != 0 ||  $hasHeader($fileName) != 0} {
				    # show link
				    set catLink $cgiPath/displayCatHead.py$query&expTitle=$cgiExpName&displayLevel=$displayLevel
				    cgi_li "[cgi_url "View file description from the catalog and/or header records" $catLink]"
				}
				
				set des "Choose parameters (and optional filters) to print - <i>standard method to access data</i>"
				set query "?fileName=[file join $expPath $fileName]&expName=$cgiExpName"
				set madDataScript "madDataBrowse"
				set cmd $cgiPath/$madDataScript$query
                                cgi_li "[cgi_url "Print file as ascii (isprint)" $cmd] - $des"
				
				set des "Click to print entire file in one step using parameters in file (no derived parameters or filters)"
				set query "?madFilename=[file join $expPath $fileName]&experimentName=$cgiExpName&instrumentName=$expInstrumentName"
				set madDataScript "basicIsprint.py"
				set cmd $cgiPath/$madDataScript$query
                                set listItem "[cgi_url "One-step file print" $cmd] - $des"
				cgi_bullet_list {cgi_li $listItem}
				
				set query "?$qp1&$qp2"
                                set cmd $cgiPath/summarizeCedarFile.cgi$query
				regsub -all " " $expTitle "+" cgiExpName
				
                                # See if we should show comment about plots/records
                                set hasRecordDir 0
                                if {[file isdirectory $recpath/$fileName/records] == 1} {
                                    set hasRecordDir 1
                                }
                                # change comment if plots/$filename/records exists
                                if { $hasRecordDir == 0 } {
                                    set des "Lists individual records, allows printing each record separately"
                                } else {
                                    set des "Lists individual records, allows printing each record separately, <i>individual record plots available</i>"
                                }
                                cgi_li "[cgi_url "Print individual records" $cmd] - $des"
				
				set query "?fileName=[file join $expPath $fileName]"
                                set cmd $cgiPath/getMadfile.cgi$query
                                set des "Download $fileName in selected Cedar format (advanced users only)"
                                cgi_li "[cgi_url "Download file" $cmd] - $des"
						if { ![ catch { set writableFileTab [ open "$expPath/fileTab.txt" "a"] } ] } {
                                	close $writableFileTab
                                	if { $remoteAddress == "internal" } {
                                    	set query "?expPath=$expPath&fileName=$fileName"
                                    	if { $privateBit($fileName) != 0 } {
                                        	cgi_li "[cgi_bold "Unpublished"] [cgi_url "(Click to make data set visible on the public web site)" $cgiPath/madPublish.cgi$query]"
                                    	} else {
                                        	cgi_li "Published [cgi_url "(Click to hide data set from the public web site)" $cgiPath/madPublish.cgi$query]"
                                    	}
                                	}
						}
                            }
                            cgi_br
                        }
                    }
                }
            }

            # Add links to html files to page
            cgi_hr
            cgi_p [cgi_bold "Additional information:"]

            # Extract titles from html files in experiment directory
            set htmlFiles1 {}
            catch { [set htmlFiles1 [glob $expPath/*.html]] }
            set i 0
            foreach htmlFile $htmlFiles1 {
                set htmlFiles1 [lreplace $htmlFiles1 $i $i [file tail $htmlFile]]
                incr i
            }
            # Extract titles from index.html files in experiment subdirectories
            set htmlFiles2 {}
            catch { [set htmlFiles2 [glob $expPath/*/index.html]] }
            set i 0
            foreach htmlFile $htmlFiles2 {
                set fileTail ""
                set fileSplit [file split $htmlFile]
                set nSplit [llength $fileSplit]
                set fileTail2 [lindex $fileSplit [expr $nSplit-2]]
                set fileTail1 [lappend fileTail [lindex $fileSplit [expr $nSplit-1]]]
                set fileTail [file join $fileTail2 $fileTail1]
                set htmlFiles2 [lreplace $htmlFiles2 $i $i $fileTail]
                incr i
            }
	    # Extract titles from index.html files in experiment subdirectories two levels down
            set htmlFiles3 {}
            catch { [set htmlFiles3 [glob $expPath/*/*/index.html]] }
            set i 0
	    set expPathLen [string length $expPath]
	    set expPathLen [expr 1 + $expPathLen]
            foreach htmlFile $htmlFiles3 {
	        set htmlFileLen [string length $htmlFile]
                set fileTail [string range $htmlFile $expPathLen $htmlFileLen]
                set htmlFiles3 [lreplace $htmlFiles3 $i $i $fileTail]
                incr i
            }
            set htmlFiles [concat $htmlFiles1 $htmlFiles2 $htmlFiles3]
            set i 0
	    set htmlArrayList {}
            foreach htmlFile $htmlFiles {
                set fp [open $expPath/$htmlFile r]
                set htmlTitle($htmlFile) "No Title"
                while {[gets $fp line] >= 0} {
                    if {[string match -nocase "*<TITLE>*" $line]} {
                        regsub -nocase <TITLE> $line "" line
                        regsub -nocase </TITLE> $line "" line
                        set htmlTitle($htmlFile) $line
			lappend htmlArrayList $line
			lappend htmlArrayList $htmlFile
                    }
                }
                close $fp
                incr i
            }
	    array set htmlArray $htmlArrayList
	    set htmlNames [array names htmlArray]
	    set htmlNames [lsort $htmlNames]

            # List html files by title
            cgi_bullet_list {
                foreach htmlName $htmlNames {
		    set result [array get htmlArray $htmlName]
		    set htmlFile [lindex $result 1]
                    if { $remoteAddress == "internal" || [ string first "rivate" $htmlTitle($htmlFile) ] == "-1" } {
                        cgi_li [cgi_url $htmlTitle($htmlFile) $linkPath/$htmlFile target=viewWindow]
                    }
                }
            }

            # Temporary section to handle public/private control of additional data
            if { ![catch { exec /bin/cat $madroot/local_rules_of_the_road.txt } returnMessage ] } {
                cgi_p $returnMessage
            }
 
            # List graphics files
            if { $remoteAddress == "internal" } {
               set listSources 1
            } else {
               set listSources 0
            }
           catch { exec $madservercgiabs/getMadplot.py h $expPath $listSources } cmdOutput
           if { "$cmdOutput" != "" } {
	       
                cgi_table border align=center {
                    cgi_table_row {
                        th "IMAGES"
                    }
                   foreach s [ split $cmdOutput "\n" ] {
                       cgi_table_row {            
                           td "$s"
                       }           
                    }
                }
            }
 
            if { $remoteAddress == "internal" } {
                # List original datasets
                set dataList {}
                if { [catch { set globList [glob $expPath/*sigma.gz] } ] == 0 } {
		    foreach thisGlob $globList {
			lappend dataList $thisGlob
		    }
		}
		if { [catch { set globList [glob $expPath/*.tar.gz] } ] == 0 } {
		    foreach thisGlob $globList {
			lappend dataList $thisGlob
		    }
		}
		if { [catch { set globList [glob $expPath/*.tgz] } ] == 0 } {
		    foreach thisGlob $globList {
			lappend dataList $thisGlob
		    }
		}
                if { [ llength $dataList ] } {
                    cgi_p [cgi_bold "Original fitted data files"]
                    cgi_bullet_list {         
                        foreach fileEntry $dataList {
                            cgi_li [cgi_url "[ file tail $fileEntry ]   (gzipped file)" $linkPath/[ file tail $fileEntry ] ]
                        }
                    } 
                }
            }
 
            # add notes section (if writable notes.txt exists in this experiment)
            if { ![ catch { set writableNotes [ open "$expPath/notes.txt" "a"] } ] } {
                close $writableNotes 
                cgi_p [cgi_bold "Notes"]
                if { ![catch { exec /bin/cat $expPath/notes.txt } returnMessage ] } { 
                    cgi_p $returnMessage
                }
                set query "?expPath=$expPath&author=&messageContent=&http_referer="
                cgi_p [cgi_url "Add to these notes" $cgiPath/madNotes.cgi$query]
            }

            cgi_hr
        }
    }
}
