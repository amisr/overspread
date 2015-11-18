proc ffw_print {} {
    
    # ffw_print prints a flattened subset of the data in a CEDAR File.
    # The file may be any of the 5 supported CEDAR formats (Madrigal, Blocked
    # Binary, Cbf, Unblocked Binary or ASCII"), and may include any mixture
    # of prologue, header and data records. The format of the file is
    # determined automatically.

    global mad cedarCode
    global infile outfile printParmsIn filterParmsIn filterMinsIn filterMaxsIn
    global printDestination

    set debug 0
    set minus "-"
    
    # Get print parameter codes
    # Ignore parameter names which cedarCode does not recognize
    # Since ffw uses deprecated cedarGetParmArray, need to hard-code year, month, day
    set printCodes {}
    set printFormats {}
    set numCodes [$cedarCode numCodes]
    foreach printParm $printParmsIn {
        for {set i 0} {$i < $numCodes} {incr i} {
	    set hardcode 0
	    if {[string compare $printParm YEAR] == 0} {
	       set code 10
	       set mnemonic YEAR
	       set printFormats [lappend printFormats "%4.0f   "]
	       set hardcode 1
	    } elseif {[string compare $printParm MONTH] == 0} {
	       set code 11
	       set mnemonic MONTH
	       set printFormats [lappend printFormats "%2.0f   "]
	       set hardcode 1
	    } elseif {[string compare $printParm DAY] == 0} {
	       set code 12
	       set mnemonic DAY
	       set printFormats [lappend printFormats "%2.0f   "]
	       set hardcode 1
	    } else {
	       set code [$cedarCode code $i]
	       set mnemonic [string trim [$cedarCode mnemonic $code]]
	    }
	    if {[string compare $printParm $mnemonic] == 0} {
		set printCodes [lappend printCodes $code]
		if {[string compare $hardcode 0] == 0} {
                    set printFormats [lappend printFormats [$cedarCode format $code]]
		}
		break
	    } elseif {[string compare $printParm D$mnemonic] == 0} {
		set printCodes [lappend printCodes $minus$code]
                set printFormats [lappend printFormats [$cedarCode format $code]]
		break
            }

	}
    }
    set npcodes [llength $printCodes]
    
    # Get filter parameter codes
    # Ignore parameter names which cedarCode does not recognize
    set filterCodes {}
    set filterMins {}
    set filterMaxs {}
    set numCodes [$cedarCode numCodes]
    set j 0
    foreach filterParm $filterParmsIn {
        for {set i 0} {$i < $numCodes} {incr i} {
            set code [$cedarCode code $i]
	    set mnemonic [string trim [$cedarCode mnemonic $code]]
	    if {[string compare $filterParm $mnemonic] == 0} {
		set filterCodes [lappend filterCodes $code]
		set filterMins [lappend filterMins [lindex $filterMinsIn $j]]
		set filterMaxs [lappend filterMaxs [lindex $filterMaxsIn $j]]
		break
	    }
	}
        incr j
    }
    set nfcodes [llength $filterCodes]

    if {$debug == 1} {    
        puts "printCodes = $printCodes"
        puts "printFormats = $printFormats"
        puts "filterCodes = $filterCodes"
        puts "filterMins = $filterMins"
        puts "filterMaxs = $filterMaxs"
    }
    
    set missing [$mad get missing]

    if {$outfile != "stdout"} {
        set out [open $outfile a+]
    } else {
        set out "stdout"
    }
    
    # Print specified data in flat file format
    
    # Loop over records
    for {set i 0} {$i<999999} {incr i} {
	set status [$mad getNextRecord]
	if {$status != 0} {
	    if {$status == "No errors" || $status == -1} {
		break
	    } else {
		puts "Error reading $infile"
		exit
	    }      
	}
    
	if {$i == 0} {
	    set startJday0 [$mad get startJday]
	}
    
	# Get time parameters from prolog
	set startJday [$mad get startJday]
	set endJday [$mad get endJday]
	set ut1 [expr 24.0*($startJday - int($startJday0))]
	set ut2 [expr 24.0*($endJday - int($startJday0))]
	set uth [expr 0.5*($ut1 + $ut2)]
        set line [$mad parmArray $printCodes $filterCodes $filterMins $filterMaxs]

        set lline [concat $line]
        set llist [llength $lline]
        if {$llist == 0} {
            continue
        }

        set nparms [llength $printCodes]
        set nlines [expr $llist/$nparms]
        for {set l 0} {$l < $nlines} {incr l} {
            set line ""
            for {set k 0} {$k < $nparms} { incr k} {
	        set printCode [lindex $printCodes $k]
	        set printFormat [lindex $printFormats $k]
                set n [expr $l*$nparms+$k]
                set parmString [format $printFormat [lindex $lline $n]]
                set line $line$parmString
	        # puts -nonewline "[lindex $lline $n] "
            }
            if {![info exists printDestination]} {
                puts $out $line
            } else {
                $printDestination insert end $line\n
            }
	}
    }
    if {$outfile != "stdout"} {
        close $out
    }
}
