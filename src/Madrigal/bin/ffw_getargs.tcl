proc ffw_getargs {argc argv} {
    
    global infile outfile printParmsIn filterParmsIn filterMinsIn filterMaxsIn
    
    # Get number of parameters
    set nargs $argc
    if {$nargs < 1} {
	puts {Usage: ffw -file filename -p parameter ... -f parameter minval maxval ...}
	exit
    }
    
    set i 0
    set outfile stdout
    set printParms {}
    set filterParmsIn {}
    set filterMinsIn {}
    set filterMaxsIn {}
    
    while {$i < $nargs} {
        set option [lindex $argv $i]
	if {$option == "-file"} {
	    incr i
	    if {$i == $nargs} {return -2}
	    set infile [lindex $argv $i]
	    incr i

	} elseif {$option == "-out"} {
	    incr i
	    if {$i == $nargs} {return -3}
	    set outfile [lindex $argv $i]
	    incr i
	
	} elseif {$option == "-print" || $option == "-p"} {
	    incr i
	    if {$i == $nargs} {return -4}
            set arg [string toupper [lindex $argv $i]]
	    set printParmsIn [lappend printParmsIn $arg]
	    incr i
	
	} elseif {$option == "-filter" || $option == "-f"} {
	    incr i
	    if {$i == $nargs} {return -5}
            set arg [string toupper [lindex $argv $i]]
	    set filterParmsIn [lappend filterParmsIn $arg]
	    incr i
	    if {$i == $nargs} {return -5}
	    set filterMinsIn [lappend filterMinsIn [lindex $argv $i]]
	    incr i
	    if {$i == $nargs} {return -5}
	    set filterMaxsIn [lappend filterMaxsIn [lindex $argv $i]]
	    incr i
    
	} else {
	    return -1
	}
    }

    return 0

}
