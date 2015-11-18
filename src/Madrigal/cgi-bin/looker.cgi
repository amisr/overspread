#!/bin/sh
# The madtclsh path is longer than 32 characters. So, we take advantage
# of the fact that a backslash continues a comment line in tcl \
exec /Users/mnicolls/Documents/Work/Madrigal/bin/madtclsh "$0" ${1+"$@"}

# $Id: looker.cgi,v 1.11 2008/07/29 15:47:04 brideout Exp $

lappend auto_path /Users/mnicolls/Documents/Work/Madrigal/madtcllib
package require cgi
package require mtl

proc sendTopForm { htmlstyle } {

    global madroot option
    
    cgi_html {
    
	cgi_head {
	    cgi_title "Looker"
	}
    
	cgi_body $htmlstyle {
    
	    cgi_center {
		cgi_h1 "Looker"
	    }
        }

        cgi_form looker {

            cgi_hr
	    cgi_h2 "Compute azimuth, elevation and range to a specified
	        array of points. Select one of the following options for
	        specifying the points."
            cgi_radio_button option=1 checked
            cgi_put "Geodetic latitude, longitude and altitude of the points"
            cgi_br
            cgi_radio_button option=2
            cgi_put "Apex latitude, longitude and altitude of the points"
            cgi_hr
            cgi_br

	    cgi_h2 "Compute geographic and geomagnetic coordinates of a specified
	        array of points."
            cgi_h4 "Select one of the following options for specifying the points:"
            cgi_radio_button option=3
            cgi_put "Geodetic latitude, longitude and altitude of the points"
            cgi_br
            cgi_radio_button option=4
            cgi_put "Azimuth, elevation and range of the points from a specified instrument (output includes aspect angle)"
            cgi_br
            cgi_hr

	    cgi_h2 "Compute azimuth, elevation and range to points
	        along a geomagnetic field line. Select one of the following
	        three options for specifying the field line."
            cgi_radio_button option=6
	    cgi_put "Geodetic latitude, longitude, altitude of a point
	        on the field line"
            cgi_br
            cgi_radio_button option=5
            cgi_put "Azimuth, elevation, range from specified instrument"
            cgi_br
            cgi_radio_button option=7
            cgi_put "Apex latitude, longitude of the field line"
            cgi_br
            cgi_hr
            
            cgi_h2 "Compute point/magnetic conjugate parameters of a specified
	        array of points."
            cgi_radio_button option=8
            cgi_put "Geodetic latitude, longitude and altitude of the points"
            cgi_br
            cgi_hr

            cgi_center {
                cgi_submit_button "=Specify input parameters"
            }
        }

    }
}

proc sendSelectionForm { htmlstyle } {

  global madroot option

  cgi_html {

    cgi_head {
      cgi_title "Looker"
    }
  
    cgi_body $htmlstyle {
    
      if {$option < 8} {
  
        cgi_form looker {

	  if {$option == 1} {
	      sendForm1 $htmlstyle
  
	  } elseif {$option == 2} {
	      sendForm2 $htmlstyle
  
	  } elseif {$option == 3} {
	      sendForm3 $htmlstyle
  
	  } elseif {$option == 4} {
              sendForm4 $htmlstyle

          # Points on field line given az, el, range  
	  } elseif {$option == 5} {
              sendForm5 $htmlstyle

	  } elseif {$option == 6} {
	      sendForm6 $htmlstyle

          } elseif {$option == 7} {
	      sendForm7 $htmlstyle
	  }
	  
	  cgi_center {
	    cgi_submit_button "=List selected data"
          }
	}
	    
      } elseif {$option == 8} {
	  cgi_form madRecDisplay method=get {
	    sendForm8 $htmlstyle
	    cgi_center {
	      cgi_submit_button "=List selected data"
            }
	  }
  
      } else {
	cgi_p "Illegal option number - $option"
      }

    }
  }
}


proc sendForm1 { htmlstyle } {

  global madroot option mne instName instLat instLon instAlt

  set option 1

  cgi_center {
    cgi_h1 "Geographic and Geomagnetic Coordinates vs Azimuth, Elevation, Range"
  }

  cgi_puts "Select an instrument, either by name or by selecting
            \"Enter instrument coordinates\" and then entering the
            latitude, longitude and altitude of the instrument. Then
            specify the latitude/longitude/altitude grid for which 
            look direction and range  will be calculated. 
            The epoch for which the IGRF geomagnetic coordinates will be
            calculated may also be specified."
  cgi_br
  cgi_puts [cgi_italic "Note: The geomagnetic field computations sometimes
            fail, usually for points near the pole or equator. Usually this
            is obvious, but apply common sense in using the results
            returned by this program."]
  cgi_br
            
  cgi_text option=$option type=hidden

  # Top-level 2x2 Table
  cgi_table cellpadding=10 align=center {

    # First row of top-level table
    cgi_table_row {

      # Row 1 column 1
      cgi_table_data {
        # Instrument selection box table
	cgi_table cellpadding=5 border="border" {
	  cgi_caption {
	    cgi_puts "Instrument Name"
	  }
          cgi_table_row valign=center {
            cgi_table_data {
	      cgi_select station size=10 {
	        cgi_option "Enter Instrument Coordinates" value=0
	        for {set i 0} {$i < [llength $mne]} {set i [expr $i+2]} {
	          set code [lindex $mne $i]
	          if {$instLat($code) > -90.0 && \
		      $instLon($code) > -360.0 && \
		      $instAlt($code) < 100.0} {
                    if {$code == 30} {
	               cgi_option $instName($code) value=$code "selected"
                    } else {
	                cgi_option $instName($code) value=$code
                    }
	          }
	        }
	      }
            }
          }
        }
      }

      # Row 1 column 2
      cgi_table_data {
        # Output specification table
	cgi_table cellpadding=5 border="border" {
	  cgi_caption {
	    cgi_puts "Output Table Grid"
	  }
	  cgi_table_row valign=center {
	     cgi_td align=center {}
	     cgi_td align=center {start}
	     cgi_td align=center {end}
	     cgi_td align=center {delta}
	  }
	  cgi_table_row valign=center {
	    cgi_td align=right {latitude}
	    cgi_table_data align=left {
	      cgi_text p1=30.0 size=6 maxlength=10
	    }
	    cgi_table_data align=left {
	      cgi_text p2=50.0 size=6 maxlength=10
	    }
	    cgi_table_data align=left {
	      cgi_text p3=10.0 size=6 maxlength=10
	    }
	  }
	  cgi_table_row valign=center {
	     cgi_td align=right {longitude}
	     cgi_table_data align=left {
	      cgi_text p4=260.0 size=6 maxlength=10
	    }
	     cgi_table_data align=left {
	       cgi_text p5=280.0 size=6 maxlength=10
	    }
	     cgi_table_data align=left {
	      cgi_text p6=10.0 size=6 maxlength=10
	    }
	  }
	  cgi_table_row valign=center {
	     cgi_td align=right {altitude}
	     cgi_table_data align=left {
	      cgi_text p7=100.0 size=6 maxlength=10
	    }
	     cgi_table_data align=left {
	       cgi_text p8=500.0 size=6 maxlength=10
	    }
	     cgi_table_data align=left {
	      cgi_text p9=100.0 size=6 maxlength=10
	    }
	  }
	}
      }

    }

    # Second row of top-level table
    cgi_table_row {

      # Row 2 column 1
      cgi_table_data valign=top {
        # Instrument coordinate specification
	cgi_table cellpadding=5 border="border" {
	  cgi_caption {
	    cgi_puts "Instrument Coordinates"
	  }
	  cgi_table_row valign=center {
	     cgi_td align=center {latitude}
	     cgi_td align=center {longitude}
	     cgi_td align=center {altitude}
          }
	  cgi_table_row valign=center {
	    cgi_table_data align=left {
	      cgi_text s1=0.0 size=6 maxlength=10
            }
	    cgi_table_data align=left {
	      cgi_text s2=0.0 size=6 maxlength=10
            }
	    cgi_table_data align=left {
	      cgi_text s3=0.0 size=6 maxlength=10
            }
          }
        }
      }

      # Row 2 column 2
      cgi_table_data valign=top {
        # Epoch specification
        set time [clock seconds]
        set year [clock format $time -format "%Y" -gmt 1]
        set byear [clock scan "January 1, $year GMT"]
        set eyear [clock scan "December 31, $year GMT"]
        set ep [expr $year + 1.0*($time - $byear)/($eyear - $byear)]
        set epoch [format "%7.2f" $ep]
	cgi_table cellpadding=5 border="border" width=100% {
	  cgi_caption {
	    cgi_puts "Geomagnetic field epoch"
	  }
	  cgi_table_row valign=center {
	     cgi_td align=center {year}
          }
	  cgi_table_row valign=center {
	    cgi_table_data align=center {
	      cgi_text tm=$epoch size=7 maxlength=10
            }
          }
        }
      }

    }
  }
  # End of top-level table
}


proc sendForm2 { htmlstyle } {

  global madroot option mne instName instLat instLon instAlt

  set option 2

  cgi_center {
    cgi_h1 "Geographic and Geomagnetic Coordinates vs Azimuth, Elevation, Range"
  }

  cgi_puts "Select an instrument, either by name or by selecting
	    \"Enter instrument coordinates\" and then entering the
	    latitude, longitude and altitude of the instrument. Then
	    specify the apex latitude, apex longitude altitude grid for
	    which look direction and range  will be calculated.  The
	    epoch for which the IGRF geomagnetic coordinates will be
	    calculated may also be specified."
  cgi_br
  cgi_puts [cgi_italic "Note: The geomagnetic field computations sometimes
            fail, usually for points near the pole or equator. Usually this
            is obvious, but apply common sense in using the results
            returned by this program."]
  cgi_br
            
  cgi_text option=$option type=hidden

  # Top-level 2x2 Table
  cgi_table cellpadding=10 align=center {

    # First row of top-level table
    cgi_table_row {

      # Row 1 column 1
      cgi_table_data {
        # Instrument selection box table
	cgi_table cellpadding=5 border="border" {
	  cgi_caption {
	    cgi_puts "Instrument Name"
	  }
          cgi_table_row valign=center {
            cgi_table_data {
	      cgi_select station size=10 {
	        cgi_option "Enter Instrument Coordinates" value=0
	        for {set i 0} {$i < [llength $mne]} {set i [expr $i+2]} {
	          set code [lindex $mne $i]
	          if {$instLat($code) > -90.0 && \
		      $instLon($code) > -360.0 && \
		      $instAlt($code) < 100.0} {
                    if {$code == 30} {
	               cgi_option $instName($code) value=$code "selected"
                    } else {
	                cgi_option $instName($code) value=$code
                    }
	          }
	        }
	      }
            }
          }
        }
      }

      # Row 1 column 2
      cgi_table_data {
        # Output specification table
	cgi_table cellpadding=5 border="border" {
	  cgi_caption {
	    cgi_puts "Output Table Grid"
	  }
	  cgi_table_row valign=center {
	     cgi_td align=center {}
	     cgi_td align=center {start}
	     cgi_td align=center {end}
	     cgi_td align=center {delta}
	  }
	  cgi_table_row valign=center {
	    cgi_td align=right {apex latitude}
	    cgi_table_data align=left {
	      cgi_text p1=30.0 size=6 maxlength=10
	    }
	    cgi_table_data align=left {
	      cgi_text p2=50.0 size=6 maxlength=10
	    }
	    cgi_table_data align=left {
	      cgi_text p3=10.0 size=6 maxlength=10
	    }
	  }
	  cgi_table_row valign=center {
	     cgi_td align=right {apex longitude}
	     cgi_table_data align=left {
	      cgi_text p4=260.0 size=6 maxlength=10
	    }
	     cgi_table_data align=left {
	       cgi_text p5=280.0 size=6 maxlength=10
	    }
	     cgi_table_data align=left {
	      cgi_text p6=10.0 size=6 maxlength=10
	    }
	  }
	  cgi_table_row valign=center {
	     cgi_td align=right {altitude}
	     cgi_table_data align=left {
	      cgi_text p7=100.0 size=6 maxlength=10
	    }
	     cgi_table_data align=left {
	       cgi_text p8=500.0 size=6 maxlength=10
	    }
	     cgi_table_data align=left {
	      cgi_text p9=100.0 size=6 maxlength=10
	    }
	  }
	}
      }

    }

    # Second row of top-level table
    cgi_table_row {

      # Row 2 column 1
      cgi_table_data valign=top {
        # Instrument coordinate specification
	cgi_table cellpadding=5 border="border" {
	  cgi_caption {
	    cgi_puts "Instrument Coordinates"
	  }
	  cgi_table_row valign=center {
	     cgi_td align=center {latitude}
	     cgi_td align=center {longitude}
	     cgi_td align=center {altitude}
          }
	  cgi_table_row valign=center {
	    cgi_table_data align=left {
	      cgi_text s1=0.0 size=6 maxlength=10
            }
	    cgi_table_data align=left {
	      cgi_text s2=0.0 size=6 maxlength=10
            }
	    cgi_table_data align=left {
	      cgi_text s3=0.0 size=6 maxlength=10
            }
          }
        }
      }

      # Row 2 column 2
      cgi_table_data valign=top {
        # Epoch specification
        set time [clock seconds]
        set year [clock format $time -format "%Y" -gmt 1]
        set byear [clock scan "January 1, $year GMT"]
        set eyear [clock scan "December 31, $year GMT"]
        set ep [expr $year + 1.0*($time - $byear)/($eyear - $byear)]
        set epoch [format "%7.2f" $ep]
	cgi_table cellpadding=5 border="border" width=100% {
	  cgi_caption {
	    cgi_puts "Geomagnetic field epoch"
	  }
	  cgi_table_row valign=center {
	     cgi_td align=center {year}
          }
	  cgi_table_row valign=center {
	    cgi_table_data align=center {
	      cgi_text tm=$epoch size=7 maxlength=10
            }
          }
        }
      }

    }
  }
  # End of top-level table
}


proc sendForm3 { htmlstyle } {

  # Examples:
  # looker 3 2002.20 0 0 0 -90.0 90.0 45.0 0.0 360.0 90.0 0.0 600.0 200.0
  # looker 3 2002.20 0 0 0 40.0 50.0 10.0 260.0 280.0 10.0 0.0 600.0 200.0

  global madroot option mne instName instLat instLon instAlt

  set option 3

  cgi_center {
    cgi_h1 "Geomagnetic Coordinates vs Latitude, Longitude, Altitude"
  }

  cgi_puts "Specify the latitude/longitude/altitude grid on which 
            geomagnetic coordinates will be calculated. 
            The epoch for which the IGRF geomagnetic coordinates will be
            calculated may also be specified."
  cgi_br
  cgi_puts [cgi_italic "Note: The geomagnetic field computations sometimes
            fail, usually for points near the pole or equator. Usually this
            is obvious, but apply common sense in using the results
            returned by this program."]
  cgi_br
            
  cgi_text option=$option type=hidden
  cgi_text station=0 type=hidden
  cgi_text s1=0 type=hidden
  cgi_text s2=0 type=hidden
  cgi_text s3=0 type=hidden

  cgi_table cellpadding=10 align=center {

cgi_table_row {
      cgi_table_data {
        # Output specification table
	cgi_table cellpadding=5 border="border" {
	  cgi_caption {
	    cgi_puts "Output Table Grid"
	  }
	  cgi_table_row valign=center {
	     cgi_td align=center {}
	     cgi_td align=center {start}
	     cgi_td align=center {end}
	     cgi_td align=center {delta}
	  }
	  cgi_table_row valign=center {
	    cgi_td align=right {latitude}
	    cgi_table_data align=left {
	      cgi_text p1=-90.0 size=6 maxlength=10
	    }
	    cgi_table_data align=left {
	      cgi_text p2=90.0 size=6 maxlength=10
	    }
	    cgi_table_data align=left {
	      cgi_text p3=45.0 size=6 maxlength=10
	    }
	  }
	  cgi_table_row valign=center {
	     cgi_td align=right {longitude}
	     cgi_table_data align=left {
	      cgi_text p4=0.0 size=6 maxlength=10
	    }
	     cgi_table_data align=left {
	       cgi_text p5=360.0 size=6 maxlength=10
	    }
	     cgi_table_data align=left {
	      cgi_text p6=90.0 size=6 maxlength=10
	    }
	  }
	  cgi_table_row valign=center {
	     cgi_td align=right {altitude}
	     cgi_table_data align=left {
	      cgi_text p7=0.0 size=6 maxlength=10
	    }
	     cgi_table_data align=left {
	       cgi_text p8=600.0 size=6 maxlength=10
	    }
	     cgi_table_data align=left {
	      cgi_text p9=200.0 size=6 maxlength=10
	    }
	  }
	}
      }
}

cgi_table_row {
      cgi_table_data valign=top {
        # Epoch specification
        set time [clock seconds]
        set year [clock format $time -format "%Y" -gmt 1]
        set byear [clock scan "January 1, $year GMT"]
        set eyear [clock scan "December 31, $year GMT"]
        set ep [expr $year + 1.0*($time - $byear)/($eyear - $byear)]
        set epoch [format "%7.2f" $ep]
	cgi_table cellpadding=5 border="border" width=100% {
	  cgi_caption {
	    cgi_puts "Geomagnetic field epoch"
	  }
	  cgi_table_row valign=center {
	     cgi_td align=center {year}
          }
	  cgi_table_row valign=center {
	    cgi_table_data align=center {
	      cgi_text tm=$epoch size=7 maxlength=10
            }
          }
        }
      }

    }
  }
}


proc sendForm4 { htmlstyle } {

  global madroot option mne instName instLat instLon instAlt

  set option 4

  cgi_center {
    cgi_h1 "Geographic and Geomagnetic Coordinates vs Azimuth, Elevation, Range"
  }

  cgi_puts "Select an instrument, either by name or by selecting
            \"Enter instrument coordinates\" and then entering the
            latitude, longitude and altitude of the instrument. Then
            specify the azimuth/elevation/range grid on which 
            geodetic and geomagnetic coordinates will be calculated. 
            The epoch for which the IGRF geomagnetic coordinates will be
            calculated may also be specified."
  cgi_br
  cgi_puts [cgi_italic "Note: The geomagnetic field computations sometimes
            fail, usually for points near the pole or equator. Usually this
            is obvious, but apply common sense in using the results
            returned by this program."]
  cgi_br
            
  cgi_text option=$option type=hidden

  # Top-level 2x2 Table
  cgi_table cellpadding=10 align=center {

    # First row of top-level table
    cgi_table_row {

      # Row 1 column 1
      cgi_table_data {
        # Instrument selection box table
	cgi_table cellpadding=5 border="border" {
	  cgi_caption {
	    cgi_puts "Instrument Name"
	  }
          cgi_table_row valign=center {
            cgi_table_data {
	      cgi_select station size=10 {
	        cgi_option "Enter Instrument Coordinates" value=0
	        for {set i 0} {$i < [llength $mne]} {set i [expr $i+2]} {
	          set code [lindex $mne $i]
	          if {$instLat($code) > -90.0 && \
		      $instLon($code) > -360.0 && \
		      $instAlt($code) < 100.0} {
                    if {$code == 30} {
	               cgi_option $instName($code) value=$code "selected"
                    } else {
	                cgi_option $instName($code) value=$code
                    }
	          }
	        }
	      }
            }
          }
        }
      }

      # Row 1 column 2
      cgi_table_data {
        # Output specification table
	cgi_table cellpadding=5 border="border" {
	  cgi_caption {
	    cgi_puts "Output Table Grid"
	  }
	  cgi_table_row valign=center {
	     cgi_td align=center {}
	     cgi_td align=center {start}
	     cgi_td align=center {end}
	     cgi_td align=center {delta}
	  }
	  cgi_table_row valign=center {
	    cgi_td align=right {azimuth}
	    cgi_table_data align=left {
	      cgi_text p1=0.0 size=6 maxlength=10
	    }
	    cgi_table_data align=left {
	      cgi_text p2=360.0 size=6 maxlength=10
	    }
	    cgi_table_data align=left {
	      cgi_text p3=45.0 size=6 maxlength=10
	    }
	  }
	  cgi_table_row valign=center {
	     cgi_td align=right {elevation}
	     cgi_table_data align=left {
	      cgi_text p4=0.0 size=6 maxlength=10
	    }
	     cgi_table_data align=left {
	       cgi_text p5=90.0 size=6 maxlength=10
	    }
	     cgi_table_data align=left {
	      cgi_text p6=30.0 size=6 maxlength=10
	    }
	  }
	  cgi_table_row valign=center {
	     cgi_td align=right {range}
	     cgi_table_data align=left {
	      cgi_text p7=0.0 size=6 maxlength=10
	    }
	     cgi_table_data align=left {
	       cgi_text p8=600.0 size=6 maxlength=10
	    }
	     cgi_table_data align=left {
	      cgi_text p9=200.0 size=6 maxlength=10
	    }
	  }
	}
      }

    }

    # Second row of top-level table
    cgi_table_row {

      # Row 2 column 1
      cgi_table_data valign=top {
        # Instrument coordinate specification
	cgi_table cellpadding=5 border="border" {
	  cgi_caption {
	    cgi_puts "Instrument Coordinates"
	  }
	  cgi_table_row valign=center {
	     cgi_td align=center {latitude}
	     cgi_td align=center {longitude}
	     cgi_td align=center {altitude}
          }
	  cgi_table_row valign=center {
	    cgi_table_data align=left {
	      cgi_text s1=0.0 size=6 maxlength=10
            }
	    cgi_table_data align=left {
	      cgi_text s2=0.0 size=6 maxlength=10
            }
	    cgi_table_data align=left {
	      cgi_text s3=0.0 size=6 maxlength=10
            }
          }
        }
      }

      # Row 2 column 2
      cgi_table_data valign=top {
        # Epoch specification
        set time [clock seconds]
        set year [clock format $time -format "%Y" -gmt 1]
        set byear [clock scan "January 1, $year GMT"]
        set eyear [clock scan "December 31, $year GMT"]
        set ep [expr $year + 1.0*($time - $byear)/($eyear - $byear)]
        set epoch [format "%7.2f" $ep]
	cgi_table cellpadding=5 border="border" width=100% {
	  cgi_caption {
	    cgi_puts "Geomagnetic field epoch"
	  }
	  cgi_table_row valign=center {
	     cgi_td align=center {year}
          }
	  cgi_table_row valign=center {
	    cgi_table_data align=center {
	      cgi_text tm=$epoch size=7 maxlength=10
            }
          }
        }
      }

    }
  }
  # End of top-level table
}


proc sendForm5 { htmlstyle } {

  global madroot option mne instName instLat instLon instAlt

  set option 5

  cgi_center {
    cgi_h1 "Field Line Coordinates Given Azimuth, Elevation, Range"
  }

  cgi_puts "Select an instrument, either by name or by selecting
            \"Enter instrument coordinates\" and then entering the
	    latitude, longitude and altitude of the instrument. Then
	    specify an azimuth, elevation and range to a field line on
	    which geodetic and geomagnetic coordinates will be
	    calculated.  The epoch for which the IGRF geomagnetic
	    coordinates will be calculated may also be specified."
  cgi_br
  cgi_puts [cgi_italic "Note: The geomagnetic field computations sometimes
	    fail, usually for points near the pole or equator. Usually
	    this is obvious, but apply common sense in using the
	    results returned by this program."]
  cgi_br
            
  cgi_text option=$option type=hidden
  cgi_text p7=0 type=hidden
  cgi_text p8=0 type=hidden
  cgi_text p9=0 type=hidden

  # Top-level 2x2 Table
  cgi_table cellpadding=10 align=center {

    # First row of top-level table
    cgi_table_row {

      # Row 1 column 1
      cgi_table_data {
        # Instrument selection box table
	cgi_table cellpadding=5 border="border" {
	  cgi_caption {
	    cgi_puts "Instrument Name"
	  }
          cgi_table_row valign=center {
            cgi_table_data {
	      cgi_select station size=10 {
	        cgi_option "Enter Instrument Coordinates" value=0
	        for {set i 0} {$i < [llength $mne]} {set i [expr $i+2]} {
	          set code [lindex $mne $i]
	          if {$instLat($code) > -90.0 && \
		      $instLon($code) > -360.0 && \
		      $instAlt($code) < 100.0} {
                    if {$code == 30} {
	               cgi_option $instName($code) value=$code "selected"
                    } else {
	                cgi_option $instName($code) value=$code
                    }
	          }
	        }
	      }
            }
          }
        }
      }

      # Row 1 column 2
      cgi_table_data {
        cgi_table {
          cgi_table_row {
            cgi_table_data {
              # Instrument coordinate specification
	      cgi_table cellpadding=5 border="border" {
	        cgi_caption {
	          cgi_puts "Field Line Coordinates"
	        }
	        cgi_table_row valign=center {
	           cgi_td align=center {azimuth}
	           cgi_td align=center {elevation}
	           cgi_td align=center {range}
                }
	        cgi_table_row valign=center {
	          cgi_table_data align=left {
	            cgi_text p1=0.0 size=6 maxlength=10
                  }
	          cgi_table_data align=left {      
	            cgi_text p2=45.0 size=6 maxlength=10
                  }
	          cgi_table_data align=left {
	            cgi_text p3=1000.0 size=6 maxlength=10
                  }
                }
              }
            }
          }
          cgi_table_row {
            cgi_table_data {
              # Instrument coordinate specification
	      cgi_table cellpadding=5 border="border" {
	        cgi_caption {
	          cgi_puts "Altitudes Along Field Line"
	        }
	        cgi_table_row valign=center {
	           cgi_td align=center {start}
	           cgi_td align=center {end}
	           cgi_td align=center {delta}
                }
	        cgi_table_row valign=center {
	          cgi_table_data align=left {
	            cgi_text p4=100.0 size=6 maxlength=10
                  }
	          cgi_table_data align=left {
	            cgi_text p5=1000.0 size=6 maxlength=10
                  }
	          cgi_table_data align=left {
	            cgi_text p6=100.0 size=6 maxlength=10
                  }
                }
              }
            }
          }
        }
      }



    }

    # Second row of top-level table
    cgi_table_row {

      # Row 2 column 1
      cgi_table_data valign=top {
        # Instrument coordinate specification
	cgi_table cellpadding=5 border="border" {
	  cgi_caption {
	    cgi_puts "Instrument Coordinates"
	  }
	  cgi_table_row valign=center {
	     cgi_td align=center {latitude}
	     cgi_td align=center {longitude}
	     cgi_td align=center {altitude}
          }
	  cgi_table_row valign=center {
	    cgi_table_data align=left {
	      cgi_text s1=0.0 size=6 maxlength=10
            }
	    cgi_table_data align=left {
	      cgi_text s2=0.0 size=6 maxlength=10
            }
	    cgi_table_data align=left {
	      cgi_text s3=0.0 size=6 maxlength=10
            }
          }
        }
      }

      # Row 2 column 2
      cgi_table_data valign=top {
        # Epoch specification
        set time [clock seconds]
        set year [clock format $time -format "%Y" -gmt 1]
        set byear [clock scan "January 1, $year GMT"]
        set eyear [clock scan "December 31, $year GMT"]
        set ep [expr $year + 1.0*($time - $byear)/($eyear - $byear)]
        set epoch [format "%7.2f" $ep]
	cgi_table cellpadding=5 border="border" width=100% {
	  cgi_caption {
	    cgi_puts "Geomagnetic field epoch"
	  }
	  cgi_table_row valign=center {
	     cgi_td align=center {year}
          }
	  cgi_table_row valign=center {
	    cgi_table_data align=center {
	      cgi_text tm=$epoch size=7 maxlength=10
            }
          }
        }
      }

    }
  }
  # End of top-level table
}


proc sendForm6 { htmlstyle } {

  global madroot option mne instName instLat instLon instAlt

  set option 6

  cgi_center {
    cgi_h1 "Field Line Coordinates Given Latitude, Longitude, and Altitude"
  }

  cgi_puts "Select an instrument, either by name or by selecting
            \"Enter instrument coordinates\" and then entering the
	    latitude, longitude and altitude of the instrument. Then
	    specify the latitude, longitude and altitude of a point on
	    a field line on which geodetic and geomagnetic coordinates
	    will be calculated.  The epoch for which the IGRF
	    geomagnetic coordinates will be calculated may also be
	    specified."
  cgi_br
  cgi_puts [cgi_italic "Note: The geomagnetic field computations sometimes
	    fail, usually for points near the pole or equator. Usually
	    this is obvious, but apply common sense in using the
	    results returned by this program."]
  cgi_br
            
  cgi_text option=$option type=hidden
  cgi_text p7=0 type=hidden
  cgi_text p8=0 type=hidden
  cgi_text p9=0 type=hidden

  # Top-level 2x2 Table
  cgi_table cellpadding=10 align=center {

    # First row of top-level table
    cgi_table_row {

      # Row 1 column 1
      cgi_table_data {
        # Instrument selection box table
	cgi_table cellpadding=5 border="border" {
	  cgi_caption {
	    cgi_puts "Instrument Name"
	  }
          cgi_table_row valign=center {
            cgi_table_data {
	      cgi_select station size=10 {
	        cgi_option "Enter Instrument Coordinates" value=0
	        for {set i 0} {$i < [llength $mne]} {set i [expr $i+2]} {
	          set code [lindex $mne $i]
	          if {$instLat($code) > -90.0 && \
		      $instLon($code) > -360.0 && \
		      $instAlt($code) < 100.0} {
                    if {$code == 30} {
	               cgi_option $instName($code) value=$code "selected"
                    } else {
	                cgi_option $instName($code) value=$code
                    }
	          }
	        }
	      }
            }
          }
        }
      }

      # Row 1 column 2
      cgi_table_data {
        cgi_table {
          cgi_table_row {
            cgi_table_data {
              # Instrument coordinate specification
	      cgi_table cellpadding=5 border="border" {
	        cgi_caption {
	          cgi_puts "Field Line Coordinates"
	        }
	        cgi_table_row valign=center {
	           cgi_td align=center {latitude}
	           cgi_td align=center {longitude}
	           cgi_td align=center {altitude}
                }
	        cgi_table_row valign=center {
	          cgi_table_data align=left {
	            cgi_text p1=45.0 size=6 maxlength=10
                  }
	          cgi_table_data align=left {      
	            cgi_text p2=270.0 size=6 maxlength=10
                  }
	          cgi_table_data align=left {
	            cgi_text p3=0.0 size=6 maxlength=10
                  }
                }
              }
            }
          }
          cgi_table_row {
            cgi_table_data {
              # Instrument coordinate specification
	      cgi_table cellpadding=5 border="border" {
	        cgi_caption {
	          cgi_puts "Altitudes Along Field Line"
	        }
	        cgi_table_row valign=center {
	           cgi_td align=center {start}
	           cgi_td align=center {end}
	           cgi_td align=center {delta}
                }
	        cgi_table_row valign=center {
	          cgi_table_data align=left {
	            cgi_text p4=100.0 size=6 maxlength=10
                  }
	          cgi_table_data align=left {
	            cgi_text p5=1000.0 size=6 maxlength=10
                  }
	          cgi_table_data align=left {
	            cgi_text p6=100.0 size=6 maxlength=10
                  }
                }
              }
            }
          }
        }
      }



    }

    # Second row of top-level table
    cgi_table_row {

      # Row 2 column 1
      cgi_table_data valign=top {
        # Instrument coordinate specification
	cgi_table cellpadding=5 border="border" {
	  cgi_caption {
	    cgi_puts "Instrument Coordinates"
	  }
	  cgi_table_row valign=center {
	     cgi_td align=center {latitude}
	     cgi_td align=center {longitude}
	     cgi_td align=center {altitude}
          }
	  cgi_table_row valign=center {
	    cgi_table_data align=left {
	      cgi_text s1=0.0 size=6 maxlength=10
            }
	    cgi_table_data align=left {
	      cgi_text s2=0.0 size=6 maxlength=10
            }
	    cgi_table_data align=left {
	      cgi_text s3=0.0 size=6 maxlength=10
            }
          }
        }
      }

      # Row 2 column 2
      cgi_table_data valign=top {
        # Epoch specification
        set time [clock seconds]
        set year [clock format $time -format "%Y" -gmt 1]
        set byear [clock scan "January 1, $year GMT"]
        set eyear [clock scan "December 31, $year GMT"]
        set ep [expr $year + 1.0*($time - $byear)/($eyear - $byear)]
        set epoch [format "%7.2f" $ep]
	cgi_table cellpadding=5 border="border" width=100% {
	  cgi_caption {
	    cgi_puts "Geomagnetic field epoch"
	  }
	  cgi_table_row valign=center {
	     cgi_td align=center {year}
          }
	  cgi_table_row valign=center {
	    cgi_table_data align=center {
	      cgi_text tm=$epoch size=7 maxlength=10
            }
          }
        }
      }

    }
  }
  # End of top-level table
}


proc sendForm7 { htmlstyle } {

  global madroot option mne instName instLat instLon instAlt

  set option 7

  cgi_center {
    cgi_h1 "Field Line Coordinates Given Azimuth, Elevation, Range"
  }

  cgi_puts "Select an instrument, either by name or by selecting
	    \"Enter instrument coordinates\" and then entering the
	    latitude, longitude and altitude of the instrument. Then
	    specify an the apex latitude and longitude of a field line
	    on which geodetic and geomagnetic coordinates will be
	    calculated.  The epoch for which the IGRF geomagnetic
	    coordinates will be calculated may also be specified."
  cgi_br
  cgi_puts [cgi_italic "Note: The geomagnetic field computations sometimes
	    fail, usually for points near the pole or equator. Usually
	    this is obvious, but apply common sense in using the
	    results returned by this program."]
  cgi_br
            
  cgi_text option=$option type=hidden
  cgi_text p3=0 type=hidden
  cgi_text p7=0 type=hidden
  cgi_text p8=0 type=hidden
  cgi_text p9=0 type=hidden

  # Top-level 2x2 Table
  cgi_table cellpadding=10 align=center {

    # First row of top-level table
    cgi_table_row {

      # Row 1 column 1
      cgi_table_data {
        # Instrument selection box table
	cgi_table cellpadding=5 border="border" {
	  cgi_caption {
	    cgi_puts "Instrument Name"
	  }
          cgi_table_row valign=center {
            cgi_table_data {
	      cgi_select station size=10 {
	        cgi_option "Enter Instrument Coordinates" value=0
	        for {set i 0} {$i < [llength $mne]} {set i [expr $i+2]} {
	          set code [lindex $mne $i]
	          if {$instLat($code) > -90.0 && \
		      $instLon($code) > -360.0 && \
		      $instAlt($code) < 100.0} {
                    if {$code == 30} {
	               cgi_option $instName($code) value=$code "selected"
                    } else {
	                cgi_option $instName($code) value=$code
                    }
	          }
	        }
	      }
            }
          }
        }
      }

      # Row 1 column 2
      cgi_table_data {
        cgi_table {
          cgi_table_row {
            cgi_table_data {
              # Instrument coordinate specification
	      cgi_table cellpadding=5 border="border" {
	        cgi_caption {
	          cgi_puts "Field Line Coordinates"
	        }
	        cgi_table_row valign=center {
	           cgi_td align=center {apex latitude}
	           cgi_td align=center {apex longitude}
                }
	        cgi_table_row valign=center {
	          cgi_table_data align=left {
	            cgi_text p1=65.0 size=6 maxlength=10
                  }
	          cgi_table_data align=left {      
	            cgi_text p2=270.0 size=6 maxlength=10
                  }
                }
              }
            }
          }
          cgi_table_row {
            cgi_table_data {
              # Instrument coordinate specification
	      cgi_table cellpadding=5 border="border" {
	        cgi_caption {
	          cgi_puts "Altitudes Along Field Line"
	        }
	        cgi_table_row valign=center {
	           cgi_td align=center {start}
	           cgi_td align=center {end}
	           cgi_td align=center {delta}
                }
	        cgi_table_row valign=center {
	          cgi_table_data align=left {
	            cgi_text p4=100.0 size=6 maxlength=10
                  }
	          cgi_table_data align=left {
	            cgi_text p5=1000.0 size=6 maxlength=10
                  }
	          cgi_table_data align=left {
	            cgi_text p6=100.0 size=6 maxlength=10
                  }
                }
              }
            }
          }
        }
      }



    }

    # Second row of top-level table
    cgi_table_row {

      # Row 2 column 1
      cgi_table_data valign=top {
        # Instrument coordinate specification
	cgi_table cellpadding=5 border="border" {
	  cgi_caption {
	    cgi_puts "Instrument Coordinates"
	  }
	  cgi_table_row valign=center {
	     cgi_td align=center {latitude}
	     cgi_td align=center {longitude}
	     cgi_td align=center {altitude}
          }
	  cgi_table_row valign=center {
	    cgi_table_data align=left {
	      cgi_text s1=0.0 size=6 maxlength=10
            }
	    cgi_table_data align=left {
	      cgi_text s2=0.0 size=6 maxlength=10
            }
	    cgi_table_data align=left {
	      cgi_text s3=0.0 size=6 maxlength=10
            }
          }
        }
      }

      # Row 2 column 2
      cgi_table_data valign=top {
        # Epoch specification
        set time [clock seconds]
        set year [clock format $time -format "%Y" -gmt 1]
        set byear [clock scan "January 1, $year GMT"]
        set eyear [clock scan "December 31, $year GMT"]
        set ep [expr $year + 1.0*($time - $byear)/($eyear - $byear)]
        set epoch [format "%7.2f" $ep]
	cgi_table cellpadding=5 border="border" width=100% {
	  cgi_caption {
	    cgi_puts "Geomagnetic field epoch"
	  }
	  cgi_table_row valign=center {
	     cgi_td align=center {year}
          }
	  cgi_table_row valign=center {
	    cgi_table_data align=center {
	      cgi_text tm=$epoch size=7 maxlength=10
            }
          }
        }
      }

    }
  }
  # End of top-level table
}


proc sendForm8 { htmlstyle } {



  global madroot option mne instName instLat instLon instAlt

  set option 3

  cgi_center {
    cgi_h1 "Point/Magnetic Conjugate Point vs Latitude, Longitude, Altitude"
  }

  cgi_puts "Specify the latitude/longitude/altitude grid on which 
            point/magnetic conjugate point parameters will be calculated. 
            This program calculates the following for both the specified
            point and its magnetic conjugate: gdlat, glon, solar zenith
            angle and shadow height."
  cgi_br
  cgi_puts "This calculation is done for a single time (UT)."
  cgi_br
            

  cgi_table cellpadding=10 align=center {

cgi_table_row {
      cgi_table_data {
        # Output specification table
	cgi_table cellpadding=5 border="border" {
	  cgi_caption {
	    cgi_puts "Output Table Grid"
	  }
	  cgi_table_row valign=center {
	     cgi_td align=center {}
	     cgi_td align=center {start}
	     cgi_td align=center {end}
	     cgi_td align=center {delta}
	  }
	  cgi_table_row valign=center {
	    cgi_td align=right {latitude}
	    cgi_table_data align=left {
	      cgi_text start_lat=-90.0 size=6 maxlength=10
	    }
	    cgi_table_data align=left {
	      cgi_text stop_lat=90.0 size=6 maxlength=10
	    }
	    cgi_table_data align=left {
	      cgi_text step_lat=45.0 size=6 maxlength=10
	    }
	  }
	  cgi_table_row valign=center {
	     cgi_td align=right {longitude}
	     cgi_table_data align=left {
	      cgi_text start_lon=-180.0 size=6 maxlength=10
	    }
	     cgi_table_data align=left {
	       cgi_text stop_lon=180.0 size=6 maxlength=10
	    }
	     cgi_table_data align=left {
	      cgi_text step_lon=90.0 size=6 maxlength=10
	    }
	  }
	  cgi_table_row valign=center {
	     cgi_td align=right {altitude}
	     cgi_table_data align=left {
	      cgi_text start_alt=0.0 size=6 maxlength=10
	    }
	     cgi_table_data align=left {
	       cgi_text stop_alt=600.0 size=6 maxlength=10
	    }
	     cgi_table_data align=left {
	      cgi_text step_alt=200.0 size=6 maxlength=10
	    }
	  }
	}
      }
}

cgi_table_row {
      cgi_table_data valign=top {
        # time specification
	cgi_table cellpadding=5 border="border" width=100% {
	  cgi_caption {
	    cgi_puts "Time (UT)"
	  }
	  cgi_table_row valign=center {
	     cgi_td align=center {year}
	     cgi_td align=center {month}
	     cgi_td align=center {day}
	     cgi_td align=center {hour}
	     cgi_td align=center {min}
	     cgi_td align=center {sec}
          }
	  cgi_table_row valign=center {
	    cgi_table_data align=center {
	      cgi_text year=2000 size=4 maxlength=4
	    }
	    cgi_table_data align=left {
	      cgi_text month=1 size=4 maxlength=4
	    }
	    cgi_table_data align=left {
	      cgi_text day=1 size=4 maxlength=4
	    }
	    cgi_table_data align=left {
	      cgi_text hour=0 size=4 maxlength=4
	    }
	    cgi_table_data align=left {
	      cgi_text min=0 size=4 maxlength=4
	    }
	    cgi_table_data align=left {
	      cgi_text sec=0 size=4 maxlength=4
	    }
          }
        }
      }
    }
    
cgi_table_row {
      cgi_table_data valign=top {
        # parameter selection
        cgi_table cellpadding=5 border="border" width=100% {
          cgi_caption {
            cgi_puts "Select parameters"
          }
          cgi_table_row valign=center {
            cgi_table_data align=left {
              cgi_checkbox pList=MAGCONJLAT checked
              cgi_put "Magnetic conjugate latitude"
            }
            cgi_table_data align=left {
              cgi_checkbox pList=MAGCONJLON checked
              cgi_put "Magnetic conjugate longitude"
            }
          }
          cgi_table_row valign=center {
            cgi_table_data align=left {
	      cgi_checkbox pList=SZEN checked
              cgi_put "Solar zenith angle"
            }
            cgi_table_data align=left {
              cgi_checkbox pList=SZENC checked
              cgi_put "Magnetic conjugate solar zenith angle"  
            }
          }
          cgi_table_row valign=center {
            cgi_table_data align=left {
	      cgi_checkbox pList=SDWHT checked
              cgi_put "Shadow height (km)"
            }
            cgi_table_data align=left {
              cgi_checkbox pList=MAGCONJSDWHT checked
              cgi_put "Magnetic conjugate shadow height (km)"  
            }
          }
        }
      }
    }
    

  }
}
  
  
proc sendResults { htmlstyle option tm slatgd slon saltgd \
                   p1 p2 p3 p4 p5 p6 p7 p8 p9} {

    global madroot

    cgi_html {
          cgi_head {
	    cgi_title "Looker"
        }
          cgi_body $htmlstyle {
    
	    cgi_center {
	        cgi_h1 "Looker Results"
	    }
            cgi_preformatted {

               #puts "looker1 $option $tm $slatgd $slon $saltgd $p1 $p2 $p3 $p4 $p5 $p6 $p7 $p8 $p9"

                catch {exec /Users/mnicolls/Documents/Work/Madrigal/bin/looker1 $option $tm $slatgd $slon $saltgd $p1 $p2 $p3 $p4 $p5 $p6 $p7 $p8 $p9} result


              cgi_puts $result 
            }
        }
    }
}
      
########################
# Start Execution Here #
########################

set looker /Users/mnicolls/Documents/Work/Madrigal/bin/looker1

# Uncomment following statement to see errors in Web page
#cgi_debug -on

madrigal madrigal
set madroot [madrigal cget -madroot]
set htmlstyle [madrigal cget -htmlstyle]
set htmlstyle [string trimleft [set htmlstyle] "<"]
set htmlstyle [string trimright [set htmlstyle] ">"]
regsub -nocase BODY $htmlstyle "" htmlstyle

  madInstrument instruments
  instruments read
  instruments getEntries mnemonic instName instLat instLon instAlt \
			 contactName contactAddress1 \
			 contactAddress2 contactAddress3 \
			 contactCity contactState contactPostalCode \
			 contactCountry contactTelephone contactEmail
  set mne [instruments cget -mnemonic]

cgi_eval {

    cgi_input
    #cgi_input "option=3&station=0&s1=0&s2=0&s3=0&p1=40.0&p2=50.0&p3=10.0&p4=260.0&p5=280.0&p6=10.0&p7=0.0&p8=600.0&p9=200.0&tm=2002.20"

    # First call. Display form.
    if {[llength [cgi_import_list]] == 0} {
	sendTopForm $htmlstyle
    
    # Have the option arguments. Display the appropriate input
    # specification form.
    } elseif {[llength [cgi_import_list]] == 1} {
        catch {cgi_import option}
        sendSelectionForm $htmlstyle

    } elseif {[llength [cgi_import_list]] == 15} {
        catch {cgi_import option}
        catch {cgi_import tm}
        catch {cgi_import station}
        catch {cgi_import p1}
        catch {cgi_import p2}
        catch {cgi_import p3}
        catch {cgi_import p4}
        catch {cgi_import p5}
        catch {cgi_import p6}
        catch {cgi_import p7}
        catch {cgi_import p8}
        catch {cgi_import p9}
        catch {cgi_import s1}
        catch {cgi_import s2}
        catch {cgi_import s3}


        if {$station == 0} {
            set slatgd $s1
            set slon $s2
            set saltgd $s3
        } else {
            set slatgd $instLat($station)
            set slon $instLon($station)
            set saltgd $instAlt($station)
        }

        sendResults $htmlstyle $option $tm $slatgd $slon $saltgd \
                    $p1 $p2 $p3 $p4 $p5 $p6 $p7 $p8 $p9
    
    } else {
	cgi_http_head
	cgi_html {
	    cgi_body $htmlstyle {
		cgi_p "Error: Must specify either 0 or 15 parameters"
	    }
	}
    }

}

exit
