global peak_flags

bind JOIN * * newjoin
bind pub $peak_flags !peak saypeak

set peakfile "autoscripts/peakusers/peakfile.db"
setudef flag peakusers


proc newjoin {nick user hand chan} {
  if {![channel get $chan peakusers]} {
    return
  }

  global peakchannel peakfile
  set notfound 1

  if {[channel get $chan peakusers]} {
    set numusers [llength [chanlist $chan]]
    if {![info exists peakchannel($chan)]} {
      set peakchannel($chan) 0
    }
    if {($numusers > $peakchannel($chan)) && ($numusers > 1)} {
      putserv "PRIVMSG $chan :New channel peak reached! ($numusers users)"
      set peakchannel($chan) $numusers
      set fs [open $peakfile r+]
      set gs [open peakfile.tmp w]
      while {![eof $fs]} {
        gets $fs line
        if {[string equal -nocase [lindex $line 0] $chan]} {
          puts $gs "[list $chan $numusers]"
          set notfound 0
        } elseif {([string length $line] > 0)} {
          puts $gs $line
        }
        if {$notfound} {
          puts $gs "$chan $numusers"
        }
      }
      close $fs
      file rename -force peakfile.tmp $peakfile
      close $gs
    }
  }
}

proc saypeak {nick user hand chan arg} {
  if {![channel get $chan peakusers]} {
    return
  }

  global peakchannel

  if {![info exists peakchannel($chan)]} {
    putlog "peakusers: No channel record exists; a user must join to start the script"
    return
  }
  putserv "PRIVMSG $chan :Peak users seen on channel: $peakchannel($chan)"
}

set fs [open $peakfile {CREAT RDONLY}]
while {![eof $fs]} {
  gets $fs line
  if {[string equal -nocase [string index $line 0] "#"]} {
    set peakchannel([lindex $line 0]) [lindex $line 1]
putlog "added [lindex $line 0]"
    if {![string is digit $peakchannel([lindex $line 0])]} {
      putlog "PeakUsers: Error reading user value for [lindex $line 0]"
    }
  } elseif {![string length $line]} {
    continue
  } else {
    putlog "PeakUsers: Invalid channel name value: [lindex $line 0]"
  }
}
close $fs
