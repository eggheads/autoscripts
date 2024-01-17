# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                   #
#    Any editing done beyond this point is done at your own risk!   #
#                                                                   #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
set acrover "2.0"
set acrorel "release"
setudef flag acro
global acrostartcmd
putlog "Loading acro.tcl $acrover ($acrorel) by Souperman..."
putlog " Visit \037http://www.eggdrop.za.net/\037 for updates and other Tcl scripts."
if {![info exists alltools_loaded]||$allt_version<204} {
 putlog "\002[file tail [info script]]\002 failed to load: please load alltools.tcl v1.6 or higher before attempting to load this script."
# return
}

bind pub -|- $acrostartcmd acrostart

proc acrostart {nick host hand chan text} {
 global acrorounds acroround acroroundscores acrorealnames
 foreach eggchan [channels] {
  if {[channel get $eggchan acro]} {
    putmsg $chan "Game is already being played in another channel, cannot start here."
    return
  }
 }
 channel set $chan +acro
   putmsg $chan "===== ACRO Game started by $nick! ====="
   set acroround 1
   array unset acroroundscores
   array set acroroundscores {}
   array unset acrorealnames
   array set acrorealnames {}
   acrosel $chan
}

proc acrosel {acrochan} {
 global acromin acromax acrocurrent acroletters acroround acrorounds
 global botnick acrosbynum acrosbyname acrorealnames acronumsbyname acrotime
 global acroroundscores acrodisgust acroplayedbyhost acrovotedbyhost
 global acrostartcmd
 if {$acroround>$acrorounds} {
  if {[array size acroroundscores]==0} {
   putmsg $acrochan "Nobody scored any points for this game! [lindex $acrodisgust [rand [llength $acrodisgust]]]"
  } else {
   putmsg $acrochan "Final scores for this game:"
   set _final {}
   set _search [array startsearch acroroundscores]
   while {[set _current [array get acroroundscores [array nextelement acroroundscores $_search]]]!=""} {
    lappend _final "[lindex $_current 1],[lindex $_current 0]"
   }
   foreach _score [lsort -decreasing $_final] {
    putmsg $acrochan "$acrorealnames([lindex [split $_score ,] 1]) [lindex [split $_score ,] 0]"
   }
  }
  putmsg $acrochan "===== Game Over! Type $acrostartcmd to play again. ====="
  foreach eggchan [channels] {
    if {[channel get $eggchan acro]} {
      channel set $eggchan -acro
    }
  }
 } else {
  set acrocurrent ""
  array unset acrosbynum
  array unset acrosbyname
# array unset acrorealnames
  array unset acronumsbyname
  array unset acronamesbynum
  array unset acroplayedbyhost
  array unset acrovotedbyhost
  array set acrosbynum {}
  array set acrosbyname {}
# array set acrorealnames {}
  array set acronumsbyname {}
  array set acronamesbynum {}
  array set acroplayedbyhost {}
  array set acrovotedbyhost {}
  set _len [expr $acromin+[rand [expr $acromax-[expr $acromin-1]]]]
  set _i 1
  while {$_i<=$_len} {
   append acrocurrent "[string index $acroletters [rand 26]]"
   incr _i
  }
  putmsg $acrochan "Round $acroround/$acrorounds: The ACRO is: \"[strupr $acrocurrent]\"."
  putmsg $acrochan "You have $acrotime seconds! /msg $botnick ACRO <your-phrase>"
  utimer $acrotime "acrostartvoting $acrochan"
  bind msg -|- ACRO acrocheck
  incr acroround
 }
}

proc acrocheck {nick host hand text} {
 global acrosbynum acrosbyname acrorealnames acronumsbyname acrocurrent acronamesbynum
 global acroplayedbyhost
 foreach eggchan [channels] {
  if {[channel get $eggchan acro]} {
   set acrochan $eggchan
   break
  }
 }
 if {[onchan $nick $acrochan]} {
  if {[info exists acroplayedbyhost([strlwr $host])]} {
   putnotc $nick "You've already played this round."
  } else {
   set _err ""
   if {[llength [split $text]]!=[strlen $acrocurrent]} {
    if {[llength [split $text]]<[strlen $acrocurrent]} {
     set _err "not enough words"
    } else {
     set _err "too many words"
    }
   } else {
    set _i 1
    while {$_i<=[strlen $acrocurrent]} {
     if {[strlwr [stridx $acrocurrent [expr $_i-1]]]==[strlwr [stridx [join [lindex [split $text] [expr $_i-1]]] 0]]} {
      incr _i
     } else {
      set _err "character's mismatched"
      break
     }
    }
   }
   if {$_err!=""} {
    putnotc $nick "There are $_err in your acro. Try again."
   } else {
    set acrosbynum([expr [array size acrosbynum]+1]) $text
    set acrosbyname([strlwr $nick]) $text
    set acrorealnames([strlwr $nick]) $nick
    set acronumsbyname([strlwr $nick]) [array size acrosbynum]
    set acronamesbynum([array size acrosbynum]) [strlwr $nick]
    set acroplayedbyhost([strlwr $host]) 1
    putnotc $nick "You are player #$acronumsbyname([strlwr $nick]) for this round."
   }
  }
 }
}

proc acrostartvoting {acrochan} {
 global acrosbynum botnick acrovotetime acrovotes acrogaptime acrovoted
 global acrodisgust
 unbind msg -|- ACRO acrocheck
 if {[array size acrosbynum]==0} {
  putmsg $acrochan "Time's up! Not a single one of you played that round! [lindex $acrodisgust [rand [llength $acrodisgust]]]"
  utimer $acrogaptime "acrosel $acrochan"
 } else {
  putmsg $acrochan "Time's up! Here are the acros:"
  array unset acrovotes
  array set acrovotes {}
  array unset acrovoted
  array set acrovoted {}
  set _i 1
  while {$_i<=[array size acrosbynum]} {
   putmsg $acrochan "$_i. $acrosbynum($_i)"
   set acrovotes($_i) 0
   incr _i
  }
  putmsg $acrochan "Let the voting begin! You have $acrovotetime seconds! /msg $botnick VOTE <number>"
  utimer $acrovotetime "acrostopvoting $acrochan"
  bind msg -|- VOTE acrocheckvote
 }
}

proc acrocheckvote {nick host hand text} {
 global acrosbynum acrovoted acrovotes acrovotedbyhost
 foreach eggchan [channels] {
  if {[channel get $eggchan acro]} {
   set acrochan $eggchan
   break
  }
 }
 if {[onchan $nick $acrochan]} {
  if {[info exists acrovotedbyhost([strlwr $host])]} {
   putnotc $nick "You've already voted."
  } else {
   set _vote [join [lindex [split $text] 0]]
   if {![string is integer $_vote]||$_vote<1||$_vote>[array size acrosbynum]} {
    putnotc $nick "Invalid vote."
   } else {
    set acrovotes($_vote) [expr $acrovotes($_vote)+1]
    set acrovoted([strlwr $nick]) 1
    set acrovotedbyhost([strlwr $host]) 1
    putnotc $nick "Your vote has been counted."
   }
  }
 }
}

proc acrostopvoting {acrochan} {
 global acrogaptime acrovoted acroroundscores acrosbynum acros acronamesbynum
 global acrorealnames acrovotes acrodisgust
 unbind msg -|- VOTE acrocheckvote
 if {[array size acrovoted]==0} {
  putmsg $acrochan "Time's up! Not a single one of you voted! [lindex $acrodisgust [rand [llength $acrodisgust]]]"
  utimer $acrogaptime "acrosel $acrochan"
 } else {
  putmsg $acrochan "Time's up! Here are the scores for this round:"
  set _i 1
  while {$_i<=[array size acrosbynum]} {
   putmsg $acrochan "$_i. $acrorealnames($acronamesbynum($_i)) $acrovotes($_i)"
   if {[info exists acroroundscores($acronamesbynum($_i))]} {
    set acroroundscores($acronamesbynum($_i)) [expr $acroroundscores($acronamesbynum($_i))+$acrovotes($_i)]
   } else {
    set acroroundscores($acronamesbynum($_i)) $acrovotes($_i)
   }
   incr _i
  }
  utimer $acrogaptime "acrosel $acrochan"
 }
}

putlog "Successfully loaded acro.tcl $acrover!"
