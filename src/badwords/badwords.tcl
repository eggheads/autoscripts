##
# Bad Words v5.1.12
#  by MC_8 - Carl M. Gregory <mc8@purehype.net>
#  v6.0 Updated for autoscripts by Geo
#  This script will only run on eggdrop 1.6.13 or greater.
#
#    R.I.P. Mom, To always be remembered; Nancy Marie Gregory.
#
# My Website - http://mc.purehype.net/
# Have a bug?  http://mc.purehype.net/bugzilla/
##

##
# Description
##
# This script will act upon anyone in a specified channel that mentions a
# certain word that you add to the bad words database (wild cards accepted).
# Such actions popular are /kick and /ban, but can do more.  This script also
# has the ability to share it's bad words database with other bot's on the
# botnet.
#
# As I use this script, I notice it can do more that just kick and ban upon a
# bad word.  It can respond to text being said in the channel in many ways.
# Example, setup a bad word for !rules, have the script respond with /msg %nick
# The rules are bla bla bla.
#
# I use this script to ban those whom spam other channels as well as websites n`
# such.  If you do the same, I suggest using mc.spam_check as well.
#
# Please note that this script can handle channels with extended ASCII
# character, but only if your using TCL 8.2 or better.
##

##
# Commands
##
# For a list of valid commands, see '.help mc.bad_words' within DCC chat with
# your bot once this script is loaded.
##

##
# Configuration
##
#

# What user restriction should be set on the DCC console commands.  Access is
# restricted on a flag basis, syntax is GLOBAL|CHANNEL.  Set this to "-|-" to
# give access to anyone (assuming they can DCC chat to the bot).
set mc_bw(:config:access) "m|m"

# How do you want to mask bans?
#      0 - *!user@host.domain
#      1 - *!*user@host.domain
#      2 - *!*@host.domain
#      3 - *!*user@*.domain
#      4 - *!*@*.domain
#      5 - nick!user@host.domain
#      6 - nick!*user@host.domain
#      7 - nick!*@host.domain
#      8 - nick!*user@*.domain
#      9 - nick!*@*.domain
#     You can also specify a type of 10 to 19 which correspond to masks 0 to 9.
#     But if the host.domain is a;
#       hostname = Instead of using a * wildcard to replace portions of the
#                  host.domain, it replaces the numbers in the host.domain with
#                  a '?' (question mark) wildcard.
#       ip       = It will mask as normal, with no '?' (question mark)
#                  replacements as does hostname.
set mc_bw(:config:banmask) 11

# Only bot's matching the flag you input below will be allowed to share with the
# bot this script is running on.  What flag do you want to designate for such a
# thing?  If your not sure as to what to set this to, then just leave it set to
# "B".  This setting is here just incase another script uses the "B" custom flag
# (as not conflict, so it's changeable).  Valid flags here can be A through Z
# (capital letters ONLY), and pertain to global flags.  You can set this to ""
# to disable sharing.  To add another bot to the sharing list, in DCC chat all
# you have to do is '.chattr <bot> +B'.
set mc_bw(:config:share:flag) "B"

# Bots can share the bad words database between each other, assuming they match
# the key you set here.  This makes it so 1, no one can hack your bot 2, no
# other bot's running this script can modify entrys in yours unless they have
# you key.
set mc_bw(:config:share:key) "BlaBlaBlaBla"

# Where do you want to store the data this script will accumulate?
set mc_bw(:config:database) "./.badwords.dat"

## SVS Client (Script Version Service) v4.0.4 ##
# Once a day, the SVS Client will connect to MC_8's SVS Server to determine if
# there is a newer version of this script available.  If a newer version is
# found, the script will be auto updated.

# [0=no/1=yes] Do you want to enable auto updating?  If you chose to disable
# auto updating, it will not automatically update the script upon finding a
# newer version.
set mc_bw(:config:svs:enable) 1

#
##

##
# Done with configurations, do not edit past here unless you know TCL.
##
#

#Script:mc_bw

catch {unset temp}
set mc_bw(info:vars) ""
foreach {temp(name) temp(value)} [array get mc_bw :config:*] {
  lappend mc_bw(info:vars) [list $temp(name) $temp(value)]
}
set mc_bw(info:loc) [info script]
array set mc_bw [list \
  script                 "Bad Words" \
  version                "v5.1.12" \
  svs:script             "badwerds" \
  svs:version            "005001012000" \
  svs:client_version     "v4.0" \
  svs:client_svs_version "004000000000" \
  svs:server             "mc.svs.purehype.net" \
  svs:port               "81" \
  svs:get                "/index.tcl"]
set mc_bw(svs:query)    "svs=$mc_bw(svs:script)&"
append mc_bw(svs:query) "version=$mc_bw(svs:version)&"
append mc_bw(svs:query) "svs_version=$mc_bw(svs:client_svs_version)"

if {![info exists numversion] || 
    ([string range $numversion 0 4] < "10613") ||
    (([string range $numversion 0 4] == "10613") &&
     ([string range $numversion 5 6] != "00"))} {
  set temp(tag) "$mc_bw(script) $mc_bw(version)"
  putloglev o * \
    "$temp(tag) by MC_8 will only work on eggdrop 1.6.13 (stable) or greater."
  putloglev o * "$temp(tag)  will not work with eggdrop $version."
  putloglev o * "$temp(tag)  not loaded."
  return 1
}

setudef flag mc.bad_words.exempt_voice
setudef flag mc.bad_words.exempt_op

# Halfops support:
if {[info commands ishalfop] == "ishalfop"} {
  setudef flag mc.bad_words.exempt_halfop
  set mc_bw(halfop:support) 1
} else {set mc_bw(halfop:support) 0}
# ^

# Configuration variable sanity.
foreach {temp(a) temp(b)} [list :config:access     "m|m" \
                                :config:database   "./.badwords.dat" \
                                :config:banmask    "2" \
                                :config:share:flag "B" \
                                :config:share:key  "key"] {
  if {![info exists mc_bw($temp(a))]} {
    set mc_bw($temp(a)) $temp(b)
  }
}
if {$mc_bw(:config:access) == ""} {
  set mc_bw(:config:access) "n|n"
}
if {![regexp -- {^[A-Z]$} $mc_bw(:config:share:flag)]} {
  set mc_bw(:config:share:flag) "B"
}
if {$mc_bw(:config:share:key) == ""} {
  set mc_bw(:config:share:key) "key"
}
#

# Error system, v3.1
proc mc:bw:error {command error arg} {
  global mc_bw version lastbind errorInfo
  putloglev o * "Error in script $mc_bw(script) $mc_bw(version)."
  putloglev o * "    Error System: v3.0"
  putloglev o * \
    "       Last Bind: [expr {[info exists lastbind]?$lastbind:"-NULL-"}]"
  putloglev o * "         Command: $command"
  putloglev o * "       Arguments: $arg"
  putloglev o * "       Error Msg: $error"
  putloglev o * \
    "    Egg. Version: [expr {[info exists version]?$version:"-NULL-"}]"
  putloglev o * "     TCL Version: [info tclversion]"
  putloglev o * "  TCL Patchlevel: [info patchlevel]"
  putloglev o * "*** Please submit this bug so MC_8 can fix it.  Visit"
  putloglev o * \
    "*** http://mc.purehype.net/bugzilla/ to properly report the bug."
  putloglev o * \
    "*** Please include ALL info. in the bug report, including the next(s)."
  error $errorInfo
}

proc mc:bw:errchk {command arg} {
  lappend ::lastCommand "$command $arg"
  set ::lastCommand\
    [lrange $::lastCommand [expr [llength $::lastCommand]-5] end]
  if {![catch {eval $command $arg} return]} {return $return}
  mc:bw:error $command $return $arg
  return 0
}
# ^

bind ctcp - ACTION mc:bw:ctcp:action
proc mc:bw:ctcp:action {nick uhost handle channel key arg} {
  return [mc:bw:errchk mc:bw:ctcp:action_ \
    [list $nick $uhost $handle $channel $key $arg]]
}
proc mc:bw:ctcp:action_ {nick uhost handle channel key arg} {
  if {[isbotnick $channel]} {set via private} \
  else {set via public}

  mc:bw:list evaluate $nick $uhost $handle $channel $arg $via
}

# +badword [channel1[,channel2[,channelN]]] <*bad string*>
utimer 1 [list mc:bw:list \
  bind dcc $mc_bw(:config:access) +badword mc:bw:dcc:+badword rebind]
proc mc:bw:dcc:+badword {handle index arg} {
  return [mc:bw:errchk mc:bw:dcc:+badword_ [list $handle $index $arg]]
}
proc mc:bw:dcc:+badword_ {handle index arg} {
  global mc_bw

  mc:bw:unlist [split $arg] {channels ""} {args ""}
  set string [join $args]
  if {$string == ""} {
    set string $channels
    set channels "global"
  } else {
    set temp() 0
    foreach channel [split $channels ,] {
      if {[validchan $channel] ||
          ($channel == "message") ||
          ($channel == "global")} {
        continue
      }
      set temp() 1
      break
    }
    if {$temp()} {
      set string "$channels $string"
      set channels "global"
    } else {
      set channels [split $channels ,]
    }
  }

  if {[regexp -- {^(-?(-h(elp)?|-\?|-s(yntax)?))?$} $arg] || ($string == "")} {
    mc:bw:help $index +badword
    return 0
  }

  mc:bw:unlist [mc:bw:list filter_channels_by_access $handle $index $channels] \
    good bad
  if {$good == ""} {
    set temp(1) "You do not have access to add this entry under"
    putdcc $index "$mc_bw(script): $temp(1) any of the channels specified."
    return 0
  }

  mc:bw:unlist [mc:bw:list add bad $good $string $handle] id

  putdcc $index "$mc_bw(script): Added '$string' under bad word id is $id."
  putdcc $index "$mc_bw(script): Channels:"
  foreach channel $good {
    putdcc $index "$mc_bw(script):    $channel"
  }
  putloglev c * "#$handle# +badword $good $string"
  return 0
}

# Usage: +exemptword [channel1[,channel2[,channelN]]] <*exempt string*>
utimer 1 [list mc:bw:list \
  bind dcc $mc_bw(:config:access) +exemptword mc:bw:dcc:+exemptword rebind]
proc mc:bw:dcc:+exemptword {handle index arg} {
  return [mc:bw:errchk mc:bw:dcc:+exemptword_ [list $handle $index $arg]]
}
proc mc:bw:dcc:+exemptword_ {handle index arg} {
  global mc_bw

  mc:bw:unlist [split $arg] {channels ""} {args ""}
  set string [join $args]
  if {$string == ""} {
    set string $channels
    set channels "global"
  } else {
    set temp() 0
    foreach channel [split $channels ,] {
      if {[validchan $channel] ||
          ($channel == "message") ||
          ($channel == "global")} {
        continue
      }
      set temp() 1
      break
    }
    if {$temp()} {
      set string "$channels $string"
      set channels "global"
    } else {
      set channels [split $channels ,]
    }
  }

  if {[regexp -- {^(-?(-h(elp)?|-\?|-s(yntax)?))?$} $arg] || ($string == "")} {
    mc:bw:help $index +exemptword
    return 0
  }

  mc:bw:unlist [mc:bw:list filter_channels_by_access $handle $index $channels] \
    good bad
  if {$good == ""} {
    set temp(1) "You do not have access to add this entry under"
    putdcc $index "$mc_bw(script): $temp(1) any of the channels specified."
    return 0
  }

  mc:bw:unlist [mc:bw:list add exempt $good $string $handle] id

  putdcc $index "$mc_bw(script): Added '$string' under exempt word id is $id."
  putdcc $index "$mc_bw(script): Channels:"
  foreach channel $good {
    putdcc $index "$mc_bw(script):    $channel"
  }
  putloglev c * "#$handle# +exemptword $good $string"
  return 0
}

# Usage: -badword <badword_id>
utimer 1 [list mc:bw:list \
  bind dcc $mc_bw(:config:access) -badword mc:bw:dcc:-badword rebind]
proc mc:bw:dcc:-badword {handle index arg} {
  return [mc:bw:errchk mc:bw:dcc:-badword_ [list $handle $index $arg]]
}
proc mc:bw:dcc:-badword_ {handle index arg} {
  global mc_bw

  mc:bw:unlist $arg {id ""}

  if {[regexp -- {^(-?(-h(elp)?|-\?|-s(yntax)?))?$} $id]} {
    mc:bw:help $index -badword
    return 0
  }

  if {![mc:bw:list exists $id]} {
    putdcc $index "$mc_bw(script): Id $id does not exist."
    return 0
  }
  if {[mc:bw:list get_info $id type] == "exempt"} {
    set temp(1) "Id $id is holding an exempt word, try"
    putdcc $index "$mc_bw(script): $temp(1) '-exemptword $id' instead."
    putloglev c * "#$handle# Denied (wrong command for type): -badword $id"
    return 0
  }

  set temp(pchannels) [mc:bw:list get_info $id channels] ;# Previous channels.
  mc:bw:unlist \
    [mc:bw:list filter_channels_by_access $handle $index $temp(pchannels)] \
    good bad
  if {$bad != ""} {
    set temp(1) "You do not have access to one or more of the channels"
    putdcc $index "$mc_bw(script): $temp(1) that this entry pertains to."
    putloglev c * "#$handle# Denied (no access): -badword $id"
    return 0
  }

  mc:bw:list remove $id
  putdcc $index "$mc_bw(script): Removed bad word id $id."
  putloglev c * "#$handle# -badword $id"
  return 0
}

# Usage: badwords [channel|'message'|'global'|'all']
utimer 1 [list mc:bw:list \
  bind dcc $mc_bw(:config:access) badwords mc:bw:dcc:badwords rebind]
proc mc:bw:dcc:badwords {handle index arg} {
  return [mc:bw:errchk mc:bw:dcc:badwords_ [list $handle $index $arg]]
}
proc mc:bw:dcc:badwords_ {handle index arg} {
  global mc_bw
  mc:bw:unlist [split $arg] [list in(channel) "all"]

  if {[regexp -- {^(-?(-h(elp)?|-\?|-s(yntax)?))$} $arg]} {
    mc:bw:help $index badwords
    return 0
  }

  if {![regexp -- {(message|global|all)} $in(channel)] &&
      (![validchan $in(channel)])} {
    putdcc $index "$mc_bw(script): Invalid channel, $in(channel)."
    return 0
  }

  mc:bw:unlist \
    [mc:bw:list filter_channels_by_access $handle $index $in(channel)] \
    temp(gchannels) temp(bchannels)
  if {$temp(gchannels) == ""} {
    putdcc $index "$mc_bw(script): You do not have access to view any channels."
    return 0
  }
  if {$temp(bchannels) != ""} {
    set temp(msg) "You have limited access, not showing these channels:"
    putdcc $index "$mc_bw(script): $temp(msg) [join $temp(bchannels) ", "]"
  }

  if {$in(channel) == "all"} {
    putloglev c * "#$handle# badwords all"
    putdcc $index "$mc_bw(script): Bad words list: all"
  } elseif {$in(channel) == "global"} {
    putloglev c * "#$handle# badwords global"
    putdcc $index "$mc_bw(script): Bad words list: global"
  } else {
    putloglev c * "#$handle# badwords [join $temp(gchannels)]"
    putdcc $index "$mc_bw(script): Bad words list: [join $temp(gchannels) ", "]"
  }
  set posted ""
  foreach channel $temp(gchannels) {
    foreach line [mc:bw:list list $channel bad] {
      if {[lsearch -exact $posted $line] > -1} {continue}
      lappend posted $line
    }
  }
  foreach line [lsort -index 0 -dictionary $posted] {
    foreach name [list id type added_time channels string added_by expire \
                       warn warn_do post_warn_do use_regexp strip exempt  \
                       share last_used] {
      set temp(line:$name) [mc:bw:list get_value_from_line $name $line]
    }
    foreach {name value} [list added_time 0 expire 0 use_regexp 0 strip 0 \
                               share 1 last_used 0 warn 0] {
      if {$temp(line:$name) == ""} {set temp(line:$name) $value}
    }

    set added_time [mc:bw:list unixtime_to_days $temp(line:added_time)]
    if {[string match *day* $added_time]} {append added_time " ago"}

    if {!$temp(line:expire)} {set expire "permanent"} \
    else {
      set expire [mc:bw:list unixtime_to_days $temp(line:expire)]
      if {[string match *day* $expire]} {set expire "expires in $expire"} \
      else {set expire "expires at $expire"}
    }

    if {!$temp(line:last_used)} {set last_used "never"} \
    else {
      set last_used [mc:bw:list unixtime_to_days $temp(line:last_used)]
      if {[string match *day* $last_used]} {append last_used " ago"}
    }

    set strip [expr {($temp(line:strip))?"stripping mIRC color codes":""}]
    set regexp \
      [expr {($temp(line:use_regexp)=="1")?\
      "using regular expression matching":""}]
    set sharing [expr {($temp(line:share))?"sharing":""}]
    set temp(1) [list $strip $regexp $sharing]
    set temp(2) ""
    foreach item $temp(1) {
      if {$item == ""} {continue}
      lappend temp(2) $item
    }
    set temp(1) [join $temp(2) ", "]
    set temp(1-0) [string toupper [string index $temp(1) 0]]
    set temp(1-1) [string tolower [string range $temp(1) 1 end]]
    set temp(1) $temp(1-0)$temp(1-1)

    putdcc $index \
      [format { [%3s] %s (%s)} $temp(line:id) $temp(line:string) $expire]
    if {$temp(1) != ""} {
      putdcc $index [format {%7s %s.} "" $temp(1)]
    }
    putdcc $index \
      [format {%7s Added by %s (%s).} "" $temp(line:added_by) $added_time]
    putdcc $index \
      [format {%7s Last used %s.} "" "$last_used"]
    putdcc $index \
      [format {%7s Channels active on: %s} "" \
        [join $temp(line:channels) ", "]]
    putdcc $index \
      [format {%7s Exemption: %s} "" $temp(line:exempt)]
    set s [expr {($temp(line:warn)=="1")?"":"s"}]
    putdcc $index \
      [format {%7s To do, if within %s warning%s:} "" $temp(line:warn) $s]
    foreach item $temp(line:warn_do) {
      putdcc $index [format {%9s %s} "" $item]
    }
    putdcc $index [format {%7s To do, otherwise:} ""]
    foreach item $temp(line:post_warn_do) {
      putdcc $index [format {%9s %s} "" $item]
    }
  }
  return 0
}

utimer 1 [list mc:bw:list \
  bind dcc $mc_bw(:config:access) badwordset mc:bw:dcc:badwordset rebind]
proc mc:bw:dcc:badwordset {handle index arg} {
  return [mc:bw:errchk mc:bw:dcc:badwordset_ [list $handle $index $arg]]
}
proc mc:bw:dcc:badwordset_ {handle index arg} {
  global mc_bw
  mc:bw:unlist [split $arg] id name {args ""}
  if {![info exists name]} {
    mc:bw:help $index badwordset
    return 0
  }
  set value [join $args]
  set id [string tolower $id]
  set name [string tolower $name]

  if {![mc:bw:list exists $id]} {
    putdcc $index "$mc_bw(script): Invalid id, $id."
    return 0
  }
  if {$name != "channels"} {
    set temp(cchannels) [mc:bw:list get_info $id channels] ;# Current channels.
    mc:bw:unlist \
      [mc:bw:list filter_channels_by_access $handle $index $temp(cchannels)] \
      has_access no_access
    if {$no_access != ""} {
      set temp(1) "You do not have access to modify at least one or more"
      set temp(2) "channels for the indicated bad word id."
      putdcc $index "$mc_bw(script): $temp(1) $temp(2)"
      return 0
    }
  }

  switch -- $name {

    "channels" {
      # Usage: badwordset <badword_id> channels [channel1[ channel2[ channelN]]]
      set value [string tolower [split $value ", "]]
      mc:bw:unlist \
        [mc:bw:list filter_channels_by_access $handle $index $value] \
        temp(gchannels) temp(bchannels)
      if {($temp(gchannels) == "") && ($value != "")} {
        putdcc $index \
          "$mc_bw(script): Invalid access for channels: $temp(bchannels)."
        return 0
      }

      set temp(putloglev) \
        "#$handle# badwordset $id channels [join $temp(gchannels) ,]"

      set temp(pchannels) \
        [mc:bw:list get_info $id channels] ;# Previous channels.
      mc:bw:unlist \
        [mc:bw:list filter_channels_by_access $handle $index $temp(pchannels)] \
        a b
      # User doesn't have access to modify previous channels listed in $b.
      foreach temp(channel) $b {lappend temp(gchannels) $temp(channel)}

      # Remove duplicates
      set temp(1) ""
      foreach temp(channel) $temp(gchannels) {
        if {[lsearch -exact $temp(1) $temp(channel)] > -1} {continue}
        lappend temp(1) $temp(channel)
      }
      set temp(gchannels) $temp(1)

      mc:bw:list modify $id channels $temp(gchannels)
      putdcc $index "$mc_bw(script): New channels for bad word id $id:"
      foreach channel $temp(gchannels) {
        putdcc $index "$mc_bw(script):    $channel"
      }
      putloglev c * $temp(putloglev)
      return 0
    }

    "expire" {
      # Usage: badwordset <badword_id> expire [XdXhXm]
      if {$value == ""} {
        set value 0
        set temp(putloglev) "#$handle# badwordset $id expire"
      } else {
        set temp(1) [mc:bw:list clean_time_string $value]
        set value [mc:bw:list time_string_to_unixtime $value]
        set temp(putloglev) "#$handle# badwordset $id expire $temp(1)"
      }

      mc:bw:list modify $id expire $value
      set temp(1) "$mc_bw(script): Bad word entry $id will expire in:"
      if {!$value} {set temp(1) "Never, perminate"} \
      else {set temp(1) [duration [expr $value - [clock seconds]]]}
      putdcc $index \
        "$mc_bw(script): Bad word entry $id will expire in: $temp(1)"

      putloglev c * $temp(putloglev)
      return 0
    }

    "warn" {
      # Usage: badwordset <badword_id> warn [number]
      if {$value == ""} {set value 0} \
      elseif {[regexp -- {[^0-9]} $value]} {
        putdcc $index "$mc_bw(script): $value is not a valid whole number."
        return 0
      }

      mc:bw:list modify $id warn $value
      set s [expr {($value == "1")?"":"s"}]
      putdcc $index \
        "$mc_bw(script): Bad word entry $id now has $value warning$s."
      putloglev c * "#$handle# badwordset $id warn $value"
      return 0
    }

    "warn_do" {
      # Usage: badwordset <badword_id> warn_do [do1['\n'do2['\n'doN]]]
      regsub -all -- {\\n} $value \n value
      set new_value ""
      foreach item [split $value \n] {
        set item [string trim $item "\t "]
        if {$item == ""} {continue}
        lappend new_value $item
      }; set value $new_value

      mc:bw:list modify $id warn_do $value

      set temp(h) "$mc_bw(script): Bad word entry $id"
      if {$value == ""} {
        putdcc $index "$temp(h) now set to do nothing upon warning."
        putloglev c * "#$handle# badwordset $id warn_do"
      } else {
        putdcc $index "$temp(h) will now do upon warning:"
        foreach item $value {
          putdcc $index "$mc_bw(script):   $item"
        }
        putloglev c * "#$handle# badwordset $id warn_do"
        foreach item $value {
          putloglev c * "     $item"
        }
      }

      return 0
    }

    "post_warn_do" {
      # Usage: badwordset <badword_id> post_warn_do <do1['\n'do2['\n'doN]]>
      regsub -all -- {\\n} $value \n value
      set new_value ""
      foreach item [split $value \n] {
        set item [string trim $item "\t "]
        if {$item == ""} {continue}
        lappend new_value $item
      }; set value $new_value

      mc:bw:list modify $id post_warn_do $value

      set temp(h) "$mc_bw(script): Bad word entry $id"
      if {$value == ""} {
        putdcc $index "$temp(h) now set to do nothing upon post warning."
        putloglev c * "#$handle# badwordset $id post_warn_do"
      } else {
        putdcc $index "$temp(h) will now do upon post warning:"
        foreach item $value {
          putdcc $index "$mc_bw(script):   $item"
        }
        putloglev c * "#$handle# badwordset $id post_warn_do"
        foreach item $value {
          putloglev c * "     $item"
        }
      }
      return 0
    }

    "-regexp" {
      # Usage: badwordset <badword_id> <+/->regexp
      mc:bw:list modify $id regexp 0
      putdcc $index \
        "$mc_bw(script): Bad word entry $id now set to not use regexp."
      return 1
    }

    "+regexp" {
      # Usage: badwordset <badword_id> <+/->regexp
      mc:bw:list modify $id regexp 1
      putdcc $index "$mc_bw(script): Bad word entry $id now set to use regexp."
      return 1
    }

    "-strip" {
      # Usage: badwordset <badword_id> <+/->strip
      mc:bw:list modify $id strip 0
      putdcc $index "$mc_bw(script): Bad word entry $id now set to not strip."
      return 1
    }

    "+strip" {
      # Usage: badwordset <badword_id> <+/->strip
      mc:bw:list modify $id strip 1
      putdcc $index "$mc_bw(script): Bad word entry $id now set to strip."
      return 1
    }

    "exempt" {
      # Usage: badwordset <badword_id> exempt [flags]
      mc:bw:unlist [split $value |] {global "-"} {channel "-"}
      set value $global|$channel

      mc:bw:list modify $id exempt $value
      putdcc $index \
        "$mc_bw(script): Bad word entry $id's exempt flags set to $value."
      putloglev c * "#$handle# badwordset $id exempt $value"
      return 0
    }

    "-share" {
      # Usage: badwordset <badword_id> <+/->share
      mc:bw:list modify $id share 0
      putdcc $index \
        "$mc_bw(script): Bad word entry $id now set to not be shared."
      return 1
    }

    "+share" {
      # Usage: badwordset <badword_id> <+/->share
      mc:bw:list modify $id share 1
      putdcc $index "$mc_bw(script): Bad word entry $id now set to be shared."
      return 1
    }

    default {
      putdcc $index "Invalid name."
      mc:bw:help $index badwordset
      return 0
    }

  }
}

# Usage: -exemptword <exemptword_id>
utimer 1 [list mc:bw:list \
  bind dcc $mc_bw(:config:access) -exemptword mc:bw:dcc:-exemptword rebind]
proc mc:bw:dcc:-exemptword {handle index arg} {
  return [mc:bw:errchk mc:bw:dcc:-exemptword_ [list $handle $index $arg]]
}
proc mc:bw:dcc:-exemptword_ {handle index arg} {
  global mc_bw

  mc:bw:unlist $arg {id ""}

  if {[regexp -- {^(-?(-h(elp)?|-\?|-s(yntax)?))?$} $id]} {
    mc:bw:help $index -exemptword
    return 0
  }

  if {![mc:bw:list exists $id]} {
    putdcc $index "$mc_bw(script): Id $id does not exist."
    return 0
  }
  if {[mc:bw:list get_info $id type] == "bad"} {
    set temp(1) "Id $id is holding a bad word, try"
    putdcc $index "$mc_bw(script): $temp(1) '-badword $id' instead."
    putloglev c * "#$handle# Denied (wrong command for type): -exemptword $id"
    return 0
  }

  set temp(pchannels) [mc:bw:list get_info $id channels] ;# Previous channels.
  mc:bw:unlist \
    [mc:bw:list filter_channels_by_access $handle $index $temp(pchannels)] \
    good bad
  if {$bad != ""} {
    set temp(1) "You do not have access to one or more of the channels"
    putdcc $index "$mc_bw(script): $temp(1) that this entry pertains to."
    putloglev c * "#$handle# Denied (no access): -exemptword $id"
    return 0
  }

  mc:bw:list remove $id
  putdcc $index "$mc_bw(script): Removed exempt word id $id."
  putloglev c * "#$handle# -exemptword $id"
  return 0
}

# Usage: exemptwords [channel|'message'|'global'|'all']
utimer 1 [list mc:bw:list \
  bind dcc $mc_bw(:config:access) exemptwords mc:bw:dcc:exemptwords rebind]
proc mc:bw:dcc:exemptwords {handle index arg} {
  return [mc:bw:errchk mc:bw:dcc:exemptwords_ [list $handle $index $arg]]
}
proc mc:bw:dcc:exemptwords_ {handle index arg} {
  global mc_bw
  mc:bw:unlist [split $arg] [list in(channel) "all"]

  if {[regexp -- {^(-?(-h(elp)?|-\?|-s(yntax)?))$} $arg]} {
    mc:bw:help $index exemptwords
    return 0
  }

  if {![regexp -- {(message|global|all)} $in(channel)] &&
      (![validchan $in(channel)])} {
    putdcc $index "$mc_bw(script): Invalid channel, $in(channel)."
    return 0
  }

  mc:bw:unlist \
    [mc:bw:list filter_channels_by_access $handle $index $in(channel)] \
    temp(gchannels) temp(bchannels)
  if {$temp(gchannels) == ""} {
    putdcc $index "$mc_bw(script): You do not have access to view any channels."
    return 0
  }
  if {$temp(bchannels) != ""} {
    set temp(msg) "You have limited access, not showing these channels:"
    putdcc $index "$mc_bw(script): $temp(msg) [join $temp(bchannels) ", "]"
  }

  if {$in(channel) == "all"} {
    putloglev c * "#$handle# exemptwords all"
    putdcc $index "$mc_bw(script): Exempt words list: all"
  } elseif {$in(channel) == "global"} {
    putloglev c * "#$handle# exemptwords global"
    putdcc $index "$mc_bw(script): Exempt words list: global"
  } else {
    putloglev c * "#$handle# exemptwords [join $temp(gchannels)]"
    putdcc $index \
      "$mc_bw(script): Exempt words list: [join $temp(gchannels) ", "]"
  }
  set posted ""
  foreach channel $temp(gchannels) {
    foreach line [mc:bw:list list $channel exempt] {
      if {[lsearch -exact $posted $line] > -1} {continue}
      lappend posted $line
    }
  }
  foreach line [lsort -index 0 -dictionary $posted] {
    foreach name [list id type added_time channels string added_by expire \
                       use_regexp strip share last_used warn] {
      set temp(line:$name) [mc:bw:list get_value_from_line $name $line]
    }
    foreach {name value} [list added_time 0 expire 0 use_regexp 0 strip 0 \
                               share 1 last_used 0 warn 0] {
      if {$temp(line:$name) == ""} {set temp(line:$name) $value}
    }

    set added_time [mc:bw:list unixtime_to_days $temp(line:added_time)]
    if {[string match *day* $added_time]} {append added_time " ago"}

    if {!$temp(line:expire)} {set expire "permanent"} \
    else {
      set expire [mc:bw:list unixtime_to_days $temp(line:expire)]
      if {[string match *day* $expire]} {set expire "expires in $expire"} \
      else {set expire "expires at $expire"}
    }

    if {!$temp(line:last_used)} {set last_used "never"} \
    else {
      set last_used [mc:bw:list unixtime_to_days $temp(line:last_used)]
      if {[string match *day* $last_used]} {append last_used " ago"}
    }

    set strip [expr {($temp(line:strip))?"stripping mIRC color codes":""}]
    set regexp \
      [expr {($temp(line:use_regexp)=="1")?
        "using regular expression matching":""}]
    set sharing [expr {($temp(line:share))?"sharing":""}]
    set temp(1) [list $strip $regexp $sharing]
    set temp(2) ""
    foreach item $temp(1) {
      if {$item == ""} {continue}
      lappend temp(2) $item
    }
    set temp(1) [join $temp(2) ", "]
    set temp(1-0) [string toupper [string index $temp(1) 0]]
    set temp(1-1) [string tolower [string range $temp(1) 1 end]]
    set temp(1) $temp(1-0)$temp(1-1)
    
    putdcc $index \
      [format { [%3s] %s (%s)} $temp(line:id) $temp(line:string) $expire]
    if {$temp(1) != ""} {
      putdcc $index [format {%7s %s.} "" $temp(1)]
    }
    putdcc $index \
      [format {%7s Added by %s (%s).} "" $temp(line:added_by) $added_time]
    putdcc $index \
      [format {%7s Last used %s.} "" "$last_used"]
    putdcc $index \
      [format {%7s Channels active on: %s} "" \
        [join $temp(line:channels) ", "]]
  }
  return 0
}

utimer 1 [list mc:bw:list \
  bind dcc $mc_bw(:config:access) exemptwordset mc:bw:dcc:exemptwordset rebind]
proc mc:bw:dcc:exemptwordset {handle index arg} {
  return [mc:bw:errchk mc:bw:dcc:exemptwordset_ [list $handle $index $arg]]
}
proc mc:bw:dcc:exemptwordset_ {handle index arg} {
  global mc_bw
  mc:bw:unlist [split $arg] id name {args ""}
  if {![info exists name]} {
    mc:bw:help $index exemptwordset
    return 0
  }
  set value $args
  set id [string tolower $id]
  set name [string tolower $name]

  if {![mc:bw:list exists $id]} {
    putdcc $index "$mc_bw(script): Invalid id, $id."
    return 0
  }
  if {$name != "channels"} {
    set temp(cchannels) [mc:bw:list get_info $id channels] ;# Current channels.
    mc:bw:unlist \
      [mc:bw:list filter_channels_by_access $handle $index $temp(cchannels)] \
      has_access no_access
    if {$no_access != ""} {
      set temp(1) "You do not have access to modify at least one or more"
      set temp(2) "channels for the indicated exempt word id."
      putdcc $index "$mc_bw(script): $temp(1) $temp(2)"
      return 0
    }
  }

  switch -- $name {

    "channels" {
# Usage: exemptwordset <exemptword_id> channels [channel1[ channel2[ channelN]]]
      set value [string tolower [split $value ", "]]
      mc:bw:unlist \
        [mc:bw:list filter_channels_by_access $handle $index $value] \
        temp(gchannels) temp(bchannels)
      if {($temp(gchannels) == "") && ($value != "")} {
        putdcc $index \
          "$mc_bw(script): Invalid access for channels: $temp(bchannels)."
        return 0
      }

      set temp(putloglev) \
        "#$handle# exemptwordset $id channels [join $temp(gchannels) ,]"

      set temp(pchannels) \
        [mc:bw:list get_info $id channels] ;# Previous channels.
      mc:bw:unlist \
        [mc:bw:list filter_channels_by_access $handle $index $temp(pchannels)] \
        a b
      # User doesn't have access to modify previous channels listed in $b.
      foreach temp(channel) $b {lappend temp(gchannels) $temp(channel)}

      # Remove duplicates
      set temp(1) ""
      foreach temp(channel) $temp(gchannels) {
        if {[lsearch -exact $temp(1) $temp(channel)] > -1} {continue}
        lappend temp(1) $temp(channel)
      }
      set temp(gchannels) $temp(1)

      mc:bw:list modify $id channels $temp(gchannels)
      putdcc $index "$mc_bw(script): New channels for exempt word id $id:"
      foreach channel $temp(gchannels) {
        putdcc $index "$mc_bw(script):    $channel"
      }
      putloglev c * $temp(putloglev)
      return 0
    }

    "expire" {
      # Usage: exemptwordset <exemptword_id> expire [XdXhXm]
      if {$value == ""} {
        set value 0
        set temp(putloglev) "#$handle# exemptwordset $id expire"
      } else {
        set temp(1) [mc:bw:list clean_time_string $value]
        set value [mc:bw:list time_string_to_unixtime $value]
        set temp(putloglev) "#$handle# exemptwordset $id expire $temp(1)"
      }

      mc:bw:list modify $id expire $value
      set temp(1) "$mc_bw(script): Exempt word entry $id will expire in:"
      if {!$value} {set temp(1) "Never, perminate"} \
      else {set temp(1) [duration [expr $value - [clock seconds]]]}
      putdcc $index \
        "$mc_bw(script): Exempt word entry $id will expire in: $temp(1)"

      putloglev c * $temp(putloglev)
      return 0
    }

    "-regexp" {
      # Usage: exemptwordset <exemptword_id> <+/->regexp
      mc:bw:list modify $id regexp 0
      putdcc $index \
        "$mc_bw(script): Exempt word entry $id now set to not use regexp."
      return 1
    }

    "+regexp" {
      # Usage: exemptwordset <exemptword_id> <+/->regexp
      mc:bw:list modify $id regexp 1
      putdcc $index \
        "$mc_bw(script): Exempt word entry $id now set to use regexp."
      return 1
    }

    "-strip" {
      # Usage: exemptwordset <exemptword_id> <+/->strip
      mc:bw:list modify $id strip 0
      putdcc $index \
        "$mc_bw(script): Exempt word entry $id now set to not strip."
      return 1
    }

    "+strip" {
      # Usage: exemptwordset <exemptword_id> <+/->strip
      mc:bw:list modify $id strip 1
      putdcc $index "$mc_bw(script): Exempt word entry $id now set to strip."
      return 1
    }

    "-share" {
      # Usage: exemptwordset <exemptword_id> <+/->share
      mc:bw:list modify $id share 0
      putdcc $index \
        "$mc_bw(script): Exempt word entry $id now set to not be shared."
      return 1
    }

    "+share" {
      # Usage: exemptwordset <exemptword_id> <+/->share
      mc:bw:list modify $id share 1
      putdcc $index \
        "$mc_bw(script): Exempt word entry $id now set to be shared."
      return 1
    }

    default {
      putdcc $index "Invalid name."
      mc:bw:help $index exemptwordset
      return 0
    }

  }
}

bind evnt - save mc:bw:evnt
proc mc:bw:evnt {type} {return [mc:bw:errchk mc:bw:evnt_ [list $type]]}
proc mc:bw:evnt_ {type} {mc:bw:list save}

proc mc:bw:msgm {nick uhost handle arg} {
  return [mc:bw:errchk mc:bw:msgm_ [list $nick $uhost $handle $arg]]
}
proc mc:bw:msgm_ {nick uhost handle arg} {
  mc:bw:list evaluate $nick $uhost $handle message $arg private
}

proc mc:bw:notc {nick uhost handle arg {channel ""}} {
  return [mc:bw:errchk mc:bw:notc_ [list $nick $uhost $handle $arg $channel]]
}
proc mc:bw:notc_ {nick uhost handle arg channel} {
  if {$nick == ""} {return}
  if {[isbotnick $channel] || ($channel == "")} {
    set channel message
    set via private
  } else {
    # ONOTICE
    regexp -- {^@?(.*)$} $channel -> channel
    set via public
  }
  mc:bw:list evaluate $nick $uhost $handle $channel $arg $via
}

proc mc:bw:pubm {nick uhost handle channel arg} {
  return [mc:bw:errchk mc:bw:pubm_ [list $nick $uhost $handle $channel $arg]]
}
proc mc:bw:pubm_ {nick uhost handle channel arg} {
  mc:bw:list evaluate $nick $uhost $handle $channel $arg public
}

bind link - * mc:bw:link
proc mc:bw:link {bot via} {
  return [mc:bw:errchk mc:bw:link_ [list $bot $via]]
}
proc mc:bw:link_ {bot via} {mc:bw:share:tx get $bot}

# Send keyword via encryption of the mc:bw:$key.
# Send data via encryption of the $key+$from, first index should be $from.
# Incoming message, list format:  <from> <message>

utimer 1 [list mc:bw:list bind bot - \
  [encrypt $mc_bw(:config:share:key) mc:bw:$mc_bw(:config:share:key)] \
    mc:bw:rx rebind]
proc mc:bw:rx {from command arg} {
  return [mc:bw:errchk mc:bw:rx_ [list $from $command $arg]]
}
proc mc:bw:rx_ {from command arg} {
  global mc_bw
  if {![matchattr $from $mc_bw(:config:share:flag)]} {return 0}
  set arg [decrypt "$mc_bw(:config:share:key)+$from" $arg]
  set arg [mc:bw:2list $arg]
  if {[lindex $arg 0] != $from} {
    putloglev o - "$mc_bw(script): Fake message received. (!= $from)"
    return 0
  }

  mc:bw:unlist [lindex $arg 1] command {args ""}

  switch -- $command {

    "request_all_data" {
      putloglev b * "$mc_bw(script): Sending bad words database to $from..."
      mc:bw:share:send $from [list synch_start]
      foreach line [mc:bw:list list] {
        set share [mc:bw:list get_value_from_line share $line]
        if {$share == "0"} {continue}
        set type [mc:bw:list get_value_from_line type $line]
        set channels [mc:bw:list get_value_from_line channels $line]
        set string [mc:bw:list get_value_from_line string $line]
        if {$type == "bad"} {
          set exempt [mc:bw:list get_value_from_line exempt $line]
          set message [list bad $channels $string $exempt]
        } elseif {$type == "exempt"} {
          set message [list exempt $channels $string]
        } else {continue}
        foreach item $line {
          mc:bw:unlist $item name value
          if {[regexp -- {^(type|channels|string|exempt)$} $name]} {continue}
          mc:bw:share:send $from [list synch v4 $message $name $value]
        }
      }
      putloglev b * "$mc_bw(script): Sent bad words database to $from."
      mc:bw:share:send $from [list synch_stop]
    }

    "synch_start" {
      putloglev b * "$mc_bw(script): Receiving bad words database from $from..."
    }

    "synch" {
      mc:bw:unlist $args version message name args
      set value $args
      if {$version != "v4"} {
        putloglev d * \
          "$mc_bw(script): Synch message from $from, unknown version $version."
        return 0
      }
      mc:bw:unlist $message type channels string {exempt ""}
      mc:bw:list add_fromshare $type $channels $string $exempt $name $value
    }

    "synch_stop" {
      putloglev b * "$mc_bw(script): received bad words database from $from."
      mc:bw:list save -quiet
      mc:bw:list setup_binds
    }

    "add" {
      set line [lindex $args 0]
      foreach item $line {
        foreach {name value} $item {set $name $value}
      }
      foreach item $line {
        mc:bw:unlist $item name value
        if {[regexp -- {^(type|channels|string)$} $name]} {continue}
        mc:bw:list add_fromshare $type $channels $string -|- $name $value
      }
      mc:bw:list save -quiet
      mc:bw:list setup_binds
    }

    "remove" {
      mc:bw:unlist $args type channels string exempt

      mc:bw:list remove_fromshare $type $channels $string $exempt
      mc:bw:list setup_binds
    }

    "modify" {
      mc:bw:unlist $args type channels string exempt name value
      if {($name == "post_warn_do") || ($name == "warn_do")} {
        set value [list $value]
      }

      mc:bw:list add_fromshare $type $channels $string $exempt $name $value
      mc:bw:list save -quiet
      mc:bw:list setup_binds
    }

  }
}

proc mc:bw:share:tx {command {args ""}} {
  return [mc:bw:errchk mc:bw:share:tx [list $command $args]]
}
proc mc:bw:share:tx {command arg} {
  global mc_bw
  set args $arg
  switch -- $command {

    "get" {
      mc:bw:unlist $args {bot ""}
      if {![islinked $bot]} {return 0}
      mc:bw:share:send $bot [list request_all_data]
    }

    "add" {
      mc:bw:unlist $args id

      foreach line [mc:bw:list list] {
        set line_id [mc:bw:list get_value_from_line id $line]
        if {$line_id != $id} {continue}

        set share [mc:bw:list get_value_from_line share $line]
        if {$share == "0"} {continue}
        set type [mc:bw:list get_value_from_line type $line]
        set channels [mc:bw:list get_value_from_line channels $line]
        set string [mc:bw:list get_value_from_line string $line]
        if {$type == "bad"} {
          set exempt [mc:bw:list get_value_from_line exempt $line]
          set message [list bad $channels $string $exempt]
        } elseif {$type == "exempt"} {
          set message [list exempt $channels $string]
        } else {continue}
        foreach item $line {
          mc:bw:unlist $item name value
          if {[regexp -- {^(type|channels|string|exempt)$} $name]} {continue}
          mc:bw:send $from [list add v4 $message $name $value]
        }
      }

    }

  }
}

# Sends encrypted information.
proc mc:bw:share:send {bot message} {
  return [mc:bw:errchk mc:bw:send [list $bot $message]]
}
proc mc:bw:send {bot message} {
  global mc_bw botnet-nick
  set message [list ${botnet-nick} $message]
  set message [encrypt "$mc_bw(:config:share:key)+${botnet-nick}" $message]
  set message \
  "[encrypt $mc_bw(:config:share:key) mc:bw:$mc_bw(:config:share:key)] $message"
  if {[string length $message] > 400} {
    putloglev d * "$mc_bw(script): Could not send botnet information. (>400)"
    return 0
  }
  if {$bot == ""} {set bots [bots]} else {set bots [list $bot]}
  foreach bot $bots {
    if {(![islinked $bot]) || (![matchattr $bot $mc_bw(:config:share:flag)])} {
      continue
    }
    putbot $bot $message
  }
}

bind time - * mc:bw:time:expire
proc mc:bw:time:expire {a b c d e} {
  return [mc:bw:errchk mc:bw:time:expire_ [list $a $b $c $d $e]]
}
proc mc:bw:time:expire_ {a b c d e} {
  mc:bw:list do_expire
}

proc mc:bw:list {command {args ""}} {
  return [mc:bw:errchk mc:bw:list_ [list $command $args]]
}
proc mc:bw:list_ {command arg} {
  global mc_bw

  if {![info exists mc_bw(list)]} {set mc_bw(list) ""}

  if {[info tclversion] > 8.0} {
    set temp(newarg) [list]
    foreach temp(item) $arg {
      lappend temp(newarg) [encoding convertto [encoding system] $temp(item)]
    }; set args $temp(newarg)
  } else {set args $arg}

  switch -- $command {

    "add" {
      # Return: <id>
      mc:bw:unlist $args type channels string handle
      set channels [string tolower $channels]
      set string [string tolower $string]

      set id [mc:bw:list get_next_id]
      set add ""
      foreach {name value} [list id         $id             \
                                 type       $type           \
                                 added_time [clock seconds] \
                                 channels   $channels       \
                                 string     $string         \
                                 added_by   $handle] {
        lappend add [list $name $value]
      }
      lappend mc_bw(list) $add
      mc:bw:share:send "" [list add $add]
      mc:bw:list save -quiet
      if {$type == "bad"} {mc:bw:list setup_binds}
      return $id
    }

    "add_fromshare" {
      mc:bw:unlist $args type channels string exempt name value
      set value [lindex $value 0]
      set channels [string tolower $channels]
      set string [string tolower $string]

      foreach line [mc:bw:list list] {
        foreach temp() [list type channels string exempt] {
          set line_$temp() [mc:bw:list get_value_from_line $temp() $line]
        }
        if {($line_type == $type) && ($line_channels == $channels) &&
            ($line_string == $string) && ($line_exempt == $exempt)} {
          set fail 0
          foreach channel $line_channels {
            if {[lsearch -exact $channels $channel] == "-1"} {
              set fail 1
              break
            }
          }
          # Note, if sending bot has more channels than is already for that
          # entry, will not add the channels.  This is good?
          if {$fail} {continue}
          set id [mc:bw:list get_value_from_line id $line]
          # At this point, we found the matching entry and should modify instead
          # of add.
          set new_list ""
          foreach line $mc_bw(list) {
            if {[mc:bw:list get_value_from_line id $line] != $id} {
              lappend new_list $line
              continue
            }
            set new_line ""
            set found 0
            foreach item $line {
              if {[lindex $item 0] == $name} {
                set item [list $name $value]
                set found 1
              }
              lappend new_line $item
            }
            if {!$found} {
              lappend new_line [list $name $value]
            }
            lappend new_list $new_line
          }
          set mc_bw(list) $new_list
          return 1
        }
      }
      # At this point, we did not find a matching entry, should add.
      set id [mc:bw:list get_next_id]
      set add ""
      foreach {name value} [list id         $id             \
                                 type       $type           \
                                 added_time [clock seconds] \
                                 channels   $channels       \
                                 string     $string         \
                                 exempt     $exempt] {
        lappend add [list $name $value]
      }
      set new_line ""
      set found 0
      foreach line $add {
        mc:bw:unlist $line lname
        if {$lname == $name} {
          set line [list $name $value]
          set found 1
        }
        lappend new_line $line
      }
      if {!$found} {
        lappend new_line [list $name $value]
      }
      lappend mc_bw(list) $add
      # Don't save here, would save on massive sych ever line (bad).
    }

    "remove" {
      # Returns nothing.
      mc:bw:unlist $args id

      set new_list ""
      set type ""
      foreach line $mc_bw(list) {
        set temp(id) [mc:bw:list get_value_from_line id $line]
        if {$temp(id) == $id} {
          foreach name [list type channels string exempt] {
            set $name [mc:bw:list get_value_from_line $name $line]
          }
          mc:bw:share:send "" [list remove $type $channels $string $exempt]
          continue
        }
        lappend new_list $line
      }
      if {$mc_bw(list) != $new_list} {
        set mc_bw(list) $new_list
        mc:bw:list save -quiet
        if {$type == "bad"} {mc:bw:list setup_binds}
      }
    }

    "remove_fromshare" {
      mc:bw:unlist $args type channels string exempt
      set channels [string tolower $channels]
      set string [string tolower $string]

      set new_list ""
      foreach line [mc:bw:list list] {
        foreach temp() [list type channels string exempt] {
          set line_$temp() [mc:bw:list get_value_from_line $temp() $line]
        }
        if {(($type == "bad") &&
             ($line_type == $type) &&
             ($line_channels == $channels) &&
             ($line_string == $string) &&
             ($line_exempt == $exempt)) ||
            (($type == "exempt") &&
             ($line_type == $type) &&
             ($line_channels == $channels) &&
             ($line_string == $string))} {
          set fail 0
          foreach channel $line_channels {
            if {[lsearch -exact $channels $channel] == "-1"} {
              set fail 1
              break
            }
          }
          if {!$fail} {
            set remove_type $line_type
            continue
          }
        }; lappend new_list $line
      }
      if {$mc_bw(list) != $new_list} {
        set mc_bw(list) $new_list
        mc:bw:list save -quiet
        if {$remove_type == "bad"} {mc:bw:list setup_binds}
      }
    }

    "modify" {
      mc:bw:unlist $args id name value

      if {![mc:bw:list exists $id]} {return 0}

      set new_list ""
      foreach line $mc_bw(list) {
        if {[mc:bw:list get_value_from_line id $line] != $id} {
          lappend new_list $line
          continue
        }
        foreach name_ [list type channels string exempt] {
          set $name_ [mc:bw:list get_value_from_line $name_ $line]
        }
        mc:bw:share:send "" \
          [list modify $type $channels $string $exempt $name $value]
        set modified_type $type
        set new_line ""
        set found 0
        foreach item $line {
          if {[lindex $item 0] == $name} {
            set found 1
            set item [list $name $value]
          }
          lappend new_line $item
        }
        if {!$found} {
          lappend new_line [list $name $value]
        }
        lappend new_list $new_line
      }
      if {$mc_bw(list) != $new_list} {
        set mc_bw(list) $new_list
        mc:bw:list save -quiet
        if {$modified_type == "bad"} {mc:bw:list setup_binds}
      }
    }

    "list" {
      # Type is 'bad'
      #   id type added_time channels string added_by expire warn warn_do \
      #   post_warn_do use_regexp strip exempt share last_used
      # Type is 'exempt'
      #   id type added_time channels string added_by expire use_regexp \
      #   strip share last_used
      mc:bw:unlist $args {channel "all"} {type "all"}
      set channel [string tolower $channel]
      set type [string tolower $type]

      if {($channel == "all") && ($type == "all")} {return $mc_bw(list)}
      set return ""
      foreach line $mc_bw(list) {
        set temp(channels) [mc:bw:list get_value_from_line channels $line]
        set temp(type) [mc:bw:list get_value_from_line type $line]
        if {(($channel == "all") ||
             ([lsearch -exact $temp(channels) $channel] > -1)) &&
            (($type == "all") || ($temp(type) == $type))} {
          lappend return $line
        }
      }; return $return
    }

    "exists" {
      # Ex: exists <id>
      # Returns 1/0.
      mc:bw:unlist $args id

      foreach line [mc:bw:list list] {
        set temp(id) [mc:bw:list get_value_from_line id $line]
        if {$temp(id) == $id} {return 1}
      }; return 0
    }

    "get_next_id" {
      # Returns the next free id.
      # Starts counting at 1.
      set id_list 0
      foreach entry [mc:bw:list list] {
        set id [mc:bw:list get_value_from_line id $entry]
        if {$id == ""} {continue}
        lappend id_list $id
      }
      set id_list [lsort -dictionary $id_list]

      for {set i 0} {$i < [llength $id_list]} {incr i} {
        set id [lindex $id_list $i]
        set next_id [lindex $id_list [expr $i+1]]
        if {$id != [expr $next_id-1]} {
          set usable_id [expr $id+1]
          break
        }
      }; return $usable_id
    }

    "filter_channels_by_access" {
      # Returns 2 lists.
      # A list of good channels, and a list of bad channels.
      global mc_bw
      mc:bw:unlist $args handle index {channels "global"}

      set channels [string tolower $channels]
      set temp(gchannels) "" ;# Good channels.
      set temp(bchannels) "" ;# Bad channels.
      if {[lsearch -exact $channels all] > -1} {
        set channels [channels]
        lappend channels global
        lappend channels message
      }
      for {set temp(index) 0} {$temp(index) < [llength $channels]} {
        incr temp(index)
      } {
        set temp(channel) [lindex $channels $temp(index)]

        # To weed out already checked channels.
        if {([lsearch -exact $temp(gchannels) $temp(channel)] > -1) ||
            ([lsearch -exact $temp(bchannels) $temp(channel)] > -1)} {
          continue
        }

        if {![regexp -- {(global|message)} $temp(channel)]} {
          if {![validchan $temp(channel)]} {
            lappend temp(bchannels) $temp(channel)
            continue
          }
          if {![matchattr $handle $mc_bw(:config:access) $temp(channel)] &&
              ($mc_bw(:config:access) != "-|-")} {
            lappend temp(bchannels) $temp(channel)
            continue
          }
        } else {
          if {![matchattr $handle $mc_bw(:config:access)] &&
              ($mc_bw(:config:access) != "-|-")} {
            lappend temp(bchannels) $temp(channel)
            continue
          }
        }
        lappend temp(gchannels) $temp(channel)
      }
      return [list $temp(gchannels) $temp(bchannels)]
    }

    "unixtime_to_days" {
      # Convert incoming unixtime to the number of days it has been since then.

      # in: unixtime
      #out: x day(s) || hh:mm
      mc:bw:unlist $args unixtime

      set diff [expr [clock seconds]-$unixtime]
      if {[set days [expr $diff/60/60/24]] != "0"} {
        set s [expr {($days == "1")?"":"s"}]
        append days " day$s"
      } else {
        set days [clock format $unixtime -format %H:%M]
      }
      return $days
    }

    "get_info" {
      # Get name's value from id.
      # Return name's value for id, else return "".
      # If no name specified, returns full info for id.
      mc:bw:unlist $args id {name ""}
      foreach line [mc:bw:list list] {
        if {[mc:bw:list get_value_from_line id $line] != $id} {continue}
        if {$name == ""} {return $line}
        return [mc:bw:list get_value_from_line $name $line]
      }; return ""
    }

    "get_value_from_line" {
      # Get name's value from line.
      # Returns name's value from line, else return "".
      mc:bw:unlist $args name line
      if {$name == "use_regexp"} {set name regexp}
      foreach item $line {
        if {[lindex $item 0] == $name} {return [lindex $item 1]}
      }; return ""
    }

    "clean_time_string" {
      # Cleans up the time string of XdXhXm.
      # Takes 1d1d1d1d1d1d and converts it to 1d0h0m.
      mc:bw:unlist $args str

      if {![regexp -- {([0-9]*)m} $str -> min]  || ($min == "")}  {set min  0}
      if {![regexp -- {([0-9]*)h} $str -> hour] || ($hour == "")} {set hour 0}
      if {![regexp -- {([0-9]*)d} $str -> day]  || ($day == "")}  {set day  0}
      return "${day}d${hour}h${min}m"
    }

    "time_string_to_unixtime" {
      # No need to clean_time_string, this does it also.
      # Returns unixtime of the time_string, relative to now.
      # time_string is XdXhXm
      mc:bw:unlist $args str

      if {![regexp -- {([0-9]*)m} $str -> min]  || ($min == "")}  {set min  0}
      if {![regexp -- {([0-9]*)h} $str -> hour] || ($hour == "")} {set hour 0}
      if {![regexp -- {([0-9]*)d} $str -> day]  || ($day == "")}  {set day  0}
      return [expr [clock seconds]+($min*60)+($hour*3600)+($day*86400)]
    }

    "save" {
      if {[lsearch -exact $args -quiet] == -1} {
        putloglev o * "Writing $mc_bw(script) file..."
      }

      set io [open $mc_bw(:config:database) w]
      puts $io "#v4:$mc_bw(script):$mc_bw(version):[clock seconds]"
      foreach line [mc:bw:list list] {
        puts $io " $line"
      }
      close $io
    }

    "load" {
      if {[lsearch -exact $args -quiet] <= -1} {
        putloglev o * "Reading $mc_bw(script) file..."
        set quiet 0
      } else {set quiet 1}

      set new_list ""
      set db_ver ""
      if {![file exists $mc_bw(:config:database)]} {
        close [open $mc_bw(:config:database) w]
      }
      set io [open $mc_bw(:config:database) r]
      for {set index 0} {![eof $io]} {incr index} {
        gets $io line
        if {!$index} {
          if {[regexp -- {^#v([0-9]*):} $line -> db_ver]} {
            if {!$quiet && ($db_ver != "4")} {
              set temp() "converting (v$db_ver -> v4)"
              putloglev o * "$mc_bw(script): Old database found, $temp()..."
            }
            continue
          }
          if {!$quiet} {
            set temp() "converting (v1 -> v4)"
            putloglev o * "$mc_bw(script): Old database found, $temp()..."
          }
        }
        if {([string trim $line] == "") ||
            ([string index $line 0] == "#")} {continue}
        set temp(line) ""
        switch -- $db_ver {
          "2" {
            lappend temp(line) [list type bad]
            lappend temp(line) [list added_time [lindex $line 0]]
            set temp(channel) [lindex $line 1]
            set temp(channel) [expr {
                ($temp(channel)=="&global")?"global":
                ($temp(channel)=="&message")?"message":$temp(channel)}]
            lappend temp(line) [list channels [list $temp(channel)]]
            lappend temp(line) [list string [lindex $line 2]]
            set temp(msg) [lindex $line 3]
            lappend temp(line) [list warn_do "/notice %nick $temp(msg)"]
            set temp(pwd) \
              [list "/ban %channel %nick 60" "/kick %channel %nick $temp(msg)"]
            lappend temp(line) [list post_warn_do $temp(pwd)]
            lappend temp(line) [list added_by [lindex $line 4]]
            lappend temp(line) [list warn [lindex $line 5]]
            lappend temp(line) [list expire [lindex $line 6]]
            lappend temp(line) [list last_used [lindex $line 7]]
          }
          "3" {
            set temp(type) [string range [lindex $line 0] 1 end]
            lappend temp(line) [list type $temp(type)]
            lappend temp(line) [list added_time [lindex $line 1]]
            set temp(channel) [lindex $line 2]
            set temp(channel) [expr {
                ($temp(channel)=="&global")?"global":
                ($temp(channel)=="&message")?"message":$temp(channel)}]
            lappend temp(line) [list channels [list $temp(channel)]]
            lappend temp(line) [list string [lindex $line 3]]
            lappend temp(line) [list added_by [lindex $line 5]]
            lappend temp(line) [list expire [lindex $line 7]]
            lappend temp(line) [list last_used [lindex $line 8]]
            if {$temp(type) == "bad"} {
              set temp(msg) [lindex $line 4]
              lappend temp(line) [list warn_do "/notice %nick $temp(msg)"]
              set temp(pwd) \
               [list "/ban %channel %nick 60" "/kick %channel %nick $temp(msg)"]
              lappend temp(line) [list post_warn_do $temp(pwd)]
              lappend temp(line) [list warn [lindex $line 6]]
            }
          }
          "4" {set temp(line) [string trim $line]}
          default {
            set line [split $line]
            set temp(channel) [lindex $line 0]
            set temp(channel) [expr {
                ($temp(channel)=="&global")?"global":
                ($temp(channel)=="&message")?"message":$temp(channel)}]
            lappend temp(line) [list channels [list $temp(channel)]]
            lappend temp(line) [list string [lindex $line 1]]
          }
        }
        if {[info tclversion] > 8.0} {
          set new_line ""
          foreach item $temp(line) {
            set new_item ""
            foreach {name value} $item {
              set new_item [list \
                [encoding convertto [encoding system] $name] \
                [encoding convertto [encoding system] $value]]
            }
            lappend new_line $new_item
          }
          lappend new_list $new_line
        } else {lappend new_list $temp(line)}
      }
      close $io
      if {!$quiet} {
        set num [llength $new_list]
        set s [expr {($num=="1")?"":"s"}]
        putloglev o * "$mc_bw(script): Loaded $num record$s."
      }
      set mc_bw(list) $new_list
      mc:bw:list setup_binds
      return 1
    }

    "setup_binds" {
      set wide_bind 0
      foreach line [mc:bw:list list all bad] {
        set use_regexp [mc:bw:list get_value_from_line use_regexp $line]
        set strip [mc:bw:list get_value_from_line strip $line]
        set channels [mc:bw:list get_value_from_line channels $line]
        if {($use_regexp == "1") || ($strip == "1") ||
            ([lsearch -exact $channels global] > -1)} {
          set wide_bind 1
          break
        }
      }
      if {$wide_bind} {
        # If any badword has 'global' as the channel, uses mIRC control code
        # stripping, or uses regular expressions then I have to bind it wide.
        mc:bw:list bind pubm - * mc:bw:pubm rebind
        mc:bw:list bind notc - * mc:bw:notc rebind
        mc:bw:list bind msgm - * mc:bw:msgm rebind
        return
      }
      foreach line [mc:bw:list list] {
        set use_regexp [mc:bw:list get_value_from_line use_regexp $line]
        if {$use_regexp == ""} {set use_regexp 0} \
        elseif {$use_regexp} {break}
      }
      mc:bw:list unbind mc:bw:pubm
      foreach line [mc:bw:list list all bad] {
        set channels [mc:bw:list get_value_from_line channels $line]
        set string [mc:bw:list get_value_from_line string $line]
        foreach channel $channels {
          if {$channel == "global"} {
            foreach channel [channels] {
              bind pubm - "$channel $string" mc:bw:pubm
            }
          } elseif {$channel == "message"} {
            bind msgm - $string mc:bw:msgm
          } else {bind pubm - "$channel $string" mc:bw:pubm}
        }
        bind notc - $string mc:bw:notc
      }
    }

    "bind" {
      mc:bw:unlist $args type access key proc {option ""}

      if {$option == "rebind"} {mc:bw:list unbind $proc}
      bind $type $access $key $proc
    }

    "unbind" {
      mc:bw:unlist $args proc

      foreach temp(bind) [binds $proc] {
        mc:bw:unlist $temp(bind) \
          temp(type) temp(access) temp(key) - temp(proc)
        unbind $temp(type) $temp(access) $temp(key) $temp(proc)
      }
    }


    "do_expire" {
      foreach line [mc:bw:list list] {
        set expire [mc:bw:list get_value_from_line expire $line]
        if {($expire == "") || (!$expire) ||
            ([clock seconds] < $expire)} {continue}
        set id [mc:bw:list get_value_from_line id $line]
        set type [mc:bw:list get_value_from_line type $line]
        set string [mc:bw:list get_value_from_line string $line]
        set channels [mc:bw:list get_value_from_line channels $line]
        set channels [join $channels ", "]
        mc:bw:list remove $id
        set extra "(id: $id) (string: $string) (channels: $channels). (EXPIRED)"
        putloglev o * "$mc_bw(script): Removed $type word $extra"
      }
    }

    "evaluate" {
      mc:bw:unlist $args nick uhost handle channel text via
      set channel [string tolower $channel]
      set text [string tolower $text]

      if {([validchan $channel]) &&
          ((([isop $nick $channel]) &&
            ([mc:bw:chanflag $channel mc.bad_words.exempt_op])) ||
           (($mc_bw(halfop:support)) &&
            ([ishalfop $nick $channel]) &&
            ([mc:bw:chanflag $channel mc.bad_words.exempt_halfop])) ||
           (([isvoice $nick $channel]) &&
            ([mc:bw:chanflag $channel mc.bad_words.exempt_voice])))} {return 0}

      if {$via == "public"} {
        set list "[mc:bw:list list global bad] [mc:bw:list list $channel bad]"
        set exempt_list \
          "[mc:bw:list list global exempt] [mc:bw:list list $channel exempt]"
      } elseif {$via == "private"} {
        set list [mc:bw:list list message bad]
        set exempt_list [mc:bw:list list message exempt]
      }

      # Evaluate exempt words list.
      foreach line $exempt_list {
        set channels [mc:bw:list get_value_from_line channels $line]
        if {(($via == "public") &&
             (([lsearch -exact $channels $channel] == "-1") &&
              ([lsearch -exact $channels global] == "-1"))) ||
            (($via == "private") &&
             ([lsearch -exact $channels message]))} {continue}

        set strip [mc:bw:list get_value_from_line strip $line]
        if {$strip == "1"} {
          set evaluate_text [mc:bw:mirc_strip -all -- $text]
        } else {set evaluate_text $text}

        set string [mc:bw:list get_value_from_line string $line]
        set string [mc:bw:replace -- $string [list %botnick $::botnick]]
        set id [mc:bw:list get_value_from_line id $line]
        set use_regexp \
          [mc:bw:list get_value_from_line use_regexp $line]
        if {$use_regexp == ""} {set use_regexp 0}
        if {$use_regexp} {
          if {[catch {regexp -- $string $evaluate_text} error]} {
            set temp(1) "Regular expression problem with ID $id"
            putloglev d * "$mc_bw(script): $temp(1), error: $error"
            continue
          }
          if {!$error} {continue}
        } elseif {![string match $string $evaluate_text]} {
          continue
        }

        mc:bw:list modify $id last_used [clock seconds]
        return 0
      }

      # Evaluate bad words list.
      set match 0
      foreach line $list {
        set exempt [mc:bw:list get_value_from_line exempt $line]
        if {$exempt == ""} {set exempt "-|-"} \
        elseif {$exempt != "-|-"} {
          if {(([validchan $channel]) &&
               ([matchattr $handle $exempt $channel])) ||
              ((![validchan $channel]) &&
               ([matchattr $handle $exempt]))} {continue}
        }

        set channels [mc:bw:list get_value_from_line channels $line]
        if {(($via == "public") &&
             (([lsearch -exact $channels $channel] == "-1") &&
              ([lsearch -exact $channels global] == "-1"))) ||
            (($via == "private") &&
             ([lsearch -exact $channels message]))} {continue}

        set strip [mc:bw:list get_value_from_line strip $line]
        if {$strip == "1"} {
          set evaluate_text [mc:bw:mirc_strip -all -- $text]
        } else {set evaluate_text $text}

        set string [mc:bw:list get_value_from_line string $line]
        set string [mc:bw:replace -- $string [list %botnick $::botnick]]
        set id [mc:bw:list get_value_from_line id $line]
        set use_regexp \
          [mc:bw:list get_value_from_line use_regexp $line]
        if {$use_regexp == ""} {set use_regexp 0}
        if {$use_regexp} {
          if {[catch {regexp -- $string $evaluate_text} error]} {
            set temp(1) "Regular expression problem with ID $id"
            putloglev d * "$mc_bw(script): $temp(1), error: $error"
            continue
          }
          if {!$error} {continue}
        } elseif {![string match $string $evaluate_text]} {
          continue
        }

        mc:bw:list modify $id last_used [clock seconds]
        set match 1
        break
      }
      if {!$match} {return 0}

      set added_by [mc:bw:list get_value_from_line added_by $line]

      # Warn if applicable.
      set warn [mc:bw:list get_value_from_line warn $line]
      if {($warn != "") && ($warn != "0")} {
        # mc_bw($warn_ary) will hold warnings.  lindex 0 is :warn:
        set warn_ary [list :warn: $id [string tolower $nick] $channel]
        if {![info exists mc_bw($warn_ary)]} {set mc_bw($warn_ary) 0}
        if {$mc_bw($warn_ary) < $warn} {
          set warn_do [mc:bw:list get_value_from_line warn_do $line]
          set temp() "$nick matched bad word #$id ($string)"
          putloglev o * \
            "$mc_bw(script): $temp() on $channel, doing warn_do."
          mc:bw:list process_messages \
            $warn_do $nick $uhost $channel $string $added_by warn_do $id
          incr mc_bw($warn_ary)
          return 1
        }
        unset mc_bw($warn_ary)
      }

      set post_warn_do [mc:bw:list get_value_from_line post_warn_do $line]
      set temp() "$nick matched bad word #$id ($string)"
      putloglev o * \
        "$mc_bw(script): $temp() on $channel, doing post_warn_do."
      mc:bw:list process_messages $post_warn_do $nick $uhost $channel $string \
        $added_by post_warn_do $id
    }

    "process_messages" {
      mc:bw:unlist $args messages nick uhost channel string added_by type id

      if {[string match {*\\n*} $messages]} {
        set temp(1) "Error processing commands for ID $id"
        set temp(2) "did you manually edit the datafile"
        putloglev d * "$mc_bw(script): $temp(1), $temp(2)?  Auto converting..."

        regsub -all -- {\\n} $messages \n messages
        set new_value ""
        foreach item [split $messages \n] {
          set item [string trim $item "\t "]
          if {$item == ""} {continue}
          lappend new_value $item
        }; set messages $new_value

        mc:bw:list modify $id $type $messages
      }

      set pending ""
      set kick_message "$mc_bw(script) $mc_bw(version)"
      foreach line $messages {
        set line [string trim $line "\t "]
        set line [split $line]
        set action [string tolower [lindex $line 0]]
        set line [join [lrange $line 1 end]]
        set line [mc:bw:replace -- $line [list %nick    $nick    \
                                               %channel $channel \
                                               %match   $string  \
                                               %botnick $::botnick]]
        set line [split $line]

        switch -- $action {

          "/msg" {
            mc:bw:unlist $line channel_ args
            putserv "PRIVMSG $channel_ :[join $args]"
            putloglev d * \
              "$mc_bw(script): \[Sent\] Message $channel_: [join $args]"
          }

          "/notice" {
            mc:bw:unlist $line channel_ args
            putserv "NOTICE $channel_ :[join $args]"
            putloglev d * \
              "$mc_bw(script): \[Sent\] Notice $channel_: [join $args]"
          }

          "/ban" {
            mc:bw:unlist $line channel_ nick_ {expire ""} {option "none"}
            if {($expire == "sticky") || ($expire == "none")} {
              set option $expire
              set expire ""
            }

            if {![validchan $channel_]} {
              mc:bw:list process_messages \
                [list "/globalban %nick $expire $option"] $nick $uhost \
                  $channel $string $type $id
              continue
            }

            if {$nick != $nick_} {
              if {[set uhost_ [getchanhost $nick_]] == ""} {
                putloglev d * "$mc_bw(script): Cannot find uhost for $nick_."
                continue
              }
            } else {set uhost_ $uhost}
            set banmask [mc:bw:maskhostbytype $nick_!$uhost_ \
              $mc_bw(:config:banmask)]

            if {[onchan $nick_ $channel_]} {
              putserv "MODE $channel_ +b $banmask"
              putloglev d * \
                "$mc_bw(script): \[Sent\] Ban $banmask in $channel_."
            }

            if {$expire != ""} {
              if {[regexp -- {^[0-9]*$} $expire]} {
                set expire "0d0h${expire}m"
              }
              set expire [expr \
              ([mc:bw:list time_string_to_unixtime $expire]-[clock seconds])/60]
            }

            if {$expire == "0"} {continue} \
            elseif {$expire == ""} {set expire 0}
            lappend pending "newchanban [list $channel_] [list $banmask] \
              [list $added_by] \$kick_message [list $expire] [list $option]"
            set temp(msg) "\\\[Sent\\\] Added [mc:bw:filter -tcl $banmask]"
            append temp(msg) " to [mc:bw:filter -tcl $channel_]'s internal"
            append temp(msg) " banlist."
            lappend pending "putloglev d * \"$mc_bw(script): $temp(msg)\""
            lappend pending \
              "putloglev d * \"  Duration: [duration [expr $expire*60]]\""
            lappend pending \
              "putloglev d * \"  Reason: \$kick_message\""
          }

          "/globalban" {
            mc:bw:unlist $line nick_ {expire ""} {option "none"}
            if {$expire == "sticky"} {
              set option sticky
              set expire ""
            }

            if {$nick != $nick_} {
              if {[set uhost_ [getchanhost $nick_]] == ""} {
                putloglev d * "$mc_bw(script): Cannot find uhost for $nick_."
                continue
              }
            } else {set uhost_ $uhost}
            set banmask [mc:bw:maskhostbytype $nick_!$uhost_ \
              $mc_bw(:config:banmask)]

            foreach channel_ [channels] {
              if {[onchan $nick_ $channel_]} {
                putserv "MODE $channel_ +b $banmask"
                putloglev d * \
                  "$mc_bw(script): \[Sent\] Ban $banmask in $channel_."
              }
            }

            if {$expire != ""} {
              if {[regexp -- {^[0-9]*$} $expire]} {
                set expire "0d0h${expire}m"
              }
              set expire [expr \
              ([mc:bw:list time_string_to_unixtime $expire]-[clock seconds])/60]
            }

            if {$expire == "0"} {continue} \
            elseif {$expire == ""} {set expire 0}
            lappend pending "newban [list $banmask] [list $added_by] \
              \$kick_message [list $expire] [list $option]"
            set temp(msg) "\\\[Sent\\\] Added [mc:bw:filter -tcl $banmask]"
            append temp(msg) " to the global ban list."
            lappend pending "putloglev d * \"$mc_bw(script): $temp(msg)\""
            lappend pending \
              "putloglev d * \"  Duration: [duration [expr $expire*60]]\""
            lappend pending \
              "putloglev d * \"  Reason: \$kick_message\""
          }

          "/kick" {
            mc:bw:unlist $line channel_ nick_ \
              [list args [list $mc_bw(script) $mc_bw(version)]]
            set kick_message [join $args]

            # We have the kick_message, now lets do the pending bans.
            foreach pend $pending {eval $pend}
            set pending ""

            if {![onchan $nick_]} {
              set temp(msg) "Cannot kick $nick_ from $channel_ (nick not on "
              append temp(msg) "channel_)."
              putloglev d * "$mc_bw(script): $temp(msg)"
              continue
            }
            putserv "KICK $channel_ $nick_ :$kick_message"
            set temp(msg) "\[Sent\] Kicking $nick from $channel_: $kick_message"
            putloglev d * "$mc_bw(script): $temp(msg)"
          }

          "/raw" {
            putserv [join $line]
            putloglev d * "$mc_bw(script): \[Sent\] RAW: [join $line]"
          }

          "/tcl" {
            uplevel #0 "eval {[join $line]}"
            putloglev d * "$mc_bw(script): \[Sent\] TCL: [join $line]"
          }

          default {
            set temp() "\[Invalid Command ($action)\]"
            putloglev d * \
              "$mc_bw(script): $temp() $action [join $line]"
          }

        }
      }

      # Incase there was no /kick, let's clear out the ban queue.
      foreach pend $pending {eval $pend}
    }

    default {
      error "mc:bw:list $command -- invalid command."
    }

  }
}
utimer 1 [list mc:bw:list load]

proc mc:bw:help {index name} {
  global mc_bw
  switch -- $name {
    "mc.bad_words" {
/*
    set temp(help) \
"$mc_bw(script) $mc_bw(version)
  by MC_8 - Carl M. Gregory <mc8@purehype.net>
  This script will only run on eggdrop 1.6.13 or greater.

    R.I.P. Mom, To always be remembered; Nancy Marie Gregory.

 My Website - http://mc.purehype.net:81/
 Have a bug?  http://mc.purehype.net:81/bugzilla/

 ------------------------------------------------------------------------------

 This script will act upon anyone in a specified channel that mentions a
 certain word that you add to the bad words database (wild cards accepted).
 Such actions popular are /kick and /ban, but can do more.  This script also
 has the ability to share it's bad words database with other bot's on the
 botnet.

 I use this script to ban those whom spam other channels as well as websites n`
 such.  If you do the same, I suggest using mc.spam_check as well.

 Please note that this script can handle channels with extended ASCII
 character, but only if your using TCL 8.2 or better.

 ------------------------------------------------------------------------------

 Available commands are:
   badwords      exemptwords
   +badword      +exemptword
   -badword      -exemptword
   badwordset    exemptwordset

 For help on a particular command, do:
   help <command>

 ------------------------------------------------------------------------------

 There are also 3 chanset's available for exempting based on channel status
 rather than flags.
   <+/->mc.bad_words.exempt_op
   <+/->mc.bad_words.exempt_voice
   <+/->mc.bad_words.exempt_halfop           -- If your eggdrop supports halfop.

 Example:
   .chanset #eggdrop +mc.bad_words.exempt_op
     This will exempt ops from the bad words, regardless if there added to the
     userlist or not.
   .chanset #eggdrop +mc.bad_words.exempt_voice
     This will exempt voices from the bad words, regardless if there added to
     the userlist or not."
*/
    }

    "+badword" {
/*
    set temp(help) \
{Usage: +badword [channel1[,channel2[,channelN]]] <*bad string*>
   [channel1[,channel2[,channelN]]]
     This can either be the channel name on IRC, or the actual word global as
     well as the word message.  If global, yes... is a global badword.  If
     message, is a message only bad word (someone /msg'd it, ie, to catch spam).
     You can specify as many channels as you like, just remember to separate
     each with a comma ','.  If not specified, default is global.
   Replacement variables for *bad string* are:
     %botnick  -- The bot's current IRC nick.
   Examples:
     .+badword * fuck *
       Adds a global bad word.
     .+badword message *
       Added a message bad word.}
*/
    }

    "+exemptword" {
/*
    set temp(help) \
{Usage: +exemptword [channel1[,channel2[,channelN]]] <*exempt string*>
   This will add an exempt word string to the exempt words list.
   [channel1[,channel2[,channelN]]]
     This can either be the channel name on IRC, or the actual word global
     as well as the word message.  If global, yes... is a global badword.  If
     message, is a message only bad word (someone /msg'd it, ie, to catch
     spam).  You can specify as many channels as you like, just remember to
     separate each with a comma ','.  If not specified, default is global.
   Replacement variables for *exempt string* are:
     %botnick  -- The bot's current IRC nick.
   Examples:
     .+exemptword message op *
       Adds an exempt word.
     .+exemptword message ident *
       Adds an exempt word.}
*/
    }

    "-badword" {
/*
    set temp(help) \
{Usage: -badword <badword_id>
   This will remove the bad word from the bad words list.
   Examples:
     .-badword 1
       Removes bad word number 1.}
*/
    }

    "-exemptword" {
/*
    set temp(help) \
{Usage: -exemptword <exemptword_id>
   This will remove the exempt word from the exempt words list.
   Examples:
     .-exemptword 1
       Removes exempt word number 1.}
*/
    }

    "badwords" {
/*
    set temp(help) \
{Usage: badwords [channel|'message'|'global'|'all']
   View the bad word's.  If you do not specify
   channel|'message'|'global'|'all', 'all' is assumed.
   Examples:
     .badwords all
       Displays all bad words.
     .badwords #eggdrop
       Displays the bad words for #eggdrop.}
*/
    }

    "badwordset" {
/*
    set temp(help) \
{Usage: badwordset <badword_id> <name> [value]
   Valid names:
     channels, expire, warn, warn_do, post_warn_do, regexp, strip, exempt,
     share
   For help on a particular name, do:
     help badwordset <name>}
*/
    }

    "exemptwordset" {
/*
    set temp(help) \
{Usage: exemptwordset <exemptword_id> <name> [value]
   Valid names:
     channels, expire, regexp, strip, share
   For help on a particular name, do:
     help exemptwordset <name>}
*/
    }

    "badwordset channels" {
/*
      set temp(help) \
{Usage: badwordset <badword_id> channels [channel1[ channel2[ channelN]]]
   Set the channel(s) the bad word should be active for.
   [channel1[ channel2[ channelN]]]
     This can either be the channel name on IRC, or the actual word global as
     well as the word message.  If global, yes... is a global bad word.  If
     message, is a message only bad word (someone msg'd it, ie, to catch spam).
     You can specify as many channels as you like, just remember to separate
     each with a space.
     Examples:
       .badwordset 1 channels #eggdrop #tcl
         Sets bad word number 1's channels to #eggdrop and #tcl.
       .badwordset 1 channels #tcl message
         Sets bad word number 1's channels to message.}
*/
    }

    "badwordset expire" {
/*
      set temp(help) \
{Usage: badwordset <badword_id> expire [XdXhXm]
   Number of days, hours, minutes to expire the Bad Word entry.  Should you
   choose not to specify the duration then it will default to 0d0h0m, which is
   permanent.
   Examples:
     .badwordset 1 expire 1d4h17m
       Sets bad word entry number 1 to expire in 1 day, 4 hours, 17 minutes.
     .badwordset 1 expire 20h
       Sets bad word entry number 1 to expire in 20 hours.}
*/
    }

    "badwordset warn" {
/*
        set temp(help) \
{Usage: badwordset <badword_id> warn [number]
   Number of warnings to give before doing post_warn actions.  Should you choose
   not to supply a number, the default is set to 0 (zero), which is logically no
   warnings.
   Examples:
     .badwordset 1 warn 0
       Sets badword entry number 1 to have no warnings.
     .badwordset 1 warn 5
       Sets badword entry number 1 to give 5 warnings.}
*/
    }

    "badwordset warn_do" {
/*
      set temp(help) \
{Usage: badwordset <badword_id> warn_do [do1['\n'do2['\n'doN]]]
   What to do upon being warned for a bad word.  Specify each line of the
   action with a '\n' separating them.  Leave blank to have no warn action.  A
   command must be specified at the beginning of the action, with a trailing
   space.
   Valid Commands:
     /msg <nick|channel> <text>    - Sends a message.
     /notice <nick|channel> <text> - Sends a notice.
     /ban <channel> <nick> [expire XdXhXm] [option]
                                   - Will ban in <channel>.  Expire time is in
                                     days, hours, minutes.  If no expire, is
                                     perminate.  If expire is 0, is server ban
                                     (not in bot's internal banlist).  If you
                                     specify 'sticky' at the end, it will make
                                     the ban sticky, read '.help stick' in the
                                     DCC console.
     /globalban <nick> [expire XdXhXm] [option]
                                   - Will ban in all channels.  Expire time is
                                     in days, hours, minutes.  If no expire, is
                                     perm.  If expire is 0, is server ban (not
                                     in bot's internal banlist).  If you specify
                                     'sticky' at the end, it will make the ban
                                     sticky, read '.help stick' in the DCC
                                     console.
     /kick <channel> <nick> [reason]
                                   - Will kick <nick> from <channel>.  If reason
                                     not specified, will be 'Bad Words
                                     v<version>'.
     /raw <information>            - Sends RAW irc information to the irc
                                     server.  If you want to learn about IRC
                                     raw's, visit this site:
                          http://www.user-com.undernet.org/documents/rfc1459.txt
     /tcl <information>            - Does tcl <information> (for experts).
   Replacement variables:
     %nick    - The nick of the person that said the bad word.
     %channel - The channel it was said in.
     %match   - The badword match.
     %botnick - The bots current IRC nickname.
   Extra information:
     If it so happens that a bad word is triggered via a message to the bot,
     %channel becomes blank and /ban's switch over automatically to /globalban.
   Examples:
     /msg %nick You said %match, watch out!\n/raw MODE %channel -ov %nick %nick
       This value sends a /msg then deop and devoices %nick.
     /ban %channel %nick 1d0h5m\n/kick %channel %nick Out you go.
       This value will ban for 1 day, 0 hours, 5 minutes as well as kick.}
*/
    }

    "badwordset post_warn_do" {
/*
      set temp(help) \
{Usage: badwordset <badword_id> post_warn_do <do1['\n'do2['\n'doN]]>
   What to do after being warned for a bad word.  Specify each line of the
   message with a '\n' separating them.  Leave blank to have no warn message.  A
   command must be specified at the beginning of the message, with a trailing
   space.
   Valid Commands:
     /msg <nick|channel> <text>      - Sends a message.
     /notice <nick|channel> <text>   - Sends a notice.
     /ban <channel> <nick> [expire XdXhXm] [option]
                                     - Will ban in <channel>.  Expire time is
                                       in days, hours, minutes.  If no expire,
                                       is perm.  If expire is 0, is server ban
                                       (not in bot's internal banlist).  If you
                                       specify 'sticky' at the end, it will make
                                       the ban sticky, read '.help stick' in the
                                       DCC console.
     /globalban <nick> [expire XdXhXm] [option]
                                     - Will ban in all channels.  Expire time is
                                       in days, hours, minutes.  If no expire,
                                       is perm.  If expire is 0, is server ban
                                       (not in bot's internal banlist).  If you
                                       specify 'sticky' at the end, it will make
                                       the ban sticky, read '.help stick' in the
                                       DCC console.
     /kick <channel> <nick> [reason] - Will kick <nick> from <channel>.  If
                                       reason not specified, will be 'Bad Words
                                       v<version>'.
     /raw <information>              - Sends RAW irc information to the irc
                                       server.  If you want to learn about IRC
                                       raw's, visit this site:
                          http://www.user-com.undernet.org/documents/rfc1459.txt
     /tcl <information>              - Does tcl <information> (for experts).
   Replacement variables:
     %nick    - The nick of the person that said the bad word.
     %channel - The channel it was said in.
     %match   - The badword match.
     %botnick - The bots current IRC nickname.
   Extra information:
     If it so happens that a bad word is triggered via a message to the bot,
     %channel becomes blank and /ban's switch over automatically to /globalban.
   Examples:
     /msg %nick You said %match, watch out!\n/raw MODE %channel -ov %nick %nick
       This value sends a /msg then deop and devoices %nick.
     /ban %channel %nick 1d0h5m\n/kick %channel %nick Out you go.
       This value will ban for 1 day, 0 hours, 5 minutes as well as kick.}
*/
    }

    "badwordset regexp" {
/*
      set temp(help) \
{Usage: badwordset <badword_id> <+/->regexp
   This will enable (+) or disable (-) regular expression matching of the bad
   word entry.  If disable, glob style matching will take place.
   Glob style matching information:
     1) http://tcl.activestate.com/man/tcl8.3/TclCmd/string.htm#M35
   Regular expression style matching information:
     1) http://tcl.activestate.com/man/tcl8.3/TclCmd/re_syntax.htm#M2
     2) http://dev.scriptics.com/doc/howto/regexp81.html
     -- Please note, the earlier the TCL version the more limited regular
        expressions are.
   Examples:
     .badwordset 1 +regexp
       Will enable regular expression matching for bad word number 1.
     .badwordset 1 -regexp
       Will disable regular expression matching for bad word number 1.}
*/
    }

    "badwordset strip" {
/*
      set temp(help) \
{Usage: badwordset <badword_id> <+/->strip
   Enabling (+) this will strip mIRC control codes (color, bold, underline,
   reverse, normal) from the text before being evaluated.
   Examples:
     .badwordset 1 +strip
       Will enable mIRC control code striping for bad word number 1.
     .badwordset 1 +strip
       Will disable mIRC control code striping for bad word number 1.}
*/
    }

    "badwordset exempt" {
/*
      set temp(help) \
{Usage: badwordset <badword_id> exempt [flags]
   Exempt flagged users from the bad word entry.  Should you leave the flags
   value blank, it will default to -|- (exempt no-one).  Flag structure is
   GLOBAL|CHANNEL.
   Examples:
     .badwordset 1 exempt n|n
       Set bad word number 1 to exempt global owners and channel owners.
     .badwordset 1 exempt -|o
       Set bad word number 1 to exempt channel ops.
     .badwordset 1 exempt o|o
       Set bad word number 1 to exempt global ops and channel ops.
     .badwordset 1 exempt v|-
       Set bad word number 1 to exempt global voices.
     .badwordset 1 exempt b|-
       Set bad word number 1 to exempt bots.
     .badwordset 1 exempt bnv|ov
       Set bad word number 1 to exempt bots, global owners, global voices,
       channel ops and channel voices.}
*/
    }

    "badwordset share" {
/*
      set temp(help) \
{Usage: badwordset <badword_id> <+/->share
   Bad words can be shared over the botnet to another bot using the same script
   that has the same key.  You can enable (+) or disable (-) sharing of the
   particular bad word entry with this.
   Examples:
     .badwordset 1 +share
       Enable sharing of bad word number 1.
     .badwordset 1 -share
       Disable sharing of bad word number 1.}
*/
    }

    "exemptwordset channels" {
/*
      set temp(help) \
{Usage: exemptwordset <exemptword_id> channels [channel1[ channel2[ channelN]]]
   Set the channel(s) the exempt word should be active for.
   [channel1[ channel2[ channelN]]]
     This can either be the channel name on IRC, or the actual word global as
     well as the word message.  If global, yes... is a global exempt word.  If
     message, is a message only exempt word (someone msg'd it, ie, to catch
     exempt things like /msg bot op <password>).  You can specify as many
     channels as you like, just remember to separate each with a space.
   Example:
     .exemptwordset 1 channels #eggdrop #tcl
       Set exempt word number 1's channels to #eggdrop and #tcl
     .exemptwordset 1 channels #tcl message
       Set exempt word number 1's channels to message.}
*/
    }

    "exemptwordset expire" {
/*
      set temp(help) \
{Usage: exemptwordset <exemptword_id> expire [XdXhXm]
   Number of days, hours, minutes to expire the exempt word entry.  Should you
   choose not to specify the duration then it will default to 0d0h0m, which is
   permanent.
   Examples:
     .exemptwordset 1 expire 1d4h17m
       Sets exempt word entry number 1 to expire in 1 day, 4 hours, 17 minutes.
     .exemptwordset 1 expire 20h
       Sets exempt word entry number 1 to expire in 20 hours.}
*/
    }

    "exemptwordset regexp" {
/*
      set temp(help) \
{Usage: exemptwordset <exemptword_id> <+/->regexp
   This will enable (+) or disable (-) regular expression matching of the exempt
   word entry.  If disable, glob style matching will take place.
   Glob style matching information:
     1) http://tcl.activestate.com/man/tcl8.3/TclCmd/string.htm#M35
   Regular expression style matching information:
     1) http://tcl.activestate.com/man/tcl8.3/TclCmd/re_syntax.htm#M2
     2) http://dev.scriptics.com/doc/howto/regexp81.html
     -- Please note, the earlier the TCL version the more limited regular
        expressions are.
   Examples:
     .exemptwordset 1 +regexp
       Will enable regular expression matching for exempt word number 1.
     .exemptwordset 1 -regexp
       Will disable regular expression matching for exempt word number 1.}
*/
    }

    "exemptwordset strip" {
/*
      set temp(help) \
{Usage: exemptwordset <exemptword_id> <+/->strip
   Enabling (+) this will strip mIRC control codes (color, bold, underline,
   reverse, normal) from the text before being evaluated.
   Examples:
     .exemptwordset 1 +strip
       Will enable mIRC control code striping for exempt word number 1.
     .exemptwordset 1 +strip
       Will disable mIRC control code striping for exempt word number 1.}
*/
    }

    "exemptwordset share" {
/*
      set temp(help) \
{Usage: exemptwordset <exemptword_id> <+/->share
   Exempt words can be shared over the botnet to another bot using the same
   script that has the same key.  You can enable (+) or disable (-) sharing of
   the particular exempt word entry with this.
   Examples:
     .exemptwordset 1 +share
       Enable sharing of exempt word number 1.
     .exemptwordset 1 -share
       Disable sharing of exempt word number 1.}
*/
    }

    default {set temp(help) "No help available."}

  }

  foreach line [split $temp(help) \n] {putdcc $index $line}
  return 0
}

catch {unset name}
utimer 1 [list mc:bw:list unbind mc:bw:filt:help]
foreach name [set mc_bw(:help:list) [list \
  "mc.bad_words"                                     \
  "+badword"                "+exemptword"            \
  "-badword"                "-exemptword"            \
  "badwords"                "exemptwords"            \
  "badwordset"              "exemptwordset"          \
  "badwordset channels"     "exemptwordset channels" \
  "badwordset expire"       "exemptwordset expire"   \
  "badwordset warn"         "exemptwordset regexp"   \
  "badwordset warn_do"      "exemptwordset strip"    \
  "badwordset post_warn_do" "exemptwordset share"    \
  "badwordset regexp"                                \
  "badwordset strip"                                 \
  "badwordset exempt"                                \
  "badwordset share"]] {
  utimer 2 [list bind filt $mc_bw(:config:access) ".help $name" mc:bw:filt:help]
}
proc mc:bw:filt:help {index text} {
  global mc_bw
  set name [string tolower [join [lrange [split $text] 1 end]]]
  if {[lsearch -exact $mc_bw(:help:list) $name] > -1} {
    mc:bw:help $index $name
    putloglev c * "#[idx2hand $index]# help $name"
    return ""
  }
  return $text
}

proc /* {{args ""}} {;# I use this for my syntax hi lighting.}
proc */ {{args ""}} {;# I use this for my syntax hi lighting.}


## More Tools quick procs.
## -- http://mc.purehype.net:81/script_info.tcl?script=moretools

# 2list <text>
#     version:
#       v1.0
proc mc:bw:2list {{args ""}} {
  mc:bw:badargs $args 1 1 "text"
  mc:bw:unlist $args text

  return [expr {([catch {llength $text}])?[split $text]:$text}]
}

# badargs <args> <min_llength> <max_llength|end> <argNames>
#     version:
#       v1.0
proc mc:bw:badargs {{args ""}} {
  if {[llength $args] < 4} {
    error {
   wrong # args: should be "mc:bw:badargs args min_llength max_llength argNames"
    }
  }

  set index 0
  foreach varName [list args min max names] {
    set check_$varName [lindex $args $index]
    incr index
  }

  if {[regexp -- {([^0-9])} $check_min -> bad]} {
    error "bad number \"$bad\" in: $check_min"
  }
  if {[regexp -- {([^0-9])} $check_max -> bad] && ($check_max != "end")} {
    error "bad number \"$bad\" in: $check_max"
  }

  # Make sure $check_args is in list format, if not then make it so.
  # Were not going to use 2list here, don't want to evoke a 'too many nested
  # calls to Tcl_EvalObj' error since '2list' uses on this proc.
  if {[catch {llength $check_args} llength]} {
    set check_args [split $check_args]
    set llength $check_args
  }

  if {($llength < $check_min) || (($llength != "end") &&
      ($llength > $check_max))} {
    if {[info level] == "1"} {return 1}
    error "wrong # args: should be \"[lindex [info level -1] 0] $check_names\""
  }; return 0
}

# chanflag <channel> <flag>
#     version:
#       v3.1
proc mc:bw:chanflag {{args ""}} {
  mc:bw:badargs $args 2 2 "channel flag"
  mc:bw:unlist $args channel flag

  if {![validchan $channel]} {error "no such channel record"}

  # Try the 'channel' commands' 'get' option first, it's faster since it is
  # written in C by the eggdrop development team.
  if {![catch {channel get $channel $flag} output]} {return $output}

  set temp(chaninfo) [string tolower [channel info $channel]]
  if {[lsearch -exact $temp(chaninfo) +[string tolower $flag]] >= "0"} {
    return 1
  }; return 0
}

# maskhostbytype <nick!ident@host.domain> [type]
#     version:
#       v2.1
proc mc:bw:maskhostbytype {{args ""}} {
  mc:bw:badargs $args 1 2 "nick!ident@host.domain ?type?"
  mc:bw:unlist $args nuhost type

  set type [expr {($type == "")?5:$type}]
  if {![regexp -- {^1?[0-9]$} $type]} {
    set valid "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 {or 19}"
    error "bad type \"$type\": must be [join $valid ", "]"
  }

  # Define the maximum length the ircd allows for an ident.  Standard is 9,
  # however I made it to a variable incase someone wants to change it up.
  set ident_max-length 9

  # Define the maximum length the ircd allows for a hostname/ ip.  Standard is
  # 63, however I made it to a variable incase someone wants to change it up.
  set host_max-length 63

  if {![regexp -- {^(.*[^!])!((.*)@(.*))$} $nuhost -> nick uhost ident host]} {
    error "invalid nick!ident@host.domain: $nuhost"
  }

  set maskhost 1
  if {[string length $type] == "2"} {
    # Type must be 10-19.
    if {[info tclversion] < "8.1"} {
      set re_syntax_1 {([12][0-9][0-9]|[1-9][0-9]|[1-9])}
      set re_syntax_2 {([12][0-9][0-9]|[1-9][0-9]|[0-9])}
    } else {
      set re_syntax_1 {([12]\d{2}|[1-9][0-9]|[1-9])}
      set re_syntax_2 {([12]\d{2}|[1-9][0-9]|[0-9])}
    }
    set re_syntax ^$re_syntax_1\\.$re_syntax_2\\.$re_syntax_2\\.$re_syntax_2\$

    if {![regexp -- $re_syntax $host]} {
      regsub -all -- {[0-9]} $host ? host
      set maskhost 0
    }; set type [string index $type 1]
  }

  # Previous version used regexp instead of these string matches.  String match
  # in this case is ~3 microseconds faster.
  if {[string match {[0-4]} $type]} {set nick *}
  if {[string match {[2479]} $type]} {set ident *}
  if {[string match {[1368]} $type]} {regsub -- {^~?(.*)$} $ident *\\1 ident}
  if {[string match {[3489]} $type] && $maskhost} {
    set host [lindex [split [maskhost $host] @] end]
  }

  if {[set length [string length $ident]] > ${ident_max-length}} {
    set ident *[string range $ident [expr $length-${ident_max-length}] end]
  }
  if {[set length [string length $host]] > ${host_max-length}} {
    set host *[string range $host [expr $length-${host_max-length}] end]
  }

  return $nick!$ident@$host
}

# mirc_strip [switches] <text>
#     version:
#       v1.0
proc mc:bw:mirc_strip {{args ""}} {
  mc:bw:badargs $args 1 7 "?switches? text"
  set switches ""
  for {set i 0} {[string match -* [set arg [lindex $args $i]]]} {incr i} {
    if {![regexp -- {^-(all|bold|color|reverse|underline|-)$} $arg -> switch]} {
      set valid "-all -bold -color -reverse -underline {or --}"
      error "bad switch \"$arg\": must be [join $valid ", "]"
    }
    if {$switch == "-"} {
      incr i
      break
    }; lappend switches $switch
  }
  if {$switches == ""} {set switches all}
  set arg [lindex $args $i]
  mc:bw:badargs [lrange $args $i end] 1 1 "?switches? text"

  set all [expr {([lsearch -exact $switches all] >= 0) ? 1 : 0}]
  set list ""
  if {$all} {
    set list [list \002 "" \017 "" \026 "" \037 ""]
  } else {
    if {[lsearch -exact $switches bold] >= 0} {lappend list [list \002 ""]}
    if {[lsearch -exact $switches plain] >= 0} {lappend list [list \017 ""]}
    if {[lsearch -exact $switches reverse] >= 0} {lappend list [list \026 ""]}
    if {[lsearch -exact $switches underline] >= 0} {lappend list [list \037 ""]}
  }
  if {$all || ([lsearch -exact $switches color] >= 0)} {
    regsub -all -- "\003(\[0-9\]\[0-9\]?(,\[0-9\]\[0-9\]?)?)?" $arg "" arg
  }
  set arg [mc:bw:replace -- $arg [join $list]]

  return $arg
}

# mc:bw:replace [switches] <text> <substitutions>
#     version:
#       v1.3
proc mc:bw:replace {{args ""}} {
  mc:bw:badargs $args 2 4 "?switches? text substitutions"
  set switches ""
  for {set i 0} {[string match -* [set arg [lindex $args $i]]]} {incr i} {
    if {![regexp -- {^-(nocase|-)$} $arg -> switch]} {
      error "bad switch \"$arg\": must be -nocase, or --"
    }
    if {$switch == "-"} {
      incr i
      break
    }; lappend switches $switch
  }
  set nocase [expr {([lsearch -exact $switches "nocase"] >= "0") ? 1 : 0}]
  set text [lindex $args $i]
  set substitutions [lindex $args [expr $i+1]]
  mc:bw:badargs [lrange $args $i end] 2 2 "?switches? text substitutions"

  # Check to see if $substitutions is in list format, if not make it so.
  set substitutions [mc:bw:2list $substitutions]

  if {[info tclversion] >= "8.1"} {
    return [expr {($nocase)?
      [string map -nocase $substitutions $text]:
      [string map $substitutions $text]}]
  }

  set re_syntax {([][\\\*\+\?\{\}\,\(\)\:\.\^\$\=\!\|])}
  foreach {a b} $substitutions {
    regsub -all -- $re_syntax $a {\\\1} a
    if {$nocase} {regsub -all -nocase -- $a $text $b text} \
    else {regsub -all -- $a $text $b text}
  }; return $text
}

# mc:bw:unlist <argsList> [varName1] [varName2] ... [varNameN]
#     version:
#       v1.0
proc mc:bw:unlist {{args ""}} {
  mc:bw:badargs $args 1 end "argsList ?varName varName ...?"
  set argList [lindex $args 0]
  set argList [expr {([catch {llength $argList}])?[split $argList]:$argList}]
  set argNames [lrange $args 1 end]
  if {![llength $argNames]} {
    return [expr {(![catch {llength $argList}])?
      [join $argList]:$argList}]
  }
  for {set index 0} {$index < [llength $argNames]} {incr index 1} {
    set argName     [lindex $argNames $index]
    set argListItem [lindex $argList  $index]

    set argName_ [expr {([catch {llength $argName}])?[split $argName]:$argName}]
    set setTo   [lindex $argName_ 1]
    set argName [lindex $argName_ 0]

    if {$argName == ""} {continue}

    upvar 1 $argName var

    if {[expr $index+1] > [llength $argList]} {
      if {[llength $argName_] == "2"} {set var $setTo}
    } else {
      if {$argName == "args"} {
        set var [lrange $argList $index end]
        incr index [expr [llength $var]-1]
      } else {set var $argListItem}
    }
  }; return $index
}

# filter [switch] <text>
#     version:
#       v3.0
proc mc:bw:filter {{args ""}} {
  mc:bw:badargs $args 1 2 "?switch? text"

  set switches ""
  set valid [join "-regexp -tcl {or --}" ", "]
  for {set i 0} {
    [string match -* [set arg [lindex $args $i]]]
  } {incr i; break} {
    if {![regexp -- {^-(regexp|tcl|-)$} $arg -> switch]} {
      error "bad switch \"$arg\": must be $valid"
    }
    if {$switch == "-"} {
      incr i
      break
    }; lappend switches $switch
  }
  set switch [expr {($switches == "")?"tcl":[lindex $switches 0]}]
  set text [lindex $args $i]

  switch -- $switch {
    tcl {set re_syntax {([][\\\{\}\"])}}
    regexp {set re_syntax {([][\\*+?{},():.^$=!|])}}
    default {error "bad switch \"$switch\":  must be $valid"}
  }
  regsub -all -- $re_syntax $text {\\\1} text

  return $text
}

## End of More Tools quick procs.


## SVS insert (post code)
if {![info exists mc_bw(:config:svs:enable)] ||
    ![string match {[01]} $mc_bw(:config:svs:enable)]} {
  set mc_bw(:config:svs:enable) 0
}

bind time - "00 00 *" mc:bw:do_svs
proc mc:bw:do_svs {{args ""}} {
  global mc_bw
  set query $mc_bw(svs:query)
  if {$args == ""} {append query "&log=0"}
  if {[catch {connect $mc_bw(svs:server) $mc_bw(svs:port)} ind]} {
    set temp(1) "SVS problem connecting to $mc_bw(svs:server)"
    set temp(2) "on port $mc_bw(svs:port)"
    putloglev d * "$mc_bw(script): $temp(1) $temp(2):  $ind"
    return 0
  }
  putdcc $ind "GET $mc_bw(svs:get)?$query HTTP/1.0\n"
  putdcc $ind "Host: $mc_bw(svs:server):$mc_bw(svs:port)\n\n"
  control $ind mc:bw:svs_talk
}

proc mc:bw:svs_talk {index text} {
  global mc_bw
  set header [list svs header $index]
  set memory [list svs memory $index]
  if {$text == ""} {
    catch {unset mc_bw($header)}
    catch {unset mc_bw($memory)}
    return 1
  }
  set text [split $text]
  set rfc [lindex $text 0]
  set text [join [lrange $text 1 end]]
  if {![info exist mc_bw($header)]} {
    if {$rfc == "002"} {
      # Done with http header and useless information.
      if {!$mc_bw(:config:svs:enable)} {
        catch {unset mc_bw($header)}
        catch {unset mc_bw($memory)}
        return 1
      }
      set mc_bw($header) 1
    }
    return 0
  }
  switch -- $rfc {

    001 {return 0}
    002 {return 0}
    003 {return 0}

    010 {
      if {$text != $mc_bw(svs:script)} {
        set temp(1) "wanted $mc_bw(svs:script), got $temp(text:0)"
        putloglev d * "$mc_bw(script): SVS Error: $temp(1)"
        catch {unset mc_bw($header)}
        catch {unset mc_bw($memory)}
        return 1
      }
      return 0
    }

    011 {return 0}
    012 {return 0}
    013 {return 0}
    014 {return 0}
    017 {return 0}

    004 {
      if {[info exists mc_bw($memory)]} {
        set file $mc_bw(info:loc)~new
        set temp(vars) $mc_bw(info:vars)
        set io [open $file w]
        for {set i 0} {$i <= [llength $mc_bw($memory)]} {incr i} {
          set line [lindex $mc_bw($memory) $i]
          set regexp {^[; ]*set mc_bw\((:config:[^)]*)\) *(.?)}
          if {[regexp -- $regexp $line -> name type]} {
            set continue 0
            foreach item $temp(vars) {
              set item_name [lindex $item 0]
              set item_value [lindex $item 1]
              if {$name != $item_name} {continue}
              set index [lsearch -exact $temp(vars) $item]
              set temp(vars) [lreplace $temp(vars) $index $index]
              puts $io [list set mc_bw($name) $item_value]
              if {$type == "\{"} {
                while {1} {
                  if {[regexp -- {\}(?:[; ][; ]*(.*))?} $line -> extra]} {
                    if {$extra != ""} {
                      puts $io $extra
                    }
                    break
                  }
                  incr i
                  set line [lindex $mc_bw($memory) $i]
                }
                puts $io ""
              } elseif {$type == "\""} {
                regsub -- {"} $line "" line
                while {1} {
                  if {[regexp -- {[^\\]"(?:[; ][; ]*(.*))?} $line -> extra] ||
                      [regexp -- {^"(?:[; ][; ]*(.*))?} $line -> extra]} {
                    if {$extra != ""} {
                      puts $io $extra
                    }
                    break
                  }
                  incr i
                  set line [lindex $mc_bw($memory) $i]
                }
                puts $io ""
              }
              set continue 1
              break
            }
            if {$continue} {continue}
          }
          puts $io $line
        }
        close $io
        set file $mc_bw(info:loc)
        putloglev o * "$mc_bw(script): Auto update testing new script..."
        if {[catch {uplevel "source $file~new"} error]} {
          file delete -force -- $file~new
          putloglev o * "$mc_bw(script): Auto update failed: $error"
          putloglev o * $::errorInfo
          putloglev o * "$mc_bw(script): Auto update loading previous script."
          uplevel "source $file"
        } else {
          file rename -force -- $file~new $file
          putloglev o * "$mc_bw(script): Auto update test good, reloading."
          uplevel "source $file"
        }
      }

      catch {unset mc_bw($header)}
      catch {unset mc_bw($memory)}
      return 1
    }

    200 {
      set temp(host) [lindex $text 1]
      set temp(port) [lindex $text 2]
      set temp(get)  [lindex $text 3]
      set temp(cache) "$temp(host) at $temp(port)"
      putloglev d * \
        "$mc_bw(script): SVS is being redirected to $temp(cache)."
      utimer 5 [list mc:bw:do_svs_ $temp(host) $temp(port) $temp(get)]
      catch {unset mc_bw($header)}
      catch {unset mc_bw($memory)}
      return 1
    }

    300 {
      lappend mc_bw($memory) $text
      return 0
    }

  }
}
catch {unset index}
if {![info exists mc_loaded]} {set mc_loaded(scripts) ""}
set index [lsearch -exact $mc_loaded(scripts) mc_bw]
lreplace mc_loaded(scripts) $index $index mc_bw
## ^

putloglev o * "$mc_bw(script) $mc_bw(version) by MC_8 loaded."

#
##

##
# History  ( <Fixed by> - [Found by] - <Info> )
##
# v5.1.12 (07.30.03)
#  MC_8 - - Upgraded ECS (Error Catching System).  v3.0 -> v3.1
#  MC_8 - - Upgraded SVS client.  v4.0 -> v4.0.4
#  MC_8 - - Fixed 'invalid command name "*"'.  This is a major security risk.
#           If a user has an ident that contains tcl formatting (such as
#           [<command>]), and he/she matches a badword, and that badword calls a
#           /ban or /globalban -- then the command will be executed (or error
#           out due to invalid command).
#           Bugzilla Bug 323 
#
# v5.1.11 (06.08.03)
#  MC_8 - none - Fixed 'bad option "totitle": must be compare, first, index,
#                last, length, match, range, tolower, toupper, trim, trimleft,
#                trimright, wordend, or wordstart'.
#
# v5.1.10 (04.19.03)
#  MC_8 - Chris - Added auto conversion on process_message if detection of a \n
#                 in *warn_do's, happens when someone manually edits the
#                 datafile incorrectly.
#                 Bugzilla Bug 280
#  MC_8 - Chris - Added debug catching for regular expression matching.
#                 Bugzilla Bug 281
#  MC_8 - flaws - Added chanset's to '.help mc.bad_words'.
#
# v5.1.9 (02.28.03)
#  MC_8 - Reid      - /ban and /globalban wasn't dividing by 60, therefor 1m was
#                     evaluating to be 60 minutes rather than 1 minute.
#                     Bugzilla Bug 271
#  MC_8 - futurebot - *warn_do's that had actions that have a space before it
#                     wouldn't detect.
#
# v5.1.8 (02.27.03)
#  MC_8 -      - Added examples to help system.
#  MC_8 -      - Fixed, if blank *wordset channels, no longer say you don't have
#                access.
#  MC_8 -      - If *wordset channels' value is blank, default is no longer
#                global.
#  MC_8 - Reid - /ban and /globalban now using XdXhXm expire ban time format.
#  MC_8 -      - /kick not pulling the correct nickname for %nick.
#  MC_8 -      - /ban and /globalban pulling ban mask from the one that
#                triggered the command and not the %nick you specify (should it
#                be a different nick).
#  MC_8 -      - *wordset channels' value can now be separated a space or a
#                comma.
#
# v5.1.7 (02.26.03)
#  MC_8 -         - Was not removing sent shared -exemptword.
#  MC_8 -         - DCC '-exemptwords' was not removing.
#  MC_8 - solics  - Was not adding sent shared +*words.
#  MC_8 - solics  - Problem using -*words, was using type of last ID instead of
#                   the id you are trying to remove.
#  MC_8 - solics  - Fixed 'can't read "exempt": no such variable'.
#                   Bugzilla Bug 267
#  MC_8 - solics  - Fixed ID duplication problem.
#  MC_8 - solics  - Listing problem for shares when modifying *warn_do's.
#                   Bugzilla Bug 268
#  MC_8 - solics  - DCC command 'exemptwords' showing list of bad words.
#  MC_8 - solics  - Fixed 'can't read "temp(line:warn)": no such variable'.
#  MC_8 - solics  - Fixed 'can't read "mc_bw(script)": no such variable'.
#                   Bugzilla Bug 267
#  MC_8 - Freeder - %channel was being replaced incorrectly due to a previous
#                   command using a <channel> argument.
#
# v5.1.6 (02.26.03)
#  MC_8 - - Strings having matching special characters were being messed up upon
#           adding.
#  MC_8 - - Not evaluating regular expression entries.
#  MC_8 - - /ban and /globalban wouldn't pulling the message from /kick
#           correctly.
#  MC_8 - MC_8   \ - Fixed 'invalid command name "Sent"'.
#         solics     Bugzilla Bug 266
#
# v5.1.5 (02.25.03)
#  MC_8 -        - Added debug console output of commands.
#  MC_8 -        - If nick not on channel when doing a /kick, will stop
#                  processing any further commands -- fixed.
#  MC_8 - solics - Not separating *warn_do's by the \n.
#                  Bugzilla Bug 264
#
# v5.1.4 (03.25.03)
#  MC_8 - solics - Fixed 'can't read "handle": no such variable'.
#                  Bugzilla Bug 263
#
# v5.1.3 (03.24.03)
#  MC_8 - Dillweed - Binds not being setup after a bot start or '.restart'.
#                    Bugzilla Bug 248
#  MC_8 -          - Database load's -quiet was wired backwards, was being quiet
#                    when not wanting to be (and vice versa).
#  MC_8 - Dillweed - Fixed the /tcl command to properly eval commands at global
#                    namespace.
#  MC_8 - Dillweed - Fixed problem with listing formats of *warn_do adding, not
#                    noticeably seen in /tcl commands.
#  MC_8 - heber \  - If the channel was set to 'global' only, the bad word or
#         Chris      exempt word would be ignored.
#                    Bugzilla Bug 245
#                    Bugzilla Bug 262
#
# v5.1.2 (02.23.03)
#  MC_8 -        - +exemptword without giving a channel where there exempt word
#                  contained a space would mess up thinking the first word was
#                  the channel.
#  MC_8 -        - +badword; if no channel given and the bad word contains a
#                  space, would just take the first word (separated by spaces)
#                  as the bad word string.  Fixed.
#  MC_8 -        - 'share' is default to on, but not showing in the list to be
#                  on unless you actually set it -- fixed.
#  MC_8 - solics - Added %botnick replacement variable for bad/ exempt string.
#                  Added %botnick replacement variable for *warn_do's.
#                  Bugzilla Bug 256
#  MC_8 - solics - Fixed /ban and /globalban finding users host when not in a
#                  channel.
#                  Bugzilla Bug 254
#  MC_8 - solics - Added a new *do command, /tcl.
#                  Bugzilla Bug 257
#
# v5.1.1 (02.21.03)
#  MC_8 - solics - Ban and globalban will now push the +b to the channel if user
#                  is found.
#                  Bugzilla Bug 252
#  MC_8 - solics - Fixed kick not showing entire kick message if /ban or
#                  /globalban is set to 0 with a /kick in the action.
#                  Bugzilla Bug 251
#  MC_8 - solics - Fixed 'can't read "index": no such variable'.
#                  Bugzilla Bug 250
#
# v5.1 (02.19.03)
#  MC_8 -     - Fixed sharing.
#  MC_8 -     - Fixed '[mc:bw:rx]: can't read "item": no such variable'.
#  MC_8 -     - Upgraded SVS Client.  v3.2 -> v4.0
#  MC_8 - ble - .+badword without giving a channel where there bad word
#               contained a space would mess up thinking the first word was the
#               channel.
#               Bugzilla Bug 247
#
# v5.0.2 (02.14.03)
#  MC_8 - ble - Fixed 'mc:bw:list get_value_from_type -- invalid command.'.
#
# v5.0.1 (02.10.03)
#  MC_8 - Minwel - Fixed 'missing "'.
#                  Bugzilla Bug 246
#
# v5.0 (01.29.03)
#  MC_8     -            - Upgraded SVS Client.  v3.1 -> v3.2
#! MC_8     - nadir    \ - Rewrote the entire script.  Everything has changed.
#!            heber    \   Bugzilla Bug 36
#!            Mubarak  \   Bugzilla Bug 170
#!            acidrain \   Bugzilla Bug 192
#!            Chris    \   Bugzilla Bug 210
#!            W0lF     \   Bugzilla Bug 211
#!            Ercan    \   Bugzilla Bug 230
#!                         Bugzilla Bug 232
#!                         Bugzilla Bug 236
#  MC_8     -            - Renamed channel flag 'mc.bw.exemptvoice' to
#                          'mc.bad_words.exempt_voice'.
#  MC_8     -            - Renamed channel flag 'mc.bw.exemptop' to
#                          'mc.bad_words.exempt_op'.
#  MC_8     -            - Renamed channel flag 'mc.bw.exempthalfop' to
#                          'mc.bad_words.exempt_halfop'.
#  MC_8     -            - Removed 'exempt:flag`, `procmsg`, `strip`,
#                          `warn_msgtype`, `warn_kick`, `warn_mode`, `bantime`,
#                          `use:regexp`  configuration variables.
#  MC_8      - Schnups    - Upgraded `maskhostbytype` tcl command.  v2.0 -> v2.1
#                           Bugzilla Bug 186
#  xentor /  -            - Added halfop support, for eggdrop v1.6.11 only.
#  MC_8                     Bugzilla Bug 171
#
# v4.1 (10.09.02)
#  MC_8 -           - Upgraded SVS client.  v2.2 -> v3.1
#  MC_8 - KrE80r    - Fixed 'invalid command name "encoding"'. [Bug: 161]
#  MC_8 - heber     - Encoded ability to support for non ASCII characters in
#                     channel names, such as `ã`.
#                     Bugzilla number: 152
#  MC_8 - heber     - Fixed 'invalid command name "badargs"'.
#                     Bugzilla number: 114
#                     Bugzilla number: 150
#  MC_8 - heber     - Fixed 'wrong # args: should be "*mc:bw:dcc:+badword hand
#                     idx arg"'.
#                     Bugzilla number: 146
#  MC_8 - heber     - Fixed 'invalid command name "2list"', added `2list` tcl
#                     command.  none -> v1.0
#                     Bugzilla number: 146
#  MC_8 - heber     - Fixed 'Error: invalid command name ...'.
#                     Bugzilla bug: 143
#  MC_8 - heber     - Fixed 'invalid command name "replace"'.
#                     Bugzilla bug: 125
#  MC_8 - heber     - Fixed 'missing close-brace: possible unbalanced brace in
#                     comment'.
#                     Bugzilla bug: 122
#  MC_8 -           - Added `unlist` tcl command.  none -> v1.0
#  MC_8 -           - Added `badargs` tcl command.  none -> v1.0
#  MC_8 -           - Upgraded `replace` tcl command.  v1.2 -> v1.3
#  MC_8 -           - Added new `mirc_strip`. none -> v1.0
#  MC_8 -           - Removed old `strip:all` tcl command. v1.1 -> none
#  MC_8 -           - Upgraded `chanflag` tcl command.  v3.0 -> v3.1
#  MC_8 -           - Upgraded `inter:masktype` tcl command, renamed to
#                     `maskhostbytype`.  v1.3 -> v2.0
#  MC_8 -           - Upgraded Error Catching System.  v2.1 -> v3.0
#  MC_8 - heber  \  - Fixed 'can't set "temp(syntax)": variable isn't array'.
#         xentor \    Bugzilla bug: 119
#         Javier
#  MC_8 - tim     \ - Fixed 'invalid command name ""'.
#         Francis \   Bugzilla number: 100
#         Victor  \
#         KrE80r
#
# v4.0.3 (07.09.02)
#  MC_8 - tim - Fixed problem auto conversion of datafile v1.0 to v3.0.
#               Bugzilla bug: 94
#  MC_8 -     - Upgraded replace proc.  v1.1 -> v1.2
#  MC_8 -     - Upgraded chanflag proc.  v2.0 -> v3.0
#  MC_8 - tim - Upgraded error system, a user reported an error of the error
#               system... witch means there was an legit error in the script but
#               it couldn't debug it because of another error.  Hopefully we can
#               use this new error catching system to track his bug down.
#               v2.0 -> v2.1  [Bug: 114]
#
# v4.0.2 (06.27.02)
#  MC_8 - Schnups   - Fixed 'Tcl error [mc:bw:msgm]: invalid channel: &message'.
#                     Bugzilla bug: 107
#  MC_8 - Mustang97 - Moved the history to the bottom.  While it's still
#                     important to those whom want to see what changed as to
#                     base there reasoning for upgrading, it's now out of the
#                     way so the newbies don't go crazy trying to 'read the
#                     whole thing' (is what I tell people that don't understand
#                     something and it's already mentioned) when they really
#                     don't NEED to know history changes.
#  MC_8 - kk_       - Upgraded SVS.  v2.1 -> v2.2
#
# v4.0.1 (06.19.02)
#  MC_8 -         - Cleaned up the formatting and working of all comments before
#                   coding.
#  MC_8 -         - Rewrote 'mc:bw:bw_' proc, it was ugly and unformatted.
#  MC_8 -         - Fixed conversion of string->list format for channel variable
#                   within the mc:bw:bw_ proc.
#  MC_8 -         - Removed settings 'exempt:op' and 'exempt:voice', don't know
#                   why I have that there when I have channel specific
#                   'mc.bw.exemptop' and 'mc.bw.exemptvoice' flags.
#  MC_8 - Ovidiu/ - Fixed 'Tcl error [mc:bw:bot]: illegal channel: U'.
#         dork/     Bugzilla bug: 106
#         nonREAL
#
# v4.0 (06.18.02)
#  MC_8 - Jürgen    - Upgraded masktype proc, was giving error 'couldn't set
#                     variable "temp"'.  v1.2 -> v1.3
#  MC_8 -           - Upgraded SVS.  v2.0 -> v2.1
#  MC_8 - dork/     - Added scan system on datafile load to ensure it's trying
#         nonREAL/    to load a valid line of data.
#         Rami        Bugzilla bug: 105
#                     Bugzilla bug: 91
#                     Bugzilla bug: 89
#  MC_8 -           - Fixed 'can't read " #abc123": no such variable'.
#                     Bugzilla bug: 95
#  MC_8 - Alexander - Fixed '[mc:bw:bot]: can't read "key": no such variable'.
#                     Bugzilla bug: 94
#  MC_8 -           - Putting a 300 character restriction on badword reasons,
#                     botnet communications only support up to 300 -- the extra
#                     100 is for other information pertaining to that particular
#                     badword entry.
#  MC_8 -           - Script was using [join [lindex ...]], don't know why...
#                     that's improper list usage and would cause bugs later on.
#  MC_8 -           - Changed how the bot communicates over the botnet, now
#                     encrypted.  Due to the totally different way this script
#                     shares info over the botnet, I'm going to up this to
#                     version 4.0, older version are not botnet comparable with
#                     this new communication method.
#  MC_8 -           - Bot wasn't communicating in proper list format over the
#                     botnet.
#                     Bugzilla bug: 88
#  MC_8 -           - Fixed a double listing error in 'findchanuser' proc.
#  MC_8 -           - Upgraded 'replace' proc.  v1.0 -> v1.1
#  MC_8 -           - Replaced 'chanflagison' with 'chanflag' proc.  -> v2.0
#  MC_8 -           - Upgraded 'masktype' proc, fixed some minor errors.
#                       o If ident@host == test@me@192.168.0.1, it would set
#                         ident to 'test' and host to 'me@192.168.0.1'.
#                       o Types *1,3,6,8 is support to remove the ident's first
#                         ~.  It was removing all continues (from left) ~'s.
#                       o A little more efficient, by cache'n some commands.
#                     v1.1 -> v1.2
#  MC_8 -           - Added the 'normal' character code to 'strip:all' proc.
#                     old -> v1.1
#                     Bugzilla bug: 71
#  MC_8 - niklas    - Added information to 'Information' field.
#                     Bugzilla bug: 67
#  MC_8 - Lisa      - Fixed 'can't read "db_ver": no such variable'.
#  MC_8 -           - Added a better error output system, for debugging
#                     purposes.
#  MC_8 -           - Fixed onotice errors; '[mc:bw:notc]: illegal channel:
#                     @#channel'.
#                     Bugzilla bug: 61
#  MC_8 -           - Fixed error in masktype proc; if type is 10-19 and the
#                     host is an ip and it has a 255 in any of the 4 atoms- it
#                     would be treated as a host rather than an ip.
#  MC_8 - skuum     - Added ability to make a ban sticky via bantime setting.
#                     Bugzilla bug: 58
#  MC_8 -           - Added exempt:op, exempt:voice.  Also changed exempt to
#                     exempt:flag.
#                     Bugzilla bug: 56
#
# v3.1 (11.14.01)
#  MC_8 -        - Updated SVS.  v1.3 -> v2.0.
#  MC_8 -        - Added help argument to show proper syntax for all commands,
#                     .<command> -?
#  MC_8 -        - Fixed 'Invalid channel option: #channel', in .+badword or
#                     +exemptword prompt interface.
#  MC_8 - Dr.W95 - Added use:regexp.
#
# v3.0.1 (10.30.01)
#  MC_8 -              - Rewrote explanation of ban masking types.
#  MC_8 -              - Rewrote the ban masking proc again, much smaller now.
#  MC_8 -              - Ban types 10-19 were not masking the host.domain
#                        correctly if it was an ip address.
#  MC_8 -              - Ban type 3, 4, 8 and 9 wasn't masking the host
#                        correctly.
#  MC_8 - Morpheus233  - Ban types 10-19 were giving tcl errors.  'TCL error
#                        [mc:bw:pubm]: syntax error in expression "[regexp --
#                        {^1[0-9]$} $type] &&', fixed.
#  MC_8 -              - Added share:flag configuration option.
#  MC_8 - Morpheus233/ - Fixed 'TCL error [mc:bw:pubm]: can't read "channel": no
#         Daniel         such variable'.
#  MC_8 - Dr.W95       - Added warn_msgtype configuration option.
#  MC_8 -              - Made minor changes to the SVS client section -- to give
#                        proper error if an invalid user is specified to receive
#                        a notification of a newer version.
#  MC_8 -              - Remove *inter:str proc, replaced with format's instead
#                        (more efficient).
#  MC_8 -              - Rewrote the ban masking proc, it's quicker.
#  MC_8 -              - Removed the findip proc.
#
# v3.0 (10.14.01)
#  MC_8 -       - Added prompt interface for +(bad|exempt)word.
#  MC_8 -       - Bots were flooding "U" on load via botnet link.
#  MC_8 -       - Bot was getting flooded down with utimers.
#  MC_8 -       - Entire expire time wasn't working.
#  MC_8 -       - Added DCC commands; exemptwords, +exemptword, -exemptword
#  MC_8 - ckitt - Fixed 'invalid command name ...' dealing with  '("1-2" arm
#                 line 1)' (part of the db auto conversion).
#  MC_8 -       - Added catches to not put a kick (nor ban) on the channel if
#                 the user (or banmask) isn't on the channel, saves bandwidth
#                 and also makes the listed 'Actions:' performed smaller.
#  MC_8 - ckitt - Relay kick message has a pre pending ':' that shouldn't be
#                 there.
#  MC_8 - ckitt - Added ability to share badwords entries among the botnet.
#  MC_8 -       - Since expire, last used, and warnings are now used per entry,
#                 the database version has changed to v2.  I coded in an auto db
#                 converter to convert v1 to v2.
#  MC_8 -       - Added last used visual.
#  MC_8 -       - Added expire option.
#  MC_8 -       - After exceeding warnings the bot was starting warning's over
#                 again at 0, for the user -- fixed.
#  MC_8 -       - Fixed `TCL error [mc:bw:???]: illegal channel: <bots irc
#                 nick>`.  This error doesn't occur on undernet, was found on
#                 dalnet.
#  MC_8 -       - If bantime was set to -1, it wasn't kicking as well.  Fixed.
#  MC_8 -       - It is no longer mandatory to quote the *bad*string* upon
#                 removal if it contains a space.
#  MC_8 -       - It now gives a better explanation of what actions were taken
#                 upon catching a violator.
#  MC_8 - ckitt - Added botnet relay features.
#  MC_8 -       - Fixed special character handling for bad word strings.
#  MC_8 -       - Change kickop and kickvoice to exemptop and exemptvoice.
#  MC_8 - ckitt - Wasn't kicking after setting a ban for bad word match on
#                 private (notice/ msg/ action)'s.
#  MC_8 - ckitt - Further adjusted replacement variable messages.
#  MC_8 - ckitt - On &global and &message's, the %channel replacement variable
#                 was looking like "%channelglobal".  Fixed.
#  MC_8 - ckitt - The bot was kicking before banning -- I put kicks and bans on
#                 the same queue (putserv).
#  MC_8 - ckitt - Fixed 'TCL error [mc:bw:pubm]: invalid command name
#                 "mc:bwerd:inter:findip"'.
#  MC_8 - ckitt - Fixed documentation about chanset flags.
#  MC_8 -       - DCC command wasn't recognizing option "all".
#  MC_8 -       - Changed the scripts name from Bad Werds to Bad Words.
#  MC_8 -       - To many changes to document -- I rewrote the entire script.
#
# v2.0 (06.27.01)
#  MC_8 - Sara    - Added a few examples within the command syntax layout.
#  MC_8 - ICU     - Fixed +/-badword DCC command requirement for the *bad*word*.
#  MC_8 - Rami    - Fixed `Tcl error [mc:msgm:weed]: wrong # args: should be
#                   "foreach varList list ?varList list ...? command"`.
#  MC_8 - KatzEye - Added ability to have perm bans, instead of timed bans.
#  MC_8 - KatzEye - Added ability to have the ban's global or channel specific.
#  MC_8 -         - Updated SVS to v1.2
#  MC_8 -         - Made the badword strings set able via DCC (on the fly)
#                   rather than editing them within the script.
#
# v1.11 (01.31.01)
#  (( Opps, forgot to log history changes for this version. ))
#
# v1.10.1 (02.01.01)
#  MC_8 -          - Added SVS.
#  MC_8 - Lawrence - Added warn_mode, to change user modes upon warning.
#
# v1.10 (08.14.00)
#  MC_8 - John Lange - Removed the ignore procedure.
#  MC_8 -            - Fixed, 'Tcl error [mc:weed]: invalid command name
#                      "findip"'.
#  MC_8 -            - Fixed, 'Tcl error [mc:weed:act]: illegal channel: police'
#                      This assuming your bot's nick is police.  Happens on
#                      ACTIONs to the bot.
#
# v1.9 (04.08.00)
#  MC_8 - eggheads - Removed this script's dependency on moretools.
#  MC_8 -          - Removed the requirement of having to "quote" searches
#                    containing spaces.
#
# v1.8.2 (02-07-00)
#  MC_8 - Speedo- - Fixed, 'set mc_time to -1 to turn off banning'.
#  MC_8 - Speedo- - Fixed kicking procedure.
#
# v1.8.1 (01-14-00)
#  MC_8 - - *search* found the word, but a search* or search would not find the
#           badword, fixed.
#  MC_8 - - Fixed the "duration ..." thing (showed up in the post via DCC),
#           forgot the []'s
#
# v1.8 (01-03-00)
#  MC_8 - - Processes messages from user X and processes them as if they were
#           text said to the rooms, witch would pull from global then channel
#           bad werd list.
#           example:  say your rule for #warez4free is "*join #*" and
#                     #warez4warez is "*ratio*", and let's say user X in
#                     #warez4free and in #warez4warez and /msg's the bot with "
#                     hey dude, join #blaboo " .. he will get booted from
#                     #warez4free but not from #warez4warez.
#           note: This function is optional.
#  MC_8 - - Added message to badwords set, for all messages, this is processed
#           before specific channel badwords are done.
#
# v1.7 (12-14-99)
#  MC_8 - - Fixed %match display, was showing (if you know tcl) lindex %match 0,
#           only taking the first if there was a space in the search.
#
# v1.6 (12-05-99)
#  MC_8 -       - Added %match variable to warning message and kick message.
#  MC_8 - Malik - If you had NO global defined (regardless of wether or not it
#                 was empty) (in other words, you DELEATED global set) the whole
#                 script ceased to work, no error messages what so ever.
#
# v1.5 (11-22-99)
#  MC_8 - unknown - Treating channel notice's as regular channel text.
#
# v1.4 (10-18-99)
#  MC_8 - m8s - Now ban before kick (as opposed to vice versa).
#  MC_8 -     - I fixed the bantype error (it was in moretools), anyhow,
#               moretools 1.1.6 min. inst.
#
# v1.3 (10-11-99)
#  MC_8 - m8s - Found minor error in ban masking option, fixed.
#  MC_8 -     - Added more options to the ban masking option.
#
# v1.0 - v1.2 (??-??-99)
#  MC_8 - - Inital release
##
