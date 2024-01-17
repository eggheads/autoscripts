#########################################################################################
# Name          m00nie::linktitle
# Description   Simple script that grabs the title of any web link spammed into the channel.
#                It does try to work around various versions of http and SNI https issue
#                this might not be fantastic....It also aims to be as simple as possible
#                to maintain and use. As ever any comments/suggestions are welcome :)
#
# Version       1.6 - Some tweaks handling large white space in titles also trying to 
#                accomodate improvements in v1.9 release of eggdrop (again thanks to
#                CrazyCat for mentioning this :D)
#               1.5 - "improving" the URL splitting (its quite horrible) but it does 
#                seem to work better now. Less 404 or 301 results :)
#               1.4 - Much improved html entity decoding. Thanks to CrazyCat for the 
#                suggesiton on a great way to do this :)
#               1.3 - Actually try to follow redirects plus better title parsing (maybe)
#               1.2 - Handle sever errors etc better so we dont spam fails
#               1.1 - Try to clean up "special" characters in titles. Also updated regexp
#                to match titles over multiple lines and headers in caps
#               1.0 - Initial release.
#
# Website       https://www.m00nie.com/eggdrop-url-title-grabbing-script
# Notes         "+chanset linktitle #yourchan" to enable in your channel
#                Edit ignore lists below to ensure only desired URLs are queried
#                Also edit the throttling timers for chan, user/nick and links
#########################################################################################
namespace eval m00nie {
    namespace eval linktitle {

    # Ignore lists! - Append/change as required
    # Users to ignore e.g. Other bots
    variable ignorenicks "love hate"

    # Domains to ignore e.g. if other scripts grab info for these already
    # subdomains like www will be required as shown below
    variable ignoredomains "www.youtube.com youtube.com www.twitter.com twitter.com"

    # Time in seconds to throttle users, channel and specific links - default of 30, 5 & 300
    variable user_throt 30
    variable chan_throt 5
    variable link_throt 300
    
    #
    # ---- Dont change things below this line -----
    #
    package require http
    # We need to verify the revision of TLS since prior to this version is missing auto host for SNI
    if { [catch {package require tls 1.7.11}] } {
    	# We dont have an autoconfigure option for SNI
        putlog "m00nie::linktitle *** WARNING *** OLD Version of TLS package installed please update to 1.7.11+ ... Will attempt to work around this in the meantime... Things might not work as expected though "
        variable httpv "0"
    } else {
        package require tls 1.7.11
        variable httpv "1"
    }
    bind pubm - * m00nie::linktitle::autoinfo
    variable version "1.6"
    variable throttled
    setudef flag linktitle

#### Script Starts here #####

proc autoinfo {nick uhost hand chan text} {
    # Check chan is enabled
    if {[channel get $chan linktitle]} {

    # Check if we should ignore the nick
    if {[regexp -nocase $nick $m00nie::linktitle::ignorenicks]} {
        putlog "m00nie::linktitle::log $nick is being ignored, string of ignored users is $m00nie::linktitle::ignorenicks"
        return
    }

    # Break text into words (made the regex easier! ;D)
    set wordlist [regexp -inline -all -- {\S+} $text]
    foreach word $wordlist {
	# Check if it looks like a URL and bin if not
	if { !([regexp -nocase {http.*} $word]) } { continue }
	# Total hack to make the following regex simplier (again) but push on / if we dont have one then remove it (classy ;D)
        # Break the URL into three parts delimited by /
        # This will b0rk on "abnormal" urls I think like user:pass@ or :port 
	if { !([regexp -nocase {\/$} $word]) } { 
		set word ${word}/ 
		regexp -nocase -- {(?:http(?:s|).{3}|)(.*?)(\/.*)} $word url host path
		regsub {\/$} $url {} url 
	} else { 
		regexp -nocase -- {(?:http(?:s|).{3}|)(.*?)(\/.*)} $word url host path
	}
        
	# Should only continue for valid URLs but check if we have any hosts
        if {[info exists host]} {
           	putlog "m00nie::linktitle::autoinfo found URL: $url and host: $host"
		
            	# Check if we should ignore the domains/host
		if {[regexp -nocase $host $m00nie::linktitle::ignoredomains]} {
                	putlog "m00nie::linktitle::log $host is being ignored, string of ignored domains is $m00nie::linktitle::ignoredomains"
                	return
		}

		# Check if the nick, channel and URL is throttle 
		if {[throttlecheck $nick $chan $url]} { return 0 }

		# Grab the page and spam it out :)
		set title [gettitle $url $host]
		set title [makepretty $title]
        	if {$title != 0} {
        		puthelp "PRIVMSG $chan :$host: \002$title\002"
		}
		unset host
	}
    }
  }
}

proc makepretty {title} {
    # Attempts to decode html entoty stuff that may appear so its human friendly
    putlog "m00nie::linktitle::makepretty is running"
    set entities {
	"à"   "\&agrave;" "à"   "\&agrave;" "á"   "\&aacute;" "â"   "\&acirc;"
	"ã"   "\&atilde;" "ä"   "\&auml;"    "å"   "\&aring;"   "æ"   "\&aelig;"
	"ç"   "\&ccedil;" "è"   "\&egrave;" "é"   "\&eacute;" "ê"   "\&ecirc;"
	"ë"   "\&euml;"    "ì"   "\&igrave;" "í"   "\&iacute;" "î"   "\&icirc;"
	"ï"   "\&iuml;"    "ð"   "\&eth;"    "ñ"   "\&ntilde;" "ò"   "\&ograve;"
	"ó"   "\&oacute;" "ô"   "\&ocirc;"   "õ"   "\&otilde;" "ö"   "\&ouml;"
	"÷"   "\&divide;" "ø"   "\&oslash;" "ù"   "\&ugrave;" "ú"   "\&uacute;"
	"û"   "\&ucirc;"   "ü"   "\&uuml;"    "ý"   "\&yacute;" "þ"   "\&thorn;"
	"ÿ"   "\&yuml;"    "\"" "\&quot;"    "\&" "\&amp;"    "€"   "\&euro;"
	"œ"   "\&oelig;"   "Ÿ"   "\&Yuml;"    "¡"   "\&iexcl;"
	"¢"   "\&cent;"    "£"   "\&pound;"   "¤"   "\&curren;" "¥"   "\&yen;"
	"¦"   "\&brvbar;" "¦"   "\&brkbar;" "§"   "\&sect;"    "¨"   "\&uml;"
	"¨"   "\&die;"    ""   "\&copy;"    "ª"   "\&ordf;"    "«"   "\&laquo;"
	"¬"   "\&not;"    "-"   "\&#8209;"   ""   "\&reg;"    "¯"   "\&macr;"
	"¯"   "\&hibar;"   "°"   "\&deg;"    "±"   "\&plusmn;" "²"   "\&sup2;"
	"³"   "\&sup3;"    "´"   "\&acute;"   "µ"   "\&micro;"   "¶"   "\&para;"
	"·"   "\&middot;" "¸"   "\&cedil;"   "¹"   "\&sup1;"    "º"   "\&ordm;"
	"»"   "\&raquo;"   "¼"   "\&frac14;" "½"   "\&frac12;" "¾"   "\&frac34;"
	"¿"   "\&iquest;" "À"   "\&Agrave;" "Á"   "\&Aacute;" "Â"   "\&Acirc;"
	"Ã"   "\&Atilde;" "Ä"   "\&Auml;"    "Å"   "\&Aring;"   "Æ"   "\&AElig;"
	"Ç"   "\&Ccedil;" "È"   "\&Egrave;" "É"   "\&Eacute;" "Ê"   "\&Ecirc;"
	"Ë"   "\&Euml;"    "Ì"   "\&Igrave;" "Í"   "\&Iacute;" "Î"   "\&Icirc;"
	"Ï"   "\&Iuml;"    "Ð"   "\&ETH;"    "Ð"   "\&Dstrok;" "Ñ"   "\&Ntilde;"
	"Ò"   "\&Ograve;" "Ó"   "\&Oacute;" "Ô"   "\&Ocirc;"   "Õ"   "\&Otilde;"
	"Ö"   "\&Ouml;"    "×"   "\&times;"   "Ø"   "\&Oslash;" "Ù"   "\&Ugrave;"
	"Ú"   "\&Uacute;" "Û"   "\&Ucirc;"   "Ü"   "\&Uuml;"    "Ý"   "\&Yacute;"
	"Þ"   "\&THORN;"   "ß"   "\&szlig;"   "\\n" "   "          "\'" "\&apos;"
	"&"   "\&#38;"    "/"   "\&#047;"    "\\" "\&#092;"    "\[" "\&#091;"   
	"<"   "\<"      ">"   "\>"      "\]" "\&#093"
	"\(" "\&#040;"    "\)" "\&#041;"    "£"   "\&#163;"
	"¨"   "\&#168;"    ""   "\&#169;"    "«"   "\&#171;"    "­"   "\&#173;"
	""   "\&#174;"    "´"   "\&#180;"    "·"   "\&#183;"    "¹"   "\&#185;"
	"»"   "\&#187;"    "¼"   "\&#188;"    "½"   "\&#189;"    "¾"   "\&#190;"
	"À"   "\&#192;"    "Á"   "\&#193;"    "Â"   "\&#194;"    "Ã"   "\&#195;"
	"Ä"   "\&#196;"    "Å"   "\&#197;"    "Æ"   "\&#198;"    "Ç"   "\&#199;"
	"È"   "\&#200;"    "É"   "\&#201;"    "Ê"   "\&#202;"    "Ë"   "\&#203;"
	"Ì"   "\&#204;"    "Í"   "\&#205;"    "Î"   "\&#206;"    "Ï"   "\&#207;"
	"Ð"   "\&#208;"    "Ñ"   "\&#209;"    "Ò"   "\&#210;"    "Ó"   "\&#211;"
	"Ô"   "\&#212;"    "Õ"   "\&#213;"    "Ö"   "\&#214;"    "×"   "\&#215;"
	"Ø"   "\&#216;"    "Ù"   "\&#217;"    "Ú"   "\&#218;"    "Û"   "\&#219;"
	"Ü"   "\&#220;"    "Ý"   "\&#221;"    "Þ"   "\&#222;"    "ß"   "\&#223;"
	"à"   "\&#224;"    "á"   "\&#225;"    "â"   "\&#226;"    "ã"   "\&#227;"
	"ä"   "\&#228;"    "å"   "\&#229;"    "æ"   "\&#230;"    "ç"   "\&#231;"
	"è"   "\&#232;"    "é"   "\&#233;"    "ê"   "\&#234;"    "ë"   "\&#235;"
	"ì"   "\&#236;"    "í"   "\&#237;"    "î"   "\&#238;"    "ï"   "\&#239;"
	"ð"   "\&#240;"    "ñ"   "\&#241;"    "ò"   "\&#242;"    "ó"   "\&#243;"
	"ô"   "\&#244;"    "õ"   "\&#245;"    "ö"   "\&#246;"    "÷"   "\&#247;"
	"ø"   "\&#248;"    "ù"   "\&#249;"    "ú"   "\&#250;"    "û"   "\&#251;"
	"ü"   "\&#252;"    "ý"   "\&#253;"    "þ"   "\&#254;"    "–"   "\&#8211;"
	"‘"   "\&#8216;"   "’"   "\&#8217;"   "ő"   "\&#337;"
    }
   return [::tcl::string::map [lreverse $entities] $title]
}
 


proc gettitle {url host} {
    putlog "m00nie::linktitle::gettitle is running"
    # Set server name and try to accomodate 'old' package
    if { $m00nie::linktitle::httpv eq 0 } {
        putlog "m00nie::linktitle::gettitle using old http package"
        ::http::register https 443 [list ::tls::socket -servername $host]
    } else {
        putlog "m00nie::linktitle::gettitle using modern http package"
        ::http::register https 443 [list ::tls::socket -autoservername true]
    }

    # Grab the page
    for { set i 1 } { $i <= 5 } { incr i } {
	set token [::http::geturl "$url" -timeout 5000]
	set x 1

	# Try to catch and follow 301/302s (safely!)
	while {[::http::ncode $token] in {301 302}} {
		putlog "m00nie::linktitle::gettitle caught a [::http::ncode $token] attempting to follow..."
		# Grab the URL from the redirect (there must be a nicer way)
    		set meta [::http::meta $token]
    		set key [lsearch -exact -nocase -inline [dict keys $meta] location]
    		set url [dict get $meta $key]
    		::http::cleanup $token
    		putlog "m00nie::linktitle::gettitle following redirect to: $url"

		# Some servers may not return a full URL so catch that...Again this is some hacky shiz just now
		if {!([regexp -nocase {^http} $url])} {
			set url "https://${host}${url}"
		}
    		set token [::http::geturl $url -timeout 5000]

		# Lets try and not fall down a black hole
		incr x
		if { $x >= 5 } { 
			putlog "m00nie::linktitle::gettitle redirected five times so giving up!"
			return 0 
		}
	}
	# We got another kind of message (not a redirect or a 200) so lets give up
	if {[::http::ncode $token] ne "200"} {
	    putlog "m00nie::linktitle::gettitle $host returned status message of [::http::ncode $token]."
	    return 0
	}


	set rawpage [::http::data $token]
	if {[string length rawpage] > 0} { break }
    }
    putlog "m00nie::linktitle::gettitle Rawpage length is: [string length $rawpage]"
    if {[string length $rawpage] == 0} { error "$host returned ZERO no data :( or we couldnt connect properly" }

    # Parse for title
    set title [regexp -nocase -inline -all -- {(?x)\<title.*?\>.*?\<\/title\>} $rawpage]
    if {[string length $title] > 0} {
        regexp -nocase {(?x)(\<title.*?\>)(.*?)(\<\/.*?\>)} $title junk head title foot
	set title [string trim $title]
	# For eggdrop v1.9+ we shouldnt need to do this
	if {$::numversion < "1090000"} {
		set title [encoding convertfrom utf-8 $title]
	}
        return $title
    } else {
        putlog "Couldnt get the title for $url :("
        return 0
    }
}

proc throttlecheck {nick chan url} {
	if {[info exists m00nie::linktitle::throttled($url)]} {
		putlog "m00nie::linktitle::throttlecheck url $url, is throttled at the moment"
		return 1
	} elseif {[info exists m00nie::linktitle::throttled($chan)]} {
		putlog "m00nie::linktitle::throttlecheck Channel $chan is throttled at the moment"
		return 1
	} elseif {[info exists m00nie::linktitle::throttled($nick)]} {
		putlog "m00nie::linktitle::throttlecheck User $nick is throttled at the moment"
                return 1
	} else {
		set m00nie::linktitle::throttled($nick) [utimer $m00nie::linktitle::user_throt [list unset m00nie::linktitle::throttled($nick)]]
		set m00nie::linktitle::throttled($chan) [utimer $m00nie::linktitle::chan_throt [list unset m00nie::linktitle::throttled($chan)]]
		set m00nie::linktitle::throttled($url) [utimer $m00nie::linktitle::link_throt [list unset m00nie::linktitle::throttled($url)]]
		return 0
	}
}

}
}
putlog "m00nie::linktitle $m00nie::linktitle::version loaded"
