# awk script for converting an iCal formatted file to a sequence of org-mode headings.
# this may not work in general but seems to work for day and timed events from Google's
# calendar, which is really all I need right now...
#
# usage:
#   awk -f THISFILE < icalinputfile.ics > orgmodeentries.org --assign NAME=category
#
# where the category is used to define a CATEGORY for all entries in
# the file and also assign that label as a tag to each entry
#
# Note: change org meta information generated below for author and
# email entries!
#
# Known bugs:
# - not so much a bug as a possible assumption: date entries with no time
#   specified are assumed to be independent of the time zone.
#
# Eric S Fraga
# 20100629 - initial version
# 20100708 - added end times to timed events
#          - adjust times according to time zone information
#          - fixed incorrect transfer for entries with ":" embedded within the text
#          - added support for multi-line summary entries (which become headlines)
# 20100709 - incorporated time zone identification
#          - fixed processing of continuation lines as Google seems to
#            have changed, in the last day, the number of spaces at
#            the start of the line for each continuation...
#          - remove backslashes used to protect commas in iCal text entries
# no further revision log after this as the file was moved into a git
# repository...
#
# Last change: 2016.05.26 08:47:12
#----------------------------------------------------------------------------------

# a function to take the iCal formatted date+time, convert it into an
# internal form (seconds since time 0), and adjust according to the
# local time zone (specified by +-seconds calculated in the BEGIN
# section)

function datetimestamp(input)
{
    # convert the iCal Date+Time entry to a format that mktime can understand
    datespec = gensub("([0-9][0-9][0-9][0-9])([0-9][0-9])([0-9][0-9])T([0-9][0-9])([0-9][0-9])([0-9][0-9]).*[\r]*", "\\1 \\2 \\3 \\4 \\5 \\6", "g", input);
    # print "date spec : " datespec; convert this date+time into
    # seconds from the beginning of time and include adjustment for
    # time zone, as determined in the BEGIN section below.  The
    # adjustment is only included if the time stamp has a Z at the
    # end.  Of course, we should actually incorporate the time zone
    # information in the time stamp line but ...
    if (0 < index(input,"Z")) {
        # For time
        # zone adjustment, I have not tested edge effects, specifically
        # what happens when UTC time is a different day to local time and
        # especially when an event with a duration crosses midnight in UTC
        # time.  It should work but...
        timestamp = mktime(datespec) + seconds;
    }
    else {
        timestamp = mktime(datespec);
    }
    # print "date spec: " datespec;
    #timestamp = mktime(datespec);
    # print "adjusted    : " timestamp
    # print "Time stamp  : " strftime("%Y-%m-%d %H:%M", timestamp);
    return timestamp;
}

# version of above but for dates only
function datestamp(input)
{
    # create a date using midnight as the time
    datespec = gensub( "([0-9][0-9][0-9][0-9])([0-9][0-9])([0-9][0-9]).*[\r]*", "\\1 \\2 \\3 0 0 0", "g", input );
    # convert to internal representation
    timestamp = mktime(datespec);
    # and finally convert to something org understands
    datestring = strftime("%Y-%m-%d %a", timestamp);
    #print "In datestamp: datespec=" datespec " timestamp=" timestamp " datestring=" datestring;
    return datestring;
}

# start of the output file now
BEGIN {
    # use a colon to separate the type of data line from the actual contents
    FS = ":";
    
    # determine the number of seconds to use for adjusting for time
    # zone difference from UTC.  This is used in the function
    # datetimestamp above.  The time zone information returned by
    # strftime() is in hours * 100 so we multiply by 36 to get
    # seconds.  This does not work for time zones that are not an
    # integral multiple of hours (e.g. Newfoundland)
    seconds = gensub("([+-])0", "\\1", 1, strftime("%z")) * 36;
    
    date1 = "";			# for start of an event
    date2 = "";			# for end of an event, if specified
    entry = ""
    first = 1;			# true until an event has been found
    headline = ""
    icalentry = ""  # the full entry for inspection
    id = ""
    indescription = 0;
    inevent = 0;                # we have VEVENTS but also other items which we do not process
    location = "";              # outlook entries, at least, often include a location
    repeat = "";                # is item repeated? if so, how often
    time1 = "";			# for start of an event, if specified
    time2 = "";			# for end of an event, if specified
    todotype = "";              # type of TODO
    
    if (NAME == "")
        NAME = "ical2org";
    
    print "# -*- mode: auto-revert; mode: org; -*-"   # suggested by Henrik Holmboe
    print "#+TITLE:     Main Google calendar entries"
    print "#+AUTHOR:    Eric S Fraga"
    print "#+EMAIL:     e.fraga@ucl.ac.uk"
    print "#+DESCRIPTION: converted using the ical2org awk script"
    print "#+CATEGORY: " NAME
    print " "
}

# continuation lines (at least from Google) start with two spaces
# if the continuation is after a description or a summary, append the entry
# to the respective variable

/^[ ]+/ { 
    if (indescription) {
        entry = entry gensub("\r", "", "g", gensub("^[ ]+", "", 1, $0));
    } else if (insummary) {
        summary = summary gensub("\r", "", "g", gensub("^[ ]+", "", 1, $0))
    } else if (inuid) {
        id = id gensub("\r", "", "g", gensub("^[ ]+", "", 1, $0))
    }
    icalentry = icalentry "\n" $0
}

/^BEGIN:VEVENT/ {
    # start of an event.  if this is the first, output the preamble from the iCal file
    if (first) {
        print "* COMMENT original iCal preamble"
        print gensub("\r", "", "g", icalentry)
        icalentry = ""
    }
    havesummary = 0;
    inevent = 1;
    first = false;
    repeat = "";
}

/^BEGIN:VTODO/ {
    if (first){
        print "* COMMENT original iCal preamble";
        print gensub("\r", "", "g", icalentry);
        icalentry = "";
        first = false;
    }
    havesummary = 0;
    intodo = 1;
    repeat = "";
    todotype = "";
}
# any line that starts at the left with a non-space character is a new data field

/^[A-Z]/ {
    # we ignore DTSTAMP lines as they change every time you download
    # the iCal format file which leads to a change in the converted
    # org file as I output the original input.  This change, which is
    # really content free, makes a revision control system update the
    # repository and confuses.
    if (! index("DTSTAMP", $1)) icalentry = icalentry "\n" $0
    # this line terminates the collection of description and summary entries
    indescription = 0;
    if (insummary) {
        havesummary = 1;
    }
    insummary = 0;
}

# this type of entry represents a day entry, not timed, with date
# stamp YYYYMMDD.  For a todo item, this indicates a scheduled item.

/^DTSTART;VALUE=DATE/ {
    # print "DTSTART date only entry: " $0;
    # date1 = gensub("([0-9][0-9][0-9][0-9])([0-9][0-9])([0-9][0-9]).*[\r]*", "\\1-\\2-\\3", "g", $2)
    date1 = datestamp($2);
    time1 = ""
}

# this represents a timed entry with date and time stamp
# YYYYMMDDTHHMMSS we ignore the seconds.  This entry may have a time
# zone specification which is currently ignored although it should be
# possible, not easy, to incorporate.  We assume that this information
# is only relevant for appointments and not TODO items.  We expect
# TODO items to have only a date for the START field and that date
# will be the scheduled date.  See above.

/^DTSTART(;TZID.*)?:/ {
    if (inevent) {
        # print "DTSTART line: " $0;
        # print "checking start time: " $2;
        date1 = strftime("%Y-%m-%d %a", datetimestamp($2));
        time1 = strftime(" %H:%M", datetimestamp($2));
        # print "====>         time: " time1;
        # print date;
    }
}

# and the same for the end date; here we extract only the time and append this to the 
# date+time found by the DTSTART entry.  We assume that entry was there, of course.
# should probably add some error checking here!  In time...

/^DTEND;VALUE=DATE/ {
    if (inevent) {
        # date2 = gensub("([0-9][0-9][0-9][0-9])([0-9][0-9])([0-9][0-9]).*[\r]", "\\1-\\2-\\3", "g", $2)
        date2 = datestamp($2);
        time2 = ""
    }
}

/^DTEND(;TZID=[^:]*)?:/ {
    if (inevent) {
        # print $0
        date2 = strftime("%Y-%m-%d %a", datetimestamp($2));
        time2 = strftime("%H:%M", datetimestamp($2));
    }
}

# TODO items may (should?) have a DUE date/time.
/^DUE(;TZID=[^:]*)?:/ {
    if (intodo){
        date2 = strftime("%Y-%m-%d %a", datetimestamp($2));
        time2 = strftime("%H:%M", datetimestamp($2));
    }
}
# deadline with only a date
/^DUE;VALUE=DATE/ {
    # print "DUE;VALUE=DATE entry:" $0
    # print "... date part is >" $2 "<"
    # print "... date2 before " date2
    if (intodo) {
        #date2 = gensub("([0-9][0-9][0-9][0-9])([0-9][0-9])([0-9][0-9]).*[\r]*", "\\1-\\2-\\3", "g", $2)
        date2 = datestamp($2);
        time2 = ""
    }
    # print "... date2 after " date2
}
# The description will the contents of the entry in org-mode.
# this line may be continued.

/^DESCRIPTION/ { 
    $1 = "";
    entry = entry "\n" gensub("\r", "", "g", $0);
    indescription = 1;
}

/^LOCATION/ {
    $1 = "";
    location = gensub("\r", "", "g", $0);
}

# the status of a TODO item: we know about NEEDS-ACTION and
# COMPLETED.  There may be others...
/^STATUS/ {
    if ($2 == "NEEDS-ACTION")
        todotype = "TODO";
    else if ($2 == "COMPLETED")
        todotype = "DONE";
    else
        todotype = "UNKNOWN";
}
# is there a repetition rule.  I don't know how general this is but
# Microsoft's Outlook calendar uses this for repeats

/^RRULE/ {
    # print ">>> Checking rule with string: " $2;
    i = match($2,"FREQ=[A-Z]+;");
    # printf(">>> Index=%d start=%d length=%d\n\n", i, RSTART, RLENGTH);
    frequency = substr($2, RSTART+5, RLENGTH-6);
    # print ">>> Frequency is " frequency "\n\n";
    i = match($2,"INTERVAL=[0-9]+;");
    interval = 1;               # default interval if none is found
    if (i>0) {
        interval = substr($2, RSTART+9, RLENGTH-10);
    }
    period = "";
    if (frequency == "DAILY") {
        period = "d";
    }
    else if (frequency == "WEEKLY") {
        period = "w";
    }
    else if(frequency == "MONTHLY") {
        period = "m";
    }
    else if(frequency == "YEARLY") {
        period = "y";
    }
    if (period != "") {
        repeat = sprintf(" +%d%s", interval, period);
    }
    # print ">>> Repeat is " repeat;
}

# the summary will be the org heading

/^SUMMARY/ { 
    $1 = "";
    if (!havesummary) {
        summary = gensub("\r", "", "g", $0);
        insummary = 1;
    }
}

# the unique ID will be stored as a property of the entry

/^UID/ { 
    $1 = "";
    id = gensub("\r", "", "g", $0);
    inuid = 1;
}

# when we reach the end of the event line, we output everything we
# have collected so far, creating a top level org headline with the
# date/time stamp, unique ID property and the contents, if any

/^END:VEVENT/ {
    # translate \n sequences to actual newlines and unprotect commas (,)
    print "* " gensub("\\\\,", ",", "g", gensub("\\\\n", " ", "g", summary)) "  :" NAME ":"
    print ":PROPERTIES:"
    print ":ID:       " id
    if (location != "") {
        print ":LOCATION: " gensub("\\\\,", ",", "g", location);
    }
    print ":END:"
    if (date1 == date2) {
        if (time2 == "")
            print "  <" date1 time1 repeat ">"
        else
            print "  <" date1 time1 "-" time2 repeat ">"
    }
    else {
        if (time1 == "")
            print "<" date1 ">--<" date2 ">"
        else
            print "  <" date1 time1 ">--<" date2 " " time2 ">"
    }
    # for the entry, convert all embedded "\n" strings to actual newlines
    print ""
    # translate \n sequences to actual newlines and unprotect commas (,)
    print gensub("\\\\,", ",", "g", gensub("\\\\n", "\n", "g", entry));
    print "** COMMENT original iCal entry"
    print gensub("\r", "", "g", icalentry)
    summary = ""
    date = ""
    date1 = ""
    date2 = ""
    time1 = ""
    time2 = ""
    entry = ""
    icalentry = ""
    indescription = 0
    inevent = 0
    insummary = 0
    period = "";
    repeat = "";
}

# the end of a TODO item is similar to an event except that the dates
# are used for scheduling and deadline information

/^END:VTODO/ {
    # translate \n sequences to actual newlines and unprotect commas (,)
    print "* " todotype " " gensub("\\\\,", ",", "g", gensub("\\\\n", " ", "g", summary)) "  :" NAME ":"
    # scheduling and deadline information come immediately after the
    # headline, before properties
    if (date1 != "") {
        if (date2 != "")
            if (time2 != "")
                print "SCHEDULED: <" date1 time1 "> DEADLINE: <" date2 " " time2 "> "
            else
                print "SCHEDULED: <" date1 time1 "> DEADLINE: <" date2 "> "
        else
            print "SCHEDULED: <" date1 time1 "> "
    } else if (date2 != "") {
        if (time2 != "")
            print "DEADLINE: <" date2 " " time2 "> "
        else
            print "DEADLINE: <" date2 "> "
    }
    # now come the properties which include the ID always and possibly
    # a location
    print ":PROPERTIES:"
    print ":ID:       " id
    if (location != "") {
        print ":LOCATION: " gensub("\\\\,", ",", "g", location);
    }
    print ":END:"
    # now the entry; we put in a blank line just because that's the
    # way I like it, ah ha ah ha... ;-)
    print ""
    # translate \n sequences to actual newlines and unprotect commas (,)
    print gensub("\\\\,", ",", "g", gensub("\\\\n", "\n", "g", entry));
    print "** COMMENT original iCal entry"
    print gensub("\r", "", "g", icalentry)
    summary = "";
    date = "";
    date1 = "";
    date2 = "";
    time1 = "";
    time2 = "";
    entry = "";
    icalentry = "";
    indescription = 0;
    inevent = 0;
    insummary = 0;
    intodo = 0;
    period = "";
    repeat = "";
}

# Local Variables:
# time-stamp-line-limit: 1000
# time-stamp-format: "%04y.%02m.%02d %02H:%02M:%02S"
# time-stamp-active: t
# time-stamp-start: "Last change:[ \t]+"
# time-stamp-end: "$"
# End:
