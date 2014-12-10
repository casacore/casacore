//# LogMessage.cc: Informational log messages with with time,priority,and origin
//# Copyright (C) 1996,1997,2001,2002,2003
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//# License for more details.
//#
//# You should have received a copy of the GNU Library General Public License
//# along with this library; if not, write to the Free Software Foundation,
//# Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include <casacore/casa/Logging/LogMessage.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/Regex.h>

#include <casacore/casa/sstream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

LogMessage::LogMessage(Priority prio)
  : priority_p(prio)
{
    // Nothing
}

LogMessage::LogMessage(const LogOrigin &sourceLocation, Priority priority)
  : origin_p(sourceLocation), priority_p(priority)
{
    // Nothing
}

LogMessage::LogMessage(const String &message, const LogOrigin &sourceLocation, 
	     Priority priority)
  : origin_p(sourceLocation), priority_p(priority)
{
    this->message(message);
}

void LogMessage::copy_other(const LogMessage &other)
{
    priority_p = other.priority_p;
    origin_p = other.origin_p;
    time_p = other.time_p;
    message_p = other.message_p;
}

LogMessage::LogMessage(const LogMessage &other)
{
    copy_other(other);
}

LogMessage &LogMessage::operator=(const LogMessage &other)
{
    if (this != &other) {
        copy_other(other);
    }
    return *this;
}

LogMessage::~LogMessage()
{
    // Nothing
}

const String &LogMessage::message() const
{
    return message_p;
}

LogMessage &LogMessage::message(const String &message, Bool keepLastTime)
{
    message_p = message;
    if (! keepLastTime) {
        time_p.now();
    }
	//Remove everything after the final newline
    Int n = message_p.length();
    while (--n >= 0 && message_p[n] == '\n') {
	; // Nothing
    }
    if (n+1 < Int(message_p.length())) {
	message_p = message_p.before(n+1);
    }

    return *this;
}

uInt LogMessage::line() const
{
    return origin_p.line();
}


LogMessage &LogMessage::line(uInt which)
{
    origin_p.line(which);
    return *this;
}

LogMessage &LogMessage::sourceLocation(const SourceLocation *where)
{
    origin_p.sourceLocation(where);
    return *this;
}

const LogOrigin &LogMessage::origin() const
{
    return origin_p;
}

LogMessage &LogMessage::origin(const LogOrigin &origin)
{
    origin_p = origin;
    return *this;
}

LogMessage::Priority LogMessage::priority() const
{
    return priority_p;
}

LogMessage &LogMessage::priority(LogMessage::Priority which)
{
    priority_p = which;
    return *this;
}

const Time &LogMessage::messageTime() const
{
    return time_p;
}

LogMessage &LogMessage::messageTime(const Time &theTime)
{
    time_p = theTime;
    return *this;
}


const String &LogMessage::toString(Priority which)
{
    static String names[11] = {
        "DEBUGGING", "DEBUG2", "DEBUG1", "INFO5", "INFO4", "INFO3", "INFO2", "INFO1", "INFO", "WARN", "SEVERE"
    };

    AlwaysAssert(which >= DEBUGGING && which <= SEVERE, AipsError);
    return names[which];
}

String LogMessage::toString() const
{
    String header  = messageTime().ISODate();
    header += "\t";
    header += toString(priority());
    header += "\t";
    if (! origin_p.isUnset()) {
        String daOrigin = origin().toString();
        if (priority_p > NORMAL1 && priority_p < WARN) {
            // Remove file and line location from origin
            daOrigin.gsub(Regex(".file .*line .*"), "");
        }
        header += daOrigin;
  }
	
  String continuationHeader = "\n" + header + "+\t";
  String message = String(message_p); // copy
  message.gsub("\n", continuationHeader);

  ostringstream os;
  os << header << "\t" << message;
  return os;
}

String LogMessage::toTermString() const
{
    String header  = messageTime().ISODate();
    header += "\t";
    header += toString(priority());
    header += "\t";
    if (! origin_p.isUnset()) {
        String daOrigin = origin().toString();
        if (priority_p > NORMAL1 && priority_p < WARN) {
            // Remove file and line location from origin
            daOrigin.gsub(Regex(".file .*line .*"), "");
        }
        header += daOrigin;
  }
	
  String continuationHeader = "\n\t";
  String message = String(message_p); // copy
  //message.gsub("\n", continuationHeader);

  ostringstream os;
  os << header << "\n" << message;
  //String pr = toString(priority());
  //os << pr.resize(6, ' ') << "   " << message;
  return os;
}

ostream &operator<<(ostream &os, const LogMessage &message)
{
    os << message.toString() << endl;
    return os;
}

} //# NAMESPACE CASACORE - END

