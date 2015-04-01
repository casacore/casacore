//# LogMessage.h: Informational log messages with with time,priority, and origin
//# Copyright (C) 1996,1997,1999,2000,2001
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
//#
//# $Id$

#ifndef CASA_LOGMESSAGE_H
#define CASA_LOGMESSAGE_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Logging/LogOrigin.h>
#include <casacore/casa/OS/Time.h>
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Informational log messages with with time, priority, and origin.
//</summary>

// <use visibility=export> 

// <reviewed reviewer="wbrouw" date="1996/08/21" tests="tLoging.cc" demos="dLogging.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="LogOrigin">LogOrigin</linkto>
// </prerequisite>
//
// <synopsis> 
// A <src>LogMessage</src> is the unit of information in the Logging system.
// A LogMessage consists of an informational text (String) message tagged with
// the following:
// <ul>
// <li> The time at which the message was generated ("computer" time, not high
//      precision astronomical time).
// <li> A priority - one of <src>DEBUGGING</src>, <src>NORMAL</src>, 
//      <src>WARN</src>, or  <src>SEVERE</src>.
// <li> A <linkto class="LogOrigin">LogOrigin</linkto>, containing the source
//      code location where the message originated. It also contains the
//      <linkto class="ObjectID">ObjectID</linkto> if the originator was a
//      distributed object. This is mostly of use in debugging.
// </ul>
// </synopsis> 
//
// <example>
// <srcblock>
// void globalFunction(Int arg)
// {
//    LogMessage logMessage(LogOrigin("globalFunction(Int arg)", WHERE));
//    ...
//    logMessage.message("my message").line(__LINE__);
//    ...
//    logMessage.message("my second message").line(__LINE__);
//    ...
// }
//
// void MyClass::member(Int arg)
// {
//    LogMessage logMessage(LogOrigin("myClass", "member(Int arg)", WHERE));
//    ...
//    logMessage.message("my message").line(__LINE__);
//    ...
// }
// </srcblock>
// A more complete example is available in the module file 
// <linkto module="Logging">Logging.h</linkto>.
// </example>
//
// <todo asof="1996/07/23">
//   <li> Formerly we had a <src>MessageType</src> enum to go along with
//        <src>Priority</src>. It was removed because the categories weren't
//        sufficiently orthogonal. However the idea is probably sound and
//        we might eventually want to put such a categorization back.
//   <li> toRecord() and fromRecord() functions will be needed when we integrate
//        logging with Glish.
// </todo>
//
class LogMessage {
public:
    //# If you change this enum, edit toString()
    // An "importance" which is assigned to each LogMessage.
    enum Priority    { 
        // Low priority - primarily used for findding problems or tracing
        // execution.
        DEBUGGING, 
        DEBUG2, 
        DEBUG1, 
	// Most messages users see should have this priority. Use for 
	// "interesting" informational messages from normally executing
	// software.
	NORMAL5, 
	NORMAL4, 
	NORMAL3, 
	NORMAL2, 
	NORMAL1, 
	NORMAL, 
	// Use messages of warning level to flag things that are unusual and
	// might well be errors. Normally the software should proceed anyway
	// rather than throw an exception.
	WARN, 
	// Report on a problem detected by the software. Messages logged at
	// this priority will often be followed by a thrown exception.
	SEVERE};

    // Create a message with the given priority and the current time, and an
    // empty origin and message.
    LogMessage(Priority priority=NORMAL); 

    // Create a message with the given location and priority, the current time
    // and an empty message. This will likely be the most commonly used 
    // constructor when a given message is to be used several times in the same
    // function.
    LogMessage(const LogOrigin &sourceLocation, Priority priority=NORMAL);

    // Create a completely filled out LogMessage.
    LogMessage(const String &message, const LogOrigin &sourceLocation, 
	       Priority=NORMAL);

    // Make <src>this</src> LogMessage a copy of <src>other</src>. Note that 
    // the time is also copied over.
    // <group>
    LogMessage(const LogMessage &other);
    LogMessage &operator=(const LogMessage &other);
    // </group>

    ~LogMessage();

    // Get the message text.
    const String &message() const;

    // Set the message text. If <src>keepLastTime</src> is <src>True</src>, the
    // previous time will be used, otherwise the current time is used. This is
    // intended for messages that come out at essentially identical times to
    // aid in, e.g., Table selections.
    LogMessage &message(const String &message, Bool keepLastTime = False);

    // Get and set the line number in the
    // <linkto class="LogOrigin">LogOrigin</linkto>. While in principle you can
    // get and set this information through the <src>origin()</src> functions,
    // in practice it is convenient to be able to directly get at the line
    // number since it and the message text are usually the only things you
    // change in a particular LogMessage object. Generally you will set the
    // line number with the <src>__LINE__</src> macro.
    // <group>
    uInt line() const;
    LogMessage &line(uInt which);
    // </group>

    // Set the source location - usually this will be called with the
    // macro WHERE.
    LogMessage &sourceLocation(const SourceLocation *where);

    // Get and set the origin of this LogMessage. If you only need the line
    // number, use the <src>line()</src> or <src>sourceOrigin()</src>
    // functions instead.
    // <group>
    const LogOrigin &origin() const;
    LogMessage &origin(const LogOrigin &origin);
    // </group>

    // Get or change the priority of this LogMessage.
    // <group>
    Priority priority() const;
    LogMessage &priority(Priority which);
    // </group>

    // Returns the time at which the message text was created. This time is
    // presently "computer operating system" precision time, not high-precision
    // astronomical time.
    const Time &messageTime() const;

    // Normally you should not manually set the time, however there may be
    // rare circumstances where it is useful - for example if you have a single
    // <src>static</src> message that you want to send out at various times.
    LogMessage &messageTime(const Time &theTime);
    
    // Turn this entire LogMessage into a String.
    String toString() const;
    String toTermString() const;

    // Map the given priority into a String - so, for example, it can be stored
    // in a table.
    static const String &toString(Priority which);
private:
    String      message_p;
    LogOrigin   origin_p;
    Priority    priority_p;
    Time        time_p;

    // Provide common implementation for copy constructor and assignment
    // operator
    void copy_other(const LogMessage &other);
};

// <summary>
// Write a LogMessage to an ostream.
// </summary>
// Write a LogMessage as a string to an ostream. Merely calls
// <src>LogMessage::toString()</src>
// <group name=LogMessage_ostream>  
ostream &operator<<(ostream &os, const LogMessage &message);
// </group>



} //# NAMESPACE CASACORE - END

#endif
