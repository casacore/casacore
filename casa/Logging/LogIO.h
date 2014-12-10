//# LogIO.h: ostream-like interface to creating log messages.
//# Copyright (C) 1997,1999,2000,2001,2003
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

#ifndef CASA_LOGIO_H
#define CASA_LOGIO_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Logging/LogMessage.h>
#include <casacore/casa/Logging/LogSink.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/iosstrfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class LogSink;
class LogOrigin;

// <summary>
// ostream-like interface to creating log messages.
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tLogIO.cc" demos="dLogging.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LogSink>LogSink</linkto> class
//   <li> <linkto class=LogMessage>LogMessage</linkto> class
//   <li> <linkto class=LogMessage>LogOrigin</linkto> class
// </prerequisite>
//
// <etymology>
// <src>Log</src> message, <src>I</src>nput/<src>O</src>utput.
// </etymology>
//
// <synopsis>
// LogIO is intended to be used in a way similar to the ostream class.
// However, rather than sending it's output to a file or stdout, it bundles
// its output up into <linkto class=LogMessage>LogMessage</linkto> objects 
// and posts them to a <linkto class=LogSink>LogSink</linkto>.
//
// When you use the "<<" operator on a LogIO, you are building up a log message
// inside the LogIO object. The message is posted when:
// <ol>
//    <li> <src>LogIO::POST()</src> is called
//    <li> You send the <src>LogIO::POST</src> or <src>LogIO::EXCEPTION</src>
//         commands to the LogIO with the shift (<src> << </src>) command.
//    <li> The LogIO object is destructed.
// </ol>
// Note that log messages may span multiple lines, so sending the LogIO a
// newline (via "\n" or endl) does not force the message to be emitted.
// </synopsis>
//
// <example>
// A LogIO may be created in the following ways:
// <srcblock>
//    LogIO   os;
// </srcblock>
// Here, <src>os</src> is attached to the global log sink, and no origin
// information is set.
//
// <srcblock>
//    TableLogSink tab(...);
//    LogIO   os(tab);
// </srcblock>
// Here, <src>os</src> is attached to <src>tab</src> (and also to the global
// log sink since every sink's <src>post</src> also calls the global sink's
// <src>post</src>).
//
// 
// <srcblock>
//    LogIO   os(LogOrigin("class", "func(args)", WHERE));
// </srcblock>
// Here, <src>os</src> is attached to the global sink and the origin
// information is set to <src>class::func(args)</src> and the line number and
// source file information is set (with <src>WHERE</src>).
//
// <srcblock>
//    TableLogSink tab(...);
//    LogIO   os(LogOrigin("class", "func(args)", WHERE), tab);
// </srcblock>
// Here all the above information is set.
//
// Once you have a <src>LogIO</src>, using it is pretty simple:
// <srcblock>
//   os << "Every good boy deserves" << 5 << " pieces of fudge!";
// </srcblock>
//
// This accumulates the message but does not send it. If you want to force it
// to be sent you can do so with either of the following methods:
// <srcblock>
//    os << LogIO::POST;     // From the Commands enum
//    os.post();             // Member function
// </srcblock>
// Note that after a post the priority is reset to NORMAL.
//
// If you want to change the level of the message you can also do so with the
// shift operator:
// <srcblock>
//   os << LogIO::DEBUGGING << "Boring message" << 
//         LogIO::SEVERE << "Error!" << LogIO::POST;
// </srcblock>
// Note that changing the priority changes the priority of the entire
// message. The message does not get posted until the POST is done.
// So in the above example the DEBUGGING priority does not do anything
// because the priority is overwritten by the SEVERE one.
//
// You can also change the origin information with the << operator:
// <srcblock>
//    os << LogOrigin("class", "func(args)");
//    os << WHERE;
// </srcblock>
//
// A class which has an operator<< to std::ostream but not LogIO can be handled
// as follows:
// <srcblock>
//   os << LogIO::SEVERE << " at ";
//   os.output() << MEpoch::Convert(time_p, MEpoch::Ref(MEpoch::UTC))();
//   os << LogIO::POST;
// </srcblock>
// </example>
//
// <motivation>
// The earlier method of creating log messages solely through LogSink and
// LogMessage required the programmer to type in more lines of code than
// this solution. Also, this interface makes it easy to drop log messages
// into existing code that uses ostreams.
// </motivation>
//
// <todo asof="1997/01/29">
//   <li> Add << operators for all classes that have ostream<< defined.
//        (We could probably do it with a template, but might result
//        in ambiguity).
//   <li> Have a function for changing the LogSink only? (You can get
//        much the same effect with operator=).
//        them?
// </todo>

class LogIO
{
public:
    // Special commands to the LogIO object
    enum Command {
	// Post the accumulated message. Equivalent to calling LogIO::post().
	POST, 
	// Post the accumulated message then throw an exception.
	// Always posts the message at SEVERE priority. Equivalent to calling 
	// LogIO::postThenThrow().
	EXCEPTION, 
	// Change the message priority to SEVERE.
	SEVERE, 
	// Change the message priority to WARN.
	WARN, 
	// Change the message priority to NORMAL.
	NORMAL, 
	NORMAL1, 
	NORMAL2, 
	NORMAL3, 
	NORMAL4, 
	NORMAL5, 
	// Change the message priority to DEBUGGING.
	DEBUG1,
	DEBUG2,
	DEBUGGING};

    // Attach this LogIO object to the global sink with no origin information.
    LogIO();
    // Attach this LogIO object to the supplied sink. A referencing copy of
    // the sink is made inside the LogIO object, so you do not need to worry
    // about memory management.
    LogIO(LogSink &sink);
    // Attach this LogIO object to the supplied origin and global sink.
    LogIO(const LogOrigin &OR);
    // Attach this LogIO object to the supplied origin and sink.
    LogIO(const LogOrigin &OR, LogSink &sink);
    
    // Copying uses reference semantics, i.e. the same sink will be shared
    // by both copies.
    // <group>
    LogIO(const LogIO &other);
    LogIO &operator=(const LogIO &other);
    // </group>

    // The destructor will post any accumulated message that has not already
    // been posted.
    ~LogIO();
    
    // Post the accumulated message.  If you wish, you can post the messages
    // only locally to the sink.
    // After the post the priority is reset to NORMAL.
    void post();
    void post(LogMessage &amess);

    // Post the accumulated message locally.
    // After the post the priority is reset to NORMAL.
    void postLocally();

    // Post the accumulated message at SEVERE priority and then throw an
    // exception.
    // After the post the priority is reset to NORMAL.
    template<typename EXC> void postThenThrow (const EXC& exc)
      { preparePostThenThrow(exc); sink_p.postThenThrow (msg_p, exc); }

    // Change the priority of the message. It does NOT post the accumulated
    // message at the old priority first.
    void priority(LogMessage::Priority which);
    LogMessage::Priority priority();
    // Change the location in the origin. Almost always this is called with the
    // macro WHERE as its argument.
    void sourceLocation(const SourceLocation *where);
    // Change the origin of the accumulated message.
    void origin(const LogOrigin &origin);

    // Acumulate output in this ostream.
    ostream& output();

    // Occasionally it is useful to interrogate the local log sink.
    LogSinkInterface &localSink();
    const LogSinkInterface &localSink() const;

private:
    // Prepare message stream for postThenThrow function.
    void preparePostThenThrow (const AipsError& x);

    LogSink sink_p;
    LogMessage msg_p;
    ostringstream *text_p;

};

// <summary>
// Functions to send commands to a LogIO object.
// </summary>
// The following commands don't change the accumulated message, rather they
// send commands to the LogIO object, either to:
// <ol>
//   <li>post the current message: <src>os << "message" << LogIO::POST;</src>
//   <li>post the current message and then throw an exception: 
//       <src>os << "error" << LogIO::EXCEPTION;</src>
//   <li> Change the priority of the current message:
//        <src>os << LogIO::DEBUGGING;</src>
//   <li> Change the origin of the message:
//        <srcblock>
//          os << LogOrigin(...);
//          os << WHERE;             // Changes only source file/line number
//        </srcblock>
// </ol>
// <group name=command>
LogIO &operator<<(LogIO &os, LogIO::Command item);
LogIO &operator<<(LogIO &os, const SourceLocation *item);
LogIO &operator<<(LogIO &os, const LogOrigin &OR);
// </group>

// <summary>
// Functions to accumulate text in the output message.
// </summary>
// Accumulate text in the output message. The last entry is for things like 
// <src>endl</src>.
// <group name=output>
LogIO &operator<<(LogIO &os, const String &item);
LogIO &operator<<(LogIO &os, const char *item);
LogIO &operator<<(LogIO &os, Double item);
LogIO &operator<<(LogIO &os, Complex item);
LogIO &operator<<(LogIO &os, DComplex item);
LogIO &operator<<(LogIO &os, Int item);
LogIO &operator<<(LogIO &os, uInt item);
LogIO &operator<<(LogIO &os, uLong item);
LogIO &operator<<(LogIO &os, Long item);
LogIO &operator<<(LogIO &os, Bool item);
LogIO &operator<<(LogIO &os, ostream &(*item)(ostream &));
// </group>

inline LogSinkInterface &LogIO::localSink()
{
    return sink_p.localSink();
}

inline const LogSinkInterface &LogIO::localSink() const
{
    return sink_p.localSink();
}


} //# NAMESPACE CASACORE - END

#endif
