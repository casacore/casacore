//# LogSink.h: Distribute LogMessages to their destination(s)
//# Copyright (C) 1996,2000
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

#if !defined(AIPS_LOG_SINK_H)
#define AIPS_LOG_SINK_H

#include <aips/aips.h>
#include <aips/Logging/LogSinkInterface.h>

#include <aips/Utilities/CountedPtr.h>
#include <aips/Utilities/PtrHolder.h>
#if defined(AIPS_STDLIB)
#include <iosfwd>
#else
class ostream;
#endif

// <summary>
// Distribute LogMessages to their destination(s)
// </summary>

// <use visibility=export>

// <reviewed reviewer="wbrouw" date="1996/08/21" tests="tLogging.cc" demos="dLogging.cc">
// </reviewed>

// <prerequisite> <li> <linkto class="LogMessage">LogMessage</linkto> <li>
// <linkto class="LogSinkInterface">LogSinkInterface</linkto>, if you are
// interested in extending the set of destinations a <src>LogMessage</src> can
// be sent.  </prerequisite>
//
// <etymology>
// Log as in "Log Book." Sink from its common usage ("source/sink") as a thing
// which can accept some substance or energy.
// </etymology>
//
// <synopsis>
// The LogSink class supplies the destination for 
// <linkto class="LogMessage">LogMessage</linkto>s. There are two destinations
// available through the <src>LogSink</src>
// <ol>
// <li> A <i>global</i> destination, which is shared by all LogSinks. The global
//      destination will typically be a GUI window or standard output.
// <li> A <i>local</i> destination which is intended to log changes to
//      particular dataset(s). The local destination will typically be an
//      AIPS++ <linkto class="Table">Table</linkto>.
// </ol>
// Normally the <src>post()</src> member function will be called which
// sends the message to both the global and local destinations, however one or
// the other may be chosen via <src>LogSink::postGlobally()</src> and
// <src>postLocally()</src> member functions.
//
// The global sink will normally be set by system library code (it defaults to
// using <src>cerr</src>. The type of local sink is defined at 
// construction time. Presently you can choose one of:
// <ol>
// <li> a <linkto class="NullLogSink">NullLogSink</linkto> which merely
//      discards the logging messages.
// <li> a <linkto class="StreamLogSink">StreamLogSink</linkto> which sends
//      the log messages to an <src>ostream</src> (typically <src>cerr</src>)
// <li> a <linkto class="TableLogSink">TableLogSink</linkto> which sends
//      the messages to an AIPS++ <linkto class=Table>Table</linkto>.
// </ol>
//
// Every <src>LogSink</src> has an attached 
// <linkto class=LogFilter>LogFilter</linkto> which is used to reject or pass
// messages. The local and global sinks have their own filters, so they can
// pass different message priorities (e.g., global <src>DEBUGGING</src> and
// local <src>NORMAL</src>). Generally applications code shouldn't change the
// global filter.
//
// </synopsis>
//
// <example>
// <srcblock>
// LogMessage logMessage(...);
// LogSink logger(LogMessage::NORMAL, "logtable");   // log locally to a 'logtable'
// logMessage.message("this is a message").line(__LINE__);
// logger.post(logMessage);                          // local and global
// </srcblock>
// More complete examples are in <linkto file=Logging.h>Logging.h</linkto>.
// </example>
//
// <h3>Advanced topics</h3>
// All possible sinks are derived from an abstract base class:
// <linkto class=LogSinkInterface>LogSinkInterface</linkto>. If you want to
// allow for logging to a different type of sink (i.e. different from
// a stream or Table) , you first need to derive a new class from
// <src>LogSinkInterface</src>, and then add a new constructor to
// <src>LogSink</src>.
//
// <src>LogSink</src> itself contains a reference to the actual object that
// disposes of the messages. Several <src>LogSink</src>'s can share the same
// actual sink via the copy constructor or assignment operator.
// <srcblock>
// LogSink logger1(LogMessage::NORMAL, "logtable");
// LogSink logger2(logger1);  // logger2 references logger1
// logger2.post(message);     // ends up in "logtable"
// </srcblock>
// You can even have different <src>LogFilter</src>'s attached to the different
// <src>LogSink</src>s.
//
// <motivation>
// Logging changes to data and informing users what the software is doing in
// detail.
// </motivation>
//
// <todo asof="1996/07/24">
//   <li> More sink types - in particular to Glish.
//   <li> A "tee" Sink type might be useful.
// </todo>

class LogSink : public LogSinkInterface
{
public:
    //#If you add more sink types, modify the <ol> in the synopsis as well.
    // Create a null local sink that throws all messages away. If a filter
    // isn't defined, default to <src>NORMAL</src>.
    // <group>
    LogSink();
    LogSink(const LogFilter &filter);
    // </group>
    // Log to an ostream. It is the responsiblity of the caller to ensure that
    // <src>os</src> will last as long as the <src>LogSink</src>s that use it.
    // Normally you would use <src>&cerr</src> as the argument.
    LogSink(const LogFilter &filter, ostream *os);
    // Log to an AIPS++ <linkto class=Table>Table</linkto>. If fileName does
    // not exist it will be created, if it does exist it will be appended to.
    // The description of the table and some useful utility functions
    // (for manipulating log table descriptions) for log tables are
    // available in <linkto class=TableLogSink>TableLogSink</linkto>.
    LogSink(const LogFilter &filter, const String &fileName);

    // Make a referencing copy of <src>other</src>. That is, if you post a
    // message to the new object, it behaves as if you had posted it to the
    // old one (so long as their filters are the same).
    // <group>
    LogSink(const LogSink &other);
    LogSink &operator=(const LogSink &other);
    // </group>

    ~LogSink();

    // Send <src>message</src> to both the local and global sink. Return
    // <src>True</src> if it passes either of them.
    Bool post(const LogMessage &message);

    // Send <src>message</src> to the global sink only. Returns <src>True</src>
    // if it passes the filter.
    static Bool postGlobally(const LogMessage &message);
    // Send <src>message</src> to the local sink only. Returns <src>True</src>
    // if it passes the filter.
    virtual Bool postLocally(const LogMessage &message);

    // Post <src>message</src> and then throw an <src>AipsError</src> exception
    // containing <src>message.toString()</src>. It is always posted as a 
    // <src>SEVERE</src> priority message, no matter what 
    // <src>message.priority()</src> says.
    // <group>
    void postThenThrow(const LogMessage &message);
    static void postGloballyThenThrow(const LogMessage &message);
    // </group>

    //# Bring out of LogSinkInterface only for documentation purposes
    // Get or set the filter of this particular <src>LogSink</src>.
    // <group>
    virtual const LogFilter &filter() const;
    virtual LogSinkInterface &filter(const LogFilter &filter);
    // </group>

    // Change the sink that this <src>LogSink</src> actually uses.
    // <group>
    const LogSinkInterface &localSink() const;
    LogSinkInterface &localSink();
    LogSink &localSink(LogSinkInterface *&fromNew);
    // </group>

    // Get/set the global sink. The global sink defaults to using
    // <src>cerr</src>. Generally applications code shouldn't change the global
    // sink.
    // <group>
    static LogSinkInterface &globalSink();
    static void globalSink(LogSinkInterface *&fromNew);
    // </group>

    // Write any pending output.
    virtual void flush();
private:
    CountedPtr<LogSinkInterface> local_sink_p;
    static CountedPtr<LogSinkInterface> global_sink_p;

    // The following is a reference to the global sink. It is created to
    // ensure that the global sink is not destroyed before the last local
    // reference to it is destroyed. This can happen if you have a static
    // LogSink (or LogIO).
    CountedPtr<LogSinkInterface> local_ref_to_global_p;
};

#endif
