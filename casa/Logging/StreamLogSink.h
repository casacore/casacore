//# StreamLogSink.h: Send log messages to an ostream.
//# Copyright (C) 1996,2000,2001,2003
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

#ifndef CASA_STREAMLOGSINK_H
#define CASA_STREAMLOGSINK_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Logging/LogSinkInterface.h>

#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Send log messages to an ostream.
// </summary>

// <use visibility=local>

// <reviewed reviewer="wbrouw" date="1996/08/21" tests="tLogging.cc" demos="dLogging.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LogSinkInterface>LogSinkInterface</linkto>
//   <li> ostream
// </prerequisite>
//
// <etymology>
// "Stream" from the family of standard C++ I/O classes.
// </etymology>
//
// <synopsis>
// <src>StreamLogSink</src> is a straightforward
// <linkto class=LogSinkInterface>LogSinkInterface</linkto> which sends its
// messages to an <src>ostream</src> (typically <src>cerr</src>) which it is 
// given at construction time. It is not intended to be used directly, rather it
// should be used through <linkto class=LogSink>LogSink</linkto>.
// </synopsis>
//
// <example>
// See <linkto file="Logging.h">Logging.h</linkto>.
// </example>
//
// <motivation>
// Writing to standard output or error will be a common way of displaying log
// messages.
// </motivation>
//
// <todo asof="1996/07/24">
//   <li> Nothing known.
// </todo>



class StreamLogSink : public LogSinkInterface {
public:
    // Defaults to <src>cerr</src> if no stream is supplied.  The caller is
    // responsible for ensuring that the supplied <src>ostream</src> ostream
    // lives at least as long as this sink. If not filter is supplied,
    // <src>NORMAL</src> is used.
    // <group>
    explicit StreamLogSink(ostream *theStream = 0, bool deleteStream = false);
    explicit StreamLogSink(LogMessage::Priority filter,
			   ostream *theStream = 0, bool deleteStream = false);
    explicit StreamLogSink(const LogFilterInterface &filter,
			   ostream *theStream = 0, bool deleteStream = false);
    // </group>

    // Make a copy of <src>other</src>. After copying, both objects will post
    // to the same stream.
    // <group>
    StreamLogSink(const StreamLogSink &other);
    StreamLogSink &operator=(const StreamLogSink &other);
    // </group>

    ~StreamLogSink();

    // Write <src>message</src> to the stream if it passes the filter. Works
    // by calling <src>operator<<(ostream &,const LogMesssage&)</src>.
    virtual Bool postLocally(const LogMessage &message);

    // write any pending output.
    virtual void flush (Bool global=True);

    // Returns the id for this class...
    static String localId( );
    // Returns the id of the LogSink in use...
    String id( ) const;

private:
    ostream *stream_p;
    bool deleteStream;
};


} //# NAMESPACE CASACORE - END

#endif
