//# StreamLogSink.cc: Send log messages to an ostream.
//# Copyright (C) 1996,2001,2003
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

#include <casacore/casa/Logging/StreamLogSink.h>
#include <casacore/casa/Logging/LogFilter.h>
#include <casacore/casa/iostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

String StreamLogSink::localId( ) {
    return String("StreamLogSink");
}

String StreamLogSink::id( ) const {
    return String("StreamLogSink");
}

StreamLogSink::StreamLogSink(ostream *theStream, bool del)
    : stream_p(theStream), deleteStream(del)
{
    if (stream_p == 0) {
        stream_p = &cerr;
    }
}

StreamLogSink::StreamLogSink(LogMessage::Priority filter,
			     ostream *theStream, bool del)
    : LogSinkInterface(LogFilter(filter)), stream_p(theStream), deleteStream(del)
{
    if (stream_p == 0) {
        stream_p = &cerr;
    }
}

StreamLogSink::StreamLogSink(const LogFilterInterface &filter,
			     ostream *theStream, bool del)
    : LogSinkInterface(filter), stream_p(theStream), deleteStream(del)
{
    if (stream_p == 0) {
        stream_p = &cerr;
    }
}

StreamLogSink::StreamLogSink(const StreamLogSink &other)
    : LogSinkInterface(other), stream_p(other.stream_p), deleteStream(false)
{
    // Nothing
}

StreamLogSink &StreamLogSink::operator=(const StreamLogSink &other)
{
    if (this != &other) {
        LogSinkInterface &This = *this;
	This = other;
	stream_p = other.stream_p;
        deleteStream = false;
    }
    return *this;
}

StreamLogSink::~StreamLogSink()
{
    if(deleteStream) delete stream_p;
    stream_p = 0;
}

Bool StreamLogSink::postLocally(const LogMessage &message) 
{
    Bool doPost = filter().pass(message);
    if (doPost) {
	LogOrigin theOrigin(message.origin());
	theOrigin.taskName(LogSinkInterface::taskName);
	const_cast<LogMessage &>(message).origin(theOrigin);
        // Cast away const
	*stream_p << message;
    }
    return doPost;
}

void StreamLogSink::flush(Bool)
{
    if(stream_p) stream_p->flush();
}

} //# NAMESPACE CASACORE - END

