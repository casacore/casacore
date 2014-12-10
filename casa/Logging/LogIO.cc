//# LogIO.cc: this defines LogIO which provides a stream like interface for logging
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

#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Logging/LogSink.h>

#include <casacore/casa/Utilities/Assert.h>

#include <casacore/casa/sstream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

LogIO::LogIO()
    : sink_p(), text_p(0)
{}

LogIO::LogIO(LogSink &sink)
    : sink_p(sink), text_p(0)
{}

LogIO::LogIO(const LogOrigin &OR)
    : sink_p(), msg_p(OR), text_p(0)
{}

LogIO::LogIO(const LogOrigin &OR, LogSink &sink)
    : sink_p(sink),  msg_p(OR), text_p(0)
{}

LogIO::LogIO(const LogIO &other)
    : sink_p(other.sink_p), msg_p(other.msg_p), text_p(0)
{}

LogIO& LogIO::operator=(const LogIO &other)
{
    if (this != &other) {
	post();
	sink_p = other.sink_p;
	msg_p = other.msg_p;
    }
    return *this;
}

LogIO::~LogIO()
{
    if (text_p) {
	post();
    }
    text_p = 0;
}

void LogIO::post(LogMessage &amess)
{
	msg_p = amess;
	sink_p.post(msg_p);
}
void LogIO::post()
{
    if (text_p) {
	msg_p.message(*text_p);
	delete text_p;
	text_p = 0;
        sink_p.post(msg_p);
    }
    // Reset priority.
    msg_p.priority(LogMessage::NORMAL);
}

void LogIO::postLocally()
{
    if (text_p) {
	msg_p.message(*text_p);
	delete text_p;
	text_p = 0;
        sink_p.postLocally(msg_p);
    }
    // Reset priority.
    msg_p.priority(LogMessage::NORMAL);
}

void LogIO::preparePostThenThrow (const AipsError& x)
{
    if (! x.getMesg().empty()) {
        output() << "; " << x.getMesg();
    }
    if (text_p == 0) {
	output() << "Unknown error!";
    }
    msg_p.message(*text_p);
    // Reset priority before the post, because that'll make a copy and
    // thereafter throw an exception.
    msg_p.priority(LogMessage::NORMAL);
    delete text_p;
    text_p = 0;
}

void LogIO::priority(LogMessage::Priority which)
{
    msg_p.priority(which);
}

LogMessage::Priority LogIO::priority()
{
	return msg_p.priority();
}

void LogIO::sourceLocation(const SourceLocation *where)
{
    msg_p.sourceLocation(where);
}

void LogIO::origin(const LogOrigin &OR)
{
    msg_p.origin(OR);
}

ostream &LogIO::output()
{
    if (!text_p) {
	text_p = new ostringstream;
	AlwaysAssert(text_p != 0, AipsError);
    }
    return *text_p;
}

//--------------------------- << operators


LogIO &operator<<(LogIO &os, LogIO::Command item)
{
    switch (item) {
    case LogIO::POST:          os.post(); break;
    case LogIO::EXCEPTION:     os.postThenThrow(AipsError()); break;
    case LogIO::SEVERE:        os.priority(LogMessage::SEVERE); break;
    case LogIO::WARN:          os.priority(LogMessage::WARN); break;
    case LogIO::NORMAL:        os.priority(LogMessage::NORMAL); break;
    case LogIO::NORMAL1:       os.priority(LogMessage::NORMAL1); break;
    case LogIO::NORMAL2:       os.priority(LogMessage::NORMAL2); break;
    case LogIO::NORMAL3:       os.priority(LogMessage::NORMAL3); break;
    case LogIO::NORMAL4:       os.priority(LogMessage::NORMAL4); break;
    case LogIO::NORMAL5:       os.priority(LogMessage::NORMAL5); break;
    case LogIO::DEBUG1:        os.priority(LogMessage::DEBUG1); break;
    case LogIO::DEBUG2:        os.priority(LogMessage::DEBUG2); break;
    case LogIO::DEBUGGING:     os.priority(LogMessage::DEBUGGING); break;
    default:
	AlwaysAssert(0 != 0, AipsError); // NOTREACHED
    }
    return os;
}

void operator<< (LogIO &os, const AipsError& x)
{
    if (! x.getMesg().empty()) {
        os.output() << "; " << x.getMesg();
    }
    os.postThenThrow (x);
}

LogIO &operator<<(LogIO &os, const SourceLocation *item)
{
    if(os.priority() < LogMessage::NORMAL1 ||
       os.priority() > LogMessage::WARN )
       os.sourceLocation(item);
    return os;
}

LogIO &operator<<(LogIO &os, const LogOrigin &OR)
{
    os.origin(OR);
    return os;
}

LogIO &operator<<(LogIO &os, ostream &(*item)(ostream &))
{
    item(os.output());
    return os;
}

LogIO &operator<<(LogIO &os, const String &item)
{
    os.output() << item;
    return os;
}

LogIO &operator<<(LogIO &os, const char *item)
{
    os.output() << item;
    return os;
}

LogIO &operator<<(LogIO &os, Double item)
{
    os.output() << item;
    return os;
}

LogIO &operator<<(LogIO &os, Complex item)
{
    os.output() << item;
    return os;
}

LogIO &operator<<(LogIO &os, DComplex item)
{
    os.output() << item;
    return os;
}

LogIO &operator<<(LogIO &os, Int item)
{
    os.output() << item;
    return os;
}

LogIO &operator<<(LogIO &os, uInt item)
{
    os.output() << item;
    return os;
}

LogIO &operator<<(LogIO &os, uLong item)
{
    os.output() << item;
    return os;
}

LogIO &operator<<(LogIO &os, Long item)
{
    os.output() << item;
    return os;
}

LogIO &operator<<(LogIO &os, Bool item)
{
    os.output() << (item ? 1:0);
    return os;
}

} //# NAMESPACE CASACORE - END

