//# <ClassFileName.h>: this defines <ClassName>, which ...
//# Copyright (C) 1997,1999,2000
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

#include <aips/Logging/LogIO.h>
#include <aips/Logging/LogSink.h>

#include <aips/Utilities/Assert.h>

#include <strstream.h>

LogIO::LogIO()
    : sink_p(), text_p(0)
{}

LogIO::LogIO(LogSink &sink)
    : sink_p(sink), text_p(0)
{}

LogIO::LogIO(const LogOrigin &or)
    : sink_p(), msg_p(or), text_p(0)
{}

LogIO::LogIO(const LogOrigin &or, LogSink &sink)
    : sink_p(sink),  msg_p(or), text_p(0)
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

void LogIO::post()
{
    if (text_p) {
	msg_p.message(*text_p);
	delete text_p;
	text_p = 0;
	sink_p.post(msg_p);
    }
}

void LogIO::postThenThrow()
{
    if (text_p == 0) {
	output() << "Unknown error!";
    }

    msg_p.message(*text_p);
    delete text_p;
    text_p = 0;
    sink_p.postThenThrow(msg_p);
}

void LogIO::priority(LogMessage::Priority which)
{
    msg_p.priority(which);
}

void LogIO::sourceLocation(const SourceLocation *where)
{
    msg_p.sourceLocation(where);
}

void LogIO::origin(const LogOrigin &or)
{
    msg_p.origin(or);
}

ostream &LogIO::output()
{
    if (!text_p) {
	text_p = new ostrstream;
	AlwaysAssert(text_p != 0, AipsError);
    }
    return *text_p;
}

//--------------------------- << operators


LogIO &operator<<(LogIO &os, LogIO::Command item)
{
    switch (item) {
    case LogIO::POST:          os.post(); break;
    case LogIO::EXCEPTION:     os.postThenThrow(); break;
    case LogIO::SEVERE:        os.priority(LogMessage::SEVERE); break;
    case LogIO::WARN:          os.priority(LogMessage::WARN); break;
    case LogIO::NORMAL:        os.priority(LogMessage::NORMAL); break;
    case LogIO::DEBUGGING:     os.priority(LogMessage::DEBUGGING); break;
    default:
	AlwaysAssert(0 != 0, AipsError); // NOTREACHED
    }
    return os;
}

LogIO &operator<<(LogIO &os, const SourceLocation *item)
{
    os.sourceLocation(item);
    return os;
}

LogIO &operator<<(LogIO &os, const LogOrigin &or)
{
    os.origin(or);
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

LogIO &operator<<(LogIO &os, Bool item)
{
    os.output() << (item ? 1:0);
    return os;
}
