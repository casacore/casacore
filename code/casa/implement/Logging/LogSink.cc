//# LogSink.cc: Distribute LogMessages to their destination(s)
//# Copyright (C) 1996,1999
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

#include <aips/Logging/LogSink.h>
#include <aips/Logging/LogMessage.h>

#include <aips/Logging/NullLogSink.h>
#include <aips/Logging/StreamLogSink.h>

#include <aips/Utilities/PtrHolder.h>
#include <aips/Utilities/Assert.h>

#include <iostream.h>

CountedPtr<LogSinkInterface> 
  LogSink::global_sink_p(new StreamLogSink(LogMessage::NORMAL, &cerr));

LogSink::LogSink()
: LogSinkInterface(LogFilter(LogMessage::NORMAL))
{
    local_sink_p = new NullLogSink(LogMessage::DEBUGGING);
    local_ref_to_global_p = LogSink::global_sink_p;
    AlwaysAssert(! local_sink_p.null(), AipsError);
}

LogSink::LogSink(const LogFilter &filter)
  : LogSinkInterface(filter),
    local_sink_p(new NullLogSink(LogMessage::DEBUGGING)),
    local_ref_to_global_p(global_sink_p)
{
    AlwaysAssert(! local_sink_p.null(), AipsError);
}

LogSink::LogSink(const LogFilter &filter, ostream *os)
  : LogSinkInterface(filter), 
    local_sink_p(new StreamLogSink(LogMessage::DEBUGGING, os)),
    local_ref_to_global_p(global_sink_p)
{
    AlwaysAssert(! local_sink_p.null(), AipsError);
}

//!!! The LogSink constructor that makes a table is in LogSink2.cc. This is done
//!!! to avoid linking in the table system if you only log to a stream.

LogSink::LogSink(const LogSink &other) 
  : LogSinkInterface(other), local_sink_p(other.local_sink_p),
    local_ref_to_global_p(global_sink_p)
{
    // Nothing
}

LogSink &LogSink::operator=(const LogSink &other)
{
    if (this != &other) {
	local_ref_to_global_p = other.local_ref_to_global_p;
        local_sink_p = other.local_sink_p;
	LogSinkInterface &This = *this;
	This = other;
    }
    return *this;
}

LogSink::~LogSink()
{
       flush();
}

Bool LogSink::post(const LogMessage &message) 
{
    Bool postedLocally =   postLocally(message);
    Bool postedGlobally =  postGlobally(message);
    return ToBool(postedLocally || postedGlobally);
}

Bool LogSink::postGlobally(const LogMessage &message)
{
    Bool posted = False;
    AlwaysAssert(!global_sink_p.null(), AipsError);
    if (global_sink_p->filter().pass(message)) {
        posted = globalSink().postLocally(message);
    }
    return posted;
}

void LogSink::postThenThrow(const LogMessage &message) 
{
    // Try not to copy message since a severe error might be caused by
    // out of memory
    if (message.priority() == LogMessage::SEVERE) {
        post(message);
	flush();
	throw(AipsError(message.toString()));
    } else {
        LogMessage messageCopy(message);
	messageCopy.priority(LogMessage::SEVERE);
	post(messageCopy);
	throw(AipsError(messageCopy.toString()));
    }
}

void LogSink::postGloballyThenThrow(const LogMessage &message)
{
    // Try not to copy message since a severe error might be caused by
    // out of memory
    if (message.priority() == LogMessage::SEVERE) {
        postGlobally(message);
	globalSink().flush();
	throw(AipsError(message.toString()));
    } else {
        LogMessage messageCopy(message);
	messageCopy.priority(LogMessage::SEVERE);
	postGlobally(messageCopy);
	globalSink().flush();
	throw(AipsError(messageCopy.toString()));
    }
}

const LogFilter &LogSink::filter() const
{
    return this->LogSinkInterface::filter();
}

LogSinkInterface &LogSink::filter(const LogFilter &thefilter)
{
    return this->LogSinkInterface::filter(thefilter);
}

const LogSinkInterface &LogSink::localSink() const
{
    return *(local_sink_p);
}

LogSinkInterface &LogSink::localSink()
{
    return *(local_sink_p);
}

LogSink &LogSink::localSink(LogSinkInterface *&fromNew)
{
    local_sink_p = fromNew;
    fromNew = 0;
    AlwaysAssert(!local_sink_p.null(), AipsError);
    return *this;
}

LogSinkInterface &LogSink::globalSink()
{
    return *global_sink_p;
}

void LogSink::globalSink(LogSinkInterface *&fromNew)
{
    global_sink_p.replace(fromNew);
    fromNew = 0;
    AlwaysAssert(!global_sink_p.null(), AipsError);
}

Bool LogSink::postLocally(const LogMessage &message) 
{
    if (filter().pass(message)) {
        return local_sink_p->postLocally(message);
    } else {
        return False;
    }
}

void LogSink::flush()
{
    if(!local_sink_p.null())
       local_sink_p->flush();
    if(!global_sink_p.null())
       global_sink_p->flush();
}
