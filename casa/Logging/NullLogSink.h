//# NullLogSink.h: Throw away all messages.
//# Copyright (C) 1996,2003
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

#ifndef CASA_NULLLOGSINK_H
#define CASA_NULLLOGSINK_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Logging/LogSinkInterface.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Throw away all messages.
// </summary>

// <use visibility=local>

// <reviewed reviewer="wbrouw" date="1996/08/21" tests="tLogging.cc" demos="dLogging.cc">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=LogSinkInterface>LogSinkInterface</linkto>
// </prerequisite>
//
// <etymology>
// Null as in "empty" or "/dev/null".
// </etymology>
//
// <synopsis>
// <src>NullLogSink</src> is a trivial
// <linkto class=LogSinkInterface>LogSinkInterface</linkto> which merely throws
// away all its messages. It is not intended to be used directly, rather it
// should be used through <linkto class=LogSink>LogSink</linkto>.
// </synopsis>
//
// <example>
// See <linkto file="Logging.h">Logging.h</linkto>.
// </example>
//
// <motivation>
// For testing, or to prevent multiply logging (local and global) to 
// <src>cerr</src> (say).
// </motivation>
//
// <todo asof="1996/07/24">
//   <li> Nothing known.
// </todo>

class NullLogSink : public LogSinkInterface {
public:
    NullLogSink();
    explicit NullLogSink(LogMessage::Priority filter);
    explicit NullLogSink(const LogFilterInterface &filter);

    NullLogSink(const NullLogSink &other);
    NullLogSink &operator=(const NullLogSink &other);

    ~NullLogSink();

    // Always throws the message away, but it does return <src>True</src> or
    // <src>False</src> depending on whether or not <src>message</src> passes
    // the filter.
    virtual Bool postLocally(const LogMessage &message);

    // Returns the id for this class...
    static String localId( );
    // Returns the id of the LogSink in use...
    String id( ) const;
};


} //# NAMESPACE CASACORE - END

#endif


