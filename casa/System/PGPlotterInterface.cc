//# PGPlotterInterface.cc: Abstract base class for PGPLOT style plotting.
//# Copyright (C) 1997,2001
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

#include <casa/System/PGPlotterInterface.h>
#include <casa/Logging/LogIO.h>

namespace casa { //# NAMESPACE CASA - BEGIN

PGPlotterInterface::~PGPlotterInterface()
{
    // Nothing
}

void PGPlotterInterface::message(const String &text)
{
    // On the logger we want to suppres blank lines.
    if (text != "") {
	LogIO os;
	os << text << LogIO::POST;
    }
}

Bool PGPlotterInterface::isAttached() const
{
    return True;
}

} //# NAMESPACE CASA - END

