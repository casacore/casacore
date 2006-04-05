//# MSSelectionError.cc: Error classes for the MSSelection classes
//# Copyright (C) 1994,1995,1996,1997,2000
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
//# $Id$

#include <ms/MeasurementSets/MSSelectionError.h>

namespace casa { //# NAMESPACE CASA - BEGIN

MSSelectionError::MSSelectionError (Category c)
: AipsError("MSSelection Error",c)
{}
MSSelectionError::MSSelectionError (const String& str,Category c)
: AipsError(str,c)
{}
MSSelectionError::~MSSelectionError () throw()
{}

void MSSelectionError::addMessage(String& mesg)
{
  message = message+mesg;
}

void MSSelectionError::changeMessage(String& mesg)
{
  message = mesg;
}

MSSelectionTimeError::MSSelectionTimeError (const String& str,Category c)
: MSSelectionError(str,c)
{}
MSSelectionTimeError::~MSSelectionTimeError () throw()
{}

MSSelectionTimeParseError::MSSelectionTimeParseError (const String& str,Category c)
: MSSelectionTimeError(str,c)
{}
MSSelectionTimeParseError::~MSSelectionTimeParseError () throw()
{}

} //# NAMESPACE CASA - END

