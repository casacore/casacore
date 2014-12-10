//# DataManError.cc: Storage manager error classes
//# Copyright (C) 1994,1995,1996,2000,2003
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

#include <casacore/tables/DataMan/DataManError.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Implementation of DataManager error classes.

DataManError::DataManError ()
: AipsError("Table DataManager error")
{ ; }
DataManError::DataManError (const String& str)
: AipsError("Table DataManager error: " + str)
{ ; }
DataManError::~DataManError () throw()
{ ; }


DataManInternalError::DataManInternalError (const String& str)
: DataManError("Internal error: " + str)
{ ; }
DataManInternalError::~DataManInternalError () throw()
{ ; }


DataManUnknownCtor::DataManUnknownCtor (const String& msg)
: DataManError(msg)
{}
DataManUnknownCtor::~DataManUnknownCtor () throw()
{}


DataManInvDT::DataManInvDT ()
: DataManError ("Invalid data type")
{ ; }
DataManInvDT::DataManInvDT (const String& name)
: DataManError ("Invalid data type when accessing column " + name)
{ ; }
DataManInvDT::~DataManInvDT () throw()
{ ; }


DataManInvOper::DataManInvOper ()
: DataManError ("Invalid operation")
{ ; }
DataManInvOper::DataManInvOper (const String& s)
: DataManError ("Invalid operation: " + s)
{ ; }
DataManInvOper::~DataManInvOper () throw()
{ ; }


DataManUnknownVirtualColumn::DataManUnknownVirtualColumn
                                            (const String& columnName,
					     const String& engineName)
: DataManError ("column " + columnName +
		" is unknown to virtual column engine " + engineName)
{ ; }
DataManUnknownVirtualColumn::~DataManUnknownVirtualColumn () throw()
{ ; }


TSMError::TSMError (const String& s)
: DataManError ("TiledStMan: " + s)
{ ; }
TSMError::~TSMError () throw()
{ ; }

} //# NAMESPACE CASACORE - END

