//# VirtColEng.cc: Abstract base class for virtual column handling
//# Copyright (C) 1994,1995,1996,2001
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

#include <casacore/tables/DataMan/VirtColEng.h>
#include <casacore/tables/DataMan/DataManError.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

VirtualColumnEngine::~VirtualColumnEngine()
{}


Bool VirtualColumnEngine::isStorageManager() const
    { return False; }

Bool VirtualColumnEngine::canAddRow() const
    { return True; }
void VirtualColumnEngine::addRow (uInt)
    {}
Bool VirtualColumnEngine::canRemoveRow() const
    { return True; }
void VirtualColumnEngine::removeRow (uInt)
    {}

//# Create, open, prepare and close do nothing unless implemented in the
// derived class.
Bool VirtualColumnEngine::flush (AipsIO&, Bool)
{ return False; }
void VirtualColumnEngine::create (uInt)
{}
void VirtualColumnEngine::open (uInt, AipsIO&)
{}
void VirtualColumnEngine::resync (uInt)
{}
void VirtualColumnEngine::prepare()
{}
void VirtualColumnEngine::deleteManager()
{}


DataManagerColumn* VirtualColumnEngine::makeScalarColumn
                                               (const String& columnName,
					        int, const String&)
{
    throw (DataManUnknownVirtualColumn (columnName, dataManagerType()));
    return 0;
}
DataManagerColumn* VirtualColumnEngine::makeIndArrColumn
                                               (const String& columnName,
					        int, const String&)
{
    throw (DataManUnknownVirtualColumn (columnName, dataManagerType()));
    return 0;
}

//# Creating a direct array is by default the same as creating
//# an indirect array.
DataManagerColumn* VirtualColumnEngine::makeDirArrColumn
                                               (const String& columnName,
					        int dataType,
						const String& dataTypeId)
    { return makeIndArrColumn (columnName, dataType, dataTypeId); }

} //# NAMESPACE CASACORE - END

