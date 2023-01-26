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

#include <casacore/tables/DataMan/VirtColEng.h>
#include <casacore/tables/DataMan/DataManError.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

VirtualColumnEngine::~VirtualColumnEngine()
{}


bool VirtualColumnEngine::isStorageManager() const
    { return false; }

bool VirtualColumnEngine::canAddRow() const
    { return true; }
void VirtualColumnEngine::addRow64 (rownr_t)
    {}
bool VirtualColumnEngine::canRemoveRow() const
    { return true; }
void VirtualColumnEngine::removeRow64 (rownr_t)
    {}

//# Create, open, prepare and close do nothing unless implemented in the
// derived class.
bool VirtualColumnEngine::flush (AipsIO&, bool)
{ return false; }
void VirtualColumnEngine::create64 (rownr_t)
{}
rownr_t VirtualColumnEngine::open64 (rownr_t nrow, AipsIO&)
  { return nrow; }
rownr_t VirtualColumnEngine::resync64 (rownr_t nrow)
  { return nrow; }
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

