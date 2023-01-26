//# VirtArrCol.cc: Base virtual column data manager class
//# Copyright (C) 1994,1995,1996,1999,2000
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

//# Includes
#include <casacore/tables/DataMan/VirtArrCol.h>
#include <casacore/tables/DataMan/DataManError.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

VirtualArrayColumnBase::~VirtualArrayColumnBase()
{}

bool VirtualArrayColumnBase::isWritable() const
    { return false; }

void VirtualArrayColumnBase::getScalarColumnV (ArrayBase&)
{ 
    throw DataManInvOper ("VirtualArrayColumn::getScalarColumnV not possible"
                          " for column " + columnName());
}
void VirtualArrayColumnBase::putScalarColumnV (const ArrayBase&)
{ 
    throw DataManInvOper ("VirtualArrayColumn::putScalarColumnV not possible"
                          " for column " + columnName());
}
void VirtualArrayColumnBase::getScalarColumnCellsV (const RefRows&,
                                                    ArrayBase&)
{ 
    throw DataManInvOper ("VirtualArrayColumn::getScalarColumnCellsV not possible"
                          " for column " + columnName());
}
void VirtualArrayColumnBase::putScalarColumnCellsV (const RefRows&,
                                                    const ArrayBase&)
{
    throw DataManInvOper ("VirtualArrayColumn::putScalarColumnCellsV not possible"
                          " for column " + columnName());
}

//# The default implementations of the shape functions throw
//# an exception.
void VirtualArrayColumnBase::setShapeColumn (const IPosition&)
{ 
    throw DataManInvOper ("VirtualArrayColumn::setShapeColumn not possible"
                          " for column " + columnName());
}
void VirtualArrayColumnBase::setShape (rownr_t, const IPosition&)
{
    throw DataManInvOper ("VirtualArrayColumn::setShape not possible"
                          " for column " + columnName());
}
bool VirtualArrayColumnBase::isShapeDefined (rownr_t)
{
    throw DataManInvOper ("VirtualArrayColumn::isShapeDefined not possible"
                          " for column " + columnName());
    return false;
}
IPosition VirtualArrayColumnBase::shape (rownr_t)
{
    throw DataManInvOper ("VirtualArrayColumn::shape not possible"
                          " for column " + columnName());
    return IPosition(0);
}

} //# NAMESPACE CASACORE - END
