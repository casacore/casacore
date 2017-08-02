//# VirtScaColBase.cc: Base virtual column data manager class
//# Copyright (C) 1994,1995,1996,1999
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

//# Includes
#include <casacore/tables/DataMan/VirtScaCol.h>
#include <casacore/tables/DataMan/DataManError.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/ValTypeId.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN


VirtualScalarColumnBase::~VirtualScalarColumnBase()
{}

Bool VirtualScalarColumnBase::isWritable() const
    { return False; }


void VirtualScalarColumnBase::getArrayV (uInt, ArrayBase&)
{ 
    throw DataManInvOper ("VirtualScalarColumn::getArrayV not possible"
                          " for column " + columnName());
}

void VirtualScalarColumnBase::putArrayV (uInt, const ArrayBase&)
{ 
    throw DataManInvOper ("VirtualScalarColumn::putArrayV not possible"
                          " for column " + columnName());
}

void VirtualScalarColumnBase::getArrayColumnV (ArrayBase&)
{ 
    throw DataManInvOper ("VirtualScalarColumn::getArrayColumnV not possible"
                          " for column " + columnName());
}

void VirtualScalarColumnBase::putArrayColumnV (const ArrayBase&)
{ 
    throw DataManInvOper ("VirtualScalarColumn::putArrayColumnV not possible"
                          " for column " + columnName());
}

void VirtualScalarColumnBase::getArrayColumnCellsV (const RefRows&, ArrayBase&)
{ 
    throw DataManInvOper ("VirtualScalarColumn::getArrayColumnCellsV not possible"
                          " for column " + columnName());
}

void VirtualScalarColumnBase::putArrayColumnCellsV (const RefRows&, const ArrayBase&)
{ 
    throw DataManInvOper ("VirtualScalarColumn::putArrayColumnCellsV not possible"
                          " for column " + columnName());
}

void VirtualScalarColumnBase::getSliceV (uInt, const Slicer&, ArrayBase&)
{ 
    throw DataManInvOper ("VirtualScalarColumn::getSliceV not possible"
                          " for column " + columnName());
}

void VirtualScalarColumnBase::putSliceV (uInt, const Slicer&, const ArrayBase&)
{ 
    throw DataManInvOper ("VirtualScalarColumn::putSliceV not possible"
                          " for column " + columnName());
}

void VirtualScalarColumnBase::getColumnSliceV (const Slicer&, ArrayBase&)
{ 
    throw DataManInvOper ("VirtualScalarColumn::getColumnSliceV not possible"
                          " for column " + columnName());
}

void VirtualScalarColumnBase::putColumnSliceV (const Slicer&, const ArrayBase&)
{ 
    throw DataManInvOper ("VirtualScalarColumn::putColumnSliceV not possible"
                          " for column " + columnName());
}

void VirtualScalarColumnBase::getColumnSliceCellsV (const RefRows&,
                                                    const Slicer&, ArrayBase&)
{ 
    throw DataManInvOper ("VirtualScalarColumn::getColumnSliceCellsV not possible"
                          " for column " + columnName());
}

void VirtualScalarColumnBase::putColumnSliceCellsV (const RefRows&,
                                                    const Slicer&, const ArrayBase&)
{ 
    throw DataManInvOper ("VirtualScalarColumn::putColumnSliceCellsV not possible"
                          " for column " + columnName());
}


} //# NAMESPACE CASACORE - END
