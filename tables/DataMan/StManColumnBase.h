//# StManColumnBase.h: Base storage manager column class
//# Copyright (C) 2019
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

#ifndef TABLES_STMANCOLUMNBASE_H
#define TABLES_STMANCOLUMNBASE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/DataManagerColumn.h>
#include <casacore/casa/Arrays/ArrayFwd.h>
#include <casacore/casa/Utilities/DataType.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Base table column storage manager class
// </summary>

// <use visibility=local>

// <reviewed reviewer="Gareth Hunt" date="94Nov17" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> DataManagerColumn
// </prerequisite>

// <etymology>
// StManColumnBase is the base class for a storage manager.
// </etymology>

// <synopsis> 
// StManColumnBase is the abstract base class to handle a column in all
// kind of storage managers. It is derived from DataManagerColumn
// and implements a few (virtual) functions handling the column's
// data type for derived storage manager column classes.
// </synopsis> 

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
// </todo>


class StManColumnBase : public DataManagerColumn
{
public:

    // Default constructor.
    StManColumnBase (int dataType);

    ~StManColumnBase();

    // Test if the given data type is supported by storage managers.
    // It is used by the function Table::isNativeDataType.
    static bool isNativeDataType (int dtype);

    // Return the data type of the column.
    // <group>
    virtual int dataType() const;
    DataType dtype() const
      { return dtype_p; }
    // </group>

    // Return the size of an element of the column's data type.
    int32_t elemSize() const
      { return elemSize_p; }

private:
    // The object cannot be copied.
    StManColumnBase (const StManColumnBase&);

    // The object cannot be assigned to.
    StManColumnBase& operator= (const StManColumnBase&);

private:
    // The data type of the column.
    DataType dtype_p;
    // The size of an element of this data type.
    int32_t      elemSize_p;
};




} //# NAMESPACE CASACORE - END

#endif
