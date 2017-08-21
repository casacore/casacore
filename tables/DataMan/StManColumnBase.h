//# StManColumnBase.h: Base storage manager column class
//# Copyright (C) 1994,1995,1996,1998,2002
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

#ifndef TABLES_STMANCOLUMNBASE_H
#define TABLES_STMANCOLUMNBASE_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/DataManagerColumn.h>
#include <casacore/casa/Utilities/DataType.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template<class T> class Vector;


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
// StManColumn handles a column for a storage manager.
// </etymology>

// <synopsis> 
// StManColumn is the abstract base class to handle a column in all
// kind of storage managers. It is derived from DataManagerColumn
// and implements several virtual functions for derived storage
// manager column classes (like StManColumnAipsIO).
//
// All get and put functions (except for single scalars) in the abstract
// base class DataManagerColumn have a generic ArrayBase& data argument.
// This is done to allow arbitrary typed arguments. This can be done
// because the data type of the derived class always matches the
// type of the data argument.
// However, at one time the ArrayBase& has to be casted to the exact type.
// Storage managers only support the standard data types; therefore
// it is possible to do the cast in a base class. This concentrates
// the burden in one class and allows the derived classes to work
// with correctly typed arguments.
// The price is an extra virtual function call, but that is not a
// problem for (expensive) operations on arrays.
// It is not done for single scalars, because that price may be too high.
// 
// See StManColumnAipsIO for the get/put functions required in a storage
// manager column class handling scalars.
// See StManColumnArrayAipsIO for the get/put functions required in a
// storage manager column class handling arrays. This class also
// contains the shape functions for direct arrays, while
// StManColumnIndArrayAipsIO contains the shape functions for indirec
// arrays.
//
// StManColumn also contains the data type of the column (which it
// gets from the derived classes at construction time) and implements
// the function dataType on behalf of its derived classes.
// </synopsis> 

// <motivation>
// Making life easier for the derived classes.
// </motivation>

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
    static Bool isNativeDataType (int dtype);

    // Return the data type of the column.
    // <group>
    virtual int dataType() const;
    DataType dtype() const
      { return dtype_p; }
    // </group>

    // Return the size of an element of the column's data type.
    Int elemSize() const
      { return elemSize_p; }

private:
    // The object cannot be copied.
    StManColumnBase (const StManColumnBase&);

    // The object cannot be assigned to.
    StManColumnBase& operator= (const StManColumnBase&);

    // Throw an "invalid operation" exception for the default
    // implementation of getArray.
    void throwGetArray() const;

    // Throw an "invalid operation" exception for the default
    // implementation of putArray.
    void throwPutArray() const;

private:
    // The data type of the column.
    DataType dtype_p;
    // The size of an element of this data type.
    Int      elemSize_p;
};




} //# NAMESPACE CASACORE - END

#endif
