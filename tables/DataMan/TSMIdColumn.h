//# TSMIdColumn.h: An id column in Tiled Storage Manager
//# Copyright (C) 1995,1996,1997,1999
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

#ifndef TABLES_TSMIDCOLUMN_H
#define TABLES_TSMIDCOLUMN_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/TSMColumn.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations


// <summary>
// An id column in Tiled Storage Manager.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=TSMColumn>TSMColumn</linkto>
//   <li> <linkto class=TSMCube>TSMCube</linkto>
//   <li> <linkto class=Record>Record</linkto>
// </prerequisite>

// <etymology>
// TSMIdColumn handles an id column for a Tiled
// Storage Manager.
// </etymology>

// <synopsis> 
// TSMIdColumn is used by
// <linkto class=TiledStMan>TiledStMan</linkto>
// to handle the access to
// a table column containing an id value of a tiled hypercube.
// Explicitly putting an id value is not possible. The only way to
// define the value is by specifying it when adding a hypercube
// in <linkto class=TiledDataStMan>TiledDataStMan</linkto>.
// <p>
// The id values are held in a TSMCube object. The row number
// determines which TSMCube object has to be accessed.
// <p>
// The creation of a TSMIdColumn object is done by a TSMColumn object.
// This process is described in more detail in the class
// <linkto class=TSMColumn>TSMColumn</linkto>.
// </synopsis> 

// <motivation>
// Handling coordinate columns in the Tiled Storage Manager is
// different from other columns.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class TSMIdColumn : public TSMColumn
{
public:

    // Create an id column from the given column.
    TSMIdColumn (const TSMColumn& column);

    // Frees up the storage.
    virtual ~TSMIdColumn();

    // Get a scalar value in the given row.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ScalarColumn get function).
    // <group>
    virtual void getBool     (rownr_t rownr, bool* dataPtr);
    virtual void getInt      (rownr_t rownr, int32_t* dataPtr);
    virtual void getuInt     (rownr_t rownr, uint32_t* dataPtr);
    virtual void getInt64    (rownr_t rownr, int64_t* dataPtr);
    virtual void getfloat    (rownr_t rownr, float* dataPtr);
    virtual void getdouble   (rownr_t rownr, double* dataPtr);
    virtual void getComplex  (rownr_t rownr, Complex* dataPtr);
    virtual void getDComplex (rownr_t rownr, DComplex* dataPtr);
    virtual void getString   (rownr_t rownr, String* dataPtr);
    // </group>

    // Put a scalar value in the given row.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ScalarColumn get function).
    // The value to be put must match the value which has already
    // been inserted by the TiledStMan::addHypercube function.
    // The put function is only there to be fully orthogonal.
    // <group>
    virtual void putBool     (rownr_t rownr, const bool* dataPtr);
    virtual void putInt      (rownr_t rownr, const int32_t* dataPtr);
    virtual void putuInt     (rownr_t rownr, const uint32_t* dataPtr);
    virtual void putInt64    (rownr_t rownr, const int64_t* dataPtr);
    virtual void putfloat    (rownr_t rownr, const float* dataPtr);
    virtual void putdouble   (rownr_t rownr, const double* dataPtr);
    virtual void putComplex  (rownr_t rownr, const Complex* dataPtr);
    virtual void putDComplex (rownr_t rownr, const DComplex* dataPtr);
    virtual void putString   (rownr_t rownr, const String* dataPtr);
    // </group>

private:
    // Forbid copy constructor.
    TSMIdColumn (const TSMIdColumn&);

    // Forbid assignment.
    TSMIdColumn& operator= (const TSMIdColumn&);
};




} //# NAMESPACE CASACORE - END

#endif
