//# TSMCoordColumn.h: A coordinate column in Tiled Storage Manager
//# Copyright (C) 1995,1996,1999
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

#ifndef TABLES_TSMCOORDCOLUMN_H
#define TABLES_TSMCOORDCOLUMN_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/TSMColumn.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations


// <summary>
// A coordinate column in Tiled Storage Manager
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
// TSMCoordColumn handles a coordinate column for a Tiled
// Storage Manager.
// </etymology>

// <synopsis> 
// TSMCoordColumn is used by 
// <linkto class=TiledStMan>TiledStMan</linkto>
// to handle the access to
// a table column containing coordinates of a tiled hypercube axis.
// There are 2 types of coordinates (as described at
// <linkto class=TableDesc:defineHypercolumn>
// TableDesc::defineHypercolumn</linkto>):
// <ol>
//  <li> As a vector. These are the coordinates of the arrays held
//       in the data cells. They are accessed via the get/putArray
//       functions. Their shapes are dependent on the hypercube shape,
//       so it is checked if they match.
//  <li> As a scalar. These are the coordinates of the extra axes
//       defined in the hypercube. They are accessed via the get/put
//       functions.
// </ol>
// The coordinates are held in a TSMCube object. The row number
// determines which TSMCube object has to be accessed.
// <p>
// The creation of a TSMCoordColumn object is done by a TSMColumn object.
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


class TSMCoordColumn : public TSMColumn
{
public:

    // Create a coordinate column from the given column.
    TSMCoordColumn (const TSMColumn& column, uInt axisNr);

    // Frees up the storage.
    virtual ~TSMCoordColumn();

    // Set the shape of the coordinate vector in the given row.
    void setShape (uInt rownr, const IPosition& shape);

    // Is the value shape defined in the given row?
    Bool isShapeDefined (uInt rownr);

    // Get the shape of the item in the given row.
    IPosition shape (uInt rownr);

    // Get a scalar value in the given row.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the Scalar/ArrayColumn get function).
    // <group>
    void getIntV      (uInt rownr, Int* dataPtr);
    void getuIntV     (uInt rownr, uInt* dataPtr);
    void getfloatV    (uInt rownr, float* dataPtr);
    void getdoubleV   (uInt rownr, double* dataPtr);
    void getComplexV  (uInt rownr, Complex* dataPtr);
    void getDComplexV (uInt rownr, DComplex* dataPtr);
    // </group>

    // Put a scalar value into the given row.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the Scalar/ArrayColumn put function).
    // <group>
    void putIntV      (uInt rownr, const Int* dataPtr);
    void putuIntV     (uInt rownr, const uInt* dataPtr);
    void putfloatV    (uInt rownr, const float* dataPtr);
    void putdoubleV   (uInt rownr, const double* dataPtr);
    void putComplexV  (uInt rownr, const Complex* dataPtr);
    void putDComplexV (uInt rownr, const DComplex* dataPtr);
    // </group>

    // Get the array value in the given row.
    // The array pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn get function).
    // The default implementation thrown an "invalid operation exception".
    // <group>
    void getArrayIntV      (uInt rownr, Array<Int>* dataPtr);
    void getArrayuIntV     (uInt rownr, Array<uInt>* dataPtr);
    void getArrayfloatV    (uInt rownr, Array<float>* dataPtr);
    void getArraydoubleV   (uInt rownr, Array<double>* dataPtr);
    void getArrayComplexV  (uInt rownr, Array<Complex>* dataPtr);
    void getArrayDComplexV (uInt rownr, Array<DComplex>* dataPtr);
    // </group>

    // Put the array value into the given row.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn put function).
    // The default implementation thrown an "invalid operation exception".
    // <group>
    void putArrayIntV      (uInt rownr, const Array<Int>* dataPtr);
    void putArrayuIntV     (uInt rownr, const Array<uInt>* dataPtr);
    void putArrayfloatV    (uInt rownr, const Array<float>* dataPtr);
    void putArraydoubleV   (uInt rownr, const Array<double>* dataPtr);
    void putArrayComplexV  (uInt rownr, const Array<Complex>* dataPtr);
    void putArrayDComplexV (uInt rownr, const Array<DComplex>* dataPtr);
    // </group>

private:
    // The axis number of the coordinate.
    uInt axisNr_p;

    // Forbid copy constructor.
    TSMCoordColumn (const TSMCoordColumn&);

    // Forbid assignment.
    TSMCoordColumn& operator= (const TSMCoordColumn&);
};




} //# NAMESPACE CASACORE - END

#endif
