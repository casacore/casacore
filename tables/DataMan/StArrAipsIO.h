//# StArrAipsIO.h: AipsIO storage manager for direct table arrays
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

#ifndef TABLES_STARRAIPSIO_H
#define TABLES_STARRAIPSIO_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/StManAipsIO.h>
#include <casacore/casa/Arrays/IPosition.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class AipsIO;


// <summary>
// AipsIO storage manager for direct table arrays
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> StManAipsIO
//   <li> StManColumnAipsIO
// </prerequisite>

// <etymology>
// StManColumnArrayAipsIO handles the access to a direct array in a table
// column using the AipsIO storage manager.
// </etymology>

// <synopsis> 
// StManColumnArrayAipsIO holds the direct arrays in memory and writes
// them into the AipsIO file when the table gets closed.
// It fully supports addition and removal of rows.
// When a row is added to the table, the direct array gets allocated.
// This is possible, because the shape of direct arrays is known.
//
// The class is derived from StManColumnAipsIO which is used to hold
// a pointer to the array.
// </synopsis> 

// <motivation>
// StManColumnArrayAipsIO handles the standard data types. The class
// is not templated, but a switch statement is used instead.
// Templates would cause too many instantiations.
// </motivation>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//   <li> Maybe TpArrayInt, etc. should be used instead of TpInt.
//   <li> get/putSlice use too many array operations.
//          To solve this requires an array constructor taking a
//          pointer to the data (which is foreseen in the new Array classes).
// </todo>


class StManColumnArrayAipsIO : public StManColumnAipsIO
{
public:

    // Create a column of the given data type.
    StManColumnArrayAipsIO (StManAipsIO*, int dataType);

    // Frees up the storage.
    ~StManColumnArrayAipsIO();

    // It can handle access to a slice in a cell.
    Bool canAccessSlice (Bool& reask) const;

    // It can handle access to an entire column.
    Bool canAccessArrayColumn (Bool& reask) const;

    // Set the shape of the arrays in the entire column.
    void setShapeColumn (const IPosition& shape);

    // Add (newNrrow-oldNrrow) rows to the column.
    // Allocate the data arrays in these rows.
    void addRow (uInt newNrrow, uInt oldNrrow);

    // Get the dimensionality of the item in the given row.
    // This is the same for all rows.
    uInt ndim (uInt rownr);

    // Get the shape of the array in the given row.
    // This is the same for all rows.
    IPosition shape (uInt rownr);

    // Get an array value in the given row.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn get function).
    // <group>
    void getArrayBoolV     (uInt rownr, Array<Bool>* dataPtr);
    void getArrayuCharV    (uInt rownr, Array<uChar>* dataPtr);
    void getArrayShortV    (uInt rownr, Array<Short>* dataPtr);
    void getArrayuShortV   (uInt rownr, Array<uShort>* dataPtr);
    void getArrayIntV      (uInt rownr, Array<Int>* dataPtr);
    void getArrayuIntV     (uInt rownr, Array<uInt>* dataPtr);
    void getArrayfloatV    (uInt rownr, Array<float>* dataPtr);
    void getArraydoubleV   (uInt rownr, Array<double>* dataPtr);
    void getArrayComplexV  (uInt rownr, Array<Complex>* dataPtr);
    void getArrayDComplexV (uInt rownr, Array<DComplex>* dataPtr);
    void getArrayStringV   (uInt rownr, Array<String>* dataPtr);
    // </group>

    // Put an array value into the given row.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn put function).
    // <group>
    void putArrayBoolV     (uInt rownr, const Array<Bool>* dataPtr);
    void putArrayuCharV    (uInt rownr, const Array<uChar>* dataPtr);
    void putArrayShortV    (uInt rownr, const Array<Short>* dataPtr);
    void putArrayuShortV   (uInt rownr, const Array<uShort>* dataPtr);
    void putArrayIntV      (uInt rownr, const Array<Int>* dataPtr);
    void putArrayuIntV     (uInt rownr, const Array<uInt>* dataPtr);
    void putArrayfloatV    (uInt rownr, const Array<float>* dataPtr);
    void putArraydoubleV   (uInt rownr, const Array<double>* dataPtr);
    void putArrayComplexV  (uInt rownr, const Array<Complex>* dataPtr);
    void putArrayDComplexV (uInt rownr, const Array<DComplex>* dataPtr);
    void putArrayStringV   (uInt rownr, const Array<String>* dataPtr);
    // </group>

    // Get a section of the array in the given row.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn getSlice function).
    // <group>
    void getSliceBoolV     (uInt rownr, const Slicer&, Array<Bool>* dataPtr);
    void getSliceuCharV    (uInt rownr, const Slicer&, Array<uChar>* dataPtr);
    void getSliceShortV    (uInt rownr, const Slicer&, Array<Short>* dataPtr);
    void getSliceuShortV   (uInt rownr, const Slicer&, Array<uShort>* dataPtr);
    void getSliceIntV      (uInt rownr, const Slicer&, Array<Int>* dataPtr);
    void getSliceuIntV     (uInt rownr, const Slicer&, Array<uInt>* dataPtr);
    void getSlicefloatV    (uInt rownr, const Slicer&, Array<float>* dataPtr);
    void getSlicedoubleV   (uInt rownr, const Slicer&, Array<double>* dataPtr);
    void getSliceComplexV  (uInt rownr, const Slicer&, Array<Complex>* dataPtr);
    void getSliceDComplexV (uInt rownr, const Slicer&, Array<DComplex>* dataPtr);
    void getSliceStringV   (uInt rownr, const Slicer&, Array<String>* dataPtr);
    // </group>

    // Put into a section of the array in the given row.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn putSlice function).
    // <group>
    void putSliceBoolV     (uInt rownr, const Slicer&,
		            const Array<Bool>* dataPtr);
    void putSliceuCharV    (uInt rownr, const Slicer&,
                            const Array<uChar>* dataPtr);
    void putSliceShortV    (uInt rownr, const Slicer&,
                            const Array<Short>* dataPtr);
    void putSliceuShortV   (uInt rownr, const Slicer&,
                            const Array<uShort>* dataPtr);
    void putSliceIntV      (uInt rownr, const Slicer&,
                            const Array<Int>* dataPtr);
    void putSliceuIntV     (uInt rownr, const Slicer&,
                            const Array<uInt>* dataPtr);
    void putSlicefloatV    (uInt rownr, const Slicer&,
                            const Array<float>* dataPtr);
    void putSlicedoubleV   (uInt rownr, const Slicer&,
                            const Array<double>* dataPtr);
    void putSliceComplexV  (uInt rownr, const Slicer&,
                            const Array<Complex>* dataPtr);
    void putSliceDComplexV (uInt rownr, const Slicer&,
                            const Array<DComplex>* dataPtr);
    void putSliceStringV   (uInt rownr, const Slicer&,
                            const Array<String>* dataPtr);
    // </group>

    // Get all array values in the column.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn getColumn function).
    // <group>
    void getArrayColumnBoolV     (Array<Bool>* dataPtr);
    void getArrayColumnuCharV    (Array<uChar>* dataPtr);
    void getArrayColumnShortV    (Array<Short>* dataPtr);
    void getArrayColumnuShortV   (Array<uShort>* dataPtr);
    void getArrayColumnIntV      (Array<Int>* dataPtr);
    void getArrayColumnuIntV     (Array<uInt>* dataPtr);
    void getArrayColumnfloatV    (Array<float>* dataPtr);
    void getArrayColumndoubleV   (Array<double>* dataPtr);
    void getArrayColumnComplexV  (Array<Complex>* dataPtr);
    void getArrayColumnDComplexV (Array<DComplex>* dataPtr);
    void getArrayColumnStringV   (Array<String>* dataPtr);
    // </group>

    // Put all arrays in the column.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn putColumn function).
    // <group>
    void putArrayColumnBoolV     (const Array<Bool>* dataPtr);
    void putArrayColumnuCharV    (const Array<uChar>* dataPtr);
    void putArrayColumnShortV    (const Array<Short>* dataPtr);
    void putArrayColumnuShortV   (const Array<uShort>* dataPtr);
    void putArrayColumnIntV      (const Array<Int>* dataPtr);
    void putArrayColumnuIntV     (const Array<uInt>* dataPtr);
    void putArrayColumnfloatV    (const Array<float>* dataPtr);
    void putArrayColumndoubleV   (const Array<double>* dataPtr);
    void putArrayColumnComplexV  (const Array<Complex>* dataPtr);
    void putArrayColumnDComplexV (const Array<DComplex>* dataPtr);
    void putArrayColumnStringV   (const Array<String>* dataPtr);
    // </group>

    // Remove the value at the given index.
    void remove (uInt index);

    // Write the data into AipsIO.
    // This will call StManColumnAipsIO::putFile which will in its turn
    // call putData in this class for each of its chunks of data.
    void putFile (uInt nrval, AipsIO&);

    // Read the data from AipsIO.
    // This will call StManColumnAipsIO::getFile which will in its turn
    // call getData in this class for each of its chunks of data.
    void getFile (uInt nrval, AipsIO&);

    // Check if the class invariants still hold.
    Bool ok() const;

private:
    // The data type of the array (as defined in DataType.h).
    int   dtypeArr_p;
    // The shape of the array.
    IPosition shape_p;
    // The nr of elements in the array.
    uInt  nrelem_p;

    // Delete the array at the given index.
    void deleteArray (uInt index);

    // Put the data of a data block.
    // datap is an array of nrval pointers to arrays.
    void putData (void* datap, uInt nrval, AipsIO&);

    // Get data arrays into a data block at the given index.
    // datap is an array of pointers to arrays. nrval arrays will
    // be allocated and read starting at datap[index].
    void getData (void* datap, uInt index, uInt nrval, AipsIO&, uInt version);

    // Forbid copy constructor.
    StManColumnArrayAipsIO (const StManColumnArrayAipsIO&);

    // Forbid assignment.
    StManColumnArrayAipsIO& operator= (const StManColumnArrayAipsIO&);
};




} //# NAMESPACE CASACORE - END

#endif
