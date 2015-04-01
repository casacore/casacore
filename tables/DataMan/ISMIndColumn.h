//# ISMIndColumn.h: A column in Incremental storage manager for indirect arrays
//# Copyright (C) 1996,1997,1998,1999,2002
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

#ifndef TABLES_ISMINDCOLUMN_H
#define TABLES_ISMINDCOLUMN_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/ISMColumn.h>
#include <casacore/tables/DataMan/StIndArray.h>
#include <casacore/casa/Arrays/IPosition.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class StManArrayFile;
class AipsIO;


// <summary>
// A column of Incremental storage manager for indirect arrays.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=ISMColumn>ISMColumn</linkto>
//   <li> <linkto class=StIndArray>StIndArray</linkto>
// </prerequisite>

// <etymology>
// ISMIndColumn represents a Column in the Incremental Storage Manager
// containing INDirect arrays.
// </etymology>

// <synopsis> 
// ISMIndColumn is the implementation of an
// <linkto class=ISMColumn>ISMColumn</linkto> class
// to handle indirect arrays. The arrays (shape and data) are stored in
// a separate file using class <linkto class=StIndArray>StIndArray</linkto>.
// The file offset of the beginning of the array in stored in the
// ISM using the standard ISMColumn functions.
// <p>
// ISMIndColumn contains functions which are called when ISMColumn
// duplicates or removes a value. In that way the array can also be
// duplicated or removed in the StIndArray file by incrementing or
// decrementing the reference count manitained in the file.
// <p>
// Unlike ISMColumn it is not tested if a value put is equal to
// the value in the previous or next row, because it is too time-consuming
// to do so (although this behaviour could be changed in the future).
// Instead the user should not put equal values to prevent storing
// equal values.
// <p>
// Note that an indirect array can have a fixed shape. In that case
// adding a row results in reserving space for the array in the StIndArray
// file, so for each row an array is present.
// On the other hand adding a row does nothing for variable shaped arrays.
// So when no data is put or shape is set, a row may contain no array at all.
// In that case the function <src>isShapeDefined</src> returns False for
// that row.
// </synopsis> 

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//   <li> Maybe TpArrayInt, etc. should be used instead of TpInt.
// </todo>


class ISMIndColumn : public ISMColumn
{
public:

    // Create a column of the given data type.
    // It keeps the pointer to its parent (but does not own it).
    ISMIndColumn (ISMBase* parent, int dataType, uInt colnr);

    // Frees up the storage.
    ~ISMIndColumn();

    // It can handle access to a slice in a cell.
    virtual Bool canAccessSlice (Bool& reask) const;

    // Add (newNrrow-oldNrrow) rows to the column.
    virtual void addRow (uInt newNrrow, uInt oldNrrow);

    // Set the (fixed) shape of the arrays in the entire column.
    virtual void setShapeColumn (const IPosition& shape);

    // Get the dimensionality of the item in the given row.
    virtual uInt ndim (uInt rownr);

    // Set the shape of the array in the given row and allocate the array
    // in the file.
    void setShape (uInt rownr, const IPosition& shape);

    // Is the shape defined (i.e. is there an array) in this row?
    virtual Bool isShapeDefined (uInt rownr);

    // Get the shape of the array in the given row.
    virtual IPosition shape (uInt rownr);

    // This storage manager can handle changing array shapes.
    Bool canChangeShape() const;

    // Get an array value in the given row.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn get function).
    // <group>
    virtual void getArrayBoolV     (uInt rownr, Array<Bool>* dataPtr);
    virtual void getArrayuCharV    (uInt rownr, Array<uChar>* dataPtr);
    virtual void getArrayShortV    (uInt rownr, Array<Short>* dataPtr);
    virtual void getArrayuShortV   (uInt rownr, Array<uShort>* dataPtr);
    virtual void getArrayIntV      (uInt rownr, Array<Int>* dataPtr);
    virtual void getArrayuIntV     (uInt rownr, Array<uInt>* dataPtr);
    virtual void getArrayfloatV    (uInt rownr, Array<float>* dataPtr);
    virtual void getArraydoubleV   (uInt rownr, Array<double>* dataPtr);
    virtual void getArrayComplexV  (uInt rownr, Array<Complex>* dataPtr);
    virtual void getArrayDComplexV (uInt rownr, Array<DComplex>* dataPtr);
    virtual void getArrayStringV   (uInt rownr, Array<String>* dataPtr);
    // </group>

    // Put an array value into the given row.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn put function).
    // <group>
    virtual void putArrayBoolV     (uInt rownr, const Array<Bool>* dataPtr);
    virtual void putArrayuCharV    (uInt rownr, const Array<uChar>* dataPtr);
    virtual void putArrayShortV    (uInt rownr, const Array<Short>* dataPtr);
    virtual void putArrayuShortV   (uInt rownr, const Array<uShort>* dataPtr);
    virtual void putArrayIntV      (uInt rownr, const Array<Int>* dataPtr);
    virtual void putArrayuIntV     (uInt rownr, const Array<uInt>* dataPtr);
    virtual void putArrayfloatV    (uInt rownr, const Array<float>* dataPtr);
    virtual void putArraydoubleV   (uInt rownr, const Array<double>* dataPtr);
    virtual void putArrayComplexV  (uInt rownr, const Array<Complex>* dataPtr);
    virtual void putArrayDComplexV (uInt rownr, const Array<DComplex>* dataPtr);
    virtual void putArrayStringV   (uInt rownr, const Array<String>* dataPtr);
    // </group>

    // Get a section of the array in the given row.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn getSlice function).
    // <group>
    virtual void getSliceBoolV     (uInt rownr, const Slicer&,
				    Array<Bool>* dataPtr);
    virtual void getSliceuCharV    (uInt rownr, const Slicer&,
				    Array<uChar>* dataPtr);
    virtual void getSliceShortV    (uInt rownr, const Slicer&,
				    Array<Short>* dataPtr);
    virtual void getSliceuShortV   (uInt rownr, const Slicer&,
				    Array<uShort>* dataPtr);
    virtual void getSliceIntV      (uInt rownr, const Slicer&,
				    Array<Int>* dataPtr);
    virtual void getSliceuIntV     (uInt rownr, const Slicer&,
				    Array<uInt>* dataPtr);
    virtual void getSlicefloatV    (uInt rownr, const Slicer&,
				    Array<float>* dataPtr);
    virtual void getSlicedoubleV   (uInt rownr, const Slicer&,
				    Array<double>* dataPtr);
    virtual void getSliceComplexV  (uInt rownr, const Slicer&,
				    Array<Complex>* dataPtr);
    virtual void getSliceDComplexV (uInt rownr, const Slicer&,
				    Array<DComplex>* dataPtr);
    virtual void getSliceStringV   (uInt rownr, const Slicer&,
				    Array<String>* dataPtr);
    // </group>

    // Put into a section of the array in the given row.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn putSlice function).
    // <group>
    virtual void putSliceBoolV     (uInt rownr, const Slicer&,
				    const Array<Bool>* dataPtr);
    virtual void putSliceuCharV    (uInt rownr, const Slicer&,
				    const Array<uChar>* dataPtr);
    virtual void putSliceShortV    (uInt rownr, const Slicer&,
				    const Array<Short>* dataPtr);
    virtual void putSliceuShortV   (uInt rownr, const Slicer&,
				    const Array<uShort>* dataPtr);
    virtual void putSliceIntV      (uInt rownr, const Slicer&,
				    const Array<Int>* dataPtr);
    virtual void putSliceuIntV     (uInt rownr, const Slicer&,
				    const Array<uInt>* dataPtr);
    virtual void putSlicefloatV    (uInt rownr, const Slicer&,
				    const Array<float>* dataPtr);
    virtual void putSlicedoubleV   (uInt rownr, const Slicer&,
				    const Array<double>* dataPtr);
    virtual void putSliceComplexV  (uInt rownr, const Slicer&,
				    const Array<Complex>* dataPtr);
    virtual void putSliceDComplexV (uInt rownr, const Slicer&,
				    const Array<DComplex>* dataPtr);
    virtual void putSliceStringV   (uInt rownr, const Slicer&,
				    const Array<String>* dataPtr);
    // </group>

    // Let the column object create its array file.
    virtual void doCreate (ISMBucket* bucket);

    // Let the column object open an existing file.
    virtual void getFile (uInt nrrow);

    // Flush and optionally fsync the data.
    virtual Bool flush (uInt nrrow, Bool fsync);

    // Resync the storage manager with the new file contents.
    virtual void resync (uInt nrrow);

    // Let the column reopen its data files for read/write access.
    virtual void reopenRW();

    // Handle the duplication of a value; i.e. increment its reference count.
    virtual void handleCopy (uInt rownr, const char* value);

    // Handle the removal of a value; i.e. decrement its reference count.
    virtual void handleRemove (uInt rownr, const char* value);

private:
    // Forbid copy constructor.
    ISMIndColumn (const ISMIndColumn&);

    // Forbid assignment.
    ISMIndColumn& operator= (const ISMIndColumn&);

    // Initialize part of the object and open/create the file.
    // It is used by doCreate and getFile.
    void init (ByteIO::OpenOption fileOption);

    // Clear the object (used by destructor and init).
    void clear();

    // Compare the values to check if a value to be put matches the
    // value in the previous or next row.
    // It always return False, because comparing large arrays is
    // too expensive (it could be changed in the future). 
    virtual Bool compareValue (const void* val1, const void* val2) const;

    // Read the shape at the given row.
    // This will cache the information in the StIndArray
    // object for that row.
    StIndArray* getShape (uInt rownr);

    // Put the shape for an array being put.
    // When there are multiple rows in the interval, it will
    // split the interval.
    StIndArray* putShape (uInt rownr, const IPosition& shape);

    // Put the shape for an array of which a slice is being put.
    // It gets the shape for the given row.
    // When there are multiple rows in the interval, it will
    // split the interval and copy the data.
    StIndArray* putShapeSliced (uInt rownr);

    // Return a pointer to the array in the given row (for a get).
    StIndArray* getArrayPtr (uInt rownr);

    // When needed, create an array in the given row with the given shape.
    // When the array is created, its data are copied when the flag is set.
    StIndArray* putArrayPtr (uInt rownr, const IPosition& shape,
			     Bool copyData);


    // The (unique) sequence number of the column.
    uInt            seqnr_p;
    // The shape of all arrays in case it is fixed.
    IPosition       fixedShape_p;
    // Switch indicating if the shape is fixed.
    Bool            shapeIsFixed_p;
    // The file containing the arrays.
    StManArrayFile* iosfile_p;
    // The indirect array object.
    StIndArray      indArray_p;
    // The indirect array exists for the row interval last accessed.
    Bool            foundArray_p;
};




} //# NAMESPACE CASACORE - END

#endif
