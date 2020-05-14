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
    virtual ~ISMIndColumn();

    // Add (newNrrow-oldNrrow) rows to the column.
    virtual void addRow (rownr_t newNrrow, rownr_t oldNrrow);

    // Set the (fixed) shape of the arrays in the entire column.
    virtual void setShapeColumn (const IPosition& shape);

    // Get the dimensionality of the item in the given row.
    virtual uInt ndim (rownr_t rownr);

    // Set the shape of the array in the given row and allocate the array
    // in the file.
    virtual void setShape (rownr_t rownr, const IPosition& shape);

    // Is the shape defined (i.e. is there an array) in this row?
    virtual Bool isShapeDefined (rownr_t rownr);

    // Get the shape of the array in the given row.
    virtual IPosition shape (rownr_t rownr);

    // This storage manager can handle changing array shapes.
    virtual Bool canChangeShape() const;

    // Get an array value in the given row.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn get function).
    virtual void getArrayV (rownr_t rownr, ArrayBase&);

    // Put an array value into the given row.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn put function).
    virtual void putArrayV (rownr_t rownr, const ArrayBase&);

    // Get a section of the array in the given row.
    // The array has to have the correct length
    // (which is guaranteed by the ArrayColumn getSlice function).
    virtual void getSliceV (rownr_t rownr, const Slicer&, ArrayBase&);

    // Put into a section of the array in the given row.
    // The array has to have the correct length
    // (which is guaranteed by the ArrayColumn putSlice function).
    virtual void putSliceV (rownr_t rownr, const Slicer&, const ArrayBase&);

    // Let the column object create its array file.
    virtual void doCreate (ISMBucket* bucket);

    // Let the column object open an existing file.
    virtual void getFile (rownr_t nrrow);

    // Flush and optionally fsync the data.
    virtual Bool flush (rownr_t nrrow, Bool fsync);

    // Resync the storage manager with the new file contents.
    virtual void resync (rownr_t nrrow);

    // Let the column reopen its data files for read/write access.
    virtual void reopenRW();

    // Handle the duplication of a value; i.e. increment its reference count.
    virtual void handleCopy (rownr_t rownr, const char* value);

    // Handle the removal of a value; i.e. decrement its reference count.
    virtual void handleRemove (rownr_t rownr, const char* value);

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
    StIndArray* getShape (rownr_t rownr);

    // Put the shape for an array being put.
    // When there are multiple rows in the interval, it will
    // split the interval.
    StIndArray* putShape (rownr_t rownr, const IPosition& shape);

    // Put the shape for an array of which a slice is being put.
    // It gets the shape for the given row.
    // When there are multiple rows in the interval, it will
    // split the interval and copy the data.
    StIndArray* putShapeSliced (rownr_t rownr);

    // Return a pointer to the array in the given row (for a get).
    StIndArray* getArrayPtr (rownr_t rownr);

    // When needed, create an array in the given row with the given shape.
    // When the array is created, its data are copied when the flag is set.
    StIndArray* putArrayPtr (rownr_t rownr, const IPosition& shape,
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
