//# StIndArrAIO.h: AipsIO storage manager for indirect table arrays
//# Copyright (C) 1994,1995,1996,1997,1999
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#ifndef TABLES_STINDARRAIO_H
#define TABLES_STINDARRAIO_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/StManAipsIO.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/IO/ByteIO.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class AipsIO;
class StManArrayFile;
class StIndArray;


// <summary>
// AipsIO storage manager for indirect table arrays
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> StManAipsIO
//   <li> StManColumnAipsIO
//   <li> StIndArray
// </prerequisite>

// <etymology>
// StManColumnIndArrayAipsIO handles the access to an indirect array
// in a table column using the AipsIO storage manager.
// </etymology>

// <synopsis> 
// StManColumnArrayAipsIO handles indirect arrays in a table column.
// An StManArrayFile object is used to read and write the arrays
// from/into a file in a simple way. So this column has a file of its own
// to store the actual data in. It uses the (unique) column sequence
// number to make the file name unique.
//
// An array (or section of an array) is only read when needed.
// It, however, caches the array shape using the helper class
// StIndArray. Pointers to these objects
// are maintained using the standard StManColumnAipsIO facilities.
// When the column gets written, the offsets in the StManArrayFile file
// get written. Those will be read back when the column is read in.
//
// When a row gets deleted or when the array gets bigger, the file space
// is lost. This storage manager is a simple one and no attempts
// are done to make it smart.
// </synopsis> 

// <motivation>
// StManColumnIndArrayAipsIO handles the standard data types. The class
// is not templated, but a switch statement is used instead.
// Templates would cause too many instantiations.
// </motivation>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
// </todo>


class StManColumnIndArrayAipsIO : public StManColumnAipsIO
{
public:

    // Create a column of the given type.
    // The StManArrayFile object is not allocated here but by doCreate.
    StManColumnIndArrayAipsIO (StManAipsIO*, int dataType);

    // Frees up the storage and delete the StManArrayFile object.
    virtual ~StManColumnIndArrayAipsIO();
	
    // Forbid copy constructor.
    StManColumnIndArrayAipsIO (const StManColumnIndArrayAipsIO&) = delete;

    // Forbid assignment.
    StManColumnIndArrayAipsIO& operator= (const StManColumnIndArrayAipsIO&) = delete;

    // Set the (fixed) shape of the arrays in the entire column.
    virtual void setShapeColumn (const IPosition& shape);

    // Add (newNrrow-oldNrrow) rows to the column.
    // Allocate the data arrays in these rows if the shape is fixed.
    virtual void addRow (rownr_t newNrrow, rownr_t oldNrrow);

    // Set the shape of the array in the given row and allocate the array
    // in the file.
    virtual void setShape (rownr_t rownr, const IPosition& shape);

    // Is the shape defined (i.e. is there an array) in this row?
    virtual Bool isShapeDefined (rownr_t rownr);

    // Get the dimensionality of the item in the given row.
    // 0 is returned if there is no array.
    virtual uInt ndim (rownr_t rownr);

    // Get the shape of the array in the given row.
    // An zero-length IPosition is returned if there is no array.
    virtual IPosition shape (rownr_t rownr);

    // This storage manager can handle changing array shapes
    // for non-FixedShape columns.
    virtual Bool canChangeShape() const;

    // Get an array value in the given row.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn get function).
    virtual void getArrayV (rownr_t rownr, ArrayBase& dataPtr);

    // Put an array value into the given row.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn put function).
    virtual void putArrayV (rownr_t rownr, const ArrayBase& dataPtr);

    // Get a section of the array in the given row.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn getSlice function).
    virtual void getSliceV (rownr_t rownr, const Slicer&, ArrayBase& dataPtr);

    // Put into a section of the array in the given row.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn putSlice function).
    virtual void putSliceV (rownr_t rownr, const Slicer&, const ArrayBase& dataPtr);

    // Remove the value in the given row.
    // This will result in lost file space.
    virtual void remove (rownr_t rownr);

    // Let the column create its array file.
    virtual void doCreate (rownr_t nrrow);

    // Write the data into AipsIO.
    // This will call StManColumnAipsIO::putFile which will in its turn
    // call putData in this class for each of its chunks of data.
    virtual void putFile (rownr_t nrval, AipsIO&);

    // Read the data from AipsIO.
    // This will call StManColumnAipsIO::getFile which will in its turn
    // call getData in this class for each of its chunks of data.
    virtual void getFile (rownr_t nrval, AipsIO&);

    // Reopen the storage manager files for read/write.
    virtual void reopenRW();

    // Check if the class invariants still hold.
    Bool ok() const;

private:
    // The storage manager.
    StManAipsIO* staioPtr_p;
    // The (unique) sequence number of the column.
    uInt seqnr_p;
    // The shape of all arrays in case it is fixed.
    IPosition fixedShape_p;
    // Switch indicating if the shape is fixed.
    Bool shapeIsFixed_p;
    // The version of the object retrieved from a file.
    // Versions < 2 use a StManArrayFile of their own.
    // Newer versions share the one in StManAipsIO.
    uInt version_p;
    // The file containing the indirect arrays.
    StManArrayFile* iosfile_p;


    // Open the file with the given mode.
    void openFile (ByteIO::OpenOption opt);

    // Delete the array in the given row.
    void deleteArray (rownr_t rownr);

    // Read the shape at the given row.
    // This will cache the information in the StIndArray
    // object for that row.
    StIndArray* getShape (rownr_t rownr);

    // Put the data of a data block.
    // datap is an array of nrval pointers to StIndArray.
    // Only the file offsets get written.
    void putData (void* datap, uInt nrval, AipsIO&);

    // Get file offsets to the arrays into a data block at the given index.
    // datap is an array of pointers to StIndArray.
    // nrval blocks will be allocated and read starting at datap[index].
    // The actual shape and array data will be read when needed.
    void getData (void* datap, uInt index, uInt nrval, AipsIO&, uInt version);
};




} //# NAMESPACE CASACORE - END

#endif
