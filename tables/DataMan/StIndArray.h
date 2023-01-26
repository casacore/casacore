//# StIndArray.h: Read/write indirect arrays
//# Copyright (C) 1994,1995,1996,1997,1999,2001
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

#ifndef TABLES_STINDARRAY_H
#define TABLES_STINDARRAY_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/StArrayFile.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/Arrays/ArrayFwd.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/BasicSL/Complex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class Slicer;
class ArrayBase;

// <summary>
// Read/write indirect arrays
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> StManArrayFile
// </prerequisite>

// <etymology>
// StIndArray stores indirect arrays on behalf of a storage manager.
// </etymology>

// <synopsis> 
// StIndArray is a helper class for accessing indirect table arrays.
// It is the interface between a storage manager like StManAipsIO
// (in particular its indirect array column class
// <linkto class="StManColumnIndArrayAipsIO:description">
// StManColumnIndArrayAipsIO</linkto>)
// and the data storage class
// <linkto class="StManArrayFile:description">StManArrayFile</linkto>
// which represents the file holding the shapes and data of the arrays.
// This file holds the data in canonical format.
//
// StIndArray holds information about an array in the file.
// <ol>
// <li> Offset of the array in the file. This points to the array shape.
//  This is stored by storage managers and serves as the mapping between
//  row number and array.
// <li> Array data offset, i.e. the length of the shape in the file.
//  Because the data is stored in canonical format, the length of the
//  shape in the file is not directly known but has to be supplied this way.
// <li> The actual shape of the array
// </ol>
// The storage manager creates an StIndArray object for each row.
// When an array is accessed for the first time,
// the array data offset and the shape will be filled in by StIndArray.
// In this way it serves as a cache for the array shape.
//
// StIndArray implements all necessary functions to get/put an array or
// an array slice from/into file supplied by the given StManArrayFile object.
// The StManArrayFile object itself has to be created by the storage manager
// and given to the StIndArray functions.
// </synopsis> 

// <motivation>
// This helper class makes it possible to share equal functionality
// between various storage managers handling indirect arrays.
// At the moment it is used by the StManAipsIO, IncrementalStMan, and
// StandardStMan storage managers, but it is not limited to them. It can
// equally well be used by any other storage manager storing (indirect) arrays
// via an StManArrayFile object.
// </motivation>

// <example>
// Note that the following example is not really useful.
// StIndArray is an internal class and should not be used by a casual user.
// The example may however give a bit of insight.
// <srcblock>
// Array<float> array(...);
// // Create an StManArrayFile object to hold the arrays.
// StManArrayFile stmanFile ("some.name", ByteIO::New);
// // Create a still empty StIndArray object for an array.
// StIndArray arrayRef(0);
// // Define the shape and allocate a float array.
// // Put the array data.
// arrayRef.setShape (stmanFile, TpFloat, array.shape());
// arrayRef.putArrayfloatV (stmanFile, &array);
// // Get the file offset of the array (for later use).
// int64_t offset = arrayRef.fileOffset();
// // Create an StIndArray object to read the array back.
// // Of course, the same object could have been used for that purpose,
// // but this shows how to create one for an existing file.
// StIndArray arrayRef2(offset);
// arrayRef2.getShape (stmanFile);             // read shape
// Array<float> array2(arrayRef2.shape());     // create with correct size
// arrayRef2.getArrayfloatV (stmanFile, &array2);
// </srcblock>
// </example>

// <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//   <li> Reuse file storage when an array gets reshaped.
//        This could be done if the array does not grow.
//        It also requires a change in StManArrayFile.
//   <li> Be smarter when accessing slices by not accessing a vector
//        at a time, but by determining and accessing the largest
//        possible consecutive area.
// </todo>


class StIndArray
{
public:
    // Construct the object with the given file offset.
    // A zero file offset means that no array has been defined yet.
    // That may be filled in later by setShape.
    StIndArray (int64_t fileOffset);

    // Copy constructor.
    StIndArray (const StIndArray&);

    // Assignment.
    StIndArray& operator= (const StIndArray&);

    ~StIndArray();

    // Get the shape.
    const IPosition& shape() const
	{return shape_p;}

    // Get the file offset.
    int64_t fileOffset() const
	{return fileOffset_p;}

    // Set the shape and allocate the array in the file.
    // This will define the array and fill in the file offset.
    // If the shape is already defined and does not change,
    // nothing is done and a false value is returned.
    // If the shape changes, the old file space is lost.
    bool setShape (StManArrayFile&, int dataType, const IPosition& shape);

    // Read the shape if not read yet.
    void getShape (StManArrayFile& ios);

    // Get the reference count.
    uint32_t refCount (StManArrayFile& ios);

    // Increment the reference count.
    void incrementRefCount (StManArrayFile& ios);

    // Decrement the reference count.
    void decrementRefCount (StManArrayFile& ios);

    // Copy the data from another array.
    // An exception if thrown if the shapes do not match.
    void copyData (StManArrayFile& ios, int dataType, const StIndArray& other);

    // Get an array value from the file at the offset held in this object.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn get function).
    void getArrayV (StManArrayFile& ios, ArrayBase& arr,
                    DataType dtype);

    // Put an array value into the file at the offset held in this object.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn put function).
    void putArrayV (StManArrayFile& ios, const ArrayBase& arr,
                    DataType dtype);

    // Get a section of the array from the file at the offset held in
    // this object.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn getSlice function).
    void getSliceV (StManArrayFile&, const Slicer&,
                    ArrayBase& dataPtr, DataType dtype);

    // Put a section of the array into the file at the offset held in
    // this object.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn putSlice function).
    void putSliceV (StManArrayFile&, const Slicer&,
                    const ArrayBase& dataPtr, DataType dtype);

private:
    int64_t     fileOffset_p;      //# offset of shape in StManArrayFile
    uint32_t      arrOffset_p;       //# extra offset to the array
    //#                              0 = arrOffset and shape not known yet
    IPosition shape_p;           //# shape of the array

    // Get sliced data, i.e. get a section of an array.
    // This function is used by getSliceXXXV to have common functionality
    // in one function. It calls the given getVec function for each
    // chunk of data. In this way the bulk of type-independent code
    // is concentrated in getSliceData resulting in small
    // type-dependent functions.
    void getSliceData (StManArrayFile&, const Slicer& ns, void* value,
		       const IPosition& userArrayShape,
		       void (*getVec) (StManArrayFile&,
				       int64_t, uint64_t, uint64_t, uint64_t, uint64_t,
				       void* dataPtr));

    // Get a (type-dependent) vector part of a slice.
    // This function is called for each chunk by putSliceData.
    // <group>
    static void getVecBoolV     (StManArrayFile&,
				 int64_t fileOffset, uint64_t arrayStart,
				 uint64_t length, uint64_t increment,
				 uint64_t valueIndex, void* value);
    static void getVecuCharV    (StManArrayFile&,
				 int64_t fileOffset, uint64_t arrayStart,
				 uint64_t length, uint64_t increment,
				 uint64_t valueIndex, void* value);
    static void getVecShortV    (StManArrayFile&,
				 int64_t fileOffset, uint64_t arrayStart,
				 uint64_t length, uint64_t increment,
				 uint64_t valueIndex, void* value);
    static void getVecuShortV   (StManArrayFile&,
				 int64_t fileOffset, uint64_t arrayStart,
				 uint64_t length, uint64_t increment,
				 uint64_t valueIndex, void* value);
    static void getVecIntV      (StManArrayFile&,
				 int64_t fileOffset, uint64_t arrayStart,
				 uint64_t length, uint64_t increment,
				 uint64_t valueIndex, void* value);
    static void getVecuIntV     (StManArrayFile&,
				 int64_t fileOffset, uint64_t arrayStart,
				 uint64_t length, uint64_t increment,
				 uint64_t valueIndex, void* value);
    static void getVecInt64V    (StManArrayFile&,
				 int64_t fileOffset, uint64_t arrayStart,
				 uint64_t length, uint64_t increment,
				 uint64_t valueIndex, void* value);
    static void getVecfloatV    (StManArrayFile&,
				 int64_t fileOffset, uint64_t arrayStart,
				 uint64_t length, uint64_t increment,
				 uint64_t valueIndex, void* value);
    static void getVecdoubleV   (StManArrayFile&,
				 int64_t fileOffset, uint64_t arrayStart,
				 uint64_t length, uint64_t increment,
				 uint64_t valueIndex, void* value);
    static void getVecComplexV  (StManArrayFile&,
				 int64_t fileOffset, uint64_t arrayStart,
				 uint64_t length, uint64_t increment,
				 uint64_t valueIndex, void* value);
    static void getVecDComplexV (StManArrayFile&,
				 int64_t fileOffset, uint64_t arrayStart,
				 uint64_t length, uint64_t increment,
				 uint64_t valueIndex, void* value);
    static void getVecStringV   (StManArrayFile&,
				 int64_t fileOffset, uint64_t arrayStart,
				 uint64_t length, uint64_t increment,
				 uint64_t valueIndex, void* value);
    // </group>

    // Put sliced data, i.e. put a section of an array.
    // This function is used by putSlice to have common functionality
    // in one function. It calls the given in putVec function for
    // chunk of data. In this way the bulk of type-independent code
    // is concentrated in putSliceData resulting in small
    // type-dependent functions.
    void putSliceData (StManArrayFile&, const Slicer& ns, const void* value,
		       const IPosition& userArrayShape,
		       void (*putVec) (StManArrayFile&,
				       int64_t, uint64_t, uint64_t, uint64_t, uint64_t,
				       const void* dataPtr));

    // Put a (type-dependent) vector part of a slice.
    // This function is called for each chunk by putSliceData.
    // <group>
    static void putVecBoolV     (StManArrayFile&,
				 int64_t fileOffset, uint64_t arrayStart,
				 uint64_t length, uint64_t increment,
				 uint64_t valueIndex, const void* value);
    static void putVecuCharV    (StManArrayFile&,
				 int64_t fileOffset, uint64_t arrayStart,
				 uint64_t length, uint64_t increment,
				 uint64_t valueIndex, const void* value);
    static void putVecShortV    (StManArrayFile&,
				 int64_t fileOffset, uint64_t arrayStart,
				 uint64_t length, uint64_t increment,
				 uint64_t valueIndex, const void* value);
    static void putVecuShortV   (StManArrayFile&,
				 int64_t fileOffset, uint64_t arrayStart,
				 uint64_t length, uint64_t increment,
				 uint64_t valueIndex, const void* value);
    static void putVecIntV      (StManArrayFile&,
				 int64_t fileOffset, uint64_t arrayStart,
				 uint64_t length, uint64_t increment,
				 uint64_t valueIndex, const void* value);
    static void putVecuIntV     (StManArrayFile&,
				 int64_t fileOffset, uint64_t arrayStart,
				 uint64_t length, uint64_t increment,
				 uint64_t valueIndex, const void* value);
    static void putVecInt64V    (StManArrayFile&,
				 int64_t fileOffset, uint64_t arrayStart,
				 uint64_t length, uint64_t increment,
				 uint64_t valueIndex, const void* value);
    static void putVecfloatV    (StManArrayFile&,
				 int64_t fileOffset, uint64_t arrayStart,
				 uint64_t length, uint64_t increment,
				 uint64_t valueIndex, const void* value);
    static void putVecdoubleV   (StManArrayFile&,
				 int64_t fileOffset, uint64_t arrayStart,
				 uint64_t length, uint64_t increment,
				 uint64_t valueIndex, const void* value);
    static void putVecComplexV  (StManArrayFile&,
				 int64_t fileOffset, uint64_t arrayStart,
				 uint64_t length, uint64_t increment,
				 uint64_t valueIndex, const void* value);
    static void putVecDComplexV (StManArrayFile&,
				 int64_t fileOffset, uint64_t arrayStart,
				 uint64_t length, uint64_t increment,
				 uint64_t valueIndex, const void* value);
    static void putVecStringV   (StManArrayFile&,
				 int64_t fileOffset, uint64_t arrayStart,
				 uint64_t length, uint64_t increment,
				 uint64_t valueIndex, const void* value);
    // </group>
	
    // Throw an exception if the shape of the given array and the table
    // array (slice) are not equal.
    void checkShape (const IPosition& userArrayShape,
		     const IPosition& tableArrayShape) const;
};




} //# NAMESPACE CASACORE - END

#endif
