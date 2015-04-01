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
//#
//# $Id$

#ifndef TABLES_STINDARRAY_H
#define TABLES_STINDARRAY_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/StArrayFile.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/BasicSL/Complex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class Slicer;
template<class T> class Array;


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
// At the moment it is used by StmanColumnIndArrayAipsIO and
// StManColumnIndArrayMirAIO (the AipsIO and Miriad-like storage
// manager, resp.), but it is not limited to them. It can equally
// well be used for any other storage manager storing (indirect) arrays
// via an StManArrayFile object.
// </motivation>

// <example>
// Note that the following example is not really useful.
// StIndArray is an internal class and should not be used by a casual user.
// The example may however give a bit of insight.
// <srcblock>
// Array<Float> array(...);
// // Create an StManArrayFile object to hold the arrays.
// StManArrayFile stmanFile ("some.name", ByteIO::New);
// // Create a still empty StIndArray object for an array.
// StIndArray arrayRef(0);
// // Define the shape and allocate a Float array.
// // Put the array data.
// arrayRef.setShape (stmanFile, TpFloat, array.shape());
// arrayRef.putArrayfloatV (stmanFile, &array);
// // Get the file offset of the array (for later use).
// Int64 offset = arrayRef.fileOffset();
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
//        This could be done when the array does not grow.
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
    StIndArray (Int64 fileOffset);

    // Copy constructor.
    StIndArray (const StIndArray&);

    // Assignment.
    StIndArray& operator= (const StIndArray&);

    ~StIndArray();

    // Get the shape.
    const IPosition& shape() const
	{return shape_p;}

    // Get the file offset.
    Int64 fileOffset() const
	{return fileOffset_p;}

    // Set the shape and allocate the array in the file.
    // This will define the array and fill in the file offset.
    // If the shape is already defined and does not change,
    // nothing is done and a False value is returned.
    // When the shape changes, the old file space is lost.
    Bool setShape (StManArrayFile&, int dataType, const IPosition& shape);

    // Read the shape if not read yet.
    void getShape (StManArrayFile& ios);

    // Get the reference count.
    uInt refCount (StManArrayFile& ios);

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
    // <group>
    void getArrayBoolV     (StManArrayFile&, Array<Bool>* dataPtr);
    void getArrayuCharV    (StManArrayFile&, Array<uChar>* dataPtr);
    void getArrayShortV    (StManArrayFile&, Array<Short>* dataPtr);
    void getArrayuShortV   (StManArrayFile&, Array<uShort>* dataPtr);
    void getArrayIntV      (StManArrayFile&, Array<Int>* dataPtr);
    void getArrayuIntV     (StManArrayFile&, Array<uInt>* dataPtr);
    void getArrayfloatV    (StManArrayFile&, Array<float>* dataPtr);
    void getArraydoubleV   (StManArrayFile&, Array<double>* dataPtr);
    void getArrayComplexV  (StManArrayFile&, Array<Complex>* dataPtr);
    void getArrayDComplexV (StManArrayFile&, Array<DComplex>* dataPtr);
    void getArrayStringV   (StManArrayFile&, Array<String>* dataPtr);
    // </group>

    // Put an array value into the file at the offset held in this object.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn put function).
    // <group>
    void putArrayBoolV     (StManArrayFile&, const Array<Bool>* dataPtr);
    void putArrayuCharV    (StManArrayFile&, const Array<uChar>* dataPtr);
    void putArrayShortV    (StManArrayFile&, const Array<Short>* dataPtr);
    void putArrayuShortV   (StManArrayFile&, const Array<uShort>* dataPtr);
    void putArrayIntV      (StManArrayFile&, const Array<Int>* dataPtr);
    void putArrayuIntV     (StManArrayFile&, const Array<uInt>* dataPtr);
    void putArrayfloatV    (StManArrayFile&, const Array<float>* dataPtr);
    void putArraydoubleV   (StManArrayFile&, const Array<double>* dataPtr);
    void putArrayComplexV  (StManArrayFile&, const Array<Complex>* dataPtr);
    void putArrayDComplexV (StManArrayFile&, const Array<DComplex>* dataPtr);
    void putArrayStringV   (StManArrayFile&, const Array<String>* dataPtr);
    // </group>

    // Get a section of the array from the file at the offset held in
    // this object.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn getSlice function).
    // <group>
    void getSliceBoolV     (StManArrayFile&, const Slicer&,
			    Array<Bool>* dataPtr);
    void getSliceuCharV    (StManArrayFile&, const Slicer&,
			    Array<uChar>* dataPtr);
    void getSliceShortV    (StManArrayFile&, const Slicer&,
			    Array<Short>* dataPtr);
    void getSliceuShortV   (StManArrayFile&, const Slicer&,
			    Array<uShort>* dataPtr);
    void getSliceIntV      (StManArrayFile&, const Slicer&,
			    Array<Int>* dataPtr);
    void getSliceuIntV     (StManArrayFile&, const Slicer&,
			    Array<uInt>* dataPtr);
    void getSlicefloatV    (StManArrayFile&, const Slicer&,
			    Array<float>* dataPtr);
    void getSlicedoubleV   (StManArrayFile&, const Slicer&,
			    Array<double>* dataPtr);
    void getSliceComplexV  (StManArrayFile&, const Slicer&,
			    Array<Complex>* dataPtr);
    void getSliceDComplexV (StManArrayFile&, const Slicer&,
			    Array<DComplex>* dataPtr);
    void getSliceStringV   (StManArrayFile&, const Slicer&,
			    Array<String>* dataPtr);
    // </group>

    // Put a section of the array into the file at the offset held in
    // this object.
    // The buffer pointed to by dataPtr has to have the correct length
    // (which is guaranteed by the ArrayColumn putSlice function).
    // <group>
    void putSliceBoolV     (StManArrayFile&, const Slicer&,
		            const Array<Bool>* dataPtr);
    void putSliceuCharV    (StManArrayFile&, const Slicer&,
                            const Array<uChar>* dataPtr);
    void putSliceShortV    (StManArrayFile&, const Slicer&,
                            const Array<Short>* dataPtr);
    void putSliceuShortV   (StManArrayFile&, const Slicer&,
                            const Array<uShort>* dataPtr);
    void putSliceIntV      (StManArrayFile&, const Slicer&,
                            const Array<Int>* dataPtr);
    void putSliceuIntV     (StManArrayFile&, const Slicer&,
                            const Array<uInt>* dataPtr);
    void putSlicefloatV    (StManArrayFile&, const Slicer&,
                            const Array<float>* dataPtr);
    void putSlicedoubleV   (StManArrayFile&, const Slicer&,
                            const Array<double>* dataPtr);
    void putSliceComplexV  (StManArrayFile&, const Slicer&,
                            const Array<Complex>* dataPtr);
    void putSliceDComplexV (StManArrayFile&, const Slicer&,
                            const Array<DComplex>* dataPtr);
    void putSliceStringV   (StManArrayFile&, const Slicer&,
                            const Array<String>* dataPtr);
    // </group>

private:
    Int64     fileOffset_p;      //# offset of shape in StManArrayFile
    uInt      arrOffset_p;       //# extra offset to the array
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
				       Int64, uInt, uInt, uInt, uInt,
				       void* dataPtr));

    // Get a (type-dependent) vector part of a slice.
    // This function is called for each chunk by putSliceData.
    // <group>
    static void getVecBoolV     (StManArrayFile&,
				 Int64 fileOffset, uInt arrayStart,
				 uInt length, uInt increment,
				 uInt valueIndex, void* value);
    static void getVecuCharV    (StManArrayFile&,
				 Int64 fileOffset, uInt arrayStart,
				 uInt length, uInt increment,
				 uInt valueIndex, void* value);
    static void getVecShortV    (StManArrayFile&,
				 Int64 fileOffset, uInt arrayStart,
				 uInt length, uInt increment,
				 uInt valueIndex, void* value);
    static void getVecuShortV   (StManArrayFile&,
				 Int64 fileOffset, uInt arrayStart,
				 uInt length, uInt increment,
				 uInt valueIndex, void* value);
    static void getVecIntV      (StManArrayFile&,
				 Int64 fileOffset, uInt arrayStart,
				 uInt length, uInt increment,
				 uInt valueIndex, void* value);
    static void getVecuIntV     (StManArrayFile&,
				 Int64 fileOffset, uInt arrayStart,
				 uInt length, uInt increment,
				 uInt valueIndex, void* value);
    static void getVecfloatV    (StManArrayFile&,
				 Int64 fileOffset, uInt arrayStart,
				 uInt length, uInt increment,
				 uInt valueIndex, void* value);
    static void getVecdoubleV   (StManArrayFile&,
				 Int64 fileOffset, uInt arrayStart,
				 uInt length, uInt increment,
				 uInt valueIndex, void* value);
    static void getVecComplexV  (StManArrayFile&,
				 Int64 fileOffset, uInt arrayStart,
				 uInt length, uInt increment,
				 uInt valueIndex, void* value);
    static void getVecDComplexV (StManArrayFile&,
				 Int64 fileOffset, uInt arrayStart,
				 uInt length, uInt increment,
				 uInt valueIndex, void* value);
    static void getVecStringV   (StManArrayFile&,
				 Int64 fileOffset, uInt arrayStart,
				 uInt length, uInt increment,
				 uInt valueIndex, void* value);
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
				       Int64, uInt, uInt, uInt, uInt,
				       const void* dataPtr));

    // Put a (type-dependent) vector part of a slice.
    // This function is called for each chunk by putSliceData.
    // <group>
    static void putVecBoolV     (StManArrayFile&,
				 Int64 fileOffset, uInt arrayStart,
				 uInt length, uInt increment,
				 uInt valueIndex, const void* value);
    static void putVecuCharV    (StManArrayFile&,
				 Int64 fileOffset, uInt arrayStart,
				 uInt length, uInt increment,
				 uInt valueIndex, const void* value);
    static void putVecShortV    (StManArrayFile&,
				 Int64 fileOffset, uInt arrayStart,
				 uInt length, uInt increment,
				 uInt valueIndex, const void* value);
    static void putVecuShortV   (StManArrayFile&,
				 Int64 fileOffset, uInt arrayStart,
				 uInt length, uInt increment,
				 uInt valueIndex, const void* value);
    static void putVecIntV      (StManArrayFile&,
				 Int64 fileOffset, uInt arrayStart,
				 uInt length, uInt increment,
				 uInt valueIndex, const void* value);
    static void putVecuIntV     (StManArrayFile&,
				 Int64 fileOffset, uInt arrayStart,
				 uInt length, uInt increment,
				 uInt valueIndex, const void* value);
    static void putVecfloatV    (StManArrayFile&,
				 Int64 fileOffset, uInt arrayStart,
				 uInt length, uInt increment,
				 uInt valueIndex, const void* value);
    static void putVecdoubleV   (StManArrayFile&,
				 Int64 fileOffset, uInt arrayStart,
				 uInt length, uInt increment,
				 uInt valueIndex, const void* value);
    static void putVecComplexV  (StManArrayFile&,
				 Int64 fileOffset, uInt arrayStart,
				 uInt length, uInt increment,
				 uInt valueIndex, const void* value);
    static void putVecDComplexV (StManArrayFile&,
				 Int64 fileOffset, uInt arrayStart,
				 uInt length, uInt increment,
				 uInt valueIndex, const void* value);
    static void putVecStringV   (StManArrayFile&,
				 Int64 fileOffset, uInt arrayStart,
				 uInt length, uInt increment,
				 uInt valueIndex, const void* value);
    // </group>
	
    // Throw an exception if the shape of the given array and the table
    // array (slice) are not equal.
    void checkShape (const IPosition& userArrayShape,
		     const IPosition& tableArrayShape) const;
};




} //# NAMESPACE CASACORE - END

#endif
