//# StArrayFile.h: Read/write array in external format for a storage manager
//# Copyright (C) 1994,1995,1996,1997,1999,2001,2002
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

#ifndef TABLES_STARRAYFILE_H
#define TABLES_STARRAYFILE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/IO/RegularFileIO.h>
#include <casacore/casa/IO/TypeIO.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/BasicSL/Complex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class MultiFileBase;
class IPosition;


// <summary>
// Read/write array in external format for a storage manager
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> ToLocal
//   <li> FromLocal
// </prerequisite>

// <etymology>
// StManArrayFile is a class used by table storage managers
// to store indirect arrays in a file.
// </etymology>

// <synopsis> 
// StManArrayFile is for use by the table storage manager, in particular
// to read/write indirectly stored arrays.
// Instead of holding the data in memory, they are written directly
// into a file. It also allows to access a part of an array, which
// is needed for the table system to access an array section.
// It does not use a cache of its own, but it is relying on the
// underlying system routines to cache and buffer adequately.
//
// This class could in principle also be used for other array purposes,
// for example, to implement a paged array class for really huge arrays.
//
// An StManArrayFile object is connected to one file. It is possible
// to hold multiple arrays in the file, each with its own shape.
// An array is stored as its shape followed by the actual data
// (all in canonical format). An array of strings is written as
// an array of offsets pointing to the actual strings.
// When a string gets a new value, the new value is written at the
// end of the file and the file space with the old value is lost.
//
// Currently only the basic types are supported, but arbitrary types
// could also be supported by writing/reading an element in the normal
// way into the AipsIO buffer. It would only require that AipsIO
// would contain a function to get its buffers and to restart them.
// </synopsis> 

// <example>
// <srcblock>
// void writeArray (const Array<Bool>& array)
// {
//     // Construct object and update file StArray.dat.
//     StManArrayFile arrayFile("StArray.dat, ByteIO::New);
//     // Reserve space for an array with the given shape and data type.
//     // This writes the shape at the end of the file and reserves
//     // space the hold the entire Bool array.
//     // It fills in the file offset where the shape is stored
//     // and returns the length of the shape in the file.
//     Int64 offset;
//     uInt shapeLength = arrayFile.putShape (array.shape(), offset, static_cast<Bool*>(0));
//     // Now put the actual array.
//     // This has to be put at the returned file offset plus the length
//     // of the shape in the file.
//     Bool deleteIt;
//     const Bool* dataPtr = array.getStorage (deleteIt);
//     arrayFile.put (offset+shapeLength, 0, array.nelements(), dataPtr);
//     array.freeStorage (dataPtr, deleteIt);
// }
// </srcblock>
// </example>

// <motivation>
// The AipsIO class was not suitable for indirect table arrays,
// because it uses memory to hold the data. Furthermore it is
// not possible to access part of the data in AipsIO.
// </motivation>

// <todo asof="$DATE:$">
//   <li> implement long double
//   <li> support arbitrary types
//   <li> when rewriting a string value, use the current file
//          space if it fits
// </todo>


class StManArrayFile
{
public:

    // Construct the object and attach it to the give file.
    // The OpenOption determines how the file is opened
    // (e.g. ByteIO::New for a new file).
    // The buffersize is used to allocate a buffer of a proper size
    // for the underlying filebuf object (see iostream package).
    // A bufferSize 0 means using the default size (currently 65536).
    StManArrayFile (const String& name, ByteIO::OpenOption,
		    uInt version=0, Bool bigEndian=True,
		    uInt bufferSize=0,
                    MultiFileBase* mfile=0);

    // Close the possibly opened file.
    ~StManArrayFile();

    // Flush and optionally fsync the data.
    // It returns True when any data was written since the last flush.
    Bool flush (Bool fsync);

    // Reopen the file for read/write access.
    void reopenRW();

    // Resync the file (i.e. clear possible cache information).
    void resync();

    // Return the current file length (merely a debug tool).
    Int64 length()
	{ return leng_p; }

    // Put the array shape and store its file offset into the offset argument.
    // Reserve file space for the associated array.
    // The length of the shape part in the file is returned.
    // The file offset plus the shape length is the starting offset of the
    // actual array data (which can be used by get and put).
    // Space is reserved to store the reference count.
    // <group>
    uInt putShape (const IPosition& shape, Int64& fileOffset,
		   const Bool* dummy);
    uInt putShape (const IPosition& shape, Int64& fileOffset,
		   const Char* dummy);
    uInt putShape (const IPosition& shape, Int64& fileOffset,
		   const uChar* dummy);
    uInt putShape (const IPosition& shape, Int64& fileOffset,
		   const Short* dummy);
    uInt putShape (const IPosition& shape, Int64& fileOffset,
		   const uShort* dummy);
    uInt putShape (const IPosition& shape, Int64& fileOffset,
		   const Int* dummy);
    uInt putShape (const IPosition& shape, Int64& fileOffset,
		   const uInt* dummy);
    uInt putShape (const IPosition& shape, Int64& fileOffset,
		   const Int64* dummy);
    uInt putShape (const IPosition& shape, Int64& fileOffset,
		   const uInt64* dummy);
    uInt putShape (const IPosition& shape, Int64& fileOffset,
		   const Float* dummy);
    uInt putShape (const IPosition& shape, Int64& fileOffset,
		   const Double* dummy);
//#//    uInt putShape (const IPosition& shape, Int64& fileOffset,
//#//                   const long double* dummy);
    uInt putShape (const IPosition& shape, Int64& fileOffset,
		   const Complex* dummy);
    uInt putShape (const IPosition& shape, Int64& fileOffset,
		   const DComplex* dummy);
    uInt putShape (const IPosition& shape, Int64& fileOffset,
		   const String* dummy);
    // </group>

    // Get the reference count.
    uInt getRefCount (Int64 offset);

    // Put the reference count.
    // An exception is thrown if a value other than 1 is put for version 0.
    void putRefCount (uInt refCount, Int64 offset);

    // Put nr elements at the given file offset and array offset.
    // The file offset of the first array element is the file offset
    // of the shape plus the length of the shape in the file.
    // The array offset is counted in number of elements. It can be
    // used to put only a (contiguous) section of the array.
    // <group>
    void put (Int64 fileOffset, uInt arrayOffset, uInt nr, const Bool*);
    void put (Int64 fileOffset, uInt arrayOffset, uInt nr, const Char*);
    void put (Int64 fileOffset, uInt arrayOffset, uInt nr, const uChar*);
    void put (Int64 fileOffset, uInt arrayOffset, uInt nr, const Short*);
    void put (Int64 fileOffset, uInt arrayOffset, uInt nr, const uShort*);
    void put (Int64 fileOffset, uInt arrayOffset, uInt nr, const Int*);
    void put (Int64 fileOffset, uInt arrayOffset, uInt nr, const uInt*);
    void put (Int64 fileOffset, uInt arrayOffset, uInt nr, const Int64*);
    void put (Int64 fileOffset, uInt arrayOffset, uInt nr, const uInt64*);
    void put (Int64 fileOffset, uInt arrayOffset, uInt nr, const Float*);
    void put (Int64 fileOffset, uInt arrayOffset, uInt nr, const Double*);
//#//    void put (Int64 fileOffset, uInt arrayOffset, uInt nr, const long double*);
    void put (Int64 fileOffset, uInt arrayOffset, uInt nr, const Complex*);
    void put (Int64 fileOffset, uInt arrayOffset, uInt nr, const DComplex*);
    void put (Int64 fileOffset, uInt arrayOffset, uInt nr, const String*);
    // </group>

    // Get the shape at the given file offset.
    // It will reshape the IPosition vector when needed.
    // It returns the length of the shape in the file.
    uInt getShape (Int64 fileOffset, IPosition& shape);

    // Get nr elements at the given file offset and array offset.
    // The file offset of the first array element is the file offset
    // of the shape plus the length of the shape in the file.
    // The array offset is counted in number of elements. It can be
    // used to get only a (contiguous) section of the array.
    // <group>
    void get (Int64 fileOffset, uInt arrayOffset, uInt nr, Bool*);
    void get (Int64 fileOffset, uInt arrayOffset, uInt nr, Char*);
    void get (Int64 fileOffset, uInt arrayOffset, uInt nr, uChar*);
    void get (Int64 fileOffset, uInt arrayOffset, uInt nr, Short*);
    void get (Int64 fileOffset, uInt arrayOffset, uInt nr, uShort*);
    void get (Int64 fileOffset, uInt arrayOffset, uInt nr, Int*);
    void get (Int64 fileOffset, uInt arrayOffset, uInt nr, uInt*);
    void get (Int64 fileOffset, uInt arrayOffset, uInt nr, Int64*);
    void get (Int64 fileOffset, uInt arrayOffset, uInt nr, uInt64*);
    void get (Int64 fileOffset, uInt arrayOffset, uInt nr, Float*);
    void get (Int64 fileOffset, uInt arrayOffset, uInt nr, Double*);
//#//    void get (Int64 fileOffset, uInt arrayOffset, uInt nr, long double*);
    void get (Int64 fileOffset, uInt arrayOffset, uInt nr, Complex*);
    void get (Int64 fileOffset, uInt arrayOffset, uInt nr, DComplex*);
    void get (Int64 fileOffset, uInt arrayOffset, uInt nr, String*);
    // </group>

    // Copy the array with <src>nr</src> elements from one file offset
    // to another.
    // <group>
    void copyArrayBool     (Int64 to, Int64 from, uInt nr);
    void copyArrayChar     (Int64 to, Int64 from, uInt nr);
    void copyArrayuChar    (Int64 to, Int64 from, uInt nr);
    void copyArrayShort    (Int64 to, Int64 from, uInt nr);
    void copyArrayuShort   (Int64 to, Int64 from, uInt nr);
    void copyArrayInt      (Int64 to, Int64 from, uInt nr);
    void copyArrayuInt     (Int64 to, Int64 from, uInt nr);
    void copyArrayInt64    (Int64 to, Int64 from, uInt nr);
    void copyArrayuInt64   (Int64 to, Int64 from, uInt nr);
    void copyArrayFloat    (Int64 to, Int64 from, uInt nr);
    void copyArrayDouble   (Int64 to, Int64 from, uInt nr);
//#//    void copyArrayLDouble  (Int64 to, Int64 from, uInt nr);
    void copyArrayComplex  (Int64 to, Int64 from, uInt nr);
    void copyArrayDComplex (Int64 to, Int64 from, uInt nr);
    void copyArrayString   (Int64 to, Int64 from, uInt nr);
    // </group>

private:
    ByteIO* file_p;                //# File object
    TypeIO* iofil_p;               //# IO object
    Int64   leng_p;                //# File length
    uInt    version_p;             //# Version of StArrayFile file
    Bool    swput_p;               //# True = put is possible
    Bool    hasPut_p;              //# True = put since last flush
    uInt    sizeChar_p;
    uInt    sizeuChar_p;
    uInt    sizeShort_p;
    uInt    sizeuShort_p;
    uInt    sizeInt_p;
    uInt    sizeuInt_p;
    uInt    sizeInt64_p;
    uInt    sizeuInt64_p;
    uInt    sizeFloat_p;
    uInt    sizeDouble_p;

    // Put a single value at the current file offset.
    // It returns the length of the value in the file.
    // <group>
    uInt put (const Int&);
    uInt put (const uInt&);
    // </group>

    // Put the array shape at the end of the file and reserve
    // space for nr elements (each lenElem bytes long).
    // It fills the file offset of the shape.
    // It returns the length of the shape in the file.
    uInt putRes (const IPosition& shape, Int64& fileOffset, float lenElem);

    // Get a single value at the current file offset.
    // It returns the length of the value in the file.
    // <group>
    uInt get (Int&);
    uInt get (uInt&);
    // </group>

    // Copy data with the given length from one file offset to another.
    void copyData (Int64 to, Int64 from, uInt length);

    // Position the file on the given offset.
    void setpos (Int64 offset);
};
    

inline void StManArrayFile::reopenRW()
{
    file_p->reopenRW();
}
inline uInt StManArrayFile::put (const Int& value)
{
    hasPut_p = True;
    return iofil_p->write (1, &value);
}
inline uInt StManArrayFile::put (const uInt& value)
{
    hasPut_p = True;
    return iofil_p->write (1, &value);
}
inline uInt StManArrayFile::get (Int& value)
{
    return iofil_p->read (1, &value);
}
inline uInt StManArrayFile::get (uInt& value)
{
    return iofil_p->read (1, &value);
}



} //# NAMESPACE CASACORE - END

#endif
