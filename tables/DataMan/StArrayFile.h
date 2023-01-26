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

#ifndef TABLES_STARRAYFILE_H
#define TABLES_STARRAYFILE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/IO/RegularFileIO.h>
#include <casacore/casa/IO/TypeIO.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <memory>

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
// (all in little or big endian format). An array of strings is written as
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
// void writeArray (const Array<bool>& array)
// {
//     // Construct object and update file StArray.dat.
//     StManArrayFile arrayFile("StArray.dat, ByteIO::New);
//     // Reserve space for an array with the given shape and data type.
//     // This writes the shape at the end of the file and reserves
//     // space the hold the entire bool array.
//     // It fills in the file offset where the shape is stored
//     // and returns the length of the shape in the file.
//     int64_t offset;
//     uint32_t shapeLength = arrayFile.putShape (array.shape(), offset, static_cast<bool*>(0));
//     // Now put the actual array.
//     // This has to be put at the returned file offset plus the length
//     // of the shape in the file.
//     bool deleteIt;
//     const bool* dataPtr = array.getStorage (deleteIt);
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
		    uint32_t version=0, bool bigEndian=true,
		    uint32_t bufferSize=0,
                    const std::shared_ptr<MultiFileBase>& = std::shared_ptr<MultiFileBase>());

    // Close the possibly opened file.
    ~StManArrayFile();

    // Flush and optionally fsync the data.
    // It returns true when any data was written since the last flush.
    bool flush (bool fsync);

    // Reopen the file for read/write access.
    void reopenRW();

    // Resync the file (i.e. clear possible cache information).
    void resync();

    // Return the current file length (merely a debug tool).
    int64_t length()
	{ return leng_p; }

    // Put the array shape and store its file offset into the offset argument.
    // Reserve file space for the associated array.
    // The length of the shape part in the file is returned.
    // The file offset plus the shape length is the starting offset of the
    // actual array data (which can be used by get and put).
    // Space is reserved to store the reference count.
    // <group>
    uint32_t putShape (const IPosition& shape, int64_t& fileOffset,
		   const bool* dummy);
    uint32_t putShape (const IPosition& shape, int64_t& fileOffset,
		   const char* dummy);
    uint32_t putShape (const IPosition& shape, int64_t& fileOffset,
		   const unsigned char* dummy);
    uint32_t putShape (const IPosition& shape, int64_t& fileOffset,
		   const int16_t* dummy);
    uint32_t putShape (const IPosition& shape, int64_t& fileOffset,
		   const uint16_t* dummy);
    uint32_t putShape (const IPosition& shape, int64_t& fileOffset,
		   const int32_t* dummy);
    uint32_t putShape (const IPosition& shape, int64_t& fileOffset,
		   const uint32_t* dummy);
    uint32_t putShape (const IPosition& shape, int64_t& fileOffset,
		   const int64_t* dummy);
    uint32_t putShape (const IPosition& shape, int64_t& fileOffset,
		   const uint64_t* dummy);
    uint32_t putShape (const IPosition& shape, int64_t& fileOffset,
		   const float* dummy);
    uint32_t putShape (const IPosition& shape, int64_t& fileOffset,
		   const double* dummy);
    uint32_t putShape (const IPosition& shape, int64_t& fileOffset,
		   const Complex* dummy);
    uint32_t putShape (const IPosition& shape, int64_t& fileOffset,
		   const DComplex* dummy);
    uint32_t putShape (const IPosition& shape, int64_t& fileOffset,
		   const String* dummy);
    // </group>

    // Get the reference count.
    uint32_t getRefCount (int64_t offset);

    // Put the reference count.
    // An exception is thrown if a value other than 1 is put for version 0.
    void putRefCount (uint32_t refCount, int64_t offset);

    // Put nr elements at the given file offset and array offset.
    // The file offset of the first array element is the file offset
    // of the shape plus the length of the shape in the file.
    // The array offset is counted in number of elements. It can be
    // used to put only a (contiguous) section of the array.
    // <group>
    void put (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, const bool*);
    void put (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, const char*);
    void put (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, const unsigned char*);
    void put (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, const int16_t*);
    void put (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, const uint16_t*);
    void put (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, const int32_t*);
    void put (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, const uint32_t*);
    void put (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, const int64_t*);
    void put (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, const uint64_t*);
    void put (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, const float*);
    void put (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, const double*);
//#//    void put (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, const long double*);
    void put (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, const Complex*);
    void put (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, const DComplex*);
    void put (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, const String*);
    // </group>

    // Get the shape at the given file offset.
    // It will reshape the IPosition vector when needed.
    // It returns the length of the shape in the file.
    uint32_t getShape (int64_t fileOffset, IPosition& shape);

    // Get nr elements at the given file offset and array offset.
    // The file offset of the first array element is the file offset
    // of the shape plus the length of the shape in the file.
    // The array offset is counted in number of elements. It can be
    // used to get only a (contiguous) section of the array.
    // <group>
    void get (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, bool*);
    void get (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, char*);
    void get (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, unsigned char*);
    void get (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, int16_t*);
    void get (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, uint16_t*);
    void get (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, int32_t*);
    void get (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, uint32_t*);
    void get (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, int64_t*);
    void get (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, uint64_t*);
    void get (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, float*);
    void get (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, double*);
//#//    void get (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, long double*);
    void get (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, Complex*);
    void get (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, DComplex*);
    void get (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, String*);
    // </group>

    // Copy the array with <src>nr</src> elements from one file offset
    // to another.
    // <group>
    void copyArrayBool     (int64_t to, int64_t from, uint64_t nr);
    void copyArrayChar     (int64_t to, int64_t from, uint64_t nr);
    void copyArrayuChar    (int64_t to, int64_t from, uint64_t nr);
    void copyArrayShort    (int64_t to, int64_t from, uint64_t nr);
    void copyArrayuShort   (int64_t to, int64_t from, uint64_t nr);
    void copyArrayInt      (int64_t to, int64_t from, uint64_t nr);
    void copyArrayuInt     (int64_t to, int64_t from, uint64_t nr);
    void copyArrayInt64    (int64_t to, int64_t from, uint64_t nr);
    void copyArrayuInt64   (int64_t to, int64_t from, uint64_t nr);
    void copyArrayFloat    (int64_t to, int64_t from, uint64_t nr);
    void copyArrayDouble   (int64_t to, int64_t from, uint64_t nr);
//#//    void copyArrayLDouble  (int64_t to, int64_t from, uint64_t nr);
    void copyArrayComplex  (int64_t to, int64_t from, uint64_t nr);
    void copyArrayDComplex (int64_t to, int64_t from, uint64_t nr);
    void copyArrayString   (int64_t to, int64_t from, uint64_t nr);
    // </group>

private:
    ByteIO* file_p;                //# File object
    TypeIO* iofil_p;               //# IO object
    int64_t   leng_p;                //# File length
    uint32_t    version_p;             //# Version of StArrayFile file
    bool    swput_p;               //# true = put is possible
    bool    hasPut_p;              //# true = put since last flush
    uint32_t    sizeChar_p;
    uint32_t    sizeuChar_p;
    uint32_t    sizeShort_p;
    uint32_t    sizeuShort_p;
    uint32_t    sizeInt_p;
    uint32_t    sizeuInt_p;
    uint32_t    sizeInt64_p;
    uint32_t    sizeuInt64_p;
    uint32_t    sizeFloat_p;
    uint32_t    sizeDouble_p;

    // Put a single value at the current file offset.
    // It returns the length of the value in the file.
    // <group>
    uint32_t put (const int32_t&);
    uint32_t put (const uint32_t&);
    // </group>

    // Put the array shape at the end of the file and reserve
    // space for nr elements (each lenElem bytes long).
    // It fills the file offset of the shape.
    // It returns the length of the shape in the file.
    uint32_t putRes (const IPosition& shape, int64_t& fileOffset, float lenElem);

    // Get a single value at the current file offset.
    // It returns the length of the value in the file.
    // <group>
    uint32_t get (int32_t&);
    uint32_t get (uint32_t&);
    // </group>

    // Copy data with the given length from one file offset to another.
    void copyData (int64_t to, int64_t from, uint64_t length);

    // Position the file on the given offset.
    void setpos (int64_t offset);
};
    

inline void StManArrayFile::reopenRW()
{
    file_p->reopenRW();
}
inline uint32_t StManArrayFile::put (const int32_t& value)
{
    hasPut_p = true;
    return iofil_p->write (1, &value);
}
inline uint32_t StManArrayFile::put (const uint32_t& value)
{
    hasPut_p = true;
    return iofil_p->write (1, &value);
}
inline uint32_t StManArrayFile::get (int32_t& value)
{
    return iofil_p->read (1, &value);
}
inline uint32_t StManArrayFile::get (uint32_t& value)
{
    return iofil_p->read (1, &value);
}



} //# NAMESPACE CASACORE - END

#endif
