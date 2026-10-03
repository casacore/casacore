// # StArrayFile.h: Read/write array in external format for a storage manager
// # Copyright (C) 1994,1995,1996,1997,1999,2001,2002
// # Associated Universities, Inc. Washington DC, USA.
// #
// # This library is free software; you can redistribute it and/or modify it
// # under the terms of the GNU Library General Public License as published by
// # the Free Software Foundation; either version 2 of the License, or (at your
// # option) any later version.
// #
// # This library is distributed in the hope that it will be useful, but WITHOUT
// # ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// # FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
// # License for more details.
// #
// # You should have received a copy of the GNU Library General Public License
// # along with this library; if not, write to the Free Software Foundation,
// # Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
// #
// # Correspondence concerning AIPS++ should be addressed as follows:
// #        Internet email: casa-feedback@nrao.edu.
// #        Postal address: AIPS++ Project Office
// #                        National Radio Astronomy Observatory
// #                        520 Edgemont Road
// #                        Charlottesville, VA 22903-2475 USA

#ifndef TABLES_STARRAYFILE_H
#define TABLES_STARRAYFILE_H

// # Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/IO/RegularFileIO.h>
#include <casacore/casa/IO/TypeIO.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/BasicSL/Complex.h>

#include <memory>
#include <type_traits>

namespace casacore {  // # NAMESPACE CASACORE - BEGIN

// # Forward Declarations
class MultiFileBase;
class IPosition;

// <summary>
// Read/write array in external format for a storage manager
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <prerequisite>
// # Classes you should understand before using this one.
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
// void writeArray (const Array<Bool>& array)
// {
//     // Construct object and update file StArray.dat.
//     StManArrayFile arrayFile("StArray.dat, ByteIO::New);
//     // Reserve space for an array with the given shape and data type.
//     // This writes the shape at the end of the file and reserves
//     // space the hold the entire Bool array.
//     // It fills in the file offset where the shape is stored
//     // and returns the length of the shape in the file.
//     int64_t offset;
//     uInt shapeLength = arrayFile.putShape (array.shape(), offset, static_cast<bool*>(0));
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

class StManArrayFile {
 public:
  // Construct the object and attach it to the give file.
  // The OpenOption determines how the file is opened
  // (e.g. ByteIO::New for a new file).
  // The buffersize is used to allocate a buffer of a proper size
  // for the underlying filebuf object (see iostream package).
  // A bufferSize 0 means using the default size (currently 65536).
  StManArrayFile(const String& name, ByteIO::OpenOption, unsigned int version = 0,
                 bool bigEndian = true, unsigned int bufferSize = 0,
                 const std::shared_ptr<MultiFileBase>& = std::shared_ptr<MultiFileBase>());

  // Close the possibly opened file.
  ~StManArrayFile();

  // Flush and optionally fsync the data.
  // It returns true when any data was written since the last flush.
  bool flush(bool fsync);

  // Reopen the file for read/write access.
  void reopenRW();

  // Resync the file (i.e. clear possible cache information).
  void resync();

  // Return the current file length (merely a debug tool).
  int64_t length() { return leng_p; }

  // Put the array shape and store its file offset into the offset argument.
  // Reserve file space for the associated array.
  // The length of the shape part in the file is returned.
  // The file offset plus the shape length is the starting offset of the
  // actual array data (which can be used by get and put).
  // Space is reserved to store the reference count.
  // <group>
  unsigned int putShape(const IPosition& shape, int64_t& fileOffset, const bool*);
  unsigned int putShape(const IPosition& shape, int64_t& fileOffset, const char*) {
    return putRes(shape, fileOffset, sizeChar_p);
  }
  unsigned int putShape(const IPosition& shape, int64_t& fileOffset, const unsigned char*) {
    return putRes(shape, fileOffset, sizeuChar_p);
  }
  unsigned int putShape(const IPosition& shape, int64_t& fileOffset, const short*) {
    return putRes(shape, fileOffset, sizeShort_p);
  }
  unsigned int putShape(const IPosition& shape, int64_t& fileOffset, const unsigned short*) {
    return putRes(shape, fileOffset, sizeuShort_p);
  }
  unsigned int putShape(const IPosition& shape, int64_t& fileOffset, const int*) {
    return putRes(shape, fileOffset, sizeInt_p);
  }
  unsigned int putShape(const IPosition& shape, int64_t& fileOffset, const unsigned int*) {
    return putRes(shape, fileOffset, sizeuInt_p);
  }
  unsigned int putShape(const IPosition& shape, int64_t& fileOffset, const int64_t*) {
    return putRes(shape, fileOffset, sizeInt64_p);
  }
  unsigned int putShape(const IPosition& shape, int64_t& fileOffset, const uint64_t*) {
    return putRes(shape, fileOffset, sizeuInt64_p);
  }
  unsigned int putShape(const IPosition& shape, int64_t& fileOffset, const float*) {
    return putRes(shape, fileOffset, sizeFloat_p);
  }
  unsigned int putShape(const IPosition& shape, int64_t& fileOffset, const double*) {
    return putRes(shape, fileOffset, sizeDouble_p);
  }
  unsigned int putShape(const IPosition& shape, int64_t& fileOffset, const Complex*);
  unsigned int putShape(const IPosition& shape, int64_t& fileOffset, const DComplex*);
  unsigned int putShape(const IPosition& shape, int64_t& fileOffset, const String*);
  // </group>

  // Get the reference count.
  unsigned int getRefCount(int64_t offset);

  // Put the reference count.
  // An exception is thrown if a value other than 1 is put for version 0.
  void putRefCount(unsigned int refCount, int64_t offset);

  // Put nr elements at the given file offset and array offset.
  // The file offset of the first array element is the file offset
  // of the shape plus the length of the shape in the file.
  // The array offset is counted in number of elements. It can be
  // used to put only a (contiguous) section of the array.
  // <group>
  void put(int64_t fileOffset, int64_t arrayOffset, uint64_t nr, const bool* data);
  void put(int64_t fileOffset, int64_t arrayOffset, uint64_t nr, const char* data) {
    PutGeneric(fileOffset, arrayOffset, nr, data);
  }
  void put(int64_t fileOffset, int64_t arrayOffset, uint64_t nr, const unsigned char* data) {
    PutGeneric(fileOffset, arrayOffset, nr, data);
  }
  void put(int64_t fileOffset, int64_t arrayOffset, uint64_t nr, const short* data) {
    PutGeneric(fileOffset, arrayOffset, nr, data);
  }
  void put(int64_t fileOffset, int64_t arrayOffset, uint64_t nr, const unsigned short* data) {
    PutGeneric(fileOffset, arrayOffset, nr, data);
  }
  void put(int64_t fileOffset, int64_t arrayOffset, uint64_t nr, const int* data) {
    PutGeneric(fileOffset, arrayOffset, nr, data);
  }
  void put(int64_t fileOffset, int64_t arrayOffset, uint64_t nr, const unsigned int* data) {
    PutGeneric(fileOffset, arrayOffset, nr, data);
  }
  void put(int64_t fileOffset, int64_t arrayOffset, uint64_t nr, const int64_t* data) {
    PutGeneric(fileOffset, arrayOffset, nr, data);
  }
  void put(int64_t fileOffset, int64_t arrayOffset, uint64_t nr, const uint64_t* data) {
    PutGeneric(fileOffset, arrayOffset, nr, data);
  }
  void put(int64_t fileOffset, int64_t arrayOffset, uint64_t nr, const float* data) {
    PutGeneric(fileOffset, arrayOffset, nr, data);
  }
  void put(int64_t fileOffset, int64_t arrayOffset, uint64_t nr, const double* data) {
    PutGeneric(fileOffset, arrayOffset, nr, data);
  }
  // #//    void put (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, const long double*
  // data);
  void put(int64_t fileOffset, int64_t arrayOffset, uint64_t nr, const Complex* data);
  void put(int64_t fileOffset, int64_t arrayOffset, uint64_t nr, const DComplex* data);
  void put(int64_t fileOffset, int64_t arrayOffset, uint64_t nr, const String* data);
  // </group>

  // Get the shape at the given file offset.
  // It will reshape the IPosition vector when needed.
  // It returns the length of the shape in the file.
  unsigned int getShape(int64_t fileOffset, IPosition& shape);

  // Get nr elements at the given file offset and array offset.
  // The file offset of the first array element is the file offset
  // of the shape plus the length of the shape in the file.
  // The array offset is counted in number of elements. It can be
  // used to get only a (contiguous) section of the array.
  // <group>
  void get(int64_t fileOffset, int64_t arrayOffset, uint64_t nr, bool* data);
  void get(int64_t fileOffset, int64_t arrayOffset, uint64_t nr, char* data) {
    GetGeneric(fileOffset, arrayOffset, nr, data);
  }
  void get(int64_t fileOffset, int64_t arrayOffset, uint64_t nr, unsigned char* data) {
    GetGeneric(fileOffset, arrayOffset, nr, data);
  }
  void get(int64_t fileOffset, int64_t arrayOffset, uint64_t nr, short* data) {
    GetGeneric(fileOffset, arrayOffset, nr, data);
  }
  void get(int64_t fileOffset, int64_t arrayOffset, uint64_t nr, unsigned short* data) {
    GetGeneric(fileOffset, arrayOffset, nr, data);
  }
  void get(int64_t fileOffset, int64_t arrayOffset, uint64_t nr, int* data) {
    GetGeneric(fileOffset, arrayOffset, nr, data);
  }
  void get(int64_t fileOffset, int64_t arrayOffset, uint64_t nr, unsigned int* data) {
    GetGeneric(fileOffset, arrayOffset, nr, data);
  }
  void get(int64_t fileOffset, int64_t arrayOffset, uint64_t nr, int64_t* data) {
    GetGeneric(fileOffset, arrayOffset, nr, data);
  }
  void get(int64_t fileOffset, int64_t arrayOffset, uint64_t nr, uint64_t* data) {
    GetGeneric(fileOffset, arrayOffset, nr, data);
  }
  void get(int64_t fileOffset, int64_t arrayOffset, uint64_t nr, float* data) {
    GetGeneric(fileOffset, arrayOffset, nr, data);
  }
  void get(int64_t fileOffset, int64_t arrayOffset, uint64_t nr, double* data) {
    GetGeneric(fileOffset, arrayOffset, nr, data);
  }
  // #//    void get (int64_t fileOffset, int64_t arrayOffset, uint64_t nr, long double* data);
  void get(int64_t fileOffset, int64_t arrayOffset, uint64_t nr, Complex* data);
  void get(int64_t fileOffset, int64_t arrayOffset, uint64_t nr, DComplex* data);
  void get(int64_t fileOffset, int64_t arrayOffset, uint64_t nr, String* data);
  // </group>

  // Copy the array with <src>nr</src> elements from one file offset
  // to another.
  // <group>
  void copyArrayBool(int64_t to, int64_t from, uint64_t nr);
  void copyArrayChar(int64_t to, int64_t from, uint64_t nr) { copyData(to, from, nr * sizeChar_p); }
  void copyArrayuChar(int64_t to, int64_t from, uint64_t nr) {
    copyData(to, from, nr * sizeuChar_p);
  }
  void copyArrayShort(int64_t to, int64_t from, uint64_t nr) {
    copyData(to, from, nr * sizeShort_p);
  }
  void copyArrayuShort(int64_t to, int64_t from, uint64_t nr) {
    copyData(to, from, nr * sizeuShort_p);
  }
  void copyArrayInt(int64_t to, int64_t from, uint64_t nr) { copyData(to, from, nr * sizeInt_p); }
  void copyArrayuInt(int64_t to, int64_t from, uint64_t nr) { copyData(to, from, nr * sizeuInt_p); }
  void copyArrayInt64(int64_t to, int64_t from, uint64_t nr) {
    copyData(to, from, nr * sizeInt64_p);
  }
  void copyArrayuInt64(int64_t to, int64_t from, uint64_t nr) {
    copyData(to, from, nr * sizeuInt64_p);
  }
  void copyArrayFloat(int64_t to, int64_t from, uint64_t nr) {
    copyData(to, from, nr * sizeFloat_p);
  }
  void copyArrayDouble(int64_t to, int64_t from, uint64_t nr) {
    copyData(to, from, nr * sizeDouble_p);
  }
  // #//    void copyArrayLDouble  (int64_t to, int64_t from, uint64_t nr);
  void copyArrayComplex(int64_t to, int64_t from, uint64_t nr);
  void copyArrayDComplex(int64_t to, int64_t from, uint64_t nr);
  void copyArrayString(int64_t to, int64_t from, uint64_t nr);
  // </group>

 private:
  template <typename T>
  void PutGeneric(int64_t fileOff, int64_t arrayOff, uint64_t nr, const T* data) {
    setpos(fileOff + arrayOff * GetTypeSize<T>());
    iofil_p->write(nr, data);
    hasPut_p = true;
  }

  template <typename T>
  void GetGeneric(int64_t fileOff, int64_t arrayOff, uint64_t nr, T* data) {
    setpos(fileOff + arrayOff * GetTypeSize<T>());
    iofil_p->read(nr, data);
  }

  template <typename T>
  unsigned GetTypeSize() const {
    if constexpr (std::is_same_v<T, char>) {
      return sizeChar_p;
    } else if constexpr (std::is_same_v<T, unsigned char>) {
      return sizeuChar_p;
    } else if constexpr (std::is_same_v<T, short>) {
      return sizeShort_p;
    } else if constexpr (std::is_same_v<T, unsigned short>) {
      return sizeuShort_p;
    } else if constexpr (std::is_same_v<T, int>) {
      return sizeInt_p;
    } else if constexpr (std::is_same_v<T, unsigned int>) {
      return sizeuInt_p;
    } else if constexpr (std::is_same_v<T, int64_t>) {
      return sizeInt64_p;
    } else if constexpr (std::is_same_v<T, uint64_t>) {
      return sizeuInt64_p;
    } else if constexpr (std::is_same_v<T, float>) {
      return sizeFloat_p;
    } else if constexpr (std::is_same_v<T, double>) {
      return sizeDouble_p;
    } else {
      static_assert(sizeof(T) == 0, "Unsupported type for GetTypeSize");
      return 0;
    }
  }

  std::shared_ptr<ByteIO> file_p;   // # File object
  std::shared_ptr<TypeIO> iofil_p;  // # IO object
  int64_t leng_p;                   // # File length
  unsigned int version_p;           // # Version of StArrayFile file
  bool swput_p;                     // # true = put is possible
  bool hasPut_p;                    // # true = put since last flush
  unsigned int sizeChar_p;
  unsigned int sizeuChar_p;
  unsigned int sizeShort_p;
  unsigned int sizeuShort_p;
  unsigned int sizeInt_p;
  unsigned int sizeuInt_p;
  unsigned int sizeInt64_p;
  unsigned int sizeuInt64_p;
  unsigned int sizeFloat_p;
  unsigned int sizeDouble_p;

  // Put a single value at the current file offset.
  // It returns the length of the value in the file.
  // <group>
  unsigned int put(const int&);
  unsigned int put(const unsigned int&);
  // </group>

  // Put the array shape at the end of the file and reserve
  // space for nr elements (each lenElem bytes long).
  // It fills the file offset of the shape.
  // It returns the length of the shape in the file.
  unsigned int putRes(const IPosition& shape, int64_t& fileOffset, float lenElem);

  // Get a single value at the current file offset.
  // It returns the length of the value in the file.
  // <group>
  unsigned int get(int&);
  unsigned int get(unsigned int&);
  // </group>

  // Copy data with the given length from one file offset to another.
  void copyData(int64_t to, int64_t from, uint64_t length);

  // Position the file on the given offset.
  void setpos(int64_t offset);
};

inline void StManArrayFile::reopenRW() { file_p->reopenRW(); }
inline unsigned int StManArrayFile::put(const int& value) {
  hasPut_p = true;
  return iofil_p->write(1, &value);
}
inline unsigned int StManArrayFile::put(const unsigned int& value) {
  hasPut_p = true;
  return iofil_p->write(1, &value);
}
inline unsigned int StManArrayFile::get(int& value) { return iofil_p->read(1, &value); }
inline unsigned int StManArrayFile::get(unsigned int& value) { return iofil_p->read(1, &value); }

}  // namespace casacore

#endif
