//# AipsIO.h: AipsIO is the object persistency mechanism of Casacore
//# Copyright (C) 1993,1994,1995,1996,1998,2000,2001
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

#ifndef CASA_AIPSIO_H
#define CASA_AIPSIO_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/IO/ByteIO.h>
#include <casacore/casa/vector.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class TypeIO;
class ByteIO;
class RegularFileIO;
class MultiFileBase;


// <summary> 
// AipsIO is the object persistency mechanism of Casacore
// </summary>

// <use visibility=export>

// <reviewed reviewer="ghunt" date="95Feb21" tests="" demos="">

// <etymology>
// AipsIO is simply the conventional shorthand for "AIPS++ input/output".
// Note that Casacore is the successor of the old AIPS++ project.
// </etymology>

// <synopsis> 
// AipsIO is a class designed to do I/O for objects.
// It reads/writes the data using a class derived from
// <linkto class=TypeIO>TypeIO</linkto>. For instance, class
// <linkto class=CanonicalIO>CanonicalIO</linkto> can be used
// to read/write the data in canonical format.
// <p>
// The TypeIO class in its turn uses a class derived from
// <linkto class=ByteIO>ByteIO</linkto> to determine where the data
// has to be written.
// <p>
// An object is written by writing all its data members.  It will be
// preceeded by a header containing type and version.
// The I/O can be done via de overloaded << and >> operators to write or
// read a single item (e.g., an int or an object).  These operators are
// already defined for all built-in data types and for Complex, DComplex,
// String, and Bool.
// Since each enumeration is a specific type, it is hard to handle them.
// Casting to Bool (which is also an enumerated type) is a possibility,
// but that assumes that each enumerated type has the same size (which
// is probably true for all compilers).
// Another possibility is to store it in an int when writing.  Reading
// can be done the opposite way, although the ARM says that an int
// cannot be assigned to an enumerated type.
// <p>
// There are also functions put, get and getnew to write or read an
// array of values.  These functions are defined for the same data types
// as << and >> (so one can write, for example, an array of Strings).
// AipsIO.put (nr, arr) writes nr values from the given array.
// AipsIO.get (nr, arr) reads nr values into the given user-supplied array.
// AipsIO.getnew (&nr, &arr) reads the number of values written into
// a new array allocated on the heap.  It returns the nr of values read
// and a pointer to the array.
// The data must be read back in the same order as it was written.
// <p>
// The functions <src>putstart(type,version)</src>
// and <src>putend()</src> must be called
// before resp. after writing all values of the object.
// It stores the given type and version of the object.
// Similarly <src>getstart(type)</src> and <src>getend()</src> must be called.
// getstart checks the type and returns the version.  By using the version
// the read function of the object can convert older versions of the
// object (which may still reside on disk) to the latest version.
// The function getNextType is available to get the type of the next
// object stored. This can be used to check the type or to open (i.e.
// issue a getstart) in the correct way.
// <p>
// When implementing a class, one should also define the operators << and >>
// for the class to allow users to write or read an object in this
// simple way (e.g., as  io >> obj;  to read an object).
// One has to define the friend functions:
// <srcblock>
//      friend AipsIO& operator<< (AipsIO&, const YourClass&);
//      friend AipsIO& operator>> (AipsIO&, YourClass&);
// </srcblock>
// since they cannot be stored in the class itself.
// The type of an object is usually passed as the class name.
// <srcblock>
//      AipsIO& operator<< (AipsIO& ios, const YourClass& object) {
//          ios.putstart ("YourClass", version);
//          ios << ....;
//          ios.putend ();
//      }
// </srcblock>
//
// The functions getpos() and setpos(offset) can be used to get and set
// the offset in the file to a given point.  They can be used to point
// to a position in the file where an object must be written or read.
// Obviously these functions are to be used by a storage manager and
// are not for public use.  Someday they should be made private with
// a friend defined.
// </synopsis> 

// <example>
// <srcblock>
//     MyClass myObject(...);                   // some object
//     AipsIO ios("file.name", ByteIO::New);    // create new file
//     ios << myObject;                         // write object
//     MyClass myObject2;
//     ios >> myObject2;                        // read it back
// </srcblock>
// This example creates an object, writes it to AipsIO and reads it
// back into another object.
// The shift functions for MyClass could be defined as follows:
// <srcblock>
// AipsIO& operator<< (AipsIO& ios, const MyClass& myObject)
// {
//     ios.putstart ("MyClass", 1);    // MyClass version 1
//     ios << ...;                     // write all data members
//     ios.putend();
// }
// AipsIO& operator>> (AipsIO& ios, const MyClass& myObject)
// {
//     // If needed, delete current data members first.
//     // Now read in the object.
//     uInt version = ios.getstart ("MyClass");
//     ios >> ...;                     // read all data members
//     ios.getend();
// }
// </srcblock>
// In this example the version is not used. In more complex objects
// it will probably be used when data members get added or changed
// in future versions of a software system.
// </example>


class AipsIO
{
public:
    // No file attached yet
    AipsIO();

    // Construct and open/create a file with the given name.
    // The actual IO is done via a CanonicalIO object on a regular file
    // using buffered IO with a buffer of the given size.
    // <br>If the MultiFileBase pointer is not null, a virtual file in the
    // MultiFileBase will be used instead of a regular file.
    explicit AipsIO (const String& fileName,
		     ByteIO::OpenOption = ByteIO::Old,
		     uInt filebufSize=65536,
////		     uInt filebufSize=1048576,
                     MultiFileBase* mfile=0);

    // Construct and open/create a file with the given name.
    // This can for instance by used to use AipsIO on a file descriptor
    // for which a <linkto class=FilebufIO>FilebufIO</linkto>
    // object has been created.
    // The actual IO is done via a CanonicalIO object on top of it.
    explicit AipsIO (ByteIO*);

    // Construct and open by connecting to the given file descriptor.
    // The actual IO is done via a filebuf object with a buffer
    // of the given size.
    explicit AipsIO (TypeIO*);

    // Close if not done yet
    ~AipsIO();

    // Open/create file (either a regular file or a MultiFileBase virtual file).
    // An exception is thrown if the object contains an already open file.
    void open (const String& fileName, ByteIO::OpenOption = ByteIO::Old,
	       uInt filebufSize=65536, MultiFileBase* mfile=0);

    // Open by connecting to the given byte stream.
    // This can for instance by used to use AipsIO on a file descriptor
    // for which a <linkto class=FilebufIO>FilebufIO</linkto>
    // object has been created.
    // The actual IO is done via a CanonicalIO object on top of it.
    // An exception is thrown if the object contains an already open file.
    void open (ByteIO*);

    // Open by connecting to the given typed byte stream.
    // An exception is thrown if the object contains an already open file.
    void open (TypeIO*);

    // Close file opened
    void close();

    // Return the file option.
    ByteIO::OpenOption fileOption() const;

    // Start putting an object.
    // This writes the object type and version. When reading back getstart
    // calls have to be done in the same way. Getstart
    // checks the type and returns the version. The user can use that to
    // correctly read back objects with different versions.
    // <br>
    // Data in the outermost object cannot be put before a putstart is done.
    // Data in nested objects can be put without an intermediate putstart.
    // However, for complex objects it is recommended to do a putstart
    // to have a better checking.
    // <br>
    // After all values (inclusing nested objects) of the object have
    // been put, a call to putend has to be done.
    // <group>
    uInt putstart (const String& objectType, uInt objectVersion);
    uInt putstart (const Char* objectType, uInt objectVersion);
    // </group>

    // Put a single value.
    // <group>
    AipsIO& operator<< (const Bool& value);
    AipsIO& operator<< (const Char& value);
    AipsIO& operator<< (const uChar& value);
    AipsIO& operator<< (const short& value);
    AipsIO& operator<< (const unsigned short& value);
    AipsIO& operator<< (const int& value);
    AipsIO& operator<< (const unsigned int& value);
    AipsIO& operator<< (const Int64& value);
    AipsIO& operator<< (const uInt64& value);
    AipsIO& operator<< (const float& value);
    AipsIO& operator<< (const double& value);
    AipsIO& operator<< (const Complex& value);
    AipsIO& operator<< (const DComplex& value);
    AipsIO& operator<< (const String& value);
    AipsIO& operator<< (const Char* value);
    // </group>

    // Put an array of values with the given number of values.
    // If the flag putNr is set, the number of values is put first.
    // <group>
    AipsIO& put (uInt nrval, const Bool* values, Bool putNR = True);
    AipsIO& put (uInt nrval, const Char* values, Bool putNR = True);
    AipsIO& put (uInt nrval, const uChar* values, Bool putNR = True);
    AipsIO& put (uInt nrval, const short* values, Bool putNR = True);
    AipsIO& put (uInt nrval, const unsigned short* values, Bool putNR = True);
    AipsIO& put (uInt nrval, const int* values, Bool putNR = True);
    AipsIO& put (uInt nrval, const unsigned int* values, Bool putNR = True);
    AipsIO& put (uInt nrval, const Int64* values, Bool putNR = True);
    AipsIO& put (uInt nrval, const uInt64* values, Bool putNR = True);
    AipsIO& put (uInt nrval, const float* values, Bool putNR = True);
    AipsIO& put (uInt nrval, const double* values, Bool putNR = True);
    AipsIO& put (uInt nrval, const Complex* values, Bool putNR = True);
    AipsIO& put (uInt nrval, const DComplex* values, Bool putNR = True);
    AipsIO& put (uInt nrval, const String* values, Bool putNR = True);
    // </group>

    // Put a vector as an array of values
    // For standard types it has the same result as put with putNR=True.
    template<typename T>
    AipsIO& put (const vector<T>& vec)
      { *this << uInt(vec.size());
        for (typename vector<T>::const_iterator iter=vec.begin();
             iter!=vec.end(); ++iter) {
          *this << *iter;
        }
        return *this;
      }
    //# Possibly specialize for standard types to make it faster.
    //# Specialize for a bool vector.
    AipsIO& put (const vector<Bool>& vec);


    // End putting an object. It returns the object length (including
    // possible nested objects).
    uInt putend();

    // Get and set file-offset.
    // <group>
    Int64 getpos ();
    Int64 setpos (Int64 offset);
    // </group>

    // Get the type of the next object stored.
    // This is not possible if a put is in progress.
    const String& getNextType();

    // Start reading an object. It will check if the given type matches
    // the one stored by putstart. It returns the object version which
    // can be used to read in older version of the object correctly.
    // <br>
    // After all values (inclusing nested objects) of the object have
    // been read, a call to getend has to be done.
    // <group>
    uInt getstart (const String& objectType);
    uInt getstart (const Char* objectType);
    // </group>

    // Get a single value.
    // <group>
    AipsIO& operator>> (Bool& value);
    AipsIO& operator>> (Char& value);
    AipsIO& operator>> (uChar& value);
    AipsIO& operator>> (short& value);
    AipsIO& operator>> (unsigned short& value);
    AipsIO& operator>> (int& value);
    AipsIO& operator>> (unsigned int& value);
    AipsIO& operator>> (Int64& value);
    AipsIO& operator>> (uInt64& value);
    AipsIO& operator>> (float& value);
    AipsIO& operator>> (double& value);
    AipsIO& operator>> (Complex& value);
    AipsIO& operator>> (DComplex& value);
    AipsIO& operator>> (String& value);
    // </group>

    // Read in nrval values into the user-supplied values buffer.
    // The buffer must be long enough.
    // <group>
    AipsIO& get (uInt nrval, Bool* values);
    AipsIO& get (uInt nrval, Char* values);
    AipsIO& get (uInt nrval, uChar* values);
    AipsIO& get (uInt nrval, short* values);
    AipsIO& get (uInt nrval, unsigned short* values);
    AipsIO& get (uInt nrval, int* values);
    AipsIO& get (uInt nrval, unsigned int* values);
    AipsIO& get (uInt nrval, Int64* values);
    AipsIO& get (uInt nrval, uInt64* values);
    AipsIO& get (uInt nrval, float* values);
    AipsIO& get (uInt nrval, double* values);
    AipsIO& get (uInt nrval, Complex* values);
    AipsIO& get (uInt nrval, DComplex* values);
    AipsIO& get (uInt nrval, String* values);
    // </group>

    // Get a vector as an array of values (similar to getnew).
    // It resizes the vector as needed.
    template<typename T>
    AipsIO& get (vector<T>& vec)
      { uInt sz;
        *this >> sz;
        vec.resize(sz);
        for (typename vector<T>::iterator iter=vec.begin();
             iter!=vec.end(); ++iter) {
          *this >> *iter;
        }
        return *this;
      }
    //# Specialize for a bool vector.
    AipsIO& get (vector<Bool>& vec);


    // Read in values as written by the function put.
    // It will read the number of values (into nrval), allocate a
    // values buffer of that length and read the values into that buffer.
    // A pointer to the buffer is returned into values.
    // <warn=caution> Although the buffer is allocated by this function,
    // the user has to delete it (using <src>delete [] values;</src>).
    // <group>
    AipsIO& getnew (uInt& nrval, Bool*& values);
    AipsIO& getnew (uInt& nrval, Char*& values);
    AipsIO& getnew (uInt& nrval, uChar*& values);
    AipsIO& getnew (uInt& nrval, short*& values);
    AipsIO& getnew (uInt& nrval, unsigned short*& values);
    AipsIO& getnew (uInt& nrval, int*& values);
    AipsIO& getnew (uInt& nrval, unsigned int*& values);
    AipsIO& getnew (uInt& nrval, Int64*& values);
    AipsIO& getnew (uInt& nrval, uInt64*& values);
    AipsIO& getnew (uInt& nrval, float*& values);
    AipsIO& getnew (uInt& nrval, double*& values);
    AipsIO& getnew (uInt& nrval, Complex*& values);
    AipsIO& getnew (uInt& nrval, DComplex*& values);
    AipsIO& getnew (uInt& nrval, String*& values);
    // </group>

    // End reading an object. It returns the object length (including
    // possible nested objects).
    // It checks if the entire object has been read (to keep the data
    // stream in sync). If not, an exception is thrown.
    uInt getend();

private:
    // Initialize everything for the open.
    // It checks if there is no outstanding open file.
    void openInit (ByteIO::OpenOption);

    // Test if put is possible (throw exception if not).
    void testput();

    // Test if get is possible (throw exception if not).
    void testget();

    // Test if get did not exceed object.
    void testgetLength();

    // Throw exception for testput
    void testputerr();

    // Throw exception for testget
    void testgeterr();

    // Throw exception for testgetLength
    void testgeterrLength();


    //  1 = file was opened by AipsIO
    //  0 = file not opened
    // -1 = file opened by user (=fd passed)
    Int          opened_p;
    // File open option
    ByteIO::OpenOption fopt_p;
    // <0 = not opened for put
    //  0 = no putstart done
    // >0 = put is possible
    int          swput_p;
    // <0 = not opened for get
    //  0 = no getstart done
    // >0 = get is possible
    int          swget_p;
    // Nested object level
    uInt         level_p;
    // Current size of objlen and objptr
    uInt         maxlev_p;
    // Object length at each level
    Block<uInt>  objlen_p;
    // Object length to be read at each level
    Block<uInt>  objtln_p;
    // Offset of length at each level
    Block<Int64>  objptr_p;
    // True = the object type has already been read
    Bool         hasCachedType_p;
    // The cached object type.
    String       objectType_p;
    // The file object.
    ByteIO*      file_p;
    // The actual IO object.
    TypeIO*      io_p;
    // Is the file is seekable?
    Bool         seekable_p;
    // magic value to check sync.
    static const uInt magicval_p;
};



// Return the file option.
inline ByteIO::OpenOption AipsIO::fileOption() const
{ return fopt_p; }


// testput tests if a put can be done; ie. if putstart has been done.
// It throws an exception if not.
// testget is similar to test if a get can be done.
inline void AipsIO::testput()
{
    if (swput_p <= 0) {
	testputerr();
    }
}
inline void AipsIO::testget()
{
    if (swget_p <= 0) {
	testgeterr();
    }
}
inline void AipsIO::testgetLength()
{
    if (objlen_p[level_p] > objtln_p[level_p]) {
	testgeterrLength();
    }
}



} //# NAMESPACE CASACORE - END

#endif
