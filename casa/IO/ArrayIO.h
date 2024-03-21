//# ArrayIO.h: text output and binary IO for an array of any dimensionality.
//# Copyright (C) 1993,1994,1995,1997,1999,2000,2001
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
//#                        charlottesville, VA 22903-2475 USA

#ifndef CASA_ARRAYIO_2_H
#define CASA_ARRAYIO_2_H

//# Includes
#include <vector>
#include <ostream>
#include <regex>

#include <casacore/casa/Arrays/ArrayFwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class AipsIO;
class LogIO;
class IPosition;
template<typename T> class Block;
class String;

// <summary>
//    Input/output operators for Arrays.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Paul Shannon" date="1995/02/21" tests="" demos="">
// This header was reviewed and revised with the goal of making it an
// example for those writing global function header files.
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Array>Array</linkto>
//   <li> ostream
//   <li> <linkto class=AipsIO>AipsIO</linkto>
// </prerequisite>

// <etymology>
//  ArrayIO is simply the conventional shorthand for "array input/output".
// </etymology>

// <synopsis> 
// These global functions provide easy input and output of (possibly)
// large and (possibly) multi-dimensional arrays.  Iteration through
// entire arrays is done behind the scenes, with no effort required
// of the client programmer.  
// These functions are global, rather than member functions of the
// Array class, because of the well-known C++ requirement that the first
// argument to an operator function (as it is declared) is the 
// left operand when the function is called.
// </synopsis>


// <example>
// <srcblock>
// IPosition shape (3,10,10,3);
// Array <float> array (shape);
// // ...initialize and manipulate the array...
// cout << "result: " << array;
// </srcblock>
//
// <motivation>
// Effortless input/output is clearly a big win.
// </motivation>
//
// <todo asof="1997/01/15">
// </todo>

// <linkfrom anchor="Array IO" classes="Array Vector Matrix Cube">
//    <here>Array IO</here> -- Input/output operators for Arrays.
// </linkfrom>
//
// <group name="Array IO">

// Write a formatted copy of the array to the LogIO output object. Merely calls
// the ostream operator<< in turn.
template<typename T, typename Alloc>
LogIO &operator<<(LogIO &os, const Array<T> &a);

// Read or write a binary representation of an Array to a file. Very
// useful for saving arrays and restoring them later.
// <br>The putArray function is put in for forwards compatibility
// of images (so new images can be read with old release).
//
// <group>

template<typename T, typename Alloc>
AipsIO &operator<< (AipsIO &, const Array<T> &);

template<typename T, typename Alloc>
void putArray (AipsIO &, const Array<T> &, const char* name);

template<typename T, typename Alloc>
AipsIO &operator>> (AipsIO &, Array<T> &);

// </group>

// </group>

// <summary>
// Global functions to read/write binary arrays from/to a file.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Gareth Hunt" date="95Mar31" tests="" demos="">
// </reviewed>

// <synopsis>
// These global functions provide disk read/write functions for an Array of
// binary numbers.  The write operation is useful, for example, to dump an
// image in binary form to disk so that it can be displayed with an external
// utility such as SAOimage.
// </synopsis>

// <example>
// <srcblock>
//    Matrix<float> picture(256, 256); picture = 0.0;
//    String fileName="picture.data";
//
//    // operations to populate picture
//    // ...
//
//    write_array (picture, fileName);
// </srcblock>
// </example>

// <todo asof="">
//   <li> These functions should eventually be replaced with something
//        more sophisticated.
// </todo>

// <linkfrom anchor="Array binary IO" classes="Array Vector Matrix Cube">
//    <here>Array binary IO</here> -- Simple binary input/output for Arrays.
// </linkfrom>

// <group name=Array binary IO>

// Write the values of an array in binary format into a file with
// the given name.
// The values are stored in local format, thus are not converted
// to a canonical format as
// <linkto class="AipsIO:description">AipsIO</linkto>
// does.
// <note role=warning>
// This function is only suitable for built-in data types.
// </note>
// <group>
template <typename T, typename Alloc>
void write_array (const Array<T>& the_array, const std::string& fileName);

template <typename T, typename Alloc>
inline void write_array (const Array<T>& the_array, const char* fileName)
    { write_array (the_array, std::string(fileName)); }
// </group>

// Read the values of an array in binary format from a file with
// the given name.
// The number of values read is the size of the Array, thus the file
// should at least contain that number of values.
// <note role=warning>
// This function is only suitable for built-in data types.
// </note>
// <group>
template <typename T, typename Alloc>
void read_array (Array<T>& the_array, const std::string& fileName);

template <typename T, typename Alloc>
inline void read_array (Array<T>& the_array, const char* fileName)
    { read_array (the_array, std::string(fileName)); }
// </group>

// </group>

// These two functions read and write a Vector of data.  The input
// may be arranged in any format (i.e. It may be recorded as one value per
// line or it may be recorded with all values on a single line).
// Values must be separated by whitespace.

// <group>
template <typename T, typename Alloc>
void readAsciiVector (Vector<T>& vec, const char* fileName);

template <typename T, typename Alloc>
void writeAsciiVector (const Vector<T>& vec, const char* fileName);
// </group>

// </group>

AipsIO& operator<< (AipsIO& aio, const IPosition& ip);
AipsIO& operator>> (AipsIO& aio, IPosition& ip);
LogIO& operator<< (LogIO& os, const IPosition& ip);

template<typename T, typename Alloc>
Block<T> makeBlock(const Array<T>& array);

template<typename T>
Vector<T> makeVector(const Block<T>& block);

Vector<String> stringToVector (const String& string, char delim = ',');
Vector<String> stringToVector (const String& string, const std::regex& delim);

} //# NAMESPACE CASACORE - END

#include <casacore/casa/IO/ArrayIO.tcc>

#endif
