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
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#ifndef CASA_ARRAYIO_H
#define CASA_ARRAYIO_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>

//# Forward declarations
#include <casacore/casa/iosfwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class AipsIO;
class LogIO;
class IPosition;
template<class T> class Array;
template<class T> class Matrix;
template<class T> class Vector;
template<class T> class Cube;
template<class T> class Block;

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
// Array <Float> array (shape);
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


// Write out an ascii representation of an array of any dimensionality.
// Arrays of dimensionality 3 or greater are written out vector by vector,
// preceeded by the position of the start of the vector. If the origin of
// the array isn't zero it is printed. The shape of the array is always
// printed.

template<class T> ostream &operator << (ostream &, const Array<T> &);

// Write a formatted copy of the array to the LogIO output object. Merely calls
// the ostream operator<< in turn.
template<class T> LogIO &operator<<(LogIO &os, const Array<T> &a);

// Read an ascii representation of an array. All types with an <src><<</src>
// operator can be handled. The basic format of the input should be:
// <srcblock>
//	[element element element ....] 
// </srcblock>
// Elements are separated by whitespace, or a comma, optionally surrounded
// by white space. <br>
// <note role=warning> Some input routines read fields between blank spaces. This
// is (at the moment) especially true for Quantities and Strings.
// In those cases
// the separator should be blank (or a comma following a blank), and the
// end ']' should have a blank in front.
// A crude fix for String arrays having separators <src>,</src> and <src>]</src>
// without blanks preceding has been made; but slows routines down </note>
// The default input is a vector of unspecified length. The input shape
// can be changed by pre-pending the input with:
// <srcblock>
//	{[shape]}
// </srcblock>
// where shape is an unsigned integer vector. The shape will be used to check
// the input length; and, depending on the possibility, to resize/reshape the
// result. However, reshaping of e.g. a Vector to a Matrix cannot be done, and
// the result will stay in the form asked.<br>
// Input order is row major, however by preceding the input with:
// <srcblock>
//	{T[shape]}	
// </srcblock>
// the order will be reversed.<br>
// Reshaping of the Array provided will depend on the type of Array and its
// state. If a general Array, the shape will be
// as defined by user. If fixed Array (e.g. Matrix, Vector, Cube) the number
// of dimesnsions will be kept. If the user specified more dimensions
// then supported (e.g. 3 for Matrix), the last dimesions will be collapsed.
// If less dimensions are specified, the missing ones will be set to 1.
// will be kept.<br>
// The read() version can be used to force a shape (ip), or an input
// transpose (it) (which can be undone by the user specifying transpose).
//
// <group>

template<class T> istream &operator >> (istream &s, Array<T> &x);
template<class T> Bool read(istream &s, Array<T> &x,
			    const IPosition *ip=0, Bool it=False);
// </group>

// General read support function for matrices.
// In principle these functions will not be
// used by general user, but could be. They can be used by Array type
// classes (like Slice, Lattice) to do the work of comparable input
// functions as the one for Arrays.
// In these functions p is the shape
// of the returned Block x. This shape is either deduced from the user
// specification; made equal to (1, nelements) if no user shape is
// given; is set to ip if specified. The function will return False (and
// p = (0)) in the case of an invalid input element; a number of elements
// input not equal to ip (if specified); the shape given by user as input
// does not conform to ip (if given) or the number of elements input.<br>
// trans will be True if transpose asked by user; or if forced by it.


template<class T> Bool readArrayBlock(istream &s, Bool &trans,
				      IPosition &p,
				      Block<T> &x,
				      const IPosition *ip=0, Bool it=False);


// Read or write a binary representation of an Array to a file. Very
// useful for saving arrays and restoring them later.
// <br>The putArray function is put in for forwards compatibility
// of images (so new images can be read with old release).
//
// <group>

template<class T> AipsIO &operator<< (AipsIO &, const Array<T> &);
template<class T> void putArray (AipsIO &, const Array<T> &, const Char* name);
template<class T> AipsIO &operator>> (AipsIO &, Array<T> &);

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
//    Matrix<Float> picture(256, 256); picture = 0.0;
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
template <class T>
void write_array (const Array<T>& the_array, const String& fileName);

template <class T>
inline void write_array (const Array<T>& the_array, const Char* fileName)
    { write_array (the_array, String(fileName)); }
// </group>

// Read the values of an array in binary format from a file with
// the given name.
// The number of values read is the size of the Array, thus the file
// should at least contain that number of values.
// <note role=warning>
// This function is only suitable for built-in data types.
// </note>
// <group>
template <class T>
void read_array (Array<T>& the_array, const String& fileName);

template <class T>
inline void read_array (Array<T>& the_array, const Char* fileName)
    { read_array (the_array, String(fileName)); }
// </group>

// </group>



// <summary>
// Global functions for Matrix/Vector input/output using ASCII format.
// </summary>

// <use visibility=export>

// <prerequisite>
//   <li> <linkto class=Matrix>Matrix</linkto>
//   <li> <linkto class=Vector>Vector</linkto>
// </prerequisite>

// <synopsis>
// These global functions support file I/O between ASCII files and
// Matrices or Vectors.
// </synopsis>

// <example>
// <srcblock>
//    Matrix<Float> picture(256, 256); picture = 0.0;
//    String fileName="picture.data";
//
//    // operations to populate picture
//    // ...
//
//    writeAsciiMatrix (picture, fileName);
// </srcblock>
// </example>

// <linkfrom anchor="Array Ascii IO" classes="Vector Matrix">
//    <here>Array Ascii IO</here> -- Simple Ascii input/output for Arrays.
// </linkfrom>

// <group name=Array Ascii IO>

// These routines read and write a Matrix of data.  The first line of
// input will be examined to determine the number of columns in the matrix.
// The maximum number of columns provided for is 100.  Each item may be up 
// to 50 characters long.
//
// Each item must be separated from others by one (or more) blank column.
// The "line" may be up to 1024 characters long.  Each subsequent line must
// contain the SAME number of items as the first line but may be any length
// (up to 1024 characters).
//
// The matrix need NOT be square.  
//
// The matrix should be declared but NOT dimensioned in the calling program.

// <group>
template <class T>
void readAsciiMatrix (Matrix<T>& mat, const Char* fileName);

template <class T>
void writeAsciiMatrix (const Matrix<T>& mat, const Char* fileName);
// </group>


// These two functions read and write a Vector of data.  The input
// may be arranged in any format (i.e. It may be recorded as one value per
// line or it may be recorded with all values on a single line).
// Values must be separated by whitespace.

// <group>
template <class T>
void readAsciiVector (Vector<T>& vec, const Char* fileName);

template <class T>
void writeAsciiVector (const Vector<T>& vec, const Char* fileName);
// </group>

// </group>



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Arrays/ArrayIO.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
