//# ArrayUtil.h: Utility functions for arrays
//# Copyright (C) 1995,1999,2000
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

#if !defined(AIPS_ARRAYUTIL_H)
#define AIPS_ARRAYUTIL_H


//# Includes
#include <aips/aips.h>
#include <aips/Arrays/Vector.h>
#include <aips/Utilities/String.h>

//# Forward Declarations


// <summary>
// Split a String into its elements.
// </summary>

// <reviewed reviewer="" date="" tests="tArrayUtil">

// <prerequisite>
//   <li> <linkto class=Vector>Vector</linkto>
//   <li> <linkto class=String>String</linkto>
// </prerequisite>

// <etymology>
// stringToVector converts a String to a Vector of Strings.
// </etymology>

// <synopsis>
// The function stringToVector splits a string into its elements
// using the given delimiter and returns them in a <src>Vector<String></src>.
// The default delimiter is a comma (,).
// It is very useful when using a function taking a vector of strings
// as shown in the example.
// <p>
// A more advanced way of splitting a string is by using a
// <linkto class=Regex>regular expression</linkto> as delimiter.
// It makes it, for example, possible to treat whitespace around a comma
// as part of the delimiter (as shown in an example below).
// <p>
// A string with length 0 results in a zero-length vector.
// </synopsis>

// <motivation>
// As shown in the example, the function stringToVector makes
// passing a Vector of Strings far easier.
// </motivation>

// <example>
// <srcblock>
// someFunction (stringToVector ("abc,def ,,gh"));
// </srcblock>
// This results in a vector with 4 elements containing the values
// "abc", "def ", "", and "gh". The vector is passed to someFunction.
// This is far easier than having to do it as:
// <srcblock>
// Vector<String> vector(4);
// vector(0) = "abc";
// vector(1) = "def ";
// vector(2) = "";
// vector(3) = "gh";
// someFunction (vector);
// </srcblock>
//
// The following example shows how to use a delimiter consisting of a comma
// surrounded by possible whitespace.
// <srcblock>
// Vector<String> result = stringToVector (source, Regex(" *, *"));
// </srcblock>
// <example>

// <group name=stringToVector>
Vector<String> stringToVector (const String& string, char delim = ',');
Vector<String> stringToVector (const String& string, const Regex& delim);
// </group>



// <summary>
// Concatenate two Arrays.
// </summary>

// <reviewed reviewer="" date="" tests="tArrayUtil">

// <prerequisite>
//   <li> <linkto class=Array>Array</linkto>
// </prerequisite>

// <etymology>
// concatenateArray concatenates two Arrays into a new Array.
// </etymology>

// <synopsis>
// The function concatenates two Arrays into a new Array.
// The shape of both arrays must match except for the last dimension.
// The shape of the resulting array is equal to that of the input
// arrays with its last dimension as the sum of both last dimensions.
// <p>
// An exception ArrayConformanceError is thrown when the shapes
// do not match.
// </synopsis>

// <motivation>
// The table system needed this function.
// </motivation>

// <example>
// <srcblock>
// Vector<Int> vector1(5);
// Vector<Int> vector2(10);
// indgen (vector1);             // fill with values 0..4
// indgen (vector2);             // fill with values 0..9
// Vector<Int> result = concatenateVector (vector1, vector2);
// </srcblock>
// The example above results in a vector with length 15 and values
// 0,1,2,3,4,0,1,2,3,4,5,6,7,8,9.
// <p>
// It can also be used with matrices or arrays with higher dimensionality
// as long as all dimensions but the last one have equal length.
// <srcblock>
// Matrix<Int> matrix1 (3,4);
// Matrix<Int> matrix2 (3,5);
// Matrix<Int> matrix3 (4,4);
// // Concatenation of matrix1 and matrix 2 will succeed and result
// // in a 3x9 matrix.
// Matrix<Int> matrixConc = concatenateArray (matrix1, matrix2);
// if (matrixConc.shape() != IPosition(2,3,9)) {
//     cout << "Error in shape of concatenated matrices" << endl;
// }
// // Concatenation of matrix1 and matrix3 will fail, because the
// // first dimensions have a different length (3 vs. 4).
// try {
//     concatenateArray (matrix1, matrix2);
// } catch (ArrayConformanceError x) {
//     cout << x.getMesg() << endl;
// }
// </srcblock>
// <example>

// <group name=concatenateArray>
template<class T>
Array<T> concatenateArray (const Array<T>& left, const Array<T>& right);
// </group>


#endif
