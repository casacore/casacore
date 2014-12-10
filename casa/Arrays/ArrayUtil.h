//# ArrayUtil.h: Utility functions for arrays
//# Copyright (C) 1995,1999,2000,2001
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

#ifndef CASA_ARRAYUTIL_H
#define CASA_ARRAYUTIL_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class Regex;

// <summary>
// Split a String into its elements.
// </summary>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tArrayUtil">

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

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tArrayUtil">

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



// <summary> Helper function for partialX functions </summary>
// <use visibility=export>
// <synopsis>
// This is a specialized helper function for functions like partialSums.
// It determines the shape of the resulting array and calculates the
// result increments when iterating linearly through the source array.
// It returns the first result axis which indicates the number of the first
// contiguous collapse axes. The number of contiguous data points is
// returned in nelemCont.
// </synopsis>
// <group name=partialFuncHelper>
uInt partialFuncHelper (Int& nelemCont,
			IPosition& resultShape, IPosition& incr,
			const IPosition& sourceShape,
			const IPosition& collapseAxes);
// </group>


// <summary>
// Reverse the order of one or more axes of an array.
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tArrayUtil2.cc">

// <synopsis>
// This function makes it possible to reverse one or more axes of an array by
// swapping around the elements of each axis.
// The resulting array is a copy of the input array with its data
// moved around according to the new order.
// If the order does not change, a copy is returned if the
// <src>alwaysCopy</src> is true. Otherwise a reference of the
// input array is returned.
// <p>
// Reversing axis 0 means that its elements are reversed.
// Reversing axis 1 means that the 
// </synopsis>

// <example>
// Reversing axis 0 of a Vector means that the Vector is reversed.
// Reversing axis 1 of a Matrix means that its rows are reversed.
// Reversing axis 0 of an N-dim array means that the elements of each Vector
// in that array are reversed.
// Reversing axis 1 of a Matrix means that its columns are reversed.
// </example>

// <group name=reverseArray>
template<class T>
Array<T> reverseArray (const Array<T>& array,
                       const IPosition& reversedAxes,
                       Bool alwaysCopy = True);
template<class T>
Array<T> reverseArray (const Array<T>& array, uInt axis,
                       Bool alwaysCopy = True);
// </group>


// <summary>
// Reorder the axes of an array.
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tArrayUtil2.cc">

// <synopsis>
// This function makes it possible to reorder the axes of an array.
// The resulting array is a copy of the input array with its data
// moved around according to the new array order.
// If the order does not change, a copy is returned if the
// <src>alwaysCopy</src> is true. Otherwise a reference of the
// input array is returned.
// <p>
// The <src>newAxisOrder</src> defines the new axes order.
// Its length can be less than the dimensionality of the input array.
// It is appended with the non-specified axes in their natural order.
// <src>newAxisOrder(i)</src> gives the axis in the original array
// which will now get axis <src>i</src>.
// </synopsis>

// <example>
// <srcblock>
//   Array<Int> result = reorderArray (someArray, IPosition(2,1,3));
// </srcblock>
// Say that someArray is a 4D array with shape [3,4,5,6].
// The non-specified axes get appended to the axis order
// specification [1,3] resulting in [1,3,0,2].
// <br> This means that axis 1 gets axis 0, axis 3 gets axis 1, axis 0 gets
// axis 2, and axis 2 gets axis 3.
// Thus the resulting shape is [4,6,3,5] and the data are moved accordingly.
// </example>

// <motivation>
// This function was needed for an efficient implementation of the
// functions partialMedians and partialFractiles.
// </motivation>

// <group name=reorderArray>
template<class T>
Array<T> reorderArray (const Array<T>& array,
		       const IPosition& newAxisOrder,
		       Bool alwaysCopy = True);
// </group>


// <summary>
// Helper function for function reorderArray.
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tArrayUtil2.cc">

// <synopsis>
// This is a specialized helper function for function reorderArray.
// It determines the shape of the resulting array and calculates the
// result increments when iterating linearly through the source array.
// It returns the number of the first non-reordered axes.
// </synopsis>

// <motivation>
// Split off common non-templated code.
// </motivation>

// <group name=reorderArrayHelper>
uInt reorderArrayHelper (IPosition& newShape, IPosition& incr,
			 const IPosition& shape, const IPosition& newAxisOrder);
// </group>



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Arrays/ArrayUtil.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
