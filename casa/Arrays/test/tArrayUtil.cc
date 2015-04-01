//# tArrayUtil.cc: Test program for functions in ArrayUtil.h
//# Copyright (C) 1995,1996,1998,1999,2000,2001,2002
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

#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayError.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/Arrays/ArrayUtil.h>

#include <casacore/casa/namespace.h>
// <summary>
// Test of functions in ArrayUtil.h.
// </summary>

// This program tests the various functions in ArrayUtil.h.
// When an argument is given to the program, it will not invoke any
// function resulting in an exception. This mode can be used to do
// proper detection of memory leaks using tools like TestCenter.


Bool testStringToVector (Bool)
{
    cout << "stringToVector..." << endl;
    Bool ok = True;

    Vector<String> vec1 = stringToVector ("");
    if (vec1.nelements() != 0) {
	cout << "Empty string should result in vector length 0" << endl;
	ok = False;
    }

    Vector<String> vec2 = stringToVector ("abc");
    if (vec2.nelements() != 1  ||  vec2(0) != "abc") {
	cout << "<abc> should result in vector length 1" << endl;
	ok = False;
    }

    Vector<String> vec3 = stringToVector (",");
    if (vec3.nelements() != 2  ||  vec3(0) != ""  ||  vec3(1) != "") {
	cout << "<,> should result in vector length 2" << endl;
	ok = False;
    }

    Vector<String> vec4 = stringToVector ("abc,defg,,h");
    if (vec4.nelements() != 4  ||  vec4(0) != "abc"  ||  vec4(1) != "defg"
    ||  vec4(2) != ""  ||  vec4(3) != "h") {
	cout << "<abc,defg,,h> should result in vector length 4" << endl;
	ok = False;
    }

    Vector<String> vec5 = stringToVector (",abc,defg,");
    if (vec5.nelements() != 4  ||  vec5(0) != ""  ||  vec5(1) != "abc"
    ||  vec5(2) != "defg"  ||  vec5(3) != "") {
	cout << "<,abc,defg,> should result in vector length 4" << endl;
	ok = False;
    }

    return ok;
}


Bool testStringToVectorRegex (Bool)
{
    cout << "stringToVectorRegex..." << endl;
    // Test using multiple spaces and a single comma as delimiter.
    Regex delim(" *, *");
    Bool ok = True;

    Vector<String> vec1 = stringToVector ("", delim);
    if (vec1.nelements() != 0) {
	cout << "Empty string should result in vector length 0" << endl;
	ok = False;
    }

    Vector<String> vec2 = stringToVector ("abc", delim);
    if (vec2.nelements() != 1  ||  vec2(0) != "abc") {
	cout << "<abc> should result in vector length 1" << endl;
	ok = False;
    }

    Vector<String> vec3 = stringToVector (",", delim);
    if (vec3.nelements() != 2  ||  vec3(0) != ""  ||  vec3(1) != "") {
	cout << "<,> should result in vector length 2" << endl;
	ok = False;
    }

    Vector<String> vec4 = stringToVector ("abc,defg,,h", delim);
    if (vec4.nelements() != 4  ||  vec4(0) != "abc"  ||  vec4(1) != "defg"
    ||  vec4(2) != ""  ||  vec4(3) != "h") {
	cout << "<abc,defg,,h> should result in vector length 4" << endl;
	ok = False;
    }

    Vector<String> vec5 = stringToVector (",abc,defg,", delim);
    if (vec5.nelements() != 4  ||  vec5(0) != ""  ||  vec5(1) != "abc"
    ||  vec5(2) != "defg"  ||  vec5(3) != "") {
	cout << "<,abc,defg,> should result in vector length 4" << endl;
	ok = False;
    }

    Vector<String> vec6 = stringToVector ("  ,  ", delim);
    if (vec6.nelements() != 2  ||  vec6(0) != ""  ||  vec6(1) != "") {
	cout << "<  ,  > should result in vector length 2" << endl;
	ok = False;
    }

    Vector<String> vec7 = stringToVector ("abc  ,  defg  ,  ,  h", delim);
    if (vec7.nelements() != 4  ||  vec7(0) != "abc"  ||  vec7(1) != "defg"
    ||  vec7(2) != ""  ||  vec7(3) != "h") {
	cout << "<abc  ,  defg  ,  ,  h> should result in vector length 4" << endl;
	ok = False;
    }

    Vector<String> vec8 = stringToVector (" abc  ", delim);
    if (vec8.nelements() != 1  ||  vec8(0) != " abc  ") {
	cout << "< abc  > should result in vector length 1" << endl;
	ok = False;
    }

    return ok;
}


Bool testConcatenateArray (Bool doExcp)
{
    cout << "concatenateArray..." << endl;
    Bool ok = True;
    Matrix<Int> matrix1 (3u,4u);
    Matrix<Int> matrix2 (3u,5u);
    Matrix<Int> matrix3 (4u,4u);
    Vector<Int> vector1 (4);
    Vector<Int> vector2 (6);
    indgen (matrix1);
    indgen (matrix2, Int(matrix1.nelements()));
    indgen (matrix3);
    indgen (vector1);
    indgen (vector2, Int(vector1.nelements()));
    Matrix<Int> matrixConc = concatenateArray (matrix1, matrix2);
    if (matrixConc.shape() != IPosition(2,3,9)) {
	cout << "Error in shape of concatenated matrices" << endl;
	ok = False;
    }
    uInt i, j;
    Int value = 0;
    for (j=0; j<9; j++) {
	for (i=0; i<3; i++) {
	    if (matrixConc(i,j) != value++) {
		cout << "Error in matrix on " << i << "," << j << endl;
		ok = False;
	    }
	}
    }

    Vector<Int> vectorConc = concatenateArray (vector1, vector2);
    if (vectorConc.shape() != IPosition(1,10)) {
	cout << "Error in shape of concatenated vectors" << endl;
	ok = False;
    }
    value = 0;
    for (i=0; i<10; i++) {
	if (vectorConc(i) != value++) {
	    cout << "Error in vector on " << i << endl;
	    ok = False;
	}
    }
    
    if (doExcp) {
	try {
	    concatenateArray (matrix1, matrix3);
	    ok = False;             // should not get here
	    cout << "1st concatenateArray exception not thrown" << endl;
	} catch (ArrayConformanceError x) {
	} 
	try {
	    concatenateArray (matrix1, vector1);
	    ok = False;             // should not get here
	    cout << "2nd concatenateArray exception not thrown" << endl;
	} catch (ArrayConformanceError x) {
	} 
    }

    return ok;
}


Bool testReorderArray (Bool doExcp)
{
  Bool ok = True;
  {
    cout << "arrayReorder 2D..." << endl;
    IPosition shape(2,3,4);
    Array<Int> arr(shape);
    indgen(arr);
    for (Int j0=0; j0<2; j0++) {
      for (Int j1=0; j1<2; j1++) {
	if (j1 != j0) {
	  IPosition axisOrder(2, j0, j1);
	  Array<Int> res = reorderArray (arr, axisOrder);
	  const IPosition& resShape = res.shape();
	  IPosition posOld(2);
	  IPosition posNew(2);
	  for (Int i1=0; i1<resShape(1); i1++) {
	    posNew(1) = i1;
	    posOld(axisOrder(1)) = i1;
	    for (Int i0=0; i0<resShape(0); i0++) {
	      posNew(0) = i0;
	      posOld(axisOrder(0)) = i0;
	      if (arr(posOld) != res(posNew)) {
		ok = False;
		cout << "for shape " << shape << resShape
		     << ", axisorder " << axisOrder << endl;
		cout << " result is " << res << endl;
	      }
	    }
	  }
	}
      }
    }
  }
  {
    cout << "arrayReorder 3D..." << endl;
    IPosition shape(3,3,4,5);
    Array<Int> arr(shape);
    indgen(arr);
    for (Int j0=0; j0<3; j0++) {
      for (Int j1=0; j1<3; j1++) {
	if (j1 != j0) {
	  for (Int j2=0; j2<3; j2++) {
	    if (j2 != j0  &&  j2 != j1) {
	      IPosition axisOrder(3, j0, j1, j2);
	      Array<Int> res = reorderArray (arr, axisOrder);
	      const IPosition& resShape = res.shape();
	      IPosition posOld(3);
	      IPosition posNew(3);
	      for (Int i2=0; i2<resShape(2); i2++) {
		posNew(2) = i2;
		posOld(axisOrder(2)) = i2;
		for (Int i1=0; i1<resShape(1); i1++) {
		  posNew(1) = i1;
		  posOld(axisOrder(1)) = i1;
		  for (Int i0=0; i0<resShape(0); i0++) {
		    posNew(0) = i0;
		    posOld(axisOrder(0)) = i0;
		    if (arr(posOld) != res(posNew)) {
		      ok = False;
		      cout << "for shape " << shape << resShape
			   << ", axisorder " << axisOrder << endl;
		      cout << " result is " << res << endl;
		    }
		  }
		}
	      }
	    }
	  }
	}
      }
    }
  }
  {
    cout << "arrayReorder 4D..." << endl;
    IPosition shape(4,3,4,5,6);
    Array<Int> arr(shape);
    indgen(arr);
    for (Int j0=0; j0<4; j0++) {
      for (Int j1=0; j1<4; j1++) {
	if (j1 != j0) {
	  for (Int j2=0; j2<4; j2++) {
	    if (j2 != j0  &&  j2 != j1) {
	      for (Int j3=0; j3<4; j3++) {
		if (j3 != j0  &&  j3 != j1  &&  j3 != j2) {
		  IPosition axisOrder(4, j0, j1, j2, j3);
		  Array<Int> res = reorderArray (arr, axisOrder);
		  const IPosition& resShape = res.shape();
		  IPosition posOld(4);
		  IPosition posNew(4);
		  for (Int i3=0; i3<resShape(3); i3++) {
		    posNew(3) = i3;
		    posOld(axisOrder(3)) = i3;
		    for (Int i2=0; i2<resShape(2); i2++) {
		      posNew(2) = i2;
		      posOld(axisOrder(2)) = i2;
		      for (Int i1=0; i1<resShape(1); i1++) {
			posNew(1) = i1;
			posOld(axisOrder(1)) = i1;
			for (Int i0=0; i0<resShape(0); i0++) {
			  posNew(0) = i0;
			  posOld(axisOrder(0)) = i0;
			  if (arr(posOld) != res(posNew)) {
			    ok = False;
			    cout << "for shape " << shape << resShape
				 << ", axisorder " << axisOrder << endl;
			    cout << " result is " << res << endl;
			  }
			}
		      }
		    }
		  }
		}
	      }
	    }
	  }
	}
      }
    }
  }
  if (doExcp) {
    try {
      reorderArray (Array<Int>(IPosition(2,3,4)), IPosition(2,1,1));
      ok = False;        // should not get here
      cout << "1st reorderArray exception not thrown" << endl;
    } catch (AipsError x) {
    } 
    try {
      reorderArray (Array<Int>(IPosition(2,3,4)), IPosition(2,1,2));
      ok = False;        // should not get here
      cout << "2nd reorderArray exception not thrown" << endl;
    } catch (AipsError x) {
    } 
  }
  return ok;
}

Bool testReverseArray()
{
  cout << "reverseArray..." << endl;
  IPosition shape(3, 2, 3, 4);
  Array<Int> arr(shape);
  Bool res = True;
  indgen(arr);
  // Test if no reversal is fine.
  IPosition axes(0);
  Array<Int> rev = reverseArray(arr, axes);
  res = res && allEQ(arr, rev);
  // Test if reversal of axis 0 is fine.
  rev = reverseArray(arr, 0);
  for (Int i=0; i<shape[2]; i++) {
    for (Int j=0; j<shape[1]; j++) {
      for (Int k=0; k<shape[0]; k++) {
        res = res  &&  (arr(IPosition(3, k, j, i))
                        == rev(IPosition(3, shape[0] - 1 - k, j, i)));
      }
    }
  }
  // Test if reversing axis 1 in both ways works fine.
  for (uInt x=0; x<2; x++) {
    rev = (x==0 ? reverseArray(arr, 1) : reverseArray(arr, IPosition(1, 1)));
    for (Int i=0; i<shape[2]; i++) {
      for (Int j=0; j<shape[1]; j++) {
        for (Int k=0; k<shape[0]; k++) {
          res = res  &&  (arr(IPosition(3, k, j, i))
                          == rev(IPosition(3, k, shape[1] - 1 - j, i)));
        }
      }
    }
  }
  // Test if reversing axis 2 in both ways works fine.
  for (uInt x=0; x<2; x++) {
    rev = (x==0 ? reverseArray(arr, 2) : reverseArray(arr, IPosition(1,2)));
    for (Int i=0; i<shape[2]; i++) {
      for (Int j=0; j<shape[1]; j++) {
        for (Int k=0; k<shape[0]; k++) {
          res = res  &&  (arr(IPosition(3, k, j, i))
                          == rev(IPosition(3, k, j, shape[2] - 1 - i)));
        }
      }
    }
  }
  // Test if reversing axes 0 and 1 together is the same as first 0 and than 1
  // and vice-versa.
  for (uInt x=0; x<3; x++) {
    rev = (x==0 ? reverseArray(reverseArray(arr, 0), 1)
           : (x==1 ? reverseArray(arr, IPosition(2, 0, 1))
              : reverseArray(arr, IPosition(2, 1, 0))));
    for (Int i=0; i<shape[2]; i++) {
      for (Int j=0; j<shape[1]; j++) {
        for (Int k=0; k<shape[0]; k++) {
          res = res  &&  (arr(IPosition(3, k, j, i))
                          == rev(IPosition(3, shape[0] - 1 - k,
                                           shape[1] - 1 - j, i)));
        }
      }
    }
  }
  // Test if reversing axes 0 and 2 works fine.
  for (uInt x=0; x<2; x++) {
    rev = (x==0 ? reverseArray(reverseArray(arr, 0), 2)
           : reverseArray(arr, IPosition(2, 0, 2)));
    for (Int i=0; i<shape[2]; i++) {
      for (Int j=0; j<shape[1]; j++) {
        for (Int k=0; k<shape[0]; k++) {
          res = res  &&  (arr(IPosition(3, k, j, i))
                          == rev(IPosition(3, shape[0] - 1 - k,
                                           j, shape[2] - 1 - i)));
        }
      }
    }
  }
  // Test if reversing axes 1 and 2 works fine.
  for (uInt x=0; x<2; x++) {
    rev = (x==0 ? reverseArray(reverseArray(arr, 1), 2)
           : reverseArray(arr, IPosition(2, 1, 2)));
    for (Int i=0; i<shape[2]; i++) {
      for (Int j=0; j<shape[1]; j++) {
        for (Int k=0; k<shape[0]; k++) {
          res = res  &&  (arr(IPosition(3, k, j, i))
                          == rev(IPosition(3, k, shape[1] - 1 - j,
                                           shape[2] - 1 - i)));
        }
      }
    }
  }
  // Test reversing of all axes.
  for (uInt x=0; x<2; x++) {
    rev = (x==0 ? reverseArray(reverseArray(reverseArray(arr, 0), 1), 2)
           : reverseArray(arr, IPosition(3, 0, 1, 2)));
    for (Int i=0; i<shape[2]; i++) {
      for (Int j=0; j<shape[1]; j++) {
        for (Int k=0; k<shape[0]; k++) {
          res = res  &&  (arr(IPosition(3, k, j, i))
                          == rev(IPosition(3, shape[0] - 1 - k,
                                           shape[1] - 1 - j,
                                           shape[2] - 1 - i)));
        }
      }
    }
  }
  // Reversing of all axes in different order should have the same result.
  for (uInt x=0; x<2; x++) {
    rev = (x==0 ? reverseArray(reverseArray(reverseArray(arr, 2), 0), 1)
           : reverseArray(arr, IPosition(3, 2, 0, 1)));
    for (Int i=0; i<shape[2]; i++) {
      for (Int j=0; j<shape[1]; j++) {
        for (Int k=0; k<shape[0]; k++) {
          res = res  &&  (arr(IPosition(3, k, j, i))
                          == rev(IPosition(3, shape[0] - 1 - k,
                                           shape[1] - 1 - j,
                                           shape[2] - 1 - i)));
        }
      }
    }
  }
  return res;
}

int main (int argc, const char*[])
{
  Bool ok = True;
  try {
    if (! testStringToVector ( (argc < 2))) {
      ok = False;
    }
    if (! testStringToVectorRegex ( (argc < 2))) {
      ok = False;
    }
    if (! testConcatenateArray( (argc < 2))) {
      ok = False;
    }
    if (! testReorderArray( (argc < 2))) {
      ok = False;
    }
    if (! testReverseArray()) {
    	ok = False;
    }
  } catch (AipsError x) {
    cout << "Caught an exception: " << x.getMesg() << endl;
    ok = False;
  } 
  if (!ok) {
    return 1;
  }
  cout << "OK" << endl;
  return 0;               // successfully executed
}
