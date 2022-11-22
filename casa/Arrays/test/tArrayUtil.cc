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

//#include "../ArrayIO.h"
#include "../ArrayMath.h"
#include "../ArrayError.h"
#include "../ArrayUtil.h"
#include "../Matrix.h"
#include "../Vector.h"

// <summary>
// Test of functions in ArrayUtil.h.
// </summary>

// This program tests the various functions in ArrayUtil.h.
// When an argument is given to the program, it will not invoke any
// function resulting in an exception. This mode can be used to do
// proper detection of memory leaks using tools like TestCenter.

#include <boost/test/unit_test.hpp>

using namespace casacore;

BOOST_AUTO_TEST_SUITE(array_util)

BOOST_AUTO_TEST_CASE( string_to_vector )
{
    Vector<std::string> vec1 = strToVector ("");
    BOOST_CHECK_EQUAL (vec1.nelements(), 0);

    Vector<std::string> vec2 = strToVector ("abc");
    BOOST_CHECK_EQUAL(vec2.nelements(), 1);
    BOOST_CHECK_EQUAL(vec2(0), "abc");

    Vector<std::string> vec3 = strToVector (",");
    BOOST_CHECK_EQUAL(vec3.nelements(), 2);
    BOOST_CHECK_EQUAL(vec3(0), "");
    BOOST_CHECK_EQUAL(vec3(1), "");

    Vector<std::string> vec4 = strToVector ("abc,defg,,h");
    BOOST_CHECK_EQUAL(vec4.nelements(), 4);
    BOOST_CHECK_EQUAL(vec4(0), "abc");
    BOOST_CHECK_EQUAL(vec4(1), "defg");
    BOOST_CHECK_EQUAL(vec4(2), "");
    BOOST_CHECK_EQUAL(vec4(3), "h");

    Vector<std::string> vec5 = strToVector (",abc,defg,");
    BOOST_CHECK_EQUAL(vec5.nelements(), 4);
    BOOST_CHECK_EQUAL(vec5(0), "");
    BOOST_CHECK_EQUAL(vec5(1), "abc");
    BOOST_CHECK_EQUAL(vec5(2), "defg");
    BOOST_CHECK_EQUAL(vec5(3), "");
}

BOOST_AUTO_TEST_CASE( string_to_vector_regex )
{
    // Test using multiple spaces and a single comma as delimiter.
    std::regex delim(" *, *");
 
    Vector<std::string> vec1 = strToVector ("", delim);
    BOOST_CHECK_EQUAL (vec1.nelements(), 0);

    Vector<std::string> vec2 = strToVector ("abc", delim);
    BOOST_CHECK_EQUAL(vec2.nelements(), 1);
    BOOST_CHECK_EQUAL(vec2(0), "abc" );

    Vector<std::string> vec3 = strToVector (",", delim);
    BOOST_CHECK_EQUAL(vec3.nelements(), 2 );
    BOOST_CHECK_EQUAL(vec3(0), ""); 
    BOOST_CHECK_EQUAL(vec3(1), "");

    Vector<std::string> vec4 = strToVector ("abc,defg,,h", delim);
    BOOST_CHECK_EQUAL(vec4.nelements(), 4);
    BOOST_CHECK_EQUAL(vec4(0), "abc");
    BOOST_CHECK_EQUAL(vec4(1), "defg");
    BOOST_CHECK_EQUAL(vec4(2), "");
    BOOST_CHECK_EQUAL(vec4(3), "h");

    Vector<std::string> vec5 = strToVector (",abc,defg,", delim);
    BOOST_CHECK_EQUAL(vec5.nelements(), 4);
    BOOST_CHECK_EQUAL(vec5(0), "");
    BOOST_CHECK_EQUAL(vec5(1), "abc");
    BOOST_CHECK_EQUAL(vec5(2), "defg");
    BOOST_CHECK_EQUAL(vec5(3), "");

    Vector<std::string> vec6 = strToVector ("  ,  ", delim);
    BOOST_CHECK_EQUAL(vec6.nelements(), 2);
    BOOST_CHECK_EQUAL(vec6(0), "");
    BOOST_CHECK_EQUAL(vec6(1), "");

    Vector<std::string> vec7 = strToVector ("abc  ,  defg  ,  ,  h", delim);
    BOOST_CHECK_EQUAL(vec7.nelements(), 4);
    BOOST_CHECK_EQUAL(vec7(0), "abc");
    BOOST_CHECK_EQUAL(vec7(1), "defg");
    BOOST_CHECK_EQUAL(vec7(2), "");
    BOOST_CHECK_EQUAL(vec7(3), "h");

    Vector<std::string> vec8 = strToVector (" abc  ", delim);
    BOOST_CHECK_EQUAL(vec8.nelements(), 1);
    BOOST_CHECK_EQUAL(vec8(0), " abc  ");
}

BOOST_AUTO_TEST_CASE( concatenate_array )
{
    bool doExcp = true;
    Matrix<int> matrix1 (3u,4u);
    Matrix<int> matrix2 (3u,5u);
    Matrix<int> matrix3 (4u,4u);
    Vector<int> vector1 (4);
    Vector<int> vector2 (6);
    indgen (matrix1);
    indgen (matrix2, int(matrix1.nelements()));
    indgen (matrix3);
    indgen (vector1);
    indgen (vector2, int(vector1.nelements()));
    Matrix<int> matrixConc(concatenateArray (matrix1, matrix2));
    BOOST_CHECK_EQUAL (matrixConc.shape(), IPosition(2,3,9));
    size_t i, j;
    int value = 0;
    for (j=0; j<9; j++) {
	for (i=0; i<3; i++) {
      BOOST_CHECK_EQUAL(matrixConc(i,j), value);
      ++value;
	}
    }

    Vector<int> vectorConc = concatenateArray (vector1, vector2);
    BOOST_CHECK_EQUAL(vectorConc.shape(), IPosition(1,10));
    value = 0;
    for (i=0; i<10; i++) {
	BOOST_CHECK_EQUAL(vectorConc(i), value);
  ++value;
    }
    
    if (doExcp) {
      BOOST_CHECK_THROW(concatenateArray (matrix1, matrix3), ArrayConformanceError);
      BOOST_CHECK_THROW(concatenateArray (matrix1, vector1), ArrayConformanceError);
    }
}

BOOST_AUTO_TEST_CASE( reorder_array_2d )
{
  IPosition shape(2,3,4);
  Array<int> arr(shape);
  indgen(arr);
  for (int j0=0; j0<2; j0++) {
    for (int j1=0; j1<2; j1++) {
if (j1 != j0) {
  IPosition axisOrder(2, j0, j1);
  Array<int> res = reorderArray (arr, axisOrder);
  const IPosition& resShape = res.shape();
  IPosition posOld(2);
  IPosition posNew(2);
  for (int i1=0; i1<resShape(1); i1++) {
    posNew(1) = i1;
    posOld(axisOrder(1)) = i1;
    for (int i0=0; i0<resShape(0); i0++) {
      posNew(0) = i0;
      posOld(axisOrder(0)) = i0;
      BOOST_CHECK_EQUAL(arr(posOld), res(posNew));
    }
  }
      }
    }
  }
}
  
BOOST_AUTO_TEST_CASE( reorder_array_3d )
{
    IPosition shape(3,3,4,5);
    Array<int> arr(shape);
    indgen(arr);
    for (int j0=0; j0<3; j0++) {
      for (int j1=0; j1<3; j1++) {
	if (j1 != j0) {
	  for (int j2=0; j2<3; j2++) {
	    if (j2 != j0  &&  j2 != j1) {
	      IPosition axisOrder(3, j0, j1, j2);
	      Array<int> res = reorderArray (arr, axisOrder);
	      const IPosition& resShape = res.shape();
	      IPosition posOld(3);
	      IPosition posNew(3);
	      for (int i2=0; i2<resShape(2); i2++) {
		posNew(2) = i2;
		posOld(axisOrder(2)) = i2;
		for (int i1=0; i1<resShape(1); i1++) {
		  posNew(1) = i1;
		  posOld(axisOrder(1)) = i1;
		  for (int i0=0; i0<resShape(0); i0++) {
		    posNew(0) = i0;
		    posOld(axisOrder(0)) = i0;
        BOOST_CHECK_EQUAL(arr(posOld), res(posNew));
		  }
		}
	      }
	    }
	  }
	}
    }
  }
}
  
BOOST_AUTO_TEST_CASE( reorder_array_4d )
{
    IPosition shape(4,3,4,5,6);
    Array<int> arr(shape);
    indgen(arr);
    for (int j0=0; j0<4; j0++) {
      for (int j1=0; j1<4; j1++) {
	if (j1 != j0) {
	  for (int j2=0; j2<4; j2++) {
	    if (j2 != j0  &&  j2 != j1) {
	      for (int j3=0; j3<4; j3++) {
		if (j3 != j0  &&  j3 != j1  &&  j3 != j2) {
		  IPosition axisOrder(4, j0, j1, j2, j3);
		  Array<int> res = reorderArray (arr, axisOrder);
		  const IPosition& resShape = res.shape();
		  IPosition posOld(4);
		  IPosition posNew(4);
		  for (int i3=0; i3<resShape(3); i3++) {
		    posNew(3) = i3;
		    posOld(axisOrder(3)) = i3;
		    for (int i2=0; i2<resShape(2); i2++) {
		      posNew(2) = i2;
		      posOld(axisOrder(2)) = i2;
		      for (int i1=0; i1<resShape(1); i1++) {
			posNew(1) = i1;
			posOld(axisOrder(1)) = i1;
			for (int i0=0; i0<resShape(0); i0++) {
			  posNew(0) = i0;
			  posOld(axisOrder(0)) = i0;
        BOOST_CHECK_EQUAL(arr(posOld), res(posNew));
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
  BOOST_CHECK_THROW(
    reorderArray (Array<int>(IPosition(2,3,4)), IPosition(2,1,1)),
                  std::runtime_error);
  BOOST_CHECK_THROW(
    reorderArray (Array<int>(IPosition(2,3,4)), IPosition(2,1,2)),
                  std::runtime_error);
}

BOOST_AUTO_TEST_CASE( reverse_array )
{
  IPosition shape(3, 2, 3, 4);
  Array<int> arr(shape);
  indgen(arr);
  // Test if no reversal is fine.
  IPosition axes(0);
  Array<int> rev = reverseArray(arr, axes);
  BOOST_CHECK( allEQ(arr, rev) );
  // Test if reversal of axis 0 is fine.
  rev = reverseArray(arr, 0);
  for (int i=0; i<shape[2]; i++) {
    for (int j=0; j<shape[1]; j++) {
      for (int k=0; k<shape[0]; k++) {
        BOOST_CHECK_EQUAL(arr(IPosition(3, k, j, i)),
                        rev(IPosition(3, shape[0] - 1 - k, j, i)));
      }
    }
  }
  // Test if reversing axis 1 in both ways works fine.
  for (size_t x=0; x<2; x++) {
    rev = (x==0 ? reverseArray(arr, 1) : reverseArray(arr, IPosition(1, 1)));
    for (int i=0; i<shape[2]; i++) {
      for (int j=0; j<shape[1]; j++) {
        for (int k=0; k<shape[0]; k++) {
          BOOST_CHECK_EQUAL(arr(IPosition(3, k, j, i)),
                          rev(IPosition(3, k, shape[1] - 1 - j, i)));
        }
      }
    }
  }
  // Test if reversing axis 2 in both ways works fine.
  for (size_t x=0; x<2; x++) {
    rev = (x==0 ? reverseArray(arr, 2) : reverseArray(arr, IPosition(1,2)));
    for (int i=0; i<shape[2]; i++) {
      for (int j=0; j<shape[1]; j++) {
        for (int k=0; k<shape[0]; k++) {
          BOOST_CHECK_EQUAL(arr(IPosition(3, k, j, i)),
                          rev(IPosition(3, k, j, shape[2] - 1 - i)));
        }
      }
    }
  }
  // Test if reversing axes 0 and 1 together is the same as first 0 and than 1
  // and vice-versa.
  for (size_t x=0; x<3; x++) {
    rev = (x==0 ? reverseArray(reverseArray(arr, 0), 1)
           : (x==1 ? reverseArray(arr, IPosition(2, 0, 1))
              : reverseArray(arr, IPosition(2, 1, 0))));
    for (int i=0; i<shape[2]; i++) {
      for (int j=0; j<shape[1]; j++) {
        for (int k=0; k<shape[0]; k++) {
          BOOST_CHECK_EQUAL(arr(IPosition(3, k, j, i)),
                          rev(IPosition(3, shape[0] - 1 - k,
                                           shape[1] - 1 - j, i)));
        }
      }
    }
  }
  // Test if reversing axes 0 and 2 works fine.
  for (size_t x=0; x<2; x++) {
    rev = (x==0 ? reverseArray(reverseArray(arr, 0), 2)
           : reverseArray(arr, IPosition(2, 0, 2)));
    for (int i=0; i<shape[2]; i++) {
      for (int j=0; j<shape[1]; j++) {
        for (int k=0; k<shape[0]; k++) {
          BOOST_CHECK_EQUAL(arr(IPosition(3, k, j, i)),
                          rev(IPosition(3, shape[0] - 1 - k,
                                           j, shape[2] - 1 - i)));
        }
      }
    }
  }
  // Test if reversing axes 1 and 2 works fine.
  for (size_t x=0; x<2; x++) {
    rev = (x==0 ? reverseArray(reverseArray(arr, 1), 2)
           : reverseArray(arr, IPosition(2, 1, 2)));
    for (int i=0; i<shape[2]; i++) {
      for (int j=0; j<shape[1]; j++) {
        for (int k=0; k<shape[0]; k++) {
          BOOST_CHECK_EQUAL(arr(IPosition(3, k, j, i)),
                          rev(IPosition(3, k, shape[1] - 1 - j,
                                           shape[2] - 1 - i)));
        }
      }
    }
  }
  // Test reversing of all axes.
  for (size_t x=0; x<2; x++) {
    rev = (x==0 ? reverseArray(reverseArray(reverseArray(arr, 0), 1), 2)
           : reverseArray(arr, IPosition(3, 0, 1, 2)));
    for (int i=0; i<shape[2]; i++) {
      for (int j=0; j<shape[1]; j++) {
        for (int k=0; k<shape[0]; k++) {
          BOOST_CHECK_EQUAL(arr(IPosition(3, k, j, i)),
                          rev(IPosition(3, shape[0] - 1 - k,
                                           shape[1] - 1 - j,
                                           shape[2] - 1 - i)));
        }
      }
    }
  }
  // Reversing of all axes in different order should have the same result.
  for (size_t x=0; x<2; x++) {
    rev = (x==0 ? reverseArray(reverseArray(reverseArray(arr, 2), 0), 1)
           : reverseArray(arr, IPosition(3, 2, 0, 1)));
    for (int i=0; i<shape[2]; i++) {
      for (int j=0; j<shape[1]; j++) {
        for (int k=0; k<shape[0]; k++) {
          BOOST_CHECK_EQUAL(arr(IPosition(3, k, j, i)),
                          rev(IPosition(3, shape[0] - 1 - k,
                                           shape[1] - 1 - j,
                                           shape[2] - 1 - i)));
        }
      }
    }
  }
}

BOOST_AUTO_TEST_SUITE_END()
