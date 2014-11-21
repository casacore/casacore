//# tFFTServer: This program tests the FFTServer and FourierTool classes
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$
//# Includes




#include <casacore/casa/aips.h>
#include <casacore/scimath/Mathematics/FFTServer.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/Cube.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>


#define AlwaysTrue(x, y)      \
do {                          \
    ++tests_done;             \
    AlwaysAssert(x, y);       \
} while (0)

unsigned tests_done = 0;
const Bool debug = False;//True;


template <class T>
void dum(const Array<T> &d)
{
  cout << d.nelements() << " elements, " << d.shape() << " " << d.shape().product() << endl;
}

template <class T>
void dump(const Vector<T> &d)
{
  dum(d);

  for (int i = 0; i < d.shape()(0); i++) {
    cout << i << ": " << d(i) << endl;
  }
  cout << endl;
}

template <class T>
void dump(const Matrix<T> &d)
{
  dum(d);
  for (int i = 0; i < d.shape()(0); i++) {
    cout << i << ":";
    for (int j = 0; j < d.shape()(1); j++) {
      cout << " " << d(i, j);
    }
   cout << endl;
  }
  cout << endl;
}

template <class T>
void dump(const Cube<T> &d)
{
  dum(d);
  for (int i = 0; i < d.shape()(0); i++) {
    cout << i << ":";
    for (int j = 0; j < d.shape()(1); j++) {
      cout << " ";
      for (int k = 0; k < d.shape()(2); k++) {
        cout << "," << d(i, j, k);
      }
    }
    cout << endl;
  }
  cout << endl;
}
template <class T>
void dump(const Array<T> &d)
{
  if (d.shape().nelements() == 1)
    dump((const Vector<T>)d);
  else if (d.shape().nelements() == 2)
    dump((const Matrix<T>)d);
  else if (d.shape().nelements() == 3)
      dump((const Cube<T>)d);
  else dum(d);
}


template <class T>
Array<T> shift(const Array<T> &input, 
               const IPosition &input_shape, 
               const IPosition &expected_shape)
{
    // Shift the array by Ni/2 in every dimension (circular boundary),
    // except along the dimension that is half size in real<->complex pairs

    Array<T> shifted_input(input.shape());
    if (debug ) cout << "shapes: " << input_shape << expected_shape << endl;

    for (unsigned i = 0; i < input.nelements(); i++) {
        IPosition here;
        IPosition shifted;
        if (input.ndim() == 1) {
            int N0 = input.shape()(0);
            int s[1];
            here    = IPosition(1, i);
            if (input.shape()(0) < std::max(input_shape(0), expected_shape(0))) {
                s[0] = i % N0;
            }
            else {
                s[0] = (i + (N0+1)/2) % N0;
            }
            shifted = IPosition(1, s[0]);
        }
        else if (input.ndim() == 2) {
            int N[2], n[2];
            N[0] = input.shape()(0);
            N[1] = input.shape()(1);
            n[0] = i % N[0];
            n[1] = i / N[0];
            int s[2];
            for (int j = 0; j < 2; j++) {
                if (input.shape()(j) < std::max(input_shape(j), expected_shape(j))) {
                    s[j] = n[j];
                }
                else {
                    s[j] = (n[j] - N[j]/2 + N[j]) % N[j];
                }
            }

            here    = IPosition(2, n[0], n[1]);
            shifted = IPosition(2, s[0], s[1]);
        }
        else if (input.ndim() == 3) {
            int N[3], n[3], s[3];
            N[0] = input.shape()(0);
            N[1] = input.shape()(1);
            N[2] = input.shape()(2);
            n[0] = i % N[0];
            n[1] = (i / N[0]) % N[1];
            n[2] = i / (N[1]*N[0]);
            for (int j = 0; j < 3; j++) {
                if (input.shape()(j) < std::max(input_shape(j), expected_shape(j))) {
                    s[j] = n[j];
                }
                else {
                    s[j] = (n[j] - N[j]/2 + N[j]) % N[j];
                }
            }
            here    = IPosition(3, n[0], n[1], n[2]);
            shifted = IPosition(3, s[0], s[1], s[2]);
        }
        else {
            AlwaysTrue(input.ndim() == 4, AipsError);
            // can't handle more than this

            int N[4], n[4], s[4];
            N[0] = input.shape()(0);
            N[1] = input.shape()(1);
            N[2] = input.shape()(2);
            N[3] = input.shape()(3);
            n[0] = i % N[0];
            n[1] = (i / N[0]) % N[1];
            n[2] = (i / (N[1]*N[0])) % N[2];
            n[3] = i / (N[2]*N[1]*N[0]);
            for (int j = 0; j < 4; j++) {
                if (input.shape()(j) < std::max(input_shape(j), expected_shape(j))) {
                    s[j] = n[j];
                }
                else {
                    s[j] = (n[j] - N[j]/2 + N[j]) % N[j];
                }
            }
            here    = IPosition(4, n[0], n[1], n[2], n[3]);
            shifted = IPosition(4, s[0], s[1], s[2], s[3]);
        }
        shifted_input(here) = input(shifted);
    }

    return shifted_input;
}


template <class T, class S>
class R2C1Deven1 {
  public:
    static Array<T> input() {
        Vector<T> input(8, 0.0f);
        input(0) = 1.0f;
        return input;
    }
    
    static Array<S> expectedResult() {
        return Vector<S>(5, Complex(1,0));
    }
};

template <class T, class S>
class R2C1Deven2 {
public:
  static Array<T> input() {
    Vector<T> input(8, 0.0f);
    input(0) = 1.0f;
    input(2) = -1.0f;
    input(4) = 1.0f;
    input(6) = -1.0f;
    return input;
  }

  static Array<S> expectedResult() {
    Vector<S> expectedResult(5, Complex(0,0));
    expectedResult(2) = Complex(4,0);
    return expectedResult;
  }
};

template <class T, class S>
class R2C1Deven3 {
public:
  static Array<T> input() {
    Vector<T> input(8, 0.0f);
    input(1) = 1.0f;
    input(3) = -1.0f;
    input(5) = 1.0f;
    input(7) = -1.0f;
    return input;
  }

  static Array<S> expectedResult() {
    Vector<S> expectedResult(5, Complex(0,0));
    expectedResult(2) = Complex(0, -4);
    return expectedResult;
  }
};

template <class T, class S>
class R2C1Deven4 {
public:
  static Array<T> input() {
    Vector<T> input(8, 0.0f);
    input(1) = 1.0f;
    input(3) = 1.0f;
    input(5) = 1.0f;
    input(7) = 1.0f;
    return input;
  }

  static Array<S> expectedResult() {
    Vector<S> expectedResult(5, Complex(0,0));
    expectedResult(0) = Complex(4, 0);
    expectedResult(4) = Complex(-4, 0);
    return expectedResult;
  }
};



template <class T, class S>
class R2C1Dodd1 {
public:
  static Array<T> input() {
    Vector<T> input(9, 0.0f);
    input(0) = 1.0f;
    return input;
  }

  static Array<S> expectedResult() {
    return Vector<S>(5, Complex(1,0));
  }
};


template <class T, class S>
class R2C1Dodd2 {
public:
  static Array<T> input() {
    return Vector<T>(9, 1.0f);
  }

  static Array<S> expectedResult() {
    Vector<S> expectedResult(5, Complex(0,0));
    expectedResult(0) = Complex(9,0);
    return expectedResult;
  }
};

template <class T, class S>
class R2C1Dodd3 {
public:
  static Array<T> input() {
    Vector<T> input(9, 1.0f);
    input(1) = 0.0f;
    input(3) = 0.0f;
    input(5) = 0.0f;
    input(7) = 0.0f;
    return input;
  }

  static Array<S> expectedResult() {
    Vector<S> expectedResult(5, Complex(0,0));
    expectedResult(0) = Complex(5,0);
    expectedResult(1) = Complex(0.5, 0.181985117133101);
    expectedResult(2) = Complex(0.5, 0.419549815588640);
    expectedResult(3) = Complex(0.5, 0.866025403784437);
    expectedResult(4) = Complex(0.5, 2.83564090980885);
    return expectedResult;
  }
};
template <class T, class S>
class R2C2Deveneven1 {
public:
  static Array<T> input() {
    Matrix<T> input(4, 6);
    input = 0.0f;
    input(0, 0) = 1.0f;
    return input;
  }

  static Array<S> expectedResult() {
    return Matrix<S>(3, 6, Complex(1, 0));
  }
};
template <class T, class S>
class R2C2Deveneven2 {
public:
  static Array<T> input() {
    return Matrix<T>(4, 6, 1.0f);
  }

  static Array<S> expectedResult() {
    Matrix<S> expectedResult(3, 6, Complex(0,0));
    expectedResult(0, 0) = Complex(24, 0);
    return expectedResult;
  }
};
template <class T, class S>
class R2C2Deveneven3 {
public:
  static Array<T> input() {
    Matrix<T> input(4, 6, 0.0f);
    input(1,1) = 1.0f;
    input(1,3) = 1.0f;
    input(1,5) = 1.0f;
    
    return input;
  }

  static Array<S> expectedResult() {
    Matrix<S> expectedResult(3, 6, Complex(0,0));
    expectedResult(0,0) = expectedResult(2,3) = Complex(3,0);
    expectedResult(0,3) = expectedResult(2,0) = Complex(-3,0);
    expectedResult(1,3) = Complex(0,3);
    expectedResult(1,0) = Complex(0,-3);
    return expectedResult;
  }
};



template <class T, class S>
class R2C2Devenodd1 {
public:
  static Array<T> input() {
    Matrix<T> input(4, 5, 0.0f);
    input(0, 0) = 1.0f;
    return input;
  }

  static Array<S> expectedResult() {
      return Matrix<S>(3, 5, Complex(1, 0));
  }
};

template <class T, class S>
class R2C2Devenodd2 {
public:
  static Array<T> input() {
      return Matrix<T>(4, 5, 1.0f);
  }

  static Array<S> expectedResult() {
      Matrix<S> expectedResult(3, 5, Complex(0,0));
      expectedResult(0,0) = Complex(20,0);
      return expectedResult;
  }
};


template <class T, class S>
class R2C2Doddeven1 {
public:
  static Array<T> input() {
    Matrix<T> input(3, 6, 0.0f);
    input(0, 0) = 1.0f;
    return input;
  }

  static Array<S> expectedResult() {
      return Matrix<S>(2, 6, Complex(1, 0));
  }
};

template <class T, class S>
class R2C2Doddeven2 {
public:
  static Array<T> input() {
      return Matrix<T>(3, 6, 1.0f);
  }

  static Array<S> expectedResult() {
      Matrix<S> expectedResult(2, 6, Complex(0,0));
      expectedResult(0,0) = Complex(18,0);
      return expectedResult;
  }
};

template <class T, class S>
class R2C2Doddodd1 {
public:
  static Array<T> input() {
    Matrix<T> input(3, 5, 0.0f);
    input(0, 0) = 1.0f;
    return input;
  }

  static Array<S> expectedResult() {
      return Matrix<S>(2, 5, Complex(1, 0));
  }
};

template <class T, class S>
class R2C2Doddodd2 {
public:
  static Array<T> input() {
      return Matrix<T>(3, 5, 1.0f);
  }

  static Array<S> expectedResult() {
      Matrix<S> expectedResult(2, 5, Complex(0,0));
      expectedResult(0,0) = Complex(15,0);
      return expectedResult;
  }
};


template <class T, class S>
class R2C3Deveneveneven1 {
public:
  static Array<T> input() {
    Cube<T> input(4, 6, 8, 0.0f);
    input(0, 0, 0) = 1.0f;
    return input;
  }

  static Array<S> expectedResult() {
      return Cube<S>(3, 6, 8, Complex(1, 0));
  }
};
template <class T, class S>
class R2C3Deveneveneven2 {
public:
  static Array<T> input() {
      return Cube<T>(4, 6, 8, 1.0f);
  }

  static Array<S> expectedResult() {
      Cube<S> expectedResult(3, 6, 8, Complex(0,0));
      expectedResult(0, 0, 0) = Complex(4*6*8,0);
      return expectedResult;
  }
};

template <class T, class S>
class R2C3Doddoddodd1 {
public:
  static Array<T> input() {
    Cube<T> input(3,5,7, 0.0f);
    input(0, 0, 0) = 1.0f;
    return input;
  }

  static Array<S> expectedResult() {
      return Cube<S>(2,5,7, Complex(1, 0));
  }
};

template <class T, class S>
class R2C3Doddoddodd2 {
public:
  static Array<T> input() {
      return Cube<T>(3,5,7, 1.0f);
  }

  static Array<S> expectedResult() {
      Cube<S> expectedResult(2,5,7, Complex(0,0));
      expectedResult(0, 0, 0) = Complex(3*5*7,0);
      return expectedResult;
  }

    static double tolerance() {
        return 100*FLT_EPSILON;
    }
};

template <class T, class S>
class R2C4Doddoddoddeven1 {
public:
  static Array<T> input() {
      Array<T> input(IPosition(4,3,5,7,4), 0.0f);
      input(IPosition(4,0)) = 1.0f;
      return input;
  }

    static Array<S> expectedResult() {
        return Array<S>(IPosition(4,2,5,7,4), Complex(1, 0));
  }
};


template <class T, class S>
class R2C4Doddoddoddeven2 {
public:
    static Array<T> input() {
        return Array<T> (IPosition(4,3,5,7,4), 1.0f);
    }
    
    static Array<S> expectedResult() {
        Array<S> expectedResult(IPosition(4,2,5,7,4), Complex(0, 0));
        expectedResult(IPosition(4,0)) = Complex(3*5*7*4,0);
        return expectedResult;
    }
    
    static double tolerance() {
        return 500*FLT_EPSILON;
    }
};

template <class S, class T>
class C2R1Deven1 {
public:
  static Array<S> input() {
    Vector<S> input(5, Complex(0, 0));
    input(0) = Complex(8, 0);
    return input;
  }

  static Array<T> expectedResult() {
      return Vector<T>(8, 1.0f);
  }
};

template <class S, class T>
class C2R1Deven2 {
public:
  static Array<S> input() {
      Vector<S> input(5, Complex(0, 0));
      input(0) = Complex(16.0f, 0.0f);
      input(2) = Complex(8.0f, 0.0f);
      return input;
  }

  static Array<T> expectedResult() {
      Vector<T> expectedResult(8, 2.0f);
      expectedResult(0) = 4.0f;
      expectedResult(2) = 0.0f;
      expectedResult(4) = 4.0f;
      expectedResult(6) = 0.0f;
      return expectedResult;
  }
};

template <class S, class T>
class C2R1Deven3 {
public:
  static Array<S> input() {
      Vector<S> input(5, Complex(0, 0));
      input(0) = Complex(0.0f, 0.0f);
      input(2) = Complex(0.0f, 4.0f);
      return input;
  }

  static Array<T> expectedResult() {
      Vector<T> expectedResult(8, 0.0f);
      expectedResult(1) = -1.0f;
      expectedResult(3) = 1.0f;
      expectedResult(5) = -1.0f;
      expectedResult(7) = 1.0f;
      return expectedResult;
  }
};
template <class S, class T>
class C2R1Deven4 {
public:
  static Array<S> input() {
      Vector<S> input(5, Complex(1, 0));
      return input;
  }

  static Array<T> expectedResult() {
      Vector<T> expectedResult(8, 0.0f);
      expectedResult(0) = 1.0f;
      return expectedResult;
  }
};
template <class S, class T>
class C2R1Deven5 {
public:
  static Array<S> input() {
      Vector<S> input(5, Complex(1, 0));
      input(1) = Complex(0,0);
      input(3) = Complex(0,0);
      return input;
  }

  static Array<T> expectedResult() {
      Vector<T> expectedResult(8, 0.0f);
      expectedResult(0) = 0.5f;
      expectedResult(4) = 0.5f;
      return expectedResult;
  }
};


template <class S, class T>
class C2R1Dodd1 {
public:
  static Array<S> input() {
    Vector<S> input(5, Complex(0, 0));
    input(0) = Complex(9, 0);
    return input;
  }

  static Array<T> expectedResult() {
      return Vector<T>(9, 1.0f);
  }
};

template <class S, class T>
class C2R1Dodd2 {
public:
  static Array<S> input() {
      Vector<S> input(5, Complex(1, 0));
      return input;
  }

  static Array<T> expectedResult() {
      Vector<T> expectedResult(9, 0.0f);
      expectedResult(0) = 1.0f;
      return expectedResult;
  }
};

template <class S, class T>
class C2R2Deveneven1 {
public:
  static Array<S> input() {
      Matrix<S> input(3, 6, Complex(0, 0));
      input(0,0) = Complex(4*6,0);
      return input;
  }

  static Array<T> expectedResult() {
      Matrix<T> expectedResult(4, 6, 1.0f);
      return expectedResult;
  }
};
template <class S, class T>
class C2R2Deveneven2 {
public:
  static Array<S> input() {
      Matrix<S> input(3, 6, Complex(1, 0));
      return input;
  }

  static Array<T> expectedResult() {
      Matrix<T> expectedResult(4, 6, 0.0f);
      expectedResult(0,0) = 1.0f;
      return expectedResult;
  }
};
template <class S, class T>
class C2R2Deveneven3 {
public:
  static Array<S> input() {
      Matrix<S> input(3, 6, Complex(0, 0));
      input(0,0) = Complex(24,0);
      input(2,0) = Complex(-24,0);
      input(0,1) = Complex(-24,0);
      input(0,2) = Complex( 24,0);
      input(0,3) = Complex(-24,0);
      input(0,4) = Complex( 24,0);
      input(0,5) = Complex(-24,0);
      return input;
  }

  static Array<T> expectedResult() {
      Matrix<T> expectedResult(4, 6, 0.0f);
      expectedResult(0,0) = expectedResult(0,1) = expectedResult(0,2) 
        = expectedResult(0,4) = expectedResult(0,5) = -1.0f;
      expectedResult(0,3) = 5.0f;
      expectedResult(1,0) = expectedResult(1,1) = expectedResult(1,2) 
        = expectedResult(1,4) = expectedResult(1,5) = 1.0f;
      expectedResult(1,3) = 7.0f;
      expectedResult(2,0) = expectedResult(2,1) = expectedResult(2,2) 
        = expectedResult(2,4) = expectedResult(2,5) = -1.0f;
      expectedResult(2,3) = 5.0f;
      expectedResult(3,0) = expectedResult(3,1) = expectedResult(3,2) 
        = expectedResult(3,4) = expectedResult(3,5) = 1.0f;
      expectedResult(3,3) = 7.0f;
      return expectedResult;
  }
};

template <class S, class T>
class C2R2Doddodd1 {
public:
  static Array<S> input() {
      Matrix<S> input(2, 5, Complex(0, 0));
      input(0, 0) = Complex(3*5, 0);
      return input;
  }

  static Array<T> expectedResult() {
      Matrix<T> expectedResult(3, 5, 1.0f);
      return expectedResult;
  }
};

template <class S, class T>
class C2R2Doddodd2 {
public:
  static Array<S> input() {
      Matrix<S> input(2, 5, Complex(1, 0));
      return input;
  }

  static Array<T> expectedResult() {
      Matrix<T> expectedResult(3, 5, 0.0f);
      expectedResult(0,0) = 1.0f;
      return expectedResult;
  }
};

template <class S, class T>
class C2R2Doddodd3 {
public:
  static Array<S> input() {
      Matrix<S> input(2, 5, Complex(0, 0));
      input(1,0) = Complex(0,45);
      input(1,1) = Complex(0,45);
      input(1,2) = Complex(0,45);
      input(1,3) = Complex(0,45);
      input(1,4) = Complex(0,45);
      return input;
  }

  static Array<T> expectedResult() {
      Matrix<T> expectedResult(3, 5, 0.0f);
      expectedResult(1,0) = -25.9808f;
      expectedResult(2,0) = 25.9808f;
      return expectedResult;
  }
};


template <class S, class T>
class C2R2Devenodd1 {
  public:
    static Array<S> input() {
        Matrix<S> input(3, 5, Complex(0, 0));
        input(0,0) = Complex(4*5, 0);
        return input;
    }
    
    static Array<T> expectedResult() {
        Matrix<T> expectedResult(4, 5, 1.0f);
        return expectedResult;
    }
};

template <class S, class T>
class C2R2Devenodd2 {
  public:
    static Array<S> input() {
        Matrix<S> input(3, 5, Complex(1, 0));
        return input;
    }
    
    static Array<T> expectedResult() {
        Matrix<T> expectedResult(4, 5, 0.0f);
        expectedResult(0,0) = 1.0f;
        return expectedResult;
    }
};


template <class S, class T>
class C2R2Doddeven1 {
  public:
    static Array<S> input() {
        Matrix<S> input(2, 6, Complex(0, 0));
        input(0,0) = Complex(3*6, 0);
        return input;
    }
    
    static Array<T> expectedResult() {
        Matrix<T> expectedResult(3, 6, 1.0f);
        return expectedResult;
    }
};

template <class S, class T>
class C2R2Doddeven2 {
  public:
    static Array<S> input() {
        Matrix<S> input(2, 6, Complex(1, 0));
        return input;
    }
    
    static Array<T> expectedResult() {
        Matrix<T> expectedResult(3, 6, 0.0f);
        expectedResult(0,0) = 1.0f;
        return expectedResult;
    }
};

template <class S, class T>
class C2R3Deveneveneven1 {
  public:
    static Array<S> input() {
        Cube<S> input(3, 6, 2, Complex(0, 0));
        input(0,0,0) = Complex(4*6*2,0);
        return input;
    }
    
    static Array<T> expectedResult() {
        Cube<T> expectedResult(4, 6, 2, 1.0f);
        return expectedResult;
    }
};
template <class S, class T>
class C2R3Deveneveneven2 {
  public:
    static Array<S> input() {
        Cube<S> input(3, 6, 2, Complex(1, 0));
        return input;
    }
    
    static Array<T> expectedResult() {
        Cube<T> expectedResult(4, 6, 2, 0.0f);
        expectedResult(0,0,0) = 1.0f;
        return expectedResult;
    }
};

template <class S, class T>
class C2R3Doddoddodd1 {
  public:
    static Array<S> input() {
        Cube<S> input(2, 5, 7, Complex(0, 0));
        input(0,0,0) = Complex(3*5*7,0);
        return input;
    }
    
    static Array<T> expectedResult() {
        Cube<T> expectedResult(3, 5, 7, 1.0f);
        return expectedResult;
    }
};
template <class S, class T>
class C2R3Doddoddodd2 {
  public:
    static Array<S> input() {
        Cube<S> input(2, 5, 7, Complex(1, 0));
        return input;
    }
    
    static Array<T> expectedResult() {
        Cube<T> expectedResult(3, 5, 7, 0.0f);
        expectedResult(0,0,0) = 1.0f;
        return expectedResult;
    }
};


template <class S, class T>
class C2R4Doddoddoddeven1 {
  public:
    static Array<S> input() {
#if PERFORMANCE_TEST
      // Useful to test how the use of threads effects CPU usage
      Array<S> input(IPosition(4,20,500,70,20), Complex(0, 0));
#else
      Array<S> input(IPosition(4,2,5,7,2), Complex(0, 0));
#endif
      input(IPosition(4,0)) = Complex(3*5*7*2,0);
      return input;
    }
    
    static Array<T> expectedResult() {
#if PERFORMANCE_TEST
      Array<T> expectedResult(IPosition(4, 38, 500, 70, 20));
#else
      Array<T> expectedResult(IPosition(4,3,5,7,2), 1.0f);
#endif
        expectedResult = 1.0f;
        return expectedResult;
    }
};

template <class S, class T>
class C2R4Doddoddoddeven2 {
  public:
    static Array<S> input() {
        Array<S> input(IPosition(4,2,5,7,2), Complex(1, 0));
        return input;
    }
    
    static Array<T> expectedResult() {
        Array<T> expectedResult(IPosition(4,3,5,7,2), 0.0f);
        expectedResult = 0.0f;
        expectedResult(IPosition(4,0)) = 1.0f;
        return expectedResult;
    }
};


template <class S>
class C2C1Deven1 {
  public:
    static Array<S> input() {
        Vector<S> input(8, Complex(0, 0));
        input(0) = Complex(1.0f, 0.0f);
        return input;
    }

    static Array<S> expectedResult() {
        Vector<S> expectedResult(8, Complex(1, 0));
        return expectedResult;
    }
};

template <class S>
class C2C1Deven2 {
  public:
    static Array<S> input() {
        Vector<S> input(8, Complex(1, 0));
        return input;
    }

    static Array<S> expectedResult() {
        Vector<S> expectedResult(8, Complex(0, 0));
        expectedResult(0) = Complex(8,0);
        return expectedResult;
    }
};

template <class S>
class C2C1Deven3 {
  public:
    static Array<S> input() {
        Vector<S> input(8, Complex(-1, 0));
        input(0) = Complex(1, 0);
        input(2) = Complex(1, 0);
        input(4) = Complex(1, 0);
        input(6) = Complex(1, 0);
        return input;
    }

    static Array<S> expectedResult() {
        Vector<S> expectedResult(8, Complex(0, 0));
        expectedResult(4) = Complex(8,0);
        return expectedResult;
    }
};

template <class S>
class C2C1Deven4 {
  public:
    static Array<S> input() {
        Vector<S> input(8, Complex(0, 0));
        input(1) = Complex(1, 0);
        input(3) = Complex(-1,0);
        input(5) = Complex(1, 0);
        input(7) = Complex(-1,0);
        return input;
    }

    static Array<S> expectedResult() {
        Vector<S> expectedResult(8, Complex(0, 0));
        expectedResult(2) = Complex(0,-4);
        expectedResult(6) = Complex(0,4);
        return expectedResult;
    }
};



template <class S>
class C2C1Dodd1 {
  public:
    static Array<S> input() {
        Vector<S> input(7, Complex(0, 0));
        input(0) = Complex(1.0f, 0.0f);
        return input;
    }

    static Array<S> expectedResult() {
        Vector<S> expectedResult(7, Complex(1, 0));
        return expectedResult;
    }
};

template <class S>
class C2C1Dodd2 {
  public:
    static Array<S> input() {
        Vector<S> input(7, Complex(1, 0));
        return input;
    }

    static Array<S> expectedResult() {
        Vector<S> expectedResult(7, Complex(0, 0));
        expectedResult(0) = Complex(7,0);
        return expectedResult;
    }
};

template <class S>
class C2C2Deveneven1 {
  public:
    static Array<S> input() {
        Matrix<S> input(4, 6, Complex(0, 0));
        input(0,0) = Complex(1,0);
        return input;
    }

    static Array<S> expectedResult() {
        Matrix<S> expectedResult(4, 6, Complex(1, 0));
        return expectedResult;
    }
};
template <class S>
class C2C2Deveneven2 {
  public:
    static Array<S> input() {
        Matrix<S> input(4, 6, Complex(1, 0));
        return input;
    }

    static Array<S> expectedResult() {
        Matrix<S> expectedResult(4, 6, Complex(0, 0));
        expectedResult(0,0) = Complex(24,0);
        return expectedResult;
    }
};
template <class S>
class C2C2Deveneven3 {
  public:
    static Array<S> input() {
        Matrix<S> input(4, 6, Complex(0, 0));
        input(1,1) = Complex(1,1);
        input(1,3) = Complex(1,1);
        input(1,5) = Complex(1,1);
        return input;
    }

    static Array<S> expectedResult() {
        Matrix<S> expectedResult(4, 6, Complex(0, 0));
        expectedResult(0,0) = expectedResult(2,3) = Complex(3,3);
        expectedResult(0,3) = expectedResult(2,0) = Complex(-3,-3);
        expectedResult(1,3) = expectedResult(3,0) = Complex(-3,3);
        expectedResult(1,0) = expectedResult(3,3) = Complex(3,-3);
        return expectedResult;
    }
};


template <class S>
class C2C2Doddodd1 {
  public:
    static Array<S> input() {
        Matrix<S> input(3, 5, Complex(0, 0));
        input(0,0) = Complex(1,0);
        return input;
    }

    static Array<S> expectedResult() {
        Matrix<S> expectedResult(3, 5, Complex(1, 0));
        return expectedResult;
    }
};
template <class S>
class C2C2Doddodd2 {
  public:
    static Array<S> input() {
        Matrix<S> input(3, 5, Complex(1, 0));
        return input;
    }

    static Array<S> expectedResult() {
        Matrix<S> expectedResult(3, 5, Complex(0, 0));
        expectedResult(0,0) = Complex(15,0);
        return expectedResult;
    }
};

template <class S>
class C2C2Devenodd1 {
  public:
    static Array<S> input() {
        Matrix<S> input(4, 5, Complex(0, 0));
        input(0,0) = Complex(1,0);
        return input;
    }

    static Array<S> expectedResult() {
        Matrix<S> expectedResult(4, 5, Complex(1, 0));
        return expectedResult;
    }
};
template <class S>
class C2C2Devenodd2 {
  public:
    static Array<S> input() {
        Matrix<S> input(4, 5, Complex(1, 0));
        return input;
    }

    static Array<S> expectedResult() {
        Matrix<S> expectedResult(4, 5, Complex(0, 0));
        expectedResult(0,0) = Complex(20,0);
        return expectedResult;
    }
};

template <class S>
class C2C2Doddeven1 {
  public:
    static Array<S> input() {
        Matrix<S> input(3, 6, Complex(0, 0));
        input(0,0) = Complex(1,0);
        return input;
    }

    static Array<S> expectedResult() {
        Matrix<S> expectedResult(3, 6, Complex(1, 0));
        return expectedResult;
    }
};
template <class S>
class C2C2Doddeven2 {
  public:
    static Array<S> input() {
        Matrix<S> input(3, 6, Complex(1, 0));
        return input;
    }

    static Array<S> expectedResult() {
        Matrix<S> expectedResult(3, 6, Complex(0, 0));
        expectedResult(0,0) = Complex(18,0);
        return expectedResult;
    }
};

template <class S>
class C2C3Doddeveneven1 {
  public:
    static Array<S> input() {
        Cube<S> input(4, 6, 8, Complex(0, 0));
        input(0,0,0) = Complex(1,0);
        return input;
    }

    static Array<S> expectedResult() {
        Cube<S> expectedResult(4, 6, 8, Complex(1, 0));
        return expectedResult;
    }
};
template <class S>
class C2C3Doddeveneven2 {
  public:
    static Array<S> input() {
        Cube<S> input(4, 6, 8, Complex(1, 0));
        return input;
    }

    static Array<S> expectedResult() {
        Cube<S> expectedResult(4, 6, 8, Complex(0, 0));
        expectedResult(0,0,0) = Complex(4*6*8,0);
        return expectedResult;
    }
};

template <class S>
class C2C3Doddoddodd1 {
  public:
    static Array<S> input() {
        Cube<S> input(3, 5, 7, Complex(0, 0));
        input(0,0,0) = Complex(1,0);
        return input;
    }

    static Array<S> expectedResult() {
        Cube<S> expectedResult(3, 5, 7, Complex(1, 0));
        return expectedResult;
    }
};
template <class S>
class C2C3Doddoddodd2 {
  public:
    static Array<S> input() {
        Cube<S> input(3, 5, 7, Complex(1, 0));
        return input;
    }

    static Array<S> expectedResult() {
        Cube<S> expectedResult(3, 5, 7, Complex(0, 0));
        expectedResult(0,0,0) = Complex(3*5*7,0);
        return expectedResult;
    }
};

template <class S>
class C2C4Doddoddoddeven1 {
  public:
    static Array<S> input() {
        Array<S> input(IPosition(4,3,5,7,4));
        input = Complex(0,0);
        input(IPosition(4,0)) = Complex(1,0);
        return input;
    }

    static Array<S> expectedResult() {
        Array<S> result, expectedResult(IPosition(4,3,5,7,4));
        expectedResult = Complex(1,0);
        return expectedResult;
    }
};
template <class S>
class C2C4Doddoddoddeven2 {
  public:
    static Array<S> input() {
        Array<S> input(IPosition(4,3,5,7,4));
        input = Complex(1,0);
        return input;
    }

    static Array<S> expectedResult() {
        Array<S> result, expectedResult(IPosition(4,3,5,7,4));
        expectedResult = Complex(0,0);
        expectedResult(IPosition(4,0)) = Complex(3*5*7*4,0);
        return expectedResult;
    }
};




template <class T, class S, class TServer, class SServer>
class Test
{
public:
  Test(FFTServer<TServer, SServer> &server, 
       Bool shifted_mode,              // use fft() instead of fft0 ?
       double epsilon,
       const Array<T> input,
       const Array<S> expectedResult)
        {
      Array<S> result(expectedResult.shape());
      
      int iterations = 1;
#if PERFORMANCE_TEST
      iterations = 10;
#endif
      for (int i = 0; i < iterations; i++) {
	if (shifted_mode) {
          server.fft(result, input);
	}
	else {
          server.fft0(result, input);
	}
      }
      
      if (debug) {
          cout << "Input:" << endl;
          dump(input);
          cout << "Result:" << endl;
          dump(result);
          cout << "Expected:" << endl;
          dump(expectedResult);
      }

      AlwaysTrue(result.shape().isEqual(expectedResult.shape()),
                   AipsError);
      AlwaysTrue(allNearAbs(result, expectedResult, 2*epsilon),
                   AipsError);

      if (input.shape().nelements() == 1){
          if (debug) { cout << "Resize..." << endl;}
          result.resize();
          
          if (shifted_mode)
              server.fft(result, input);
          else
              server.fft0(result, input);
          
          if (debug) {
              cout << "Input:" << endl;
              dump(input);
              cout << "Result:" << endl;
              dump(result);
              cout << "Expected:" << endl;
              dump(expectedResult);
          }
          AlwaysTrue(result.shape().isEqual(expectedResult.shape()),
                       AipsError);
          AlwaysTrue(allNearAbs(result, expectedResult, epsilon),
                       AipsError);

          int out_size = expectedResult.nelements();
          if (out_size % 2 == 0) {
              server.resize(IPosition(1, out_size), FFTEnums::REALTOCOMPLEX);

              if (shifted_mode)
                  server.fft(result, input);
              else
                  server.fft0(result, input);
              AlwaysTrue(result.shape().isEqual(expectedResult.shape()),
                           AipsError);
              AlwaysTrue(allNearAbs(result, expectedResult, epsilon),
                           AipsError);
              
              server.resize(IPosition(1, out_size-1), FFTEnums::COMPLEX);
              
              if (shifted_mode)
                  server.fft(result, input);
              else
                  server.fft0(result, input);
              AlwaysTrue(result.shape().isEqual(expectedResult.shape()),
                           AipsError);
              AlwaysTrue(allNearAbs(result, expectedResult, epsilon),
                           AipsError);
          }
      }
  }
};

template <class T, class S>
class TestFFTShift
{
public:
  TestFFTShift()  // test the complex->complex fft shift function
  {

    FFTServer<T, S> server; 
    
    int iterations = 1;

      Array<S> a(IPosition(2,2,10));
      a(IPosition(2,0,0)) = Complex(0.,0.);
      a(IPosition(2,0,1)) = Complex(1.,0.);
      a(IPosition(2,0,2)) = Complex(2.,0.);
      a(IPosition(2,0,3)) = Complex(3.,0.);
      a(IPosition(2,0,4)) = Complex(4.,0.);
      a(IPosition(2,0,5)) = Complex(5.,0.);
      a(IPosition(2,0,6)) = Complex(6.,0.);
      a(IPosition(2,0,7)) = Complex(7.,0.);
      a(IPosition(2,0,8)) = Complex(8.,0.);
      a(IPosition(2,0,9)) = Complex(9.,0.);
      a(IPosition(2,1,0)) = Complex(0.,0.);
      a(IPosition(2,1,1)) = Complex(10.,0.);
      a(IPosition(2,1,2)) = Complex(20.,0.);
      a(IPosition(2,1,3)) = Complex(30.,0.);
      a(IPosition(2,1,4)) = Complex(40.,0.);
      a(IPosition(2,1,5)) = Complex(50.,0.);
      a(IPosition(2,1,6)) = Complex(60.,0.);
      a(IPosition(2,1,7)) = Complex(70.,0.);
      a(IPosition(2,1,8)) = Complex(80.,0.);
      a(IPosition(2,1,9)) = Complex(90.,0.);

      Array<Bool> aflags(IPosition(2,2,10));
      aflags(IPosition(2,0,0)) = True;
      aflags(IPosition(2,0,1)) = True;
      aflags(IPosition(2,0,2)) = True;
      aflags(IPosition(2,0,3)) = True;
      aflags(IPosition(2,0,4)) = False;
      aflags(IPosition(2,0,5)) = True;
      aflags(IPosition(2,0,6)) = True;
      aflags(IPosition(2,0,7)) = False;
      aflags(IPosition(2,0,8)) = True;
      aflags(IPosition(2,0,9)) = True;
      aflags(IPosition(2,1,0)) = True;
      aflags(IPosition(2,1,1)) = True;
      aflags(IPosition(2,1,2)) = True;
      aflags(IPosition(2,1,3)) = True;
      aflags(IPosition(2,1,4)) = False;
      aflags(IPosition(2,1,5)) = True;
      aflags(IPosition(2,1,6)) = True;
      aflags(IPosition(2,1,7)) = False;
      aflags(IPosition(2,1,8)) = True;
      aflags(IPosition(2,1,9)) = True;

      Array<Bool> aflags2(IPosition(2,2,10));
      aflags2(IPosition(2,0,0)) = True;
      aflags2(IPosition(2,0,1)) = True;
      aflags2(IPosition(2,0,2)) = True;
      aflags2(IPosition(2,0,3)) = True;
      aflags2(IPosition(2,0,4)) = True;
      aflags2(IPosition(2,0,5)) = True;
      aflags2(IPosition(2,0,6)) = False;
      aflags2(IPosition(2,0,7)) = False;
      aflags2(IPosition(2,0,8)) = True;
      aflags2(IPosition(2,0,9)) = True;
      aflags2(IPosition(2,1,0)) = True;
      aflags2(IPosition(2,1,1)) = True;
      aflags2(IPosition(2,1,2)) = True;
      aflags2(IPosition(2,1,3)) = True;
      aflags2(IPosition(2,1,4)) = True;
      aflags2(IPosition(2,1,5)) = True;
      aflags2(IPosition(2,1,6)) = False;
      aflags2(IPosition(2,1,7)) = False;
      aflags2(IPosition(2,1,8)) = True;
      aflags2(IPosition(2,1,9)) = True;

      Array<S> b(IPosition(2,10,2));
      b(IPosition(2,0,0)) = Complex(0.,0.);
      b(IPosition(2,1,0)) = Complex(1.,0.);
      b(IPosition(2,2,0)) = Complex(2.,0.);
      b(IPosition(2,3,0)) = Complex(3.,0.);
      b(IPosition(2,4,0)) = Complex(4.,0.);
      b(IPosition(2,5,0)) = Complex(5.,0.);
      b(IPosition(2,6,0)) = Complex(6.,0.);
      b(IPosition(2,7,0)) = Complex(7.,0.);
      b(IPosition(2,8,0)) = Complex(8.,0.);
      b(IPosition(2,9,0)) = Complex(9.,0.);
      b(IPosition(2,0,1)) = Complex(0.,0.);
      b(IPosition(2,1,1)) = Complex(10.,0.);
      b(IPosition(2,2,1)) = Complex(20.,0.);
      b(IPosition(2,3,1)) = Complex(30.,0.);
      b(IPosition(2,4,1)) = Complex(40.,0.);
      b(IPosition(2,5,1)) = Complex(50.,0.);
      b(IPosition(2,6,1)) = Complex(60.,0.);
      b(IPosition(2,7,1)) = Complex(70.,0.);
      b(IPosition(2,8,1)) = Complex(80.,0.);
      b(IPosition(2,9,1)) = Complex(90.,0.);

      Array<S> expect(IPosition(2,2,10));
      expect(IPosition(2,0,0)) = Complex(9.,0.);	
      expect(IPosition(2,0,1)) = Complex(0.,0.); 	
      expect(IPosition(2,0,2)) = Complex(1.,0.); 	
      expect(IPosition(2,0,3)) = Complex(2.,0.); 	
      expect(IPosition(2,0,4)) = Complex(3.,0.); 	
      expect(IPosition(2,0,5)) = Complex(4.,0.); 	
      expect(IPosition(2,0,6)) = Complex(5.,0.); 	
      expect(IPosition(2,0,7)) = Complex(6.,0.); 	
      expect(IPosition(2,0,8)) = Complex(7.,0.); 	
      expect(IPosition(2,0,9)) = Complex(8.,0.); 	
      expect(IPosition(2,1,0)) = Complex(90.,0.); 	
      expect(IPosition(2,1,1)) = Complex(0.,0.); 	
      expect(IPosition(2,1,2)) = Complex(10.,0.);	
      expect(IPosition(2,1,3)) = Complex(20.,0.);	
      expect(IPosition(2,1,4)) = Complex(30.,0.);	
      expect(IPosition(2,1,5)) = Complex(40.,0.);	
      expect(IPosition(2,1,6)) = Complex(50.,0.);	
      expect(IPosition(2,1,7)) = Complex(60.,0.);	
      expect(IPosition(2,1,8)) = Complex(70.,0.);	
      expect(IPosition(2,1,9)) = Complex(80.,0.); 
 
      Array<S> expectb(IPosition(2,2,10));
      expectb(IPosition(2,0,0)) = Complex(9.,0.);	
      expectb(IPosition(2,0,1)) = Complex(0.,0.); 	
      expectb(IPosition(2,0,2)) = Complex(1.,0.); 	
      expectb(IPosition(2,0,3)) = Complex(2.,0.); 	
      expectb(IPosition(2,0,4)) = Complex(3.,0.); 	
      expectb(IPosition(2,0,5)) = Complex(4.,0.); 	
      expectb(IPosition(2,0,6)) = Complex(5.,0.); 	
      expectb(IPosition(2,0,7)) = Complex(6.,0.); 	
      expectb(IPosition(2,0,8)) = Complex(7.,0.); 	
      expectb(IPosition(2,0,9)) = Complex(8.,0.); 	
      expectb(IPosition(2,1,0)) = Complex(90.,0.); 	
      expectb(IPosition(2,1,1)) = Complex(0.,0.); 	
      expectb(IPosition(2,1,2)) = Complex(10.,0.);	
      expectb(IPosition(2,1,3)) = Complex(20.,0.);	
      expectb(IPosition(2,1,4)) = Complex(30.,0.);	
      expectb(IPosition(2,1,5)) = Complex(40.,0.);	
      expectb(IPosition(2,1,6)) = Complex(50.,0.);	
      expectb(IPosition(2,1,7)) = Complex(60.,0.);	
      expectb(IPosition(2,1,8)) = Complex(70.,0.);	
      expectb(IPosition(2,1,9)) = Complex(80.,0.);  

      Array<S> expectc(IPosition(2,2,10));
      expectc(IPosition(2,0,0)) = Complex(1.,0.);
      expectc(IPosition(2,0,1)) = Complex(2.,0.);
      expectc(IPosition(2,0,2)) = Complex(3.,0.);
      expectc(IPosition(2,0,3)) = Complex(4.,0.);
      expectc(IPosition(2,0,4)) = Complex(5.,0.);
      expectc(IPosition(2,0,5)) = Complex(6.,0.);
      expectc(IPosition(2,0,6)) = Complex(7.,0.);
      expectc(IPosition(2,0,7)) = Complex(8.,0.);
      expectc(IPosition(2,0,8)) = Complex(9.,0.);
      expectc(IPosition(2,0,9)) = Complex(0.,0.);
      expectc(IPosition(2,1,0)) = Complex(10.,0.);
      expectc(IPosition(2,1,1)) = Complex(20.,0.);
      expectc(IPosition(2,1,2)) = Complex(30.,0.);
      expectc(IPosition(2,1,3)) = Complex(40.,0.);
      expectc(IPosition(2,1,4)) = Complex(50.,0.);
      expectc(IPosition(2,1,5)) = Complex(60.,0.);
      expectc(IPosition(2,1,6)) = Complex(70.,0.);
      expectc(IPosition(2,1,7)) = Complex(80.,0.);
      expectc(IPosition(2,1,8)) = Complex(90.,0.);
      expectc(IPosition(2,1,9)) = Complex(0.,0.);

      Array<S> expectd(IPosition(2,2,10));
      expectd(IPosition(2,0,0)) = Complex(1.,0.);
      expectd(IPosition(2,0,1)) = Complex(2.,0.);
      expectd(IPosition(2,0,2)) = Complex(3.,0.);
      expectd(IPosition(2,0,3)) = Complex(4.,0.);
      expectd(IPosition(2,0,4)) = Complex(5.,0.);
      expectd(IPosition(2,0,5)) = Complex(6.,0.);
      expectd(IPosition(2,0,6)) = Complex(7.,0.);
      expectd(IPosition(2,0,7)) = Complex(8.,0.);
      expectd(IPosition(2,0,8)) = Complex(9.,0.);
      expectd(IPosition(2,0,9)) = Complex(0.,0.);
      expectd(IPosition(2,1,0)) = Complex(10.,0.);
      expectd(IPosition(2,1,1)) = Complex(20.,0.);
      expectd(IPosition(2,1,2)) = Complex(30.,0.);
      expectd(IPosition(2,1,3)) = Complex(40.,0.);
      expectd(IPosition(2,1,4)) = Complex(50.,0.);
      expectd(IPosition(2,1,5)) = Complex(60.,0.);
      expectd(IPosition(2,1,6)) = Complex(70.,0.);
      expectd(IPosition(2,1,7)) = Complex(80.,0.);
      expectd(IPosition(2,1,8)) = Complex(90.,0.);
      expectd(IPosition(2,1,9)) = Complex(0.,0.);

      Array<Bool> expflags(IPosition(2,2,10));
      expflags(IPosition(2,0,0)) = False;
      expflags(IPosition(2,0,1)) = True;
      expflags(IPosition(2,0,2)) = True;
      expflags(IPosition(2,0,3)) = True;
      expflags(IPosition(2,0,4)) = True;
      expflags(IPosition(2,0,5)) = False;
      expflags(IPosition(2,0,6)) = True;
      expflags(IPosition(2,0,7)) = True;
      expflags(IPosition(2,0,8)) = False;
      expflags(IPosition(2,0,9)) = True;
      expflags(IPosition(2,1,0)) = False;
      expflags(IPosition(2,1,1)) = True;
      expflags(IPosition(2,1,2)) = True;
      expflags(IPosition(2,1,3)) = True;
      expflags(IPosition(2,1,4)) = True;
      expflags(IPosition(2,1,5)) = False;
      expflags(IPosition(2,1,6)) = True;
      expflags(IPosition(2,1,7)) = True;
      expflags(IPosition(2,1,8)) = False;
      expflags(IPosition(2,1,9)) = True;

      Array<Bool> expflagsb(IPosition(2,2,10));
      expflagsb(IPosition(2,0,0)) = False;
      expflagsb(IPosition(2,0,1)) = True;
      expflagsb(IPosition(2,0,2)) = True;
      expflagsb(IPosition(2,0,3)) = True;
      expflagsb(IPosition(2,0,4)) = True;
      expflagsb(IPosition(2,0,5)) = True;
      expflagsb(IPosition(2,0,6)) = True;
      expflagsb(IPosition(2,0,7)) = True;
      expflagsb(IPosition(2,0,8)) = True;
      expflagsb(IPosition(2,0,9)) = True;
      expflagsb(IPosition(2,1,0)) = False;
      expflagsb(IPosition(2,1,1)) = True;
      expflagsb(IPosition(2,1,2)) = True;
      expflagsb(IPosition(2,1,3)) = True;
      expflagsb(IPosition(2,1,4)) = True;
      expflagsb(IPosition(2,1,5)) = True;
      expflagsb(IPosition(2,1,6)) = True;
      expflagsb(IPosition(2,1,7)) = True;
      expflagsb(IPosition(2,1,8)) = True;
      expflagsb(IPosition(2,1,9)) = True;

      Array<Bool> expflagsc(IPosition(2,2,10));
      expflagsc(IPosition(2,0,0)) = True;
      expflagsc(IPosition(2,0,1)) = True;
      expflagsc(IPosition(2,0,2)) = True;
      expflagsc(IPosition(2,0,3)) = False;
      expflagsc(IPosition(2,0,4)) = True;
      expflagsc(IPosition(2,0,5)) = True;
      expflagsc(IPosition(2,0,6)) = False;
      expflagsc(IPosition(2,0,7)) = True;
      expflagsc(IPosition(2,0,8)) = True;
      expflagsc(IPosition(2,0,9)) = False;
      expflagsc(IPosition(2,1,0)) = True;
      expflagsc(IPosition(2,1,1)) = True;
      expflagsc(IPosition(2,1,2)) = True;
      expflagsc(IPosition(2,1,3)) = False;
      expflagsc(IPosition(2,1,4)) = True;
      expflagsc(IPosition(2,1,5)) = True;
      expflagsc(IPosition(2,1,6)) = False;
      expflagsc(IPosition(2,1,7)) = True;
      expflagsc(IPosition(2,1,8)) = True;
      expflagsc(IPosition(2,1,9)) = False;

      Array<Bool> expflagsd(IPosition(2,2,10));
      expflagsd(IPosition(2,0,0)) = True;
      expflagsd(IPosition(2,0,1)) = True;
      expflagsd(IPosition(2,0,2)) = True;
      expflagsd(IPosition(2,0,3)) = True;
      expflagsd(IPosition(2,0,4)) = True;
      expflagsd(IPosition(2,0,5)) = True;
      expflagsd(IPosition(2,0,6)) = True;
      expflagsd(IPosition(2,0,7)) = True;
      expflagsd(IPosition(2,0,8)) = True;
      expflagsd(IPosition(2,0,9)) = False;
      expflagsd(IPosition(2,1,0)) = True;
      expflagsd(IPosition(2,1,1)) = True;
      expflagsd(IPosition(2,1,2)) = True;
      expflagsd(IPosition(2,1,3)) = True;
      expflagsd(IPosition(2,1,4)) = True;
      expflagsd(IPosition(2,1,5)) = True;
      expflagsd(IPosition(2,1,6)) = True;
      expflagsd(IPosition(2,1,7)) = True;
      expflagsd(IPosition(2,1,8)) = True;
      expflagsd(IPosition(2,1,9)) = False;

      Array<Bool> expflagse(IPosition(2,2,10));
      expflagse(IPosition(2,0,0)) = True;
      expflagse(IPosition(2,0,1)) = True;
      expflagse(IPosition(2,0,2)) = False;
      expflagse(IPosition(2,0,3)) = False;
      expflagse(IPosition(2,0,4)) = True;
      expflagse(IPosition(2,0,5)) = False;
      expflagse(IPosition(2,0,6)) = False;
      expflagse(IPosition(2,0,7)) = True;
      expflagse(IPosition(2,0,8)) = False;
      expflagse(IPosition(2,0,9)) = False;
      expflagse(IPosition(2,1,0)) = True;
      expflagse(IPosition(2,1,1)) = True;
      expflagse(IPosition(2,1,2)) = False;
      expflagse(IPosition(2,1,3)) = False;
      expflagse(IPosition(2,1,4)) = True;
      expflagse(IPosition(2,1,5)) = False;
      expflagse(IPosition(2,1,6)) = False;
      expflagse(IPosition(2,1,7)) = True;
      expflagse(IPosition(2,1,8)) = False;
      expflagse(IPosition(2,1,9)) = False;

      Array<Bool> expflagse2(IPosition(2,2,10));
      expflagse2(IPosition(2,0,0)) = True;
      expflagse2(IPosition(2,0,1)) = True;
      expflagse2(IPosition(2,0,2)) = True;
      expflagse2(IPosition(2,0,3)) = True;
      expflagse2(IPosition(2,0,4)) = True;
      expflagse2(IPosition(2,0,5)) = False;
      expflagse2(IPosition(2,0,6)) = False;
      expflagse2(IPosition(2,0,7)) = False;
      expflagse2(IPosition(2,0,8)) = True;
      expflagse2(IPosition(2,0,9)) = False;
      expflagse2(IPosition(2,1,0)) = True;
      expflagse2(IPosition(2,1,1)) = True;
      expflagse2(IPosition(2,1,2)) = True;
      expflagse2(IPosition(2,1,3)) = True;
      expflagse2(IPosition(2,1,4)) = True;
      expflagse2(IPosition(2,1,5)) = False;
      expflagse2(IPosition(2,1,6)) = False;
      expflagse2(IPosition(2,1,7)) = False;
      expflagse2(IPosition(2,1,8)) = True;
      expflagse2(IPosition(2,1,9)) = False;

      Array<T> ra(IPosition(2,2,10));
      ra(IPosition(2,0,0)) = 0.;
      ra(IPosition(2,0,1)) = 1.;
      ra(IPosition(2,0,2)) = 2.;
      ra(IPosition(2,0,3)) = 3.;
      ra(IPosition(2,0,4)) = 4.;
      ra(IPosition(2,0,5)) = 5.;
      ra(IPosition(2,0,6)) = 6.;
      ra(IPosition(2,0,7)) = 7.;
      ra(IPosition(2,0,8)) = 8.;
      ra(IPosition(2,0,9)) = 9.;
      ra(IPosition(2,1,0)) = 0.;
      ra(IPosition(2,1,1)) = 10.;
      ra(IPosition(2,1,2)) = 20.;
      ra(IPosition(2,1,3)) = 30.;
      ra(IPosition(2,1,4)) = 40.;
      ra(IPosition(2,1,5)) = 50.;
      ra(IPosition(2,1,6)) = 60.;
      ra(IPosition(2,1,7)) = 70.;
      ra(IPosition(2,1,8)) = 80.;
      ra(IPosition(2,1,9)) = 90.;

      Array<T> rexpect(IPosition(2,2,10));
      rexpect(IPosition(2,0,0)) = 9.;	
      rexpect(IPosition(2,0,1)) = 0.; 	
      rexpect(IPosition(2,0,2)) = 1.; 	
      rexpect(IPosition(2,0,3)) = 2.; 	
      rexpect(IPosition(2,0,4)) = 3.; 	
      rexpect(IPosition(2,0,5)) = 4.; 	
      rexpect(IPosition(2,0,6)) = 5.; 	
      rexpect(IPosition(2,0,7)) = 6.; 	
      rexpect(IPosition(2,0,8)) = 7.; 	
      rexpect(IPosition(2,0,9)) = 8.; 	
      rexpect(IPosition(2,1,0)) = 90.; 	
      rexpect(IPosition(2,1,1)) = 0.; 	
      rexpect(IPosition(2,1,2)) = 10.;	
      rexpect(IPosition(2,1,3)) = 20.;	
      rexpect(IPosition(2,1,4)) = 30.;	
      rexpect(IPosition(2,1,5)) = 40.;	
      rexpect(IPosition(2,1,6)) = 50.;	
      rexpect(IPosition(2,1,7)) = 60.;	
      rexpect(IPosition(2,1,8)) = 70.;	
      rexpect(IPosition(2,1,9)) = 80.; 
 
      Array<T> rexpectb(IPosition(2,2,10));
      rexpectb(IPosition(2,0,0)) = 9.;	
      rexpectb(IPosition(2,0,1)) = 0.; 	
      rexpectb(IPosition(2,0,2)) = 1.; 	
      rexpectb(IPosition(2,0,3)) = 2.; 	
      rexpectb(IPosition(2,0,4)) = 3.; 	
      rexpectb(IPosition(2,0,5)) = 4.; 	
      rexpectb(IPosition(2,0,6)) = 5.; 	
      rexpectb(IPosition(2,0,7)) = 6.; 	
      rexpectb(IPosition(2,0,8)) = 7.; 	
      rexpectb(IPosition(2,0,9)) = 8.; 	
      rexpectb(IPosition(2,1,0)) = 90.; 	
      rexpectb(IPosition(2,1,1)) = 0.; 	
      rexpectb(IPosition(2,1,2)) = 10.;	
      rexpectb(IPosition(2,1,3)) = 20.;	
      rexpectb(IPosition(2,1,4)) = 30.;	
      rexpectb(IPosition(2,1,5)) = 40.;	
      rexpectb(IPosition(2,1,6)) = 50.;	
      rexpectb(IPosition(2,1,7)) = 60.;	
      rexpectb(IPosition(2,1,8)) = 70.;	
      rexpectb(IPosition(2,1,9)) = 80.;  


#if PERFORMANCE_TEST
    iterations = 10;
#endif
    for (int it = 0; it < iterations; it++) {

      cout << "--- zero shift ----------------------------------------------------" << endl;
      
      uInt whichAxis = 1;
      Double relshift = 0.;
      Array<S> inVal;
      inVal.assign(a);

      server.fftshift(inVal, whichAxis, relshift, False);

      {
	for(uInt i=0; i<2; i++){
	  for(uInt j=0; j<10; j++){
	    DComplex diff = inVal(IPosition(2,i,j))-a(IPosition(2,i,j));
	    cout << i << " " << j << " " << inVal(IPosition(2,i,j)) << " " << a(IPosition(2,i,j)) << endl;
	    AlwaysAssert( (diff.real()==0. && diff.imag()==0.), AipsError);
	  }
	}
      }

      cout << "--- right-shift by 1 channel in second axis -----------------------" << endl;
      
      whichAxis = 1;
      relshift = 1./10.;
      inVal.assign(a);

      server.fftshift(inVal, whichAxis, relshift, False);

      {
	for(uInt i=0; i<2; i++){
	  for(uInt j=0; j<10; j++){
	    DComplex diff = inVal(IPosition(2,i,j))-expect(IPosition(2,i,j));
	    cout << i << " " << j << " " << inVal(IPosition(2,i,j)) << " " << expect(IPosition(2,i,j)) << endl;
	    AlwaysAssert((abs(diff.real())<2E-5) && (abs(diff.imag())<2E-5), AipsError);
	  }
	}
      }

      cout << "--- two consecutive leftshifts by 0.5 channels in second axis -----------------------" << endl;

      relshift = -0.5/10.;
      inVal.assign(a);

      server.fftshift(inVal, whichAxis, relshift, False);
      server.fftshift(inVal, whichAxis, relshift, False); // two consecutive shifts should shift by 1 channel

      {
	for(uInt i=0; i<2; i++){
	  for(uInt j=0; j<10; j++){
	    DComplex diff = inVal(IPosition(2,i,j))-expectc(IPosition(2,i,j));
	    cout << i << " " << j << " " << inVal(IPosition(2,i,j)) << " " << expectc(IPosition(2,i,j)) << endl;
	    AlwaysAssert((abs(diff.real())<2E-5) && (abs(diff.imag())<2E-5), AipsError);
	  }
	}
      }

      cout << "--- rightshift by 1 channel in first axis -----------------------" << endl;

      whichAxis = 0;
      relshift = 1./10.;

      server.fftshift(b, whichAxis, relshift, False);

      {
	for(uInt i=0; i<2; i++){
	  for(uInt j=0; j<10; j++){
	    DComplex diff = b(IPosition(2,j,i))-expect(IPosition(2,i,j));
	    cout << i << " " << j << " " << b(IPosition(2,j,i)) << " " << expect(IPosition(2,i,j)) << endl;
	    AlwaysAssert((abs(diff.real())<2E-5) && (abs(diff.imag())<2E-5), AipsError);
	  }
	}
      }


      cout << "--- right-shift by 1 channel in first axis with flags all not set -----------------------" << endl;

      whichAxis = 1;
      relshift = 1./10.;
      Array<S> outVal;
      Array<Bool> outFlag;
      Array<Bool> inFlags;
      inFlags.assign(aflags);
      inFlags = True;

      server.fftshift(outVal, outFlag, a, inFlags, whichAxis, relshift, True, False);

      {
	for(uInt i=0; i<2; i++){
	  for(uInt j=0; j<10; j++){
	    DComplex diff = outVal(IPosition(2,i,j))-expect(IPosition(2,i,j));
	    cout << i << " " << j << " " << outVal(IPosition(2,i,j)) << " " << expect(IPosition(2,i,j)) << endl;
	    cout << "flag " << i << " " << j << " " << outFlag(IPosition(2,i,j)) << " " << expflagsb(IPosition(2,i,j)) << endl;
	    AlwaysAssert((abs(diff.real())<2E-5) && (abs(diff.imag())<2E-5), AipsError);
	    AlwaysAssert(outFlag(IPosition(2,i,j)) == expflagsb(IPosition(2,i,j)), AipsError);
	  }
	}
      }

      cout << "--- right-shift by 1 channel in first axis with flags partially set -----------------------" << endl;

      whichAxis = 1;
      relshift = 1./10.;

      server.fftshift(outVal, outFlag, a, aflags, whichAxis, relshift, True, False);

      {
	for(uInt i=0; i<2; i++){
	  for(uInt j=0; j<10; j++){
	    DComplex diff = outVal(IPosition(2,i,j))-expectb(IPosition(2,i,j));
	    cout << i << " " << j << " " << outVal(IPosition(2,i,j)) << " " << expectb(IPosition(2,i,j)) << endl;
	    cout << "flag " << i << " " << j << " " << outFlag(IPosition(2,i,j)) << " " << expflags(IPosition(2,i,j)) << endl;
	    AlwaysAssert((abs(diff.real())<2E-5) && (abs(diff.imag())<2E-5), AipsError);
	    AlwaysAssert(outFlag(IPosition(2,i,j)) == expflags(IPosition(2,i,j)), AipsError);
	  }
	}
      }

      cout << "--- zero shift with flags partially set -----------------------" << endl;

      whichAxis = 1;
      relshift = 0.;

      server.fftshift(outVal, outFlag, a, aflags, whichAxis, relshift, True, False);

      {
	for(uInt i=0; i<2; i++){
	  for(uInt j=0; j<10; j++){
	    DComplex diff = outVal(IPosition(2,i,j))-a(IPosition(2,i,j));
	    cout << i << " " << j << " " << outVal(IPosition(2,i,j)) << " " << a(IPosition(2,i,j)) << endl;
	    cout << "flag " << i << " " << j << " " << outFlag(IPosition(2,i,j)) << " " << aflags(IPosition(2,i,j)) << endl;
	    AlwaysAssert((diff.real()==0. && diff.imag()==0.), AipsError);
	    AlwaysAssert(outFlag(IPosition(2,i,j)) == aflags(IPosition(2,i,j)), AipsError);
	  }
	}
      }

      cout << "--- two consecutive left-shifts by 0.5 channels in first axis with flags not set -----------" << endl;

      whichAxis = 1;
      relshift = -0.5/10.;
      inFlags = True;

      server.fftshift(outVal, outFlag, a, inFlags, whichAxis, relshift, True, False);

      inVal.assign(outVal);
      server.fftshift(outVal, outFlag, inVal, inFlags, whichAxis, relshift, True, False);

      {
	for(uInt i=0; i<2; i++){
	  for(uInt j=0; j<10; j++){
	    DComplex diff = outVal(IPosition(2,i,j))-expectc(IPosition(2,i,j));
	    cout << i << " " << j << " " << outVal(IPosition(2,i,j)) << " " << expectc(IPosition(2,i,j)) << endl;
	    cout << "flag " << i << " " << j << " " << outFlag(IPosition(2,i,j)) << " " << expflagsd(IPosition(2,i,j)) << endl;
	    AlwaysAssert((abs(diff.real())<2E-5) && (abs(diff.imag())<2E-5), AipsError);
	    AlwaysAssert(outFlag(IPosition(2,i,j)) == expflagsd(IPosition(2,i,j)), AipsError);
	  }
	}
      }

      cout << "--- left-shift by 1 channel in first axis with flags partially set -----------" << endl;

      whichAxis = 1;
      relshift = -1./10.;

      server.fftshift(outVal, outFlag, a, aflags, whichAxis, relshift, True, False);

      {
	for(uInt i=0; i<2; i++){
	  for(uInt j=0; j<10; j++){
	    DComplex diff = outVal(IPosition(2,i,j))-expectd(IPosition(2,i,j));
	    cout << i << " " << j << " " << outVal(IPosition(2,i,j)) << " " << expectd(IPosition(2,i,j)) << endl;
	    cout << "flag " << i << " " << j << " " << outFlag(IPosition(2,i,j)) << " " << expflagsc(IPosition(2,i,j)) << endl;
	    AlwaysAssert((abs(diff.real())<2E-5) && (abs(diff.imag())<2E-5), AipsError);
	    AlwaysAssert(outFlag(IPosition(2,i,j)) == expflagsc(IPosition(2,i,j)), AipsError);
	  }
	}
      }

      cout << "--- left-shift by 1.5 channels in first axis with flags partially set -----------" << endl;

      whichAxis = 1;
      relshift = -1.5/10.;

      server.fftshift(outVal, outFlag, a, aflags, whichAxis, relshift, True, False);

      {
	for(uInt i=0; i<2; i++){
	  for(uInt j=0; j<10; j++){
	    cout << i << " " << j << " " << outVal(IPosition(2,i,j)) << endl;
	    cout << "flag " << i << " " << j << " " << outFlag(IPosition(2,i,j)) << " " << expflagse(IPosition(2,i,j)) << endl;
	    AlwaysAssert(outFlag(IPosition(2,i,j)) == expflagse(IPosition(2,i,j)), AipsError);
	  }
	}
      }

      cout << "--- left-shift by 0.25 channels in first axis with flags partially set -----------" << endl;

      whichAxis = 1;
      relshift = -0.25/10.;

      server.fftshift(outVal, outFlag, a, aflags2, whichAxis, relshift, True, False);

      {
	for(uInt i=0; i<2; i++){
	  for(uInt j=0; j<10; j++){
	    cout << i << " " << j << " " << outVal(IPosition(2,i,j)) << endl;
	    cout << "flag " << i << " " << j << " " << outFlag(IPosition(2,i,j)) << " " << expflagse2(IPosition(2,i,j)) << endl;
	    AlwaysAssert(outFlag(IPosition(2,i,j)) == expflagse2(IPosition(2,i,j)), AipsError);
	  }
	}
      }
      cout << "--- left-shift by 0.5 channels in first axis with flags partially set -----------" << endl;

      whichAxis = 1;
      relshift = -0.5/10.;

      server.fftshift(outVal, outFlag, a, aflags2, whichAxis, relshift, True, False);

      {
	for(uInt i=0; i<2; i++){
	  for(uInt j=0; j<10; j++){
	    cout << i << " " << j << " " << outVal(IPosition(2,i,j)) << endl;
	    cout << "flag " << i << " " << j << " " << outFlag(IPosition(2,i,j)) << " " << expflagse2(IPosition(2,i,j)) << endl;
	    AlwaysAssert(outFlag(IPosition(2,i,j)) == expflagse2(IPosition(2,i,j)), AipsError);
	  }
	}
      }
      cout << "--- left-shift by 0.55 channels in first axis with flags partially set -----------" << endl;

      whichAxis = 1;
      relshift = -0.55/10.;

      server.fftshift(outVal, outFlag, a, aflags2, whichAxis, relshift, True, False);

      {
	for(uInt i=0; i<2; i++){
	  for(uInt j=0; j<10; j++){
	    cout << i << " " << j << " " << outVal(IPosition(2,i,j)) << endl;
	    cout << "flag " << i << " " << j << " " << outFlag(IPosition(2,i,j)) << " " << expflagse2(IPosition(2,i,j)) << endl;
	    AlwaysAssert(outFlag(IPosition(2,i,j)) == expflagse2(IPosition(2,i,j)), AipsError);
	  }
	}
      }
      cout << "--- left-shift by 0.75 channels in first axis with flags partially set -----------" << endl;

      whichAxis = 1;
      relshift = -0.75/10.;

      server.fftshift(outVal, outFlag, a, aflags2, whichAxis, relshift, True, False);

      {
	for(uInt i=0; i<2; i++){
	  for(uInt j=0; j<10; j++){
	    cout << i << " " << j << " " << outVal(IPosition(2,i,j)) << endl;
	    cout << "flag " << i << " " << j << " " << outFlag(IPosition(2,i,j)) << " " << expflagse2(IPosition(2,i,j)) << endl;
	    AlwaysAssert(outFlag(IPosition(2,i,j)) == expflagse2(IPosition(2,i,j)), AipsError);
	  }
	}
      }

      cout << "--- right-shift by 1 channel in first axis with flags all not set, real data ----------------" << endl;

      whichAxis = 1;
      relshift = 1./10.;
      Array<T> routVal;
      inFlags.assign(aflags);
      inFlags = True;

      server.fftshift(routVal, outFlag, ra, inFlags, whichAxis, relshift, True);

      {
	for(uInt i=0; i<2; i++){
	  for(uInt j=0; j<10; j++){
	    Double diff = routVal(IPosition(2,i,j))-rexpect(IPosition(2,i,j));
	    cout << i << " " << j << " " << routVal(IPosition(2,i,j)) << " " << rexpect(IPosition(2,i,j)) << endl;
	    cout << "flag " << i << " " << j << " " << outFlag(IPosition(2,i,j)) << " " << expflagsb(IPosition(2,i,j)) << endl;
	    AlwaysAssert((fabs(diff)<2E-5), AipsError);
	    AlwaysAssert(outFlag(IPosition(2,i,j)) == expflagsb(IPosition(2,i,j)), AipsError);
	  }
	}
      }

      cout << "--- right-shift by 1 channel in first axis with flags partially set, real data ----------------" << endl;

      whichAxis = 1;
      relshift = 1./10.;

      server.fftshift(routVal, outFlag, ra, aflags, whichAxis, relshift, True);

      {
	for(uInt i=0; i<2; i++){
	  for(uInt j=0; j<10; j++){
	    Double diff = routVal(IPosition(2,i,j))-rexpectb(IPosition(2,i,j));
	    cout << i << " " << j << " " << routVal(IPosition(2,i,j)) << " " << rexpectb(IPosition(2,i,j)) << endl;
	    cout << "flag " << i << " " << j << " " << outFlag(IPosition(2,i,j)) << " " << expflags(IPosition(2,i,j)) << endl;
	    AlwaysAssert(fabs(diff)<2E-5, AipsError);
	    AlwaysAssert(outFlag(IPosition(2,i,j)) == expflags(IPosition(2,i,j)), AipsError);
	  }
	}
      }


    } // enf for it=0

  }
};


template <class T, class S, template<class,class> class P, class TServer, class SServer>
class TestR2C   // real->complex and complex->real
{
  public:
    TestR2C(FFTServer<TServer, SServer> &server, double epsilon = FLT_EPSILON) 
        {
            Array<T> input          = P<T,S>::input();
            Array<S> expectedResult = P<T,S>::expectedResult();
            Test<T, S, TServer, SServer> (server, False, epsilon, input, expectedResult);
            Test<T, S, TServer, SServer> (server, True , epsilon, 
                          shift<T>(input,          input.shape(), expectedResult.shape()), 
                          shift<S>(expectedResult, input.shape(), expectedResult.shape()));

	    // Test the non-default  constInput = True
	    Bool constInput = True;

	    Array<T> input_before;
	    Array<S> whatever(expectedResult.shape());
	    input_before = input;

	    server.fft(whatever, input, constInput);

	    AlwaysTrue(allNearAbs(input, input_before, epsilon),
		       AipsError);

	    server.fft0(whatever, input, constInput);

	    AlwaysTrue(allNearAbs(input, input_before, epsilon),
		       AipsError);


        }
};

template <class T, template <class> class P, class TServer, class SServer>
class TestC2C
{
  private:
    void go(FFTServer<TServer, SServer> &server,
            Bool shifted,
            double epsilon1,
            double epsilon2,
            const Array<T> &input,
            const Array<T> &expected) 
        {
            Test<T, T, TServer, SServer> (server, shifted, epsilon1, input, expected);

	    // For complex arrays, excersize in-place transform
	    {
		Array<T> copy;       // Note: copy constructor would give just a reference, not a copy!
		                     // Assignement operator creates a true copy!
		copy = input;
		
		if (shifted) {
		    server.fft(copy);
		}
		else {
		    server.fft0(copy);
		}
		
		AlwaysTrue(copy.shape().isEqual(expected.shape()),
			   AipsError);
		AlwaysTrue(allNearAbs(copy, expected, epsilon1),
			   AipsError);

		// ... and inverse in-place

		if (debug) {
		    cout << "Input:" << endl;
		    dump(copy);
		}
		if (shifted) {
		    server.fft(copy, False);
		}
		else {
		    server.fft0(copy, False);
		}

		if (debug) {
		    cout << "Result:" << endl;
		    dump(copy);
		    cout << "Expected:" << endl;
		    dump(input);
		}
			    
		AlwaysTrue(copy.shape().isEqual(input.shape()),
			   AipsError);
		AlwaysTrue(allNearAbs(copy, input, 2*epsilon1),
			   AipsError);
	    }

            // For C2C, excercise inverse transformations too

            Array<T> p1, p2;
            
            if (shifted) {
                server.fft(p1, expected, False);
                server.fft(p2, p1, False);
            }
            else {
                server.fft0(p1, expected, False);
                server.fft0(p2, p1, False);
            }
            // Now input should be the forward transform of p2
            Test<T, T, TServer, SServer> (server, shifted, epsilon2, p2, input);
        }
            
  public:
    TestC2C(FFTServer<TServer, SServer> &server,
            double epsilon1 = FLT_EPSILON,
            double epsilon2 = FLT_EPSILON)
        {
            Array<T> input          = P<T>::input();
            Array<T> expectedResult = P<T>::expectedResult();

            go(server, False, epsilon1, epsilon2, input, expectedResult);

            // repeat using shifted arrays

            go(server, True, epsilon1, epsilon2, 
               shift<T>(input         , input.shape(), expectedResult.shape()),
               shift<T>(expectedResult, input.shape(), expectedResult.shape()));
        }
};



template <class T, class S>
void run_tests()
{
    FFTServer<T, S> server0(IPosition(1,8));
    TestR2C<T, S, R2C1Deven1, T, S> t1(server0);
    TestR2C<T, S, R2C1Deven2, T, S> t2(server0);
    TestR2C<T, S, R2C1Deven3, T, S> t3(server0);
    TestR2C<T, S, R2C1Deven4, T, S> t4(server0);
    TestR2C<T, S, R2C1Dodd1, T, S> t5(server0);
    TestR2C<T, S, R2C1Dodd2, T, S> t6(server0);
    TestR2C<T, S, R2C1Dodd3, T, S> t7(server0);
    TestR2C<T, S, R2C2Deveneven1, T, S> t8(server0);
    TestR2C<T, S, R2C2Deveneven2, T, S> t9(server0);
#ifdef HAVE_FFTW3
    FFTServer<T, S> server(IPosition(1,8));
    server = server0;              // test assignment
#else
    FFTServer<T, S> server(server0);
#endif
    TestR2C<T, S, R2C2Deveneven3, T, S> t10(server);
    TestR2C<T, S, R2C2Devenodd1, T, S> t11(server);
    TestR2C<T, S, R2C2Devenodd2, T, S> t12(server);
    TestR2C<T, S, R2C2Doddeven1, T, S> t13(server);         
    TestR2C<T, S, R2C2Doddeven2, T, S> t14(server);
    TestR2C<T, S, R2C2Doddodd1, T, S> t15(server);
    TestR2C<T, S, R2C2Doddodd2, T, S> t16(server);
    TestR2C<T, S, R2C3Deveneveneven1, T, S> t17(server);
    TestR2C<T, S, R2C3Deveneveneven2, T, S> t18(server);
    TestR2C<T, S, R2C3Doddoddodd1, T, S> t19(server);
    TestR2C<T, S, R2C3Doddoddodd2, T, S> t20(server, 100*FLT_EPSILON);
    TestR2C<T, S, R2C4Doddoddoddeven1, T, S> t21(server);
    TestR2C<T, S, R2C4Doddoddoddeven2, T, S> t22(server, 500*FLT_EPSILON);

    TestR2C<S, T, C2R1Deven1, T, S> c2r1(server);
    TestR2C<S, T, C2R1Deven2, T, S> c2r2(server);
    TestR2C<S, T, C2R1Deven3, T, S> c2r3(server);
    TestR2C<S, T, C2R1Deven4, T, S> c2r4(server);
    TestR2C<S, T, C2R1Deven5, T, S> c2r5(server);
    TestR2C<S, T, C2R1Dodd1, T, S> c2r6(server);
    TestR2C<S, T, C2R1Dodd2, T, S> c2r7(server);
    TestR2C<S, T, C2R2Deveneven1, T, S> c2r8(server);
    TestR2C<S, T, C2R2Deveneven2, T, S> c2r9(server);
    TestR2C<S, T, C2R2Deveneven3, T, S> c2r10(server, 4*FLT_EPSILON);
    TestR2C<S, T, C2R2Doddodd1, T, S> c2r11(server);
    TestR2C<S, T, C2R2Doddodd2, T, S> c2r12(server);
    TestR2C<S, T, C2R2Doddodd3, T, S> c2r13(server, 500*FLT_EPSILON);
    TestR2C<S, T, C2R2Devenodd1, T, S> c2r14(server);
    TestR2C<S, T, C2R2Devenodd2, T, S> c2r15(server);
    TestR2C<S, T, C2R2Doddeven1, T, S> c2r16(server);
    TestR2C<S, T, C2R2Doddeven2, T, S> c2r17(server);
    TestR2C<S, T, C2R3Deveneveneven1, T, S> c2r18(server);
    TestR2C<S, T, C2R3Deveneveneven2, T, S> c2r19(server);
    TestR2C<S, T, C2R3Doddoddodd1, T, S> c2r20(server);
    TestR2C<S, T, C2R3Doddoddodd2, T, S> c2r21(server);
    TestR2C<S, T, C2R4Doddoddoddeven1, T, S> c2r22(server);
    TestR2C<S, T, C2R4Doddoddoddeven2, T, S> c2r23(server);
    TestC2C<S, C2C1Deven1, T, S> c2c1(server);
    TestC2C<S, C2C1Deven2, T, S> c2c2(server);
    TestC2C<S, C2C1Deven3, T, S> c2c3(server);
    TestC2C<S, C2C1Deven4, T, S> c2c4(server);
    TestC2C<S, C2C1Dodd1, T, S> c2c5(server, FLT_EPSILON, 5*FLT_EPSILON);
    TestC2C<S, C2C1Dodd2, T, S> c2c6(server, 5*FLT_EPSILON, 5*FLT_EPSILON);
    TestC2C<S, C2C2Deveneven1, T, S> c2c7(server);
    TestC2C<S, C2C2Deveneven2, T, S> c2c8(server);
    TestC2C<S, C2C2Deveneven3, T, S> c2c9(server, 5*FLT_EPSILON);
    TestC2C<S, C2C2Doddodd1, T, S> c2c10(server);
    TestC2C<S, C2C2Doddodd2, T, S> c2c11(server);
    TestC2C<S, C2C2Devenodd1, T, S> c2c12(server);
    TestC2C<S, C2C2Devenodd2, T, S> c2c13(server);
    TestC2C<S, C2C2Doddeven1, T, S> c2c14(server);
    TestC2C<S, C2C2Doddeven2, T, S> c2c15(server);
    TestC2C<S, C2C3Doddeveneven1, T, S> c2c16(server);
    TestC2C<S, C2C3Doddeveneven2, T, S> c2c17(server);
    TestC2C<S, C2C3Doddoddodd1, T, S> c2c18(server, FLT_EPSILON, 2*FLT_EPSILON);
    TestC2C<S, C2C3Doddoddodd2, T, S> c2c19(server, 100*FLT_EPSILON, 2*FLT_EPSILON);
    TestC2C<S, C2C4Doddoddoddeven1, T, S> c2c20(server, FLT_EPSILON, 2*FLT_EPSILON);
    TestC2C<S, C2C4Doddoddoddeven2, T, S> c2c21(server, 500*FLT_EPSILON, 2*FLT_EPSILON);

    TestFFTShift<T, S> ();

    return;
}

int main()
{
  tests_done = 0;
  try {
      // Test combinations of
      // - single/double precision
      // - real->complex, complex->real, complex->complex
      // - forward / inverse
      // - 1d, 2d, 3d, 4d data
      // - even/odd dimensions
      // - fft() / fft0()
      // - inplace / copy
      // - const / non-const input

      run_tests<Float, Complex>();
      run_tests<Double, DComplex>();

  }
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  } 
  cout << tests_done << " tests OK" << endl;
  return 0;
}
