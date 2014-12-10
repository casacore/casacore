//# Array2Math.cc: Arithmetic functions defined on Arrays
//# Copyright (C) 1993,1994,1995,1996,1999,2000,2001,2002
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

#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayError.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/iostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN


void throwArrayShapes (const char* name)
{
  throw ArrayConformanceError ("ArrayMath/Logical function " + String(name) +
                               ": arrays not conformant");
}


//# We could use macros to considerably reduce the number of lines, however
//# that makes it harder to debug, understand, etc.

Array<Complex> conj(const Array<Complex> &carray)
{
  return arrayTransformResult (carray, casacore::Conj<Complex>());
}
  
Array<DComplex> conj(const Array<DComplex> &carray)
{
  return arrayTransformResult (carray, casacore::Conj<DComplex>());
}

Matrix<Complex> conj(const Matrix<Complex> &carray)
{
  return Matrix<Complex>(conj ((const Array<Complex>&)carray));
}
  
Matrix<DComplex> conj(const Matrix<DComplex> &carray)
{
  return Matrix<DComplex>(conj ((const Array<DComplex>&)carray));
}

void conj(Array<Complex> &rarray, const Array<Complex> &carray)
{
  checkArrayShapes (carray, rarray, "conj");
  arrayTransform (carray, rarray, casacore::Conj<Complex>());
}

void conj(Array<DComplex> &rarray, const Array<DComplex> &carray)
{
  checkArrayShapes (carray, rarray, "conj");
  arrayTransform (carray, rarray, casacore::Conj<DComplex>());
}

void real(Array<Float> &rarray, const Array<Complex> &carray)
{
  checkArrayShapes (carray, rarray, "real");
  arrayTransform (carray, rarray, casacore::Real<Complex,Float>());
}

void real(Array<Double> &rarray, const Array<DComplex> &carray)
{
  checkArrayShapes (carray, rarray, "real");
  arrayTransform (carray, rarray, casacore::Real<DComplex,Double>());
}

void imag(Array<Float> &rarray, const Array<Complex> &carray)
{
  checkArrayShapes (carray, rarray, "imag");
  arrayTransform (carray, rarray, casacore::Imag<Complex,Float>());
}

void imag(Array<Double> &rarray, const Array<DComplex> &carray)
{
  checkArrayShapes (carray, rarray, "imag");
  arrayTransform (carray, rarray, casacore::Imag<DComplex,Double>());
}

void amplitude(Array<Float> &rarray, const Array<Complex> &carray)
{
  checkArrayShapes (carray, rarray, "amplitude");
  arrayTransform (carray, rarray, casacore::CAbs<Complex,Float>());
}

void amplitude(Array<Double> &rarray, const Array<DComplex> &carray)
{
  checkArrayShapes (carray, rarray, "amplitude");
  arrayTransform (carray, rarray, casacore::CAbs<DComplex,Double>());
}

void phase(Array<Float> &rarray, const Array<Complex> &carray)
{
  checkArrayShapes (carray, rarray, "pahse");
  arrayTransform (carray, rarray, casacore::CArg<Complex,Float>());
}

void phase(Array<Double> &rarray, const Array<DComplex> &carray)
{
  checkArrayShapes (carray, rarray, "phase");
  arrayTransform (carray, rarray, casacore::CArg<DComplex,Double>());
}

Array<Float> real(const Array<Complex> &carray)
{
  Array<Float> rarray(carray.shape());
  real(rarray, carray);
  return rarray;
}
Array<Double> real(const Array<DComplex> &carray)
{
  Array<Double> rarray(carray.shape());
  real(rarray, carray);
  return rarray;
}


Array<Float> imag(const Array<Complex> &carray)
{
  Array<Float> rarray(carray.shape());
  imag(rarray, carray);
  return rarray;
}
Array<Double> imag(const Array<DComplex> &carray)
{
  Array<Double> rarray(carray.shape());
  imag(rarray, carray);
  return rarray;
}

Array<Float> amplitude(const Array<Complex> &carray)
{
  Array<Float> rarray(carray.shape());
  amplitude(rarray, carray);
  return rarray;
}
Array<Double> amplitude(const Array<DComplex> &carray)
{
  Array<Double> rarray(carray.shape());
  amplitude(rarray, carray);
  return rarray;
}

Array<Float> phase(const Array<Complex> &carray)
{
  Array<Float> rarray(carray.shape());
  phase(rarray, carray);
  return rarray;
}

Array<Double> phase(const Array<DComplex> &carray)
{
  Array<Double> rarray(carray.shape());
  phase(rarray, carray);
  return rarray;
}

// <thrown>
//     <item> ArrayError
// </thrown>
void ComplexToReal(Array<Float> &rarray, const Array<Complex> &carray)
{
  if (rarray.nelements() != 2*carray.nelements()) {
    throw(ArrayError("::ComplexToReal(Array<Float> &rarray, const "
                     "Array<Complex> &carray) - rarray.nelements() != "
                     "2*carray.nelements()"));
  }
  if (rarray.contiguousStorage()  &&  carray.contiguousStorage()) {
    memcpy (const_cast<Float*>(rarray.data()), carray.data(),
            rarray.nelements() * sizeof(Float));
  } else {
    Array<Complex>::const_iterator citer=carray.begin();
    Array<Float>::iterator rend = rarray.end();
    for (Array<Float>::iterator riter = rarray.begin();
         riter!=rend;  ++riter, ++citer) {
      *riter = real(*citer);
      ++riter;
      *riter = imag(*citer);
    }
  }
}

// <thrown>
//     <item> ArrayError
// </thrown>
void ComplexToReal(Array<Double> &rarray, const Array<DComplex> &carray)
{
  if (rarray.nelements() != 2*carray.nelements()) {
    throw(ArrayError("::ComplexToReal(Array<Double> &rarray, const "
                     "Array<DComplex> &carray) - rarray.nelements() != "
                     "2*carray.nelements()"));
  }
  if (rarray.contiguousStorage()  &&  carray.contiguousStorage()) {
    memcpy (const_cast<Double*>(rarray.data()), carray.data(),
            rarray.nelements() * sizeof(Double));
  } else {
    Array<DComplex>::const_iterator citer=carray.begin();
    Array<Double>::iterator rend = rarray.end();
    for (Array<Double>::iterator riter = rarray.begin();
         riter!=rend;  ++riter, ++citer) {
      *riter = real(*citer);
      ++riter;
      *riter = imag(*citer);
    }
  }
}

Array<Float> ComplexToReal(const Array<Complex> &carray)
{
  IPosition shape = carray.shape();
  shape(0) *= 2;
  Array<Float> retval(shape);
  ComplexToReal(retval, carray);
  return retval;
}

Array<Double> ComplexToReal(const Array<DComplex> &carray)
{
  IPosition shape = carray.shape();
  shape(0) *= 2;
  Array<Double> retval(shape);
  ComplexToReal(retval, carray);
  return retval;
}

// <thrown>
//     <item> ArrayError
// </thrown>
void RealToComplex(Array<Complex> &carray, const Array<Float> &rarray)
{
  if (rarray.nelements() != 2*carray.nelements()) {
    throw(ArrayError("::RealToComplex(Array<Complex> &carray, const "
                     "Array<Float> &rarray) - rarray.nelements() != "
                     "2*carray.nelements()"));
  }
  if (rarray.contiguousStorage()  &&  carray.contiguousStorage()) {
    memcpy (const_cast<Complex*>(carray.data()), rarray.data(),
            rarray.nelements() * sizeof(Float));
  } else {
    Array<Complex>::iterator citer=carray.begin();
    Array<Float>::const_iterator rend = rarray.end();
    for (Array<Float>::const_iterator riter = rarray.begin();
         riter!=rend;  ++riter, ++citer) {
      Float r = *riter;
      ++riter;
      *citer = Complex(r, *riter);
    }
  }
}

// <thrown>
//     <item> ArrayError
// </thrown>
void RealToComplex(Array<DComplex> &carray, const Array<Double> &rarray)
{
  if (rarray.nelements() != 2*carray.nelements()) {
    throw(ArrayError("::RealToComplex(Array<DComplex> &carray, const "
                     "Array<Double> &rarray) - rarray.nelements() != "
                     "2*carray.nelements()"));
  }
  if (rarray.contiguousStorage()  &&  carray.contiguousStorage()) {
    memcpy (const_cast<DComplex*>(carray.data()), rarray.data(),
            rarray.nelements() * sizeof(Double));
  } else {
    Array<DComplex>::iterator citer=carray.begin();
    Array<Double>::const_iterator rend = rarray.end();
    for (Array<Double>::const_iterator riter = rarray.begin();
         riter!=rend;  ++riter, ++citer) {
      Double r = *riter;
      ++riter;
      *citer = DComplex(r, *riter);
    }
  }
}


// <thrown>
//    <item> ArrayError
// </thrown>
Array<Complex> RealToComplex(const Array<Float> &rarray)
{
  IPosition shape = rarray.shape();
  if (shape(0) %2 == 1) { // Odd size
    throw(ArrayError("Array<Complex> RealToComplex(const Array<Float> &"
                     "rarray) - rarray.shape()(0) not even"));
  }
  shape(0) /= 2;
  Array<Complex> retval(shape);
  RealToComplex(retval, rarray);
  return retval;
}

// <thrown>
//    <item> ArrayError
// </thrown>
Array<DComplex> RealToComplex(const Array<Double> &rarray)
{
  IPosition shape = rarray.shape();
  if (shape(0) %2 == 1) { // Odd size
    throw(ArrayError("Array<DComplex> RealToDComplex(const Array<Double> &"
                     "rarray) - rarray.shape()(0) not even"));
  }
  shape(0) /= 2;
  Array<DComplex> retval(shape);
  RealToComplex(retval, rarray);
  return retval;
}


} //# NAMESPACE CASACORE - END

