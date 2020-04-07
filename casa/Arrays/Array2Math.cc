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

#include "ArrayMath.h"
#include "ArrayError.h"
#include "Matrix.h"

#include <complex>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# We could use macros to considerably reduce the number of lines, however
//# that makes it harder to debug, understand, etc.

Array<std::complex<float>> conj(const Array<std::complex<float>> &carray)
{
  return arrayTransformResult (carray, [](std::complex<float> v) { return std::conj(v); });
}
  
Array<std::complex<double>> conj(const Array<std::complex<double>> &carray)
{
  return arrayTransformResult (carray, [](std::complex<double> v) { return std::conj(v); });
}

Matrix<std::complex<float>> conj(const Matrix<std::complex<float>> &carray)
{
  return Matrix<std::complex<float>>(conj ((const Array<std::complex<float>>&)carray));
}
  
Matrix<std::complex<double>> conj(const Matrix<std::complex<double>> &carray)
{
  return Matrix<std::complex<double>>(conj ((const Array<std::complex<double>>&)carray));
}

void conj(Array<std::complex<float>> &rarray, const Array<std::complex<float>> &carray)
{
  checkArrayShapes (carray, rarray, "conj");
  arrayTransform (carray, rarray, [](std::complex<float> v) { return std::conj(v); });
}

void conj(Array<std::complex<double>> &rarray, const Array<std::complex<double>> &carray)
{
  checkArrayShapes (carray, rarray, "conj");
  arrayTransform (carray, rarray, [](std::complex<double> v) { return std::conj(v); });
}

void real(Array<float> &rarray, const Array<std::complex<float>> &carray)
{
  checkArrayShapes (carray, rarray, "real");
  // std::real is only a template since c++14 :(
  arrayTransform (carray, rarray, [](std::complex<float> v) { return std::real(v); });
}

void real(Array<double> &rarray, const Array<std::complex<double>> &carray)
{
  checkArrayShapes (carray, rarray, "real");
  arrayTransform (carray, rarray, [](std::complex<double> v) { return std::real(v); });
}

void imag(Array<float> &rarray, const Array<std::complex<float>> &carray)
{
  checkArrayShapes (carray, rarray, "imag");
  arrayTransform (carray, rarray, [](std::complex<float> v) { return std::imag(v); });
}

void imag(Array<double> &rarray, const Array<std::complex<double>> &carray)
{
  checkArrayShapes (carray, rarray, "imag");
  arrayTransform (carray, rarray, [](std::complex<double> v) { return std::imag(v); });
}

void amplitude(Array<float> &rarray, const Array<std::complex<float>> &carray)
{
  checkArrayShapes (carray, rarray, "amplitude");
  arrayTransform (carray, rarray, std::abs<float>);
}

void amplitude(Array<double> &rarray, const Array<std::complex<double>> &carray)
{
  checkArrayShapes (carray, rarray, "amplitude");
  arrayTransform (carray, rarray, std::abs<double>);
}

void phase(Array<float> &rarray, const Array<std::complex<float>> &carray)
{
  checkArrayShapes (carray, rarray, "pahse");
  arrayTransform (carray, rarray, [](std::complex<float> v) { return std::arg(v); });
}

void phase(Array<double> &rarray, const Array<std::complex<double>> &carray)
{
  checkArrayShapes (carray, rarray, "phase");
  arrayTransform (carray, rarray, [](std::complex<double> v) { return std::arg(v); });
}

Array<float> real(const Array<std::complex<float>> &carray)
{
  Array<float> rarray(carray.shape());
  real(rarray, carray);
  return rarray;
}
Array<double> real(const Array<std::complex<double>> &carray)
{
  Array<double> rarray(carray.shape());
  real(rarray, carray);
  return rarray;
}


Array<float> imag(const Array<std::complex<float>> &carray)
{
  Array<float> rarray(carray.shape());
  imag(rarray, carray);
  return rarray;
}
Array<double> imag(const Array<std::complex<double>> &carray)
{
  Array<double> rarray(carray.shape());
  imag(rarray, carray);
  return rarray;
}

Array<float> amplitude(const Array<std::complex<float>> &carray)
{
  Array<float> rarray(carray.shape());
  amplitude(rarray, carray);
  return rarray;
}
Array<double> amplitude(const Array<std::complex<double>> &carray)
{
  Array<double> rarray(carray.shape());
  amplitude(rarray, carray);
  return rarray;
}

Array<float> phase(const Array<std::complex<float>> &carray)
{
  Array<float> rarray(carray.shape());
  phase(rarray, carray);
  return rarray;
}

Array<double> phase(const Array<std::complex<double>> &carray)
{
  Array<double> rarray(carray.shape());
  phase(rarray, carray);
  return rarray;
}

// <thrown>
//     <item> ArrayError
// </thrown>
void ComplexToReal(Array<float> &rarray, const Array<std::complex<float>> &carray)
{
  if (rarray.nelements() != 2*carray.nelements()) {
    throw(ArrayError("::ComplexToReal(Array<float> &rarray, const "
                     "Array<std::complex<float>> &carray) - rarray.nelements() != "
                     "2*carray.nelements()"));
  }
  if (rarray.contiguousStorage()  &&  carray.contiguousStorage()) {
    memcpy (const_cast<float*>(rarray.data()), carray.data(),
            rarray.nelements() * sizeof(float));
  } else {
    Array<std::complex<float>>::const_iterator citer=carray.begin();
    Array<float>::iterator rend = rarray.end();
    for (Array<float>::iterator riter = rarray.begin();
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
void ComplexToReal(Array<double> &rarray, const Array<std::complex<double>> &carray)
{
  if (rarray.nelements() != 2*carray.nelements()) {
    throw(ArrayError("::ComplexToReal(Array<double> &rarray, const "
                     "Array<std::complex<double>> &carray) - rarray.nelements() != "
                     "2*carray.nelements()"));
  }
  if (rarray.contiguousStorage()  &&  carray.contiguousStorage()) {
    memcpy (const_cast<double*>(rarray.data()), carray.data(),
            rarray.nelements() * sizeof(double));
  } else {
    Array<std::complex<double>>::const_iterator citer=carray.begin();
    Array<double>::iterator rend = rarray.end();
    for (Array<double>::iterator riter = rarray.begin();
         riter!=rend;  ++riter, ++citer) {
      *riter = real(*citer);
      ++riter;
      *riter = imag(*citer);
    }
  }
}

Array<float> ComplexToReal(const Array<std::complex<float>> &carray)
{
  IPosition shape = carray.shape();
  shape(0) *= 2;
  Array<float> retval(shape);
  ComplexToReal(retval, carray);
  return retval;
}

Array<double> ComplexToReal(const Array<std::complex<double>> &carray)
{
  IPosition shape = carray.shape();
  shape(0) *= 2;
  Array<double> retval(shape);
  ComplexToReal(retval, carray);
  return retval;
}

// <thrown>
//     <item> ArrayError
// </thrown>
void RealToComplex(Array<std::complex<float>> &carray, const Array<float> &rarray)
{
  if (rarray.nelements() != 2*carray.nelements()) {
    throw(ArrayError("::RealToComplex(Array<std::complex<float>> &carray, const "
                     "Array<float> &rarray) - rarray.nelements() != "
                     "2*carray.nelements()"));
  }
  if (rarray.contiguousStorage()  &&  carray.contiguousStorage()) {
    std::copy_n(rarray.data(), rarray.nelements(), reinterpret_cast<float*>(const_cast<std::complex<float>*>(carray.data())));
  } else {
    Array<std::complex<float>>::iterator citer=carray.begin();
    Array<float>::const_iterator rend = rarray.end();
    for (Array<float>::const_iterator riter = rarray.begin();
         riter!=rend;  ++riter, ++citer) {
      float r = *riter;
      ++riter;
      *citer = std::complex<float>(r, *riter);
    }
  }
}

// <thrown>
//     <item> ArrayError
// </thrown>
void RealToComplex(Array<std::complex<double>> &carray, const Array<double> &rarray)
{
  if (rarray.nelements() != 2*carray.nelements()) {
    throw(ArrayError("::RealToComplex(Array<std::complex<double>> &carray, const "
                     "Array<double> &rarray) - rarray.nelements() != "
                     "2*carray.nelements()"));
  }
  if (rarray.contiguousStorage()  &&  carray.contiguousStorage()) {
    std::copy_n(rarray.data(), rarray.nelements(), reinterpret_cast<double*>(const_cast<std::complex<double>*>(carray.data())));
  } else {
    Array<std::complex<double>>::iterator citer=carray.begin();
    Array<double>::const_iterator rend = rarray.end();
    for (Array<double>::const_iterator riter = rarray.begin();
         riter!=rend;  ++riter, ++citer) {
      double r = *riter;
      ++riter;
      *citer = std::complex<double>(r, *riter);
    }
  }
}


// <thrown>
//    <item> ArrayError
// </thrown>
Array<std::complex<float>> RealToComplex(const Array<float> &rarray)
{
  IPosition shape = rarray.shape();
  if (shape(0) %2 == 1) { // Odd size
    throw(ArrayError("Array<std::complex<float>> RealToComplex(const Array<float> &"
                     "rarray) - rarray.shape()(0) not even"));
  }
  shape(0) /= 2;
  Array<std::complex<float>> retval(shape);
  RealToComplex(retval, rarray);
  return retval;
}

// <thrown>
//    <item> ArrayError
// </thrown>
Array<std::complex<double>> RealToComplex(const Array<double> &rarray)
{
  IPosition shape = rarray.shape();
  if (shape(0) %2 == 1) { // Odd size
    throw(ArrayError("Array<std::complex<double>> RealToComplex(const Array<double> &"
                     "rarray) - rarray.shape()(0) not even"));
  }
  shape(0) /= 2;
  Array<std::complex<double>> retval(shape);
  RealToComplex(retval, rarray);
  return retval;
}


IPosition checkExpandArray (IPosition& mult,
                            IPosition& inshp,
                            const IPosition& inShape,
                            const IPosition& outShape,
                            const IPosition& alternate)
{
  if (inShape.size() == 0) {
    throw ArrayError("expandArray: input array cannot be empty");
  }
  mult.resize (outShape.size());
  inshp.resize (outShape.size());
  inshp = 1;                             // missing axes have length 1
  IPosition alt(outShape.size(), 0);
  for (size_t i=0; i<outShape.size(); ++i) {
    if (i < inShape.size()) {
      inshp[i] = inShape[i];
    }
    if (inshp[i] <= 0  ||  inshp[i] > outShape[i]  ||
        outShape[i] % inshp[i] != 0) {
      throw ArrayError("expandArray: length of each input array axis must "
                       "be <= output axis and divide evenly");
    }
    // Note that for an input length 1, linear and alternate come to the same.
    // Linear is faster, so use that if possible.
    if (i < alternate.size()  &&  inshp[i] > 1) {
      alt[i] = alternate[i];
    }
    mult[i] = outShape[i] / inshp[i];
  }
  return alt;
}


} //# NAMESPACE CASACORE - END

