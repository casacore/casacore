//# Array2Math.cc: Arithmetic functions defined on Arrays
//# Copyright (C) 1993,1994,1995,1996,1999,2000
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

#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayError.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Mathematics/Math.h>

static Bool ArrayMinMaxPrinted = False;

void ArrayMinMaxPrintOnceDeprecated ()
{
    if (ArrayMinMaxPrinted == False) {
        cout << "\n\n"
             << "The following minMax() function is deprecated\n"
             << " due to its nonstandard argument order:\n"
             << "    void minMax(const Array<T> &a, T &min, T &max)\n"
             << "Use this minMax() instead:\n"
             << "    void minMax(T &min, T &max, const Array<T> &a)\n"
             << endl;

        ArrayMinMaxPrinted = True;
    }
    return;
}


//# We could use macros to considerably reduce the number of lines, however
//# that makes it harder to debug, understand, etc.

Array<Complex> conj(const Array<Complex> &carray)
{
  Array<Complex> retval(carray.copy());
  Bool delc;

  Complex *cptr = retval.getStorage(delc);
  
  uInt n=carray.nelements();
  for (uInt i=0; i<n; i++) cptr[i] = conj(cptr[i]);
  retval.putStorage(cptr, delc);
  return retval;
};
  
Array<DComplex> conj(const Array<DComplex> &carray)
{
  Array<DComplex> retval(carray.copy());
  Bool delc;

  DComplex *cptr = retval.getStorage(delc);
  
  uInt n=carray.nelements();
  for (uInt i=0; i<n; i++) cptr[i] = conj(cptr[i]);
  retval.putStorage(cptr, delc);
  return retval;
};

Matrix<Complex> conj(const Matrix<Complex> &carray)
{
  Matrix<Complex> retval(carray.copy());
  Bool delc;

  Complex *cptr = retval.getStorage(delc);
  
  uInt n=carray.nelements();
  for (uInt i=0; i<n; i++) cptr[i] = conj(cptr[i]);
  retval.putStorage(cptr, delc);
  return retval;
};
  
Matrix<DComplex> conj(const Matrix<DComplex> &carray)
{
  Matrix<DComplex> retval(carray.copy());
  Bool delc;

  DComplex *cptr = retval.getStorage(delc);
  
  uInt n=carray.nelements();
  for (uInt i=0; i<n; i++) cptr[i] = conj(cptr[i]);
  retval.putStorage(cptr, delc);
  return retval;
};

void  real(Array<Float> &rarray, const Array<Complex> &carray)
{
    if (rarray.shape() != carray.shape()) {
	throw(ArrayConformanceError("void  real(Array<Float> &rarray, "
				    "const Array<Complex> &carray) "
				    " - shapes not identical"));
    }
    Bool delc, delr;
    const Complex* cptr = carray.getStorage (delc);
    Float* rptr = rarray.getStorage (delr);
    uInt n = rarray.nelements();
    for (uInt i=0; i<n; i++) rptr[i] = cptr[i].real();
    carray.freeStorage (cptr, delc);
    rarray.putStorage (rptr, delr);
}

void  real(Array<Double> &rarray, const Array<DComplex> &carray)
{
    if (rarray.shape() != carray.shape()) {
	throw(ArrayConformanceError("void  real(Array<Double> &rarray, "
				    "const Array<DComplex> &carray) "
				    " - shapes not identical"));
    }
    Bool delc, delr;
    const DComplex* cptr = carray.getStorage (delc);
    Double* rptr = rarray.getStorage (delr);
    uInt n = rarray.nelements();
    for (uInt i=0; i<n; i++) rptr[i] = cptr[i].real();
    carray.freeStorage (cptr, delc);
    rarray.putStorage (rptr, delr);
}

void  imag(Array<Float> &rarray, const Array<Complex> &carray)
{
    if (rarray.shape() != carray.shape()) {
	throw(ArrayConformanceError("void  imag(Array<Float> &rarray, "
				    "const Array<Complex> &carray) "
				    " - shapes not identical"));
    }
    Bool delc, delr;
    const Complex* cptr = carray.getStorage (delc);
    Float* rptr = rarray.getStorage (delr);
    uInt n = rarray.nelements();
    for (uInt i=0; i<n; i++) rptr[i] = cptr[i].imag();
    carray.freeStorage (cptr, delc);
    rarray.putStorage (rptr, delr);
}

void  imag(Array<Double> &rarray, const Array<DComplex> &carray)
{
    if (rarray.shape() != carray.shape()) {
	throw(ArrayConformanceError("void  imag(Array<Double> &rarray, "
				    "const Array<DComplex> &carray) "
				    " - shapes not identical"));
    }
    Bool delc, delr;
    const DComplex* cptr = carray.getStorage (delc);
    Double* rptr = rarray.getStorage (delr);
    uInt n = rarray.nelements();
    for (uInt i=0; i<n; i++) rptr[i] = cptr[i].imag();
    carray.freeStorage (cptr, delc);
    rarray.putStorage (rptr, delr);
}

void  amplitude(Array<Float> &rarray, const Array<Complex> &carray)
{
    if (rarray.shape() != carray.shape()) {
	throw(ArrayConformanceError("void  amplitude(Array<Float> &rarray, "
				    "const Array<Complex> &carray) "
				    " - shapes not identical"));
    }
    Bool delc, delr;
    const Complex* cptr = carray.getStorage (delc);
    Float* rptr = rarray.getStorage (delr);
    uInt n = rarray.nelements();
    for (uInt i=0; i<n; i++) rptr[i] = fabs(cptr[i]);
    carray.freeStorage (cptr, delc);
    rarray.putStorage (rptr, delr);
}

void  amplitude(Array<Double> &rarray, const Array<DComplex> &carray)
{
    if (rarray.shape() != carray.shape()) {
	throw(ArrayConformanceError("void  amplitude(Array<Double> &rarray, "
				    "const Array<DComplex> &carray) "
				    " - shapes not identical"));
    }
    Bool delc, delr;
    const DComplex* cptr = carray.getStorage (delc);
    Double* rptr = rarray.getStorage (delr);
    uInt n = rarray.nelements();
    for (uInt i=0; i<n; i++) rptr[i] = fabs(cptr[i]);
    carray.freeStorage (cptr, delc);
    rarray.putStorage (rptr, delr);
}

void  phase(Array<Float> &rarray, const Array<Complex> &carray)
{
    if (rarray.shape() != carray.shape()) {
	throw(ArrayConformanceError("void  phase(Array<Float> &rarray, "
				    "const Array<Complex> &carray) "
				    " - shapes not identical"));
    }
    Bool delc, delr;
    const Complex* cptr = carray.getStorage (delc);
    Float* rptr = rarray.getStorage (delr);
    uInt n = rarray.nelements();
    for (uInt i=0; i<n; i++) rptr[i] = arg(cptr[i]);
    carray.freeStorage (cptr, delc);
    rarray.putStorage (rptr, delr);
}

void  phase(Array<Double> &rarray, const Array<DComplex> &carray)
{
    if (rarray.shape() != carray.shape()) {
	throw(ArrayConformanceError("void  phase(Array<Double> &rarray, "
				    "const Array<DComplex> &carray) "
				    " - shapes not identical"));
    }
    Bool delc, delr;
    const DComplex* cptr = carray.getStorage (delc);
    Double* rptr = rarray.getStorage (delr);
    uInt n = rarray.nelements();
    for (uInt i=0; i<n; i++) rptr[i] = arg(cptr[i]);
    carray.freeStorage (cptr, delc);
    rarray.putStorage (rptr, delr);
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
    if (rarray.ndim() != carray.ndim()) {
	throw(ArrayError("::ComplexToReal(Array<Float> &rarray, const "
			 "Array<Complex> &carray) - rarray.ndim() != "
			 "carray.ndim()"));
    }
    if (rarray.shape()(0) != 2*carray.shape()(0)) {
	throw(ArrayError("::ComplexToReal(Array<Float> &rarray, const "
			 "Array<Complex> &carray) - rarray.shape()(0) != "
			 "2*carray.shape()(0)"));
    }

    Bool delr, delc;

    Float *rptr = rarray.getStorage(delr);
    const Complex *cptr = carray.getStorage(delc);

    uInt n=carray.nelements();
    for (uInt i=0; i<n; i++) {
	rptr[2*i] = cptr[i].real();
	rptr[2*i+1] = cptr[i].imag();
    }
    rarray.putStorage(rptr, delr);
    carray.freeStorage(cptr, delc);
}

// <thrown>
//     <item> ArrayError
// </thrown>
void ComplexToReal(Array<Double> &rarray, const Array<DComplex> &carray)
{
    if (rarray.ndim() != carray.ndim()) {
	throw(ArrayError("::DComplexToReal(Array<Double> &rarray, const "
			 "Array<DComplex> &carray) - rarray.ndim() != "
			 "carray.ndim()"));
    }
    if (rarray.shape()(0) != 2*carray.shape()(0)) {
	throw(ArrayError("::DComplexToReal(Array<Double> &rarray, const "
			 "Array<DComplex> &carray) - rarray.shape()(0) != "
			 "2*carray.shape()(0)"));
    }

    Bool delr, delc;

    Double *rptr = rarray.getStorage(delr);
    const DComplex *cptr = carray.getStorage(delc);

    uInt n=carray.nelements();
    for (uInt i=0; i<n; i++) {
	rptr[2*i] = cptr[i].real();
	rptr[2*i+1] = cptr[i].imag();
    }
    rarray.putStorage(rptr, delr);
    carray.freeStorage(cptr, delc);
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
    if (rarray.ndim() != carray.ndim()) {
	throw(ArrayError("::RealToComplex(Array<Complex> &carray, const "
			 "Array<Float> &rarray) - rarray.ndim() != "
			 "carray.ndim()"));
    }
    if (rarray.shape()(0) != 2*carray.shape()(0)) {
	throw(ArrayError("::RealToComplex(Array<Complex> &carray, const "
			 "Array<Float> &rarray) - rarray.shape()(0) != "
			 "2*carray.shape()(0)"));
    }

    Bool delr, delc;

    const Float *rptr = rarray.getStorage(delr);
    Complex *cptr = carray.getStorage(delc);

    uInt n=carray.nelements();
    for (uInt i=0; i<n; i++) {
      ///	cptr[i].real() = rptr[2*i];
      ///	cptr[i].imag() = rptr[2*i+1];
      cptr[i] = Complex(rptr[2*i], rptr[2*i+1]);
    }
    rarray.freeStorage(rptr, delr);
    carray.putStorage(cptr, delc);
}

// <thrown>
//     <item> ArrayError
// </thrown>
void RealToComplex(Array<DComplex> &carray, const Array<Double> &rarray)
{
    if (rarray.ndim() != carray.ndim()) {
	throw(ArrayError("::RealToDComplex(Array<DComplex> &carray, const "
			 "Array<Double> &rarray) - rarray.ndim() != "
			 "carray.ndim()"));
    }
    if (rarray.shape()(0) != 2*carray.shape()(0)) {
	throw(ArrayError("::RealToDComplex(Array<DComplex> &carray, const "
			 "Array<Double> &rarray) - rarray.shape()(0) != "
			 "2*carray.shape()(0)"));
    }

    Bool delr, delc;

    const Double *rptr = rarray.getStorage(delr);
    DComplex *cptr = carray.getStorage(delc);

    uInt n=carray.nelements();
    for (uInt i=0; i<n; i++) {
      ///	cptr[i].real() = rptr[2*i];
      ///	cptr[i].imag() = rptr[2*i+1];
      cptr[i] = DComplex(rptr[2*i], rptr[2*i+1]);
    }
    rarray.freeStorage(rptr, delr);
    carray.putStorage(cptr, delc);
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

 






