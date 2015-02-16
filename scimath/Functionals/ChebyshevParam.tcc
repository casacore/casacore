//# ChebyshevParam.cc  a function class that defines a ChebyshevParam polynomial
//# Copyright (C) 2000,2001,2002,2003
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

#ifndef SCIMATH_CHEBYSHEVPARAM_TCC
#define SCIMATH_CHEBYSHEVPARAM_TCC

//# Includes
#include <casacore/scimath/Functionals/ChebyshevParam.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayUtil.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Containers/RecordInterface.h>
#include <casacore/casa/Utilities/MUString.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors
template <class T>
ChebyshevParam<T>::ChebyshevParam() :
  Function1D<T>(1), def_p(T(0)), 
  minx_p(T(-1)), maxx_p(T(1)), mode_p(ChebyshevEnums::CONSTANT) {} 

template <class T>
ChebyshevParam<T>::ChebyshevParam(const uInt n) :
  Function1D<T>(n+1), def_p(T(0)), 
  minx_p(T(-1)), maxx_p(T(1)), mode_p(ChebyshevEnums::CONSTANT) {} 

template <class T>
ChebyshevParam<T>::ChebyshevParam(const uInt n, const RecordInterface&) :
  Function1D<T>(n+1), def_p(T(0)), 
  minx_p(T(-1)), maxx_p(T(1)), mode_p(ChebyshevEnums::CONSTANT) 
{ } 

template <class T>
ChebyshevParam<T>::ChebyshevParam(const T &min, const T &max,
				  ChebyshevEnums::OutOfIntervalMode mode,
				  const T &defval) :
  Function1D<T>(1), def_p(defval), mode_p(mode) {
  param_p[0] = 1;
  this->setInterval(min, max);
}

template <class T>
ChebyshevParam<T>::ChebyshevParam(const Vector<T> &coeffs,
				  const T &min, const T &max, 
				  ChebyshevEnums::OutOfIntervalMode,
				  const T &defval) :
  Function1D<T>(coeffs.nelements()), def_p(defval), 
    minx_p(min), maxx_p(max), mode_p(ChebyshevEnums::CONSTANT) 
{ 
    setCoefficients(coeffs);
}

template <class T>
ChebyshevParam<T>::ChebyshevParam(const Vector<T> &coeffs,
				  const RecordInterface& mode) :
  Function1D<T>(coeffs.nelements()), def_p(T(0)), 
    minx_p(T(-1)), maxx_p(T(1)), mode_p(ChebyshevEnums::CONSTANT) 
{ 
    setMode(mode);
    setCoefficients(coeffs);
}

template <class T>
ChebyshevParam<T>::ChebyshevParam(const ChebyshevParam &other) :
  Function1D<T>(other), def_p(other.def_p), 
  minx_p(other.minx_p), maxx_p(other.maxx_p), mode_p(other.mode_p) {} 

template <class T>
ChebyshevParam<T> &
ChebyshevParam<T>::operator=(const ChebyshevParam<T> &other) {
  if (this != &other) {
    mode_p = other.mode_p;
    minx_p = other.minx_p;
    maxx_p = other.maxx_p;
    def_p = other.def_p;
  }
  return *this;
}

template <class T>
ChebyshevParam<T>::~ChebyshevParam() {}

//# Operators

//# Member functions
template <class T>
void ChebyshevParam<T>::setCoefficients(const Vector<T> &coeffs) {
  if (coeffs.nelements() == 0) {
    throw AipsError("ChebyshevParam<T>::setCoeffiecients(): "
		    "empty Vector passed");
  }
  for (uInt i=0; i<coeffs.nelements(); ++i) setCoefficient(i, coeffs[i]);
}

template <class T>
void ChebyshevParam<T>::setCoefficient(const uInt which,
					 const T &value) {
  if (which >= nparameters()) {
    uInt sz = nparameters();
    FunctionParam<T> cfp(param_p);
    param_p = FunctionParam<T>(which+1);
    for (uInt i=0; i<sz; ++i) {
      param_p[i] = cfp[i];
      param_p.mask(i) =cfp.mask(i);
    }
  }
  param_p[which] = value;
}

template <class T>
const Vector<T> &ChebyshevParam<T>::getCoefficients() const {
  return param_p.getParameters();
}

template <class T>
void ChebyshevParam<T>::derivativeCoeffs(Vector<T> &coeffs,
					   const T &xmin, const T &xmax) {
    // first get power series coefficients
    Vector<T> ce(coeffs);
    chebyshevToPower(ce);
    // take the derivative
    Vector<T> &dce = coeffs;
    dce.resize(ce.nelements()-1);
    for (uInt i=1; i<ce.nelements(); ++i) dce(i-1) =
					    T(2*i)*ce(i) / (xmax-xmin);
    // convert back to ChebyshevParam
    powerToChebyshev(dce);
}

template <class T>
void ChebyshevParam<T>::powerToChebyshev(Vector<T> &coeffs) {
  uInt n = coeffs.nelements();
  // Create an inverse transformation matrix
  Matrix<T> poly(n, n, T(0));
  poly(0,0) = T(1);
  poly(1,1) = T(1);
  T scale;
  for (uInt i=2; i<n; i++) {
    scale = T(1) / pow(T(2), T(i-1));
    Int j;
    uInt k;
    for (j=i, k=1; j>1; j-=2, k++) {
      poly(j,i) = scale;
      scale *= T((i - k + 1) / k);
    }
    poly(j,i) = scale;
    if (j == 0) poly(j,i) /= 2;
  }
  // multiply transformation matrix by coefficient vector
  for (uInt i=0; i<n; i++) {
    coeffs(i) *= poly(i,i);
    for (uInt k=i+2; k<n; k += 2) coeffs(i) += poly(i,k)*coeffs(k);
  }
}

template <class T>
void ChebyshevParam<T>::chebyshevToPower(Vector<T> &coeffs) {
  uInt n = coeffs.nelements();
  // Create a transformation matrix
  Matrix<T> cheb(n, n, T(0));
  cheb(0,0) = T(1);
  cheb(1,1) = T(1);
  for (uInt i=2; i<n; i++) {
    for (Int j=i; j>0; j -= 2) {
      if (j > 1) cheb(j-2,i) -= cheb(j-2,i-2);
      cheb(j,i) += T(2)*cheb(j-1,i-1);
    }
  }
  // multiply transformation matrix by coefficient vector
  for (uInt i=0; i<n; i++) {
    coeffs(i) *= cheb(i,i);
    for (uInt k=i+2; k<n; k += 2) coeffs(i) += cheb(i,k)*coeffs(k);
  }
}

template <class T>
Vector<String> ChebyshevParam<T>::modes_s = 
stringToVector("constant zeroth extrapolate cyclic edge", ' ');

template <class T>
Bool ChebyshevParamModeImpl<T>::hasMode() const { return True; }

template <class T>
void ChebyshevParamModeImpl<T>::setMode(const RecordInterface& in) {

    // interval of interest
    if (in.isDefined(String("interval"))) {
	RecordFieldId fld("interval");
	if (in.type(in.idToNumber(fld)) == TpArrayDouble   ||
	    in.type(in.idToNumber(fld)) == TpArrayComplex  ||
	    in.type(in.idToNumber(fld)) == TpArrayDComplex ||
	    in.type(in.idToNumber(fld)) == TpArrayFloat    ||
	    in.type(in.idToNumber(fld)) == TpArrayInt)
	{
	    Vector<T> intv;
	    in.get(fld, intv);
	    if (intv(0) < intv(1)) 
		this->setInterval(intv(0), intv(1));
	    else 
		this->setInterval(intv(0), intv(1));
	}
    }

    // default value
    if (in.isDefined(String("default"))) {
	RecordFieldId fld("default");
	if (in.type(in.idToNumber(fld)) == TpDouble   ||
	    in.type(in.idToNumber(fld)) == TpComplex  ||
	    in.type(in.idToNumber(fld)) == TpDComplex ||
	    in.type(in.idToNumber(fld)) == TpFloat    ||
	    in.type(in.idToNumber(fld)) == TpInt)
	{
	    T def;
	    in.get(fld, def);
	    this->setDefault(def);
	}
    }

    // out-of-interval mode
    if (in.isDefined(String("intervalMode"))) {
	RecordFieldId fld("intervalMode");
	if (in.type(in.idToNumber(fld)) == TpString) {
	    String mode;
	    in.get(fld, mode);
	    uInt match = MUString::minimaxNC(mode, modes_s);
	    if (mode.length() > 0 && match < modes_s.nelements()) 
		setOutOfIntervalMode(static_cast<ChebyshevEnums::OutOfIntervalMode>(match));
	    else 
		throw AipsError(String("Unrecognized intervalMode: ") + mode);
	}
    }
}	    

template <class T>
void ChebyshevParamModeImpl<T>::getMode(RecordInterface& out) const {
    Vector<T> intv(2);
    intv(0) = getIntervalMin();
    intv(1) = getIntervalMax();

    out.define(RecordFieldId("interval"), intv);
    out.define(RecordFieldId("default"), getDefault());
    out.define(RecordFieldId("intervalMode"), modes_s(getOutOfIntervalMode()));
}

// specialization for AutoDiff

template <class T>
void ChebyshevParamModeImpl<AutoDiff<T> >::setMode(const RecordInterface& in) {

    // interval of interest
    if (in.isDefined(String("interval"))) {
	RecordFieldId fld("interval");
	if (in.type(in.idToNumber(fld)) == TpArrayDouble   ||
	    in.type(in.idToNumber(fld)) == TpArrayComplex  ||
	    in.type(in.idToNumber(fld)) == TpArrayDComplex ||
	    in.type(in.idToNumber(fld)) == TpArrayFloat    ||
	    in.type(in.idToNumber(fld)) == TpArrayInt)
	{
	    Vector<T> intv;
	    in.get(fld, intv);
	    if (intv(0) < intv(1)) 
		this->setInterval(intv(0), intv(1));
	    else 
		this->setInterval(intv(0), intv(1));
	}
    }

    // default value
    if (in.isDefined(String("default"))) {
	RecordFieldId fld("default");
	if (in.type(in.idToNumber(fld)) == TpDouble   ||
	    in.type(in.idToNumber(fld)) == TpComplex  ||
	    in.type(in.idToNumber(fld)) == TpDComplex ||
	    in.type(in.idToNumber(fld)) == TpFloat    ||
	    in.type(in.idToNumber(fld)) == TpInt)
	{
	    T def;
	    in.get(fld, def);
	    this->setDefault(def);
	}
    }

    // out-of-interval mode
    if (in.isDefined(String("intervalMode"))) {
	RecordFieldId fld("intervalMode");
	if (in.type(in.idToNumber(fld)) == TpString) {
	    String mode;
	    in.get(fld, mode);
	    uInt match = MUString::minimaxNC(mode, this->modes_s);
	    if (mode.length() > 0 && match < this->modes_s.nelements()) 
		this->setOutOfIntervalMode(static_cast<ChebyshevEnums::OutOfIntervalMode>(match));
	    else 
		throw AipsError(String("Unrecognized intervalMode: ") + mode);
	}
    }
}	    

template <class T>
void ChebyshevParamModeImpl<AutoDiff<T> >::getMode(RecordInterface& out) const 
{
    Vector<T> intv(2);
    intv(0) = this->getIntervalMin().value();
    intv(1) = this->getIntervalMax().value();

    out.define(RecordFieldId("interval"), intv);
    out.define(RecordFieldId("default"), this->getDefault().value());
    out.define(RecordFieldId("intervalMode"), this->modes_s(this->getOutOfIntervalMode()));
}

// specialization for AutoDiffA

template <class T>
void ChebyshevParamModeImpl<AutoDiffA<T> >::setMode(const RecordInterface& in) {

    // interval of interest
    if (in.isDefined(String("interval"))) {
	RecordFieldId fld("interval");
	if (in.type(in.idToNumber(fld)) == TpArrayDouble   ||
	    in.type(in.idToNumber(fld)) == TpArrayComplex  ||
	    in.type(in.idToNumber(fld)) == TpArrayDComplex ||
	    in.type(in.idToNumber(fld)) == TpArrayFloat    ||
	    in.type(in.idToNumber(fld)) == TpArrayInt)
	{
	    Vector<T> intv;
	    in.get(fld, intv);
	    if (intv(0) < intv(1)) 
		this->setInterval(intv(0), intv(1));
	    else 
		this->setInterval(intv(0), intv(1));
	}
    }

    // default value
    if (in.isDefined(String("default"))) {
	RecordFieldId fld("default");
	if (in.type(in.idToNumber(fld)) == TpDouble   ||
	    in.type(in.idToNumber(fld)) == TpComplex  ||
	    in.type(in.idToNumber(fld)) == TpDComplex ||
	    in.type(in.idToNumber(fld)) == TpFloat    ||
	    in.type(in.idToNumber(fld)) == TpInt)
	{
	    T def;
	    in.get(fld, def);
	    this->setDefault(def);
	}
    }

    // out-of-interval mode
    if (in.isDefined(String("intervalMode"))) {
	RecordFieldId fld("intervalMode");
	if (in.type(in.idToNumber(fld)) == TpString) {
	    String mode;
	    in.get(fld, mode);
	    uInt match = MUString::minimaxNC(mode, this->modes_s);
	    if (mode.length() > 0 && match < this->modes_s.nelements()) 
		this->setOutOfIntervalMode(static_cast<ChebyshevEnums::OutOfIntervalMode>(match));
	    else 
		throw AipsError(String("Unrecognized intervalMode: ") + mode);
	}
    }
}	    

template <class T>
void ChebyshevParamModeImpl<AutoDiffA<T> >::getMode(RecordInterface& out) const
{
    Vector<T> intv(2);
    intv(0) = this->getIntervalMin().value();
    intv(1) = this->getIntervalMax().value();

    out.define(RecordFieldId("interval"), intv);
    out.define(RecordFieldId("default"), this->getDefault().value());
    out.define(RecordFieldId("intervalMode"), this->modes_s(this->getOutOfIntervalMode()));
}


} //# NAMESPACE CASACORE - END


#endif
