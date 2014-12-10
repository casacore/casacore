//# FunctionFactory.h: a class for creating Function objects from Records
//# Copyright (C) 2002
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
//#
//# $Id$

#ifndef SCIMATH_FUNCTIONALSPROXY_H
#define SCIMATH_FUNCTIONALSPROXY_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/scimath/Functionals/FunctionHolder.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class FunctionalProxy {
public:
  FunctionalProxy() {;}
  // type 0==Double, other == DComplex
  FunctionalProxy(const Record& rec, Int type=0);
  virtual ~FunctionalProxy();
  Vector<Double> f(const Vector<Double>& val);
  Vector<Double> fdf(const Vector<Double>& val);
  void add(const FunctionalProxy& func);
  Vector<DComplex> fc(const Vector<DComplex>& val);
  Vector<DComplex> fdfc(const Vector<Double>& val);
  void addc(const FunctionalProxy& func);
  Record asrecord();
  Int npar() const;
  uInt ndim() const;
  void setparameters(const Vector<Double>& val);
  void setparametersc(const Vector<DComplex>& val);
  void setmasks(const Vector<Bool>& val);

  void setmask(Int i, Bool val);
  void setpar(Int i, Double val);
  void setparc(Int i, DComplex val);

  Vector<Bool> masks() const;
  Vector<Double> parameters() const;
  Vector<DComplex> parametersc() const;

private:
  Record fhd2rec();
  Record fhdc2rec();
  void rec2fhdc(const Record& rec);
  void rec2fhd(const Record& rec);
  Int type_;
  FunctionHolder<Double> fhd_;
  FunctionHolder<DComplex> fhdc_;


};
}
#endif
