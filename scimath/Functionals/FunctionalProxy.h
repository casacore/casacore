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
  // type 0==double, other == DComplex
  FunctionalProxy(const Record& rec, int32_t type=0);
  virtual ~FunctionalProxy();
  Vector<double> f(const Vector<double>& val);
  Vector<double> fdf(const Vector<double>& val);
  void add(const FunctionalProxy& func);
  Vector<DComplex> fc(const Vector<DComplex>& val);
  Vector<DComplex> fdfc(const Vector<double>& val);
  void addc(const FunctionalProxy& func);
  Record asrecord();
  int32_t npar() const;
  uint32_t ndim() const;
  void setparameters(const Vector<double>& val);
  void setparametersc(const Vector<DComplex>& val);
  void setmasks(const Vector<bool>& val);

  void setmask(int32_t i, bool val);
  void setpar(int32_t i, double val);
  void setparc(int32_t i, DComplex val);

  Vector<bool> masks() const;
  Vector<double> parameters() const;
  Vector<DComplex> parametersc() const;

private:
  Record fhd2rec();
  Record fhdc2rec();
  void rec2fhdc(const Record& rec);
  void rec2fhd(const Record& rec);
  int32_t type_;
  FunctionHolder<double> fhd_;
  FunctionHolder<DComplex> fhdc_;


};
}
#endif
