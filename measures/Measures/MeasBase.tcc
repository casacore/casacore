//# MeasBase.cc: Base class for all measures
//# Copyright (C) 1995-2002,2007
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

#ifndef MEASURES_MEASBASE_TCC
#define MEASURES_MEASBASE_TCC

//# Includes
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/measures/Measures/MeasBase.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors
template <class Mv, class Mr>
MeasBase<Mv,Mr>::MeasBase() :
  data(), ref(), unit() {}

template <class Mv, class Mr>
MeasBase<Mv,Mr>::MeasBase(const MeasBase<Mv,Mr> &other) :
  Measure(other), data(other.data), ref(other.ref), unit(other.unit) {}

template <class Mv, class Mr>
MeasBase<Mv,Mr> &MeasBase<Mv,Mr>::operator=(const MeasBase<Mv,Mr> &other) {
  if (this != &other) {
    data = other.data;
    ref = other.ref;
    unit = other.unit;
  }
  return *this;
}

template <class Mv, class Mr>
MeasBase<Mv,Mr>::MeasBase(const Mv &dt, const Mr &rf) : 
  data(dt), ref(rf), unit() {}

template <class Mv, class Mr>
MeasBase<Mv,Mr>::MeasBase(const Mv &dt, uInt rf) : 
  data(dt), ref(Mr(rf)), unit() {}

template <class Mv, class Mr>
MeasBase<Mv,Mr>::MeasBase(const Quantity &dt, const Mr &rf) : 
  data(dt), ref(rf), unit(dt.getUnit()) {}

template <class Mv, class Mr>
MeasBase<Mv,Mr>::MeasBase(const Quantity &dt, uInt rf) : 
  data(dt), ref(Mr(rf)), unit(dt.getUnit()) {}

template <class Mv, class Mr>
MeasBase<Mv,Mr>::MeasBase(const Measure *dt) :
  data(*(Mv*)(dt->getData())), ref(*(Mr*)(dt->getRefPtr())),
  unit(dt->getUnit()) {} 

template <class Mv, class Mr>
MeasBase<Mv,Mr>::MeasBase(const Mr &rf) : 
  data(), ref(rf), unit() {}

template <class Mv, class Mr>
MeasBase<Mv,Mr>::MeasBase(const uInt rf) :
  data(), ref(Mr(rf)), unit() {}

//# Destructor
template <class Mv, class Mr>
MeasBase<Mv,Mr>::~MeasBase() {}

//# Operators

//# Member functions
template <class Mv, class Mr>
void MeasBase<Mv,Mr>::clear() {
  data = Mv();
  ref = Mr();
  unit = Unit();
}

template <class Mv, class Mr>
Bool MeasBase<Mv,Mr>::areYou(const String &tp) const {
  return (capitalize(tp) == tellMe());
}

template <class Mv, class Mr>
void MeasBase<Mv,Mr>::assured(const String &tp) const {
  if (capitalize(tp) != tellMe()) {
    throw(AipsError("Illegal Measure type in context: " +
		    tellMe()));
  }
}

template <class Mv, class Mr>
const MeasValue* MeasBase<Mv,Mr>::getData() const {
  return &data;
}

template <class Mv, class Mr>
void MeasBase<Mv,Mr>::set(const Mv &dt) {
  data = dt;
}

template <class Mv, class Mr>
void MeasBase<Mv,Mr>::set(const Mr &rf) {
  ref = rf;
}

template <class Mv, class Mr>
void MeasBase<Mv,Mr>::set(const Mv &dt, const Mr &rf) {
  data = dt;
  ref = rf;
}

template <class Mv, class Mr>
void MeasBase<Mv,Mr>::set(const Unit &inunit) {
  unit = inunit;
}

template <class Mv, class Mr>
void MeasBase<Mv,Mr>::set(const MeasValue &dt) {
  data = dynamic_cast<const Mv &>(dt);
}

template <class Mv, class Mr>
Bool MeasBase<Mv,Mr>::putValue(const Vector<Quantum<Double> > &in) {
  return data.putValue(in);
}

template <class Mv, class Mr>
Mr MeasBase<Mv,Mr>::getRef() const {
  return ref;
}

template <class Mv, class Mr>
const Mv &MeasBase<Mv,Mr>::getValue() const {
  return data;
}

template <class Mv, class Mr>
const Unit &MeasBase<Mv,Mr>::getUnit() const {
  return unit;
}

template <class Mv, class Mr>
MRBase *MeasBase<Mv,Mr>::getRefPtr() const {
  // Throw away const
  return (MRBase *) &ref;
}

template <class Mv, class Mr>
void MeasBase<Mv,Mr>::print(std::ostream &os) const {
  os << tellMe() << ": " << data;
}

} //# NAMESPACE CASACORE - END


#endif
