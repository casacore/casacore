//# SpectralFit.cc: Least Squares fitting of spectral elements to spectrum
//# Copyright (C) 2001,2002
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

//# Includes
#include <components/SpectralComponents/SpectralFit.h>
#include <components/SpectralComponents/SpectralElement.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Constructors
SpectralFit::SpectralFit() :
  slist_p(0), iter_p(0) {}

SpectralFit::SpectralFit(const SpectralList &in) :
  slist_p(in), iter_p(0) {}

SpectralFit::SpectralFit(const SpectralFit &other) :
  slist_p(other.slist_p), iter_p(other.iter_p), chiSq_p(other.chiSq_p) {}

SpectralFit::~SpectralFit() {}

SpectralFit &SpectralFit::operator=(const SpectralFit &other) {
  if (this != &other) {
    slist_p = other.slist_p;
    iter_p = other.iter_p;
    chiSq_p = other.chiSq_p;
  };
  return *this;
}

void SpectralFit::setFitElement(uInt index, const SpectralElement &elem) {
  slist_p.set(elem, index);
  iter_p = 0;
}

void SpectralFit::addFitElement(const SpectralElement &elem) {
  slist_p.add(elem);
  iter_p = 0;
}

void SpectralFit::addFitElement(const SpectralList &elem) {
  slist_p.add(elem);
  iter_p = 0;
}

void SpectralFit::clear() {
  slist_p.clear();
  iter_p = 0;
}

} //# NAMESPACE CASA - END



//# Cater for Double and Float profiles
#ifdef AIPS_NO_TEMPLATE_SRC
#include <components/SpectralComponents/SpectralFit2.tcc>

namespace casa { //# NAMESPACE CASA - BEGIN
template Bool SpectralFit::fit<Double>(Vector<Double> const &,
			       Vector<Double> const &,
			       const Vector<Bool> *);
template Bool SpectralFit::fit<Float>(Vector<Float> const &,
			       Vector<Float> const &,
			       const Vector<Bool> *);
template Bool SpectralFit::fit<Float>(Vector<Float> const &,
			       Vector<Float> const &,
			       Vector<Bool> const &);
template Bool SpectralFit::fit<Double>(Vector<Double> const &,
			       Vector<Double> const &,
			       Vector<Double> const &,
			       const Vector<Bool> *);
template Bool SpectralFit::fit<Float>(Vector<Float> const &,
			       Vector<Float> const &,
			       Vector<Float> const &,
			       const Vector<Bool> *);
template Bool SpectralFit::fit<Float>(Vector<Float> const &,
			       Vector<Float> const &,
			       Vector<Float> const &,
			       Vector<Bool> const &);
} //# NAMESPACE CASA - END
#endif
