//# SpectralElement.cc: Describes (a set of related) spectral lines
//# Copyright (C) 2001
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
#include <trial/Wnbt/SpectralElement.h>
#include <aips/Exceptions/Error.h>
#include <aips/Utilities/String.h>
#include <aips/Quanta/MUString.h>
#include <aips/iostream.h>

//# Constructors
SpectralElement::SpectralElement() :
  tp_p(SpectralElement::GAUSSIAN), n_p(0),
  ampl_p(1.0), center_p(0.0), sigma_p(1.0) {}

/// Check the type and the values; add a (type, vector one)
SpectralElement::SpectralElement(SpectralElement::Types tp, const Double ampl,
				 const Double center, const Double sigma) :
  tp_p(tp), n_p(0),
  ampl_p(ampl), center_p(center), sigma_p(sigma) {
  check();
}

SpectralElement::SpectralElement(const uInt n) :
  tp_p(SpectralElement::POLYNOMIAL), n_p(n),
  ampl_p(1.0), center_p(0.0), sigma_p(1.0) {}

SpectralElement::SpectralElement(const SpectralElement &other) :
  tp_p(other.tp_p), n_p(other.n_p),
  ampl_p(other.ampl_p), center_p(other.center_p), sigma_p(other.sigma_p) {
  check();
}

SpectralElement::~SpectralElement() {};

SpectralElement &SpectralElement::operator=(const SpectralElement &other) {
  if (this != &other) {
    tp_p = other.tp_p;
    n_p = other.n_p;
    ampl_p = other.ampl_p;
    center_p = other.center_p;
    sigma_p = other.sigma_p;
    check();
  };
  return *this;
}

const String *const SpectralElement::allTypes(Int &nall,
					      const SpectralElement::Types
					      *&typ) {
  static const String tname[SpectralElement::N_Types] = {
    String("GAUSSIAN"),
    String("POLYNOMIAL") };

  static const SpectralElement::Types oname[SpectralElement::N_Types] = {
    SpectralElement::GAUSSIAN,
    SpectralElement::POLYNOMIAL };

  nall = SpectralElement::N_Types;
  typ    = oname;
  return tname;
}
 
const String &SpectralElement::fromType(SpectralElement::Types tp) {
  Int nall;
  const SpectralElement::Types *typ;
  const String *const tname = SpectralElement::allTypes(nall, typ);
  
  return tname[tp];
}

Bool SpectralElement::toType(SpectralElement::Types &tp,
			     const String &typname) {
  Int nall;
  const SpectralElement::Types *typ;
  const String *const tname = SpectralElement::allTypes(nall, typ);
  
  // Make sure a value returned
  tp = typ[0];
  Int i = MUString::minimaxNC(typname, SpectralElement::N_Types, tname);
  if (i >= nall) return False;
  tp = typ[i];
  return True;
}

void SpectralElement::set(SpectralElement::Types tp, const Double ampl,
			  const Double center, const Double sigma) {
  tp_p = tp;
  ampl_p = ampl;
  center_p = center;
  sigma_p = sigma;
  check();
}

void SpectralElement::setType(SpectralElement::Types tp) {
  tp_p = tp;
}

void SpectralElement::setAmpl(Double ampl) {
  ampl_p = ampl;
} 

void SpectralElement::setCenter(Double center) {
  center_p = center;
}

void SpectralElement::setSigma(Double sigma) {
  sigma_p = sigma;
  check();
}

void SpectralElement::setDegree(uInt n) {
  n_p = n;
}

void SpectralElement::check() {
  if (tp_p == GAUSSIAN && sigma_p == 0.0) {
    throw(AipsError("An illegal sigma of zero was specified for a"
		    " gaussian SpectralElement"));
  };
}

ostream &operator<<(ostream &os, const SpectralElement &elem) {
  switch (elem.getType()) {
    
  case SpectralElement::GAUSSIAN:
  default:
    os << SpectralElement::fromType((elem.getType())) << " element: " << endl;;
    os << "  Amplitude: " << elem.getAmpl() << endl;
    os << "  Center:    " << elem.getCenter() << endl;
    os << "  Sigma:     " << elem.getSigma() << endl;
    break;
  };

  return os;
}
