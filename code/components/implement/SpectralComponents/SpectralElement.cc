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

#include <aips/Containers/Record.h>
#include <aips/Containers/RecordInterface.h>
#include <aips/Exceptions/Error.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Mathematics/Math.h>
#include <aips/Quanta/MUString.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>

#include <aips/iostream.h>

//# Constructors
SpectralElement::SpectralElement() :
  tp_p(SpectralElement::GAUSSIAN), n_p(0),
  par_p(3) {
  par_p(0) = 1.0;
  par_p(1) = 0.0;
  par_p(2) = 2*sqrt(C::ln2)/C::pi;
}

SpectralElement::SpectralElement(SpectralElement::Types tp, const Double ampl,
				 const Double center, const Double sigma) :
  tp_p(tp), n_p(0),
  par_p(3) {
  if (tp != GAUSSIAN) {
    throw(AipsError("SpectralElement: Only GAUSSIAN can have ampl, "
		    "center and sigma"));
  };
  par_p(0) = ampl;
  par_p(1) = center;
  par_p(2) = sigma;
  check();
}

SpectralElement::SpectralElement(const uInt n) :
  tp_p(SpectralElement::POLYNOMIAL), n_p(n),
  par_p(n+1) {
  par_p = 0;
}

SpectralElement::SpectralElement(SpectralElement::Types tp,
				 const Vector<Double> &param) :
  tp_p(tp), n_p(0),
  par_p(0) {
  if (tp_p == GAUSSIAN) {
    if (param.nelements() != 3) {
      throw(AipsError("SpectralElement: GAUSSIAN must have "
		      "3 parameters"));
    };
    par_p.resize(3);
  } else if (tp_p == POLYNOMIAL) {
    if (param.nelements() == 0) {
      throw(AipsError("SpectralElement: POLYNOMIAL must have "
		      "at least 1 parameter"));
    };
    n_p = param.nelements()-1;
    par_p.resize(n_p+1);
  };
  for (uInt i=0; i<param.nelements(); i++) par_p(i) = param(i);
  check();
}

SpectralElement::SpectralElement(const SpectralElement &other) :
  tp_p(other.tp_p), n_p(other.n_p),
  par_p(0) {
  par_p = other.par_p;
  check();
}

SpectralElement::~SpectralElement() {};

SpectralElement &SpectralElement::operator=(const SpectralElement &other) {
  if (this != &other) {
    tp_p = other.tp_p;
    n_p = other.n_p;
    par_p = other.par_p;
    check();
  };
  return *this;
}

Double SpectralElement::operator()(const Double x) const {
  if (tp_p == GAUSSIAN) {
    return  par_p(0)*exp(-(x-par_p(1))*(x-par_p(1))*4*C::ln2/
			 par_p(2)/par_p(2));
  };
  Double s(0);
  if (tp_p == POLYNOMIAL) {
    for (uInt i=n_p; i<=n_p; i--) {
      s += par_p(i); s *= x;
    };
  };    
  return s;
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

Double SpectralElement::getAmpl() const {
  checkGauss();
  return par_p(0);
};

Double SpectralElement::getCenter() const {
  checkGauss();
  return par_p(1);
};

Double SpectralElement::getSigma() const {
  checkGauss();
  return par_p(2);
};

Double SpectralElement::getFWHM() const {
  checkGauss();
  return sqrt(32.0*C::ln2)*par_p(2);
}

uInt SpectralElement::getDegree() const {
  checkPoly();
  return n_p;
};

void SpectralElement::set(SpectralElement::Types tp,
			  const Vector<Double> &param) {
  tp_p = tp;
  n_p = 0;
  if (tp_p == GAUSSIAN) {
    if (param.nelements() != 3) {
      throw(AipsError("SpectralElement: GAUSSIAN must have "
		      "3 parameters"));
    };
    par_p.resize(3);
  };
  if (tp_p == POLYNOMIAL) {
    if (param.nelements() == 0) {
      throw(AipsError("SpectralElement: POLYNOMIAL must have "
		      "at least 1 parameter"));
    };
    n_p = param.nelements()-1;
    par_p.resize(n_p+1);
  };
  for (uInt i=0; i<param.nelements(); i++) par_p(i) = param(i);
  check();
}

void SpectralElement::setAmpl(Double ampl) {
  checkGauss();
  par_p(0) = ampl;
} 

void SpectralElement::setCenter(Double center) {
  checkGauss();
  par_p(1) = center;
}

void SpectralElement::setSigma(Double sigma) {
  checkGauss();
  par_p(2) = sigma;
  check();
}

void SpectralElement::setDegree(uInt n) {
  checkPoly();
  n_p = n;
  par_p.resize(n_p+1);
  par_p = 0;
}

const String &SpectralElement::ident() const {
  static String myid = "spectrel";
  return myid;
}

void SpectralElement::checkGauss() const {
  if (tp_p != GAUSSIAN) {
    throw(AipsError("SpectralElement: GAUSSIAN element expected"));
  };
}

void SpectralElement::checkPoly() const {
  if (tp_p != POLYNOMIAL) {
    throw(AipsError("SpectralElement: POLYNOMIAL element expected"));
  };
}

void SpectralElement::check() const {
  if (tp_p == GAUSSIAN && par_p(2) <= 0.0) {
    throw(AipsError("SpectralElement: An illegal non-positive sigma was "
		    "specified for a gaussian SpectralElement"));
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
