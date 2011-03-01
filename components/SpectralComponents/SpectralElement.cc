//# SpectralElement.cc: Describes (a set of related) spectral lines
//# Copyright (C) 2001,2004
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
#include <components/SpectralComponents/SpectralElement.h>

#include <casa/BasicSL/Constants.h>
#include <casa/BasicSL/String.h>
#include <casa/Exceptions/Error.h>
#include <casa/Utilities/MUString.h>
#include <scimath/Mathematics/AutoDiffMath.h>
#include <scimath/Functionals/CompiledFunction.h>

#include <casa/iostream.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Constants
const Double SpectralElement::SigmaToFWHM = sqrt(8.0*C::ln2);

//# Constructors
SpectralElement::SpectralElement() :
  tp_p(SpectralElement::GAUSSIAN), n_p(0), str_p(),
  par_p(3), err_p(3), fix_p(3) {
  par_p(0) = 1.0;
  par_p(1) = 0.0;
  par_p(2) = 2*sqrt(C::ln2)/C::pi;
  err_p = 0;
  fix_p = False;
}

SpectralElement::SpectralElement(SpectralElement::Types tp, const Double ampl,
				 const Double center, const Double sigma) :
  tp_p(tp), n_p(0), str_p(),
  par_p(3), err_p(3), fix_p(3) {
  if (tp != GAUSSIAN) {
    throw(AipsError("SpectralElement: Only GAUSSIAN can have ampl, "
		    "center and sigma"));
  }
  par_p(0) = ampl;
  par_p(1) = center;
  par_p(2) = abs(sigma);
  err_p = 0;
  fix_p = False;
  check();
}

SpectralElement::SpectralElement(const uInt n) :
  tp_p(SpectralElement::POLYNOMIAL), n_p(n), str_p(),
  par_p(n+1), err_p(n+1), fix_p(n+1) {
  par_p = 0;
  err_p = 0;
  fix_p = False;
}

SpectralElement::SpectralElement(const String &str,
				 const Vector<Double> &param) :
  tp_p(SpectralElement::COMPILED), n_p(0), str_p(str),
  par_p(0), err_p(0), fix_p(0) {
  check();
  par_p = 0;
  err_p = 0;
  fix_p = False;
  if (param.nelements() != par_p.nelements()) {
    throw(AipsError("SpectralElement: COMPILED number of parameters "
		    "disagress with given number of parameters"));
  }
  par_p = param;
}

SpectralElement::SpectralElement(SpectralElement::Types tp,
				 const Vector<Double> &param) :
  tp_p(tp), n_p(0), str_p(),
  par_p(0), err_p(0), fix_p(0) {
  uInt n = 0;
  if (tp_p == GAUSSIAN) {
    if (param.nelements() != 3) {
      throw(AipsError("SpectralElement: GAUSSIAN must have "
		      "3 parameters"));
    }
    n = 3;
  } else if (tp_p == POLYNOMIAL) {
    if (param.nelements() == 0) {
      throw(AipsError("SpectralElement: POLYNOMIAL must have "
		      "at least 1 parameter"));
    }
    n_p = param.nelements()-1;
    n = n_p+1;
  }
  par_p.resize(n);
  err_p.resize(n);
  fix_p.resize(n);
  par_p = param;
  if (tp_p == GAUSSIAN && par_p(2) < 0.0) par_p[2] = -par_p[2];
  err_p = 0;
  fix_p = False;
  check();
}

SpectralElement::SpectralElement(const SpectralElement &other) :
  RecordTransformable(),
  tp_p(other.tp_p), n_p(other.n_p), str_p(other.str_p),
  par_p(0), err_p(0), fix_p(0) {
  par_p = other.par_p;
  if (tp_p == GAUSSIAN && par_p(2) < 0.0) par_p[2] = -par_p[2];
  err_p = other.err_p;
  fix_p = other.fix_p;
  check();
}

SpectralElement::~SpectralElement() {}

SpectralElement &SpectralElement::operator=(const SpectralElement &other) {
  if (this != &other) {
    tp_p = other.tp_p;
    n_p = other.n_p;
    str_p = other.str_p;
    par_p = other.par_p;
    if (tp_p == GAUSSIAN && par_p(2) < 0.0) par_p[2] = -par_p[2];
    err_p = other.err_p;
    fix_p = other.fix_p;
    check();
  }
  return *this;
}

Double SpectralElement::operator()(const Double x) const {
  if (tp_p == GAUSSIAN) {
    return par_p(0)*exp(-0.5 * (x-par_p(1))*(x-par_p(1)) / par_p(2)/par_p(2));
  }
  Double s = par_p(n_p);
  if (tp_p == POLYNOMIAL) {
    for (uInt i=n_p-1; i<n_p; i--) {
      s *= x; s += par_p(i); 
    }
  }
  if (tp_p == COMPILED) {
    CompiledFunction<Double> comp;
    comp.setFunction(str_p);
    comp.parameters().setParameters(par_p);
    s = comp(x);
  }
  return s;
}

Double SpectralElement::operator[](const uInt n) const {
  if (n >= par_p.nelements()) {
      throw(AipsError("SpectralElement: Illegal index for parameter"));
  }
  return par_p(n);
}

const String* SpectralElement::allTypes(Int &nall,
                                        const SpectralElement::Types
                                        *&typ) {
  static const String tname[SpectralElement::N_Types] = {
    String("GAUSSIAN"),
    String("POLYNOMIAL"),
    String("COMPILED") };

  static const SpectralElement::Types oname[SpectralElement::N_Types] = {
    SpectralElement::GAUSSIAN,
    SpectralElement::POLYNOMIAL,
    SpectralElement::COMPILED };

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
}

Double SpectralElement::getCenter() const {
  checkGauss();
  return par_p(1);
}

Double SpectralElement::getSigma() const {
  checkGauss();
  return par_p(2);
}

Double SpectralElement::getFWHM() const {
  checkGauss();
  return sigmaToFWHM(par_p(2));
}

void SpectralElement::get(Vector<Double> &param) const {
  param.resize(par_p.nelements());
  param = par_p;
}

Double SpectralElement::getAmplErr() const {
  checkGauss();
  return err_p(0);
}

Double SpectralElement::getCenterErr() const {
  checkGauss();
  return err_p(1);
}

Double SpectralElement::getSigmaErr() const {
  checkGauss();
  return err_p(2);
}

Double SpectralElement::getFWHMErr() const {
  checkGauss();
  return sigmaToFWHM(err_p(2));
}

void SpectralElement::getError(Vector<Double> &err) const {
  err.resize(err_p.nelements());
  err = err_p;
}

uInt SpectralElement::getDegree() const {
  checkPoly();
  return n_p;
}

const String &SpectralElement::getCompiled() const {
  checkCompiled();
  return str_p;
}


void SpectralElement::setError(const Vector<Double> &err) {
    if (err.nelements() != err_p.nelements()) {
      throw(AipsError("SpectralElement: setting incorrect number of errors "
		      "in the element"));
    }
    err_p = err;
}

void SpectralElement::setAmpl(Double ampl) {
  checkGauss();
  par_p(0) = ampl;
  err_p(0) = 0;
} 

void SpectralElement::setCenter(Double center) {
  checkGauss();
  par_p(1) = center;
  err_p(1) = 0;
}

void SpectralElement::setSigma(Double sigma) {
  checkGauss();
  par_p(2) = sigma;
  if (tp_p == GAUSSIAN && par_p(2) < 0.0) par_p[2] = -par_p[2];
  err_p(2) = 0;
  check();
}

void SpectralElement::setFWHM(Double fwhm) {
  setSigma(sigmaFromFWHM(fwhm));
}

void SpectralElement::setDegree(uInt n) {
  checkPoly();
  n_p = n;
  par_p.resize(n_p+1);
  err_p.resize(n_p+1);
  fix_p.resize(n_p+1);
  par_p = 0;
  err_p = 0;
  fix_p = False;
}

void SpectralElement::setCompiled(const String &str) {
  checkCompiled();
  str_p = str;
  check();
  par_p = 0;
  err_p = 0;
  fix_p = False;
}

void SpectralElement::fixAmpl(const Bool fix) {
  checkGauss();
  fix_p(0) = fix;
}

void SpectralElement::fixCenter(const Bool fix) {
  checkGauss();
  fix_p(1) = fix;
}

void SpectralElement::fixSigma(const Bool fix) {
  checkGauss();
  fix_p(2) = fix;
}

void SpectralElement::fixFWHM(const Bool fix) {
  checkGauss();
  fix_p(2) = fix;
}

void SpectralElement::fix(const Vector<Bool> &fix) {
    if (fix.nelements() != fix_p.nelements()) {
      throw(AipsError("SpectralElement: setting incorrect number of fixed "
		      "in the element"));
    }
    fix_p = fix;
}

Bool SpectralElement::fixedAmpl() const {
  checkGauss();
  return fix_p(0);
}

Bool SpectralElement::fixedCenter() const {
  checkGauss();
  return fix_p(1);
}

Bool SpectralElement::fixedSigma() const {
  checkGauss();
  return fix_p(2);
}

Bool SpectralElement::fixedFWHM() const {
  checkGauss();
  return fix_p(2);
}

const Vector<Bool> &SpectralElement::fixed() const {
  return fix_p;
}

const String &SpectralElement::ident() const {
  static String myid = "spectrel";
  return myid;
}

void SpectralElement::checkGauss() const {
  if (tp_p != GAUSSIAN) {
    throw(AipsError("SpectralElement: GAUSSIAN element expected"));
  }
}

void SpectralElement::checkPoly() const {
  if (tp_p != POLYNOMIAL) {
    throw(AipsError("SpectralElement: POLYNOMIAL element expected"));
  }
}

void SpectralElement::checkCompiled() const {
  if (tp_p != COMPILED) {
    throw(AipsError("SpectralElement: COMPILED element expected"));
  }
}

void SpectralElement::check() {
  if (tp_p == GAUSSIAN && par_p(2) <= 0.0) {
    throw(AipsError("SpectralElement: An illegal zero sigma was "
		    "specified for a Gaussian SpectralElement"));
  }
  if (tp_p == COMPILED) {
    CompiledFunction<Double> comp;
    if (!comp.setFunction(str_p)) {
      throw(AipsError("SpectralElement: An illegal functional string "
		      "was specified for a compiled SpectralElement"));
    }
    par_p.resize(comp.nparameters());
    err_p.resize(comp.nparameters());
    fix_p.resize(comp.nparameters());
  }
}

ostream &operator<<(ostream &os, const SpectralElement &elem) {
  os << SpectralElement::fromType((elem.getType())) << " element: " << endl;

  switch (elem.getType()) {
  case SpectralElement::GAUSSIAN:
    os << "  Amplitude: " << elem.getAmpl() << ", " << elem.getAmplErr();
    if (elem.fixedAmpl()) os << " (fixed)";
    os << endl << "  Center:    " << elem.getCenter() << ", " << elem.getCenterErr();
    if (elem.fixedCenter()) os << " (fixed)";
    os << endl << "  Sigma:     " << elem.getSigma() << ", " << elem.getSigmaErr();
    if (elem.fixedSigma()) os << " (fixed)";
    os << endl;
    break;
  case SpectralElement::POLYNOMIAL:
    os << "  Degree:    " << elem.getDegree() << endl;
  case SpectralElement::COMPILED:
    if (elem.getType() == SpectralElement::COMPILED) {
      os << "  Function:    " << elem.getCompiled() << endl;
    }
  default:
    for (uInt i=0; i<elem.getOrder(); i++) {
      os << "  Parameter " << i << ":" << elem[i];
      if (elem.fixed()(i)) os << " (fixed)";
      os << endl;
    }
    break;
  }

  return os;
}

Double SpectralElement::sigmaToFWHM (const Double sigma) {
   return SigmaToFWHM * sigma;
}

Double SpectralElement::sigmaFromFWHM(const Double fwhm) {
  return fwhm / SigmaToFWHM;
}


} //# NAMESPACE CASA - END


//# Cater for Double and Float
#ifdef AIPS_NO_TEMPLATE_SRC
#include <components/SpectralComponents/Spectral4Element.tcc>

namespace casa { //# NAMESPACE CASA - BEGIN
template void SpectralElement::set<Double>(Vector<Double> const &);
template void SpectralElement::set<Float>(Vector<Float> const &);
template void SpectralElement::set<Double>(SpectralElement::Types tp,
				   Vector<Double> const &);
template void SpectralElement::set<Float>(SpectralElement::Types tp,
				   Vector<Float> const &);
} //# NAMESPACE CASA - END
#endif

