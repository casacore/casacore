//# FittingProxy:  This class gives  access to fitting
//# Copyright (C) 2006
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

//# Includes

#include <casacore/scimath/Fitting/LSQaips.h>
#include <casacore/scimath/Fitting/LinearFitSVD.h>
#include <casacore/scimath/Fitting/NonLinearFitLM.h>
#include <casacore/scimath/Fitting/GenericL2Fit.h>
#include <casacore/scimath/Fitting/NonLinearFit.h>
#include <casacore/scimath/Functionals/FunctionHolder.h>
#include <casacore/scimath/Functionals/HyperPlane.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayAccessor.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/VectorIter.h>
#include <casacore/casa/Arrays/VectorSTLIterator.h>
#include <casacore/casa/Containers/RecordFieldId.h>
#include <casacore/casa/Exceptions/Error.h>

#include <casacore/scimath/Fitting/FittingProxy.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// FitType
// Constructor
FittingProxy::FitType::FitType() :
  fitter_p(0), fitterCX_p(0),
  n_p(0), nceq_p(0), nreal_p(0), typ_p(0),
  colfac_p(1e-8), lmfac_p(1e-3), 
  soldone_p(false), nr_p(0) {}

FittingProxy::FitType::~FitType() {
  delete fitter_p; fitter_p = 0;
  delete fitterCX_p; fitterCX_p = 0;
}

// Methods
void FittingProxy::FitType::setFitter(GenericL2Fit<double> *ptr) {
  delete fitter_p; fitter_p = 0;
  delete fitterCX_p; fitterCX_p = 0;
  fitter_p = ptr;
}

void FittingProxy::FitType::setFitterCX(GenericL2Fit<DComplex> *ptr) {
  delete fitter_p; fitter_p = 0;
  delete fitterCX_p; fitterCX_p = 0;
  fitterCX_p = ptr;
}

GenericL2Fit<double> *const& FittingProxy::FitType::getFitter() const {
  return fitter_p;
}

GenericL2Fit<DComplex> *const& FittingProxy::FitType::getFitterCX() const {
  return fitterCX_p;
}

void FittingProxy::FitType::setStatus(int32_t n, int32_t typ,
				 double colfac, double lmfac) {
  n_p = n;
  typ_p = typ;
  nceq_p = (typ == 3 || typ == 11) ? 2*n_p : n_p;
  nreal_p = (typ_p != 0) ? 2*n_p : n_p;
  colfac_p = colfac;
  lmfac_p = lmfac;
}

void FittingProxy::FitType::setSolved(bool solved) {
  soldone_p = solved;
}

// FittingProxy
// Constructors
FittingProxy::FittingProxy() :
  nFitter_p(0), list_p(0) {}

// Destructor
FittingProxy::~FittingProxy() {
  for (uint32_t i=0; i<nFitter_p; i++) {
    delete list_p[i]; list_p[i] = 0;
  }
  delete [] list_p;
}

// Methods

int32_t FittingProxy::getid()
{
  int32_t id = -1;
  while (id<0) {
    for (uint32_t i=0; i<nFitter_p; i++) {
      if (!list_p[i]) {
	id = i;
	break;
      }
    }
    // Make some more
    if (id<0) {
      uint32_t n = nFitter_p;
      nFitter_p++;
      nFitter_p *= 2;
      FitType **list = list_p;
      list_p = new FitType *[nFitter_p];
      for (uint32_t i=0; i<nFitter_p; i++) {
	list_p[i] = 0;
	if (i<n) list_p[i] = list[i];
      }
      delete [] list;
    }
  }
  list_p[id] = new FittingProxy::FitType;
  return id;
}

Record FittingProxy::getstate(int32_t id)
{
  Record res;
  if (list_p[id]->getFitter()) {
    res.define(String("n"), list_p[id]->getN());
    res.define(String("typ"), list_p[id]->getType());
    res.define(String("colfac"), list_p[id]->getColfac());
    res.define(String("lmfac"), list_p[id]->getLMfac());
  }      
  return res;
}


bool FittingProxy::init(int32_t id, int32_t n, int32_t tp, double colfac, double lmfac)
{
  // init: init a fitter
  if (tp == 0) {
    if (!list_p[id]->getFitter()) {
      list_p[id]->setFitter(new LinearFitSVD<double>);
    }
    list_p[id]->getFitter()->set(n);
    list_p[id]->getFitter()->set(abs(colfac), abs(lmfac));
  } else {
    if (!list_p[id]->getFitterCX()) {
      list_p[id]->setFitterCX(new LinearFitSVD<DComplex>);
    }
    list_p[id]->getFitterCX()->set(n);
    list_p[id]->getFitterCX()->set(abs(colfac), abs(lmfac));
  }
  list_p[id]->setStatus(n, tp,
			  abs(colfac), abs(lmfac));
  list_p[id]->setSolved(false);
  return true;
}

bool FittingProxy::done(int32_t id)
{
  if (!list_p[id]->getFitter() && !list_p[id]->getFitterCX()) {
    throw(AipsError("Trying to undo a non-existing fitter"));
  }
  list_p[id]->setFitter(0);
  return true;
}

bool FittingProxy::reset(int32_t id)
{
  if (!list_p[id]->getFitter() && !list_p[id]->getFitterCX()) {
    throw(AipsError("Trying to reset a non-existing fitter"));
  }
  if (list_p[id]->getFitter()) list_p[id]->getFitter()->reset();
  else list_p[id]->getFitterCX()->reset();
  list_p[id]->setSolved(false);
  return true;
}

bool FittingProxy::set(int32_t id, int32_t nin, int32_t tpin, double colfac, double lmfac)
{
  if (!list_p[id]->getFitter() && !list_p[id]->getFitterCX()) {
    throw(AipsError("Trying to set properties of non-existing fitter"));
  }
  int32_t n = nin;
  int32_t tp = tpin;
  double cf = colfac;
  double lmf = lmfac;
  if (n == -1)  n  = list_p[id]->getN();
  if (tp== -1)  tp = list_p[id]->getType();
  if (cf < 0)   cf = list_p[id]->getColfac();
  if (lmf< 0)  lmf = list_p[id]->getLMfac();
  if (list_p[id]->getFitter()) {
    list_p[id]->getFitter()->set(n);
    list_p[id]->getFitter()->set(cf, lmf);
  } else {
    list_p[id]->getFitter()->set(n);
    list_p[id]->getFitter()->set(cf, lmf);
  }
  list_p[id]->setStatus(n, tp, cf, lmf);
  list_p[id]->setSolved(false);
  return true;
}

Record FittingProxy::functional(int32_t id, const Record& fnc, 
				const Vector<double>& xval,
				const Vector<double>& yval, 
				const Vector<double>& wt,
				int32_t mxit, const Record& constraint)
{
  
  int32_t rank, deficiency;
  double sd, mu, chi2;
  Vector<double> constr, err, returnval;
  Array<double> covar;
  String errmsg;
  NonLinearFitLM<double> fitter;
  fitter.setMaxIter(mxit);
  fitter.asWeight(true);
  FunctionHolder<double> fnh;
  Function<AutoDiff<double> > *fn(0);
  if (!fnh.getRecord(errmsg, fn, fnc)) throw(AipsError(errmsg));
  fitter.setFunction(*fn);
  if (xval.nelements() != fn->ndim()*yval.nelements()) {
    throw(AipsError("Functional fitter x and y lengths disagree"));
  }

  for (uint32_t i=0; i<constraint.nfields(); ++i) {
    RecordFieldId fid = i;
    if (constraint.type(i) != TpRecord) {
      throw(AipsError("Illegal definition of constraint in addconstraint"));
    }
    const RecordInterface &con = constraint.asRecord(fid);
    if (con.isDefined(String("fnct")) && con.isDefined(String("x")) &&
	con.isDefined(String("y")) &&
	con.type(con.idToNumber(RecordFieldId("fnct"))) == TpRecord &&
	con.type(con.idToNumber(RecordFieldId("x"))) == TpArrayDouble &&
	con.type(con.idToNumber(RecordFieldId("y"))) == TpDouble) {
      Vector<double> x;
      con.get(RecordFieldId("x"), x);
      double y;
      con.get(RecordFieldId("y"), y);
      HyperPlane<AutoDiff<double> > constrFun(x.nelements());
      fitter.addConstraint(constrFun, x, y);
    } else {
      throw(AipsError("Illegal definition of a constraint in addconstraint"));
    }
  }
  IPosition ip2(2, xval.nelements(), fn->ndim());
  if (fn->ndim() > 1) ip2[0] /= fn->ndim();
  Matrix<double> mval(ip2);
  Array<double>::const_iterator cit = xval.begin();
  for (ArrayAccessor<double, Axis<0> > i(mval); i!=i.end(); ++i) {
    for (uint32_t j=0; j<fn->ndim(); ++cit, ++j) i.index<Axis<1> >(j) = *cit;
  }
  if (wt.nelements() == 0 ||
      (wt.nelements() == 1 && yval.nelements() != 1)) {
    returnval = fitter.fit(mval, yval);
  } else {
    returnval = fitter.fit(mval, yval, wt);
  }
  rank = fitter.getRank();
  deficiency = fitter.getDeficiency();
  sd = fitter.getSD();
  mu = fitter.getWeightedSD();
  chi2 = fitter.getChi2();
  constr.resize(returnval.nelements()*fitter.getDeficiency());
  double *conit = constr.data();
  casacore::Vector<double> ctmp(returnval.nelements());
  double *ctit = ctmp.data();
  for (uint32_t i=0; i<fitter.getDeficiency(); ++i) {
    ctmp = fitter.getSVDConstraint(i);
    for (uint32_t j=0; j<returnval.nelements(); ++j) *conit++ = ctit[j];
  }
  covar = fitter.compuCovariance();
  err.resize();
  fitter.getErrors(err);
  list_p[id]->setSolved(true);
  Record out;
  out.define("rank", rank);
  out.define("sd", sd);
  out.define("mu", mu);
  out.define("chi2", chi2);
  out.define("constr", constr);
  out.define("covar", covar);
  out.define("error", err);
  out.define("deficiency", deficiency);
  out.define("sol", returnval);
  return out;
}

Record FittingProxy::linear(int32_t id, const Record& fnc, 
			    const Vector<double>& xval,
			    const Vector<double>& yval, 
			    const Vector<double>& wt,
			    const Record& constraint)
{
  
  int32_t rank, deficiency;
  double sd, mu, chi2;
  Vector<double> constr, err, returnval;
  Array<double> covar;
  String errmsg;
  LinearFitSVD<double> fitter;
  fitter.asWeight(true);
  FunctionHolder<double> fnh;
  Function<AutoDiff<double> > *fn(0);
  if (!fnh.getRecord(errmsg, fn, fnc)) throw(AipsError(errmsg));
  fitter.setFunction(*fn);
  if (xval.nelements() != fn->ndim()*yval.nelements()) {
    throw(AipsError("Linear fitter x and y lengths disagree"));
  }
  for (uint32_t i=0; i<constraint.nfields(); ++i) {
    RecordFieldId fid = i;
    if (constraint.type(i) != TpRecord) {
      throw(AipsError("Illegal definition of constraint in addconstraint"));
    }
    const RecordInterface &con = constraint.asRecord(fid);
    if (con.isDefined(String("fnct")) && con.isDefined(String("x")) &&
	con.isDefined(String("y")) &&
	con.type(con.idToNumber(RecordFieldId("fnct"))) == TpRecord &&
	con.type(con.idToNumber(RecordFieldId("x"))) == TpArrayDouble &&
	con.type(con.idToNumber(RecordFieldId("y"))) == TpDouble) {
      Vector<double> x;
      con.get(RecordFieldId("x"), x);
      double y;
      con.get(RecordFieldId("y"), y);
      HyperPlane<AutoDiff<double> > constrFun(x.nelements());
      fitter.addConstraint(constrFun, x, y);
    } else {
      throw(AipsError("Illegal definition of a constraint in addconstraint"));
    }
  }
  IPosition ip2(2, xval.nelements(), fn->ndim());
  if (fn->ndim() > 1) ip2[0] /= fn->ndim();
  Matrix<double> mval(ip2);
  Array<double>::const_iterator cit = xval.begin();
  for (ArrayAccessor<double, Axis<0> > i(mval); i!=i.end(); ++i) {
    for (uint32_t j=0; j<fn->ndim(); ++cit, ++j) i.index<Axis<1> >(j) = *cit;
  }
  if (wt.nelements() == 0 ||
      (wt.nelements() == 1 && yval.nelements() != 1)) {
    returnval = fitter.fit(mval, yval);
  } else {
    returnval = fitter.fit(mval, yval, wt);
  }
  rank = fitter.getRank();
  deficiency = fitter.getDeficiency();
  sd = fitter.getSD();
  mu = fitter.getWeightedSD();
  chi2 = fitter.getChi2();
  constr.resize(returnval.nelements()*fitter.getDeficiency());
  double *conit = constr.data();
  casacore::Vector<double> ctmp(returnval.nelements());
  for (uint32_t i=0; i<fitter.getDeficiency(); ++i) {
    ctmp = fitter.getSVDConstraint(i);
    double *ctit = ctmp.data();
    for (uint32_t j=0; j<returnval.nelements(); ++j) *conit++ = ctit[j];
  }
  covar = fitter.compuCovariance();
  err.resize();
  fitter.getErrors(err);
  list_p[id]->setSolved(true);
  Record out;
  out.define("rank", rank);
  out.define("sd", sd);
  out.define("mu", mu);
  out.define("chi2", chi2);
  out.define("constr", constr);
  out.define("covar", covar);
  out.define("error", err);
  out.define("deficiency", deficiency);
  out.define("sol", returnval);
  return out;
}

Record FittingProxy::cxfunctional(int32_t id, const Record& fnc, 
				  const Vector<DComplex>& xval,
				  const Vector<DComplex>& yval, 
				  const Vector<DComplex>& wt,
				  int32_t mxit, const Record& constraint)
{
  
  int32_t rank, deficiency;
  double sd, mu, chi2;
  Vector<DComplex> err, returnval;
  Vector<double> constr;
  Array<DComplex> covar;
  String errmsg;
  NonLinearFitLM<DComplex> fitter;
  fitter.setMaxIter(mxit);
  fitter.asWeight(true);
  FunctionHolder<DComplex> fnh;
  Function<AutoDiff<DComplex> > *fn(0);
  if (!fnh.getRecord(errmsg, fn, fnc)) throw(AipsError(errmsg));
  fitter.setFunction(*fn);
  if (xval.nelements() != fn->ndim()*yval.nelements()) {
    throw(AipsError("Functional fitter x and y lengths disagree"));
  }

  for (uint32_t i=0; i<constraint.nfields(); ++i) {
    RecordFieldId fid = i;
    if (constraint.type(i) != TpRecord) {
      throw(AipsError("Illegal definition of constraint in addconstraint"));
    }
    const RecordInterface &con = constraint.asRecord(fid);
    if (con.isDefined(String("fnct")) && con.isDefined(String("x")) &&
	con.isDefined(String("y")) &&
	con.type(con.idToNumber(RecordFieldId("fnct"))) == TpRecord &&
	con.type(con.idToNumber(RecordFieldId("x"))) == TpArrayDComplex &&
	con.type(con.idToNumber(RecordFieldId("y"))) == TpDComplex) {
      Vector<DComplex> x;
      con.get(RecordFieldId("x"), x);
      DComplex y;
      con.get(RecordFieldId("y"), y);
      HyperPlane<AutoDiff<DComplex> > constrFun(x.nelements());
      fitter.addConstraint(constrFun, x, y);
    } else {
      throw(AipsError("Illegal definition of a constraint in addconstraint"));
    }
  }
  IPosition ip2(2, xval.nelements(), fn->ndim());
  if (fn->ndim() > 1) ip2[0] /= fn->ndim();
  Matrix<DComplex> mval(ip2);
  Array<DComplex>::const_iterator cit = xval.begin();
  for (ArrayAccessor<DComplex, Axis<0> > i(mval); i!=i.end(); ++i) {
    for (uint32_t j=0; j<fn->ndim(); ++cit, ++j) i.index<Axis<1> >(j) = *cit;
  }
  if (wt.nelements() == 0 ||
      (wt.nelements() == 1 && yval.nelements() != 1)) {
    returnval = fitter.fit(mval, yval);
  } else {
    returnval = fitter.fit(mval, yval, wt);
  }
  rank = fitter.getRank();
  deficiency = fitter.getDeficiency();
  sd = fitter.getSD();
  mu = fitter.getWeightedSD();
  chi2 = fitter.getChi2();
  constr.resize(returnval.nelements()*fitter.getDeficiency());
  double *conit = constr.data();
  casacore::Vector<double> ctmp(returnval.nelements());
  double *ctit = ctmp.data();
  for (uint32_t i=0; i<fitter.getDeficiency(); ++i) {
    ctmp = fitter.getSVDConstraint(i);
    for (uint32_t j=0; j<returnval.nelements(); ++j) *conit++ = ctit[j];
  }
  fitter.getCovariance(covar);
  err.resize();
  fitter.getErrors(err);
  list_p[id]->setSolved(true);
  Record out;
  out.define("rank", rank);
  out.define("sd", sd);
  out.define("mu", mu);
  out.define("chi2", chi2);
  out.define("constr", constr);
  out.define("covar", covar);
  out.define("error", err);
  out.define("deficiency", deficiency);
  out.define("sol", returnval);
  return out;
}

Record FittingProxy::cxlinear(int32_t id, const Record& fnc, 
			      const Vector<DComplex>& xval,
			      const Vector<DComplex>& yval, 
			      const Vector<DComplex>& wt,
			      const Record& constraint)
{
  
  int32_t rank, deficiency;
  double sd, mu, chi2;
  Vector<DComplex> err, returnval;
  Vector<double> constr;
  Array<DComplex> covar;
  String errmsg;
  LinearFitSVD<DComplex> fitter;
  fitter.asWeight(true);
  FunctionHolder<DComplex> fnh;
  Function<AutoDiff<DComplex> > *fn(0);
  if (!fnh.getRecord(errmsg, fn, fnc)) throw(AipsError(errmsg));
  fitter.setFunction(*fn);
  if (xval.nelements() != fn->ndim()*yval.nelements()) {
    throw(AipsError("Linear fitter x and y lengths disagree"));
  }

  for (uint32_t i=0; i<constraint.nfields(); ++i) {
    RecordFieldId fid = i;
    if (constraint.type(i) != TpRecord) {
      throw(AipsError("Illegal definition of constraint in addconstraint"));
    }
    const RecordInterface &con = constraint.asRecord(fid);
    if (con.isDefined(String("fnct")) && con.isDefined(String("x")) &&
	con.isDefined(String("y")) &&
	con.type(con.idToNumber(RecordFieldId("fnct"))) == TpRecord &&
	con.type(con.idToNumber(RecordFieldId("x"))) == TpArrayDComplex &&
	con.type(con.idToNumber(RecordFieldId("y"))) == TpDComplex) {
      Vector<DComplex> x;
      con.get(RecordFieldId("x"), x);
      DComplex y;
      con.get(RecordFieldId("y"), y);
      HyperPlane<AutoDiff<DComplex> > constrFun(x.nelements());
      fitter.addConstraint(constrFun, x, y);
    } else {
      throw(AipsError("Illegal definition of a constraint in addconstraint"));
    }
  }
  IPosition ip2(2, xval.nelements(), fn->ndim());
  if (fn->ndim() > 1) ip2[0] /= fn->ndim();
  Matrix<DComplex> mval(ip2);
  Array<DComplex>::const_iterator cit = xval.begin();
  for (ArrayAccessor<DComplex, Axis<0> > i(mval); i!=i.end(); ++i) {
    for (uint32_t j=0; j<fn->ndim(); ++cit, ++j) i.index<Axis<1> >(j) = *cit;
  }
  if (wt.nelements() == 0 ||
      (wt.nelements() == 1 && yval.nelements() != 1)) {
    returnval = fitter.fit(mval, yval);
  } else {
    returnval = fitter.fit(mval, yval, wt);
  }
  rank = fitter.getRank();
  deficiency = fitter.getDeficiency();
  sd = fitter.getSD();
  mu = fitter.getWeightedSD();
  chi2 = fitter.getChi2();
  constr.resize(returnval.nelements()*fitter.getDeficiency());
  double *conit = constr.data();
  casacore::Vector<double> ctmp(returnval.nelements());
  for (uint32_t i=0; i<fitter.getDeficiency(); ++i) {
    ctmp = fitter.getSVDConstraint(i);
    double *ctit = ctmp.data();
    for (uint32_t j=0; j<returnval.nelements(); ++j) *conit++ = ctit[j];
  }

  fitter.getCovariance(covar);  
  err.resize();
  fitter.getErrors(err);
  list_p[id]->setSolved(true);
  Record out;
  out.define("rank", rank);
  out.define("sd", sd);
  out.define("mu", mu);
  out.define("chi2", chi2);
  out.define("constr", constr);
  out.define("covar", covar);
  out.define("error", err);
  out.define("deficiency", deficiency);
  out.define("sol", returnval);
  return out;
}

} //# NAMESPACE CASACORE - END
