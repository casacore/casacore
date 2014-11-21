//# FunctionalProxy.cc:  This class gives a common object to functionals
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
//#
//# $Id$

//# Includes
#include <casacore/scimath/Functionals/FunctionalProxy.h>
#include <casacore/casa/Exceptions/Error.h>

#include <casacore/casa/namespace.h>
// Constructors
FunctionalProxy::FunctionalProxy(const Record& rec, Int type) : type_(type)
{
  if (type == 0)
    rec2fhd(rec);
  else
    rec2fhdc(rec);
}

FunctionalProxy::~FunctionalProxy() {}


Record FunctionalProxy::fhd2rec()
{
  Record rec;
  String err;
  if (! fhd_.toRecord(err, rec) )
    throw AipsError(err);
  return rec;
}

Record FunctionalProxy::fhdc2rec()
{
  Record rec;
  String err;
  if (! fhdc_.toRecord(err, rec) )
    throw AipsError(err);
  return rec;
}

uInt FunctionalProxy::ndim() const {
  if (type_ == 0)
    return fhd_.asFunction().ndim();
  else
    return fhdc_.asFunction().ndim();  
}

Record  FunctionalProxy::asrecord()
{
  if (type_ == 0) return fhd2rec();
  else return fhdc2rec();
}

void FunctionalProxy::rec2fhdc(const Record& rec)
{
  String err;
  if (! fhdc_.fromRecord(err, rec) )
    throw AipsError(err);
}

void FunctionalProxy::rec2fhd(const Record& rec)
{
  String err;
  if (! fhd_.fromRecord(err, rec) )
    throw AipsError(err);
}

Vector<Double> FunctionalProxy::f(const Vector<Double>& val)
{
  Int nd=1;
  if (fhd_.asFunction().ndim() != 0) nd = fhd_.asFunction().ndim();
  Vector<Double> out(val.nelements()/nd);
  Vector<Double> in(nd);
  for (uInt i=0; i<val.nelements()/nd; ++i) {
    for (Int j=0; j<nd; ++j) in[j] = val[i*nd+j];
    out[i] = fhd_.asFunction()(in);
  }
  return out;
}

Vector<Double> FunctionalProxy::fdf(const Vector<Double>& val)
{
  String errmsg;
  // this is a workaround until I understand AutoDiff
  FunctionHolder<Double> fnh;
  Record rec = fhd2rec();
  Function<AutoDiff<Double> > *fn(0);
  if (!fnh.getRecord(errmsg, fn, rec))
    throw(AipsError(errmsg));
  //
  Int nd=1;
  if (fn->ndim() != 0) nd = fn->ndim();
  Vector<Double> out(val.nelements()/nd *
		     (fn->nparameters()+1));
  Vector<Double> in(nd);
  for (uInt i=0; i<val.nelements()/nd; ++i) {
    for (Int j=0; j<nd; ++j) in[j] = val[i*nd+j];
    AutoDiff<Double> res = (*fn)(in);
    out[i] = res.value();
    for (uInt k=0; k<fn->nparameters(); ++k) {
      out[(k+1)*val.nelements()/nd+i] = res.deriv(k);
    }
  }
  return out;
}

void FunctionalProxy::add(const FunctionalProxy& func)
{
  if (!fhd_.addFunction(func.fhd_.asFunction())) {
    throw(AipsError("Cannot add Function"));
  }
}

Vector<DComplex> FunctionalProxy::fc(const Vector<DComplex>& val)
{
  Int nd=1;
  if (fhdc_.asFunction().ndim() != 0) nd = fhdc_.asFunction().ndim();
  Vector<DComplex> out(val.nelements()/nd);
  Vector<DComplex> in(nd);
  for (uInt i=0; i<val.nelements()/nd; ++i) {
    for (Int j=0; j<nd; ++j) in[j] = val[i*nd+j];
    out[i] = fhdc_.asFunction()(in);
  }
  return out;
}


Vector<DComplex> FunctionalProxy::fdfc(const Vector<Double>& val)
{
  String errmsg;
  // this is a workaround until I understand AutoDiff
  FunctionHolder<DComplex> fnh;
  Record rec = fhd2rec();
  Function<AutoDiff<DComplex> > *fn(0);
  if (!fnh.getRecord(errmsg, fn, rec))
    throw(AipsError(errmsg));
  //
  Int nd=1;
  if (fn->ndim() != 0) nd = fn->ndim();
  Vector<DComplex> out(val.nelements()/nd *
		       (fn->nparameters()+1));
  Vector<DComplex> in(nd);
  for (uInt i=0; i<val.nelements()/nd; ++i) {
    for (Int j=0; j<nd; ++j) in[j] = val[i*nd+j];
    AutoDiff<DComplex> res = (*fn)(in);
    out[i] = res.value();
    for (uInt k=0; k<fn->nparameters(); ++k) {
      out[(k+1)*val.nelements()/nd+i] = res.deriv(k);
    }
  }
  return out;
}

void FunctionalProxy::addc(const FunctionalProxy& func)
			    
{
  if (!fhdc_.addFunction(func.fhdc_.asFunction())) {
    throw(AipsError("Cannot add Function"));
  }
}

Int FunctionalProxy::npar() const {
  if (type_ == 0)
    return fhd_.asFunction().nparameters();
  else
    return fhdc_.asFunction().nparameters();  
}

void FunctionalProxy::setparameters(const Vector<Double>& val)
{
  uInt n = (fhd_.asFunction()).nparameters();
  if (val.nelements() != n)
    throw(AipsError("number of parameters doesn't match functional"));

  Record rec = fhd2rec();
  rec.define("params", val);
  rec2fhd(rec);
}
void FunctionalProxy::setparametersc(const Vector<DComplex>& val)
{
  uInt n = (fhdc_.asFunction()).nparameters();
  if (val.nelements() != n)
    throw(AipsError("number of parameters doesn't match functional"));

  Record rec = fhdc2rec();
  rec.define("params", val);
  rec2fhdc(rec);
}

void FunctionalProxy::setmasks(const Vector<Bool>& val) {
  uInt n;
  if (type_ == 0)
    n = (fhd_.asFunction()).nparameters();
  else
    n = (fhdc_.asFunction()).nparameters();
  
  if (val.nelements() != n)
    throw(AipsError("number of parameters doesn't match functional"));
  
  Record rec;
  if (type_ == 0) {
    rec = fhd2rec();
    rec.define("masks", val);
  } else {
    rec = fhdc2rec();
    rec2fhd(rec);
  }
}

void FunctionalProxy::setmask(Int idx, Bool val)
{
  Int n;
  if (type_ == 0) {
    n = (fhd_.asFunction()).nparameters();
  } else {
    n = (fhdc_.asFunction()).nparameters();
  }
  if (idx < 0 || idx >= n )
    throw(AipsError("mask index out of bounds"));
  
  if (type_ == 0) {
    Record rec = fhd2rec();
    Vector<Bool> v = rec.toArrayBool("masks");
    v[idx] = val;
    rec.define("masks", v);
    rec2fhd(rec);
  } else {
    Record rec = fhdc2rec();
    Vector<Bool> v = rec.toArrayBool("masks");
    v[idx] = val;
    rec.define("masks", v);
    rec2fhdc(rec);
  }
}

void FunctionalProxy::setpar(Int idx, Double val)
{
  Int n = (fhd_.asFunction()).nparameters();
  if (idx < 0 || idx >= n )
    throw(AipsError("parameter index out of bounds"));
  Record rec = fhd2rec();
  Vector<Double> v = rec.toArrayDouble("params");
  v[idx] = val;
  rec.define("params", v);
  rec2fhd(rec);
}
void FunctionalProxy::setparc(Int idx, DComplex val)
{
  Int n = (fhdc_.asFunction()).nparameters();
  if (idx < 0 || idx >= n )
    throw(AipsError("parameter index out of bounds"));
  Record rec = fhdc2rec();
  Vector<DComplex> v = rec.toArrayDComplex("params");
  v[idx] = val;
  rec.define("params", v);
  rec2fhdc(rec);
}

Vector<Bool> FunctionalProxy::masks() const
{
  if (type_ == 0)
    return (fhd_.asFunction()).parameters().getParamMasks();
  else
    return (fhdc_.asFunction()).parameters().getParamMasks();
}

Vector<Double> FunctionalProxy::parameters() const
{  
  return (fhd_.asFunction()).parameters().getParameters() ;
}

Vector<DComplex> FunctionalProxy::parametersc() const
{
  return (fhdc_.asFunction()).parameters().getParameters() ;
}
