//# MeasConvert.cc:  Conversion of Measures
//# Copyright (C) 1995,1996,1997,1998
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
#ifdef __GNUG__
#include <aips/Measures/Quantum.h>
typedef Quantum<Double> gpp_MeasConvert_bug1;
#endif
#include <aips/Exceptions/Error.h>
#include <aips/Measures/MeasConvert.h>

//# Constructors
template<class M, class F, class MC>
MeasConvert<M,F,MC>::MeasConvert() :
  model(0), unit(), outref(), 
  offin(0), offout(0), crout(0), cvdat(0), lres(0), locres(0) {
    init();
}

template<class M, class F, class MC>
MeasConvert<M,F,MC>::MeasConvert(const MeasConvert<M,F,MC> &other) :
  model(0), unit(), outref(),
  offin(0), offout(0), crout(0), cvdat(0), lres(0), locres(0) {
    init();
    copy(other);
  }

template<class M, class F, class MC>
MeasConvert<M,F,MC> &MeasConvert<M,F,MC>::operator=(const MeasConvert<M,F,MC> &other) {
  if (this != &other) {
    copy(other);
  };
  return *this;
}

template<class M, class F, class MC>
MeasConvert<M,F,MC>::MeasConvert(const M &ep) :
  model(0), unit(ep.unit), outref(),
  offin(0), offout(0), crout(0), cvdat(0), lres(0), locres(0) {
    init();
    model = new M(ep);
    create();
  }

template<class M, class F, class MC>
MeasConvert<M,F,MC>::MeasConvert(const M &ep, const MeasRef<M> &mr) :
  model(0), unit(ep.unit), outref(),
  offin(0), offout(0), crout(0), cvdat(0), lres(0), locres(0) {
    init();
    model = new M(ep);
    outref = mr;
    create();
  }

template<class M, class F, class MC>
MeasConvert<M,F,MC>::MeasConvert(const Measure &ep, const MeasRef<M> &mr) :
  model(0), unit(ep.getUnit()), outref(),
  offin(0), offout(0), crout(0), cvdat(0), lres(0), locres(0) {
    init();
    model = ep.clone();
    outref = mr;
    create();
  }

template<class M, class F, class MC>
MeasConvert<M,F,MC>::MeasConvert(const M &ep, uInt mr) :
  model(0), unit(ep.unit), outref(),
  offin(0), offout(0), crout(0), cvdat(0), lres(0), locres(0) {
    init();
    model = new M(ep);
    outref = MeasRef<M>(mr);
    create();
  }

template<class M, class F, class MC>
MeasConvert<M,F,MC>::MeasConvert(const MeasRef<M> &mrin,
				 const MeasRef<M> &mr) :
  model(0), unit(), outref(),
  offin(0), offout(0), crout(0), cvdat(0), lres(0), locres(0) {
    init();
    model = new M(F(), mrin);
    outref = mr;
    create();
  }

template<class M, class F, class MC>
MeasConvert<M,F,MC>::MeasConvert(const MeasRef<M> &mrin,
				 uInt mr) :
  model(0), unit(), outref(),
  offin(0), offout(0), crout(0), cvdat(0), lres(0), locres(0) {
    init();
    model = new M(F(), mrin);
    outref = MeasRef<M>(mr);
    create();
  }

template<class M, class F, class MC>
MeasConvert<M,F,MC>::MeasConvert(uInt mrin,
				 const MeasRef<M> &mr) :
  model(0), unit(), outref(),
  offin(0), offout(0), crout(0), cvdat(0), lres(0), locres(0) {
    init();
    model = new M(F(), MeasRef<M>(mrin));
    outref = mr;
    create();
  }

template<class M, class F, class MC>
MeasConvert<M,F,MC>::MeasConvert(uInt mrin,
				 uInt mr) :
  model(0), unit(), outref(),
  offin(0), offout(0), crout(0), cvdat(0), lres(0), locres(0) {
    init();
    model = new M(F(), MeasRef<M>(mrin));
    outref = MeasRef<M>(mr);
    create();
  }

template<class M, class F, class MC>
MeasConvert<M,F,MC>::MeasConvert(const Unit &inunit, const MeasRef<M> &mrin,
				 const MeasRef<M> &mr) :
  model(0), unit(inunit), outref(),
  offin(0), offout(0), crout(0), cvdat(0), lres(0), locres(0) {
    init();
    model = new M( F(), mrin);
    outref = mr;
    create();
  }

template<class M, class F, class MC>
MeasConvert<M,F,MC>::MeasConvert(const Unit &inunit, const MeasRef<M> &mrin,
				 uInt mr) :
  model(0), unit(inunit), outref(),
  offin(0), offout(0), crout(0), cvdat(0), lres(0), locres(0) {
    init();
    model = new M( F(), mrin);
    outref = MeasRef<M>(mr);
    create();
  }

template<class M, class F, class MC>
MeasConvert<M,F,MC>::MeasConvert(const Unit &inunit, uInt mrin,
				 const MeasRef<M> &mr) :
  model(0), unit(inunit), outref(),
  offin(0), offout(0), crout(0), cvdat(0), lres(0), locres(0) {
    init();
    model = new M( F(), MeasRef<M>(mrin));
    outref = mr;
    create();
  }

template<class M, class F, class MC>
MeasConvert<M,F,MC>::MeasConvert(const Unit &inunit, uInt mrin,
				 uInt mr) :
  model(0), unit(inunit), outref(),
  offin(0), offout(0), crout(0), cvdat(0), lres(0), locres(0) {
    init();
    model = new M( F(), MeasRef<M>(mrin));
    outref = MeasRef<M>(mr);
    create();
  }

//# Destructor
template<class M, class F, class MC>
MeasConvert<M,F,MC>::~MeasConvert() {
  clear();
}

//# Operators
template<class M, class F, class MC>
const M &MeasConvert<M,F,MC>::operator()() {
  return operator()(*(F*)(model->getData()));
}

template<class M, class F, class MC>
const M &MeasConvert<M,F,MC>::operator()(Double val) {
  if (unit.empty()) {
    *locres = F(val);
  } else {
    *locres = F(Quantity(val,unit));
  };
  return operator()(*locres);
}

template<class M, class F, class MC>
const M &MeasConvert<M,F,MC>::operator()(const Vector<Double> &val) {
  if (unit.empty()) {
    *locres = F(val);
  } else {
    *locres = F(Quantum<Vector<Double> >(val,unit));
  };
  return operator()(*locres);
}

template<class M, class F, class MC>
const M &MeasConvert<M,F,MC>::operator()(const Quantum<Double> &val) {
  unit = val.getUnit();
  *locres = F(val);
  return operator()(*locres);
}

template<class M, class F, class MC>
const M &MeasConvert<M,F,MC>::operator()(const Quantum<Vector<Double> > &val) {
  unit = val.getUnit();
  *locres = F(val);
  return operator()(*locres);
}

template<class M, class F, class MC>
const M &MeasConvert<M,F,MC>::operator()(const F &val) {
  *locres = convert(val);
  if (offout) {
    *locres -= *offout;
  }
  lres++; lres %= 4;
  *(result[lres]) = M(*locres,outref);
  return (*(result[lres]));
}

template<class M, class F, class MC>
const M &MeasConvert<M,F,MC>::operator()(const M &val) {
  setModel(val);
  return operator()(*(F*)(model->getData()));
}

template<class M, class F, class MC>
const M &MeasConvert<M,F,MC>::operator()(const M &val, const MeasRef<M> &mr) {
  set(val,mr);
  return operator()(*(F*)(model->getData()));
}

template<class M, class F, class MC>
const M &MeasConvert<M,F,MC>::operator()(const M &val, uInt mr) {
  set(val,mr);
  return operator()(*(F*)(model->getData()));
}

template<class M, class F, class MC>
const M &MeasConvert<M,F,MC>::operator()(const MeasRef<M> &mr) {
  setOut(mr);
  return operator()(*(F*)(model->getData()));
}

template<class M, class F, class MC>
const M &MeasConvert<M,F,MC>::operator()(uInt mr) {
  setOut(mr);
  return operator()(*(F*)(model->getData()));
}

//# Member functions
template<class M, class F, class MC>
void MeasConvert<M,F,MC>::init() {
  cvdat = new MC();
  for (Int i=0; i<4; i++) {
    result[i] = new M();
  };
  
  locres = new F();
}

template<class M, class F, class MC>
void MeasConvert<M,F,MC>::clear() {
  delete model; model = 0;
  unit = Unit();
  outref = MeasRef<M>();
  crout.resize(0, True);
  delete cvdat; cvdat = 0;
  delete offin; offin = 0;
  delete offout; offout = 0;
  delete locres; locres = 0;
  for (Int j=0; j < 4; j++) {
    delete result[j]; result[j] = 0;
  };
}

template<class M, class F, class MC>
void MeasConvert<M,F,MC>::copy(const MeasConvert<M,F,MC> &other) {
  clear();
  init();
  if (other.model)
    model = new M(other.model);
  unit = other.unit;
  outref = other.outref;
  create();
}

template<class M, class F, class MC>
void MeasConvert<M,F,MC>::addMethod(uInt method) {
  crout.resize(crout.nelements() + 1);
  crout[crout.nelements() - 1] = method;
}

template<class M, class F, class MC>
Int MeasConvert<M,F,MC>::nMethod() const {
  return crout.nelements();
}

template<class M, class F, class MC>
uInt MeasConvert<M,F,MC>::getMethod(uInt which) const {
  return crout[which];
}

template<class M, class F, class MC>
void MeasConvert<M,F,MC>::create() {
  delete offin; offin = 0;
  if (model && model->getRefPtr()->offset()) {
    F *ptmp = (F *)(model->getRefPtr()->offset()->getData());
    // Next due to compiler error (gcc)
    MRBase *rptmp(model->getRefPtr());
    uInt tptmp = rptmp->getType();
    MeasFrame mftmp = rptmp->getFrame();
    MeasRef<M> rtmp(tptmp, mftmp);
    MeasRef<M> mrtmp(*(MeasRef<M>*)(model->getRefPtr()->
				    offset()->getRefPtr()));
    if (!mrtmp.empty()) {
      M mtmp(*ptmp, mrtmp);
      offin = new F(MeasConvert<M,F,MC>(mtmp, rtmp).convert());
    } else {
      offin = new F(*ptmp);
    }
  }
  delete offout; offout = 0;
  if (outref.offset()) {
    F *ptmp = (F *)(outref.offset()->getData());
    MeasRef<M> rtmp(outref.getType(), outref.getFrame());
    MeasRef<M> mrtmp(*(MeasRef<M> *)(outref.offset()->getRefPtr()));
    if (!mrtmp.empty()) {
      M mtmp(*ptmp, mrtmp);
      offout = new F(MeasConvert<M,F,MC>(mtmp, rtmp).convert());
    } else {
      offout = new F(*ptmp);
    }
  }
  crout.resize(0, True);
  // Make sure a reference given
  if (model && model->getRefPtr()->empty()) {
    ((MeasBase<F, MeasRef<M> > *)model)->set(MeasRef<M>(M::DEFAULT));
  };
  if (model && 
      !(model->getRefPtr()->empty()) && !(outref.empty())) {
    // Next due to compiler error (gcc)
    MRBase *rptmp(model->getRefPtr());
    MeasFrame mftmp = rptmp->getFrame();
    if (!(mftmp.empty()) &&
	!(outref.getFrame().empty()) &&
	mftmp != outref.getFrame()) {
      MRBase *reftmp = new MeasRef<M>(M::DEFAULT);
      cvdat->getConvert(*this, *model->getRefPtr(), 
			*reftmp);
      cvdat->getConvert(*this, *reftmp,
			outref);
      delete reftmp;
    }else {
      cvdat->getConvert(*this, *model->getRefPtr(), outref);
    };
  };
}

template<class M, class F, class MC>
const F &MeasConvert<M,F,MC>::convert() {
  return convert(*(F*)(model->getData()));
}

template<class M, class F, class MC>
const F &MeasConvert<M,F,MC>::convert(const F &val) {
  *locres = val;
  if (offin) {
    *locres += *offin;
  }
  cvdat->doConvert(*locres, *model->getRefPtr(), outref, *this);
  return *locres;
}

template<class M, class F, class MC>
void MeasConvert<M,F,MC>::setModel(const Measure &val) {
  delete model; model = 0;
  model = new M(&val);
  unit = val.getUnit();
  create();
}

template<class M, class F, class MC>
void MeasConvert<M,F,MC>::setOut(const MeasRef<M> &mr) {
  outref = mr;
  create();
}

template<class M, class F, class MC>
void MeasConvert<M,F,MC>::setOut(uInt mr) {
  outref = MeasRef<M>(mr);
  create();
}

template<class M, class F, class MC>
void MeasConvert<M,F,MC>::set(const M &val, const MeasRef<M> &mr) {
  delete model; model = 0;
  model = new M(val);
  unit = val.unit;
  outref = mr;
  create();
}

template<class M, class F, class MC>
void MeasConvert<M,F,MC>::set(const M &val, uInt mr) {
  delete model; model = 0;
  model = new M(val);
  unit = val.unit;
  outref = MeasRef<M>(mr);
  create();
}

template<class M, class F, class MC>
void MeasConvert<M,F,MC>::set(const MeasValue &val) {
  if (model) {
    model->set(val);
  } else {
    model = new M(&val);
    create();
  };
}

template<class M, class F, class MC>
void MeasConvert<M,F,MC>::set(const Unit &inunit) {
  unit = inunit;
}    

template<class M, class F, class MC>
void MeasConvert<M,F,MC>::print(ostream &os) const {
  os << "Converter with";
  if (model) os << " Template Measure" << *model;
  if (!outref.empty()) os << " Output reference" << outref;
}
