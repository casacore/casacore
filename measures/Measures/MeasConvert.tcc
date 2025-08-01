//# MeasConvert.cc:  Conversion of Measures
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#ifndef MEASURES_MEASCONVERT_TCC
#define MEASURES_MEASCONVERT_TCC

//# Includes
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/measures/Measures/MeasBase.h>
#include <casacore/measures/Measures/MeasConvert.h>
#include <casacore/measures/Measures/MeasFrame.h>
#include <casacore/measures/Measures/MCBase.h>
#include <casacore/measures/Measures/MRBase.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constructors
template<class M>
MeasConvert<M>::MeasConvert() {
  init();
}

template<class M>
MeasConvert<M>::MeasConvert(const MeasConvert<M> &other) :
    MConvertBase(other) {
  init();
  copy(other);
}

template<class M>
MeasConvert<M> &MeasConvert<M>::operator=(const MeasConvert<M> &other) {
  if (this != &other) {
    copy(other);
  }
  return *this;
}

template<class M>
MeasConvert<M>::MeasConvert(const M &ep) :
    unit(ep.unit) {
  init();
  model = std::make_unique<M>(ep);
  create();
}

template<class M>
MeasConvert<M>::MeasConvert(const M &ep, const typename M::Ref &mr) :
    unit(ep.unit) {
  init();
  model = std::make_unique<M>(ep);
  outref = mr;
  create();
}

template<class M>
MeasConvert<M>::MeasConvert(const Measure &ep, const typename M::Ref &mr) :
    unit(ep.getUnit()) {
  init();
  model.reset(ep.clone());
  outref = mr;
  create();
}

template<class M>
MeasConvert<M>::MeasConvert(const M &ep, typename M::Types mr) :
    unit(ep.unit) {
  init();
  model = std::make_unique<M>(ep);
  outref = typename M::Ref(mr);
  create();
}

template<class M>
MeasConvert<M>::MeasConvert(const Measure &ep, typename M::Types mr) :
    unit(ep.getUnit()) {
  init();
  model.reset(ep.clone());
  outref = typename M::Ref(mr);
  create();
}

template<class M>
MeasConvert<M>::MeasConvert(const typename M::Ref &mrin,
			    const typename M::Ref &mr) {
  init();
  model = std::make_unique<M>(typename M::MVType(), mrin);
  outref = mr;
  create();
}

template<class M>
MeasConvert<M>::MeasConvert(const typename M::Ref &mrin,
			    typename M::Types mr) {
  init();
  model = std::make_unique<M>(typename M::MVType(), mrin);
  outref = typename M::Ref(mr);
  create();
}

template<class M>
MeasConvert<M>::MeasConvert(typename M::Types mrin,
			    const typename M::Ref &mr) {
  init();
  model = std::make_unique<M>(typename M::MVType(), typename M::Ref(mrin));
  outref = mr;
  create();
}

template<class M>
MeasConvert<M>::MeasConvert(typename M::Types mrin,
			    typename M::Types mr) {
  init();
  model = std::make_unique<M>(typename M::MVType(), typename M::Ref(mrin));
  outref = typename M::Ref(mr);
  create();
}

template<class M>
MeasConvert<M>::MeasConvert(const Unit &inunit, const typename M::Ref &mrin,
			    const typename M::Ref &mr) : unit(inunit)
{
  init();
  model = std::make_unique<M>( typename M::MVType(), mrin);
  outref = mr;
  create();
}

template<class M>
MeasConvert<M>::MeasConvert(const Unit &inunit, const typename M::Ref &mrin,
			    typename M::Types mr) : unit(inunit)
{
  init();
  model = std::make_unique<M>( typename M::MVType(), mrin);
  outref = typename M::Ref(mr);
  create();
}

template<class M>
MeasConvert<M>::MeasConvert(const Unit &inunit, typename M::Types mrin,
			    const typename M::Ref &mr) : unit(inunit)
{
  init();
  model = std::make_unique<M>( typename M::MVType(), typename M::Ref(mrin));
  outref = mr;
  create();
}

template<class M>
MeasConvert<M>::MeasConvert(const Unit &inunit, typename M::Types mrin,
			    typename M::Types mr) : unit(inunit)
{
  init();
  model = std::make_unique<M>( typename M::MVType(), typename M::Ref(mrin));
  outref = typename M::Ref(mr);
  create();
}

//# Destructor
template<class M>
MeasConvert<M>::~MeasConvert() {
  clear();
}

//# Operators
template<class M>
const M &MeasConvert<M>::operator()() {
  return operator()(*(typename M::MVType*)(model->getData()));
}

template<class M>
const M &MeasConvert<M>::operator()(Double val) {
  if (unit.empty()) {
    *locres = typename M::MVType(val);
  } else {
    *locres = typename M::MVType(Quantity(val,unit));
  }
  return operator()(*locres);
}

template<class M>
const M &MeasConvert<M>::operator()(const Vector<Double> &val) {
  if (unit.empty()) *locres = typename M::MVType(val);
  else *locres = typename M::MVType(Quantum<Vector<Double> >(val,unit));
  return operator()(*locres);
}

template<class M>
const M &MeasConvert<M>::operator()(const Quantum<Double> &val) {
  unit = val.getUnit();
  *locres = typename M::MVType(val);
  return operator()(*locres);
}

template<class M>
const M &MeasConvert<M>::operator()(const Quantum<Vector<Double> > &val) {
  unit = val.getUnit();
  *locres = typename M::MVType(val);
  return operator()(*locres);
}

template<class M>
const M &MeasConvert<M>::operator()(const typename M::MVType &val) {
  *locres = convert(val);
  if (offout) *locres -= *offout;
  lres++; lres %= 4;
  *(result[lres]) = M(*locres,outref);
  return (*(result[lres]));
}

template<class M>
const M &MeasConvert<M>::operator()(const MeasVal *val) {
  return operator()(*(const typename M::MVType *)(val));
}

template<class M>
const M &MeasConvert<M>::operator()(const M &val) {
  setModel(val);
  return operator()(*(typename M::MVType*)(model->getData()));
}

template<class M>
const M &MeasConvert<M>::operator()(const M &val, const typename M::Ref &mr) {
  set(val,mr);
  return operator()(*(typename M::MVType*)(model->getData()));
}

template<class M>
const M &MeasConvert<M>::operator()(const M &val, typename M::Types mr) {
  set(val,mr);
  return operator()(*(typename M::MVType*)(model->getData()));
}

template<class M>
const M &MeasConvert<M>::operator()(const typename M::Ref &mr) {
  setOut(mr);
  return operator()(*(typename M::MVType*)(model->getData()));
}

template<class M>
const M &MeasConvert<M>::operator()(typename M::Types mr) {
  setOut(mr);
  return operator()(*(typename M::MVType*)(model->getData()));
}

//# Member functions
template<class M>
void MeasConvert<M>::init() {
  cvdat = std::make_unique<typename M::MCType>();
  for (Int i=0; i<4; i++) result[i] = std::make_unique<M>();
  locres = std::make_unique<typename M::MVType>();
}

template<class M>
void MeasConvert<M>::clear() {
  model.reset();
  unit = Unit();
  outref = typename M::Ref();
  crout.clear();
  crtype = 0;
  cvdat->clearConvert();
  cvdat.reset();
  offin.reset();
  offout.reset();
  locres.reset();
  for (Int j=0; j < 4; j++) {
    result[j].reset();
  }
}

template<class M>
void MeasConvert<M>::copy(const MeasConvert<M> &other) {
  clear();
  init();
  if (other.model) model = std::make_unique<M>(other.model.get());
  unit = other.unit;
  outref = other.outref;
  create();
}

template<class M>
void MeasConvert<M>::addMethod(uInt method) {
  crout.push_back(method);
}

template<class M>
void MeasConvert<M>::addFrameType(uInt tp) {
  crtype |= tp;
}

template<class M>
Int MeasConvert<M>::nMethod() const {
  return crout.size();
}

template<class M>
uInt MeasConvert<M>::getMethod(uInt which) const {
  return crout[which];
}

template<class M>
void MeasConvert<M>::create() {
  offin.reset();
  if (model && model->getRefPtr()->offset()) {
    typename M::MVType *ptmp =
      (typename M::MVType *)(model->getRefPtr()->offset()->getData());
    // Next due to compiler error (gcc)
    MRBase *rptmp(model->getRefPtr());
    typename M::Types tptmp = static_cast<typename M::Types>(rptmp->getType());
    MeasFrame mftmp = rptmp->getFrame();
    typename M::Ref rtmp(tptmp, mftmp);
    typename M::Ref mrtmp(*(typename M::Ref*)(model->getRefPtr()->
					      offset()->getRefPtr()));
    if (!mrtmp.empty()) {
      M mtmp(*ptmp, mrtmp);
      offin = std::make_unique<typename M::MVType>(MeasConvert<M>(mtmp, rtmp).convert());
    } else {
      offin = std::make_unique<typename M::MVType>(*ptmp);
    }
  }
  offout.reset();
  if (outref.offset()) {
    typename M::MVType *ptmp =
      (typename M::MVType *)(outref.offset()->getData());
    typename M::Ref rtmp(outref.getType(), outref.getFrame());
    typename M::Ref mrtmp(*(typename M::Ref *)(outref.offset()->getRefPtr()));
    if (!mrtmp.empty()) {
      M mtmp(*ptmp, mrtmp);
      offout = std::make_unique<typename M::MVType>(MeasConvert<M>(mtmp, rtmp).convert());
    } else {
      offout = std::make_unique<typename M::MVType>(*ptmp);
    }
  }
  crout.clear();
  crtype = 0;
  // Make sure a reference given
  if (model && model->getRefPtr()->empty()) {
    ((MeasBase<typename M::MVType, typename M::Ref > *)model.get())
      ->set(typename M::Ref(M::DEFAULT));
  }
  if (outref.empty()) outref = typename M::Ref(M::DEFAULT);
  if (model && !(model->getRefPtr()->empty()) && !(outref.empty())) {
    // Next due to compiler error (gcc)
    MRBase *rptmp(model->getRefPtr());
    const MeasFrame& mftmp = rptmp->getFrame();
    if (!mftmp.empty() && !outref.getFrame().empty() && mftmp != outref.getFrame()) {
      typename M::Ref reftmp(M::DEFAULT);
      cvdat->getConvert(*this, *model->getRefPtr(), reftmp);
      cvdat->getConvert(*this, reftmp, outref);
    } else {
      cvdat->getConvert(*this, *model->getRefPtr(), outref);
    }
  }
}

template<class M>
const typename M::MVType &MeasConvert<M>::convert() {
  return convert(*(typename M::MVType*)(model->getData()));
}

template<class M>
const typename M::MVType &MeasConvert<M>::
convert(const typename M::MVType &val) {
  *locres = val;
  if (offin) *locres += *offin;
  cvdat->doConvert(*locres, *model->getRefPtr(), outref, *this);
  return *locres;
}

template<class M>
void MeasConvert<M>::setModel(const Measure &val) {
  model = std::make_unique<M>(&val);
  unit = val.getUnit();
  create();
}

template<class M>
void MeasConvert<M>::setOut(const typename M::Ref &mr) {
  outref = mr;
  create();
}

template<class M>
void MeasConvert<M>::setOut(typename M::Types mr) {
  outref = typename M::Ref(mr);
  create();
}

template<class M>
void MeasConvert<M>::set(const M &val, const typename M::Ref &mr) {
  model = std::make_unique<M>(val);
  unit = val.unit;
  outref = mr;
  create();
}

template<class M>
void MeasConvert<M>::set(const M &val, typename M::Types mr) {
  model = std::make_unique<M>(val);
  unit = val.unit;
  outref = typename M::Ref(mr);
  create();
}

template<class M>
void MeasConvert<M>::set(const MeasValue &val) {
  if (model) {
    model->set(val);
  } else {
    model = std::make_unique<M>(&val);
    create();
  }
}

template<class M>
void MeasConvert<M>::set(const Unit &inunit) {
  unit = inunit;
}    

template<class M>
void MeasConvert<M>::print(ostream &os) const {
  os << "Converter with";
  if (model) os << " Template Measure" << *model;
  if (!outref.empty()) os << " Output reference" << outref;
}

} //# NAMESPACE CASACORE - END


#endif
