//# MeasConvert.cc:  Conversion of Measures
//# Copyright (C) 1995, 1996
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
typedef Quantum<Double> gpp_measconvert_bug1;
#endif
#include <aips/Exceptions/Error.h>
#include <aips/Utilities/Assert.h>
#include <aips/Measures/MeasConvert.h>

//# Constructors
template<class M, class F>
MeasConvert<M,F>::MeasConvert() :
model(0), unit(), outref(), crout(0), cstruc(MC_N_Struct, (void *) 0),
offin(0), offout(0), lres(0), locres(0) {
    init();
}

template<class M, class F>
MeasConvert<M,F>::MeasConvert(const MeasConvert<M,F> &other) :
model(0), unit(), outref(), crout(0), cstruc(MC_N_Struct, (void *) 0),
offin(0), offout(0), lres(0), locres(0) {
    init();
    copy(other);
}

template<class M, class F>
MeasConvert<M,F> &MeasConvert<M,F>::operator=(const MeasConvert<M,F> &other) {
    init();
    copy(other);
    return *this;
}

template<class M, class F>
MeasConvert<M,F>::MeasConvert(const M &ep) :
model(0), unit(ep.unit), outref(), crout(0), cstruc(MC_N_Struct, (void *) 0),
offin(0), offout(0), lres(0), locres(0) {
    init();
    model = new M(ep);
    create();
}

template<class M, class F>
MeasConvert<M,F>::MeasConvert(const M &ep, const MeasRef<M> &mr) :
model(0), unit(ep.unit), outref(), crout(0), cstruc(MC_N_Struct, (void *) 0),
offin(0), offout(0), lres(0), locres(0) {
    init();
    model = new M(ep);
    outref = mr;
    create();
}

template<class M, class F>
MeasConvert<M,F>::MeasConvert(const M &ep, uInt mr) :
model(0), unit(ep.unit), outref(), crout(0), cstruc(MC_N_Struct, (void *) 0),
offin(0), offout(0), lres(0), locres(0) {
    init();
    model = new M(ep);
    outref = MeasRef<M>(mr);
    create();
}

template<class M, class F>
MeasConvert<M,F>::MeasConvert(const MeasRef<M> &mrin,
			      const MeasRef<M> &mr) :
model(0), unit(), outref(), crout(0), cstruc(MC_N_Struct, (void *) 0),
offin(0), offout(0), lres(0), locres(0) {
    init();
    model = new M(F(), mrin);
    outref = mr;
    create();
}

template<class M, class F>
MeasConvert<M,F>::MeasConvert(const MeasRef<M> &mrin,
			      uInt mr) :
model(0), unit(), outref(), crout(0), cstruc(MC_N_Struct, (void *) 0),
offin(0), offout(0), lres(0), locres(0) {
    init();
    model = new M(F(), mrin);
    outref = MeasRef<M>(mr);
    create();
}

template<class M, class F>
MeasConvert<M,F>::MeasConvert(uInt mrin,
			      const MeasRef<M> &mr) :
model(0), unit(), outref(), crout(0), cstruc(MC_N_Struct, (void *) 0),
offin(0), offout(0), lres(0), locres(0) {
    init();
    model = new M(F(), MeasRef<M>(mrin));
    outref = mr;
    create();
}

template<class M, class F>
MeasConvert<M,F>::MeasConvert(uInt mrin,
			      uInt mr) :
model(0), unit(), outref(), crout(0), cstruc(MC_N_Struct, (void *) 0),
offin(0), offout(0), lres(0), locres(0) {
    init();
    model = new M(F(), MeasRef<M>(mrin));
    outref = MeasRef<M>(mr);
    create();
}

template<class M, class F>
MeasConvert<M,F>::MeasConvert(const Unit &inunit, const MeasRef<M> &mrin,
			      const MeasRef<M> &mr) :
model(0), unit(inunit), outref(), crout(0), cstruc(MC_N_Struct, (void *) 0),
offin(0), offout(0), lres(0), locres(0) {
    init();
    model = new M( F(), mrin);
    outref = mr;
    create();
}

template<class M, class F>
MeasConvert<M,F>::MeasConvert(const Unit &inunit, const MeasRef<M> &mrin,
			      uInt mr) :
model(0), unit(inunit), outref(), crout(0), cstruc(MC_N_Struct, (void *) 0),
offin(0), offout(0), lres(0), locres(0) {
    init();
    model = new M( F(), mrin);
    outref = MeasRef<M>(mr);
    create();
}

template<class M, class F>
MeasConvert<M,F>::MeasConvert(const Unit &inunit, uInt mrin,
			      const MeasRef<M> &mr) :
model(0), unit(inunit), outref(), crout(0), cstruc(MC_N_Struct, (void *) 0),
offin(0), offout(0), lres(0), locres(0) {
    init();
    model = new M( F(), MeasRef<M>(mrin));
    outref = mr;
    create();
}

template<class M, class F>
MeasConvert<M,F>::MeasConvert(const Unit &inunit, uInt mrin,
			      uInt mr) :
model(0), unit(inunit), outref(), crout(0), cstruc(MC_N_Struct, (void *) 0),
offin(0), offout(0), lres(0), locres(0) {
    init();
    model = new M( F(), MeasRef<M>(mrin));
    outref = MeasRef<M>(mr);
    create();
}

//# Destructor
template<class M, class F>
MeasConvert<M,F>::~MeasConvert() {
    clear();
}

//# Operators
template<class M, class F>
const M &MeasConvert<M,F>::operator()() {
    return operator()(model->data);
}

template<class M, class F>
const M &MeasConvert<M,F>::operator()(Double val) {
    if (unit.empty()) {
	*locres = F(val);
    } else {
	*locres = F(Quantity(val,unit));
    };
    return operator()(*locres);
}

template<class M, class F>
const M &MeasConvert<M,F>::operator()(const Vector<Double> &val) {
    if (unit.empty()) {
	*locres = F(val(0));
    } else {
	*locres = F(Quantum<Vector<Double> >(val,unit));
    };
    return operator()(*locres);
}

template<class M, class F>
const M &MeasConvert<M,F>::operator()(const Quantum<Double> &val) {
    unit = val.getUnit();
    *locres = F(val);
    return operator()(*locres);
}

template<class M, class F>
const M &MeasConvert<M,F>::operator()(const Quantum<Vector<Double> > &val) {
    unit = val.getUnit();
    *locres = F(val);
    return operator()(*locres);
}

template<class M, class F>
const M &MeasConvert<M,F>::operator()(const F &val) {
    *locres = convert(val);
    if (offout) {
	*locres -= *offout;
    }
    lres++; lres %= 4;
    *(result[lres]) = M(*locres,outref);
    return (*(result[lres]));
}

template<class M, class F>
const M &MeasConvert<M,F>::operator()(const M &val) {
    setModel(val);
    return operator()(model->data);
}

template<class M, class F>
const M &MeasConvert<M,F>::operator()(const M &val, const MeasRef<M> &mr) {
    set(val,mr);
    return operator()(model->data);
}

template<class M, class F>
const M &MeasConvert<M,F>::operator()(const M &val, uInt mr) {
    set(val,mr);
    return operator()(model->data);
}

template<class M, class F>
const M &MeasConvert<M,F>::operator()(const MeasRef<M> &mr) {
    setOut(mr);
    return operator()(model->data);
}

template<class M, class F>
const M &MeasConvert<M,F>::operator()(uInt mr) {
    setOut(mr);
    return operator()(model->data);
}

//# Member functions
template<class M, class F>
void MeasConvert<M,F>::init() {
    for (Int j=0; j < cstruc.nelements(); j++) {
      cstruc[j] = (void *) 0;
    };
    for (Int i=0; i<4; i++) {
	result[i] = new M();
    };
    locres = new F();
}

template<class M, class F>
void MeasConvert<M,F>::clear() {
    delete model; model = 0;
    unit = Unit();
    outref = MeasRef<M>();
    crout.resize(0, True);
    M::clearConvert(*this);
    for (Int i=0; i < cstruc.nelements(); i++) {
	cstruc[i] = (void *) 0;
    };
    delete offin; offin = 0;
    delete offout; offout = 0;
    delete locres; locres = 0;
    for (Int j=0; j < 4; j++) {
	delete result[j]; result[j] = 0;
    };
}

template<class M, class F>
void MeasConvert<M,F>::copy(const MeasConvert<M,F> &other) {
    clear();
    init();
    model = new M(*(other.model));
    unit = other.unit;
    outref = other.outref;
    create();
}

template<class M, class F>
void MeasConvert<M,F>::clearMethod() {
    crout.resize(0, True);
    for (Int i=0; i < cstruc.nelements(); i++) {
	delete cstruc[i]; cstruc[i] = 0;
    };
}

template<class M, class F>
void MeasConvert<M,F>::addMethod(uInt method) {
   crout.resize(crout.nelements() + 1);
    crout[crout.nelements() - 1] = method;
}

template<class M, class F>
Int MeasConvert<M,F>::nMethod() const {
  return crout.nelements();
}

template<class M, class F>
uInt MeasConvert<M,F>::getMethod(uInt which) const {
    return crout[which];
}

template<class M, class F>
void MeasConvert<M,F>::insertMethod(uInt method) {
    crout.resize(crout.nelements() + 1);
    for (Int i=crout.nelements() - 1; i>0; i--) {
	crout[i] = crout[i-1];
    }
    crout[0] = method;
}

template<class M, class F>
void MeasConvert<M,F>::addStruct(uInt which, void *struc) {
    DebugAssert(which < cstruc.nelements(), AipsError);
    delete cstruc[which]; cstruc[which] = (void *) 0;
    cstruc[which] = struc;
}

template<class M, class F>
const void *MeasConvert<M,F>::getStruct(uInt which) const {
    DebugAssert(which < cstruc.nelements(), AipsError);
    return cstruc[which];
}

template<class M, class F>
void MeasConvert<M,F>::create() {
    delete offin; offin = 0;
    if (model && model->ref.offset()) {
	F *ptmp = (F *)(model->ref.offset()->getData());
	MeasRef<M> rtmp(model->ref.getType(), model->ref.getFrame());
	MeasRef<M> mrtmp(model->ref.offset()->getRef());
	if (!mrtmp.empty()) {
	    M mtmp(*ptmp, mrtmp);
	    offin = new F(MeasConvert<M,F>(mtmp, rtmp).convert());
	} else {
	    offin = new F(*ptmp);
	}
    }
    delete offout; offout = 0;
    if (outref.offset()) {
	F *ptmp = (F *)(outref.offset()->getData());
	MeasRef<M> rtmp(outref.getType(), outref.getFrame());
	MeasRef<M> mrtmp(outref.offset()->getRef());
	if (!mrtmp.empty()) {
	    M mtmp(*ptmp, mrtmp);
            offout = new F(MeasConvert<M,F>(mtmp, rtmp).convert());
	} else {
	    offout = new F(*ptmp);
	}
    }
    crout.resize(0, True);
    DebugAssert(M::N_StructUse <= MC_N_Struct, AipsError);
    if (model && 
	!(model->ref.empty()) && !(outref.empty())) {
	if (!(model->ref.getFrame().empty()) &&
	    !(outref.getFrame().empty()) &&
	    model->ref.getFrame() != outref.getFrame()) {
	    MeasRef<M> reftmp(M::DEFAULT);
	    M::getConvert(*this, model->ref, 
			  reftmp);
	    M::getConvert(*this, reftmp,
			  outref);
	}else {
	    M::getConvert(*this, model->ref, outref);
	};
    };
}

template<class M, class F>
const F &MeasConvert<M,F>::convert() {
    return convert(model->data);
}

template<class M, class F>
const F &MeasConvert<M,F>::convert(const F &val) {
    *locres = val;
    if (offin) {
	*locres += *offin;
    }
    M::doConvert(*locres, model->ref, outref, *this);
    return *locres;
}

template<class M, class F>
void MeasConvert<M,F>::setModel(const M &val) {
    delete model; model = 0;
    model = new M(val);
    unit = val.unit;
    create();
}

template<class M, class F>
void MeasConvert<M,F>::setOut(const MeasRef<M> &mr) {
    outref = mr;
    create();
}

template<class M, class F>
void MeasConvert<M,F>::setOut(uInt mr) {
    outref = MeasRef<M>(mr);
    create();
}

template<class M, class F>
void MeasConvert<M,F>::set(const M &val, const MeasRef<M> &mr) {
    delete model; model = 0;
    model = new M(val);
    unit = val.unit;
    outref = mr;
    create();
}

template<class M, class F>
void MeasConvert<M,F>::set(const M &val, uInt mr) {
    delete model; model = 0;
    model = new M(val);
    unit = val.unit;
    outref = MeasRef<M>(mr);
    create();
}

template<class M, class F>
void MeasConvert<M,F>::set(const F &val) {
    if (model) {
	model->set(val);
    } else {
	model = new M(val);
	create();
    };
}

template<class M, class F>
void MeasConvert<M,F>::set(const Unit &inunit) {
    unit = inunit;
}    

//# Global functions
template<class M, class F>
ostream &operator<<(ostream &os, const MeasConvert<M,F> &mr) {
    os << "Converter with";
    if (mr.model) os << " Template Measure" << *mr.model;
    if (!mr.outref.empty()) os << " Output reference" << mr.outref;
    return os;
}
