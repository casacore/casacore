//# MeasRef.cc:  Reference frame for physical measures
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
#include <aips/Quanta/Quantum.h>
typedef Quantum<Double> gpp_measref_bug1;
#endif
#include <aips/Exceptions/Error.h>
#include <aips/Utilities/String.h>
#include <aips/Measures/MeasRef.h>


//# Constructors
template<class Ms> MeasRef<Ms>::MeasRef() :
  rep(0) {}

template<class Ms>
MeasRef<Ms>::MeasRef(const MeasRef<Ms> &other) {
  rep = other.rep;
  if (rep) rep->cnt++;
}

template<class Ms>
MeasRef<Ms> &MeasRef<Ms>::operator=(const MeasRef<Ms> &other) {
  if (this != &other) {
    if (other.rep) other.rep->cnt++;
    if (rep && --rep->cnt == 0) {
      delete rep;
    }
    rep = other.rep;
  }
  return *this;
}

template<class Ms>
MeasRef<Ms>::MeasRef(uInt tp) :
  rep(0) {
    create();
    rep->type = tp;
  }

template<class Ms>
MeasRef<Ms>::MeasRef(uInt tp, const Ms &ep) :
  rep(0) {
    create();
    rep->type = tp;
    rep->offmp = new Ms(ep);
  }

template<class Ms>
MeasRef<Ms>::MeasRef(uInt tp, const MeasFrame &mf) :
  rep(0) {
    create();
    rep->type = tp;
    rep->frame = mf;
  }

template<class Ms>
MeasRef<Ms>::MeasRef(uInt tp, const MeasFrame &mf, const Ms &ep) :
  rep(0) {
    create();
    rep->type = tp;
    rep->offmp = new Ms(ep);
    rep->frame = mf;
  }

template<class Ms>
void MeasRef<Ms>::create() {
  if (!rep) rep = new RefRep();
}

//# Destructor
template<class Ms>
MeasRef<Ms>::~MeasRef() {
  if (rep && --rep->cnt <= 0) {
    delete rep;
  };
}

//# Operators
template<class Ms>
Bool MeasRef<Ms>::operator==(const MeasRef<Ms> &other) const {
  return ToBool(rep == other.rep);
}

template<class Ms>
Bool MeasRef<Ms>::operator!=(const MeasRef<Ms> &other) const {
  return ToBool(rep != other.rep);
}

//# Member functions
template<class Ms>
Bool MeasRef<Ms>::empty() const {
  return ToBool(!rep);
}

template<class Ms>
const String &MeasRef<Ms>::showMe() {
  return Ms::showMe();
}

template<class Ms>
uInt MeasRef<Ms>::getType() const{
  return (rep ? rep->type : 0);
}

template<class Ms>
MeasFrame &MeasRef<Ms>::getFrame() {
  create();
  return (rep->frame);
}

template<class Ms>
const MeasFrame &MeasRef<Ms>::framePosition(MRBase &ref1,
					    MRBase &ref2) {
  if (!ref1.empty() && ref1.getFrame().position()) {
    return ref1.getFrame();
  } else if (!ref2.empty() && ref2.getFrame().position()) {
  } else {
    throw(AipsError("No MeasFrame specified for conversion of " + 
		    Ms::showMe()));
  };
  return ref2.getFrame();
}

template<class Ms>
const MeasFrame &MeasRef<Ms>::frameEpoch(MRBase &ref1,
					 MRBase &ref2) {
  if (!ref1.empty() && ref1.getFrame().epoch()) {
    return ref1.getFrame();
  } else if (!ref2.empty() && ref2.getFrame().epoch()) {
  } else {
    throw(AipsError("No MeasFrame specified for conversion of " + 
		    Ms::showMe()));
  };
  return ref2.getFrame();
}

template<class Ms>
const MeasFrame &MeasRef<Ms>::frameDirection(MRBase &ref1,
					     MRBase &ref2) {
  if (!ref1.empty() && ref1.getFrame().direction()) {
    return ref1.getFrame();
  } else if (!ref2.empty() && ref2.getFrame().direction()) {
  } else {
    throw(AipsError("No MeasFrame specified for conversion of " + 
		    Ms::showMe()));
  };
  return ref2.getFrame();
}

template<class Ms>
const MeasFrame &MeasRef<Ms>::frameRadialVelocity(MRBase &ref1,
						  MRBase &ref2) {
  if (!ref1.empty() && ref1.getFrame().radialVelocity()) {
    return ref1.getFrame();
  } else if (!ref2.empty() && ref2.getFrame().radialVelocity()) {
  } else {
    throw(AipsError("No MeasFrame specified for conversion of " + 
		    Ms::showMe()));
  };
  return ref2.getFrame();
}

template<class Ms>
const Measure *const MeasRef<Ms>::offset() const {
  return ( rep ? rep->offmp : 0);
}

template<class Ms>
void MeasRef<Ms>::setType(uInt tp) {
  set(tp);
}

template<class Ms>
void MeasRef<Ms>::set(uInt tp) {
  create();
  rep->type = tp;
}

template<class Ms>
void MeasRef<Ms>::set(const Ms &ep) {
  create();
  if (rep->offmp) {
    delete rep->offmp; rep->offmp = 0;
  };
  rep->offmp = new Ms(ep);
}

template<class Ms>
void MeasRef<Ms>::set(const Measure &ep) {
  create();
  if (rep->offmp) {
    delete rep->offmp; rep->offmp = 0;
  };
  rep->offmp = ep.clone();
}

template<class Ms>
void MeasRef<Ms>::set(const MeasFrame &mf) {
  create();
  rep->frame = mf;
}

template<class Ms>
MeasRef<Ms> MeasRef<Ms>::copy() {
  MeasRef<Ms> tmp;
  tmp.create();
  tmp.rep->type = rep->type;
  if (rep->offmp) tmp.rep->offmp = rep->offmp->clone();
  tmp.rep->frame = rep->frame;
  return tmp;
}

template<class Ms>
void MeasRef<Ms>::print(ostream &os) const {
  os << "Reference for an " << showMe(); 
  os << " with Type: " << Ms::showType(getType());
  if (offset()) {
    os << ", Offset: " << *(offset());
  };
  // Get rid of const
  if (!((MeasRef<Ms> *)(this))->getFrame().empty()) {
    os << "," << endl << ((MeasRef<Ms> *)(this))->getFrame();
  };
}
