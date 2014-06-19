//# SpectralList.cc: A set of SpectralElements
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
#include <components/SpectralComponents/SpectralList.h>

#include <casa/Exceptions/Error.h>
#include <casa/Containers/RecordInterface.h>
#include <casa/Containers/Record.h>
#include <components/SpectralComponents/GaussianSpectralElement.h>
#include <casa/Utilities/PtrHolder.h>
#include <components/SpectralComponents/SpectralElementFactory.h>

#include <casa/iostream.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Constructors
SpectralList::SpectralList() :
  nmax_p(0), list_p(0) {}

SpectralList::SpectralList(uInt nmax) :
  nmax_p(nmax), list_p(0) {
}

SpectralList::SpectralList(const SpectralElement &in) :
  nmax_p(0), list_p(1) {
  list_p[0] = in.clone();
}

SpectralList::SpectralList(const SpectralList &other) :
  nmax_p(other.nmax_p), list_p(other.list_p.nelements()) {
  for (uInt i=0; i<list_p.nelements(); i++) {
    list_p[i] = other.list_p[i]->clone();
  }
}

SpectralList::~SpectralList() {
  clear();
}

SpectralList &SpectralList::operator=(const SpectralList &other) {
  if (this != &other) {
    clear();
    nmax_p = other.nmax_p;
    list_p.resize(other.list_p.nelements());
    for (uInt i=0; i<list_p.nelements(); i++) {
      list_p[i] = other.list_p[i]->clone();
    }
  }
  return *this;
}

Double SpectralList::operator()(const Double x) const {
  Double s(0);
  for (uInt i=0; i<list_p.nelements(); i++) s += (*list_p[i])(x);
  return s;
}

const SpectralElement* SpectralList::operator[](const uInt n) const {
  if (n >= list_p.nelements()) {
      throw(AipsError("SpectralList: Illegal index for element"));
  }
  return list_p[n];
}

SpectralElement* SpectralList::operator[](const uInt n) {
  if (n >= list_p.nelements()) {
      throw(AipsError("SpectralList: Illegal index for element"));
  }
  return list_p[n];
}

Bool SpectralList::add(const SpectralElement &in) {
  uInt i = list_p.nelements();
  if (nmax_p != 0 && i >= nmax_p) return False;
  list_p.resize(i+1);
  list_p[i] = in.clone();
  return True;
}

Bool SpectralList::add(const SpectralList &in) {
  for (uInt i=0; i<in.nelements(); i++) {
	  if (! add(*in[i])) {
		  return False;
	  }
  }
  return True;
}

void SpectralList::insert(const SpectralElement &in) {
	uInt n = list_p.nelements();
	uInt i;
	for (i=0; i<n; i++) {
		if (compar(in, *list_p[i]) > 0) {
			break;
		}
	}
	if (i == n) add(in);
	else {
		if (nmax_p != 0 && n >= nmax_p) {
			delete list_p[n-1]; list_p[n-1] = 0;
		} else {
			list_p.resize(n+1);
			list_p[n++] = 0;
		}
		for (uInt j=n-1; j>i; j--) list_p[j] = list_p[j-1];
		if (in.getType() == SpectralElement::GAUSSIAN) {
			const GaussianSpectralElement *gIn = dynamic_cast<const GaussianSpectralElement *>(&in);
			list_p[i] = new GaussianSpectralElement(*gIn);
		}
		else {
			// FIXME for other subclasses
			list_p[i] = in.clone();
		}
	}
}

void SpectralList::insert(const SpectralList &in) {
	for (uInt i=0; i<in.nelements(); i++) {
		insert(*in[i]);
	}
}

Bool SpectralList::set(const SpectralElement &in, const uInt which) {
  uInt i = list_p.nelements();
  if (nmax_p != 0 && which >= nmax_p) return False;
  if (which > i) return False;
  if (which == i) add(in);
  delete list_p[which]; list_p[which] = 0;
  list_p[which] = in.clone();
  return True;
}

void SpectralList::clear() {
  for (uInt i=0; i<list_p.nelements(); i++) {
    delete list_p[i]; list_p[i] = 0;
  }
  list_p.resize(0, True);
}

void SpectralList::set(const uInt nmax) {
  if (nmax != 0 && nmax < list_p.nelements()) {
    for (uInt i=nmax; i<list_p.nelements(); i++) {
      delete list_p[i]; list_p[i] = 0;
    }
    list_p.resize(nmax, True);
  }
  nmax_p = nmax;
}

Bool SpectralList::fromRecord (String& errMsg, const RecordInterface& container)
{
   clear();
   for (uInt i=0; i<container.nfields(); i++) {
      if (container.dataType(i)==TpRecord) {
         const RecordInterface& rec = container.asRecord(i);
         PtrHolder<SpectralElement> specEl(SpectralElementFactory::fromRecord(rec));
         add(*specEl);
      } else {
         errMsg = String("Illegal record structure");
         return False;
      }
   }
   return True;
}

Bool SpectralList::toRecord(RecordInterface& container) const {
   String errMsg;
   for (uInt i=0; i<list_p.nelements(); i++) {
      Record elRec;
      list_p[i]->toRecord(elRec);
      container.defineRecord(i, elRec);
   }
   return True;
}
  

void SpectralList::sort() {
  uInt n = list_p.nelements();
  if (n < 2) return;
  SpectralElement *x;
  for (uInt i=0; i<n-1; i++) {
    for (uInt j=n-1; j>i; j--) {
      if (compar(*list_p[j-1], *list_p[j]) < 0) {
	x = list_p[j-1];
	list_p[j-1] = list_p[j];
	list_p[j] = x;
      }
    }
  }
}

Int SpectralList::compar(
	const SpectralElement &p1,
	const SpectralElement &p2
) const {
	SpectralElement::Types p1Type = p1.getType();
	SpectralElement::Types p2Type = p2.getType();
	Double p1Amp = 0;
	Double p2Amp = 0;
	if (p1Type == SpectralElement::GAUSSIAN) {
		const GaussianSpectralElement *g1 = dynamic_cast<const GaussianSpectralElement *>(&p1);
		p1Amp = g1->getAmpl();
	}
	if (p2Type == SpectralElement::GAUSSIAN) {
		const GaussianSpectralElement *g2 = dynamic_cast<const GaussianSpectralElement *>(&p2);
		p2Amp = g2->getAmpl();
	}
	if (p1Amp > p2Amp) {
		return 1;
	}
	else if (p1Amp < p2Amp) {
		return -1;
	}
	else {
		return 0;
	}
}

ostream &operator<<(ostream &os, const SpectralList &lst) {
  os << lst.nelements() << " in SpectralList:" << endl;
  for (uInt i=0; i<lst.nelements(); i++) os << *lst[i];

  return os;
}

} //# NAMESPACE CASA - END


//# Cater for Double and Float
#ifdef AIPS_NO_TEMPLATE_SRC
#include <components/SpectralComponents/SpectralList2.tcc>

namespace casa { //# NAMESPACE CASA - BEGIN
template void SpectralList::residual<Double>(Vector<Double> &) const;
template void SpectralList::residual<Float>(Vector<Float> &) const;
template void SpectralList::evaluate<Double>(Vector<Double> &) const;
template void SpectralList::evaluate<Float>(Vector<Float> &) const;
template void SpectralList::residual<Double>(Vector<Double> &, 
				     Vector<Double> const &) const; 
template void SpectralList::residual<Float>(Vector<Float> &,
				     Vector<Float> const &) const; 
template void SpectralList::evaluate<Double>(Vector<Double> &, 
				     Vector<Double> const &) const; 
template void SpectralList::evaluate<Float>(Vector<Float> &,
				     Vector<Float> const &) const; 
} //# NAMESPACE CASA - END
#endif
