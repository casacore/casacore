//# Copyright (C) 1996,1998,1999,2002
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

#ifndef CASA_QVECTOR_TCC
#define CASA_QVECTOR_TCC

#include <casacore/casa/Quanta/QVector.h>

#include <casacore/casa/Arrays/ArrayMath.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template <class T> QVector<T>::QVector() : Quantum<Vector<T> >() {}

template <class T> QVector<T>::QVector(const Vector<T>& v, const Unit& u) : Quantum<Vector<T> >(v, u) {}

template <class T> QVector<T>::QVector(const Vector<Quantum<T> >& q)
    : Quantum<Vector<T> >(Vector<T>(q.size()), "") {
    uInt n = q.size();
    if (n == 0) {
        return;
    }
    Unit u(q[0].getFullUnit());
    this->setUnit(u);
    Vector<T> copy(n);
    typename Vector<T>::iterator iter = copy.begin();
    typename Vector<T>::iterator end = copy.end();
    typename Vector<Quantum<T> >::const_iterator qiter = q.begin();
    while (iter != end) {
        *iter = qiter->getValue(u, True);
        ++iter;
        ++qiter;
    }
    this->setValue(copy);
}

template <class T> Quantum<T> QVector<T>::operator[](uInt index) const {
	return Quantum<T>(this->getValue()[index], this->getUnit());
}

template <class T> size_t QVector<T>::size() const {
	return this->getValue().size();
}

template <class T> size_t QVector<T>::nelements() const {
	return this->getValue().nelements();
}

template <class T> void QVector<T>::scale(T d) {
	this->setValue(d*this->getValue());
}

template <class T> QVector<T> QVector<T>::operator+(const QVector<T>& that) const {
	return QVector<T>(this->getValue() + that.getValue(this->getFullUnit()), this->getFullUnit());
}

template <class T> QVector<T> QVector<T>::operator-(const QVector<T>& that) const {
	return QVector<T>(this->getValue() - that.getValue(this->getFullUnit()), this->getFullUnit());
}

template <class T> QVector<T> QVector<T>::operator/(const T d) const {
	return QVector<T>((Vector<T>)(this->getValue()/d), this->getFullUnit());
}

template <class T> Quantum<T> QVector<T>::min() const {
	return Quantum<T>(casacore::min(this->getValue()), this->getFullUnit());
}

template <class T> Quantum<T> QVector<T>::max() const {
	return Quantum<T>(casacore::max(this->getValue()), this->getFullUnit());
}

} //# NAMESPACE CASACORE - END


#endif
