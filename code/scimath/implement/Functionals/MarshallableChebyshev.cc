//# MarshalableChebyshev.h: a Marshallable Chebyshev polynomial
//# Copyright (C) 2002
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
//#! ========================================================================
//# $Id$

#include <trial/Functionals/MarshallableChebyshev.h>
#include <aips/Arrays/Array.h>

template<class T> 
const String MarshallableChebyshev<T>::modenames[] = 
        { "default", "zeroth", "extrapolate", "cyclic", "edge" };

template<class T> 
const String MarshallableChebyshev<T>::FUNCTYPE("chebyshev");

template<class T> 
const String MarshallableChebyshev<T>::FUNCFIELDS[] = 
        { "coeffs", "mode", "def", "interval" };

template<class T> 
void MarshallableChebyshev<T>::store(Record& out) const {
    loadFuncType(out);

    out.define(FUNCFIELDS[COEFFS], getCoefficients());
    out.define(FUNCFIELDS[MODE], modenames[getOutOfIntervalMode()]);
    out.define(FUNCFIELDS[DEF], getDefault());

    Vector<Double> intv(2);
    intv(0) = getIntervalMin();
    intv(1) = getIntervalMax();
    out.define(FUNCFIELDS[INTERVAL], intv);
}

template<class T> 
MarshallableChebyshev<T>::MarshallableChebyshev(const Record& gr)
    throw(InvalidSerializationError)
    : Chebyshev<T>(), FunctionMarshallable(FUNCTYPE) 
{
    SerialHelper input(gr);
    input.checkFuncType(FUNCTYPE);

    if (input.exists(FUNCFIELDS[COEFFS])) {
	Vector<T> coeffs;
	input.get(coeffs, FUNCFIELDS[COEFFS]);
	setCoefficients(coeffs);
    }
    if (input.exists(FUNCFIELDS[MODE])) {
	String modename;
	uInt i=0;
	input.get(modename, FUNCFIELDS[MODE]);
	for(i=0; i < Chebyshev<T>::NOutOfIntervalModes; i++) {
	    if (modename == modenames[i]) break;
	}
	if (i == Chebyshev<T>::NOutOfIntervalModes) 
	    throw InvalidSerializationError(String("Unrecognized mode: ")
						 + modename);
	setOutOfIntervalMode(
	    static_cast<Chebyshev<T>::OutOfIntervalMode>(i));
    }
    if (input.exists(FUNCFIELDS[DEF])) {
	T defval(0);
	input.get(defval, FUNCFIELDS[DEF]);
	setDefault(defval);
    }
    if (input.exists(FUNCFIELDS[INTERVAL])) {
	T mn, mx;
	input.get(mn, FUNCFIELDS[INTERVAL], 0);
	input.get(mx, FUNCFIELDS[INTERVAL], 1);
	setInterval(mn, mx);
    }
}
