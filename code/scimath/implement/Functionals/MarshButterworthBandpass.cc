//# MarshButterworthBandpass.cc: a Marshallable SimButterworthBandpass
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

#include <trial/Functionals/MarshButterworthBandpass.h>
#include <aips/Arrays/Array.h>

template<class T> 
const String MarshButterworthBandpass<T>::FUNCTYPE("butterworthbp");

template<class T> 
const String MarshButterworthBandpass<T>::FUNCFIELDS[] = 
        { "bpass", "order", "peak" };

template<class T> 
void MarshButterworthBandpass<T>::store(Record& out) const {
    loadFuncType(out);

    Vector<Double> bpass(3);
    bpass(0) = getMinCutoff();
    bpass(1) = getCenter();
    bpass(2) = getMaxCutoff();
    out.define(FUNCFIELDS[BPASS], bpass);

    Vector<Double> order(2);
    order(0) = getMinOrder();
    order(1) = getMaxOrder();
    out.define(FUNCFIELDS[ORDER], order);

    out.define(FUNCFIELDS[PEAK], getPeak());
}

template<class T> 
MarshButterworthBandpass<T>::MarshButterworthBandpass(const Record& gr)
    throw(InvalidSerializationError)
    : SimButterworthBandpass<T>(), FunctionMarshallable(FUNCTYPE) 
{
    SerialHelper input(gr);
    input.checkFuncType(FUNCTYPE);

    if (input.exists(FUNCFIELDS[BPASS])) {
	Vector<T> bpass;
	input.get(bpass, FUNCFIELDS[BPASS]);
	if (bpass.nelements() < 3)
	    throw InvalidSerializationError(FUNCFIELDS[BPASS] + 
				 " field contains fewer than three elements");
	setMinCutoff(bpass(0));
	setCenter(bpass(1));
	setMaxCutoff(bpass(2));
    }
    if (input.exists(FUNCFIELDS[ORDER])) {
	Vector<T> order;
	input.get(order, FUNCFIELDS[ORDER]);
	if (order.nelements() < 2)
	    throw InvalidSerializationError(FUNCFIELDS[ORDER] + 
				 " field contains fewer than two elements");
	setMinOrder(order(0));
	setMaxOrder(order(1));
    }
    if (input.exists(FUNCFIELDS[PEAK])) {
	T peak(0);
	input.get(peak, FUNCFIELDS[PEAK]);
	setPeak(peak);
    }
}
