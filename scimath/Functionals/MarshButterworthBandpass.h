//# MarshButterworthBandpass.h: a Marshallable SimButterworthBandpass
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

#ifndef SCIMATH_MARSHBUTTERWORTHBANDPASS_H
#define SCIMATH_MARSHBUTTERWORTHBANDPASS_H

#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/SimButterworthBandpass.h>
#include <casacore/scimath/Functionals/FunctionMarshallable.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations

// <summary> A Butterworth function class that supports serialization
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Function>Function</linkto>
// </prerequisite>
//
// <etymology>
// "Marsh" is short for "Marshallable" which means that the class can 
// be serialized into a form that can be transmitted to another 
// execution context.  "ButterBandpass" refers to its parent class:
// SimButterworthBandpass.
// </etymology>
//
// <synopsis>
// This class is a specialization of SimButterworthBandpass class that 
// supports serialization.  That is, it allows one to write the state of the 
// SimButterworthBandpass function object into a Record.  This record 
// can then be transmitted to another execution context
// where it can be "reconstituted" as a new object with 
// identical state as this one.  This documentation focusses on this 
// serialization functionality (also known as "marshalling"); for details 
// about the general features of this Butterworth function, see the 
// <linkto class="SimButterworthBandpass">SimButterworthBandpass</linkto> 
// class.
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// Making SimButterworthBandpass Marshallable provides a convenient way of 
// configuring the simulator tool from .
// </motivation>
//
// <thrown>
//    <li> Assertion in debug mode if attempt is made to address incorrect
//		coefficients
// </thrown>
//

template<class T>
class MarshButterworthBandpass : public SimButterworthBandpass<T>, 
			      public FunctionMarshallable 
{
public:
    static const String FUNCTYPE;
    static const String FUNCFIELDS[];

    // definitions of the fields stored in a serialized Record.  The 
    // actual string names are stored in FUNCFIELDS
    enum FieldNames {
	// the minimum cutoff, center, and maximum cutoff values
	BPASS,
	// the orders of the transitions between pass and no-pass
	ORDER,
	// the peak value
	PEAK,
	// the number of supported fields
	NFieldNames
    };

    //# Constructors
    // create a zero-th order (all-pass) Butterworth bandpass.
    MarshButterworthBandpass() : 
	SimButterworthBandpass<T>(), FunctionMarshallable(FUNCTYPE) {}

    // create a Butterworth bandpass function.
    MarshButterworthBandpass(uInt minord, uInt maxord, 
			  T mincut=T(-1), T maxcut=T(1), 
			  T center=T(0), T peak=T(1)) :
	SimButterworthBandpass<T>(minord, maxord, mincut, maxcut, 
				   center, peak), 
	FunctionMarshallable(FUNCTYPE) 
    {}

    // create a fully specified Butterworth polynomial from parameters 
    // stored in a Record.  
    explicit MarshButterworthBandpass(const Record& gr) 
	throw(InvalidSerializationError);

    // create a deep copy of another Butterworth polynomial
    // <group>
    MarshButterworthBandpass(const SimButterworthBandpass<T> &other) : 
	SimButterworthBandpass<T>(other), FunctionMarshallable(FUNCTYPE) {}
    MarshButterworthBandpass(const MarshButterworthBandpass<T> &other) : 
	SimButterworthBandpass<T>(other), FunctionMarshallable(other) {}
    // </group>
  
    // make a (deep) copy of another Butterworth polynomial
    // <group>
    MarshButterworthBandpass<T> &operator=(
	const MarshButterworthBandpass<T> &other) 
    {
	FunctionMarshallable::operator=(other);
	SimButterworthBandpass<T>::operator=(other); 
	return *this; 
    }
    MarshButterworthBandpass<T> &operator=(
	const SimButterworthBandpass<T> &other) 
    {
	SimButterworthBandpass<T>::operator=(other); 
	return *this; 
    }
    // </group>
  
    // Destructor
    virtual ~MarshButterworthBandpass() {}
  
    // store the state of this Function into a Record
    virtual void store(Record& gr) const;

    // Create a copy of this object.  The caller is responsible for
    // deleting the pointer. 
    virtual Function<T> *clone() const {
	return new MarshButterworthBandpass<T>(*this); 
    }
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/MarshButterworthBandpass.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
