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

#ifndef SCIMATH_MARSHALLABLECHEBYSHEV_H
#define SCIMATH_MARSHALLABLECHEBYSHEV_H

#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/Chebyshev.h>
#include <casacore/scimath/Functionals/FunctionMarshallable.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations

// <summary> A Chebyshev function class that supports serialization
// </summary>

// <use visibility=export>

// <reviewed reviewer="wbrouw" date="2001/11/12" tests="tChebyshev" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=Function>Function</linkto>
// </prerequisite>
//
// <etymology>
// This class is named after Chebyshev Type I polynomials.  "Marshallable" 
// means that the class can be serialized into a form that can be transmitted 
// to another execution context.  
// </etymology>
//
// <synopsis>
// This class is a specialization of the Chebyshev class that supports 
// serialization.  That is, it allows one to write the state of the Chebyshev 
// polynomial object into a Record.  This record can then be transmitted 
// to another execution context where it 
// can be "reconstituted" as a new object with identical state as this one.  
// This documentation focusses on this serialization functionality (also known 
// as "marshalling"); for details about the general features of the Chebyshev 
// polynomial series, see the <linkto class="Chebyshev">Chebyshev</linkto> 
// class.
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// Making Chebyshev Marshallable provides a convenient way of configuring
// the simulator tool from .
// </motivation>
//
// <thrown>
//    <li> Assertion in debug mode if attempt is made to address incorrect
//		coefficients
// </thrown>
//

template<class T>
class MarshallableChebyshev : public Chebyshev<T>, 
				public FunctionMarshallable 
{
private:
    static const String modenames[];

public:
    static const String FUNCTYPE;
    static const String FUNCFIELDS[];

    // definitions of the fields stored in a serialized Record.  The 
    // actual string names are stored in FUNCFIELDS
    enum FieldNames {
	// the array of coefficients
	COEFFS,
	// the default mode
	MODE,
	// the default value to use when mode=CONSTANT
	DEF,
	// the 2-element double array
	INTERVAL,
	// the number of supported fields
	NFieldNames
    };

    //# Constructors
    // create a zero-th order Chebyshev polynomial with the first coefficient
    // equal to zero.  The bounded domain is [T(-1), T(1)].  The 
    // OutOfDomainMode is CONSTANT, and the default value is T(0).
    MarshallableChebyshev() : 
	Chebyshev<T>(), FunctionMarshallable(FUNCTYPE) {}

    // create an n-th order Chebyshev polynomial with the coefficients
    // equal to zero.  The bounded domain is [T(-1), T(1)].  The 
    // OutOfDomainMode is CONSTANT, and the default value is T(0).
    explicit MarshallableChebyshev(const uInt n) : 
	Chebyshev<T>(n), FunctionMarshallable(FUNCTYPE)  {}

    // create a zero-th order Chebyshev polynomical with the first coefficient
    // equal to one.  
    //   min is the minimum value of its Chebyshev interval, and 
    //   max is the maximum value.  
    //   mode sets the behavior of the function outside the Chebyshev interval
    //      (see setOutOfIntervalMode() and OutOfIntervalMode enumeration 
    //      definition for details).  
    //   defval is the value returned when the function is evaluated outside
    //      the Chebyshev interval and mode=CONSTANT.
    MarshallableChebyshev(const T &min, const T &max,
			    const typename ChebyshevEnums::
			    OutOfIntervalMode mode=ChebyshevEnums::CONSTANT,
			    const T &defval=T(0)) :
	Chebyshev<T>(min, max, mode, defval), FunctionMarshallable(FUNCTYPE) 
    {}
  
    // create a fully specified Chebyshev polynomial.  
    //   coeffs holds the coefficients of the Chebyshev polynomial (see 
    //      setCoefficients() for details).
    //   min is the minimum value of its canonical range, and 
    //   max is the maximum value.  
    //   mode sets the behavior of the function outside the Chebyshev interval
    //      (see setOutOfIntervalMode() and OutOfIntervalMode enumeration 
    //      definition for details).  
    //   defval is the value returned when the function is evaluated outside
    //      the canonical range and mode=CONSTANT.
    MarshallableChebyshev(const Vector<T> &coeffs, 
			    const T &min, const T &max, 
			    const typename ChebyshevEnums::
			    OutOfIntervalMode mode=ChebyshevEnums::CONSTANT, 
			    const T &defval=T(0)) :
	Chebyshev<T>(coeffs, min, max, mode, defval), 
	FunctionMarshallable(FUNCTYPE)  
    {}
  
    // create a fully specified Chebyshev polynomial from parameters 
    // stored in a Record.  
    explicit MarshallableChebyshev(const Record& gr) 
	throw(InvalidSerializationError);

    // create a deep copy of another Chebyshev polynomial
    // <group>
    MarshallableChebyshev(const Chebyshev<T> &other) : 
	Chebyshev<T>(other), FunctionMarshallable(FUNCTYPE) {}
    MarshallableChebyshev(const MarshallableChebyshev<T> &other) : 
	Chebyshev<T>(other), FunctionMarshallable(other) {}
    // </group>
  
    // make a (deep) copy of another Chebyshev polynomial
    // <group>
    MarshallableChebyshev<T> &operator=(
	const MarshallableChebyshev<T> &other) 
    {
	FunctionMarshallable::operator=(other);
	Chebyshev<T>::operator=(other); 
	return *this; 
    }
    MarshallableChebyshev<T> &operator=(
	const Chebyshev<T> &other) 
    {
	Chebyshev<T>::operator=(other); 
	return *this; 
    }
    // </group>
  
    // Destructor
    virtual ~MarshallableChebyshev() {}
  
    // store the state of this Function into a Record
    virtual void store(Record& gr) const;

    // Create a copy of this object.  The caller is responsible for
    // deleting the pointer. 
    virtual Function<T> *clone() const { 
	return new MarshallableChebyshev<T>(*this); 
    }
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/MarshallableChebyshev.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
