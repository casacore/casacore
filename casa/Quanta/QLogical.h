//# QLogical.h: class to manipulate physical, dimensioned quantities
//# Copyright (C) 1994,1995,1996,1998,1999,2000
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

#ifndef CASA_QLOGICAL_H
#define CASA_QLOGICAL_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Arrays/LogiArrayFwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations

//# Typedefs

// <summary>
//   Logical operations for the Quantum class.
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tQuantum">
//
// <prerequisite>
//   <li> <linkto class=Unit>Unit</linkto>
//   <li> <linkto class=Quantum>Quantum</linkto>
// </prerequisite>
//
// <etymology>
// QLogical derived from Quantum logical functions
// </etymology>
//
// <synopsis> 
// Quantities are values with a unit. Their basic specification can be one of
// two forms:
// <srcblock>
// Quantity( Double value, String unit);	// or: Unit unit
// Quantum<Type> ( Type value, String unit)	// or: Unit unit
// </srcblock>
//
// A unit is a string of known unit fields separated
// by 'space' or '.' (to indicate multiply) or '/' (to indicate divide).
// See the <linkto class=Unit>Unit</linkto> class for details.
// Example: km/s/(Mpc.s)2  is identical to km.s-1.Mpc-2.s-2
//
// This file defines the logical operations that can be done on
// <linkto class=Quantum><src>Quantum<T></src></linkto>.
//
// They can be subdivided into various groupings:
// <ul>
//   <li> <linkto file="QLogical.h#equality">Straight comparisons: 
//          unequal if non-conforming units or different values</linkto>
//   <li> <linkto file="QLogical.h#compare">Comparisons</linkto>
//   <li> <linkto file="QLogical.h#foreign">Special make Bool routines
//                to cater for array comparisons</linkto>
// </ul>
//
// The operations defined are:
// <ul>
//   <li> Quantum<T> == Quantum<T> or ==T 
//   <li> Quantum<T> != Quantum<T> or !=T 
//   <li> > < >= <= of Quantum<T> or T and Quantum<T>
//   <li> near, nearAbs(Quantum<T> or T, Quantum<T> or T [, tolerance]) 
// </ul>
// </synopsis> 
//
// <motivation>
// To separate the logical operations from Quantum
// </motivation>
//
// <todo asof="941201">
//   <li> Some inlining (did not work first go)
//   <li> Recode with allEQ etc if part of library for all data types,
//		and get rid of the special QMakeBool(), and the
//		include LogiArrayFwd.h
// </todo>
//
// <linkfrom anchor="Quantum logical operations" classes="Quantum">
//   <here>Quantum logical operations</here> -- Logical operations
//   for the Quantum class.
// </linkfrom>

// <group name="Quantum logical operations">

//
// Straight comparisons: unequal if non-conforming units or different values
// if units made equal. I.e. <src> 1in != 1m </src>,
// <src> 1in != 1s </src>, <src> 36in == 1yd </src>./
// <group name="equality">
template <class Qtype>
Bool operator==(const Quantum<Qtype> &left, const Quantum<Qtype> &other);
template <class Qtype>
Bool operator==(const Quantum<Qtype> &left, const Qtype &other);
template <class Qtype>
Bool operator==(const Qtype &left, const Quantum<Qtype> &other);
template <class Qtype>
Bool operator!=(const Quantum<Qtype> &left, const Quantum<Qtype> &other);
template <class Qtype>
Bool operator!=(const Quantum<Qtype> &left, const Qtype &other);
template <class Qtype>
Bool operator!=(const Qtype &left, const Quantum<Qtype> &other);
// </group>

// Near-ness tests: unequal if non-conforming units. In other cases the
// tolerance is in the units of the first argument, with the second argument
// converted, if necessary, to the same units as the first.
// Note (SUN?) compiler does not accept default arguments in template functions
// <group name="near">
template <class Qtype>
Bool near(const Quantum<Qtype> &left, const Quantum<Qtype> &other);
template <class Qtype>
Bool near(const Quantum<Qtype> &left, const Quantum<Qtype> &other,
	  Double tol);
template <class Qtype>
Bool near(const Quantum<Qtype> &left, const Qtype &other);
template <class Qtype>
Bool near(const Quantum<Qtype> &left, const Qtype &other,
	  Double tol);
template <class Qtype>
Bool near(const Qtype &left, const Quantum<Qtype> &other);
template <class Qtype>
Bool near(const Qtype &left, const Quantum<Qtype> &other,
	  Double tol);
template <class Qtype>
Bool nearAbs(const Quantum<Qtype> &left, const Quantum<Qtype> &other);
template <class Qtype>
Bool nearAbs(const Quantum<Qtype> &left, const Quantum<Qtype> &other,
	  Double tol);
template <class Qtype>
Bool nearAbs(const Quantum<Qtype> &left, const Quantum<Qtype> &other,
	  const Quantum<Qtype>& tol);
template <class Qtype>
Bool nearAbs(const Quantum<Qtype> &left, const Qtype &other);
template <class Qtype>
Bool nearAbs(const Quantum<Qtype> &left, const Qtype &other,
	  Double tol);
template <class Qtype>
Bool nearAbs(const Qtype &left, const Quantum<Qtype> &other);
template <class Qtype>
Bool nearAbs(const Qtype &left, const Quantum<Qtype> &other,
	  Double tol);
// </group>
//
// Comparisons. The comparisons are done on values at equal units with
// transparent conversion if necessary.
// <thrown>
//   <li> AipsError if non-conforming units
// </thrown>
// <group name="compare">
template <class Qtype>
Bool operator<(const Quantum<Qtype> &left, const Quantum<Qtype> &other);
template <class Qtype>
Bool operator<(const Quantum<Qtype> &left, const Qtype &other);
template <class Qtype>
Bool operator<(const Qtype &left, const Quantum<Qtype> &other);
template <class Qtype>
Bool operator>(const Quantum<Qtype> &left, const Quantum<Qtype> &other);
template <class Qtype>
Bool operator>(const Quantum<Qtype> &left, const Qtype &other);
template <class Qtype>
Bool operator>(const Qtype &left, const Quantum<Qtype> &other);
template <class Qtype>
Bool operator<=(const Quantum<Qtype> &left, const Quantum<Qtype> &other);
template <class Qtype>
Bool operator<=(const Quantum<Qtype> &left, const Qtype &other);
template <class Qtype>
Bool operator<=(const Qtype &left, const Quantum<Qtype> &other);
template <class Qtype>
Bool operator>=(const Quantum<Qtype> &left, const Quantum<Qtype> &other);
template <class Qtype>
Bool operator>=(const Quantum<Qtype> &left, const Qtype &other);
template <class Qtype>
Bool operator>=(const Qtype &left, const Quantum<Qtype> &other);
// </group>
//
// Special make Bool routines to cater for array comparisons
// <group name="foreign">
Bool QMakeBool(Int val);
Bool QMakeBool(const LogicalArray &val);
// </group>

//# Inline Implementations

// </group>


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Quanta/QLogical.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
