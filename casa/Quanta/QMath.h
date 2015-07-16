//# QMath.h: Mathematical operations for the Quantum class.
//# Copyright (C) 1994,1995,1996,1998,1999,2000,2004
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

#ifndef CASA_QMATH_H
#define CASA_QMATH_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Quanta/Quantum.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template <class T> class Array;

//# Typedefs

// <summary>
//   Mathematical operations for the Quantum class.
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
// QMath derived from Quantum Mathematical functions
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
// This file defines the mathematical operations that can be done on
// <linkto class=Quantum><src>Quantum<T></src></linkto>.
//
// They can be subdivided into various groupings:
// <ul>
//   <li> <linkto file="QMath.h#prefix">Unary operations</linkto>
//   <li> <linkto file="QMath.h#in-place">In place arithmetic functions: left hand side changed in place</linkto>
//   <li> <linkto file="QMath.h#math">Arithmetic functions: return <src>Quantum<T></src></linkto>
//   <li> <linkto file="QMath.h#arithmetic">Some useful arithmetic (linear) functions</linkto>
//   <li> <linkto file="QMath.h#trigonometric">Trigonometric functions</linkto>
//   <li> <linkto file="QMath.h#foreign">Functions to implement integer ceil/floor</linkto>
// </ul>
//
// The operations/functions defined are:
// <ul>
//   <li> unary <src>+(Quantum<T>)</src>
//   <li> unary <src>-(Quantum<T>)</src>
//   <li> <src>+=Quantum<T>; +=T; -=Quantum<T>; -=T;</src>
//   <li> <src>*=Quantum<T>, *=T; /=Quantum<T>; /=T;</src>
//   <li> <src>+/-/*//</src> for <src>Quantum<T>,Quantum<T>; T,Quantum<T>; Quantum<T>,T;</src>
//   <li> <src>abs, ceil, floor(Quantum<T>)</src>
//   <li> <src>pow(Quantum<T>, Int);</src>
//   <li> <src>sin, cos, tan(Quantum<T>)</src> with proper unit handling
//   <li> <src>asin, acos, atan, atan2(Quantum<T>)</src> with proper unit handling
//   <li> <src>log, log10, exp, root, sqrt</src> with proper unit handling
// </ul>
// <note role=warning>
// Some operators are implemented as member functions, and can be found in the
// <linkto class=Quantum>Quantum</linkto> class.
// </note>
// </synopsis> 
//
// <motivation>
// To separate the mathematical operations from Quantum.
// </motivation>
//
// <todo asof="941201">
//   <li> Some inlining (did not work first go)
// </todo>

// <linkfrom anchor="Quantum mathematical operations" classes="Quantum">
//   <here>Quantum mathematical operations</here> -- Mathematical operations
//   for the Quantum class.
// </linkfrom>

// <group name="Quantum mathematical operations">


// Unary operations
// <group name="prefix">
// See <linkto class=Quantum>Quantum</linkto> class
// </group>


// In place arithmetic functions: left hand side changed in place
// <thrown>
//   <li> AipsError if non-conforming units (+ and -)
//   <li> AipsError if illegal result unit (* and /; programming error)
// </thrown>
// <group name="in-place">
// See <linkto class=Quantum>Quantum</linkto> class
// </group>


// Arithmetic operators: return Quantum<T>
// <thrown>
//   <li> AipsError if non-conforming units (+ and -)
// </thrown>
// <group name="math">
// See <linkto class=Quantum>Quantum</linkto> class for equal argument types
template <class Qtype>
Quantum<Qtype> operator+(const Quantum<Qtype> &left, const Qtype &other);
template <class Qtype>
Quantum<Qtype> operator+(const Qtype &left, const Quantum<Qtype> &other);
template <class Qtype>
Quantum<Qtype> operator-(const Quantum<Qtype> &left, const Qtype &other);
template <class Qtype>
Quantum<Qtype> operator-(const Qtype &left, const Quantum<Qtype> &other);
template <class Qtype>
Quantum<Qtype> operator*(const Quantum<Qtype> &left, const Qtype &other);
template <class Qtype>
Quantum<Qtype> operator*(const Qtype &left, const Quantum<Qtype> &other);
template <class Qtype>
Quantum<Qtype> operator/(const Quantum<Qtype> &left, const Qtype &other);
template <class Qtype>
Quantum<Qtype> operator/(const Qtype &left, const Quantum<Qtype> &other);
// </group>


// Some useful arithmetic (linear) functions
// <group name="arithmetic">
// Return the Quantum raised to specified power; take the (integer) root;
// and integerization
// <thrown>
//   <li> AipsError if power exponent too large (abs > 100)
//   <li> AipsError if root exponent zero
// </thrown>
template <class Qtype>
Quantum<Qtype> pow(const Quantum<Qtype> &left, Int p);
template <class Qtype>
Quantum<Qtype> root(const Quantum<Qtype> &left, Int p);
template <class Qtype>
Quantum<Qtype> sqrt(const Quantum<Qtype> &left);
template <class Qtype>
Quantum<Qtype> abs(const Quantum<Qtype> &left);
template <class Qtype>
Quantum<Qtype> ceil(const Quantum<Qtype> &left);
template <class Qtype>
Quantum<Qtype> floor(const Quantum<Qtype> &left);
// </group>


// Trigonometric and exponential functions
// For direct functions input should be in angles, output will be empty units.
// For inverse functions input should be empty, output in radians
// <thrown>
//    <li> AipsError if incorrect units. I.e. non-angle for direct functions,
//		non-empty for inverse functions; non-empty for exp and log
// </thrown>
// <group name="trigonometric">
template <class Qtype>
Quantum<Qtype> sin(const Quantum<Qtype> &left);
template <class Qtype>
Quantum<Qtype> cos(const Quantum<Qtype> &left);
template <class Qtype>
Quantum<Qtype> tan(const Quantum<Qtype> &left);
template <class Qtype>
Quantum<Qtype> asin(const Quantum<Qtype> &left);
template <class Qtype>
Quantum<Qtype> acos(const Quantum<Qtype> &left);
template <class Qtype>
Quantum<Qtype> atan(const Quantum<Qtype> &left);
template <class Qtype>
Quantum<Qtype> atan2(const Quantum<Qtype> &left, const Quantum<Qtype> &other);
template <class Qtype>
Quantum<Qtype> atan2(const Quantum<Qtype> &left, const Qtype &other);
template <class Qtype>
Quantum<Qtype> atan2(const Qtype &left, const Quantum<Qtype> &other);
template <class Qtype>
Quantum<Qtype> log(const Quantum<Qtype> &left);
template <class Qtype>
Quantum<Qtype> log10(const Quantum<Qtype> &left);
template <class Qtype>
Quantum<Qtype> exp(const Quantum<Qtype> &left);
// </group>


// min and max
template <class Qtype>
Quantum<Qtype> min(const Quantum<Qtype> &left, const Quantum<Qtype> &other);
template <class Qtype>
Quantum<Qtype> max(const Quantum<Qtype> &left, const Quantum<Qtype> &other);


// Functions to implement integer ceil/floor and others
// <group name="foreign">
Int ceil(const Int &val);
Int floor(const Int &val);
#if !(defined(AIPS_CXX11) || (defined(__APPLE_CC__) && __APPLE_CC__ > 5621))
Float real(const Float &val);
Double real(const Double &val);
#endif
Array<Complex> operator *(const Array<Complex> &in, Double f);
Array<Complex> operator /(const Array<Complex> &in, Double f);
Array<DComplex> operator *(const Array<DComplex> &in, Double f);
Array<DComplex> operator /(const Array<DComplex> &in, Double f);
Array<Float> operator *(const Array<Float> &in, Double f);
Array<Float> operator /(const Array<Float> &in, Double f);
Array<Int> operator *(const Array<Int> &in, Double f);
Array<Int> operator /(const Array<Int> &in, Double f);
// </group>
//# Inline Implementations

// </group>


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Quanta/QMath.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
