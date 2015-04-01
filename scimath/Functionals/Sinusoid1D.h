//# Sinusoid1D.h: A one dimensional Sinusoid class
//# Copyright (C) 1997,2001,2002,2005
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

#ifndef SCIMATH_SINUSOID1D_H
#define SCIMATH_SINUSOID1D_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/Sinusoid1DParam.h>
#include <casacore/scimath/Functionals/Function1D.h>
#include <casacore/scimath/Mathematics/AutoDiff.h>
#include <casacore/scimath/Mathematics/AutoDiffMath.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations

// <summary> A one dimensional Sinusoid class.
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tSinusoid1D" 
// demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Sinusoid1DParam">Sinusoid1DParam</linkto>
//   <li> <linkto class="Function">Function</linkto>
// </prerequisite>

// <etymology> 
// A Sinusoid1D functional is designed for calculating a
// Sinusoid in one dimension. 
// </etymology>

// <synopsis> 
// A <src>Sinusoid1D</src> is described by an amplitude, a period,
// and a location of a peak. Its fundamental operation is evaluating itself
// at some <src>x</src>. The
// parameters (amplitude, period, and x0) may be changed at run time. 
//
// The functional form is <src> A*cos(2*pi(x-x0)/P) </src>
//
// The parameter interface (see 
// <linkto class="Sinusoid1DParam">Sinusoid1DParam</linkto> class), 
// is used to provide an interface to the
// <linkto module="Fitting"> Fitting </linkto> classes. 
//
// There are 3 parameters that are used to describe the Sinusoid:
// <ol>
// <li> The amplitude of the Sinusoid. This is the value 
//      returned using the <src> amplitude </src> member function.
// <li> The period of the Sinusoid in the x direction. This is 
//      the value returned using the <src> period </src> member function.
//	The period is expressed in full cycles.
// <li> The location of a peak of the Sinusoid (i.e. where
// <src>x=pi+k.2pi</src>)
// </ol>
//
// An enumeration for the <src>AMPLITUDE</src>, <src>PERIOD</src> and
// <src>X0</src> parameter index is provided, enabling the setting
// and reading of parameters with the <src>[]</src> operator. The 
// <src>mask()</src> methods can be used to check and set the parameter masks.
//
// </synopsis>

// <example>
// <srcblock>
//    Sinusoid1D<Double> sf(5.0, 25.0, 7);
//    sf(25);              // = -0.9369
//    sf.setAmplitude(1.0);
//    sf[PERIOD] = 2.0;                
//    sf.setX0(0.0);
//    sf(0.5);             // = 0.0
// </srcblock>
// </example>

// <templating arg=T>
//  <li> T should have standard numerical operators and cos() function. Current
//	implementation only tested for real types.
//  <li> To obtain derivatives, the derivatives should be defined.
// </templating>

// <thrown>
//    <li> AipsError if incorrect parameter number specified.
//    <li> Assertion in debug mode if operator(Vector<>) with empty Vector
// </thrown>

template<class T> class Sinusoid1D : public Sinusoid1DParam<T>
{
public:
  //# Enumerations
  
  //# Constructors
  // Constructs the Sinusoids, Defaults:
  //  amplitude=1, period==1, x0=0. I.e. a cosinusoid with <src>cos(x)</src>.
  // <note role=warning> Could not use default arguments
  // that worked both with gcc and IRIX </note>
  // <group>
  Sinusoid1D() : Sinusoid1DParam<T>() {}
  explicit Sinusoid1D(const T &amplitude) :
    Sinusoid1DParam<T>(amplitude) {}
  Sinusoid1D(const T &amplitude, const T &period) :
    Sinusoid1DParam<T>(amplitude, period) {}
  Sinusoid1D(const T &amplitude, const T &period, const T &x0) :
    Sinusoid1DParam<T>(amplitude, period, x0) {}
  // </group>

  // Copy constructor (deep copy)
  // <group>
  Sinusoid1D(const Sinusoid1D &other) : Sinusoid1DParam<T>(other) {}
  template <class W>
    Sinusoid1D(const Sinusoid1D<W> &other) : Sinusoid1DParam<T>(other) {}
  // </group>

  // Copy assignment (deep copy)
  Sinusoid1D<T> &operator=(const Sinusoid1D<T> &other) {
    Sinusoid1DParam<T>::operator=(other); return *this; }
    
  // Destructor
  virtual ~Sinusoid1D() {}

  //# Operators    
  // Evaluate the Sinusoid at <src>x</src>.
  // If a vector is used as the argument only its first element is used.
  // <group>
  virtual T eval(typename Function1D<T>::FunctionArg x) const;
  // </group>
    
  //# Member functions
  // Return a copy of this object from the heap. The caller is responsible 
  // for deleting this pointer. 
  // <group>
  virtual Function<T> *clone() const { return new Sinusoid1D<T>(*this); }
  virtual Function<typename FunctionTraits<T>::DiffType> *cloneAD() const {
    return new Sinusoid1D<typename FunctionTraits<T>::DiffType>(*this); }
  virtual Function<typename FunctionTraits<T>::BaseType> *cloneNonAD() const {
    return new Sinusoid1D<typename FunctionTraits<T>::BaseType>(*this); }
  // </group>

  //# Make members of parent classes known.
protected:
  using Sinusoid1DParam<T>::param_p;
public:
  using Sinusoid1DParam<T>::nparameters;
  using Sinusoid1DParam<T>::AMPLITUDE;
  using Sinusoid1DParam<T>::PERIOD;
  using Sinusoid1DParam<T>::X0;
};


#define Sinusoid1D_PS Sinusoid1D
// <summary> Partial specialization of Sinusoid1D for <src>AutoDiff</src>
// </summary>

// <synopsis>
// <note role=warning> The name <src>Sinusoid1D_PS</src> is only for cxx2html
// documentation problems. Use <src>Sinusoid1D</src> in your code.</note>
// </synopsis>

template <class T> class Sinusoid1D_PS<AutoDiff<T> > :
public Sinusoid1DParam<AutoDiff<T> >
{
public:
  //# Constructors
  // Constructs one dimensional Sinusoids.
  // <group>
  Sinusoid1D_PS() : Sinusoid1DParam<AutoDiff<T> >() {}
  explicit Sinusoid1D_PS(const AutoDiff<T> &amplitude) :
    Sinusoid1DParam<AutoDiff<T> >(amplitude) {}
  Sinusoid1D_PS(const AutoDiff<T> &amplitude, const AutoDiff<T> &period) :
    Sinusoid1DParam<AutoDiff<T> >(amplitude, period) {}
  Sinusoid1D_PS(const AutoDiff<T> &amplitude, const AutoDiff<T> &period,
		  const AutoDiff<T> &x0) :
    Sinusoid1DParam<AutoDiff<T> >(amplitude, period, x0) {}
  // </group>

  // Copy constructor (deep copy)
  // <group>
  Sinusoid1D_PS(const Sinusoid1D_PS &other) :
    Sinusoid1DParam<AutoDiff<T> >(other) {}
  template <class W>
  Sinusoid1D_PS(const Sinusoid1D_PS<W> &other) :
    Sinusoid1DParam<AutoDiff<T> >(other) {}
  // </group>

  // Copy assignment (deep copy)
  Sinusoid1D_PS<AutoDiff<T> > &
    operator=(const Sinusoid1D_PS<AutoDiff<T> > &other) {
    Sinusoid1DParam<AutoDiff<T> >::operator=(other); return *this; }
    
  // Destructor
  virtual ~Sinusoid1D_PS() {}
    
  //# Operators    
  // Evaluate the Sinusoid at <src>x</src>.
  // <group>
  virtual AutoDiff<T>
    eval(typename Function<AutoDiff<T> >::FunctionArg x) const;
  // </group>
    
  //# Member functions
  // Return a copy of this object from the heap. The caller is responsible 
  // for deleting this pointer.
  // <group>
  virtual Function<AutoDiff<T> > *clone() const {
    return new Sinusoid1D<AutoDiff<T> >(*this); }
  virtual Function<typename FunctionTraits<AutoDiff<T> >::DiffType>
    *cloneAD() const {
    return new Sinusoid1D<typename FunctionTraits<AutoDiff<T> >::DiffType>
      (*this); }
  virtual Function<typename FunctionTraits<AutoDiff<T> >::BaseType>
    *cloneNonAD() const {
    return new Sinusoid1D<typename FunctionTraits<AutoDiff<T> >::BaseType>
      (*this); }
  // </group>

protected:
  //# Make members of parent classes known.
  using Sinusoid1DParam<AutoDiff<T> >::param_p;
  using Sinusoid1DParam<AutoDiff<T> >::nparameters;
  using Sinusoid1DParam<AutoDiff<T> >::AMPLITUDE;
  using Sinusoid1DParam<AutoDiff<T> >::PERIOD;
  using Sinusoid1DParam<AutoDiff<T> >::X0;
};

#undef Sinusoid1D_PS

} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/Sinusoid1D.tcc>
#include <casacore/scimath/Functionals/Sinusoid1D2.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
