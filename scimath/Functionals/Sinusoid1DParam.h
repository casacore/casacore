//# Sinusoid1DParam.h: Parameter handling for one dimensional Sinusoid class
//# Copyright (C) 2001,2002,2005
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

#ifndef SCIMATH_SINUSOID1DPARAM_H
#define SCIMATH_SINUSOID1DPARAM_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/Function1D.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations

// <summary> Parameter handling for one dimensional Sinusoid class
// </summary>

// <use visibility=local>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="tSinusoid1D" 
// demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="FunctionParam">FunctionParam</linkto> class
//   <li> <linkto class="Function1D">Function1D</linkto> class
// </prerequisite>

// <etymology> 
// A 1-dimensional sinusoid's parameters.
// </etymology>

// <synopsis> 
// A <src>Sinusoid1D</src> is described by an amplitude, a period,
// and a location of a peak.
// The parameters (amplitude, period, and x0) may be changed at run time. 
//
// The functional form is <src> A*cos(2*pi(x-x0)/P) </src>
//
// The parameter interface (see 
// <linkto class="FunctionParam">FunctionParam</linkto> class), 
// is used to provide an interface to the
// <linkto module="Fitting">Fitting</linkto> classes. 
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
// <src>X0</src> parameter index is provided.
//
// This class is in general used implicitly by the <src>Sinusoid1D</src>
// class only.
// </synopsis>

// <example>
// <srcblock>
//    Sinusoid1D<Double> sf(5.0, 25.0, 7);
//    sf(25);            // = -4.911
//    sf.setAmplitude(1.0);
//    sf[Sinusoid1D<Double>::PERIOD] = 2.0;                
//    sf.setX0(0.0);
//    sf(0.5);             // = 1.0
// </srcblock>
// </example>

// <templating arg=T>
//  <li> T should have standard numerical operators and exp() function. Current
//	implementation only tested for real types (and their AutoDiffs).
// </templating>

// <thrown>
//    <li> Assertion in debug mode if attempt is made to set a negative width
//    <li> AipsError if incorrect parameter number specified.
// </thrown>

template<class T> class Sinusoid1DParam : public Function1D<T>
{
public:
  //# Enumerations
  // Parameter numbers
  enum { AMPLITUDE=0, PERIOD, X0 };
  
  //# Constructors
  // Constructs the Sinusoids, Defaults:
  //  amplitude=1, period==1, x0=0. I.e. a cosinusoid with <src>cos(x)</src>.
  // <note role=warning> Could not use default arguments
  // that worked both with gcc and IRIX </note>
  // <group>
  Sinusoid1DParam();
  explicit Sinusoid1DParam(const T &amplitude);
  Sinusoid1DParam(const T &amplitude, const T &period);
  Sinusoid1DParam(const T &amplitude, const T &period, const T &x0);
  // </group>

  // Copy constructor (deep copy)
  // <group>
  Sinusoid1DParam(const Sinusoid1DParam &other);
  template <class W>
    Sinusoid1DParam(const Sinusoid1DParam<W> &other) :
    Function1D<T>(other) {}
  // </group>

  // Copy assignment (deep copy)
  Sinusoid1DParam<T> &operator=(const Sinusoid1DParam<T> &other);
    
  // Destructor
  virtual ~Sinusoid1DParam();

  //# Operators    
    
  //# Member functions
  // Give name of function
  virtual const String &name() const { static String x("sinusoid1d");
    return x; }

  // Get or set the amplitude of the Sinusoid
  // <group>
  T amplitude() const { return param_p[AMPLITUDE]; }
  void setAmplitude(const T &amplitude) { param_p[AMPLITUDE] = amplitude; }
  // </group>

  // Get or set the x0 of the Sinusoid, the location of a peak.
  // <group>
  T x0() const { return param_p[X0]; }
  void setX0(const T &x0) { param_p[X0] = x0; }
  // </group>

  // Get or set the period of the Sinusoid in full cycles.
  // <group>
  T period() const { return param_p[PERIOD]; }
  void setPeriod(const T &period) { param_p[PERIOD] = period; }
  // </group>

  //# Make members of parent classes known.
protected:
  using Function1D<T>::param_p;
public:
  using Function1D<T>::nparameters;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/Sinusoid1DParam.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
