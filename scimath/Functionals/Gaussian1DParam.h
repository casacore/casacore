//# Gaussian1DParam.h:  Parameter handling for one-dimensional Gaussian class
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

#ifndef SCIMATH_GAUSSIAN1DPARAM_H
#define SCIMATH_GAUSSIAN1DPARAM_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/scimath/Functionals/Function1D.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations

// <summary>  Parameter handling for one dimensional Gaussian class.</summary>

// <use visibility=local>

// <reviewed reviewer="tcornwel" date="1996/02/22" tests="tGaussian1D" 
// demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="FunctionParam">FunctionParam</linkto> class
//   <li> <linkto class="Function1D">Function1D</linkto> class
// </prerequisite>

// <etymology> 
// A 1-dimensional Gaussian's parameters.
// </etymology>

// <synopsis> 
// A <src>Gaussian1D</src> is described by a height, center, and width.
// The parameters (height, center and width) may be changed at run time. 
//
// The width of the Gaussian (for the constructors or the <src>setWidth
// </src> function) is always specified in terms of the full width at half
// maximum (FWHM). It is always positive and attempts to set a non-positive
// width will throw an assertion when in debug mode.
//
// The peak height of the Gaussian can be specified at construction time or
// by using the <src> setHeight </src> function. Alternatively the <src>
// setFlux </src> function can be used to implicitly set the peak height by
// specifying the integrated area under the Gaussian. The height (or flux)
// can be positive, negative or zero, as this class makes no assumptions on
// what quantity the height represents.
//
// <note role=tip> Changing the width of the Gaussian will not affect
// its peak height but will change its flux. So you should always set the
// width before setting the flux. </note>
//
// The parameter interface (see 
// <linkto class="FunctionParam">FunctionParam</linkto> class), 
// is used to provide an interface to the
// <linkto module="Fitting">Fitting</linkto> classes. 
//
// There are 3 parameters that are used to describe the Gaussian:
// <ol>
// <li> The height of the Gaussian. This is identical to the value 
//      returned using the <src>height()</src> member function.
// <li> The center of the Gaussian in the x direction. This is identical to
//      the value returned using the <src>center()</src> member function. 
// <li> The width (FWHM) of the Gaussian. To aid convergence of
//      the non-linear fitting routines this parameter is allowed to be
//      negative. This does not affect the shape of the Gaussian as the
//	square of the width is used when evaluating the function.
// </ol>
//
// An enumeration for the <src>HEIGHT</src>, <src>WIDTH</src> and
// <src>CENTER</src> parameter index is provided, enabling the setting
// and reading of parameters with the <src>[]</src> operator. The 
// <src>mask()</src> methods can be used to check and set the parameter masks.
//
// This class is in general used implicitly by the <src>Gaussian1D</src>
// class only.
// </synopsis>

// <example>
// <srcblock>
//    Gaussian1D<Double> gf(5.0, 25.0, 7);
//    gf(25);            // = 5.0
//    gf.setHeight(1.0);
//    gf[WIDTH](2.0);                
//    gf[CENTER](0.0);
//    gf(1);             // = 0.5*height = 0.5
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

// <todo asof="2001/08/19">
//   <li> Gaussians that know about their DFT's could be required eventually.
// </todo>

template<class T> class Gaussian1DParam : public Function1D<T> {
public:
  //# Enumerations
  enum { HEIGHT=0, CENTER, WIDTH };
  
  //# Constructors
  // Constructs the one dimensional Gaussians. Defaults:
  // height=1, center=0, width(FWHM)=1.
  // <note role=warning> Could not use default arguments
  // that worked both with gcc and IRIX and all templates</note>
  // <group>
  Gaussian1DParam();
  explicit Gaussian1DParam(const T &height);
  Gaussian1DParam(const T &height, const T &center);
  Gaussian1DParam(const T &height, const T &center, const T &width);
  // </group>

  // Copy constructor (deep copy)
  // <group>
  Gaussian1DParam(const Gaussian1DParam<T> &other);
  template <class W>
    Gaussian1DParam(const Gaussian1DParam<W> &other) :
    Function1D<T>(other),
    fwhm2int(T(1.0)/sqrt(log(T(16.0)))) {}
  // </group>
  // Copy assignment (deep copy)
  Gaussian1DParam<T> &operator=(const Gaussian1DParam<T> &other);
    
  // Destructor
  virtual ~Gaussian1DParam();

  //# Operators    

  //# Member functions
  // Give name of function
  virtual const String &name() const { static String x("gaussian1d");
    return x; }

  // Get or set the peak height of the Gaussian
  // <group>
  T height() const { return param_p[HEIGHT]; }
  void setHeight(const T &height) { param_p[HEIGHT] = height; }
  // </group>

  // Get or set the analytical integrated area underneath the Gaussian.
  // Use these functions as an alternative to the height functions.
  // <group>
  T flux() const;
  void setFlux(const T &flux);
  // </group>

  // Get or set the center ordinate of the Gaussian
  // <group>
  T center() const { return param_p[CENTER]; }
  void setCenter(const T &cnter) { param_p[CENTER] = cnter; }
  // </group>

  // Get or set the FWHM of the Gaussian.
  // <group>
  T width() const { return param_p[WIDTH]; }
  void setWidth(const T &width) { param_p[WIDTH] = width; }
  // </group>

protected:
  // Constant to scale halfwidth at 1/e to FWHM
  T fwhm2int; 

  //# Make members of parent classes known.
protected:
  using Function1D<T>::param_p;
public:
  using Function1D<T>::nparameters;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/Gaussian1DParam.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
