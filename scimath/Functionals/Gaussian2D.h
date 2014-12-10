//# Gaussian2D.h: A two-dimensional Gaussian class
//# Copyright (C) 1995,1996,1997,2001,2002,2005
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

#ifndef SCIMATH_GAUSSIAN2D_H
#define SCIMATH_GAUSSIAN2D_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/Gaussian2DParam.h>
#include <casacore/scimath/Functionals/Function.h>
#include <casacore/scimath/Mathematics/AutoDiff.h>
#include <casacore/scimath/Mathematics/AutoDiffMath.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations
template<class T> class Vector;

// <summary> A two dimensional Gaussian class.</summary>

// <use visibility=export>

// <reviewed reviewer="tcornwel" date="1996/02/22" tests="tGaussian2D" 
// demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Gaussian2DParam">Gaussian2DParam</linkto>
//   <li> <linkto class="Function">Function</linkto>
// </prerequisite>

// <etymology> 
// A Gaussian2D functional is designed exclusively for calculating a
// Gaussian (or Normal) distribution in two dimensions. Other classes exist
// for calculating these functions in two
// (<linkto class=Gaussian1D>Gaussian1D</linkto>) and N 
// (<linkto class=GaussianND>GaussianND</linkto>) dimensions.
// </etymology>

// <synopsis> 
// A <src>Gaussian2D</src> is described by a height, center, and width,
// and position angle. Its fundamental operation is evaluating itself
// at some <src>(x,y)</src>
// coordinate. Its parameters (height, center and width, position angle) may
// be changed at run time.
//
// The width of the Gaussian (for the constructors or the <src> setWidth
// </src> function) is always specified in terms of the full width at half
// maximum (FWHM). The major axis is parallel with the y axis when the
// position angle is zero. The major axis will always have a larger width
// than the minor axis. 
//
// It is not possible to set the width of the major axis (using the <src>
// setMajorAxis </src> function) smaller than the width of the current minor
// axis. Similarly it is not possible to set the width of the minor axis
// (using the <src> setMinorAxis </src> function) to be larger than the
// current major axis. Exceptions are thrown if these rules are violated or
// if the either the major or minor axis is set to a non-positive width. To
// set both axis in one hit use the <src> setWidth </src> function. All
// these restrictions can be overcome when the parameters interface is used
// (see below).
//
// The position angle is the angle between the y axis and the major axis and
// is measured counterclockwise, so a position angle of 45 degrees rotates
// the major axis to the line where y=-x. The position angle is always
// specified and returned in radians. When using the <src> setPA </src>
// function its value must be between -2pi and + 2pi, and the returned value
// from the <src> pa </src> function will always be a value between 0 and
// pi. 
//
// The axial ratio can be used as an alternative to specifying the width of
// the minor axis. It is the ratio between the minor and major axis
// widths. The axial ratio is constrained to be between zero and one, and
// specifying something different (using setAxialRatio) will throw an
// exception.
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
// <linkto class="Gaussian2DParam">Gaussian2DParam</linkto> class), 
// is used to provide an interface to the
// <linkto module="Fitting">Fitting</linkto> classes. 
//
// There are 6 parameters that are used to describe the Gaussian:
// <ol>
// <li> The height of the Gaussian. This is identical to the value 
//      returned using the <src> height </src> member function.
// <li> The center of the Gaussian in the x direction. This is identical to
//      the value returned using the <src> xCenter </src> member function. 
// <li> The center of the Gaussian in the y direction. This is identical to
//      the value returned using the <src> yCenter </src> member function. 
// <li> The width (FWHM) of the Gaussian on one axis. Initially this will be
//      the major axis, but if the parameters are adjusted by a Fitting
//      class, it may become the axis with the smaller width. To aid
//      convergence of the non-linear fitting routines this parameter is
//      allowed to be negative. This does not affect the shape of the
//      Gaussian as the squares of the widths are used when evaluating the
//      function.
// <li> A modified axial ratio. This parameter is the ratio of the width on
//      the 'other' axis (which initially is the minor axis) and axis given
//      by parameter YWIDTH. Because these internal widths are allowed to be
//      negative and because there is no constraints on which axis is the
//      larger one the modified axial ratio is not constrained to be between
//      zero and one.
// <li> The position angle. This represents the angle (in radians) between
//      the axis used by parameter 4, and the y axis, measured
//      counterclockwise. If parameter 4 represents the major axis width
//      then this parameter will be identical to the position angle,
//      otherwise it will be different by 90 degrees. The tight constraints
//      on the value of the rotation angle enforced by the setPA() function
//      are relaxed so that any value between -6000 and 6000 is allowed. It
//      is still interpreted in radians. 
// </ol>
//
// An enumeration for the parameter index is provided, enabling the setting
// and reading of parameters with the <src>[]</src> operator. The 
// <src>mask()</src> methods can be used to check and set the parameter masks.
//
// </synopsis>

// <example>
// <srcblock>
//   Gaussian2D<Double> g(10.0, 0.0, 0.0, 2.0, 1.0, 0.0);
//   Vector<Double> x(2);
//   x(0) = 1.0; x(1) = 0.5;
//   cout << "g(" << x(0) << "," << x(1) << ") = " << g(x) << endl;
// </srcblock>
// </example>

// <templating arg=T>
//  <li> T should have standard numerical operators and exp() function. Current
//	implementation only tested for real types.
//  <li> To obtain derivatives, the derivatives should be defined.
// </templating>

// <thrown>
//    <li> Assertion in debug mode if attempt is made to set a negative width
//    <li> AipsError if incorrect parameter number specified.
//    <li> Assertion in debug mode if operator(Vector<>) with empty Vector
// </thrown>

// <todo asof="2001/08/19">
//   <li> Gaussians that know about their DFT's could be required eventually.
// </todo>

template<class T> class Gaussian2D : public Gaussian2DParam<T>
{
public:
  //# Enumerations
  
  //# Constructors
  // Constructs the two dimensional Gaussians. Defaults:
  // height=1, center=0, width(FWHM)=1, PA=0. The center and width vectors
  // must have two elements  
  // <note role=warning> Could not use default arguments
  // that worked both with gcc and IRIX </note>
  // <group>
  Gaussian2D() : Gaussian2DParam<T>() {}
  Gaussian2D(const T &height, const Vector<T> &center, 
	       const Vector<T> &width, const T &pa) :
    Gaussian2DParam<T>(height, center, width, pa) {}
  Gaussian2D(const T &height, const T &xCenter, const T &yCenter,
	       const T &majorAxis, const T &axialRatio, const T &pa) :
    Gaussian2DParam<T>(height, xCenter, yCenter, majorAxis,
		       axialRatio, pa) {}
  // </group>

  // Copy constructor (deep copy)
  // <group>
  Gaussian2D(const Gaussian2D<T> &other) : Gaussian2DParam<T>(other) {}
  template <class W>
    Gaussian2D(const Gaussian2D<W> &other) : Gaussian2DParam<T>(other) {}
  // </group>

  // Copy assignment (deep copy)
  Gaussian2D<T> &operator=(const Gaussian2D<T> &other) {
    Gaussian2DParam<T>::operator=(other); return *this; }
    
  // Destructor
  virtual ~Gaussian2D() {}

  //# Operators  
  // Evaluate the Gaussian at <src>x</src>.
  // <group>
  virtual T eval(typename Function<T>::FunctionArg x) const;
  // </group>

  //# Member functions
  // Return a copy of this object from the heap. The caller is responsible 
  // for deleting this pointer.
  // <group>
  virtual Function<T> *clone() const { return new Gaussian2D<T>(*this); }
  virtual Function<typename FunctionTraits<T>::DiffType> *cloneAD() const {
    return new Gaussian2D<typename FunctionTraits<T>::DiffType>(*this); }
  virtual Function<typename FunctionTraits<T>::BaseType> *cloneNonAD() const {
    return new Gaussian2D<typename FunctionTraits<T>::BaseType>(*this); }
  // </group>

  //# Make members of parent classes known.
protected:
  using Gaussian2DParam<T>::param_p;
  using Gaussian2DParam<T>::thePA;
  using Gaussian2DParam<T>::theCpa;
  using Gaussian2DParam<T>::theSpa;
  using Gaussian2DParam<T>::theXwidth;
public:
  using Gaussian2DParam<T>::HEIGHT;
  using Gaussian2DParam<T>::XCENTER;
  using Gaussian2DParam<T>::YCENTER;
  using Gaussian2DParam<T>::YWIDTH;
  using Gaussian2DParam<T>::RATIO;
  using Gaussian2DParam<T>::PANGLE;
  using Gaussian2DParam<T>::fwhm2int;
};

#define Gaussian2D_PS Gaussian2D

// <summary> Partial specialization of Gaussian2D for <src>AutoDiff</src>
// </summary>

// <synopsis>
// <note role=warning> The name <src>Gaussian2D_PS</src> is only for cxx2html
// documentation problems. Use <src>Gaussian2D</src> in your code.</note>
// </synopsis>

template <class T> class Gaussian2D_PS<AutoDiff<T> > : 
public Gaussian2DParam<AutoDiff<T> >
{
public:
  //# Constructors
  // Constructs two dimensional Gaussians.
  // <group>
  Gaussian2D_PS() : Gaussian2DParam<AutoDiff<T> >() {}
  Gaussian2D_PS(const AutoDiff<T> &height,
		  const Vector<AutoDiff<T> > &center, 
		  const Vector<AutoDiff<T> > &width,
		  const AutoDiff<T> &pa) :
    Gaussian2DParam<AutoDiff<T> >(height, center, width, pa) {}
  Gaussian2D_PS(const AutoDiff<T> &height, const AutoDiff<T> &xCenter,
		  const AutoDiff<T> &yCenter, const AutoDiff<T> &majorAxis,
		  const AutoDiff<T> &axialRatio, const AutoDiff<T> &pa) :
    Gaussian2DParam<AutoDiff<T> >(height, xCenter, yCenter, majorAxis,
		       axialRatio, pa) {}
  // </group>

  // Copy constructor (deep copy)
  // <group>
  Gaussian2D_PS(const Gaussian2D_PS &other) :
    Gaussian2DParam<AutoDiff<T> >(other) {}
  template <class W>
  Gaussian2D_PS(const Gaussian2D_PS<W> &other) :
    Gaussian2DParam<AutoDiff<T> >(other) {}
  // </group>

  // Copy assignment (deep copy)
  Gaussian2D_PS<AutoDiff<T> > &
    operator=(const Gaussian2D_PS<AutoDiff<T> > &other) {
    Gaussian2DParam<AutoDiff<T> >::operator=(other); return *this; }
    
  // Destructor
  virtual ~Gaussian2D_PS() {}

  //# Operators    
  // Evaluate the Gaussian and its derivatives at <src>x</src>.
  // <group>
  virtual AutoDiff<T> eval(typename Function<AutoDiff<T> >::FunctionArg x) const;
  // </group>

  //# Member functions
  // Return a copy of this object from the heap. The caller is responsible 
  // for deleting this pointer.
  // <group>
  virtual Function<AutoDiff<T> > *clone() const {
    return new Gaussian2D<AutoDiff<T> >(*this); }
  virtual Function<typename FunctionTraits<AutoDiff<T> >::DiffType>
    *cloneAD() const {
    return new Gaussian2D<typename FunctionTraits<AutoDiff<T> >::DiffType>
      (*this); }
  virtual Function<typename FunctionTraits<AutoDiff<T> >::BaseType>
    *cloneNonAD() const {
    return new Gaussian2D<typename FunctionTraits<AutoDiff<T> >::BaseType>
      (*this); }
  // </group>

  //# Make members of parent classes known.
protected:
  using Gaussian2DParam<AutoDiff<T> >::param_p;
  using Gaussian2DParam<AutoDiff<T> >::thePA;
  using Gaussian2DParam<AutoDiff<T> >::theCpa;
  using Gaussian2DParam<AutoDiff<T> >::theSpa;
  using Gaussian2DParam<AutoDiff<T> >::theXwidth;
public:
  using Gaussian2DParam<AutoDiff<T> >::HEIGHT;
  using Gaussian2DParam<AutoDiff<T> >::XCENTER;
  using Gaussian2DParam<AutoDiff<T> >::YCENTER;
  using Gaussian2DParam<AutoDiff<T> >::YWIDTH;
  using Gaussian2DParam<AutoDiff<T> >::RATIO;
  using Gaussian2DParam<AutoDiff<T> >::PANGLE;
  using Gaussian2DParam<AutoDiff<T> >::fwhm2int;
};

#undef Gaussian2D_PS


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/Gaussian2D.tcc>
#include <casacore/scimath/Functionals/Gaussian2D2.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
