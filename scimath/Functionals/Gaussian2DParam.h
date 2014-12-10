//# Gaussian2DParam.h: Parameter handling for 2 dimensional Gaussian class
//# Copyright (C) 2001,2002,2003
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
//#
//# $Id$

#ifndef SCIMATH_GAUSSIAN2DPARAM_H
#define SCIMATH_GAUSSIAN2DPARAM_H

#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/Function.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward declarations
template<class T> class Vector;

// <summary> Parameter handling for 2 dimensional Gaussian class
// </summary>

// <use visibility=local>

// <reviewed reviewer="mwieringa" date="1996/10/d24" tests="tGaussian2D">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="FunctionParam">FunctionParam</linkto> class
//   <li> <linkto class="Function">Function</linkto> class
// </prerequisite>

// <etymology> 
// A 2-dimensional Gaussian's parameters.
// </etymology>

// <synopsis>

// A <src>Gaussian2D</src> is described by a height, center, and width,
// and position angle.

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
// if either the major or minor axis is set to a non-positive width. To
// set both axis in one hit use the <src> setWidth </src> function. All
// these restrictions can be overcome when the parameters interface is used
// (see below).
//
// The position angle is the angle between the y axis and the major axis and
// is measured counter-clockwise, so a position angle of 45 degrees rotates
// the major axis to the line where <src>y=-x</src>.
// The position angle is always
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
// <linkto class="FunctionParam">FunctionParam</linkto> class), 
// is used to provide an interface to the
// <linkto module="Fitting"> Fitting </linkto> classes. 
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
//      by parameter 4. Because these internal widths are allowed to be
//      negative and because there is no constraints on which axis is the
//      larger one the modified axial ratio is not constrained to be between
//      zero and one.
// <li> The rotation angle. This represents the angle (in radians) between
//      the axis used by parameter 4, and the y axis, measured
//      counterclockwise. If parameter 4 represents the major axis width
//      then this parameter will be identical to the position angle,
//      otherwise it will be different by 90 degrees. The tight constraints
//      on the value of the rotation angle enforced by the setPA() function
//      are relaxed so that any value between -6000 and 6000 is allowed. It
//      is still interpreted in radians. 
// </ol>
//
// An enumeration for the <src>HEIGHT</src>, <src>XCENTER</src>,
// <src>YCENTER</src>, <src>YWIDTH</src>, <src>RATIO</src>, <src>PANGLE</src>
// parameter index is provided, enabling the setting
// and reading of parameters with the <src>[]</src> operator. The 
// <src>mask()</src> methods can be used to check and set the parameter masks.
//
// This class is in general used implicitly by the <src>Gaussian2D</src>
// class only.
//
// <note role=tip>
// Other points to bear in mind when fitting this class to measured data
// are:
// <ul>
// <li> If you need to fit a circular Gaussian to data you MUST set the
//      axial ratio to one, and mask the position angle and axial ratio
//      parameters. This avoids rank deficiency in the fitting routines as
//      the position angle is meaningless when the major and minor axis are
//      equal.
// <li> If fitting an elliptical Gaussian your initial model should not be a
//      circular Gaussian. 
// </ul>
// </note>
//
// </synopsis>

// <example>
// <srcblock> 
// Gaussian2D<Double> g(10.0, 0.0, 0.0, 2.0, 1.0, 0.0);
// Vector<Double> x(2);
// x(0) = 1.0; x(1) = 0.5;
// cout << "g(" << x(0) << "," << x(1) << ") = " << g(x) << endl;
// </srcblock>
// </example>

// <motivation>
// Gaussian2D objects allow us to represent models of
// the sky in a more conventional way than the generic interface used in the
// GaussianND class does.
// </motivation>

// <templating arg=T>
//  <li> T should have standard numerical operators and exp() function. Current
//	implementation only tested for real types (and AutoDiff of them).
// </templating>

// <thrown>
//    <li> Assertion in debug mode if attempt is made to set a negative width
//    <li> AipsError if incorrect parameter number specified.
// </thrown>

// <todo asof="2001/08/19">
//   <li> Gaussians that know about their DFT's could be required eventually.
// </todo>

template<class T> class Gaussian2DParam : public Function<T>
{
public:
  //# Enumerations
  enum { HEIGHT=0, XCENTER, YCENTER, YWIDTH, RATIO, PANGLE};
  
  //# Constructors
  // Constructs the two dimensional Gaussians. Defaults:
  // height=1, center=0, width(FWHM)=1, pa=0.
  // <group>
  Gaussian2DParam();
  Gaussian2DParam(const T &height, const Vector<T> &center, 
		  const Vector<T> &width, const T &pa);
  Gaussian2DParam(const T &height, const T &xCenter, const T &yCenter,
		  const T &majorAxis, const T &axialRatio,
		  const T &pa);
  // </group>

  // Copy constructor (deep copy)
  // <group>
  Gaussian2DParam(const Gaussian2DParam<T> &other);
  template <class W>
    Gaussian2DParam(const Gaussian2DParam<W> &other) :
    Function<T>(other),
    fwhm2int(T(1.0)/sqrt(log(T(16.0)))) { majorAxis(); setPA(PA()); }
  // </group>

  // Copy assignment (deep copy)
  Gaussian2DParam<T> &operator=(const Gaussian2DParam<T> &other);
    
  // Destructor
  virtual ~Gaussian2DParam();

  //# Operators    
 
  // Variable dimensionality
  virtual uInt ndim() const { return 2; }

  //# Member functions
  // Give name of function
  virtual const String &name() const { static String x("gaussian2d");
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
  Vector<T> center() const;
  void setCenter(const Vector<T> &center);
  T xCenter() const { return param_p[XCENTER]; }
  void setXcenter(const T &cnter) { param_p[XCENTER] = cnter; }
  T yCenter() const { return param_p[YCENTER]; }
  void setYcenter(const T &cnter) { param_p[YCENTER] = cnter; }
  // </group>

  // Set or get the FWHM of the Gaussian.
  // <group>
  Vector<T> width() const;
  void setWidth(const Vector<T> &width);
  T majorAxis() const;
  void setMajorAxis(const T &width);
  T minorAxis() const;
  void setMinorAxis(const T &width);
  T axialRatio() const;
  void setAxialRatio(const T &axialRatio);
  // </group> 

  // Set/get the rotation angle (orientation) of the Gaussian.  PA is given
  // in radians counterclockwise. 
  // <group>
  T PA() const;
  void setPA(const T &pa);
  // </group>

protected:
  // Constant to scale halfwidth at 1/e to FWHM
  T fwhm2int;
  // cached vale of the PA
  mutable T thePA;
  // cached values of the cos and sine of thePA
  // <group>
  mutable T theSpa;
  mutable T theCpa;
  // </group>
  // cached vale of the Xwidth = ratio*theYwidth;
  mutable T theXwidth;

  //# Make members of parent classes known.
protected:
  using Function<T>::param_p;
public:
  using Function<T>::nparameters;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/Gaussian2DParam.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
