//# Gaussian3D.h: A three-dimensional Gaussian class
//# Copyright (C) 1995,1996,1997,2001,2002
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

#if !defined(AIPS_GAUSSIAN3D_H)
#define AIPS_GAUSSIAN3D_H

#include <aips/aips.h>
#include <trial/Functionals/Gaussian3DParam.h>
#include <aips/Functionals/Function.h>
#include <aips/Mathematics/AutoDiff.h>
#include <aips/Mathematics/AutoDiffMath.h>

// <summary> A three dimensional Gaussian class.</summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tGaussian3D" 
// demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="Gaussian3DParam">Gaussian3DParam</linkto>
//   <li> <linkto class="Function">Function</linkto>
// </prerequisite>

// <etymology> 
// A Gaussian3D functional is designed exclusively for calculating a
// Gaussian (or Normal) distribution in two dimensions. Other classes exist
// for calculating these functions in two
// (<linkto class=Gaussian1D>Gaussian1D</linkto>) and N 
// (<linkto class=GaussianND>GaussianND</linkto>) dimensions.
// </etymology>

// <synopsis> 
// A <src>Gaussian3D</src> is described by a height, center, and width,
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
// <linkto class="Gaussian3DParam">Gaussian3DParam</linkto> class), 
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
//   Gaussian3D<Double> g(10.0, 0.0, 0.0, 2.0, 1.0, 0.0);
//   Vector<Double> x(2);
//   x(0) = 1.0; x(1) = 0.5;
//   cout << "g(" << x(0) << "," << x(1) << ") = " << g(x) << endl;
// </srcblock>
// </example>

// <templating arg=T>
//  <li> T should have standard numerical operators and exp() function. Current
//      implementation only tested for real types.
//  <li> To obtain derivatives, the derivatives should be defined.
// </templating>

// <thrown>
//    <li> Assertion in debug mode if attempt is made to set a negative width
//    <li> AipsError if incorrect parameter number specified.
//    <li> Assertion in debug mode if operator(Vector<>) with empty Vector
// </thrown>

// <todo asof="">
// </todo>



template<class T> class Vector;

template<class T> class Gaussian3D : public Gaussian3DParam<T>
{
public:
  // A functional for a rotated, 3D Gaussian. Similar to Gaussian2D, but
  // the xWidth, yWidth, and zWidth parameters are not adjusted for FWHM;
  // they are identical to the parameters used in the function.

  Gaussian3D();
  Gaussian3D(T height, const Vector<T>& center, 
	     const Vector<T>& width, T theta, T phi);
  Gaussian3D(T height, T xCenter, T yCenter, T zCenter,
             T xWidth, T yWidth, T zWidth, T theta, T phi);
  Gaussian3D(const Gaussian3D &other);
  virtual ~Gaussian3D();

  Gaussian3D<T> &operator=(const Gaussian3D<T> &other);

  T eval(typename Function<T>::FunctionArg x) const;
  T operator()(const T& x, const T& y, const T& z) const;

  virtual Function<T> *clone() const;

private:
  T sq(T v) const;
};


// AUTODIFF SPECIALIZATION

#define Gaussian3D_PS Gaussian3D  

// <summary> Partial specialization of Gaussian3D for <src>AutoDiff</src>
// </summary>

// <synopsis>
// <note role=warning> The name <src>Gaussian3D_PS</src> is only for cxx2html
// documentation problems. Use <src>Gaussian3D</src> in your code.</note>
// </synopsis>


template <class T> class Gaussian3D_PS<AutoDiff<T> > : public Gaussian3DParam<AutoDiff<T> > 
{
public:
  Gaussian3D_PS();
  Gaussian3D_PS(const AutoDiff<T> &height, 
                const Vector<AutoDiff<T> >& center, 
                const Vector<AutoDiff<T> >& width, 
                const AutoDiff<T>& theta,   
                const AutoDiff<T>& phi);
  Gaussian3D_PS(const AutoDiff<T>& height,  const AutoDiff<T>& xCenter,
                const AutoDiff<T>& yCenter, const AutoDiff<T>& zCenter,
                const AutoDiff<T>& xWidth,  const AutoDiff<T>& yWidth,
                const AutoDiff<T>& zWidth,  const AutoDiff<T>& theta,
                const AutoDiff<T>& phi);
  Gaussian3D_PS(const Gaussian3D_PS<AutoDiff<T> > &other);
  virtual ~Gaussian3D_PS();
//
  Gaussian3D_PS<AutoDiff<T> > &operator=(const Gaussian3D_PS<AutoDiff<T> > &other);
//
  AutoDiff<T> eval(typename Function<AutoDiff<T> >::FunctionArg x) const;
  AutoDiff<T> operator()(const AutoDiff<T>& x, 
                         const AutoDiff<T>& y, 
                         const AutoDiff<T>& z) const;
  virtual Function<AutoDiff<T> > *clone() const;
private:
  T sq(T v) const;  
};

#undef Gaussian3D_PS

#endif
