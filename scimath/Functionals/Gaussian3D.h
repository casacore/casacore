//# Gaussian3D.h: A three-dimensional Gaussian class
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

#ifndef SCIMATH_GAUSSIAN3D_H
#define SCIMATH_GAUSSIAN3D_H

#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/Gaussian3DParam.h>
#include <casacore/scimath/Functionals/Function.h>
#include <casacore/scimath/Mathematics/AutoDiff.h>
#include <casacore/scimath/Mathematics/AutoDiffMath.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN
  //# Forward Declarations.
  template<class T> class Vector;


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
// Gaussian (or Normal) distribution in three dimensions. Other classes exist
// for calculating these functions in one
// (<linkto class=Gaussian1D>Gaussian1D</linkto>), two
// (<linkto class=Gaussian2D>Gaussian2D</linkto>), and N
// (<linkto class=GaussianND>GaussianND</linkto>) dimensions.
// </etymology>

// <synopsis> 
// A <src>Gaussian3D</src> is described by a height, center, and width,
// and position angles. Its fundamental operation is evaluating itself
// at some <src>(x,y,z)</src>
// coordinate. Its parameters (height, center and width, position angles) may
// be changed at run time.

// The width of the Gaussian is now specified in terms of the full width 
// at half maximum (FWHM), like the 2D and 1D Gaussian functional classes.  

// The three axis values refer to the x, y, and z axes, and unlike with the
// 2D Gaussian any of the three axes may be the longest; instead, the position
// angles are restricted.  The first position angle, theta, is the longitudinal
// angle, referring to the rotation (counterclockwise) around the z-axis.  The
// second, phi, is the latidudinal  angle, referring to the rotation around 
// the theta-rotated y axis.  The domain of both angles is -pi/4 < A < pi/4,
// although the angles are not constrained when fitting and can be set outside
// the domain by setting the parameters directly using Functional operator[].
// (Note that the use of theta and phi corresponds to the mathematics
// convention for these angles, not the physics convention.)

// The parameter interface (see 
// <linkto class="Gaussian3DParam">Gaussian3DParam</linkto> class), 
// is used to provide an interface to the
// <linkto module="Fitting">Fitting</linkto> classes. 
//
// There are 9 parameters that are used to describe the Gaussian:
// <ol>
// <li> The height of the Gaussian. This is identical to the value
//      returned using the <src> height </src> member function.
// <li> The center of the Gaussian in the x direction. This is identical to
//      the value returned using the <src> xCenter </src> member function.
// <li> The center of the Gaussian in the y direction. This is identical to
//      the value returned using the <src> yCenter </src> member function.
// <li> The center of the Gaussian in the z direction. This is identical to
//      the value returned using the <src> zCenter </src> member function.
// <li> The width of the Gaussian along the x-axis.
// <li> The width of the Gaussian along the y-axis.
// <li> The width of the Gaussian along the z-axis.
// <li> The longitudinal position angle, theta (in radians)
// <li> The latitudinal position angle, phi (also in radians). 
// </ol>

// An enumeration for the parameter index is provided, enabling the setting
// and reading of parameters with the <src>[]</src> operator. The 
// <src>mask()</src> methods can be used to check and set the parameter masks.
//
// </synopsis>

// <example>
// <srcblock>
// Gaussian3D<Double> g(9.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0);
// Vector<Double> x(3);
// x(0) = 1.0; x(1) = 0.5; x(2) = 0.0
// cout << "g(" << x(0) << "," << x(1) << "," << x(2) << ")=" << g(x) << endl;
// </srcblock>
// </example>

// <motivation>
// The GaussianND class does not contain explicit derivatives
// and was insufficient for fitting 3D Gaussians to data.
// </motivation>

// <templating arg=T>
//  <li> T should have standard numerical operators and exp() function. Current
//      implementation only tested for real types.
//  <li> To obtain derivatives, the derivatives should be defined.
// </templating>

// <thrown>
//  <li> Assertion in debug mode if attempt is made to set a negative width
//  <li> AipsError if incorrect parameter number specified.
//  <li> Assertion in debug mode if operator(Vector<>) with empty Vector
// </thrown>

// <todo asof="2002/07/22">
//   <li> Optimize derivative calculations for faster fitting?
// </todo>



template<class T> class Gaussian3D : public Gaussian3DParam<T>
{
public:
  // A functional for a rotated, 3D Gaussian. Similar to Gaussian2D, but
  // the xWidth, yWidth, and zWidth parameters are not adjusted for FWHM;
  // they are identical to the parameters used in the function.

  // Constructs the three-dimensional Gaussians.  Defaults:
  // height = 1, center = {0,0,0}, width = {1,1,1}, theta = phi = 0.
  // The center and width vectors must have three elements.
  // <group>
  Gaussian3D();
  Gaussian3D(T height, const Vector<T>& center, 
	     const Vector<T>& width, T theta, T phi);
  Gaussian3D(T &height, T &xCenter, T &yCenter, T &zCenter,
             T &xWidth, T &yWidth, T &zWidth, T &theta, T &phi);
  // </group>

  // Copy constructor
  // <group>
  Gaussian3D(const Gaussian3D<T> &other);
  template <class W>
    Gaussian3D(const Gaussian3D<W> &other) : Gaussian3DParam<T>(other) {}
  // </group>

  // Destructor
  virtual ~Gaussian3D();

  // Assignment operator
  Gaussian3D<T> &operator=(const Gaussian3D<T> &other);

  // Evaluate the Gaussian at <src>x</src>.
  virtual T eval(typename Function<T>::FunctionArg x) const;

  // Return a copy of this object from the heap.  The caller is responsible
  // for deleting this pointer.
  // <group>
  virtual Function<T> *clone() const;
  virtual Function<typename FunctionTraits<T>::DiffType> *cloneAD() const {
    return new Gaussian3D<typename FunctionTraits<T>::DiffType>(*this); }
  virtual Function<typename FunctionTraits<T>::BaseType> *cloneNonAD() const {
    return new Gaussian3D<typename FunctionTraits<T>::BaseType>(*this); }
  // </group>

private:
  // AutoDiff does not have a square() function, so one is provided here.
  T sq(T v) const;

  //# Make members of parent classes known.
protected:
  using Gaussian3DParam<T>::param_p;
  using Gaussian3DParam<T>::stoT_p;
  using Gaussian3DParam<T>::stoP_p;
  using Gaussian3DParam<T>::cosT_p;
  using Gaussian3DParam<T>::cosP_p;
  using Gaussian3DParam<T>::sinT_p;
  using Gaussian3DParam<T>::sinP_p;
  using Gaussian3DParam<T>::cosTcosP_p;
  using Gaussian3DParam<T>::cosTsinP_p;
  using Gaussian3DParam<T>::sinTcosP_p;
  using Gaussian3DParam<T>::sinTsinP_p;
public:
  using Gaussian3DParam<T>::H;
  using Gaussian3DParam<T>::CX;
  using Gaussian3DParam<T>::CY;
  using Gaussian3DParam<T>::CZ;
  using Gaussian3DParam<T>::AX;
  using Gaussian3DParam<T>::AY;
  using Gaussian3DParam<T>::AZ;
  using Gaussian3DParam<T>::THETA;
  using Gaussian3DParam<T>::PHI;
  using Gaussian3DParam<T>::fwhm2int;
  using Gaussian3DParam<T>::settrigvals;
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
  Gaussian3D_PS(AutoDiff<T>& height,  AutoDiff<T>& xCenter,
                AutoDiff<T>& yCenter, AutoDiff<T>& zCenter,
                AutoDiff<T>& xWidth,  AutoDiff<T>& yWidth,
                AutoDiff<T>& zWidth,  AutoDiff<T>& theta,
                AutoDiff<T>& phi);
  Gaussian3D_PS(const Gaussian3D_PS<AutoDiff<T> > &other);
  template <class W>
  Gaussian3D_PS(const Gaussian3D_PS<W> &other) :
    Gaussian3DParam<AutoDiff<T> >(other) {}
   virtual ~Gaussian3D_PS();
//
  Gaussian3D_PS<AutoDiff<T> > &operator=(const Gaussian3D_PS<AutoDiff<T> > &other);
//
  virtual AutoDiff<T> eval(typename Function<AutoDiff<T> >::FunctionArg x) const;
  virtual Function<AutoDiff<T> > *clone() const;
  virtual Function<typename FunctionTraits<AutoDiff<T> >::DiffType>
    *cloneAD() const {
    return new Gaussian3D<typename FunctionTraits<AutoDiff<T> >::DiffType>
      (*this); }
  virtual Function<typename FunctionTraits<AutoDiff<T> >::BaseType>
    *cloneNonAD() const {
    return new Gaussian3D<typename FunctionTraits<AutoDiff<T> >::BaseType>
      (*this); }

private:
  T sq(T v) const;  

  //# Make members of parent classes known.
protected:
  using Gaussian3DParam<AutoDiff<T> >::param_p;
  using Gaussian3DParam<AutoDiff<T> >::stoT_p;
  using Gaussian3DParam<AutoDiff<T> >::stoP_p;
  using Gaussian3DParam<AutoDiff<T> >::cosT_p;
  using Gaussian3DParam<AutoDiff<T> >::cosP_p;
  using Gaussian3DParam<AutoDiff<T> >::sinT_p;
  using Gaussian3DParam<AutoDiff<T> >::sinP_p;
  using Gaussian3DParam<AutoDiff<T> >::cosTcosP_p;
  using Gaussian3DParam<AutoDiff<T> >::cosTsinP_p;
  using Gaussian3DParam<AutoDiff<T> >::sinTcosP_p;
  using Gaussian3DParam<AutoDiff<T> >::sinTsinP_p;
public:
  using Gaussian3DParam<AutoDiff<T> >::H;
  using Gaussian3DParam<AutoDiff<T> >::CX;
  using Gaussian3DParam<AutoDiff<T> >::CY;
  using Gaussian3DParam<AutoDiff<T> >::CZ;
  using Gaussian3DParam<AutoDiff<T> >::AX;
  using Gaussian3DParam<AutoDiff<T> >::AY;
  using Gaussian3DParam<AutoDiff<T> >::AZ;
  using Gaussian3DParam<AutoDiff<T> >::THETA;
  using Gaussian3DParam<AutoDiff<T> >::PHI;
  using Gaussian3DParam<AutoDiff<T> >::fwhm2int;
  using Gaussian3DParam<AutoDiff<T> >::settrigvals;
};

#undef Gaussian3D_PS


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/Gaussian3D.tcc>
#include <casacore/scimath/Functionals/Gaussian3D2.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif





