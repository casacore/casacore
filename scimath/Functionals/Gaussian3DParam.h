//# Gaussian3DParam.h: Parameter handling for 3 dimensional Gaussian class
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
//#
//# $Id$

#ifndef SCIMATH_GAUSSIAN3DPARAM_H
#define SCIMATH_GAUSSIAN3DPARAM_H

#include <casacore/casa/aips.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/scimath/Functionals/Function.h>
#include <casacore/scimath/Mathematics/AutoDiff.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN
  //# Forward Declarations.
  template<class T> class Vector;


// <summary> Parameter handling for 3 dimensional Gaussian class
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="tGaussian3DParam">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="FunctionParam">FunctionParam</linkto> class
//   <li> <linkto class="Function">Function</linkto> class
// </prerequisite>

// <etymology>
// A 3-dimensional Gaussian's parameters.
// </etymology>

// <synopsis>

// A <src>Gaussian3D</src> is described by a height, center, width,   
// and two position angles.

// The width of the Gaussian is now specified in terms of the full width 
// at half maximum (FWHM), as with the 1D and 2D Gaussian functional classes.

// The three axis values refer to the x, y, and z axes, and unlike with the
// 2D Gaussian any of the three axes may be the longest.  Instead, the position
// angles are restricted:  The first position angle, theta, is the longitudinal
// angle, referring to the rotation (counterclockwise) around the z-axis.  The
// second, phi, is the latidudinal  angle, referring to the rotation around 
// the theta-rotated y axis.  The domain of both angles is -pi/4 < A < pi/4.
// (Note that the use of theta and phi corresponds to the mathematical
// convention for these angles, not the physics convention.)

// The parameter interface (see
// <linkto class="FunctionParam">FunctionParam</linkto> class),
// is used to provide an interface to the
// <linkto module="Fitting"> Fitting </linkto> classes.
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


// An enumeration for the <src>H</src>, <src>CX</src>,
// <src>CY</src>,<src>CZ</src>, <src>AX</src>, <src>AY</src>,
// <src>AZ</src>, <src>THETA</src>, <src>PHI</src>
// parameter index is provided, enabling the setting
// and reading of parameters with the <src>[]</src> operator. The
// <src>mask()</src> methods can be used to check and set the parameter masks. 
//      
// This class is in general used implicitly by the <src>Gaussian3D</src>
// class only.
// 
// <note role=tip>
// Other points to bear in mind when fitting this class to measured data
// are:
// <ul>
// <li> If you need to fit a circular Gaussian to data you should mask one or
//      both  position angles. This avoids rank deficiency in the fitting 
//      routines as the position angle is meaningless when the axes are
//      equal.  
// </ul>
// </note>
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


// <templating arg=T>
//  <li> T should have standard numerical operators and exp() function. Current
//      implementation only tested for real types (and AutoDiff of them).
// </templating>

// <thrown>
//    <li> Assertion in debug mode if attempt is made to set a negative width
//    <li> AipsError if incorrect parameter number specified.
//    <li> others?
// </thrown>
  
// <todo asof="2002/07/19">
//   <li> Gaussians that know about their DFT's could be required eventually.
// </todo>



template<class Type> class Gaussian3DParam : public Function<Type>
{
  // Parameter handling for the functional for 3D Gaussian Class.
  // Similar to Gaussian2DParam, but width parameters are not adjusted
  // for FWHM; they are identical to the parameters used in the function.
  
  // Position angle parameters are restricted to -PI/4 < angle < PI/4.
 
public:

  //#Enumerations
  enum 
  {
    H=0,              // value of Gaussian at the center
    CX,               // X center value
    CY,               // Y center value
    CZ,               // Z center value
    AX,               // width along X axis when T = P = 0
    AY,               // width along Y axis when T = P = 0
    AZ,               // width along Z axis when T = P = 0
    THETA,            // rotation about Z axis.
    PHI,              // rotation around X and Y axes (which depends on T).
    NPAR              // number of total parameters (9)
  };

  // Constructs the three dimensional Gaussians.  Defaults:
  // height = 1, center = {0,0,0}, width = {1,1,1}, theta = phi = 0
  // <group>
  Gaussian3DParam();
  Gaussian3DParam(Type height, const Vector<Type>& center, 
	            const Vector<Type>& width, Type theta, Type phi);
  Gaussian3DParam(Type &height, Type &xCenter, Type &yCenter, Type &zCenter,
                    Type &xWidth, Type &yWidth, Type &zWidth, 
                    Type &theta, Type &phi);
  // </group>

  // Copy construcor
  // <group>
  Gaussian3DParam(const Gaussian3DParam<Type> &other);
  template <class W>
    Gaussian3DParam(const Gaussian3DParam<W> &other) :
    Function<Type>(other),
    fwhm2int(Type(1.0)/sqrt(log(Type(16.0)))) { settrigvals(); }
  // </group>

  // Copy assignment
  Gaussian3DParam<Type> &operator=(const Gaussian3DParam<Type> &other);

  // Destructor
  virtual ~Gaussian3DParam();
  
  //# Member functions
  // Give name of function
  virtual const String &name() const { static String x("gaussian3d");
  return x; }

  // Return dimensionality
  virtual uInt ndim() const {return 3;}

  // Get or set the peak height of the Gaussian
  // <group>
  Type height() const;
  void setHeight(const Type & height);
  // </group>

  // Get or set the total flux of the Gaussian.  (Note: Since this changes
  // the height of the Gaussian but not its width, always set the width 
  // before setting the flux.)
  // <group>
  Type flux() const;                     
  void setFlux(const Type & flux);
  // </group>

  // Get or cet the center coordinates of the Gaussian
  // <group>
  Vector<Type> center() const;
  void setCenter(const Vector<Type>& center);
  Type xCenter() const;
  void setXcenter(const Type & xcenter);
  Type yCenter() const;
  void setYcenter(const Type & ycenter);
  Type zCenter() const;
  void setZcenter(const Type & zcenter);
  // </group>

  // Get or set the sigma-width of the Gaussian
  // <group>
  Vector<Type> width() const;
  void setWidth(const Vector<Type>& width);
  void setXwidth(const Type & xwidth);
  Type xWidth() const;
  void setYwidth(const Type & ywidth);
  Type yWidth() const;
  void setZwidth(const Type & zwidth);
  Type zWidth() const;
  // </group>

  // Get or set the rotation angles of the Gaussian.  
  // Theta=logitude, phi=latitude
  // <group>
  Type theta() const;
  void settheta(const Type & sT);
  Type phi() const;
  void setphi(const Type & sP);
  // </group>

protected:

  void settrigvals() const;

  Type fwhm2int;           // const to scale halfwidth at 1/e to FWHM

  mutable Type stoT_p;     // used to check if cached values below are updated
  mutable Type stoP_p;     //                            
  mutable Type cosT_p,sinT_p; // cached values of the cos and sine of THETA
  mutable Type cosP_p,sinP_p; //                                      PHI
  mutable Type cosTcosP_p; //cached values of products of cos/sine of angles
  mutable Type cosTsinP_p;
  mutable Type sinTcosP_p;
  mutable Type sinTsinP_p;

  //# Make members of parent classes known.
protected:
  using Function<Type>::param_p;
public:
  using Function<Type>::nparameters;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/Gaussian3DParam.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif




