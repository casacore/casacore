//# CLInterpolator2D.h: Abstract base class for interpolator used by CurvedLattice2D
//# Copyright (C) 2003
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
//# You should have receied a copy of the GNU Library General Public License
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

#ifndef LATTICES_CLINTERPOLATOR2D_H
#define LATTICES_CLINTERPOLATOR2D_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/AxesMapping.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template<class T> class MaskedLattice;


// <summary>
// Abstract base class for interpolator used by CurvedLattice2D.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="tCurvedLattice2D.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=CurvedLattice2D>CurvedLattice2D</linkto>
// </prerequisite>

// <etymology>
// The CL in CLInterpolator2D means CurvedLattice.
// The 2D means that interpolation in 2 dimensions needs to be done.
// </etymology>

// <synopsis>
// CurvedLattice2D needs lattice data which are not on exact grid points.
// Therefore some interpolation scheme is needed. The abstract base class
// CLInterpolation2D makes it possible for CurvedLattice2D to use any
// interpolation scheme.
// Currently the only derived class is
// <linkto class=CLIPNearest2D>CLIPNearest2D</linkto>
// <br>
// Apart from interpolating and returning data, a derived class also has to
// return a mask. For instance, in a possible derived class using
// 4-point interpolation, the interpolation scheme has to take the
// image mask into account, and make a mask for its output data (say that
// the output point is masked off if its 4 input points are masked off).
//
// This base class has some data members defining the lattice and the
// lattice axes to be interpolated. When these data members are set,
// the virtual function <src>preset</src> is called. A derived class
// can implement this function to do some precalculations, etc..
// </synopsis>

// <motivation>
// This class makes it possible to hide the interpolation to be used
// from the CurvedLattice2D class.
// </motivation>


template<class T>
class CLInterpolator2D
{
public:

  CLInterpolator2D()
  : itsLatticePtr(0) {;}

  virtual ~CLInterpolator2D();

  // Let a derived class make a copy of itself.
  virtual CLInterpolator2D<T>* clone() const = 0;

  // Set the internals to the values of the CurvedLattice using it.
  // Note that only a copy of the lattice pointer is made.
  // Thereafter the virtual function preset() is called to give a derived
  // class the opportunity to do some initial work.
  void set (MaskedLattice<T>* lattice,
	    const AxesMapping& axesMap,
	    uInt axis1, uInt axis2, uInt curveAxis);

  // Get the data for the given pixel points (on axis1 and axis2) and
  // the chunk in the other axes as given by the section.
  // The Slicer is fixed and the buffer has the correct shape.
  virtual void getData (Array<T>& buffer,
			const Vector<Float>& x,
			const Vector<Float>& y,
			const Slicer& section) = 0;

  // Get the mask for the given pixel points (on axis1 and axis2) and
  // the chunk in the other axes as given by the section.
  // The Slicer is fixed and the buffer has the correct shape.
  virtual void getMask (Array<Bool>& buffer,
			const Vector<Float>& x,
			const Vector<Float>& y,
			const Slicer& section) = 0;

protected:
  // Copy constructor can only be used by derived classes.
  CLInterpolator2D (const CLInterpolator2D<T>&);

  // Assignment can only be used by derived classes.
  CLInterpolator2D& operator= (const CLInterpolator2D<T>&);

  // Let a derived class do some initial work after set is called.
  // The default implementation does nothing.
  virtual void preset();


  MaskedLattice<T>* itsLatticePtr;
  AxesMapping       itsAxesMap;
  uInt              itsAxis1;
  uInt              itsAxis2;
  uInt              itsCurveAxis;
  Bool              itsIsRef;       // True = lattice returns array reference
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/LatticeMath/CLInterpolator2D.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif 
