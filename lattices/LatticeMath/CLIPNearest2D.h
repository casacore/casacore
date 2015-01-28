//# CLIPNearest2D.h: Nearest neighbour interpolator for CurvedLattice2D
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

#ifndef LATTICES_CLIPNEAREST2D_H
#define LATTICES_CLIPNEAREST2D_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/LatticeMath/CLInterpolator2D.h>
#include <casacore/casa/Arrays/AxesMapping.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template<class T> class Lattice;


// <summary>
// Arbitrarily shaped 1-dim lattice crosscut
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tPixelCurve.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto class=CLInterpolator2D>CLInterpolator2D</linkto>
// </prerequisite>

// <etymology>
// CLIP means CLInterpolator (its base class).
// The 2D means that interpolation in 2 dimensions needs to be done.
// </etymology>

// <synopsis>
// CLIPNearest2D is a realisation of the abstract base class CLInterpolator2D.
// This class interpolates in a very simple way by taking the nearest
// neighbour.
//
// Note that the base class contains the lattice to be interpolated and
// the axis to be used in the interpolation.
// </synopsis>


template<class T>
class CLIPNearest2D: public CLInterpolator2D<T>
{
public:
  // Only default constructor is needed.
  // The set function in the base class defines the lattice and axes.
  CLIPNearest2D();

  // Make a copy of the object.
  virtual CLIPNearest2D<T>* clone() const;

  // Get the data for the given pixel points (on axis1 and axis2) and
  // the chunk in the other axes as given by the section.
  virtual void getData (Array<T>& buffer,
			const Vector<Float>& x,
			const Vector<Float>& y,
			const Slicer& section);

  // Get the mask for the given pixel points (on axis1 and axis2) and
  // the chunk in the other axes as given by the section.
  virtual void getMask (Array<Bool>& buffer,
			const Vector<Float>& x,
			const Vector<Float>& y,
			const Slicer& section);

  //# Make members of parent class known.
protected:
  using CLInterpolator2D<T>::itsAxesMap;
  using CLInterpolator2D<T>::itsAxis1;
  using CLInterpolator2D<T>::itsAxis2;
  using CLInterpolator2D<T>::itsCurveAxis;
  using CLInterpolator2D<T>::itsIsRef;
  using CLInterpolator2D<T>::itsLatticePtr;
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/LatticeMath/CLIPNearest2D.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif 
