//# LatticeSlice1D.h: 1-D slice from a Lattice
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2002,2003
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

#ifndef LATTICES_LATTICESLICE1D_H
#define LATTICES_LATTICESLICE1D_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/iosstrfwd.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
template <class T> class MaskedLattice;
class IPosition;
class Interpolate2D;
class PixelCurve1D;
class String;

// <summary>
// Extract a 1-D slice from a Lattice
// </summary>
// <use visibility=export>
// <reviewed reviewer="" date="yyyy/mm/dd" tests="test/tLatticeSlice1D" demos="">
// </reviewed>
// <prerequisite>
//   <li> <linkto class=MaskedLattice>MaskedLattice</linkto>
// </prerequisite>

// <etymology>
// </etymology>

// <synopsis>
// This class extracts an interpolated 1-D slice from a Lattice
// with a range of interpolation schemes available. The slice must lie in 
// the plane of two cardinal axes.
// </synopsis>
//
// <note role=tip>
// </note>
//
// <example>
// <srcBlock>
// IPosition shape(2, 20, 30);                    // Create MaskedLattice
// ArrayLattice<Float> arrLat(shape);
// SubLattice<Float> subLat(arrLat);
// LatticeSlice1D<Float> slicer(subLat);
//
// IPosition blc(2); blc = 0;                     // Extract slice between corners
// IPosition trc(shape-1);
// Vector<Float> data;
// Vector<Bool> mask;
// slicer.getSlice (data, mask, blc, trc);
// </srcBlock>
// </example>

// <motivation>
// Users often want to see cross-cuts through their data.
// </motivation>

// <todo asof="2004/04/16">
//   <li> Handle curves not in cardinal axis plane
//   <li> Derive from MaskedLattice ?
// </todo>


template <class T> class LatticeSlice1D 
{
public:

// Interpolation method
   enum Method {NEAREST=0, LINEAR=1, CUBIC=2, N_TYPES};

// Default constructor - object useless
   LatticeSlice1D ();

// Constructor 
   LatticeSlice1D (const MaskedLattice<T>& lattice, Method method=LINEAR);

// Copy constructor (reference semantics)
   LatticeSlice1D(const LatticeSlice1D<T> &other);

// Destructor
   virtual ~LatticeSlice1D ();

// Assignment operator (reference semantics)
   LatticeSlice1D<T>& operator=(const LatticeSlice1D<T> &other);

// Get 1-D slice.  PixelCurve1D supplies the locus of the slice in
// the plane specified by axis0 and axis1.  The pixel coordinate for
// the rest of the lattice is specified in <src>coord</src>.
   void getSlice (Vector<T>& data, Vector<Bool>& mask,
                  const PixelCurve1D& curve, uInt axis0, uInt axis1,
                  const IPosition& coord);

// Get 1-D slice between blc & trc. These start and end points must be 
// in a cardinal plane of the lattice.  If nPts is 0 it is set automatically to
// the length of the slice.   
   void getSlice (Vector<T>& data, Vector<Bool>& mask,
                  const IPosition& blc, const IPosition& trc, uInt nPts=0);

// Get the (x,y) pixel coordinates from the last slice and the distance along
// the slice in pixels.. Also recover the axes of the slice plane
   void getPosition (uInt& axis0, uInt& axis1, Vector<Float>& x, 
                     Vector<Float>& y, Vector<Float>& distance) const;

// Recover interpolation method
   Method interpolationMethod () const {return itsMethod;};

   static Method stringToMethod (const String& method);

private:
// Check the suppliec curve is valid.
   void checkCurve (IPosition& blc, IPosition& trc, 
                    const IPosition& coord, const PixelCurve1D& curve);
// Find the slice plane.
   void findPlane (const IPosition& blc,
                   const IPosition& trc);
// Get the interpolated slice
   void doGetSlice (Vector<T>& data, Vector<Bool>& mask,
                    const PixelCurve1D& curve,
                    const IPosition& blc, const IPosition& trc);
// Make Interpolator
   void makeInterpolator (Method method);
  
//
   MaskedLattice<T>* itsLatticePtr;
   Interpolate2D* itsInterpPtr;
   Method itsMethod;
   Vector<Float> itsX;
   Vector<Float> itsY;
   Vector<Double> itsPos;
   uInt itsAxis0;
   uInt itsAxis1;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/LatticeMath/LatticeSlice1D.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
