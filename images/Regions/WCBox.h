//# WCBox.h: Class to define a box shaped WC region
//# Copyright (C) 1998,1999,2001
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
//# $Id$



#ifndef IMAGES_WCBOX_H
#define IMAGES_WCBOX_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/images/Regions/WCRegion.h>
#include <casacore/lattices/LRegions/RegionType.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Quanta/Quantum.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class LCRegion;
class TableRecord;
class IPosition;


// <summary>
// Class to define a world coordinate box region of interest in an image.
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="" tests="">
// </reviewed>
//
// <prerequisite>
//   <li> <linkto class=WCRegion>WCRegion</linkto>
//   <li> <linkto class=LCRegion>LCRegion</linkto>
//   <li> <linkto class=CoordinateSystem>CoordinateSystem</linkto>
// </prerequisite>
//
// <synopsis> 
// The corners of the box are specified in world  coordinates, but the 
// region enclosed by those corners is a box in lattice coordinates.
// Thus, the volume enclosed does not follow world coordinate contours.
//
// All this class does, apart from constructing itself, is know
// how to save itself to a <src>Record</src> and how to convert itself
// to an <src>LCRegion</src>.  The conversion allows you to apply
// a <src>WCBox</src> constructed with one <src>CoordinateSystem</src>
// to another <src>CoordinateSystem</src>. That is, you can apply a 
// <src>WCBox</src> from this image to that image.
//
// The flexibility of the  <src>CoordinateSystem</src> class should
// be kept in mind when using this class.   Recall that a 
// <src>CoordinateSystem</src> has world and pixel axes, and
// that these axes can be independently removed and independently
// (re)ordered.
//
// During construction, the length of the world coordinate vectors may be
// smaller than the number world axes in the supplied <src>CoordinateSystem</src>.
// It is assumed that the units of the world coordinates are the same as those
// encapsulated in the construction <src>CoordinateSystem</src> and in the same
// order as specified (either intrinsically, or by the world axes 
// specification vectors).
// 
// The following rules are followed during conversion to an <src>LCRegion</src>.
// <ol>
// <li> The number of elements in the supplied <src>latticeShape</src> must be equal
// to the number of pixel axes in the supplied <src>CoordinateSystem</src>.
// <li> The order of the pixel axes in the supplied  <src>CoordinateSystem</src>
// is assumed to be the order of the axes in the lattice for which the
// supplied <src>latticeShape</src> is appropriate.
// <li> The <src>CoordinateSystem</src> supplied to the <src>toLCRegion</src> 
// function does not have to be identical in structure to that from 
// which the <src>WCBox</src> was constructed.  They can consist 
// of different numbers of world and pixel axes and be in different
// orders.  
// <li> For every world axis in the supplied <src>CoordinateSystem</src>
// that is also present (somewhere) in the construction <src>CoordinateSystem</src>
// the blc/trc corresponding to that world axis will be 
// converted to pixels appropriate to the supplied <src>CoordinateSystem</src>.  
// The order of this pixel based blc/trc will be the order of the pixel axes of
// the supplied <src>CoordinateSystem</src>
// <li> For every world axis in the supplied <src>CoordinateSystem</src>
// that is not present in the construction <src>CoordinateSystem</src>,
// the supplied <src>latticeShape</src> value for the corresponding
// pixel axis is used, setting <src>blc=0</src> and <src>trc=latticeShape-1</src>
// for that axis.  
// <li> Once the pixel based blc/trc has been created, then, with
// the supplied <src>latticeShape</src>, it is used to create the
// <src>LCBox</src>, which is supplied as a pointer to the base
// class <src>LCRegion</src>.
// </ol>
//
// Note that when determining whether a world axis from one
// <src>CoordinateSystem</src>is present on another, it is
// considered to not be a match if two coordinates of the
// same type (e.g. <src>DirectionCoordinate</src>) have different
// specific types (e.g. J2000 and GALACTIC, or TOPO and LSR for
// a <src>SpectralCoordinate</src>)
// </synopsis> 
//
// <example>
// Let us give some examples with pseudo-code.
// cSys is the construction CoordinateSystem 
// and cSys2 is the supplied CoordinateSystem.
// We list their world axes in the square brackets.
// The construction blc/trc values don't matter
// as long as there cSys.nWorldAxes() of them.
// Similarly, the values of shape don't matter
// as long as there are cSys2.nPixelAxes() of them.
// <srcblock>
// cSys = [ra, dec, freq];
// cSys2 = [ra, dec];
// blc = [,,];
// trc = [,,];
// shape = [,];
// WCBox box(blc, trc, cSys);
// LCRegion* pR = box.toLCRegion(cSys2, shape);
// </srcblock>
// The resultant LCBox will have corners converted
// according to
// <srcblock>
// blcLC(0) <- blc(0);
// blcLC(1) <- blc(1);
// trcLC(0) <- trc(0);
// trcLC(1) <- trc(1);
// </srcblock>
// 
// </example>
//
// <example>
// <srcblock>
// cSys = [ra, dec, freq];
// cSys2 = [freq, stokes];
// blc = [,,];
// trc = [,,];
// shape = [,];
// WCBox box(blc, trc, cSys);
// LCRegion* pR = box.toLCRegion(cSys2, shape);
// </srcblock>
//
// The resultant LCBox will have corners converted
// according to
//
// <srcblock>
// blcLC(0) <- blc(2);
// blcLC(1) = 0;
// trcLC(0) <- trc(2);
// trcLC(1) = shape(1) - 1;
// </srcblock>
// 
// </example>
//
// <example>
// <srcblock>
// cSys = [ra, dec];
// cSys2 = [ra, dec, freq];
// blc = [,];
// trc = [,];
// shape = [,,];
// WCBox box(blc, trc, cSys);
// LCRegion* pR = box.toLCRegion(cSys2, shape);
// </srcblock>
//
// The resultant LCBox will have corners converted
// according to
//
// <srcblock>
// blcLC(0) <- blc(0);
// blcLC(1) <- blc(1);
// blcLC(2) = 0l
// trcLC(0) <- trc(0);
// trcLC(1) <- trc(1);
// trcLC(2) = shape(2)-1;
// </srcblock>
//
// </example>
//
// <example>
// <srcblock>
// cSys = [ra, dec, freq];
// cSys2 = [freq, ra, dec];
// blc = [,,];
// trc = [,,];
// shape = [,,];
// WCBox box(blc, trc, cSys);
// LCRegion* pR = box.toLCRegion(cSys2, shape);
// </srcblock>
//
// The resultant LCBox will have corners converted
// according to
//
// <srcblock>
// blcLC(0) <- blc(2);
// blcLC(1) <- blc(0);
// blcLC(2) <- blc(1);
// trcLC(0) <- trc(2);
// trcLC(1) <- trc(0);
// trcLC(2) <- trc(1);
// </srcblock>
//
// </example>
//
// <example>
// In this example we make it a bit harder by
// reordering the pixel axes too.    The new order
// of the pixel axes in terms of the original
// order [0,1,2] is given after the world axes
//
// <srcblock>
// cSys = [ra, dec, freq], [0, 1, 2];
// cSys2 = [freq, ra, dec, stokes], [3, 0, 2, 1];
// blc = [,,];
// trc = [,,];
// shape = [,,,];
// WCBox box(blc, trc, cSys);
// LCRegion* pR = box.toLCRegion(cSys2, shape);
// </srcblock>
//
// Take the first world axis of cSys2 as an example.
// First, "freq" is found as the world axis number
// 2 in cSys.  Then, when it is converted to 
// a pixel coordinate, it will turn up as 
// the value on pixel axis 1. The supplied shape
// must be appropriate to a [stokes, freq, dec, ra] lattice.
// The resultant LCBox will therefore have corners 
// converted according to
//
// <srcblock>
// blcLC(0) = 0
// blcLC(1) <- blc(2);
// blcLC(2) <- blc(1);
// blcLC(3) <- blc(0);
// 
// trcLC(0) = shape(0)-1;
// trcLC(1) <- trc(2);
// trcLC(2) <- trc(1);
// trcLC(3) <- trc(0);
// </srcblock>
// </example>
//
// <motivation>
// Users must be able to specify regions in world as well as lattice
// coordinates. 
// </motivation>
//
// <note>
//  In all of the constructors, the order of the specified world
//  coordinates is that of the *PIXEL AXES* (not world axes) in the 
//  <src>CoordinateSystem</src>.  This is the natural order for a user to want 
//  to specify them in.
// </note>
//
// <note>
//  For the constructors specifying the world values as simple doubles,
//  it is *ASSUMED* that the units of those doubles are the same as
//  the native units of the <src>CoordinateSystem</src> for each axis.
// </note>
//  
// <note>
//  World coordinates may be specified as absolute or offset.  If the
//  latter, they are offset with respect to the reference pixel of
//  the <src>CoordinateSystem</src>.
// </note>
// <todo asof="1998/05/20">
//   <li> Implement offset coordinates
// </todo>

class WCBox : public WCRegion
{
public:
   WCBox();

   // Construct from vectors of world coordinates 
   // defining the box corners.  It is assumed that the
   // order of the values is in the order of the pixel axes
   // in the given coordinate system.
   // <group>
   WCBox(const Vector<Quantum<Double> >& blc,
         const Vector<Quantum<Double> >& trc,
         const CoordinateSystem& cSys,
         const Vector<Int>& absRel);
   // </group>

   // Construct from vectors of world coordinates 
   // defining the box corners.   You specify the pixel 
   // axis order of the world values. 
   // <group>
   WCBox(const Vector<Quantum<Double> >& blc,
         const Vector<Quantum<Double> >& trc,
         const IPosition& pixelAxes,
         const CoordinateSystem& cSys,
         const Vector<Int>& absRel);
   // </group>

   // Construct from the bounding box of an  <src>LCRegion</src>.  
   WCBox(const LCRegion& region,
         const CoordinateSystem& cSys);

   // Copy constructor (reference semantics [except for <src>CoordinateSystem</src>])
   WCBox (const WCBox& other);

   // Destructor
   virtual ~WCBox();

   // Assignment (copy semantics) 
   WCBox& operator= (const WCBox& other);

   // Comparison
   virtual Bool operator==(const WCRegion& other) const;

   // Clone a WCBox object.
   virtual WCRegion* cloneRegion() const;

   // WCBox can extend a region.
   virtual Bool canExtend() const;

   // Make a new box from the given axesin this box.
   WCBox splitBox (const IPosition& axes) const;

   // Convert to an LCRegion using the supplied <src>CoordinateSystem</src> 
   // and shape.  
   virtual LCRegion* doToLCRegion (const CoordinateSystem& cSys,
                                   const IPosition& latticeShape,
                                   const IPosition& pixelAxesMap,
                                   const IPosition& outOrder) const;

   // Convert the WCBox object to a record.
   // The record can be used to make the object persistent.
   // The <src>tableName</src> argument can be used by derived
   // classes (e.g. LCPagedMask) to put very large objects.
   virtual TableRecord toRecord(const String& tableName) const;

   // Convert to a WCBox from a record.
   static WCBox* fromRecord (const TableRecord& rec,
                             const String& tableName);

   // Returns WCBox
   static String className();

   // Return region type.  Returns the class name 
   virtual String type() const;

private:
   Vector<Quantum<Double> > itsBlc;
   Vector<Quantum<Double> > itsTrc;
   IPosition itsPixelAxes;
   CoordinateSystem itsCSys;
   Vector<Int> itsAbsRel;
   Bool itsNull;


// Check units of quanta are consistent with CoordinateSystem
   void checkUnits (const IPosition& pixelAxes,
                    const Vector<Quantum<Double> >& values,
                    const CoordinateSystem& cSys);

// Convert relative pixels to absolute or fill in defaults
   void convertPixel(Double& pixel,
                     const Quantum<Double>& value,
                     const Int absRel,
                     const Double refPix,
                     const Int shape,
                     const Bool isBlc) const;

};



} //# NAMESPACE CASACORE - END

#endif
