//# WCPolygon.h: Class to define a polygonal world coordinate region
//# Copyright (C) 1998
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

#if !defined(AIPS_WCPOLYGON_H)
#define AIPS_WCPOLYGON_H

//# Includes
#include <aips/aips.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Images/WCRegion.h>
#include <aips/Arrays/Vector.h>

//# Forward Declarations
class LCRegion;
class LCPolygon;
class TableRecord;
class IPosition;


// <summary>
// Class to define a 2-D polygonal world coordinate region of interest in an image.
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
// The corners of the 2-D polygon are given by world coordinates. The
// vertices are connected by straight lines in lattice coordinates.
//
// All this class does, apart from constructing itself, is know
// how to save itself to a <src>Record</src> and how to convert itself
// to an <src>LCRegion</src>.  The conversion allows you to apply
// a <src>WCPolygon</src> constructed with one <src>CoordinateSystem</src> 
// to another <src>CoordinateSystem</src>. That is, you can apply a 
// <src>WCPolygon</src> from this image to that image.
//
// At construction, it is assumed that the units of the world 
// coordinates are the same as those  encapsulated in the 
// construction <src>CoordinateSystem</src>.   You must tell
// the constructor, which world axes the x and vectors
// are associated with.  Make sure you account for reordering.
// For example, if you reordered [ra,dec] to be [dec,ra]
// with the <src>CoordinateSystem::transpose(,)</src> fuction
// and wished the x vector to be ra, and the y vector to
// be dec, then <src>worldAxes=[1,0]</src>.
//
// The <src>CoordinateSystem</src> supplied to the <src>toLCRegion</src> 
// (which returns a pointer to an <src>LCPolygongon</src> object)
// function does not have to be identical in structure to that with
// which the <src>WCPolygon</src> was constructed.  However, each world 
// axis given in the <src>worldAxes</src> vector at construction must be present 
// somewhere (order is unimportant) in the supplied <src>CoordinateSystem</src>.
//
//
// The supplied lattice shape must be 2-D and corresponds to the 
// pixel axes of the two world axes of the supplied
// <src>CoordinateSystem</src> which match those of the construction
// <src>CoordinateSystem</src>.
//
// </synopsis> 
//
// <example>
// Let us give some examples with pseudo-code. 
// cSys is the construction CoordinateSystem 
// and cSys2 is the supplied CoordinateSystem.
// We list their world axes in the square brackets.
// The construction polygon values don't matter.
// Similarly, the values of shape don't matter
// as long as there are 2 of them.
// <srcblock>
// cSys = [ra, dec, freq];
// cSys2 = [ra, dec];
// axes=[0,1];
// shape = [,];
// WCPolygon poly(x, y, axes, cSys);
// LCRegion* pR = poly.toLCRegion(cSys2, shape);
// </srcblock>
// The resultant LCPolygon will have vertices converted
// with the [ra, dec] axes from cSys2
// </example>
//
//
// <example>
// <srcblock>
// cSys = [ra, dec, freq];
// cSys2 = [ra, dec];
// axes=[0,2];
// shape = [,];
// WCPolygon poly(x, y, axes, cSys);
// LCRegion* pR = poly.toLCRegion(cSys2, shape);
// </srcblock>
// This will throw an exception because the [freq] axis
// is missing in cSys2
// </example>
//
// <example>
// In this example we make it a bit harder by
// reordering the pixel axes too.    The new order
// of the pixel axes in terms of the original
// order [0,1,2...] is given after the world axes
//
// <srcblock>
// cSys = [ra, dec, freq];
// cSys2 = [stokes, freq, ra, dec], [3,2,1,0];
// axes=[1,2];
// shape = [,];
// WCPolygon poly(x, y, axes, cSys);
// LCRegion* pR = poly.toLCRegion(cSys2, shape);
// </srcblock>
// The resultant LCPolygon will have vertices converted
// with the [ra, dec] axes from cSys2.  The fact that
// the pixel axes of cSys2 were reordered is  accounted
// for internally, but does not extrude further.
// </example>
//
// <example>
// In this example we make it a bit harder by
// remove a pixel axis.
//
// <srcblock>
// cSys = [ra, dec, freq];
// cSys2 = [stokes, freq, ra, dec];
// cSys2.removePixelAxis(1, cSys2.referencePixel()(1));
// axes=[1,2];
// shape = [,];
// WCPolygon poly(x, y, axes, cSys);
// LCRegion* pR = poly.toLCRegion(cSys2, shape);
// </srcblock>
// This will throw an exception because the removed
// pixel axis, pixel axis  number 1,
// corresponds to the [freq] world axis
// in cSys2, and the [freq] axis is one of those
// specified at construction.   Although the world
// axis is still present, it is not possible to
// convert to a pixel coordinate if the pixel axis
// is not there.
// </example>
//
// <motivation>
// Users must be able to specify regions in world as well as lattice
// coordinates. 
// </motivation>
//
// <todo asof="1998/05/20">
// <li> 
// </todo>

class WCPolygon : public WCRegion
{
public:
    WCPolygon();

   // Construct from two vectors of world coordinates (absolute
   // or offset) defining the polygon vertices.  The world coordinate
   // axes pertaining to the x and y vectors, respectively, are given
   // in the vector <src>worldAxes</src>.   If <src>isOffset</src> is 
   // True, then the world coordinates are offset relative to the 
   // reference pixel of the  supplied <src>CoordinateSystem</src>.
   // <group>
   WCPolygon(const Vector<Double>& x,
             const Vector<Double>& y,
             const Vector<Int>& worldAxes,
             const CoordinateSystem& cSys,
             const Bool isOffset=False);
   WCPolygon(const Vector<Float>& x,
             const Vector<Float>& y,
             const Vector<Int>& worldAxes,
             const CoordinateSystem& cSys,
             const Bool isOffset=False);
   // </group>

   // Construct from an <src>LCPolygon</src>. You specify which world axes
   // the polygon belongs to (x and then y)
   WCPolygon(const LCPolygon& polygon,
             const Vector<Int>& worldAxes,
             const CoordinateSystem& cSys);

   // Copy constructor (reference semantics).
   WCPolygon (const WCPolygon& other);

   // Destructor
   virtual ~WCPolygon();

   // Assignment (copy semantics) 
   WCPolygon& operator= (const WCPolygon& other);

   // Comparison
   // <group> 
   virtual Bool operator==(const WCRegion& other) const;
   virtual Bool operator!=(const WCRegion& other) const;
   // </group>

   // Clone a WCPolygon object.
   virtual WCPolygon* cloneRegion() const;

   // Convert to an LCRegion using the given coordinate system.
   virtual LCRegion* toLCRegion (const CoordinateSystem& cSys,
                                 const IPosition& latticeShape) const;

   // Convert the WCPolygon object to a record.
   // The record can be used to make the object persistent.
   // The <src>tableName</src> argument can be used by derived
   // classes (e.g. LCPagedMask) to put very large objects.
   virtual TableRecord toRecord(const String& tableName) const;

   // Convert to a WCPolygon from a record.
   static WCPolygon* fromRecord (const TableRecord& rec,
                                 const String& tableName);

   // Returns "WCPolygon"
   static String className();

   // Return region type.  Returns the class name   
   virtual String type() const;    


private:
   Vector<Double> itsXWC;
   Vector<Double> itsYWC;
   Vector<Int> itsWorldAxes;   // must be Int not uInt as no uInt in Glish
   CoordinateSystem itsCSys;
   Bool itsIsOffset;

};


#endif
