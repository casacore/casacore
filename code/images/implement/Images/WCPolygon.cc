//# WCPolygon.cc: Class to define a 2D polygonal world coordinate region of interest 
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
//# $Id$

#include <trial/Images/WCPolygon.h>

#include <aips/Arrays/ArrayLogical.h>
#include <trial/Coordinates/DirectionCoordinate.h>
#include <trial/Coordinates/SpectralCoordinate.h>
#include <aips/Exceptions/Error.h>
#include <aips/Lattices/IPosition.h>
#include <trial/Lattices/LCPolygon.h>
#include <aips/Mathematics/Math.h>
#include <aips/Measures/Unit.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>

//#include <iostream.h>


// Force the compiler to know about these

typedef Vector<String> gppBug_VectorString;


WCPolygon::WCPolygon()
//
//Default constructor
// 
: itsIsOffset(False)
{}

WCPolygon::WCPolygon(const Vector<Double>& xWC,
                     const Vector<Double>& yWC,
                     const Vector<Int>& worldAxes,
                     const CoordinateSystem& cSys,
                     const Bool isOffset)
//
// Constructor from Double vectors
// worldAxes is Int rather than uInt because there is
// no unsigned integer in Glish. Therefore, when the 
// axes are stored in a Record and converted to a GlishRecord
// and then sent to Glish, they must be Int
// 
: itsXWC(xWC.copy()),
  itsYWC(yWC.copy()),
  itsWorldAxes(worldAxes.copy()),
  itsCSys(cSys),
  itsIsOffset(isOffset)
{
   AlwaysAssert (itsXWC.nelements() == itsYWC.nelements(), AipsError);
   AlwaysAssert (itsWorldAxes.nelements() == 2, AipsError);
   AlwaysAssert (itsWorldAxes(0) >= 0 &&
                 itsWorldAxes(0) < Int(itsCSys.nWorldAxes()), AipsError);
   AlwaysAssert (itsWorldAxes(1) >= 0 &&
                 itsWorldAxes(1) < Int(itsCSys.nWorldAxes()), AipsError);
   AlwaysAssert (itsWorldAxes(0) != itsWorldAxes(1), AipsError);
   AlwaysAssert (itsCSys.nWorldAxes() > 0, AipsError);
   AlwaysAssert (itsIsOffset == False, AipsError);
  
}


WCPolygon::WCPolygon(const Vector<Float>& xWC,
                     const Vector<Float>& yWC,
                     const Vector<Int>& worldAxes,
                     const CoordinateSystem& cSys,
                     const Bool isOffset)
//
// Constructor from Float vectors
//
: itsWorldAxes(worldAxes.copy()),
  itsCSys(cSys),
  itsIsOffset(isOffset)
{
   AlwaysAssert (xWC.nelements() == yWC.nelements(), AipsError);
   AlwaysAssert (itsWorldAxes.nelements() == 2, AipsError);
   AlwaysAssert (itsWorldAxes(0) >= 0 &&
                 itsWorldAxes(0) < Int(itsCSys.nWorldAxes()), AipsError);
   AlwaysAssert (itsWorldAxes(1) >= 0 &&
                 itsWorldAxes(1) < Int(itsCSys.nWorldAxes()), AipsError);
   AlwaysAssert (itsWorldAxes(0) != itsWorldAxes(1), AipsError);
   AlwaysAssert (itsIsOffset == False, AipsError);

// Copy Float to double

   itsXWC.resize(xWC.nelements());
   itsYWC.resize(yWC.nelements());
   for (uInt i=0; i<itsXWC.nelements(); i++) {
      itsXWC(i) = Double(xWC(i));
      itsYWC(i) = Double(yWC(i));
   }
}


WCPolygon::WCPolygon(const LCPolygon& polyLC,
                     const Vector<Int>& worldAxes,
                     const CoordinateSystem& cSys)
//
// Constructor from an LCPolygon
//
: itsWorldAxes(worldAxes.copy()),
  itsCSys(cSys),
  itsIsOffset(False)
{
   AlwaysAssert (worldAxes.nelements() == 2,AipsError);
   AlwaysAssert (itsCSys.nPixelAxes() >= 2,AipsError);
   AlwaysAssert (itsWorldAxes(0) >= 0 &&
                 itsWorldAxes(0) < Int(itsCSys.nWorldAxes()), AipsError);
   AlwaysAssert (itsWorldAxes(1) >= 0 &&
                 itsWorldAxes(1) < Int(itsCSys.nWorldAxes()), AipsError);

// Get polygon x and y 

   Vector<Float> x = polyLC.x();
   Vector<Float> y = polyLC.y();
   itsXWC.resize(x.nelements());
   itsYWC.resize(y.nelements());


// Find pixel axes for the specified world axes

   Int xPixelAxis = itsCSys.worldAxisToPixelAxis(worldAxes(0));
   Int yPixelAxis = itsCSys.worldAxisToPixelAxis(worldAxes(1));
   if (xPixelAxis == -1 || yPixelAxis==-1) {
      throw (AipsError ("WCPolygon - the pixel axes correspoding to the given world axes have been removed"));
   }


// Create vectors for conversions
 
   Vector<Double> world(itsCSys.nWorldAxes());
   Vector<Double> pixel(itsCSys.referencePixel());


// Convert to world

   for (uInt i=0; i<x.nelements(); i++) {
      pixel(xPixelAxis) = x(i);
      pixel(yPixelAxis) = y(i);
      if (!itsCSys.toWorld(world,pixel)) {
         throw (AipsError ("WCPolygon - Cannot convert LCPolygon vertices because "+cSys.errorMessage()));
      }

// Assign world coordinates of polygon
 
      itsXWC(i) = world(worldAxes(0));
      itsYWC(i) = world(worldAxes(1));
   }
}


WCPolygon::~WCPolygon()
// 
// Destructor.  Does nothing.
//
{}

   
WCPolygon::WCPolygon (const WCPolygon& that)
//
// Copy constructor (reference semantics)
//
: itsXWC(that.itsXWC),   
  itsYWC(that.itsYWC),
  itsWorldAxes(that.itsWorldAxes),
  itsCSys(that.itsCSys),             // This one makes a copy
  itsIsOffset(that.itsIsOffset)
{}
 
WCPolygon& WCPolygon::operator= (const WCPolygon& that)
// 
// Assignment (copy semantics)
//
{
   if (this != &that) {
      itsXWC.resize(that.itsXWC.nelements());
      itsYWC.resize(that.itsYWC.nelements());
      itsWorldAxes.resize(that.itsWorldAxes.nelements());

      itsXWC = that.itsXWC;
      itsYWC = that.itsYWC;
      itsWorldAxes = that.itsWorldAxes;
      itsCSys = that.itsCSys;
      itsIsOffset = that.itsIsOffset;
    }
    return *this;
}

Bool WCPolygon::operator== (const WCRegion& other) const
{  
// Type check

   if (type() != other.type()) return False;

// Caste

   const WCPolygon& that = (const WCPolygon&)other;

// Check private data

   if (itsIsOffset != that.itsIsOffset) return False;

   if (itsXWC.nelements() != that.itsXWC.nelements()) return False;
   if (itsYWC.nelements() != that.itsYWC.nelements()) return False;

   Bool deleteX1, deleteY1;
   Bool deleteX2, deleteY2;
   const Double* pX1 = itsXWC.getStorage(deleteX1);
   const Double* pY1 = itsYWC.getStorage(deleteY1);
   const Double* pX2 = that.itsXWC.getStorage(deleteX2);
   const Double* pY2 = that.itsYWC.getStorage(deleteY2);
   for (uInt i=0; i<itsXWC.nelements(); i++) {
      if (!near(pX1[i], pX2[i])) return False;
      if (!near(pY1[i], pY2[i])) return False;
   }
   itsXWC.freeStorage(pX1, deleteX1);
   itsYWC.freeStorage(pY1, deleteY1);
   that.itsXWC.freeStorage(pX2, deleteX2);
   that.itsYWC.freeStorage(pY2, deleteY2);
 
   if (itsWorldAxes.nelements() != that.itsWorldAxes.nelements()) return False;
   for (i=0; i<itsWorldAxes.nelements(); i++) {
      if (itsWorldAxes(i) != that.itsWorldAxes(i)) return False;
   }

   if (!itsCSys.near(&(that.itsCSys))) return False;

   return True;
}
 
Bool WCPolygon::operator!= (const WCRegion& other) const
{
   if (WCPolygon::operator==(other)) return False;
   return True;
}



WCPolygon* WCPolygon::cloneRegion() const
{
   return new WCPolygon(*this);
}


TableRecord WCPolygon::toRecord(const String&) const
{
   TableRecord rec;
   defineRecordFields(rec, className());  
   rec.define ("x", itsXWC);
   rec.define ("y", itsYWC);
   rec.define ("worldAxes", itsWorldAxes);
   rec.define ("offset", itsIsOffset);
   if (!itsCSys.save(rec, "coordinates")) {
      throw (AipsError ("WCPolygon::toRecord: could not save Coordinate System"));
   }

   return rec;
}


WCPolygon* WCPolygon::fromRecord (const TableRecord& rec,
                                  const String&)
//
// The record is always stored by this class with
// the field "worldAxes"   However, for the Glish function
// wcpolygon, it is also useful to be able to specify the
// axes as pixel axes (try and protect Glish users from the
// distinction).  Thus, we allow a field "pixelAxes".
// If this is present, we convert the pixel axes to world
// axes before reconstituting the WCPolygon
//
{
   CoordinateSystem* pCSys =  
      CoordinateSystem::restore(rec,"coordinates");
   if (rec.isDefined("worldAxes")) {
      return new WCPolygon(Vector<Double>(rec.asArrayDouble ("x")),
                           Vector<Double>(rec.asArrayDouble ("y")),
                           Vector<Int>(rec.asArrayInt ("worldAxes")),
                           *pCSys, rec.asBool("offset"));
   } else if (rec.isDefined("pixelAxes")) {
//
// Convert pixel axes to world axes
//
      Vector<Int> worldAxes(pCSys->nWorldAxes());
      Vector<Int> pixelAxes = Vector<Int>(rec.asArrayInt ("pixelAxes"));
      for (uInt i=0; i<pixelAxes.nelements(); i++) {
         Int worldAxis = pCSys->pixelAxisToWorldAxis(pixelAxes(i));
         if (worldAxis == -1) {
            throw (AipsError ("WCPolygon::fromRecord - some of the pixel axes have no world axis"));
         } else {
            worldAxes(i) = worldAxis;
         }
//
// Return the WCPolygon
// 
         return new WCPolygon(Vector<Double>(rec.asArrayDouble ("x")),
                              Vector<Double>(rec.asArrayDouble ("y")),
                              worldAxes, *pCSys, rec.asBool("offset"));
      }
   } else {
      throw (AipsError ("WCPolygon::fromRecord - record has neither worldAxes nor pixelAxes fields defined"));
   }
   delete pCSys;
   return 0;
}


LCRegion* WCPolygon::toLCRegion (const CoordinateSystem& cSys,
                                 const IPosition& latticeShape) const
{

// Make sure that we are not using a null Polygon 
 
   AlwaysAssert (itsXWC.nelements() > 0, AipsError); 
   AlwaysAssert (itsWorldAxes.nelements() > 0, AipsError); 


// Some checks.  The supplied CS must have at least two pixel axes,
// although at this point we don't know if they are the right ones !

   AlwaysAssert (latticeShape.nelements() == 2, AipsError);
   AlwaysAssert (cSys.nPixelAxes()>=2, AipsError);
   AlwaysAssert (cSys.nWorldAxes() > 0, AipsError);


// Make a world axis map.  worldAxisMap(i) says where world axis i from
// the construction CS is in the supplied CS.  worldAxisTranspose(i) is 
// the location of world axis i  from the supplied CS in the construction 
// CS.   
 
   Vector<Int> worldAxisMap;
   Vector<Int> worldAxisTranspose;
   if (!cSys.worldMap (worldAxisMap, worldAxisTranspose, itsCSys)) {
      throw (AipsError ("WCPolygon::toLCregion: "+cSys.errorMessage()));
   }
//   cout << "map=" << worldAxisMap.ac() << endl;

   if (worldAxisMap(itsWorldAxes(0)) != -1 &&
       worldAxisMap(itsWorldAxes(1)) != -1) {
/*
      cout << "Construction world axes are "
           << itsCSys.worldAxisNames()(itsWorldAxes(0))
           << " and " 
           << itsCSys.worldAxisNames()(itsWorldAxes(1)) << endl;
      cout << "Supplied polygon world axes are "
           << cSys.worldAxisNames()(worldAxisMap(itsWorldAxes(0)))
           << " and " 
           << cSys.worldAxisNames()(worldAxisMap(itsWorldAxes(1))) << endl;
*/
   } else {
      throw (AipsError ("WCPolygon::toLCRegion: supplied CoordinateSystem does not contain needed world axes"));
   }


// Make a copy of the supplied CoordinateSystem so we can mess 
// about with it

   CoordinateSystem cSysTmp(cSys);


// Assign indexers for output world->pixel conversion vectors
// The world and pixel axes are not necessarily in the same order

   uInt pXWC = worldAxisMap(itsWorldAxes(0));
   uInt pYWC = worldAxisMap(itsWorldAxes(1));
   Int  pXLC = cSys.worldAxisToPixelAxis(pXWC);
   Int  pYLC = cSys.worldAxisToPixelAxis(pYWC);
   if (pXLC == -1 || pYLC == -1) {
      throw (AipsError ("WCPolygon::toLCRegion: could not find output pixel axes in supplied CoordinateSystem"));
   }
//   cout << "pXWC,pYWC=" << pXWC << ", " << pYWC << endl;
//   cout << "pXLC,pYLC=" << pXLC << ", " << pYLC << endl;


// Set units

   Vector<String> units(cSysTmp.worldAxisUnits().copy());
   units(pXWC) = itsCSys.worldAxisUnits()(itsWorldAxes(0));
   units(pYWC) = itsCSys.worldAxisUnits()(itsWorldAxes(1));
   if (!cSysTmp.setWorldAxisUnits(units, True)) {
      throw (AipsError ("WCPolygon::toLCRegion: world axis units of CoordinateSystems do not conform"));
   }


// Create vectors for conversion.  We must pad the world coordinates with the 
// reference values for the other dimensions other than the polygon world axes

   Vector<Double> xLC(itsXWC.nelements());
   Vector<Double> yLC(itsYWC.nelements());
   Vector<Double> world(cSysTmp.referenceValue().copy());
   Vector<Double> pixel(cSysTmp.nPixelAxes());


// Make the conversions to pixels with the supplied CS
// for each polygon vertex.  

   for (uInt i=0; i<itsXWC.nelements(); i++) {
      world(pXWC) = itsXWC(i);
      world(pYWC) = itsYWC(i);
      if (!cSysTmp.toPixel(pixel, world)) {
         throw (AipsError ("WCPolygon::toLCRegion: "+cSysTmp.errorMessage()));
      }

// Assign polygon pixel coordinates

      xLC(i) = pixel(pXLC);
      yLC(i) = pixel(pYLC);
   }


// Return the LCPolygon.  The latticeShape axes are in one to one 
// correspondence with the pixel axes corresponding to 
// worldAxes(0) and worldAxes(1), respectively.

   return LCPolygon(xLC, yLC, latticeShape).cloneRegion();


}


String WCPolygon::className()
{
  return "WCPolygon";
}

String WCPolygon::type() const
{
  return className();
}


