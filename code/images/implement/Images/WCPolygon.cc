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
#include <aips/Quanta/Unit.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Quanta/QuantumHolder.h>
#include <aips/Quanta/QLogical.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>


WCPolygon::WCPolygon()
//
//Default constructor
// 
: itsNull(True)
{
   unitInit();
}

WCPolygon::WCPolygon(const Vector<Quantum<Double> >& x,
                     const Vector<Quantum<Double> >& y,
                     const Vector<uInt>& pixelAxes,
                     const CoordinateSystem& cSys,
                     const RegionType::AbsRelType absRel)
: itsX(x.copy()),
  itsY(y.copy()),
  itsPixelAxes(pixelAxes.copy()),
  itsCSys(cSys),
  itsAbsRel(absRel),
  itsNull(False)
//
{
   AlwaysAssert (itsCSys.nPixelAxes() >= 2, AipsError);
   AlwaysAssert (itsCSys.nWorldAxes() >= 2, AipsError);
   String msg;
//
   if (itsX.nelements() != itsY.nelements()) {
      msg = String("WCPolygon - the  X and Y vectors must be the same length");
      throw (AipsError (msg));
   }
   if (itsX.nelements() < 3) {
      msg = String("WCPolygon - you must give at least 3 vertices");
      throw (AipsError (msg));
   }
   if (itsPixelAxes.nelements() != 2) {
      msg = String("WCPolygon - you must give 2 pixel axes");
      throw (AipsError (msg));
   }
   if (itsPixelAxes(0) > itsCSys.nPixelAxes()-1 ||
       itsPixelAxes(1) > itsCSys.nPixelAxes()-1) {
      msg = String("WCPolygon - the specified pixel axes are greater than") +
            String("the number of pixel axes in the CoordinateSystem");
      throw (AipsError (msg));
   }
   if (itsPixelAxes(0) == itsPixelAxes(1)) {
      msg = String("WCPolygon - you have specified the same pixel axis twice !");
      throw (AipsError (msg));
   }
   if (itsAbsRel != RegionType::Abs) {
      throw (AipsError ("WCPolygon - cannot handle relative coordinates yet"));
   }         
//
   unitInit();
   Vector<Int> worldAxes;
   checkAxes (worldAxes, itsPixelAxes, itsCSys);

// Check units

   Vector<String> units = cSys.worldAxisUnits();
   Quantum<Double> tmp;
   uInt i;
//
   for (i=0; i<itsX.nelements(); i++) {
      tmp = itsX(i);
      if (tmp.getFullUnit() != Unit(units(worldAxes(0)))) {
         String error = 
          "WCPolygon - units of X vector (" + tmp.getUnit() + 
          ") inconsistent with units of CoordinateSystem (" +
          units(worldAxes(0)) + ")";
         throw (AipsError (error));
      }
   }
//
   for (i=0; i<itsY.nelements(); i++) {
      tmp = itsY(i);
      if (tmp.getFullUnit() != Unit(units(worldAxes(1)))) {
         String error = 
          "WCPolygon - units of Y vector (" + tmp.getUnit() + 
          ") inconsistent with units of CoordinateSystem (" +
          units(worldAxes(1)) + ")";
         throw (AipsError (error));
      }
   }
}


WCPolygon::WCPolygon(const LCPolygon& polyLC,
                     const Vector<uInt>& pixelAxes,
                     const CoordinateSystem& cSys)
//
// Constructor from an LCPolygon
//
: itsPixelAxes(pixelAxes.copy()),
  itsCSys(cSys),
  itsAbsRel(RegionType::Abs),
  itsNull(False)
{
   AlwaysAssert (itsCSys.nPixelAxes() >= 2, AipsError);
   AlwaysAssert (itsCSys.nWorldAxes() >= 2, AipsError);
   String msg;
//
   if (itsPixelAxes.nelements() != 2) {
      msg = String("WCPolygon - you must give 2 pixel axes");
      throw (AipsError (msg));
   }
   if (itsPixelAxes(0) > itsCSys.nPixelAxes()-1 ||
       itsPixelAxes(1) > itsCSys.nPixelAxes()-1) {
      msg = String("WCPolygon - the specified pixel axes are greater than") +
            String("the number of pixel axes in the CoordinateSystem");
      throw (AipsError (msg));
   }
   if (itsPixelAxes(0) == itsPixelAxes(1)) {
      msg = String("WCPolygon - you have specified the same pixel axis twice !");
      throw (AipsError (msg));
   }
   if (itsAbsRel != RegionType::Abs) {
      throw (AipsError ("WCPolygon - cannot handle relative coordinates yet"));
   }
//
   unitInit();
   Vector<Int> worldAxes;
   checkAxes (worldAxes, itsPixelAxes, itsCSys);

// Get polygon x and y 

   Vector<Float> x = polyLC.x();
   Vector<Float> y = polyLC.y();
   itsX.resize(x.nelements());
   itsY.resize(y.nelements());

// Create vectors for conversions
 
   Vector<Double> world(itsCSys.nWorldAxes());
   Vector<Double> pixel(itsCSys.referencePixel().copy());
   String xUnits = itsCSys.worldAxisUnits()(worldAxes(0));
   String yUnits = itsCSys.worldAxisUnits()(worldAxes(1));

// Convert to world

   for (uInt i=0; i<x.nelements(); i++) {
      pixel(itsPixelAxes(0)) = x(i);
      pixel(itsPixelAxes(1)) = y(i);
      if (!itsCSys.toWorld(world,pixel)) {

         throw (AipsError ("WCPolygon - Cannot convert LCPolygon vertices because "+cSys.errorMessage()));
      }

// Assign world coordinates of polygon
 
      itsX(i) = Quantum<Double>(world(worldAxes(0)), xUnits);
      itsY(i) = Quantum<Double>(world(worldAxes(1)), yUnits);
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
: itsX(that.itsX),   
  itsY(that.itsY),
  itsPixelAxes(that.itsPixelAxes),
  itsCSys(that.itsCSys),             // This one makes a copy
  itsAbsRel(that.itsAbsRel),
  itsNull(that.itsNull)
{}
 
WCPolygon& WCPolygon::operator= (const WCPolygon& that)
// 
// Assignment (copy semantics)
//
{
   if (this != &that) {
      itsX.resize(that.itsX.nelements());
      itsY.resize(that.itsY.nelements());
      itsPixelAxes.resize(that.itsPixelAxes.nelements());

      itsX = that.itsX;
      itsY = that.itsY;
      itsPixelAxes = that.itsPixelAxes;
      itsCSys = that.itsCSys;
      itsAbsRel = that.itsAbsRel;
      itsNull = that.itsNull;  
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

   if (itsAbsRel != that.itsAbsRel) return False;
   if (itsNull != that.itsNull) return False;
   if (itsX.nelements() != that.itsX.nelements()) return False;
   if (itsY.nelements() != that.itsY.nelements()) return False;
//
   for (uInt i=0; i<itsX.nelements(); i++) {
      if (itsX(i) != that.itsX(i)) return False;
      if (itsY(i) != that.itsY(i)) return False;
   }
   if (itsPixelAxes.nelements() != that.itsPixelAxes.nelements()) return False;
   for (i=0; i<itsPixelAxes.nelements(); i++) {
      if (itsPixelAxes(i) != that.itsPixelAxes(i)) return False;
   }
   if (!itsCSys.near(&(that.itsCSys))) return False;

   return True;
}
 

WCRegion* WCPolygon::cloneRegion() const
{
   return new WCPolygon(*this);
}


TableRecord WCPolygon::toRecord(const String&) const
{
// Create record

   TableRecord rec;
   defineRecordFields(rec, className());  

// Convert pixelAxes to Int from uInt because Glish
// can't handle uInt.  Convert to 1-rel.
 
   Vector<Int> pAxes(itsPixelAxes.nelements());
   for (uInt i=0; i<pAxes.nelements(); i++) pAxes(i) = Int(itsPixelAxes(i)) + 1;
   rec.define("oneRel", True);
   rec.define ("pixelAxes", pAxes);

// Save polygon. Convert abspix to one rel

   Int nX = Int(itsX.nelements());
   Int nY = Int(itsY.nelements());
   rec.define("nX", nX);
   rec.define("nY", nY);
   Quantum<Double> tmpQ;
   Double tmpD;
//
   String error;
   TableRecord rec2;
   Int j;
   for (j=0; j<nX; j++) {
      tmpQ = itsX(j);
      if (tmpQ.getUnit() == "pix") {
         tmpD = tmpQ.getValue();
         if (itsAbsRel == RegionType::Abs) tmpD += 1.0;  
         tmpQ.setValue(tmpD);
      }
      QuantumHolder h(tmpQ);
      ostrstream oss;
      oss << j;
      String num(oss);
      if (!h.toRecord(error, rec2)) {
         throw (AipsError ("WCPolygon::toRecord - could not save X vector because "+error));
      }
      rec.defineRecord("x"+num, rec2);
   }
   for (j=0; j<nY; j++) {
      tmpQ = itsY(j);
      if (tmpQ.getUnit() == "pix") {
         tmpD = tmpQ.getValue();
         if (itsAbsRel == RegionType::Abs) tmpD += 1.0;  
         tmpQ.setValue(tmpD);
      }
      QuantumHolder h(tmpQ);
      ostrstream oss;
      oss << j;
      String num(oss);
      if (!h.toRecord(error, rec2)) {
         throw (AipsError ("WCPolygon::toRecord - could not save Y vector because "+error));
      }
      rec.defineRecord("y"+num, rec2);
   }
   rec.define ("absrel", Int(itsAbsRel));
   if (!itsCSys.save(rec, "coordinates")) {
      throw (AipsError ("WCPolygon::toRecord: could not save Coordinate System"));
   }

   return rec;
}


WCPolygon* WCPolygon::fromRecord (const TableRecord& rec,
                                  const String&)
{
// Get CoordinateSystem

   CoordinateSystem* pCSys =  CoordinateSystem::restore(rec,"coordinates");

// Get pixel axes and convert to uInt and zero rel.  

   Vector<Int> pixelAxes = Vector<Int>(rec.asArrayInt ("pixelAxes"));
   Vector<uInt> pixelAxes2(pixelAxes.nelements());
   Bool oneRel = rec.asBool("oneRel");
   RegionType::AbsRelType absRel = RegionType::AbsRelType(rec.asInt("absrel"));
   for (uInt i=0; i<pixelAxes.nelements(); i++) pixelAxes2(i) = uInt(pixelAxes(i)) - 1;
//
   WCPolygon* pPoly = 0;
   Int nX = rec.asInt("nX");
   Int nY = rec.asInt("nY");
   Vector<Quantum<Double> > x(nX);
   Vector<Quantum<Double> > y(nY);
   QuantumHolder h;
   String error;
   Double tmpD;
//
   Int j;
   for (j=0; j<nX; j++) {
      ostrstream oss;
      oss << j;
      String num(oss);
      const RecordInterface& subRecord = rec.asRecord("x"+num);
      if (!h.fromRecord(error, subRecord)) {
         throw (AipsError ("WCPolygon::fromRecord - could not recover X vector because "+error));
      }
      x(j) = h.asQuantumDouble();

// Convert from 1-rel to 0-rel for absolute pixel units

      if (oneRel) {     
         if (x(j).getUnit() == "pix") {
            tmpD = x(j).getValue();
            if (absRel == RegionType::Abs) tmpD -= 1.0;
            x(j).setValue(tmpD);   
         }
      }
   }
   for (j=0; j<nY; j++) {
      ostrstream oss;
      oss << j;
      String num(oss);
      const RecordInterface& subRecord = rec.asRecord("y"+num);
      if (!h.fromRecord(error, subRecord)) {
         throw (AipsError ("WCPolygon::fromRecord - could not recover Y vector because "+error));
      }
      y(j) = h.asQuantumDouble();

// Convert from 1-rel to 0-rel for absolute pixel units

      if (oneRel) {     
         if (y(j).getUnit() == "pix") {
            tmpD = y(j).getValue();
            if (absRel == RegionType::Abs) tmpD -= 1.0;
            y(j).setValue(tmpD);   
         }
      }
   }
   for (j=0;j<Int(x.nelements());j++) {
    cout << "x,y=" << x(j) << ", " << y(j) << endl;
   }
   pPoly = new WCPolygon(x, y, pixelAxes2, *pCSys, absRel);
   delete pCSys;
   return pPoly;
}


LCRegion* WCPolygon::toLCRegion (const CoordinateSystem& cSys,
                                 const IPosition& latticeShape) const
{

// Make sure that we are not using a null Polygon 

   if (itsNull) {
      throw (AipsError ("WCPolygon:toLCregion - this is a null WCPolygon object"));
   }

// Some checks.  The supplied CS must have at least two pixel axes,
// although at this point we don't know if they are the right ones !

   AlwaysAssert (latticeShape.nelements() == 2, AipsError);
   AlwaysAssert (cSys.nPixelAxes()>=2, AipsError);
   AlwaysAssert (cSys.nWorldAxes() > 0, AipsError);


// Find the world axes corresponding to the construction pixel axes

   Vector<Int> worldAxes;
   checkAxes(worldAxes, itsPixelAxes, itsCSys);

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
   
   if (worldAxisMap(worldAxes(0)) != -1 &&
       worldAxisMap(worldAxes(1)) != -1) {
/*
      cout << "Construction world axes are "
           << itsCSys.worldAxisNames()(worldAxes(0))
           << " and " 
           << itsCSys.worldAxisNames()(worldAxes(1)) << endl;
      cout << "Supplied polygon world axes are "
           << cSys.worldAxisNames()(worldAxisMap(worldAxes(0)))
           << " and " 
           << cSys.worldAxisNames()(worldAxisMap(worldAxes(1))) << endl;
*/
   } else {
      throw (AipsError 
        ("WCPolygon::toLCRegion: supplied CoordinateSystem does not contain needed world axes"));
   }


// Make a copy of the supplied CoordinateSystem so we can mess 
// about with it

   CoordinateSystem cSysTmp(cSys);

// Assign indexers for output world->pixel conversion vectors
// The world and pixel axes are not necessarily in the same order

   uInt pXWC = worldAxisMap(worldAxes(0));
   uInt pYWC = worldAxisMap(worldAxes(1));
   Int  pXLC = cSysTmp.worldAxisToPixelAxis(pXWC);
   Int  pYLC = cSysTmp.worldAxisToPixelAxis(pYWC);
   if (pXLC == -1 || pYLC == -1) {
      throw (AipsError ("WCPolygon::toLCRegion: could not find output pixel axes in supplied CoordinateSystem"));
   }
//   cout << "pXWC,pYWC=" << pXWC << ", " << pYWC << endl;
//   cout << "pXLC,pYLC=" << pXLC << ", " << pYLC << endl;


// Get units

   Vector<String> units(cSysTmp.worldAxisUnits().copy());
/*
   units(pXWC) = itsCSys.worldAxisUnits()(worldAxes(0));
   units(pYWC) = itsCSys.worldAxisUnits()(worldAxes(1));
   if (!cSysTmp.setWorldAxisUnits(units, True)) {
      throw (AipsError ("WCPolygon::toLCRegion: world axis units of CoordinateSystems do not conform"));
   }
*/

// Create vectors for conversion.  We must pad the world coordinates with the 
// reference values for the other dimensions other than the polygon world axes

   Vector<Double> xLC(itsX.nelements());
   Vector<Double> yLC(itsY.nelements());
   Vector<Double> world(cSysTmp.referenceValue().copy());
   Vector<Double> pixel(cSysTmp.nPixelAxes());

// Make the conversions to pixels with the supplied CS
// for each polygon vertex.  

   for (uInt i=0; i<itsX.nelements(); i++) {
      world(pXWC) = itsX(i).getValue(units(worldAxes(0)));
      world(pYWC) = itsY(i).getValue(units(worldAxes(1)));
      if (!cSysTmp.toPixel(pixel, world)) {
         throw (AipsError ("WCPolygon::toLCRegion: "+cSysTmp.errorMessage()));
      }

// Assign polygon pixel coordinates

      if (itsX(i).getUnit() == "pix") {
         xLC(i) = itsX(i).getValue();
      } else {
         xLC(i) = pixel(pXLC);
      }
      if (itsY(i).getUnit() == "pix") {
         yLC(i) = itsY(i).getValue();
      } else {
         yLC(i) = pixel(pYLC);
      }
   }


// Return the LCPolygon.  The latticeShape axes are in one to one 
// correspondence with the pixel axes corresponding to 
// pixelAxes(0) and pixelAxes(1), respectively.

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


void WCPolygon::checkAxes (Vector<Int>& worldAxes,
                           const Vector<uInt>& pixelAxes,
                           const CoordinateSystem& cSys) const
{
   worldAxes.resize(2);
   worldAxes(0) = cSys.pixelAxisToWorldAxis(pixelAxes(0));
   if (worldAxes(0) == -1) {
      throw (AipsError ("WCPolygon::checkAxes - pixelAxes(0) has no corresponding world axis"));
   }
   worldAxes(1) = cSys.pixelAxisToWorldAxis(pixelAxes(1));
   if (worldAxes(1) == -1) {
      throw (AipsError ("WCPolygon::checkAxes - pixelAxes(1) has no corresponding world axis"));
   }
}

void WCPolygon::unitInit() 
{
   static doneUnitInit = False;
   if (!doneUnitInit) {
      UnitMap::putUser("pix",UnitVal(1.0), "absolute pixel units");
      doneUnitInit = True;
   }
}

