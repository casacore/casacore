//# WCPolygon.cc: Class to define a 2D polygonal world coordinate region of interest 
//# Copyright (C) 1998,1999,2000,2001
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

#include <casacore/images/Regions/WCPolygon.h>

#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/coordinates/Coordinates/DirectionCoordinate.h>
#include <casacore/coordinates/Coordinates/SpectralCoordinate.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/lattices/LRegions/LCPolygon.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Quanta/Unit.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Quanta/QuantumHolder.h>
#include <casacore/casa/Quanta/QLogical.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/BasicSL/String.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

WCPolygon::WCPolygon()
//
//Default constructor
// 
: itsNull(true)
{
   unitInit();
}

WCPolygon::WCPolygon(const Quantum<Vector<double> >& x,
                     const Quantum<Vector<double> >& y,
                     const IPosition& pixelAxes,
                     const CoordinateSystem& cSys,
                     const RegionType::AbsRelType absRel)
: itsX(x),
  itsY(y),
  itsPixelAxes(pixelAxes),
  itsCSys(cSys),
  itsAbsRel(absRel),
  itsNull(false)
//
{
   AlwaysAssert (itsCSys.nPixelAxes() >= 2, AipsError);
   AlwaysAssert (itsCSys.nWorldAxes() >= 2, AipsError);
   String msg;
//
   Vector<double> xV = itsX.getValue();
   Vector<double> yV = itsY.getValue();
   if (xV.nelements() != yV.nelements()) {
      msg = String("WCPolygon - the X and Y vectors must be the same length");
      throw (AipsError (msg));
   }
   if (xV.nelements() < 3) {
      msg = String("WCPolygon - you must give at least 3 vertices");
      throw (AipsError (msg));
   }
   if (itsPixelAxes.nelements() != 2) {
      msg = String("WCPolygon - you must give 2 pixel axes");
      throw (AipsError (msg));
   }
   if (itsPixelAxes(0) > int32_t(itsCSys.nPixelAxes()-1) ||
       itsPixelAxes(1) > int32_t(itsCSys.nPixelAxes()-1)) {
      msg = String("WCPolygon - the specified pixel axes are greater than") +
            String("the number of pixel axes in the CoordinateSystem");
      throw (AipsError (msg));
   }
   if (itsPixelAxes(0) == itsPixelAxes(1)) {
      msg = String("WCPolygon - you have specified the same pixel axis twice !");
      throw (AipsError (msg));
   }

// Check axes/units

   unitInit();
   Vector<String> units(2);
   units[0] = itsX.getUnit();
   units[1] = itsY.getUnit();

   checkAxes (itsPixelAxes, itsCSys, units);

// Create the axis descriptions.
   
   for (uint32_t i=0; i<itsPixelAxes.nelements(); i++) {
     addAxisDesc (makeAxisDesc (itsCSys, itsPixelAxes(i)));
   }
}


WCPolygon::WCPolygon(const LCPolygon& polyLC,
                     const IPosition& pixelAxes,
                     const CoordinateSystem& cSys)
//
// Constructor from an LCPolygon
//
: itsPixelAxes(pixelAxes),
  itsCSys(cSys),
  itsAbsRel(RegionType::Abs),
  itsNull(false)
{
   AlwaysAssert (itsCSys.nPixelAxes() >= 2, AipsError);
   AlwaysAssert (itsCSys.nWorldAxes() >= 2, AipsError);
   String msg;
//
   if (itsPixelAxes.nelements() != 2) {
      msg = String("WCPolygon - you must give 2 pixel axes");
      throw (AipsError (msg));
   }
   if (itsPixelAxes(0) > int32_t(itsCSys.nPixelAxes()-1) ||
       itsPixelAxes(1) > int32_t(itsCSys.nPixelAxes()-1)) {
      msg = String("WCPolygon - the specified pixel axes are greater than") +
            String("the number of pixel axes in the CoordinateSystem");
      throw (AipsError (msg));
   }
   if (itsPixelAxes(0) == itsPixelAxes(1)) {
      msg = String("WCPolygon - you have specified the same pixel axis twice !");
      throw (AipsError (msg));
   }
   Vector<int32_t> worldAxes(2);
   worldAxes(0) = itsCSys.pixelAxisToWorldAxis(pixelAxes(0));
   worldAxes(1) = itsCSys.pixelAxisToWorldAxis(pixelAxes(1));
   if (worldAxes(0) == -1) {
      throw (AipsError ("WCPolygon - pixelAxes(0) has no corresponding world axis"));
   }
   if (worldAxes(1) == -1) {
      throw (AipsError ("WCPolygon - pixelAxes(1) has no corresponding world axis"));
   }

// Get polygon x and y 

   Vector<float> xP = polyLC.x();
   Vector<float> yP = polyLC.y();

// Create vectors for conversions
 
   Vector<double> world(itsCSys.nWorldAxes());
   Vector<double> pixel(itsCSys.referencePixel().copy());
   String xUnits = itsCSys.worldAxisUnits()(worldAxes(0));
   String yUnits = itsCSys.worldAxisUnits()(worldAxes(1));

// Convert to world

   Vector<double> xW(xP.nelements());
   Vector<double> yW(yP.nelements());
   uint32_t i;
   for (i=0; i<xP.nelements(); i++) {
      pixel(itsPixelAxes(0)) = xP(i);
      pixel(itsPixelAxes(1)) = yP(i);
      if (!itsCSys.toWorld(world,pixel)) {
         throw (AipsError ("WCPolygon - Cannot convert LCPolygon vertices because "
                           + cSys.errorMessage()));
      }

// Assign world coordinates of polygon
 
      xW(i) = world(worldAxes(0));
      yW(i) = world(worldAxes(1));
   }

// Create quantum

   itsX = Quantum<Vector<double> >(xW, xUnits);
   itsY = Quantum<Vector<double> >(yW, yUnits);

// Init units

   unitInit();

// Create the axis descriptions.
   
   for (i=0; i<itsPixelAxes.nelements(); i++) {
     addAxisDesc (makeAxisDesc (itsCSys, itsPixelAxes(i)));
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
: WCRegion(that),
  itsX(that.itsX),   
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
      WCRegion::operator= (that);
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

bool WCPolygon::operator== (const WCRegion& other) const
{  
// Type check

   if (type() != other.type()) return false;

// Caste

   const WCPolygon& that = (const WCPolygon&)other;

// Check private data

   if (itsAbsRel != that.itsAbsRel) return false;
   if (itsNull != that.itsNull) return false;

// Exact match for units and values is required.  That is,
// the check is not done in intrinsic values.  

   if (itsX.getUnit() != that.itsX.getUnit())  return false;
   if (itsY.getUnit() != that.itsY.getUnit())  return false;
//
   Vector<double> x1 = itsX.getValue();
   Vector<double> y1 = itsY.getValue();
   Vector<double> x2 = that.itsX.getValue();
   Vector<double> y2 = that.itsY.getValue();
   if (x1.nelements() != x2.nelements()) return false;
   if (y1.nelements() != y2.nelements()) return false;
//
   uint32_t i;
   for (i=0; i<x1.nelements(); i++) {
      if (x1(i) != x2(i)) return false;
      if (y1(i) != y2(i)) return false;
   }
   if (itsPixelAxes.nelements() != that.itsPixelAxes.nelements()) return false;
   for (i=0; i<itsPixelAxes.nelements(); i++) {
      if (itsPixelAxes(i) != that.itsPixelAxes(i)) return false;
   }
   if (!itsCSys.near(that.itsCSys)) return false;

   return true;
}
 

WCRegion* WCPolygon::cloneRegion() const
{
   return new WCPolygon(*this);
}

bool WCPolygon::canExtend() const  
{
    return false;
}


TableRecord WCPolygon::toRecord(const String&) const
{
// Create record

   unitInit();
   TableRecord rec;
   defineRecordFields(rec, className());  

// Convert to 1-rel.

   rec.define("oneRel", true); 
//
   const uint32_t nAxes = itsPixelAxes.nelements();
   Vector<int32_t> pixelAxes(nAxes);
   pixelAxes = (itsPixelAxes+1).asVector();
   rec.define ("pixelAxes", pixelAxes);

// Save polygon. Convert abspix to one rel

   {
      Vector<double> tmp(itsX.getValue());
      String units = itsX.getUnit();
      if (units == "pix" && itsAbsRel == RegionType::Abs) {
         for (uint32_t i=0; i<tmp.nelements(); i++) tmp(i) += 1.0;
      }
      Quantum<Vector<double> > tmpQ(itsX);
      tmpQ.setValue(tmp);
//
      QuantumHolder h(tmpQ);
      TableRecord rec2;
      String error;
      if (!h.toRecord(error, rec2)) {
         throw (AipsError ("WCPolygon::toRecord - could not save X Quantum vector because "+error));
      }
      rec.defineRecord("x", rec2);
   }
   {
      Vector<double> tmp(itsY.getValue());
      String units = itsY.getUnit();
      if (units == "pix" && itsAbsRel == RegionType::Abs) {
         for (uint32_t i=0; i<tmp.nelements(); i++) tmp(i) += 1.0;
      }
      Quantum<Vector<double> > tmpQ(itsY);
      tmpQ.setValue(tmp);
//
      QuantumHolder h(tmpQ);
      TableRecord rec2;
      String error;
      if (!h.toRecord(error, rec2)) {
         throw (AipsError ("WCPolygon::toRecord - could not save Y Quantum vector because "+error));
      }
      rec.defineRecord("y", rec2);
   }
//
   rec.define ("absrel", int32_t(itsAbsRel));
   if (!itsCSys.save(rec, "coordinates")) {
      throw (AipsError ("WCPolygon::toRecord: could not save Coordinate System"));
   }
//
   return rec;
}


WCPolygon* WCPolygon::fromRecord (const TableRecord& rec,
                                  const String&)
{
// Get CoordinateSystem

   unitInit();
   CoordinateSystem* pCSys =  CoordinateSystem::restore(rec,"coordinates");
   bool oneRel = rec.asBool("oneRel");
   RegionType::AbsRelType absRel = RegionType::AbsRelType(rec.asInt("absrel"));

// Get pixel axes and convert to zero rel.  

   Vector<int32_t> tmp = Vector<int32_t>(rec.toArrayInt ("pixelAxes"));
   IPosition pixelAxes(tmp);
   if (oneRel) pixelAxes -= 1;

// Get the polygon

   Quantum<Vector<double> > xQ;
   Quantum<Vector<double> > yQ;
   String error, units;
//
   {
      QuantumHolder h;
      const RecordInterface& subRecord = rec.asRecord("x");
      if (!h.fromRecord(error, subRecord)) {
         throw (AipsError ("WCPolygon::fromRecord - could not recover X Quantum vector because "+error));
      }
      xQ = h.asQuantumVectorDouble();
      units = xQ.getUnit();

// Convert from 1-rel to 0-rel for absolute pixel units

      if (units=="pix" && absRel==RegionType::Abs && oneRel) {
         Vector<double> x = xQ.getValue();
         for (uint32_t i=0; i<x.nelements(); i++) x(i) -= 1.0;
         xQ.setValue(x);
      }
   }
   {
      QuantumHolder h;
      const RecordInterface& subRecord = rec.asRecord("y");
      if (!h.fromRecord(error, subRecord)) {
         throw (AipsError ("WCPolygon::fromRecord - could not recover Y Quantum vector because "+error));
      }
      yQ = h.asQuantumVectorDouble();
      units = yQ.getUnit();

// Convert from 1-rel to 0-rel for absolute pixel units

      if (units=="pix" && absRel==RegionType::Abs && oneRel) {
         Vector<double> y = yQ.getValue();
         for (uint32_t i=0; i<y.nelements(); i++) y(i) -= 1.0;
         yQ.setValue(y);
      }
   }

// Make WCPolygon

   WCPolygon* pPoly = 0;
   pPoly = new WCPolygon(xQ, yQ, pixelAxes, *pCSys, absRel);
//
   delete pCSys;
   return pPoly;
}


LCRegion* WCPolygon::doToLCRegion (const CoordinateSystem& cSys,
                                   const IPosition& latticeShape,  
                                   const IPosition& pixelAxesMap,
                                   const IPosition& outOrder) const
{

// Make sure that we are not using a null Polygon 

   if (itsNull) {
      throw (AipsError ("WCPolygon:doToLCregion - this is a null WCPolygon object"));
   }


// Find where the polygon axes are in the output CS

   int32_t xPixelAxis = pixelAxesMap(0); 
   int32_t yPixelAxis = pixelAxesMap(1); 
   int32_t xWorldAxis = cSys.pixelAxisToWorldAxis(xPixelAxis);
   int32_t yWorldAxis = cSys.pixelAxisToWorldAxis(yPixelAxis);

//
   String xUnits = itsX.getUnit();
   String yUnits = itsY.getUnit();
   Vector<String> units = cSys.worldAxisUnits();
//
   bool xIsWorld = true;
   bool yIsWorld = true;
   Vector<double> xValue;
   if (xUnits!="pix" && xUnits!="frac") {
      xValue = itsX.getValue(units(xWorldAxis));  
   } else {
      xIsWorld  = false;
      xValue = itsX.getValue();
   }
   Vector<double> yValue;
   if (yUnits!="pix" && yUnits!="frac") {
      yValue = itsY.getValue(units(yWorldAxis));
   } else {
      yIsWorld  = false;
      yValue = itsY.getValue();
   }


// Prepare  world and pixel vectors for conversion per vertex

   const uint32_t nValues = xValue.nelements();
   Vector<double> xLC(nValues);
   Vector<double> yLC(nValues);
   Vector<double> world(cSys.referenceValue().copy());
   Vector<double> pixel(cSys.nPixelAxes());
   Vector<int32_t> absRel(cSys.nWorldAxes());
   absRel = RegionType::Abs;
   absRel(xWorldAxis) = absRel(yWorldAxis) = itsAbsRel;
//
   Vector<double> refPix = cSys.referencePixel();
   for (uint32_t i=0; i<nValues; i++) {

// For pix/frac use reference value.  

      if (xIsWorld) world(xWorldAxis) = xValue(i);
      if (yIsWorld) world(yWorldAxis) = yValue(i);


// Convert from relative world to absolute world if needed

      makeWorldAbsolute (world, absRel, cSys, latticeShape);

// Convert to pixel

      if (!cSys.toPixel(pixel, world)) {
         throw (AipsError ("WCPolygon::doToLCRegion: "+cSys.errorMessage()));
      }

// Assign polygon pixel coordinates

      xLC(i) = pixel(xPixelAxis);
      convertPixel(xLC(i), xValue(i), xUnits, itsAbsRel, refPix(xPixelAxis),
                   latticeShape(xPixelAxis));
      yLC(i) = pixel(yPixelAxis);
      convertPixel(yLC(i), yValue(i), yUnits, itsAbsRel, refPix(yPixelAxis),
                   latticeShape(yPixelAxis));
   }

// Return the LCPolygon.  

   IPosition outShape(2);
   outShape(outOrder(0)) = latticeShape(xPixelAxis);
   outShape(outOrder(1)) = latticeShape(yPixelAxis);


   if (outOrder(0)==0) {
      return new LCPolygon(xLC, yLC, outShape);
   }
   return new LCPolygon(yLC, xLC, outShape);

}


String WCPolygon::className()
{
  return "WCPolygon";
}

String WCPolygon::type() const
{
  return className();
}

} //# NAMESPACE CASACORE - END

