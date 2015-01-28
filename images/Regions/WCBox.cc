//# WCBox.cc: Class to define a world coordinate box region of interest in an image
//# Copyright (C) 1998,1999,2000,2001,2003
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

#include <casacore/images/Regions/WCBox.h>

#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/coordinates/Coordinates/DirectionCoordinate.h>
#include <casacore/coordinates/Coordinates/SpectralCoordinate.h>
#include <casacore/coordinates/Coordinates/StokesCoordinate.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/lattices/LRegions/LCBox.h>
#include <casacore/lattices/LRegions/LCRegion.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Quanta/Unit.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Quanta/QuantumHolder.h>
#include <casacore/casa/Quanta/QLogical.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/LinearSearch.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/measures/Measures/Stokes.h>

#include <casacore/casa/iomanip.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/sstream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

WCBox::WCBox()
//
//Default constructor
// 
: itsNull(True)
{
   unitInit();
}

WCBox::WCBox(const Vector<Quantum<Double> >& blc,
             const Vector<Quantum<Double> >& trc,
             const CoordinateSystem& cSys,
             const Vector<Int>& absRel)
//
// Constructor from Quantities.  blc and trc are in the 
// order of the pixel axes of CS.  Currently relative
// world coordinates are not handled, only relative pixel
// coordinates.
//
: itsBlc(blc.copy()),
  itsTrc(trc.copy()),
  itsCSys(cSys),
  itsAbsRel(absRel.copy()),
  itsNull(False)
{
   AlwaysAssert (itsCSys.nWorldAxes() > 0, AipsError);
   AlwaysAssert (itsCSys.nPixelAxes() > 0, AipsError);
//
   String msg;
   if (itsBlc.nelements() != itsTrc.nelements()) {
      msg = String("WCBox - you gave more values for the blc than the trc");
      throw (AipsError (msg));
   }
   if (itsAbsRel.nelements() != 0 && 
       itsAbsRel.nelements() != itsBlc.nelements()) {
      msg = String("WCBox - you must specify as many values for absRel as blc/trc");
      throw (AipsError (msg));
   }
   if (itsBlc.nelements() > itsCSys.nPixelAxes()) {
      msg = String("WCBox - you gave more values for the blc than ") +
            String("there are axes in the CoordinateSystem");
      throw (AipsError (msg));
   }
   if (itsTrc.nelements() > itsCSys.nPixelAxes()) {
      msg = String("WCBox - you gave more values for the trc than ") +
            String("there are axes in the CoordinateSystem");
      throw (AipsError (msg));
   }

// Set pixelAxes and absRel  (defaults to abs) vectors

   const uInt nAxes = itsBlc.nelements();
   uInt i;
   if (nAxes > 0) {
      itsPixelAxes.resize(nAxes);
      for (i=0; i<nAxes; i++) itsPixelAxes(i) = i;
//
      if (itsAbsRel.nelements() == 0) {
         itsAbsRel.resize(nAxes);
         for (i=0; i<nAxes; i++) itsAbsRel(i) = RegionType::Abs;
      }
   }

// Check units are consistent with the given CoordinateSystem

   unitInit();
   checkUnits(itsPixelAxes, itsBlc, cSys);
   checkUnits(itsPixelAxes, itsTrc, cSys);

// Create the axis descriptions.

   for (i=0; i<nAxes; i++) {
     addAxisDesc (makeAxisDesc (itsCSys, i));
   }
   
}



WCBox::WCBox(const Vector<Quantum<Double> >& blc,
             const Vector<Quantum<Double> >& trc,
             const IPosition& pixelAxes,
             const CoordinateSystem& cSys,
             const Vector<Int>& absRel)
//
// Constructor from Quantities with specification of
// axes. Currently relative world coordinates are not handled, 
// only relative pixel coordinates.
//
: itsBlc(blc.copy()),
  itsTrc(trc.copy()),
  itsPixelAxes(pixelAxes),  
  itsCSys(cSys),
  itsAbsRel(absRel.copy()),
  itsNull(False)
{
   AlwaysAssert (itsCSys.nWorldAxes() > 0, AipsError);
   AlwaysAssert (itsCSys.nPixelAxes() > 0, AipsError);
//
   String msg;
   if (itsBlc.nelements() != itsTrc.nelements()) {
      msg = String("WCBox - you must specify as many blc as trc values");
      throw (AipsError (msg));
   }
   if (itsBlc.nelements() != itsPixelAxes.nelements()) {
      msg = String("WCBox - you must specify as many blc/trc values as pixel axes");
      throw (AipsError (msg));
   }
   if (itsAbsRel.nelements() != 0 && 
       itsAbsRel.nelements() != itsBlc.nelements()) {
      msg = String("WCBox - you must specify as many values for absRel as blc/trc");
      throw (AipsError (msg));
   }
   if (itsPixelAxes.nelements() > itsCSys.nPixelAxes()) {
      msg = String("WCBox - you gave more pixel axes than ") +
            String("there are axes in the CoordinateSystem");
      throw (AipsError (msg));
   }

// If the absRel vector is null, it defaults to absolute 

   const uInt nAxes = itsPixelAxes.nelements();
   uInt i;
   if (itsAbsRel.nelements() == 0 && nAxes > 0) {
      itsAbsRel.resize(nAxes);
      for (i=0; i<nAxes; i++) itsAbsRel(i) = RegionType::Abs;
   }

// Check units are consistent with the given CoordinateSystem

   unitInit();
   checkUnits(itsPixelAxes, itsBlc, cSys);
   checkUnits(itsPixelAxes, itsTrc, cSys);


// Create the axis descriptions.  checkUnits will have complained
// if an itsPixelAxes(i) is invalid

   for (i=0; i<itsPixelAxes.nelements(); i++) {
     addAxisDesc (makeAxisDesc (itsCSys, itsPixelAxes(i)));
   }
}

WCBox::WCBox(const LCRegion& region,
             const CoordinateSystem& cSys)
//
// Constructor from the bounding box of an LCRegion
//
: itsCSys(cSys),
  itsNull(False)
{
   AlwaysAssert (itsCSys.nWorldAxes() > 0, AipsError);
   AlwaysAssert (itsCSys.nPixelAxes() > 0, AipsError);
   String msg;

// Get bounding box

   Slicer boundingBox = region.boundingBox();
   IPosition start = boundingBox.start();
   IPosition end = boundingBox.end();
   if (start.nelements() != itsCSys.nPixelAxes() ||
       end.nelements() != itsCSys.nPixelAxes()) {
      msg = String("WCBox - the dimensions of the LCRegion bounding box must ") +
            String("be the same as the number of pixel axes in the CoordinateSystem");
      throw (AipsError (msg));
   }
   unitInit();

// Create vectors for conversions

   Vector<Double> wBlc(itsCSys.nWorldAxes());
   Vector<Double> wTrc(itsCSys.nWorldAxes());
   Vector<Double> pixel(itsCSys.nPixelAxes());

// Convert corners.  The conversion arranges the world values
// in the order corresponding to the pixel axes.

   uInt i;
   for (i=0; i<start.nelements(); i++) pixel(i) = start(i);
   if (!itsCSys.toWorld(wBlc, pixel)) {
      throw (AipsError ("WCBox - Cannot convert blc of LCBox because "+itsCSys.errorMessage()));
   }
   for (i=0; i<end.nelements(); i++) pixel(i) = end(i);
   if (!itsCSys.toWorld(wTrc, pixel)) {
      throw (AipsError ("WCBox - Cannot convert trc of LCBox because "+itsCSys.errorMessage()));
   }

// Create quanta vectors, and pixel axis vectors

   itsBlc.resize(wBlc.nelements());
   itsTrc.resize(wTrc.nelements());
   itsPixelAxes.resize(itsBlc.nelements());
   itsAbsRel.resize(itsBlc.nelements());
   for (i=0; i<itsCSys.nPixelAxes(); i++) {
      Int worldAxis = itsCSys.pixelAxisToWorldAxis(i);
      if (worldAxis != -1) {
         itsBlc(i) = Quantum<Double>(wBlc(i), itsCSys.worldAxisUnits()(worldAxis));         
         itsTrc(i) = Quantum<Double>(wTrc(i), itsCSys.worldAxisUnits()(worldAxis));         
      } else {
         throw (AipsError ("WCBox - missing world axis in Coordinate System"));
      }
//
      itsPixelAxes(i) = i;
      itsAbsRel(i) = RegionType::Abs;
   }

// Create the axis descriptions.

   for (i=0; i<itsPixelAxes.nelements(); i++) {
     addAxisDesc (makeAxisDesc (itsCSys, i));
   }
}



WCBox::~WCBox()
// 
// Destructor.  Does nothing.
//
{}

   
WCBox::WCBox (const WCBox& other)
//
// Copy constructor (reference semantics).  We don't use
// copy semantics for consistency with other region classes.
// Because the constructor does make a copy of the input vectors
// this is ok because they can't be changed
//
: WCRegion(other),
  itsBlc(other.itsBlc),
  itsTrc(other.itsTrc),
  itsPixelAxes(other.itsPixelAxes),
  itsCSys(other.itsCSys),            // This one makes a copy
  itsAbsRel(other.itsAbsRel),
  itsNull(other.itsNull)
{}
 
WCBox& WCBox::operator= (const WCBox& other)
// 
// Assignment (copy semantics)
//
{
   if (this != &other) {
      WCRegion::operator= (other);
      itsBlc.resize(other.itsBlc.nelements());
      itsTrc.resize(other.itsTrc.nelements());
      itsPixelAxes.resize(other.itsPixelAxes.nelements());
      itsAbsRel.resize(other.itsAbsRel.nelements());
//
      itsBlc = other.itsBlc;
      itsTrc = other.itsTrc;
      itsPixelAxes = other.itsPixelAxes;
      itsCSys = other.itsCSys;
      itsAbsRel = other.itsAbsRel;
      itsNull = other.itsNull;
    }
    return *this;
}

Bool WCBox::operator== (const WCRegion& other) const
{
// Type check

   if (type() != other.type()) return False;

// Base class

   if (!WCRegion::operator== (other)) return False;

// Caste
  
   const WCBox& that = (const WCBox&)other;

// Check private data

   if (itsNull != that.itsNull) return False;
   if (itsBlc.nelements() != that.itsBlc.nelements()) return False;
   if (itsTrc.nelements() != that.itsTrc.nelements()) return False;
   if (itsPixelAxes.nelements() != that.itsPixelAxes.nelements()) return False;

// Exact match for units and values is required.  That is,
// the check is not done in intrinsic values.

   for (uInt i=0; i<itsBlc.nelements(); i++) {
      if (itsBlc(i).getValue() != that.itsBlc(i).getValue()) return False;
      if (itsBlc(i).getUnit() != that.itsBlc(i).getUnit()) return False;
//
      if (itsTrc(i).getValue() != that.itsTrc(i).getValue()) return False;
      if (itsTrc(i).getUnit() != that.itsTrc(i).getUnit()) return False;
//
      if (itsPixelAxes(i) != that.itsPixelAxes(i)) return False;
      if (itsAbsRel(i) != that.itsAbsRel(i)) return False;
   }
   if (!itsCSys.near(that.itsCSys)) return False;

   return True;
}


WCRegion* WCBox::cloneRegion() const
{
   return new WCBox(*this);
}

WCBox WCBox::splitBox (const IPosition& axes) const
{
   uInt nAxes = axes.nelements();
   Vector<Quantum<Double> > blc(nAxes);
   Vector<Quantum<Double> > trc(nAxes);
   IPosition pixelAxes(nAxes);
   Vector<Int> absRel(nAxes);
   for (uInt i=0; i<nAxes; i++) {
      uInt axis = axes(i);
      AlwaysAssert (axis < itsBlc.nelements(), AipsError);
      blc(i) = itsBlc(axis);
      trc(i) = itsTrc(axis);
      absRel(i) = itsAbsRel(axis);
      pixelAxes(i) = itsPixelAxes(axis);
   }
   return WCBox (blc, trc, pixelAxes, itsCSys, absRel);
}

TableRecord WCBox::toRecord(const String&) const
//
// Don't bother "itsNull" as if its null, an exception will 
// be generated anyway when trying to reconstruct from
// the record. 
//
// pixelAxes, blc, trc, absRel will all be the same length
//
{
// Define pixel units

   unitInit();

// Create record

   TableRecord rec;
   defineRecordFields(rec, className());
   rec.define ("absrel", itsAbsRel);
   rec.define("oneRel", True);
//
   const uInt nAxes = itsPixelAxes.nelements();
   Vector<Int> pixelAxes(nAxes);
   if (nAxes > 0) pixelAxes = (itsPixelAxes+1).asVector();
   rec.define("pixelAxes", pixelAxes);
//
   String error;
   TableRecord recBlc, recTrc, recT;
   Quantum<Double> tmpQ;
   Double tmpD;
//
   for (uInt j=0; j<nAxes; j++) {
      tmpQ = itsBlc(j);
      if (tmpQ.getUnit() == "pix") {
         tmpD = tmpQ.getValue();
         if (itsAbsRel(j) == RegionType::Abs) tmpD += 1.0;
         tmpQ.setValue(tmpD);
      }
      QuantumHolder h(tmpQ);
      if (!h.toRecord(error, recT)) {
         throw (AipsError ("WCBox::toRecord - could not save blc because "+error));
      }
      recBlc.defineRecord(j, recT);
   }
   rec.defineRecord("blc", recBlc);

//
   for (uInt j=0; j<nAxes; j++) {
      tmpQ = itsTrc(j);
      if (tmpQ.getUnit() == "pix") {
         tmpD = tmpQ.getValue();
         if (itsAbsRel(j) == RegionType::Abs) tmpD += 1.0;
         tmpQ.setValue(tmpD);
      }
      QuantumHolder h(tmpQ);
      if (!h.toRecord(error, recT)) {
         throw (AipsError ("WCBox::toRecord - could not save blc because "+error));
      }
      recTrc.defineRecord(j, recT);
   }
   rec.defineRecord("trc", recTrc);

//
   if (!itsCSys.save(rec, "coordinates")) {
      throw (AipsError ("WCBox::toRecord - could not save Coordinate System"));
   }
   return rec;
}



WCBox* WCBox::fromRecord (const TableRecord& rec,
                          const String&)
{
// Get coordinate system

   CoordinateSystem* pCSys = CoordinateSystem::restore(rec,"coordinates");
   WCBox* pBox;

// Define pixel units

   unitInit();

// See if the values in the record are 1-rel or 0-rel

   Bool oneRel = rec.asBool("oneRel");

// Get the pixelAxes.  Pixel things must be converted to zero rel

   Vector<Int> axes = Vector<Int>(rec.toArrayInt ("pixelAxes"));
   const uInt nAxes = axes.nelements();
   IPosition pixelAxes(nAxes);
   for (uInt i=0; i<nAxes; i++) {
      pixelAxes(i) = axes(i);
      if (oneRel) pixelAxes(i) -= 1;
   }

// Get the absRel vector

   Vector<Int> absRel = Vector<Int>(rec.toArrayInt ("absrel"));
   uInt nAbsRel = absRel.nelements();

// Get the blc and trc quantity vectors

   String error;
   Vector<Quantum<Double> > blc, trc;
   Double tmpD;
   QuantumHolder h;
//
   uInt j;
   const RecordInterface& blcRec = rec.asRecord("blc");
   const RecordInterface& trcRec = rec.asRecord("trc");
   if (blcRec.nfields() != trcRec.nfields()) {
      throw (AipsError ("WCBox::fromRecord - blc and trc must be the same length"));
   }
//
   uInt nFields = blcRec.nfields();
   if (nAbsRel == 0) {
      if (nFields > 0) {
         absRel.resize(nFields);
         absRel = RegionType::Abs;
      }
   } else {
      if (nAbsRel != nFields) {
         throw (AipsError ("WCBox::fromRecord - absrel must be same length as blc/trc"));
      }
   }
//
   if (nFields > 0) {
      blc.resize(nFields);
      trc.resize(nFields);
//
      for (j=0; j<nFields; j++) {
         const RecordInterface& subRec1 = blcRec.asRecord(j);
         if (!h.fromRecord(error, subRec1)) {
           throw (AipsError ("WCBox::fromRecord - could not recover blc because "+error));
         }
         blc(j) = h.asQuantumDouble();
         if (oneRel && blc(j).getUnit() == "pix") {
            tmpD = blc(j).getValue();
            if (absRel(j) == RegionType::Abs) tmpD -= 1.0;
            blc(j).setValue(tmpD);
         }
//
         const RecordInterface& subRec2 = trcRec.asRecord(j);
         if (!h.fromRecord(error, subRec2)) {
           throw (AipsError ("WCBox::fromRecord - could not recover trc because "+error));
         }
         trc(j) = h.asQuantumDouble();
         if (oneRel && trc(j).getUnit() == "pix") {
            tmpD = trc(j).getValue();
            if (absRel(j) == RegionType::Abs) tmpD -= 1.0;
            trc(j).setValue(tmpD);
         }
      }
   }

// Make box

   pBox = new WCBox(blc, trc, pixelAxes, *pCSys, absRel);

//
   delete pCSys;
   return pBox;
}



Bool WCBox::canExtend() const
{
    return True;
}

LCRegion* WCBox::doToLCRegion (const CoordinateSystem& cSys,
                               const IPosition& latticeShape,
                               const IPosition& pixelAxesMap,
                               const IPosition& outOrder) const
// 
// It is the callers responsibility to ensure that the
// order of the pixel axes in the supplied CS is the same 
// as that of the Lattice for which the shape is supplied.
//
{
 
// Make sure that we are not using a null WCBox

   if (itsNull) {
      throw (AipsError ("WCBox:toLCregion - this is a null WCBox object"));
   }

// Get a copy of the given coordinate system so we can mess about with it

   CoordinateSystem cSysTmp(cSys);

// World coordinate vectors

   Vector<Double> wBlc(cSysTmp.referenceValue().copy());
   Vector<String> blcUnits(cSysTmp.worldAxisUnits().copy());
   Vector<Double> wTrc(cSysTmp.referenceValue().copy());
   Vector<String> trcUnits(cSysTmp.worldAxisUnits().copy());

// Reorder world coordinates for output CS and set units.
// "funny" values and units (default, pix, frac) are handled later and are 
// ignored at this stage

   uInt i;
   for (i=0; i<itsPixelAxes.nelements(); i++) {
      Int latticePixelAxis = pixelAxesMap(i);
      Quantity value = itsBlc(i);   
      if (value.getUnit() != "pix" && value.getUnit() != "frac" &&
          value.getUnit() != "default") {
         Int worldAxis = cSysTmp.pixelAxisToWorldAxis(latticePixelAxis);
         wBlc(worldAxis) = value.getValue();
         blcUnits(worldAxis) = value.getUnit();
      }
      value = itsTrc(i);   
      if (value.getUnit() != "pix" && value.getUnit() != "frac" &&
          value.getUnit() != "default") {
         Int worldAxis = cSysTmp.pixelAxisToWorldAxis(latticePixelAxis);
         wTrc(worldAxis) = value.getValue();
         trcUnits(worldAxis) = value.getUnit();
      }
   }

// Convert to pixels for all pixel axes of cSysTmp for blc and trc

   if (!cSysTmp.setWorldAxisUnits(blcUnits)) {
      throw (AipsError ("WCBox:doToLCregion - blc units are inconsistent with CoordinateSystem"));
   }
   makeWorldAbsolute (wBlc, itsAbsRel, cSysTmp, latticeShape);
   Vector<Double> pBlc;
   if (!cSysTmp.toPixel(pBlc, wBlc)) {
      throw (AipsError ("WCBox:doToLCregion - conversion of blc to pixel coordinates failed"));
   }
//
   if (!cSysTmp.setWorldAxisUnits(trcUnits)) {
      throw (AipsError ("WCBox:doToLCregion - trc units are inconsistent with CoordinateSystem"));
   }
   makeWorldAbsolute (wTrc, itsAbsRel, cSysTmp, latticeShape);
   Vector<Double> pTrc;
   if (!cSysTmp.toPixel(pTrc, wTrc)) {
      throw (AipsError ("WCBox:doToLCregion - conversion of trc to pixel coordinates failed"));
   }

// Now recover only those values from pBlc that we actually
// want.  Here we handle frac/pixel/default units as well.

   Vector<Double> refPix = cSysTmp.referencePixel();
   const uInt nAxes = outOrder.nelements();
   Vector<Double> outBlc(nAxes);
   Vector<Double> outTrc(nAxes);
   IPosition outShape(nAxes);
   for (i=0; i<itsPixelAxes.nelements(); i++) {
      Int latticePixelAxis = pixelAxesMap(i);
//
      Double pixel = pBlc(latticePixelAxis);
      convertPixel(pixel, itsBlc(i), itsAbsRel(i), refPix(i),
                   latticeShape(latticePixelAxis), True);
      outBlc(outOrder(i)) = pixel;
//
      pixel = pTrc(latticePixelAxis);
      convertPixel(pixel, itsTrc(i), itsAbsRel(i), refPix(i),
                   latticeShape(latticePixelAxis), False);
      outTrc(outOrder(i)) = pixel;
//
      outShape(outOrder(i)) = latticeShape(latticePixelAxis);
   }
//
   for (i=itsPixelAxes.nelements(); i<nAxes; i++) {
      Int latticePixelAxis = pixelAxesMap(i);
      outBlc(outOrder(i)) = 0;
      outTrc(outOrder(i)) = latticeShape(latticePixelAxis) - 1;
      outShape(outOrder(i)) = latticeShape(latticePixelAxis);
   }

// Create the LCBox.  It will throw an exception if blc > trc

   return new LCBox(outBlc, outTrc, outShape);

}


String WCBox::className() 
{
   return "WCBox";
}

String WCBox::type() const
{
   return className();
}

// Private functions


void WCBox::checkUnits (const IPosition& pixelAxes, 
                        const Vector<Quantum<Double> >& values,
                        const CoordinateSystem& cSys)
//
// CHeck the units of the given quanta are consistent
// with the CoordinateSystem.  The quanta are in the
// order of the pixel axes.   
//
{
   if (pixelAxes.nelements() != values.nelements()) {
      throw (AipsError ("WCBox::checkUnits - internal error"));
   }
//
   Vector<String> units = cSys.worldAxisUnits();
   Quantum<Double> tmp;


// Check units

   for (uInt i=0; i<values.nelements(); i++) {
      Int worldAxis = itsCSys.pixelAxisToWorldAxis(pixelAxes(i));
      if (worldAxis != -1) {
         tmp = values(i);
         if (tmp.getUnit() != "pix" && tmp.getUnit() != "default" &&
             tmp.getUnit() != "def" && tmp.getUnit() != "frac") {
            if (tmp.getFullUnit() != Unit(units(worldAxis))) {
               String msg = String("WCBox::checkUnits - units of box blc (") + tmp.getUnit() + 
                            String(") inconsistent with units of Coordinate System (") +
                            units(worldAxis) + String(")");
               throw (AipsError (msg));
            }
         }
      } else {
         throw (AipsError ("WCBox::checkUnits - missing world axis in Coordinate System"));
      }
   }
}



void WCBox::convertPixel(Double& pixel,
                         const Quantum<Double>& value,
                         const Int absRel,
                         const Double refPix,
                         const Int shape,
                         const Bool isBlc) const
{

// Defaults get 0 or shape-1

   if (value.getUnit() == "default") {
      if (isBlc) {
         pixel = 0;
      } else {
         pixel = shape - 1;
      }
   } else {

// Deal with pixel or fractional coordinates

      Bool world = True;
      if (value.getUnit() == "pix") {
         pixel = value.getValue();
         world = False;
      } else if (value.getUnit() == "frac") {
         pixel = value.getValue() * shape;
	 if (!isBlc) {
 	    pixel -= 1;
	 }
         world = False;
      }

// Convert to absolute pixel; rel = abs - ref

      if (!world) {
         if (absRel == RegionType::RelRef) {
            pixel += refPix;
         } else if (absRel == RegionType::RelCen) {
            pixel += Double(shape)/2;
         }
      }
   }
}

} //# NAMESPACE CASACORE - END

