//# WCBox.cc: Class to define a world coordinate box region of interest in an image
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

#include <trial/Images/WCBox.h>

#include <aips/Arrays/ArrayLogical.h>
#include <trial/Coordinates/DirectionCoordinate.h>
#include <trial/Coordinates/SpectralCoordinate.h>
#include <aips/Exceptions/Error.h>
#include <aips/Lattices/IPosition.h>
#include <aips/Lattices/Slicer.h>
#include <trial/Lattices/LCBox.h>
#include <trial/Lattices/LCRegion.h>
#include <aips/Mathematics/Math.h>
#include <aips/Quanta/Unit.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Quanta/QuantumHolder.h>
#include <aips/Quanta/QLogical.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/LinearSearch.h>
#include <aips/Utilities/String.h>

#include <iostream.h>
#include <strstream.h>


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
             const RegionType::AbsRelType absRel)
//
// Constructor from Quantities.  blc and trc are in the 
// order of the pixel axes of CS.  We will need a vector of absRel
// for each axis eventually.
//
//
: itsBlc(blc.copy()),
  itsTrc(trc.copy()),
  itsCSys(cSys),
  itsAbsRel(absRel),
  itsNull(False)
{
   AlwaysAssert (itsCSys.nWorldAxes() > 0, AipsError);
   AlwaysAssert (itsCSys.nPixelAxes() > 0, AipsError);
//
   String msg;
   if (itsBlc.nelements() > itsCSys.nPixelAxes()) {
      msg = String("WCBox - you gave more values for the BLC than ") +
            String("there are axes in the CoordinateSystem");
      throw (AipsError (msg));
   }
   if (itsTrc.nelements() > itsCSys.nPixelAxes()) {
      msg = String("WCBox - you gave more values for the TRC than ") +
            String("there are axes in the CoordinateSystem");
      throw (AipsError (msg));
   }
   if (itsAbsRel != RegionType::Abs) {
      throw (AipsError ("WCBox - cannot handle relative coordinates yet"));
   }

// Check units are consistent with the given CoordinateSystem

   unitInit();
   checkUnits(itsBlcPixelAxes, itsBlc, cSys);
   checkUnits(itsTrcPixelAxes, itsTrc, cSys);

}


WCBox::WCBox(const Vector<Quantum<Double> >& blc,
             const Vector<Quantum<Double> >& trc,
             const Vector<uInt>& blcPixelAxes,
             const Vector<uInt>& trcPixelAxes,
             const CoordinateSystem& cSys,
             const RegionType::AbsRelType absRel)
//
// Constructor from Quantities.  blc and trc are in the 
// order of the pixel axes of CS
//
: itsBlc(blc.copy()),
  itsTrc(trc.copy()),
  itsBlcPixelAxes(blcPixelAxes.copy()),
  itsTrcPixelAxes(trcPixelAxes.copy()),
  itsCSys(cSys),
  itsAbsRel(absRel),
  itsNull(False)
{
   AlwaysAssert (itsCSys.nWorldAxes() > 0, AipsError);
   AlwaysAssert (itsCSys.nPixelAxes() > 0, AipsError);
//
   String msg;
   if (itsBlc.nelements() > itsCSys.nPixelAxes()) {
      msg = String("WCBox - you gave more values for the BLC than ") +
            String("there are axes in the CoordinateSystem");
      throw (AipsError (msg));
   }
   if (itsTrc.nelements() > itsCSys.nPixelAxes()) {
      msg = String("WCBox - you gave more values for the TRC than ") +
            String("there are axes in the CoordinateSystem");
      throw (AipsError (msg));
   }
   if (itsBlcPixelAxes.nelements() != itsBlc.nelements()) {
      msg = String("WCBox - you must specify as many BLC values as BLC axes");
      throw (AipsError (msg));
   }
   if (itsTrcPixelAxes.nelements() != itsTrc.nelements()) {
      msg = String("WCBox - you must specify as many TRC values as TRC axes");
      throw (AipsError (msg));
   }
   if (itsAbsRel != RegionType::Abs) {
      throw (AipsError ("WCBox - cannot handle relative coordinates yet"));
   }

// Check units are consistent with the given CoordinateSystem

   unitInit();
   checkUnits(itsBlcPixelAxes, itsBlc, cSys);
   checkUnits(itsTrcPixelAxes, itsTrc, cSys);
}


WCBox::WCBox(const LCRegion& region,
             const CoordinateSystem& cSys)
//
// Constructor from the bounding box of an LCRegion
//
: itsCSys(cSys),
  itsAbsRel(RegionType::Abs),
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
   itsBlcPixelAxes.resize(itsBlc.nelements());
   itsTrcPixelAxes.resize(itsTrc.nelements());
   for (i=0; i<itsCSys.nPixelAxes(); i++) {
      Int worldAxis = itsCSys.pixelAxisToWorldAxis(i);
      if (worldAxis != -1) {
         itsBlc(i) = Quantum<Double>(wBlc(i), itsCSys.worldAxisUnits()(worldAxis));         
         itsTrc(i) = Quantum<Double>(wTrc(i), itsCSys.worldAxisUnits()(worldAxis));         
      } else {
         throw (AipsError ("WCBox - missing world axis in Coordinate System"));
      }
//
      itsBlcPixelAxes(i) = i;
      itsTrcPixelAxes(i) = i;
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
: itsBlc(other.itsBlc),
  itsTrc(other.itsTrc),
  itsBlcPixelAxes(other.itsBlcPixelAxes),
  itsTrcPixelAxes(other.itsTrcPixelAxes),
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
      itsBlc.resize(other.itsBlc.nelements());
      itsTrc.resize(other.itsTrc.nelements());
      itsBlcPixelAxes.resize(other.itsBlcPixelAxes.nelements());
      itsTrcPixelAxes.resize(other.itsTrcPixelAxes.nelements());
//
      itsBlc = other.itsBlc;
      itsTrc = other.itsTrc;
      itsBlcPixelAxes = other.itsBlcPixelAxes;
      itsTrcPixelAxes = other.itsTrcPixelAxes;
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

// Caste
  
   const WCBox& that = (const WCBox&)other;

// Check private data

   if (itsAbsRel != that.itsAbsRel) return False;

   if (itsNull != that.itsNull) return False;
   if (itsBlc.nelements() != that.itsBlc.nelements()) return False;
   if (itsTrc.nelements() != that.itsTrc.nelements()) return False;
   if (itsBlcPixelAxes.nelements() != that.itsBlcPixelAxes.nelements()) return False;
   if (itsTrcPixelAxes.nelements() != that.itsTrcPixelAxes.nelements()) return False;

   uInt i;
   for (i=0; i<itsBlc.nelements(); i++) {
      if (itsBlc(i) != that.itsBlc(i)) return False;
   }
   for (i=0; i<itsTrc.nelements(); i++) {
      if (itsTrc(i) != that.itsTrc(i)) return False;
   }
   if (!itsCSys.near(&(that.itsCSys))) return False;

   return True;
}


WCRegion* WCBox::cloneRegion() const
{
   return new WCBox(*this);
}


TableRecord WCBox::toRecord(const String&) const
//
// Don't bother "itsNull" as if its null, an exception will 
// be generated anyway when trying to reconstruct from
// the record
//
{
// Define pixel units

   unitInit();

// Create record

   TableRecord rec;
   defineRecordFields(rec, className());

// Convert blcPixelAxes and trcPixelAxes Int from uInt because Glish
// can't handle uInt.  Convert to 1-rel.
 
   Vector<Int> pixelAxes(itsBlcPixelAxes.nelements());
   uInt i;
   for (i=0; i<pixelAxes.nelements(); i++) {
      pixelAxes(i) = Int(itsBlcPixelAxes(i)) + 1;
   }
   pixelAxes.resize(itsTrcPixelAxes.nelements());
   for (i=0; i<pixelAxes.nelements(); i++) {
      pixelAxes(i) = Int(itsTrcPixelAxes(i)) + 1;
   }
   rec.define("trcPixelAxes", pixelAxes); 

// Zero-relative, absolute pixel values are converted to one-relative

   Int nBlc = Int(itsBlc.nelements());
   Int nTrc = Int(itsTrc.nelements());
   rec.define("nBlc", nBlc);
   rec.define("nTrc", nTrc);
   rec.define("oneRel", True);
//
   String error;
   TableRecord rec2;
   Quantum<Double> tmpQ;
   Double tmpD;
   Int j;
   for (j=0; j<nBlc; j++) {
      tmpQ = itsBlc(j);
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
         throw (AipsError ("WCBox::toRecord2 - could not save blc because "+error));
      }
      rec.defineRecord("blc"+num, rec2);
   }
   for (j=0; j<nTrc; j++) {
      tmpQ = itsTrc(j);
      if (tmpQ.getUnit() == "pix") {
         tmpD = tmpQ.getValue();
         if (itsAbsRel == RegionType::Abs) tmpD += 1.0;
         tmpQ.setValue(tmpD);
      }
      ostrstream oss;
      oss << j; 
      String num(oss);
      QuantumHolder h(tmpQ);
      if (!h.toRecord(error, rec2)) {
         throw (AipsError ("WCBox::toRecord2 - could not save trc because "+error));
      }
      rec.defineRecord("trc"+num, rec2);
   }
//
   rec.define ("absrel", Int(itsAbsRel));
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

// Get the values

   Bool oneRel = rec.asBool("oneRel");
   RegionType::AbsRelType absRel = RegionType::AbsRelType(rec.asInt("absrel"));
   Int nBlc = rec.asInt("nBlc");
   Int nTrc = rec.asInt("nTrc");
   Vector<Quantum<Double> > blc(nBlc);
   Vector<Quantum<Double> > trc(nTrc);
   Double tmpD;
//
   QuantumHolder h;
   String error;
//
   Int j;
   for (j=0; j<nBlc; j++) {
      ostrstream oss; 
      oss << j; 
      String num(oss);
      const RecordInterface& subRecord = rec.asRecord("blc"+num);
      if (!h.fromRecord(error, subRecord)) {
         throw (AipsError ("WCBox::fromRecord2 - could not recover blc because "+error));
      }
      blc(j) = h.asQuantumDouble();

// Convert from 1-rel to 0-rel for absolute pixel units

      if (oneRel) {
         if (blc(j).getUnit() == "pix") {
            tmpD = blc(j).getValue();
            if (absRel == RegionType::Abs) tmpD -= 1.0;
            blc(j).setValue(tmpD);
         }
      }
   }
   for (j=0; j<nTrc; j++) {
      ostrstream oss; 
      oss << j; 
      String num(oss);
      const RecordInterface& subRecord = rec.asRecord("trc"+num);
      if (!h.fromRecord(error, subRecord)) {
         throw (AipsError ("WCBox::fromRecord2 - could not recover trc because "+error));
      }
      trc(j) = h.asQuantumDouble();

// Convert from 1-rel to 0-rel for absolute pixel units

      if (oneRel) {
         if (trc(j).getUnit() == "pix") {
            tmpD = trc(j).getValue();
            if (absRel == RegionType::Abs) tmpD -= 1.0;
            trc(j).setValue(tmpD);
         }
      }
   }


// Construct WCBox

   if (rec.isDefined("blcPixelAxes") && rec.isDefined("trcPixelAxes")) {
      uInt i;
      Vector<Int> pixelAxes = Vector<Int>(rec.asArrayInt ("blcPixelAxes"));
      Vector<uInt> blcPixelAxes(pixelAxes.nelements());
      for (i=0; i<blcPixelAxes.nelements(); i++) blcPixelAxes(i) = uInt(pixelAxes(i)) - 1;
//
      pixelAxes = Vector<Int>(rec.asArrayInt ("trcPixelAxes"));
      Vector<uInt> trcPixelAxes(pixelAxes.nelements());
      for (i=0; i<trcPixelAxes.nelements(); i++) trcPixelAxes(i) = uInt(pixelAxes(i)) - 1;
//
      pBox = new WCBox(blc, trc, blcPixelAxes, trcPixelAxes, *pCSys, absRel);
   } else {
      pBox = new WCBox(blc, trc, *pCSys, absRel);
   }

//
   delete pCSys;
   return pBox;
}



LCRegion* WCBox::toLCRegion (const CoordinateSystem& cSys,
                             const IPosition& latticeShape) const
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

// There must be as many dimensions in the lattice shape as
// there are pixel axes in the supplied CoordinateSystem,

   AlwaysAssert (latticeShape.nelements() == cSys.nPixelAxes(), AipsError);
   AlwaysAssert (cSys.nWorldAxes() > 0, AipsError);


// Make a world axis map.  worldAxisMap(i) says where world axis i from the
// construction CS is in the supplied CS.  worldAxisTranspose(i) is  the
// location of world axis i  from the supplied CS in the construction CS.   
 
   Vector<Int> worldAxisMap;
   Vector<Int> worldAxisTranspose;
   if (!cSys.worldMap (worldAxisMap, worldAxisTranspose, itsCSys)) {
      throw (AipsError ("WCBox::toLCregion - "+cSys.errorMessage()));
   }


// Get a copy of the given coordinate system so we can mess about with it

   CoordinateSystem cSysTmp(cSys);

// Reorder the construction world blc & trc and units vectors so
// that they are in the order of the corresponding world
// axes of the supplied CoordinateSystem.  

   uInt nWorld = cSysTmp.nWorldAxes();
   Vector<Double> blc(cSysTmp.referenceValue().copy());
   Vector<Double> trc(cSysTmp.referenceValue().copy());
   Vector<String> blcUnits(cSysTmp.worldAxisUnits().copy());
   Vector<String> trcUnits(cSysTmp.worldAxisUnits().copy());
   Vector<String> units(cSysTmp.worldAxisUnits().copy());
   Int nBlc = itsBlcPixelAxes.nelements();
   Int nTrc = itsTrcPixelAxes.nelements();
   Bool found;
   Int pPt = 0;
   Int cWorldAxis;
 
   for (uInt i=0; i<nWorld; i++) {
      cWorldAxis = worldAxisTranspose(i);
      if (cWorldAxis != -1) {

// This given CS world axis is found in the construction CS. Recall
// worldAxisTranspose(i) is  the location of world axis i  from 
// the given CS in the construction CS.    Find the construction pixel 
// axis corresponding to the found world axis

         if (itsCSys.worldAxisToPixelAxis(cWorldAxis) == -1) {

// The construction CS has no pixel axis corresponding to this
// world axis, so there is no appropriate construction blc/trc value

         } else {

// Now see if the WCBox was constructed with blc & trc values for
// this axis (i->cWorldAxis->cPixelAxis).  If so, extract the
// value and units.  Values with pixel units are ignored for now.

            uInt cPixelAxis = itsCSys.worldAxisToPixelAxis(cWorldAxis);
            pPt = linearSearch(found, itsBlcPixelAxes, cPixelAxis, nBlc);
            if (found && itsBlc(pPt).getUnit()!="pix") {
               blc(i) = itsBlc(pPt).getValue();
               blcUnits(i) = itsBlc(pPt).getUnit();
            }
            pPt = linearSearch(found, itsTrcPixelAxes, cPixelAxis, nTrc);
            if (found && itsTrc(pPt).getUnit()!="pix") {
               trc(i) = itsTrc(pPt).getValue();
               trcUnits(i) = itsTrc(pPt).getUnit();
            }
         }
      }
   }
//   cout << "WCBox::toLCRegion - world blc, trc = " << blc.ac() << ", " << trc.ac() << endl;
//   cout << "WCBox::toLCRegion - world units, blc, trc = " << blcUnits.ac() << ", " << trcUnits.ac() << endl;


// Convert the world coordinate blc/trc to  absolute  pixels with the 
// supplied CoordinateSystem.    The returned vectors will have as many
// elements as the number of pixel axes in the given CS so that removed
// pixel axes are excluded.

   Vector<Double> blcLC, trcLC;
   if (!setFloatingBox(blcLC, trcLC, blc, trc, cSysTmp, 
                       blcUnits, trcUnits, itsAbsRel)) {
      throw (AipsError
         ("WCBox::toLCRegion - could not convert world coordinates to absolute pixels"));
   }

//   cout << "WCBox::toLCRegion - pixel = " << blcLC.ac() << ", " << trcLC.ac() << endl;

// The returned blcLC and trcLC vectors will be in the order of the pixel axes
// in the supplied CS.   The toPixel function of CS will have done this for us.
// It is the responsibility of the caller to have the pixel axes in the
// same order as those of the lattice to which latticeShape pertains.
// Here we fill in any defaults.

   for (i=0; i<cSysTmp.nPixelAxes(); i++) {
      Int worldAxis = cSysTmp.pixelAxisToWorldAxis(i);
      cWorldAxis = worldAxisTranspose(worldAxis);

      if (worldAxis==-1) {

// This given CS pixel axis does not have a world axis

         blcLC(i) = 0.0;
         trcLC(i) = Double(latticeShape(i)-1);
      } else {
         if (cWorldAxis==-1) {

// For this given CS pixel axis, there is no corresponding world axis 
// found in the construction CS. 

            blcLC(i) = 0.0;
            trcLC(i) = Double(latticeShape(i)-1);
         } else {
//
// This given CS axis (i->worldAxis) is found in the construction CS. 
// We allow the construction blc/trc to have less elements than there
// are pixel axes in the construction CS.  We fill in the defaults 
// for the missing ones here.

            if (itsCSys.worldAxisToPixelAxis(cWorldAxis)==-1) {

// The construction CS world axis has no pixel axis.  Therefore there
// can be no corresponding specified world value at construction
//
               blcLC(i) = 0.0;
               trcLC(i) = Double(latticeShape(i) - 1);
            } else {

// Finally.  The construction CS world axis has a pixel axis too.
// Let's see if it's in the construction specified list of pixel axes. 
// If the axis is found, and if the units are pixels, replace by that value

               uInt cPixelAxis = itsCSys.worldAxisToPixelAxis(cWorldAxis);
               pPt = linearSearch(found, itsBlcPixelAxes, cPixelAxis, nBlc);
               if (pPt == -1) {
                  blcLC(i) = 0.0;
               } else {
                  if (itsBlc(pPt).getUnit()=="pix") {
                     blcLC(i) = itsBlc(pPt).getValue();
                     convertPixel(blcLC(i), itsAbsRel, 
                                  cSysTmp.referencePixel()(i),
                                  latticeShape(i));
                  }
               }
               pPt = linearSearch(found, itsTrcPixelAxes, cPixelAxis, nTrc);
               if (pPt == -1) {
                  trcLC(i) = Double(latticeShape(i) - 1);
               } else {
                  if (itsTrc(pPt).getUnit()=="pix") {
                     trcLC(i) = itsTrc(pPt).getValue();
                     convertPixel(trcLC(i), itsAbsRel, 
                                  cSysTmp.referencePixel()(i),
                                  latticeShape(i));
                  }
               }
            }
         }
      }
   }


//   cout << "WCBox::toLCRegion - pixel = " << blcLC.ac() << ", " << trcLC.ac() << endl;


// Create the LCBox.  It will throw an exception if blc > trc

   return LCBox(blcLC, trcLC, latticeShape).cloneRegion();

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


void WCBox::checkUnits (Vector<uInt>& pixelAxes, 
                        const Vector<Quantum<Double> >& values,
                        const CoordinateSystem& cSys)
//
// CHeck the units of the given quanta are consistent
// with the CoordinateSystem.  The quanta are in the
// order of the pixel axes.  If on input the pixelAxes vector
// has length > 0, then it specifies the pixel axis order
// of the values.
//
{

// Fill pixel axes if necessary

   uInt i;
   if (pixelAxes.nelements()==0) {
      pixelAxes.resize(values.nelements());
      for (i=0; i<pixelAxes.nelements(); i++) pixelAxes(i) = i;
   }
   Vector<String> units = cSys.worldAxisUnits();
   Quantum<Double> tmp;

// Check units

   for (i=0; i<values.nelements(); i++) {
      Int worldAxis = itsCSys.pixelAxisToWorldAxis(pixelAxes(i));
      if (worldAxis != -1) {
         tmp = values(i);
         if (tmp.getUnit() != "pix" && tmp.getFullUnit() != Unit(units(worldAxis))) {
            throw (AipsError ("WCBox::checkUnits - units of box blc inconsistent with units of Coordinate System"));
         }
      } else {
         throw (AipsError ("WCBox::checkUnits - missing world axis in Coordinate System"));
      }
   }
}



Bool WCBox::setFloatingBox(Vector<Double>& blcLC,
                           Vector<Double>& trcLC,
                           const Vector<Double>& blc,
                           const Vector<Double>& trc,
                           CoordinateSystem& cSys,
                           const Vector<String>& blcUnits,
                           const Vector<String>& trcUnits,
                           const RegionType::AbsRelType absRel) const
//
// Convert the world coordinates (absolute or offset)
// to absolute pixels.  The blc and trc must be 
// checked to be consistent with themselves and the 
// CoordinateSystem before entry.   Note that the output
// vector is of length nPixelAxes; it reflects removed
// pixel axes.  The pixel values are also in the order
// of the pixel axes in the CS (could be different
// to world axes order)
//
{

// Convert corners to absolute pixel


   const uInt nPixelAxes = cSys.nPixelAxes();
   blcLC.resize(nPixelAxes);
   trcLC.resize(nPixelAxes);

   if (absRel == RegionType::Abs) {

// Set units and convert

      if (!cSys.setWorldAxisUnits(blcUnits, True)) return False;
      if (!cSys.toPixel(blcLC, blc)) return False;
//
      if (!cSys.setWorldAxisUnits(trcUnits, True)) return False;
      if (!cSys.toPixel(trcLC, trc)) return False;
   } else {
      return False;
   }
   return True;
}


void WCBox::unitInit() 
{
   static doneUnitInit = False;
   if (!doneUnitInit) {
      UnitMap::putUser("pix",UnitVal(1.0), "pixel units");
      doneUnitInit = True;
   }
}


void WCBox::convertPixel(Double& pixel,
                         const RegionType::AbsRelType absRel,
                         const Double refPix,
                         const Int shape) const
{

   if (absRel == RegionType::RelRef) {
      pixel -= refPix;
   } else if (absRel == RegionType::RelCen) {
      Double centre = shape/2;
      pixel -= centre;
   } else if (absRel == RegionType::Abs) {
//
//   } else if (absRel == RegionType::RelDir) {
//
    } else {
      throw (AipsError ("WCBox::convertPixel - unrecognized relAbs type"));
   }
}
