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
#include <aips/Measures/Unit.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/LinearSearch.h>
#include <aips/Utilities/String.h>


// Force the compiler to know about these

typedef Vector<String> gppBug_VectorString;


WCBox::WCBox()
//
//Default constructor
// 
: itsIsOffset(False),
  itsNull(True)
{}

WCBox::WCBox(const Vector<Double>& blcWC,
             const Vector<Double>& trcWC,
             const CoordinateSystem& cSys,
             const Bool isOffset)
//
// Constructor from Double vectors.
// blc and trc are in the order of world
// axes of CS
//
: itsBlcWC(blcWC.copy()),
  itsTrcWC(trcWC.copy()),
  itsCSys(cSys),
  itsIsOffset(isOffset),
  itsNull(False)
{
   AlwaysAssert (itsBlcWC.nelements() <= itsCSys.nWorldAxes(), AipsError);
   AlwaysAssert (itsTrcWC.nelements() <= itsCSys.nWorldAxes(), AipsError);
   AlwaysAssert (itsCSys.nWorldAxes() > 0, AipsError);
   AlwaysAssert (itsIsOffset == False, AipsError);

// Set world axes vectors

   itsBlcWorldAxes.resize(itsBlcWC.nelements());
   for (uInt i=0; i<itsBlcWC.nelements(); i++) itsBlcWorldAxes(i) = i;

   itsTrcWorldAxes.resize(itsTrcWC.nelements());
   for (i=0; i<itsTrcWC.nelements(); i++) itsTrcWorldAxes(i) = i;
}


WCBox::WCBox(const Vector<Float>& blcWC,
             const Vector<Float>& trcWC,
             const CoordinateSystem& cSys,
             const Bool isOffset)
//
// Constructor from Float vectors
// blc and trc are in the order of world
// axes of CS
//
: itsCSys(cSys),
  itsIsOffset(isOffset),
  itsNull(False)
{
   AlwaysAssert (blcWC.nelements() <= itsCSys.nWorldAxes(), AipsError);
   AlwaysAssert (trcWC.nelements() <= itsCSys.nWorldAxes(), AipsError);
   AlwaysAssert (itsCSys.nWorldAxes() > 0, AipsError);
   AlwaysAssert (itsIsOffset == False, AipsError);

// Copy Float to Double

   itsBlcWC.resize(blcWC.nelements());
   itsTrcWC.resize(trcWC.nelements());
   for (uInt i=0; i<itsBlcWC.nelements(); i++) {
      itsBlcWC(i) = Double(blcWC(i));
   }
   for (i=0; i<itsTrcWC.nelements(); i++) {
      itsTrcWC(i) = Double(trcWC(i));
   }

// Set world axes vectors

   itsBlcWorldAxes.resize(itsBlcWC.nelements());
   for (i=0; i<itsBlcWC.nelements(); i++) itsBlcWorldAxes(i) = i;

   itsTrcWorldAxes.resize(itsTrcWC.nelements());
   for (i=0; i<itsTrcWC.nelements(); i++) itsTrcWorldAxes(i) = i;
}


WCBox::WCBox(const Vector<Double>& blcWC,
             const Vector<Double>& trcWC,
             const Vector<uInt> blcWorldAxes,
             const Vector<uInt> trcWorldAxes,
             const CoordinateSystem& cSys,
             const Bool isOffset)
//
// Constructor from Double vectors
// blc and trc are in the order specified
//
: itsBlcWC(blcWC.copy()),
  itsTrcWC(trcWC.copy()),
  itsBlcWorldAxes(blcWorldAxes.copy()),
  itsTrcWorldAxes(trcWorldAxes.copy()),
  itsCSys(cSys),
  itsIsOffset(isOffset),
  itsNull(False)
{
   AlwaysAssert (itsBlcWC.nelements() <= itsCSys.nWorldAxes(), AipsError);
   AlwaysAssert (itsTrcWC.nelements() <= itsCSys.nWorldAxes(), AipsError);
   AlwaysAssert (blcWorldAxes.nelements() == itsBlcWC.nelements(), AipsError);
   AlwaysAssert (trcWorldAxes.nelements() == itsTrcWC.nelements(), AipsError);   
   AlwaysAssert (itsCSys.nWorldAxes() > 0, AipsError);
   AlwaysAssert (itsIsOffset == False, AipsError);
}


WCBox::WCBox(const Vector<Float>& blcWC,
             const Vector<Float>& trcWC,
             const Vector<uInt> blcWorldAxes,
             const Vector<uInt> trcWorldAxes,
             const CoordinateSystem& cSys,
             const Bool isOffset)
//
// Constructor from Double vectors
// blc and trc are in the order specified
//
: itsBlcWorldAxes(blcWorldAxes.copy()),
  itsTrcWorldAxes(trcWorldAxes.copy()),
  itsCSys(cSys),
  itsIsOffset(isOffset),
  itsNull(False)
{
   AlwaysAssert (blcWC.nelements() <= itsCSys.nWorldAxes(), AipsError);
   AlwaysAssert (trcWC.nelements() <= itsCSys.nWorldAxes(), AipsError);
   AlwaysAssert (blcWorldAxes.nelements() == blcWC.nelements(), AipsError);
   AlwaysAssert (trcWorldAxes.nelements() == trcWC.nelements(), AipsError);   
   AlwaysAssert (itsCSys.nWorldAxes() > 0, AipsError);
   AlwaysAssert (itsIsOffset == False, AipsError);

// Copy Float to Double

   itsBlcWC.resize(blcWC.nelements());
   itsTrcWC.resize(trcWC.nelements());
   for (uInt i=0; i<itsBlcWC.nelements(); i++) {
      itsBlcWC(i) = Double(blcWC(i));
   }
   for (i=0; i<itsTrcWC.nelements(); i++) {
      itsTrcWC(i) = Double(trcWC(i));
   }
}




WCBox::WCBox(const LCRegion& region,
             const CoordinateSystem& cSys)
//
// Constructor from the bounding box of an LCRegion
//
: itsCSys(cSys),
  itsIsOffset(False),
  itsNull(False)
{
// Get bounding box

   Slicer boundingBox = region.box();
   IPosition start = boundingBox.start();
   IPosition end = boundingBox.end();
   AlwaysAssert (start.nelements() == itsCSys.nPixelAxes(), AipsError);
   AlwaysAssert (end.nelements() == itsCSys.nPixelAxes(), AipsError);
   AlwaysAssert (itsCSys.nWorldAxes() > 0, AipsError);

// Create vectors for conversions

   Vector<Double> world(itsCSys.nWorldAxes());
   Vector<Double> pixel(itsCSys.nPixelAxes());

// Convert blc

   for (uInt i=0; i<start.nelements(); i++) pixel(i) = start(i);
   if (!itsCSys.toWorld(world, pixel)) {
      throw (AipsError ("WCBox - Cannot convert blc of LCBox because "+itsCSys.errorMessage()));
   }
   itsBlcWC = world;

// Convert trc

   for (i=0; i<end.nelements(); i++) pixel(i) = end(i);
   if (!itsCSys.toWorld(world, pixel)) {
      throw (AipsError ("WCBox - Cannot convert trc of LCBox because "+itsCSys.errorMessage()));
   }
   itsTrcWC = world;


// Set world axes vectors

   itsBlcWorldAxes.resize(itsBlcWC.nelements());
   for (i=0; i<itsBlcWC.nelements(); i++) itsBlcWorldAxes(i) = i;

   itsTrcWorldAxes.resize(itsTrcWC.nelements());
   for (i=0; i<itsTrcWC.nelements(); i++) itsTrcWorldAxes(i) = i;
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
: itsBlcWC(other.itsBlcWC),
  itsTrcWC(other.itsTrcWC),
  itsBlcWorldAxes(other.itsBlcWorldAxes),
  itsTrcWorldAxes(other.itsTrcWorldAxes),
  itsCSys(other.itsCSys),            // This one makes a copy
  itsIsOffset(other.itsIsOffset),
  itsNull(other.itsNull)
{}
 
WCBox& WCBox::operator= (const WCBox& other)
// 
// Assignment (copy semantics)
//
{
   if (this != &other) {
      itsBlcWC.resize(other.itsBlcWC.nelements());
      itsTrcWC.resize(other.itsTrcWC.nelements());
      itsBlcWorldAxes.resize(other.itsBlcWorldAxes.nelements());
      itsTrcWorldAxes.resize(other.itsTrcWorldAxes.nelements());

      itsBlcWC = other.itsBlcWC;
      itsTrcWC = other.itsTrcWC;
      itsBlcWorldAxes = other.itsBlcWorldAxes;
      itsTrcWorldAxes = other.itsTrcWorldAxes;
      itsCSys = other.itsCSys;
      itsIsOffset = other.itsIsOffset;
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

   if (itsIsOffset != that.itsIsOffset) return False;
   if (itsNull != that.itsNull) return False;
   if (itsBlcWC.nelements() != that.itsBlcWC.nelements()) return False;
   if (itsTrcWC.nelements() != that.itsTrcWC.nelements()) return False;
   if (itsBlcWorldAxes.nelements() != that.itsBlcWorldAxes.nelements()) return False;
   if (itsTrcWorldAxes.nelements() != that.itsTrcWorldAxes.nelements()) return False;
   for (uInt i=0; i<itsBlcWC.nelements(); i++) {
      if (!near(itsBlcWC(i),that.itsBlcWC(i))) return False;
      if (!near(itsBlcWorldAxes(i),that.itsBlcWorldAxes(i))) return False;
   }
   for (i=0; i<itsTrcWC.nelements(); i++) {
      if (!near(itsTrcWC(i),that.itsTrcWC(i))) return False;
      if (!near(itsTrcWorldAxes(i),that.itsTrcWorldAxes(i))) return False;
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
// be generated anwyay when trying to reconstruct from
// the record
//
{
// Convert blc/trcAxes to Int from uInt because Glish
// can't handle uInt

   Vector<Int> blcAxes(itsBlcWorldAxes.nelements());
   Vector<Int> trcAxes(itsTrcWorldAxes.nelements());
   for (uInt i=0; i<blcAxes.nelements(); i++) {
      blcAxes(i) = Int(itsBlcWorldAxes(i));
   }
   for (i=0; i<trcAxes.nelements(); i++) {
      trcAxes(i) = Int(itsTrcWorldAxes(i));
   }

   TableRecord rec;
   defineRecordFields(rec, className());
   rec.define ("blc", itsBlcWC);
   rec.define ("trc", itsTrcWC);
   rec.define ("blcAxes", blcAxes);
   rec.define ("trcAxes", trcAxes);
   rec.define ("isOffset", itsIsOffset);
   if (!itsCSys.save(rec, "coordinates")) {
      throw (AipsError ("WCBox::toRecord - could not save Coordinate System"));
   }
   return rec;
}


WCBox* WCBox::fromRecord (const TableRecord& rec,
                          const String&)

{
   CoordinateSystem* pCSys = CoordinateSystem::restore(rec,"coordinates");

// Convert blc/trcAxes to uInt from Int because Glish
// can't handle uInt.  SInce they started life as uInt,
// by definition, the conversion is safe

   Vector<Int> blcAxes = Vector<Int>(rec.asArrayInt("blcAxes"));
   Vector<Int> trcAxes = Vector<Int>(rec.asArrayInt("trcAxes"));
   Vector<uInt> blcAxes2(blcAxes.nelements());
   Vector<uInt> trcAxes2(trcAxes.nelements());
   for (uInt i=0; i<blcAxes.nelements(); i++) {
      blcAxes2(i) = uInt(blcAxes(i));
   }
   for (i=0; i<trcAxes.nelements(); i++) {
      trcAxes2(i) = uInt(trcAxes(i));
   }

   return new WCBox(Vector<Double>(rec.asArrayDouble ("blc")),
                    Vector<Double>(rec.asArrayDouble ("trc")),
                    blcAxes2, trcAxes2, *pCSys,
                    rec.asBool("isOffset"));
   delete pCSys;
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


// Get a copy of the coordinate system so we can mess about with it

   CoordinateSystem cSysTmp(cSys);

// Reorder the construction world blc & trc and units vectors so
// that they are in the order of the corresponding world
// axes of the supplied CoordinateSystem.  

   uInt nWorld = cSysTmp.nWorldAxes();
   Vector<Double> blcWC(cSysTmp.referenceValue().copy());
   Vector<Double> trcWC(cSysTmp.referenceValue().copy());
   Vector<String> blcUnits(cSysTmp.worldAxisUnits().copy());
   Vector<String> trcUnits(cSysTmp.worldAxisUnits().copy());
   Vector<String> units(cSysTmp.worldAxisUnits().copy());
   Int nBlc = itsBlcWorldAxes.nelements();
   Int nTrc = itsTrcWorldAxes.nelements();
   Bool found;
   Int wPt = 0;
   uInt cWorldAxis;
 
   for (uInt i=0; i<nWorld; i++) {
      if (worldAxisTranspose(i) != -1) {

// This given CS world axis is found in the construction CS. But was
// the WCBox constructed with a corresponding value for blc & trc ?

         cWorldAxis = worldAxisTranspose(i);
         wPt = linearSearch(found, itsBlcWorldAxes, cWorldAxis, nBlc);
         if (found) {
            blcWC(i) = itsBlcWC(wPt);
            blcUnits(i) = itsCSys.worldAxisUnits()(cWorldAxis);
         }
         wPt = linearSearch(found, itsTrcWorldAxes, cWorldAxis, nTrc);
         if (found) {
            trcWC(i) = itsTrcWC(wPt);
            trcUnits(i) = itsCSys.worldAxisUnits()(cWorldAxis);
         }
      }
   }
//   cout << "WCBox::toLCRegion - after units = " << units.ac() << endl;
//   cout << "WCBox::toLCRegion - world = " << blcWC.ac() << ", " << trcWC.ac() << endl;


// Convert the world coordinate blc/trc to  absolute  pixels with the 
// supplied CoordinateSystem.    The returned vectors will have as many
// elements as the number of pixel axes in the given CS so that removed
// pixel axes are excluded.

   Vector<Double> blcLC, trcLC;
   if (!setFloatingBox(blcLC, trcLC, blcWC, trcWC, cSysTmp, 
                       blcUnits, trcUnits, itsIsOffset)) {
      throw (AipsError
         ("WCBox::toLCRegion - could not convert world coordinates to absolute pixels"));
   }


// The returned blcLC and trcLC vectors will be in the order of the pixel axes
// in the supplied CS.   The toPixel function of CS will have done this for us.
// It is the responsibility of the caller to have the pixel axes in the
// same order as those of the lattice to which latticeShape pertains.
// Here we fill in any defaults.

   for (i=0; i<cSysTmp.nPixelAxes(); i++) {
      Int worldAxis = cSysTmp.pixelAxisToWorldAxis(i);
      if (worldAxisTranspose(worldAxis) == -1) {
         blcLC(i) = 0.0;
         trcLC(i) = Double(latticeShape(i)-1);
      } else {

// We allow the construction blc/trc to have less elements than there
// are world axes so fill in the defaults here.

         cWorldAxis = worldAxisTranspose(worldAxis);
         if (linearSearch(found, itsBlcWorldAxes, cWorldAxis, nBlc) == -1) {
            blcLC(i) = 0.0;
         }
         if (linearSearch(found, itsTrcWorldAxes, cWorldAxis, nTrc) == -1) {
            trcLC(i) = Double(latticeShape(i) - 1);
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

Bool WCBox::setFloatingBox(Vector<Double>& blcLC,
                           Vector<Double>& trcLC,
                           const Vector<Double>& blcWC,
                           const Vector<Double>& trcWC,
                           CoordinateSystem& cSys,
                           const Vector<String>& blcUnits,
                           const Vector<String>& trcUnits,
                           const Bool isOffset) const
//
// Convert the world coordinates (absolute or offset)
// to absolute pixels.  The blcWC and trcWC must be 
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

   if (isOffset) {
      return False;
   } else {

// Set units and convert

      if (!cSys.setWorldAxisUnits(blcUnits, True)) return False;
      if (!cSys.toPixel(blcLC, blcWC)) return False;
//
      if (!cSys.setWorldAxisUnits(trcUnits, True)) return False;
      if (!cSys.toPixel(trcLC, trcWC)) return False;
   }
   return True;
}
