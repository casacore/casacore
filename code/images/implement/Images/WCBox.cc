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
#include <aips/Utilities/String.h>


// Force the compiler to know about these

typedef Vector<String> gppBug_VectorString;


WCBox::WCBox()
//
//Default constructor
// 
: itsIsOffset(False)
{}

WCBox::WCBox(const Vector<Double>& blcWC,
             const Vector<Double>& trcWC,
             const CoordinateSystem& cSys,
             const Bool isOffset)
//
// Constructor from Double vectors
//
: itsBlcWC(blcWC.copy()),
  itsTrcWC(trcWC.copy()),
  itsCSys(cSys),
  itsIsOffset(isOffset)
{
   AlwaysAssert (itsBlcWC.nelements() == itsTrcWC.nelements(), AipsError);
   AlwaysAssert (itsBlcWC.nelements() == itsCSys.nWorldAxes(), AipsError);
   AlwaysAssert (itsBlcWC.nelements() > 0, AipsError);
   AlwaysAssert (itsCSys.nWorldAxes() > 0, AipsError);
   AlwaysAssert (itsIsOffset == False, AipsError);
}


WCBox::WCBox(const Vector<Float>& blcWC,
             const Vector<Float>& trcWC,
             const CoordinateSystem& cSys,
             const Bool isOffset)
//
// Constructor from Float vectors
//
: itsCSys(cSys),
  itsIsOffset(isOffset)
{
   AlwaysAssert (blcWC.nelements() == trcWC.nelements(), AipsError);
   AlwaysAssert (blcWC.nelements() == itsCSys.nWorldAxes(), AipsError);
   AlwaysAssert (blcWC.nelements() > 0, AipsError);
   AlwaysAssert (itsCSys.nWorldAxes() > 0, AipsError);
   AlwaysAssert (itsIsOffset == False, AipsError);


// Copy Float to Double

   itsBlcWC.resize(blcWC.nelements());
   itsTrcWC.resize(blcWC.nelements());
   for (uInt i=0; i<itsBlcWC.nelements(); i++) {
      itsBlcWC(i) = Double(blcWC(i));
      itsTrcWC(i) = Double(trcWC(i));
   }
}

WCBox::WCBox(const LCRegion& region,
             const CoordinateSystem& cSys)
//
// Constructor from the bounding box of an LCRegion
//
: itsCSys(cSys),
  itsIsOffset(False)
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

}




WCBox::~WCBox()
// 
// Destructor.  Does nothing.
//
{}

   
WCBox::WCBox (const WCBox& that)
//
// Copy constructor (reference semantics).  We don't use
// copy semantics for consistency with other region classes.
// Because the constructor does make a copy of the input vectors
// this is ok because they can't be changed
//
: itsBlcWC(that.itsBlcWC),
  itsTrcWC(that.itsTrcWC),
  itsCSys(that.itsCSys),            // This one makes a copy
  itsIsOffset(that.itsIsOffset)
{}
 
WCBox& WCBox::operator= (const WCBox& that)
// 
// Assignment (copy semantics)
//
{
   if (this != &that) {
      itsBlcWC.resize(that.itsBlcWC.nelements());
      itsTrcWC.resize(that.itsTrcWC.nelements());

      itsBlcWC = that.itsBlcWC;
      itsTrcWC = that.itsTrcWC;
      itsCSys = that.itsCSys;
      itsIsOffset = that.itsIsOffset;
    }
    return *this;
}



WCBox* WCBox::cloneRegion() const
{
   return new WCBox(*this);
}


TableRecord WCBox::toRecord(const String&) const
{
   TableRecord rec;
   defineRecordFields(rec, className());
   rec.define ("blc", itsBlcWC);
   rec.define ("trc", itsTrcWC);
   rec.define ("offset", itsIsOffset);
   if (!itsCSys.save(rec, "CoordinateSystem")) {
      throw (AipsError ("WCBox::toRecord: could not save CoordinateSystem"));
   }

   return rec;
}


WCBox* WCBox::fromRecord (const TableRecord& rec,
                          const String&)

{
   return new WCBox(Vector<Double>(rec.asArrayDouble ("blc")),
                    Vector<Double>(rec.asArrayDouble ("trc")),
                    *(CoordinateSystem::restore(rec,"CoordinateSystem")),
                    rec.asBool("offset"));
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

   AlwaysAssert (itsBlcWC.nelements() > 0, AipsError);
 

// There must be as many dimensions in the lattice shape as
// there are pixel axes in the supplied CoordinateSystem,

   AlwaysAssert (latticeShape.nelements() == cSys.nPixelAxes(), AipsError);
   AlwaysAssert (cSys.nWorldAxes() > 0, AipsError);


// Make a world axis map.  worldAxisMap(i) says where world axis i from
// the construction CS is in the supplied CS.  worldAxisTranspose(i) is 
// the location of world axis i  from the supplied CS in the construction 
// CS.   
// 
 
   Vector<Int> worldAxisMap;
   Vector<Int> worldAxisTranspose;
   if (!cSys.worldMap (worldAxisMap, worldAxisTranspose, itsCSys)) {
      throw (AipsError ("WCBox::toLCregion: "+cSys.errorMessage()));
   }


// Get a copy of the coordinate system so we can mess about with it

   CoordinateSystem cSysTmp(cSys);


// Reorder the construction world blc & trc and units vectors so
// that they are in the order of the corresponding world
// axes of the supplied CoordinateSystem.  

   uInt nWorld = cSys.nWorldAxes();
   Vector<Double> blcWC(cSys.referenceValue().copy());
   Vector<Double> trcWC(cSys.referenceValue().copy());
   Vector<String> units(cSys.worldAxisUnits().copy());

//   cout << "WCBox::toLCRegion - before units = " << units.ac() << endl;

   for (uInt i=0; i<nWorld; i++) {
      if (worldAxisTranspose(i) != -1) {

// This world axis is found in the construction CS

         blcWC(i) = itsBlcWC(worldAxisTranspose(i));
         trcWC(i) = itsTrcWC(worldAxisTranspose(i));
         units(i) = itsCSys.worldAxisUnits()(worldAxisTranspose(i));
      }
   }
//   cout << "WCBox::toLCRegion - after units = " << units.ac() << endl;
//   cout << "WCBox::toLCRegion - world = " << blcWC.ac() << ", " << trcWC.ac() << endl;

// Set units

   if (!cSysTmp.setWorldAxisUnits(units, True)) {
      throw (AipsError ("WCBox::toLCRegion: world axis units of CoordinateSystems do not conform"));
   }


// Convert the world coordinate blc/trc to  absolute  pixels with the 
// supplied CoordinateSystem.    The returned vectors will have as many
// elements as the number of pixel axes in the given CS so that removed
// pixel axes are excluded.

   Vector<Double> blcLC, trcLC;
   if (!setFloatingBox(blcLC, trcLC, blcWC, trcWC, cSysTmp, itsIsOffset)) {
      throw (AipsError ("WCBox::toLCRegion: could not convert world coordinates to absolute pixels"));
   }


// The returned blcLC and trcLC vectors will be in the order of the pixel axes
// in the supplied CS.   The toPixel function of CS will have done this for us.
// It is the responsibility of the caller to have the pixel axes in the
// same order as those of the lattice to which latticeShape pertains.

   for (i=0; i<cSys.nPixelAxes(); i++) {
      Int worldAxis = cSys.pixelAxisToWorldAxis(i);
      if (worldAxisTranspose(worldAxis) == -1) {
         blcLC(i) = 0.0;
         trcLC(i) = Double(latticeShape(i)-1);
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

// Private functions

Bool WCBox::setFloatingBox(Vector<Double>& blcLC,
                           Vector<Double>& trcLC,
                           const Vector<Double>& blcWC,
                           const Vector<Double>& trcWC,
                           const CoordinateSystem& cSys,
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
   Bool ok;

   if (isOffset) {
      ok = False;
   } else {
      ok = cSys.toPixel(blcLC, blcWC);
      if (ok) ok = cSys.toPixel(trcLC, trcWC);
   }
   return ok;
}
