//# WCLELMask.cc: Class to define a mask as a LEL expression
//# Copyright (C) 2000
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

#include <trial/Images/WCLELMask.h>
#include <trial/Images/ImageExprParse.h>
#include <trial/Images/ImageExpr.h>
#include <trial/Lattices/LCLELMask.h>
#include <trial/Lattices/LatticeExpr.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>


WCLELMask::WCLELMask()
: itsImageExpr (0),
  itsLattExpr  (0)
{}

WCLELMask::WCLELMask (const String& command)
: itsCommand   (command),
  itsImageExpr (0),
  itsLattExpr  (0)
{
  try {
    LatticeExprNode node = ImageExprParse::command (command);
    const IPosition shapeOut = node.shape();
    // Get the CoordinateSystem of the expression
    const LELAttribute& attr = node.getAttribute();
    const LELLattCoordBase& lattCoord = attr.coordinates().coordinates();
    if (! lattCoord.hasCoordinates()) {
      itsLattExpr = new LatticeExpr<Bool> (node);
    } else {
      itsImageExpr = new ImageExpr<Bool> (node, command);
      const CoordinateSystem& cSys = itsImageExpr->coordinates();
      uInt naxes = itsImageExpr->ndim();
      for (uInt i=0; i<naxes; i++) {
	addAxisDesc (makeAxisDesc (cSys, i));
      }
    }
  } catch (AipsError x) {
    throw AipsError (x.getMesg() + "\nError in creating WCLELMask");
  }
}

WCLELMask::WCLELMask (const ImageExpr<Bool>& expr)
: itsImageExpr (0),
  itsLattExpr  (0)
{
  itsImageExpr = new ImageExpr<Bool> (expr);
  const CoordinateSystem& cSys = itsImageExpr->coordinates();
  uInt naxes = itsImageExpr->ndim();
  for (uInt i=0; i<naxes; i++) {
    addAxisDesc (makeAxisDesc (cSys, i));
  }
}

WCLELMask::WCLELMask (const LatticeExpr<Bool>& expr)
: itsImageExpr (0),
  itsLattExpr  (0)
{
  itsLattExpr = new LatticeExpr<Bool> (expr);
}

WCLELMask::WCLELMask (const WCLELMask& that)
: itsImageExpr (0),
  itsLattExpr  (0)
{
  operator= (that);
}

WCLELMask::~WCLELMask()
{
  delete itsImageExpr;
  delete itsLattExpr;
}
 
WCLELMask& WCLELMask::operator= (const WCLELMask& that)
{
  if (this != &that) {
    WCRegion::operator= (that);
    delete itsImageExpr;
    itsImageExpr = 0;
    delete itsLattExpr;
    itsLattExpr = 0;
    itsCommand = that.itsCommand;
    if (that.itsImageExpr != 0) {
      itsImageExpr = new ImageExpr<Bool> (*that.itsImageExpr);
    }
    if (that.itsLattExpr != 0) {
      itsLattExpr = new LatticeExpr<Bool> (*that.itsLattExpr);
    }
  }
  return *this;
}

Bool WCLELMask::operator== (const WCRegion& that) const
{
  // Type check
  if (type() != that.type()) return False;
  // Base class
  if (!WCRegion::operator== (that)) return False;
  // Cast
  const WCLELMask& That = dynamic_cast<const WCLELMask&>(that);
  // Check private data
  if (itsCommand != That.itsCommand) return False;
  if ((itsImageExpr==0) != (That.itsImageExpr==0)) return False;
  if ((itsLattExpr==0) != (That.itsLattExpr==0)) return False;
  return True;
}


WCRegion* WCLELMask::cloneRegion() const
{
   return new WCLELMask(*this);
}


uInt WCLELMask::ndim() const
{
  if (itsLattExpr != 0) {
    return itsLattExpr->ndim();
  }
  if (itsImageExpr != 0) {
    return itsImageExpr->ndim();
  }
  return 0;
}

TableRecord WCLELMask::toRecord(const String&) const
{
  // Create record
   TableRecord rec;
   defineRecordFields(rec, className());
   rec.define ("expr", itsCommand);
   return rec;
}


WCLELMask* WCLELMask::fromRecord (const TableRecord& rec,
				  const String&)
{
  // Get the expression.
  String command = rec.asString ("expr");
  return new WCLELMask(command);
}


Bool WCLELMask::canExtend() const
{
    return False;
}


LCRegion* WCLELMask::toLCRegion (const CoordinateSystem& cSys,
				 const IPosition& latticeShape) const
{
  if (itsImageExpr != 0) {
    return WCRegion::toLCRegion (cSys, latticeShape);
  }
  if (! latticeShape.isEqual (itsLattExpr->shape())) {
    throw AipsError ("WCLELMask::toLCRegion - "
		     "shapes of mask (lattice) expression and image mismatch");
  }
  return new LCLELMask (*itsLattExpr);
}

LCRegion* WCLELMask::doToLCRegion (const CoordinateSystem& cSys,
				   const IPosition& latticeShape,
				   const IPosition& pixelAxesMap,
				   const IPosition& outOrder) const
{
  AlwaysAssert (itsImageExpr != 0, AipsError);
  const uInt naxes = pixelAxesMap.nelements();
  const IPosition& shape = itsImageExpr->shape();
  AlwaysAssert (naxes == shape.nelements(), AipsError);
  for (uInt i=1; i<naxes; i++) {
    if (outOrder(i) <= outOrder(i-1)
    ||  pixelAxesMap(i) <= pixelAxesMap(i-1)) {
      throw AipsError ("WCLELMask::toLCRegion - "
		       "the order of the mask axes cannot be changed");
    }
  }
  for (uInt i=0; i<naxes; i++) {
    if (shape(i) != latticeShape(pixelAxesMap(i))) {
      throw AipsError ("WCLELMask::toLCRegion - "
		       "axes lengths of mask expression and image mismatch");
    }
  }
  return new LCLELMask (itsImageExpr->expression());
}


String WCLELMask::className() 
{
   return "WCLELMask";
}

String WCLELMask::type() const
{
   return className();
}
