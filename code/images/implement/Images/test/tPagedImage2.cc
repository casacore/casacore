//# tPagedImage2.cc:  test the regions in the PagedImage class
//# Copyright (C) 1999
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or(at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#include <trial/Images/PagedImage.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/CoordinateUtil.h>

#include <aips/Arrays/Array.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/Vector.h>
#include <aips/Exceptions/Error.h>
#include <aips/Lattices/IPosition.h>
#include <trial/Lattices/LCSlicer.h>
#include <trial/Lattices/LCPagedMask.h>
#include <trial/Lattices/LatticeExpr.h>
#include <trial/Images/ImageRegion.h>
#include <trial/Images/RegionHandler.h>
#include <aips/Tables/TableDesc.h>
#include <aips/Tables/SetupNewTab.h>
#include <aips/Tables/Table.h>
#include <aips/Tables/TableRecord.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/DataType.h>
#include <aips/Utilities/String.h>
#include <iostream.h>


int main()
{
  try {
    IPosition shape(2,32,8);
    LCSlicer box1(IPosition(2,0), shape-1);

// Create a PagedImage.
    {
      PagedImage<Float> pIm(shape, CoordinateUtil::defaultCoords2D(),
			    "tPagedImage2_tmp.img");
      AlwaysAssertExit (! pIm.isMasked());
      AlwaysAssertExit (! pIm.isMaskWritable());
      AlwaysAssertExit (pIm.isWritable());
      AlwaysAssertExit (pIm.isPaged());
      pIm.set(1);
      
// Create a region and add it to the image.
      pIm.defineRegion ("reg1", box1);
      ImageRegion reg = pIm.getRegion("reg1");
      AlwaysAssertExit (reg == ImageRegion(box1));

// Define the region as the default.
      pIm.setDefaultMask ("reg1");
      AlwaysAssertExit (pIm.getDefaultMask() == "reg1");
      AlwaysAssertExit (! pIm.isMasked());         // Slicer does not have mask
      AlwaysAssertExit (! pIm.isMaskWritable());

// Check number of elements.
      LatticeExprNode expr (nelements(pIm));
      AlwaysAssertExit (expr.getDouble() == shape.product());
    }

    { 
      PagedImage<Float> pIm ("tPagedImage2_tmp.img");
      AlwaysAssertExit (pIm.getDefaultMask() == "reg1");
      AlwaysAssertExit (! pIm.isMasked());          //
      AlwaysAssertExit (! pIm.isMaskWritable());
      AlwaysAssertExit (pIm.isWritable());
      AlwaysAssertExit (pIm.isPaged());
      AlwaysAssertExit (pIm.getRegionPtr() != 0);
      ImageRegion reg = pIm.getRegion("reg1");
      AlwaysAssertExit (reg == ImageRegion(box1));

      pIm.setDefaultMask ("");
      AlwaysAssertExit (! pIm.isMasked());
      AlwaysAssertExit (pIm.getRegionPtr() == 0);

      Array<Bool> mask(shape);
      mask = True;
      AlwaysAssertExit (allEQ(pIm.getMask(), mask));

// Create a mask and make it default region.
      pIm.defineRegion ("reg2", RegionHandler::makeMask (pIm, "reg2"));
      pIm.setDefaultMask ("reg2");
      AlwaysAssertExit (pIm.getDefaultMask() == "reg2");
      AlwaysAssertExit (pIm.isMasked());
      AlwaysAssertExit (pIm.isMaskWritable());

// Put a mask and check it is correct.
      mask(IPosition(2,0,0)) = False;
      pIm.putMask (mask);
      AlwaysAssertExit (allEQ(pIm.getMask(), mask));

// Now get the mask as a region and check it is correct.
      ImageRegion reg1 (pIm.getRegion (pIm.getDefaultMask()));
      AlwaysAssertExit (reg1.isLCRegion());
      AlwaysAssertExit (allEQ(reg1.asLCRegion().get(), mask));

// Check number of elements.
      LatticeExprNode expr (nelements(pIm));
      AlwaysAssertExit (expr.getDouble() == shape.product()-1);
    }

    cout<< "ok"<< endl;
  } catch (AipsError x) {
    cerr << "Exception caught: " << x.getMesg() << endl;
    exit(1);
  } end_try;

  exit(0);
}
