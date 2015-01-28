//# tPagedImage2.cc:  test the regions in the PagedImage class
//# Copyright (C) 1999,2000,2001
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

#include <casacore/images/Images/PagedImage.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/coordinates/Coordinates/CoordinateUtil.h>

#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Arrays/IPosition.h>
#include <casacore/lattices/LRegions/LCSlicer.h>
#include <casacore/lattices/LRegions/LCPagedMask.h>
#include <casacore/lattices/LEL/LatticeExpr.h>
#include <casacore/images/Regions/ImageRegion.h>
#include <casacore/images/Regions/RegionHandler.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/SetupNewTab.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/casa/OS/File.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
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
      AlwaysAssertExit (pIm.isWritable());
      AlwaysAssertExit (pIm.isPaged());
      AlwaysAssertExit (pIm.canDefineRegion());
      AlwaysAssertExit (! pIm.hasPixelMask());
      pIm.set(1);
      
// Create a region as a mask and add it to the image.
// The region won't be found in the regions.
      pIm.defineRegion ("reg1", box1, RegionHandler::Masks);
      ImageRegion reg = pIm.getRegion("reg1");
      AlwaysAssertExit (reg == ImageRegion(box1));
      AlwaysAssertExit (pIm.getImageRegionPtr
                            ("reg1", RegionHandler::Regions, False) == 0);

// Define the region as the default.
      pIm.setDefaultMask ("reg1");
      AlwaysAssertExit (pIm.getDefaultMask() == "reg1");
      AlwaysAssertExit (! pIm.isMasked());        // Slicer does not have mask
      AlwaysAssertExit (! pIm.hasPixelMask());

// Check number of elements.
      LatticeExprNode expr (nelements(pIm));
      AlwaysAssertExit (expr.getDouble() == shape.product());
    }

    { 
      PagedImage<Float> pIm ("tPagedImage2_tmp.img");
      AlwaysAssertExit (pIm.getDefaultMask() == "reg1");
      AlwaysAssertExit (! pIm.isMasked());
      AlwaysAssertExit (! pIm.hasPixelMask());
      AlwaysAssertExit (pIm.isWritable());
      AlwaysAssertExit (pIm.isPaged());
      AlwaysAssertExit (pIm.getRegionPtr() != 0);
      ImageRegion reg = pIm.getRegion("reg1");
      AlwaysAssertExit (reg == ImageRegion(box1));

// Define the region in the regions group and check it can be found.
      pIm.defineRegion ("regr1", reg, RegionHandler::Regions);
      const ImageRegion* imregptr;
      imregptr = pIm.getImageRegionPtr ("regr1", RegionHandler::Regions, False);
      AlwaysAssertExit (imregptr != 0);
      delete imregptr;
      AlwaysAssertExit (pIm.getImageRegionPtr
                            ("regr1", RegionHandler::Masks, False) == 0);
      imregptr = pIm.getImageRegionPtr ("regr1", RegionHandler::Any, False);
      AlwaysAssertExit (imregptr != 0);
      delete imregptr;

// Rename the region in the regions group and check it can be found.
      pIm.renameRegion ("regr2", "regr1", RegionHandler::Regions);
      imregptr = pIm.getImageRegionPtr ("regr2", RegionHandler::Regions, False);
      AlwaysAssertExit (imregptr != 0);
      delete imregptr;
      AlwaysAssertExit (pIm.getImageRegionPtr
                            ("regr2", RegionHandler::Masks, False) == 0);
      imregptr = pIm.getImageRegionPtr ("regr2", RegionHandler::Any, False);
      AlwaysAssertExit (imregptr != 0);
      delete imregptr;

      pIm.setDefaultMask ("");
      AlwaysAssertExit (! pIm.isMasked());
      AlwaysAssertExit (pIm.getRegionPtr() == 0);

      Array<Bool> mask(shape);
      mask = True;
      AlwaysAssertExit (allEQ(pIm.getMask(), mask));

// Create a mask and make it default region.
      pIm.makeMask ("reg2", True, True);
      AlwaysAssertExit (pIm.getDefaultMask() == "reg2");
      AlwaysAssertExit (File("tPagedImage2_tmp.img/reg2").isDirectory());
      AlwaysAssertExit (pIm.isMasked());
      AlwaysAssertExit (pIm.hasPixelMask());
      AlwaysAssertExit (pIm.pixelMask().isWritable());

// Put a mask and check it is correct.
      mask(IPosition(2,0,0)) = False;
      pIm.pixelMask().put (mask);
      AlwaysAssertExit (allEQ(pIm.getMask(), mask));

// Rename that mask and make sure the table and  default mask are renamed too.
      pIm.renameRegion ("reg2n", "reg2");
      AlwaysAssertExit (pIm.getDefaultMask() == "reg2n");
      AlwaysAssertExit (File("tPagedImage2_tmp.img/reg2n").isDirectory());
      AlwaysAssertExit (! File("tPagedImage2_tmp.img/reg2").exists());
      AlwaysAssertExit (pIm.isMasked());

// Make a unique name.
      AlwaysAssertExit (pIm.makeUniqueRegionName ("reg2n") == "reg2n1");
      AlwaysAssertExit (pIm.makeUniqueRegionName ("reg2n", 3) == "reg2n3");
      AlwaysAssertExit (pIm.makeUniqueRegionName ("reg2na", 3) == "reg2na3");

// Now get the mask as a region and check it is correct.
      {
	ImageRegion reg1 (pIm.getRegion (pIm.getDefaultMask()));
	AlwaysAssertExit (reg1.isLCRegion());
	AlwaysAssertExit (allEQ(reg1.asLCRegion().get(), mask));
      }

// Check number of elements.
      {
	LatticeExprNode expr (nelements(pIm));
	AlwaysAssertExit (expr.getDouble() == shape.product()-1);
      }

// Remove the region, which should also remove the default mask.
      pIm.removeRegion ("reg2n");
      AlwaysAssertExit (pIm.getDefaultMask() == "");
      AlwaysAssertExit (! pIm.isMasked());
    }

    cout<< "ok"<< endl;
  } catch (AipsError x) {
    cerr << "Exception caught: " << x.getMesg() << endl;
    return 1;
  } 

  return 0;
}
