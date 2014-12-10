//# tImageEmpty: Test creating an image without writing it explicitly
//# Copyright (C) 2010
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
#include <casacore/coordinates/Coordinates/CoordinateUtil.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>

using namespace casacore;

// This test program was created because of a problem detected in the
// CASA Imager::clone function. It created an image without writing/flushing
// it, which worked well until the TSM changes made to support mmap.
// It was fixed in the TSM (for all modes) and tested in tTableEmpty and here.

void doIt (const IPosition& cubeShape, const IPosition tileShape)
{
  {
    // Create the image without explicitly writing it.
    // The destructor should create it all (with arbitrary data).
    PagedImage<Float> newImage(TiledShape(cubeShape, tileShape),
                               CoordinateUtil::defaultCoords2D(),
                               "tImageEmpty_tmp.img");
  }
  {
    // Check the shapes and if data can be read.
    PagedImage<Float> image ("tImageEmpty_tmp.img");
    AlwaysAssertExit (image.shape() == cubeShape);
    AlwaysAssertExit (image.niceCursorShape() == tileShape);
    image.get();
  }
}

int main()
{
  // Try it with various tile shapes.
  doIt (IPosition(2,256,256), IPosition(2,256,256));
  doIt (IPosition(2,256,256), IPosition(2,32,32));
  doIt (IPosition(2,256,256), IPosition(2,29,31));
}
