//# tImageUtilities.cc: Test program for the static ImageUtilities functions
//# Copyright (C) 2001
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

#include <trial/Images/ImageUtilities.h>
#include <trial/Images/PagedImage.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <aips/Lattices/PagedArray.h>
#include <aips/Arrays/IPosition.h>
#include <aips/OS/RegularFile.h>
#include <aips/OS/Directory.h>
#include <aips/IO/RegularFileIO.h>
#include <aips/Utilities/Assert.h>
#include <aips/iostream.h>


void doIt()
{
  CoordinateSystem csys2 = CoordinateUtil::defaultCoords2D();
  CoordinateSystem csys3 = CoordinateUtil::defaultCoords3D();
  CoordinateSystem csys4 = CoordinateUtil::defaultCoords4D();
  AlwaysAssertExit (ImageUtilities::compareCoordinates (csys2, csys2) == 0);
  AlwaysAssertExit (ImageUtilities::compareCoordinates (csys3, csys3) == 0);
  AlwaysAssertExit (ImageUtilities::compareCoordinates (csys4, csys4) == 0);
  AlwaysAssertExit (ImageUtilities::compareCoordinates (csys2, csys3) == -1);
  AlwaysAssertExit (ImageUtilities::compareCoordinates (csys2, csys4) == -1);
  AlwaysAssertExit (ImageUtilities::compareCoordinates (csys3, csys4) == -1);
  AlwaysAssertExit (ImageUtilities::compareCoordinates (csys3, csys2) == 1);
  AlwaysAssertExit (ImageUtilities::compareCoordinates (csys4, csys2) == 1);
  AlwaysAssertExit (ImageUtilities::compareCoordinates (csys4, csys3) == 1);

  IPosition newAxes, stretchAxes;
  AlwaysAssertExit (ImageUtilities::findExtendAxes (newAxes, stretchAxes,
						    IPosition(4,5,6,1,5),
						    IPosition(4,5,1,1,1),
						    csys4, csys4));
  AlwaysAssertExit (newAxes.isEqual (IPosition()));
  AlwaysAssertExit (stretchAxes.isEqual (IPosition(2,1,3)));
  AlwaysAssertExit (ImageUtilities::findExtendAxes (newAxes, stretchAxes,
						    IPosition(3,5,6,3),
						    IPosition(2,5,6),
						    csys3, csys2));
  AlwaysAssertExit (newAxes.isEqual (IPosition(1,2)));
  AlwaysAssertExit (stretchAxes.isEqual (IPosition()));
  AlwaysAssertExit (ImageUtilities::findExtendAxes (newAxes, stretchAxes,
						    IPosition(4,5,6,3,4),
						    IPosition(2,5,6),
						    csys4, csys2));
  AlwaysAssertExit (newAxes.isEqual (IPosition(2,2,3)));
  AlwaysAssertExit (stretchAxes.isEqual (IPosition()));
  AlwaysAssertExit (ImageUtilities::findExtendAxes (newAxes, stretchAxes,
						    IPosition(4,5,6,3,4),
						    IPosition(3,5,6,1),
						    csys4, csys3));
  AlwaysAssertExit (newAxes.isEqual (IPosition(1,2)));
  AlwaysAssertExit (stretchAxes.isEqual (IPosition(1,3)));
}

void doIt2()
{
  IPosition newAxes, stretchAxes;
  {
    CoordinateSystem csys1;
    CoordinateSystem csys2;
    CoordinateUtil::addDirAxes (csys1);
    CoordinateUtil::addFreqAxis (csys2);
    AlwaysAssertExit (ImageUtilities::compareCoordinates (csys1, csys2) == 9);
    AlwaysAssertExit (ImageUtilities::compareCoordinates (csys2, csys1) == 9);
    AlwaysAssertExit (! ImageUtilities::findExtendAxes (newAxes, stretchAxes,
							IPosition(2,5,6),
							IPosition(1,8),
							csys1, csys2));

    CoordinateUtil::addDirAxes (csys2);
    AlwaysAssertExit (ImageUtilities::findExtendAxes (newAxes, stretchAxes,
						      IPosition(3,8,5,6),
						      IPosition(2,1,1),
						      csys2, csys1));
    AlwaysAssertExit (newAxes.isEqual (IPosition(1,0)));
    AlwaysAssertExit (stretchAxes.isEqual (IPosition(2,1,2)));

    CoordinateUtil::addFreqAxis (csys1);
    AlwaysAssertExit (ImageUtilities::compareCoordinates (csys1, csys2) == 9);
    AlwaysAssertExit (ImageUtilities::compareCoordinates (csys2, csys1) == 9);
    AlwaysAssertExit (! ImageUtilities::findExtendAxes (newAxes, stretchAxes,
							IPosition(3,5,6,3),
							IPosition(3,3,4,6),
							csys1, csys2));
  }
}

void doTypes()
{
  {
    PagedImage<Float> img (IPosition(2,10,10),
			   CoordinateUtil::defaultCoords2D(),
			   "tImageUtilities_tmp.img");
  }
  AlwaysAssertExit (ImageUtilities::imageType ("tImageUtilities_tmp.img")
		    == ImageUtilities::AIPSPP);
  {
    PagedArray<Float> arr (IPosition(2,10,10),
			   "tImageUtilities_tmp.img");
  }
  AlwaysAssertExit (ImageUtilities::imageType ("tImageUtilities_tmp.img")
		    == ImageUtilities::UNKNOWN);
  {
    Directory dir("tImageUtilities_tmp.mir");
    dir.create();
    RegularFile rfile("tImageUtilities_tmp.mir/image");
    rfile.create();
  }
  AlwaysAssertExit (ImageUtilities::imageType ("tImageUtilities_tmp.mir")
		    == ImageUtilities::UNKNOWN);
  {
    RegularFile rfile("tImageUtilities_tmp.mir/header");
    rfile.create();
  }
  AlwaysAssertExit (ImageUtilities::imageType ("tImageUtilities_tmp.mir")
		    == ImageUtilities::MIRIAD);
  {
    Directory dir("tImageUtilities_tmp");
    dir.create();
    RegularFile rfile("tImageUtilities_tmp/a.image");
    rfile.create();
  }
  AlwaysAssertExit (ImageUtilities::imageType ("tImageUtilities_tmp/a.image")
		    == ImageUtilities::UNKNOWN);
  char buf[2880];
  memset (buf, ' ', 2880);
  memcpy (buf, "SIMPLE  =   T  ", 17);
  {
    RegularFileIO file (RegularFile("tImageUtilities_tmp/a.image"),
			ByteIO::Update);
    file.write (2879, buf);
  }
  AlwaysAssertExit (ImageUtilities::imageType ("tImageUtilities_tmp/a.image")
		    == ImageUtilities::UNKNOWN);
  {
    RegularFileIO file (RegularFile("tImageUtilities_tmp/a.image"),
			ByteIO::Update);
    file.write (2880, buf);
  }
  AlwaysAssertExit (ImageUtilities::imageType ("tImageUtilities_tmp/a.image")
		    == ImageUtilities::FITS);
  {
    RegularFile rfile("tImageUtilities_tmp/a.descr");
    rfile.create();
  }
  AlwaysAssertExit (ImageUtilities::imageType ("tImageUtilities_tmp/a.image")
		    == ImageUtilities::GIPSY);
}
  
int main()
{
  try {
    doIt();
    doIt2();
    doTypes();
  } catch (AipsError& x) {
    cout << "Unexpected exception: " << x.getMesg() << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
