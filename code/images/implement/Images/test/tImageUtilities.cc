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
    doTypes();
  } catch (AipsError& x) {
    cout << "Unexpected exception: " << x.getMesg() << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
