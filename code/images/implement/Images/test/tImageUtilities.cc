//# tImageUtilities.cc: Test program for the static ImageUtilities functions
//# Copyright (C) 2001,2002,2003
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
#include <trial/Images/ImageFITSConverter.h>
#include <trial/Coordinates/CoordinateSystem.h>
#include <trial/Coordinates/CoordinateUtil.h>
#include <aips/Lattices/PagedArray.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/OS/RegularFile.h>
#include <aips/OS/Directory.h>
#include <aips/IO/RegularFileIO.h>
#include <aips/Logging/LogIO.h>
#include <aips/Logging/LogOrigin.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/PtrHolder.h>
#include <aips/iostream.h>

void doOpens()
{
   Directory dir("tImageUtilities_tmp");
   dir.create();
   LogIO os(LogOrigin("tImageUtilities", "main()", WHERE));

//
   {
      String name1("tImageUtilities_tmp/app.img");
      PagedImage<Float> img (IPosition(2,10,10),
   			      CoordinateUtil::defaultCoords2D(), 
                              name1);
      String error;
      String name2("tImageUtilities_tmp/fits.img");
      Bool ok = ImageFITSConverter::ImageToFITS(error, img, name2,
                                                64, True, True, -32, 1, -1,
                                                True);
      if (ok);			// Satisfy compiler
//
      {
         PtrHolder<ImageInterface<Float> > im;
         ImageUtilities::openImage(im, name1, os);
      }
      {
         PtrHolder<ImageInterface<Float> > im;
         ImageUtilities::openImage(im, name2, os);
      }
   }
//
  dir.removeRecursive();
}
void doTypes()
{
   Directory dir("tImageUtilities_tmp");
   dir.create();
  {
    PagedImage<Float> img (IPosition(2,10,10),
			   CoordinateUtil::defaultCoords2D(),
			   "tImageUtilities_tmp/app.img");
  }
  AlwaysAssertExit (ImageUtilities::imageType ("tImageUtilities_tmp/app.img")
		    == ImageUtilities::AIPSPP);
  {
    PagedArray<Float> arr (IPosition(2,10,10),
			   "tImageUtilities_tmp/app.img");
  }
  AlwaysAssertExit (ImageUtilities::imageType ("tImageUtilities_tmp/app.img")
		    == ImageUtilities::UNKNOWN);
  {
    Directory dir("tImageUtilities_tmp/mir.img");
    dir.create();
    RegularFile rfile("tImageUtilities_tmp/mir.img/image");
    rfile.create();
  }
  AlwaysAssertExit (ImageUtilities::imageType ("tImageUtilities_tmp/mir.img")
		    == ImageUtilities::UNKNOWN);
  {
    RegularFile rfile("tImageUtilities_tmp/mir.img/header");
    rfile.create();
  }
  AlwaysAssertExit (ImageUtilities::imageType ("tImageUtilities_tmp/mir.img")
		    == ImageUtilities::MIRIAD);
  {
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
//
  dir.removeRecursive();
}

void listWorld (const Vector<Quantum<Double> >& wPars)
{
   cerr << "World" << endl;
   if (wPars.nelements()==3){ 
      cerr << "  Major, minor, pa = " << wPars(0) << ", " << wPars(1)
           << ", " << wPars(2) << endl;
   } if (wPars.nelements()==5) {
      cerr << "  X, Y = " << wPars(0) << ", " << wPars(1) << endl;
      cerr << "  Major, minor, pa = " << wPars(2) << ", " << wPars(3)
           << ", " << wPars(4) << endl;
   }
}
   
void listPixel(const Vector<Double>& pPars)
{
   cerr << "Pixel" << endl;
   if (pPars.nelements()==3) {
      cerr << "  Major, minor, pa = " << pPars(0) << ", " << pPars(1) << ", " 
           << pPars(2)/C::degree  << endl;   
   } else if (pPars.nelements()==5) {
      cerr << "  X, Y = " << pPars(0) << ", " << pPars(1) << endl;
      cerr << "  Major, minor, pa = " << pPars(2) << ", " << pPars(3) << ", " 
           << pPars(4)/C::degree  << endl;   
   }
}

void doConversions()
{
   LogOrigin lor("tImageUtilities", "doConversions()", WHERE);
   LogIO os(lor);
//
   {
      CoordinateSystem cSys = CoordinateUtil::defaultCoords2D();
      cerr << "inc = " << cSys.increment() << endl;
      const Vector<String>& units = cSys.worldAxisUnits();
      IPosition pixelAxes(2, 0, 1);
      IPosition worldAxes(2, 0, 1);
//
      Vector<Double> world;
      Vector<Double> pixel = cSys.referencePixel().copy();
      pixel += 10.0;
      cSys.toWorld(world, pixel);
//
      Vector<Quantum<Double> > wPars(5);

// Position

      wPars(0).setValue(world(worldAxes(0)));
      wPars(0).setUnit(units(worldAxes(0)));
      wPars(1).setValue(world(worldAxes(1)));
      wPars(1).setUnit(units(worldAxes(1)));

// Shape

      wPars(2).setValue(20.0);
      wPars(2).setUnit(Unit(String("arcmin")));
      wPars(3).setValue(10.0);
      wPars(3).setUnit(Unit(String("arcmin")));
      wPars(4).setValue(70.0);
      wPars(4).setUnit(Unit(String("deg")));

// Convert to pixel

      Vector<Double> pPars;
      ImageUtilities::worldWidthsToPixel (os, pPars, wPars, cSys, pixelAxes);

// Back to world

      Vector<Double> pPars2(5);
      pPars2(0) = pixel(pixelAxes(0));
      pPars2(1) = pixel(pixelAxes(1));
      for (uInt i=0; i<3; i++) {
         pPars2(i+2) = pPars(i);
      }
      Vector<Quantum<Double> > wPars2;
      ImageUtilities::pixelWidthsToWorld (os, wPars2, pPars2, cSys, pixelAxes);     
//
      listWorld(wPars);
      listPixel(pPars);
      listWorld(wPars2);
//
      for (uInt i=0; i<wPars.nelements(); i++) {
//         AlwaysAssert(wPars(i)==wPars2(i), AipsError);
      }
   }
}



  
int main()
{
  try {
    doTypes();
    doOpens();
//    doConversions();
  } catch (AipsError& x) {
    cout << "Unexpected exception: " << x.getMesg() << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
