//# tDirectionCoordinate.cc: Test program for DirectionCoordinate
//# Copyright (C) 1998,1999,2000,2001,2002,2003,2004
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

 
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/Matrix.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/measures/Measures/MCDirection.h>

#include <casacore/measures/Measures/MDirection.h>
#include <casacore/casa/OS/Timer.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Quanta/MVDirection.h>
#include <casacore/coordinates/Coordinates/DirectionCoordinate.h>
#include <casacore/coordinates/Coordinates/Projection.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/tables/Tables/TableRecord.h>
#include <casacore/coordinates/Coordinates/CoordinateSystem.h>
#include <casacore/casa/Logging/LogIO.h>

#include <wcslib/wcs.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/namespace.h>
#include <iomanip>

DirectionCoordinate makeCoordinate(MDirection::Types type,
                                   Projection& proj,
                                   Vector<double>& crval,
                                   Vector<double>& crpix,
                                   Vector<double>& cdelt,
                                   Matrix<double>& xform);

void doit (DirectionCoordinate& lc, 
           MDirection::Types type,
           const Vector<String>& axisName,
           const String& axisUnit);
void doit2 (DirectionCoordinate& lc, 
            Vector<double>& crval,
            Vector<double>& crpix,
            Vector<double>& cdelt,
            Matrix<double>& xform);
void doit3 (DirectionCoordinate& lc);
void doit4 (DirectionCoordinate& lc);
void doit5 (DirectionCoordinate& lc);
void doit6();
void doit7 ();
void doit8 ();
void doit9 ();
void doit10 ();

#if defined(USE_THREADS)
#include <thread>

void _create_many_coordinates()
{
	// Simply copying a coordinate originally caused multithreading problems
    for(size_t i = 0; i != 1000; i++) {
        DirectionCoordinate d1;
        DirectionCoordinate d2(d1);
    }
}

void test_multithreaded_behaviour()
{
	std::cout << "*** Test multithreaded coordinate creation" << std::endl;
	auto t1 = std::thread(_create_many_coordinates);
	auto t2 = std::thread(_create_many_coordinates);
	t1.join();
	t2.join();
}
#endif // USE_THREADS




int main()
{

#if defined(USE_THREADS)
    {
        test_multithreaded_behaviour();
    }
#endif // USE_THREADS

   try {

      Projection proj;
      Vector<double> crval, crpix, cdelt;
      Matrix<double> xform;

// Constructors

      {
         DirectionCoordinate lc;
      }
      {
         DirectionCoordinate lc = makeCoordinate(MDirection::J2000,
                                                 proj, crval, crpix,
                                                 cdelt, xform);
      }
//    
      {
         for (uint32_t i=0; i<MDirection::N_Types; i++) {
            MDirection::Types tp = MDirection::castType(i);
            MDirection::GlobalTypes gt = MDirection::globalType (i);
            if (gt != MDirection::GAZEL) {
               DirectionCoordinate lc = makeCoordinate(tp,
                                                       proj, crval, crpix,
                                                       cdelt, xform);
            }
         }
      }

// Test near function

     {
         DirectionCoordinate lc  = makeCoordinate(MDirection::J2000,
                                                 proj, crval, crpix,
                                                 cdelt, xform);
         DirectionCoordinate lc2 = makeCoordinate(MDirection::J2000,
                                                 proj, crval, crpix, 
                                                 cdelt, xform);
         if (!lc.near(lc2)) {
            throw(AipsError(String("Failed near test 1 because") + lc.errorMessage()));
         }
         Vector<int32_t> excludeAxes(1, 0);
         if (!lc.near(lc2, excludeAxes)) {
            throw(AipsError(String("Failed near test 2 because") + lc.errorMessage()));
         }
     } 

// Test Quantum constructor interface

     {
        Matrix<double> xform(2,2);
        xform = 0.0;
        xform.diagonal() = 1.0;
        Projection proj(Projection::SIN);
        MDirection::Types type(MDirection::B1950);
//
        Vector<double> crval(2);
        Vector<double> crpix(2);
        Vector<double> cdelt(2);
//
        crval(0) = 0.1; crval(1) = 0.5;
        crpix(0) = 100.0; crpix(1) = 120.0;
        cdelt(0) = 1e-6; cdelt(1) = 2e-6;
//
        DirectionCoordinate dc1(type, proj, crval(0), crval(1),
                                cdelt(0), cdelt(1),
                                xform, crpix(0), crpix(1));
//
        Quantum<double> lon(crval(0)*180.0/C::pi, "deg");
        Quantum<double> lat(crval(1)*180.0/C::pi, "deg");
        Quantum<double> dlon(cdelt(0)*180.0/C::pi, "deg");
        Quantum<double> dlat(cdelt(1)*180.0/C::pi, "deg");
        Quantum<double> longPole(999.0, "deg");
        Quantum<double> latPole(999.0, "deg");
//
        DirectionCoordinate dc2(type, proj, lon, lat, dlon, dlat,
                                xform, crpix(0), crpix(1), longPole, latPole);
//
        if (!dc1.near(dc2)) {
           throw(AipsError("Quantum interface constructor failed consistency test"));
        }
      }

// Test WCS constructor (only partial test)

      {
         ::wcsprm wcs;
         wcs.flag = -1;
         int iret = wcsini(1, 2, &wcs);
         if (iret!=0) {
           throw(AipsError(String("Failed to make wcs structure")));
         }
//
         String ctype0("RA---SIN");
         String ctype1("DEC--SIN");
//
         strncpy (wcs.ctype[0], ctype0.chars(), 9);
         strncpy (wcs.ctype[1], ctype1.chars(), 9);
//
         iret = wcsset(&wcs);
         if (iret!=0) {
           throw(AipsError(String("Failed to init wcs structure")));
         }
//
         DirectionCoordinate dc3 (MDirection::J2000, wcs);
//
         DirectionCoordinate dc4(dc3);
         if (!dc3.near(dc4)) {
            throw(AipsError(String("wcs interface constructor failed consistency test 1")));
         }
         DirectionCoordinate dc5;
         dc5 = dc3;
         if (!dc5.near(dc3)) {
            throw(AipsError(String("wcs interface constructor failed consistency test 2")));
         }
         wcsfree(&wcs);
      }  


// Test the rest

      {
         DirectionCoordinate lc  = makeCoordinate(MDirection::J2000,
                                                 proj, crval, crpix,
                                                 cdelt, xform);
         Vector<String> axisNames(2); 
         axisNames(0) = "Right Ascension";
         axisNames(1) = "Declination";
         String axisUnit = "rad";
         doit(lc, MDirection::J2000, axisNames, axisUnit);
      }
      {
         DirectionCoordinate lc  = makeCoordinate(MDirection::J2000,
                                                 proj, crval, crpix,
                                                 cdelt, xform);
         doit2(lc, crval, crpix, cdelt, xform);
      }
      {
         DirectionCoordinate lc  = makeCoordinate(MDirection::J2000,
                                                 proj, crval, crpix,
                                                 cdelt, xform);
         doit3(lc);
      }
      {
         DirectionCoordinate lc  = makeCoordinate(MDirection::GALACTIC,
                                                 proj, crval, crpix,
                                                 cdelt, xform);
         Vector<String> axisNames(2); 
         axisNames(0) = "Longitude";
         axisNames(1) = "Latitude";
         String axisUnit = "rad";
         doit(lc, MDirection::GALACTIC, axisNames, axisUnit);
      }
      {
         DirectionCoordinate lc  = makeCoordinate(MDirection::GALACTIC,
                                                 proj, crval, crpix,
                                                 cdelt, xform);
         doit2(lc, crval, crpix, cdelt, xform);
      }

      {
         DirectionCoordinate lc  = makeCoordinate(MDirection::GALACTIC,
                                                 proj, crval, crpix,
                                                 cdelt, xform);
         doit3(lc);
      }
      {
         DirectionCoordinate lc  = makeCoordinate(MDirection::J2000,
                                                 proj, crval, crpix,
                                                 cdelt, xform);
         doit4(lc);
      }
      {
         DirectionCoordinate lc  = makeCoordinate(MDirection::J2000,
                                                 proj, crval, crpix,
                                                 cdelt, xform);
         doit5(lc);
      }
      {
         doit6 ();
      }
      {
         doit7 ();
      }
      {
         doit8();
      }
      {
         doit9();
      }
      {
         doit10();
      }
      {
    	  // getPixelArea
    	  DirectionCoordinate dc  = makeCoordinate(
    	      MDirection::J2000, proj, crval, crpix,
    	      cdelt, xform
    	  );
    	  cout << "cdelt " << cdelt << endl;
    	  Quantity pixelArea = dc.getPixelArea();
    	  AlwaysAssert(pixelArea.getValue() == fabs(cdelt[0]*cdelt[1]), AipsError);
    	  Vector<String> units = dc.worldAxisUnits();
    	  AlwaysAssert(
    	      pixelArea.getUnit()
    	      == (Quantity(1, units[0])*Quantity(1, units[1])).getUnit(),
    	      AipsError
    	  );
      }
      {
    	  cout << "*** Test rotate" << endl;
    	  DirectionCoordinate dc  = makeCoordinate(
    	      MDirection::J2000, proj, crval, crpix,
    	      cdelt, xform
    	  );
    	  // No rotation
    	  Coordinate *c = dc.rotate(Quantity(0, "deg"));
    	  Vector<double> p1(2);
    	  p1[0] = 10;
    	  p1[1] = 20;
    	  Vector<double> p2(2);
    	  p2[0] = 200;
    	  p2[1] = 240;
    	  Vector<double> got, exp;
    	  c->toWorld(got, p1);
    	  dc.toWorld(exp, p1);
    	  Vector<double> p1World = exp.copy();
    	  AlwaysAssert(allTrue(got == exp), AipsError);
    	  c->toWorld(got, p2);
    	  dc.toWorld(exp, p2);
    	  Vector<double> p2World = exp.copy();

    	  AlwaysAssert(allTrue(got == exp), AipsError);
          delete c;

    	  // non-zero rotation
    	  c = dc.rotate(Quantity(30, "deg"));
    	  c->toPixel(got, p1World);
    	  Vector<double> orig;
    	  dc.toPixel(orig, p1World);
    	  exp[0] = 72.05771366;
    	  exp[1] = -11.60254038;
    	  AlwaysAssert(allNear(got, exp, 1e-8), AipsError);
          delete c;
      }
      {
    	  cout << "*** Test convert()" << endl;
    	  Projection p(Projection::SIN, Vector<double>(2, 0));
    	  Matrix<double> xform(2, 2, 0);
    	  xform.diagonal() = 1;
    	  DirectionCoordinate origGalactic(
    	      MDirection::GALACTIC, p,
    	      Quantity(0.15064170309942734, "rad"),
    	      Quantity(-0.0027706408478612907, "rad"),
    	      Quantity(-6, "arcsec"), Quantity(6, "arcsec"),
    	      xform, 1573.5, -6.5
    	  );

    	  Record r;
    	  origGalactic.save(r, "coords");
    	  cout << "*** original " << r << endl;
    	  Quantity angle;
    	  DirectionCoordinate converted = origGalactic.convert(angle, MDirection::J2000);
    	  Record s;
    	  converted.save(s, "coords");
    	  cout << "*** converted " << s << endl;

    	  Vector<double> origRefVal = origGalactic.referenceValue();
    	  Vector<String> units = origGalactic.worldAxisUnits();
    	  MDirection origRefDir(
    		  Quantity(origRefVal[0], units[0]),
    	      Quantity(origRefVal[1], units[1]),
    	  	  MDirection::GALACTIC
    	  );
    	  Quantum<Vector<double> > expRefDir = MDirection::Convert(
    	      origRefDir, MDirection::J2000
    	  )().getAngle();
    	  Vector<double> gotRefDir = converted.referenceValue();
    	  AlwaysAssert(
    	      near(
    	          gotRefDir[0], expRefDir.getValue(units[0])[0],
    	          1e-8
    	      ),
    	      AipsError
    	  );
    	  AlwaysAssert(
    	      near(
    	          gotRefDir[1],
    	          expRefDir.getValue(units[1])[1],
    	          1e-8
    	      ),
    	      AipsError
    	  );
    	  Vector<double> origWorld;
    	  Vector<double> pixel(2, 20);
    	  origGalactic.toWorld(origWorld, pixel);
    	  MDirection origDir2(
    	      Quantity(origWorld[0], units[0]),
    	      Quantity(origWorld[1], units[1]),
    	      MDirection::GALACTIC
    	  );
    	  Quantum<Vector<double> > expDir = MDirection::Convert(
    	      origDir2, MDirection::J2000
    	  )().getAngle();
    	  Vector<double> gotDir;
    	  converted.toWorld(gotDir, pixel);
    	  Vector<double> pixela(2, -6.5);
    	  pixela[0] = 1573.5;
    	  Vector<double> pixelb = pixela.copy();
    	  pixelb[0] = 1574.5;
    	  //Vector<double> pixelc = pixela.copy();
    	  //pixelc[0] = 1500;
    	  Vector<double> worlda, worldb, worldc;
    	  converted.toWorld(worlda, pixela);
    	  converted.toWorld(worldb, pixelb);
    	  //converted.toWorld(worldc, pixelc);

    	  cout << "*** a " << worlda << endl;
    	  cout << "*** b " << worldb << endl;
    	  //cout << "*** c " << worldc << endl;
    	  cout << "*** inc " << converted.increment() << endl;

    	  std::map<std::pair<int32_t, int32_t>, double> expAngle;
    	  expAngle[std::pair<int32_t, int32_t>(0, -80)] = -165.668663;
    	  expAngle[std::pair<int32_t, int32_t>(0, -60)] = -159.272545;
    	  expAngle[std::pair<int32_t, int32_t>(0, -40)] = -136.458146;
    	  expAngle[std::pair<int32_t, int32_t>(0, -20)] = -56.3749304;
    	  expAngle[std::pair<int32_t, int32_t>(0, 0)] = -23.4795798;
    	  expAngle[std::pair<int32_t, int32_t>(0, 20)] = -15.2757315;
    	  expAngle[std::pair<int32_t, int32_t>(0, 40)] = -12.3186824;
    	  expAngle[std::pair<int32_t, int32_t>(0, 60)] = -11.4331534;
    	  expAngle[std::pair<int32_t, int32_t>(0, 80)] = -11.9775365;
    	  expAngle[std::pair<int32_t, int32_t>(4, -80)] = 128.395658;
    	  expAngle[std::pair<int32_t, int32_t>(4, -60)] = 114.424745;
    	  expAngle[std::pair<int32_t, int32_t>(4, -40)] = 93.4929423;
    	  expAngle[std::pair<int32_t, int32_t>(4, -20)] = 71.2514923;
    	  expAngle[std::pair<int32_t, int32_t>(4, 0)] = 55.048387;
    	  expAngle[std::pair<int32_t, int32_t>(4, 20)] = 45.7495975;
    	  expAngle[std::pair<int32_t, int32_t>(4, 40)] = 41.4598396;
    	  expAngle[std::pair<int32_t, int32_t>(4, 60)] = 40.9329567;
    	  expAngle[std::pair<int32_t, int32_t>(4, 80)] = 44.0088699;
    	  expAngle[std::pair<int32_t, int32_t>(8, -80)] = 68.355075;
    	  expAngle[std::pair<int32_t, int32_t>(8, -60)] = 61.8452755;
    	  expAngle[std::pair<int32_t, int32_t>(8, -40)] = 58.6597989;
    	  expAngle[std::pair<int32_t, int32_t>(8, -20)] = 58.6455176;
    	  expAngle[std::pair<int32_t, int32_t>(8, 0)] = 61.8014664;
    	  expAngle[std::pair<int32_t, int32_t>(8, 20)] = 68.2804866;
    	  expAngle[std::pair<int32_t, int32_t>(8, 40)] = 78.0043392;
    	  expAngle[std::pair<int32_t, int32_t>(8, 60)] = 89.9436203;
    	  expAngle[std::pair<int32_t, int32_t>(8, 80)] = 101.89424;
    	  expAngle[std::pair<int32_t, int32_t>(12, -80)] = 11.9775367;
    	  expAngle[std::pair<int32_t, int32_t>(12, -60)] = 11.4331535;
    	  expAngle[std::pair<int32_t, int32_t>(12, -40)] = 12.3186824;
    	  expAngle[std::pair<int32_t, int32_t>(12, -20)] = 15.2757315;
    	  expAngle[std::pair<int32_t, int32_t>(12, 0)] = 23.4795796;
    	  expAngle[std::pair<int32_t, int32_t>(12, 20)] = 56.3749301;
    	  expAngle[std::pair<int32_t, int32_t>(12, 40)] = 136.458146;
    	  expAngle[std::pair<int32_t, int32_t>(12, 60)] = 159.272545;
    	  expAngle[std::pair<int32_t, int32_t>(12, 80)] = 165.668663;
    	  expAngle[std::pair<int32_t, int32_t>(16, -80)] = -44.0088701;
    	  expAngle[std::pair<int32_t, int32_t>(16, -60)] = -40.9329566;
    	  expAngle[std::pair<int32_t, int32_t>(16, -40)] = -41.4598395;
    	  expAngle[std::pair<int32_t, int32_t>(16, -20)] = -45.7495973;
    	  expAngle[std::pair<int32_t, int32_t>(16, 0)] = -55.0483868;
    	  expAngle[std::pair<int32_t, int32_t>(16, 20)] = -71.2514922;
    	  expAngle[std::pair<int32_t, int32_t>(16, 40)] = -93.4929422;
    	  expAngle[std::pair<int32_t, int32_t>(16, 60)] = -114.424745;
    	  expAngle[std::pair<int32_t, int32_t>(16, 80)] = -128.395658;
    	  expAngle[std::pair<int32_t, int32_t>(20, -80)] = -101.894241;
    	  expAngle[std::pair<int32_t, int32_t>(20, -60)] = -89.9436205;
    	  expAngle[std::pair<int32_t, int32_t>(20, -40)] = -78.0043394;
    	  expAngle[std::pair<int32_t, int32_t>(20, -20)] = -68.2804869;
    	  expAngle[std::pair<int32_t, int32_t>(20, 0)] = -61.8014666;
    	  expAngle[std::pair<int32_t, int32_t>(20, 20)] = -58.6455178;
    	  expAngle[std::pair<int32_t, int32_t>(20, 40)] = -58.659799;
    	  expAngle[std::pair<int32_t, int32_t>(20, 60)] = -61.8452755;
    	  expAngle[std::pair<int32_t, int32_t>(20, 80)] = -68.3550746;
    	  // test various points around the sky
    	  for (int32_t ha=0; ha<24; ha+=4) {
    	      for (int32_t dec=-80; dec<85; dec+=20) {
    	          DirectionCoordinate j2000(
    	              MDirection::J2000, p, Quantity(ha*15, "deg"),
    	              Quantity(dec, "deg"), Quantity(-1, "arcsec"),
    	              Quantity(1, "arcsec"), xform, 100, 100
    	          );
    	          units = j2000.worldAxisUnits();
    	          Vector<double> j2000RefVal = j2000.referenceValue();
    	          MDirection j2000RefDir(
    	              Quantity(j2000RefVal[0], units[0]),
    	              Quantity(j2000RefVal[1], units[1]),
    	              MDirection::J2000
    	          );
    	          DirectionCoordinate toGalactic = j2000.convert(
    	              angle, MDirection::GALACTIC
    	          );
    	          AlwaysAssert(
    	              allEQ(toGalactic.worldAxisUnits(), units), AipsError
    	          );
    	          AlwaysAssert(
    	              allEQ(toGalactic.linearTransform(), xform), AipsError
    	          );
    	          AlwaysAssert(
    	              toGalactic.projection().type() == p.type(), AipsError
    	          );
    	          AlwaysAssert(
    	              near(
    	                  angle.getValue("deg"),
    	                  expAngle[std::pair<int32_t, int32_t>(ha, dec)], 1e-7
    	              ), AipsError
    	          );
    	          DirectionCoordinate backToJ2000 = toGalactic.convert(
    	              angle, MDirection::J2000
    	          );
    	          Vector<double> newJ2000RefVal = backToJ2000.referenceValue();
    	          if (newJ2000RefVal[0] < -0.1) {
    	              newJ2000RefVal[0] += 2*C::pi;
    	          }
    	          AlwaysAssert(
    	              allNearAbs(newJ2000RefVal, j2000RefVal, 1e-9), AipsError
    	          );
    	          AlwaysAssert(
    	              near(
    	                  angle.getValue("deg"),
    	                  -expAngle[std::pair<int32_t, int32_t>(ha, dec)], 1e-7
    	              ), AipsError
    	          );
    	      }
    	  }
      }
      {
    	  cout << "*** test hasSquarePixels()" << endl;
    	  DirectionCoordinate dc(
    	      MDirection::J2000, Projection::SIN,
    	      Quantity(1, "rad"), Quantity(1, "rad"),
    	      Quantity(-6, "arcsec"), Quantity(6, "arcsec"),
    	      xform, 1573.5, -6.5
    	  );
    	  AlwaysAssert(dc.hasSquarePixels(), AipsError);
    	  dc = DirectionCoordinate(
    		  MDirection::J2000, Projection::SIN,
    		  Quantity(1, "rad"), Quantity(1, "rad"),
    		  Quantity(-6, "arcsec"), Quantity(-6, "arcsec"),
    		  xform, 1573.5, -6.5
    	  );
    	  dc = DirectionCoordinate(
    		  MDirection::J2000, Projection::SIN,
    		  Quantity(1, "rad"), Quantity(1, "rad"),
    		  Quantity(6, "arcsec"), Quantity(-6, "arcsec"),
    		  xform, 1573.5, -6.5
    	  );
    	  AlwaysAssert(dc.hasSquarePixels(), AipsError);
    	  dc = DirectionCoordinate(
    		  MDirection::J2000, Projection::SIN,
    		  Quantity(1, "rad"), Quantity(1, "rad"),
    		  Quantity(6, "arcsec"), Quantity(6, "arcsec"),
    		  xform, 1573.5, -6.5
    	  );
    	  AlwaysAssert(dc.hasSquarePixels(), AipsError);
    	  dc = DirectionCoordinate(
    		  MDirection::J2000, Projection::SIN,
    		  Quantity(1, "rad"), Quantity(1, "rad"),
    		  Quantity(7, "arcsec"), Quantity(6, "arcsec"),
    		  xform, 1573.5, -6.5
    	  );
    	  AlwaysAssert(! dc.hasSquarePixels(), AipsError);

      }
      {
    	  // test isNCP()
    	  Vector<double> parms(2, 0);
    	  Projection projection(Projection::SIN, parms);
    	  double refLong = 0;
    	  double refLat = 0.5;
    	  double inc = C::pi/180/3600;
    	  Matrix<double> xform = Matrix<double>::identity(2);
    	  double refX = 0;
    	  double refY = 0;
    	  DirectionCoordinate dc(
    	      MDirection::J2000, projection,
    	      refLong, refLat, inc, -inc,
    	  	  xform, refX, refY
    	  );
    	  AlwaysAssert(! dc.isNCP(), AipsError);
    	  parms[1] = 1/tan(refLat);
    	  projection = Projection(Projection::SIN, parms);
    	  dc = DirectionCoordinate(
    	      MDirection::J2000, projection,
    	      refLong, refLat, inc, -inc,
    	      xform, refX, refY
    	  );
    	  AlwaysAssert(dc.isNCP(), AipsError);
    	  projection = Projection(Projection::TAN, parms);
    	  dc = DirectionCoordinate(
    	      MDirection::J2000, projection,
    	      refLong, refLat, inc, -inc,
    	      xform, refX, refY
    	  );
    	  AlwaysAssert(! dc.isNCP(), AipsError);
      }
      {
          cout << "*** test toWorld without converting to conversion layer frame" << endl;
          Matrix<double> xform(2, 2, 0);
          xform.diagonal() = 1;
          DirectionCoordinate dc(
              MDirection::J2000, Projection::SIN, Quantity(0, "deg"), Quantity(0, "deg"),
              Quantity(-1, "arcsec"), Quantity(1, "arcsec"), xform, 0, 0
          );
          dc.setReferenceConversion(MDirection::GALACTIC);
          Vector<double> pixel(2, 60);
          Vector<double> world(2);
          // default uses true
          dc.toWorld(world, pixel);
          AlwaysAssert(near(world[0], 1.6811, 1e-5), AipsError);
          AlwaysAssert(near(world[1], -1.05011, 1e-5), AipsError);
          dc.toWorld(world, pixel, true);
          AlwaysAssert(near(world[0], 1.6811, 1e-5), AipsError);
          AlwaysAssert(near(world[1], -1.05011, 1e-5), AipsError);
          dc.toWorld(world, pixel, false);
          AlwaysAssert(near(world[0], 6.28289, 1e-5), AipsError);
          AlwaysAssert(near(world[1], 2.90888e-4, 1e-5), AipsError);
      }

  } catch (const std::exception& x) {
      cerr << "aipserror: error " << x.what() << endl;
      return (1);
   }

   cout << "ok" << endl; 
   return (0);
}


DirectionCoordinate makeCoordinate(MDirection::Types type,
                                   Projection& proj,
                                   Vector<double>& crval, 
                                   Vector<double>& crpix, 
                                   Vector<double>& cdelt, 
                                   Matrix<double>& xform)
{
   crval.resize(2);
   crpix.resize(2);
   cdelt.resize(2);
   crval(0) = 0.1; 
   crval(1) = -0.5;
   crpix(0) = 100.0; 
   crpix(1) = 120.0;
   cdelt(0) = -1e-6; 
   cdelt(1) = 2e-6;
   xform.resize(2,2);
   xform = 0.0;
   xform.diagonal() = 1.0;
   proj = Projection::SIN;
   return DirectionCoordinate(type, proj, crval(0), crval(1),
                              cdelt(0), cdelt(1),
                              xform, crpix(0), crpix(1), 999.0, 999.0);
}
 


void doit (DirectionCoordinate& lc,
           MDirection::Types type,
           const Vector<String>& axisNames,
           const String& axisUnit)
{

// Test copy constructor

   {
       DirectionCoordinate lc2(lc);
       if (!lc.near(lc2)) {
          throw(AipsError("Failed copy constructor test"));
       }
   } 

// Test assignment

   {
       DirectionCoordinate lc2;
       lc2 = lc;
       if (!lc.near(lc2)) {
          throw(AipsError("Failed assignment test"));
       }
   } 

// Test member functions
  
   if (lc.type() != Coordinate::DIRECTION) {
      throw(AipsError("Failed type test"));
   }
   if (lc.directionType() != type) {
      throw(AipsError("Failed directionType test"));
   }
   if (lc.showType() != "Direction") {
      throw(AipsError("Failed showType test"));
   }
//
   if (lc.nPixelAxes() != 2) {
      throw(AipsError("Failed nPixelAxes test"));
   }
//
   if (lc.nWorldAxes() != 2) {
      throw(AipsError("Failed nWorldAxes test"));
   }
//
   if (!allEQ(axisNames, lc.worldAxisNames())) {
      throw(AipsError("Failed world axis name recovery test"));
   }
   Vector<String> names(axisNames.copy());
   names(0) = "Horsies";
   if (!lc.setWorldAxisNames(names)) {
      throw(AipsError(String("Failed to set world axis name because") + lc.errorMessage()));
   }
   if (!allEQ(names, lc.worldAxisNames())) {
      throw(AipsError("Failed axis name set/recovery test"));
   }
//
   Vector<String> units(2); units(0) = axisUnit; units(1) = axisUnit;
   if (!allEQ(units, lc.worldAxisUnits())) {
      throw(AipsError("Failed world axis units recovery test"));
   }
   units(0) = "deg";
   if (!lc.setWorldAxisUnits(units)) {
      throw(AipsError(String("Failed to set world axis units because ") + lc.errorMessage()));
   }
   if (!allEQ(units, lc.worldAxisUnits())) {
      throw(AipsError("Failed world axis units set/recovery test"));
   }
//
// Test record saving
//
   TableRecord rec;
   if (!lc.save(rec, "Direction")) {
      throw(AipsError("Coordinate saving to Record failed"));  
   }  
   DirectionCoordinate* plc = DirectionCoordinate::restore(rec, "Direction");
   if (!plc->near(lc, 1e-6)) {
      throw(AipsError(String("Restore failed with") + plc->errorMessage()));
   }
   delete plc;
//
// Test clone
//
   Coordinate* plc2 = lc.clone();
   if (!plc2->near(lc, 1e-6)) {
      throw(AipsError("Clone function failed"));  
   }
   delete plc2;
}


void doit2 (DirectionCoordinate& lc,
            Vector<double>& crval, 
            Vector<double>& crpix, 
            Vector<double>& cdelt, 
            Matrix<double>& xform)
{
   if (!allEQ(crval, lc.referenceValue())) {
      throw(AipsError("Failed reference value recovery test"));
   }
//
   if (!allEQ(cdelt, lc.increment())) {
      throw(AipsError("Failed increment recovery test"));
   }
//
   if (!allEQ(crpix, lc.referencePixel())) {
      throw(AipsError("Failed reference pixel recovery test"));
   }
//
   if (!allEQ(xform, lc.linearTransform())) {
      throw(AipsError("Failed Direction transform recovery test"));
   }
//
   crval /= 2.0;
   if (!lc.setReferenceValue(crval)) {
      throw(AipsError(String("Failed to set reference value because") + lc.errorMessage()));
   }
   if (!allEQ(crval, lc.referenceValue())) {
      throw(AipsError("Failed reference value set/recovery test"));
   }
//
   cdelt *= 3.0;
   if (!lc.setIncrement(cdelt)) {
      throw(AipsError(String("Failed to set increment because") + lc.errorMessage()));
   }
   if (!allEQ(cdelt, lc.increment())) {
      throw(AipsError("Failed increment set/recovery test"));
   }
//
   crpix += 13.0;
   if (!lc.setReferencePixel(crpix)) {
      throw(AipsError(String("Failed to set reference pixel because") + lc.errorMessage()));
   }
   if (!allEQ(crpix, lc.referencePixel())) {
      throw(AipsError("Failed reference pixel set/recovery test"));
  }
//       
   xform.diagonal() = -2.0;
   Matrix<double> xformOut(2,2);
   xformOut = 0.;
   xformOut.diagonal() = 1.0;
   Vector<double> cdeltOut; 
   cdeltOut = cdelt * -2.0;
   if (!lc.setLinearTransform(xform)) {
      throw(AipsError(String("Failed to set linear transform because") + lc.errorMessage()));
   }
   if (!allEQ(xformOut, lc.linearTransform())) {
      throw(AipsError("Failed linear transform set/recovery test (wrong resulting xform)"));
   }
   if (!allEQ(cdeltOut, lc.increment())) {
      throw(AipsError("Failed linear transform set/recovery test (wrong resulting cdelt)"));
   }
}

void doit3 (DirectionCoordinate& lc)
{
//
// Test conversion
//
   const Vector<double>& refPix = lc.referencePixel();
   const Vector<double>& refVal = lc.referenceValue();
   Vector<double> pixel(2), world(2);
//
   pixel = refPix.copy();
   if (!lc.toWorld(world, pixel)) {
      throw(AipsError(String("toWorld conversion (0) failed because ") + lc.errorMessage())); 
   }
   if (!allNear(refVal, world, 1e-6)) {
      throw(AipsError("World values (0) are wrong"));
   }
//
   world = refVal.copy();
   if (!lc.toPixel(pixel, world)) {
      throw(AipsError(String("toPixel conversion (0) failed because ") + lc.errorMessage())); 
   }
   if (!allNear(refPix, pixel, 1e-6)) {
      throw(AipsError("Pixel values (0) are wrong"));
   }
//
   Vector<String> axisUnits = lc.worldAxisUnits().copy();
   axisUnits.set("rad");
   if (!lc.setWorldAxisUnits(axisUnits)) {
      throw(AipsError(String("failed to set world axis units to radians because ") + lc.errorMessage())); 
   }
//
   pixel(0) = 12.2;
   pixel(1) = -22.2;
//
   if (!lc.toWorld(world, pixel)) {
      throw(AipsError(String("toWorld conversion (1) failed because ") + lc.errorMessage()));
   }
   Vector<double> pixel2(2);
   if (!lc.toPixel(pixel2, world)) {
      throw(AipsError(String("toPixel conversion (1) failed because ") + lc.errorMessage()));
   }
   if (!allNear(pixel2, pixel, 1e-6)) {
         throw(AipsError("Coordinate conversion reflection 1 failed"));
   }
   MVDirection dirV;
   if (!lc.toWorld(dirV, pixel)) {
      throw(AipsError(String("toWorld conversion (3) failed because ") + lc.errorMessage())); 
   }
   if (!allNear(dirV.get(), world, 1e-6)) {
         throw(AipsError("Coordinate conversion values (MVDirection) are wrong"));
   }
   Vector<double> pix2;
   pix2 = lc.toPixel(dirV);
   AlwaysAssert(allNear(pix2, pixel, 1e-6), AipsError);

   axisUnits.set("deg");
   if (!lc.setWorldAxisUnits(axisUnits)) {
      throw(AipsError(String("failed to set world axis units to degrees because ") + lc.errorMessage())); 
   }
   if (!allNear(dirV.get(), world, 1e-6)) {
         throw(AipsError("Coordinate conversion values (MVDirection) are wrong"));
   }
   Vector<double> pixel3;
   if (!lc.toPixel(pixel3, dirV)) {
      throw(AipsError(String("toPixel conversion (3) failed because ") + lc.errorMessage()));
   }
   if (!allNear(pixel3, pixel, 1e-6)) {
         throw(AipsError("Coordinate conversion reflection 3 failed"));
   }
//
   MDirection dir;
   if (!lc.toWorld(dir, pixel)) {
      throw(AipsError(String("toWorld conversion (2) failed because ") + lc.errorMessage())); 
   }
   if (!allNear(dir.getValue().get(), world, 1e-6)) {
         throw(AipsError("Coordinate conversion values (MDirection) are wrong"));
   }
   axisUnits.set("rad");
   if (!lc.setWorldAxisUnits(axisUnits)) {
      throw(AipsError(String("failed to set world axis units to radians because ") + lc.errorMessage())); 
   }
   if (!lc.toPixel(pixel3, dir)) {
      throw(AipsError(String("toPixel conversion (2) failed because ") + lc.errorMessage()));
   }
   if (!allNear(pixel3, pixel, 1e-6)) {
         throw(AipsError("Coordinate conversion reflection 2 failed"));
   }
   MDirection converted = MDirection::Convert(dir, MDirection::SUPERGAL)();
   Vector<double> pixel4;
   if (!lc.toPixel(pixel4, converted)) {
	   throw(AipsError(String("toPixel conversion (3) after conversion failed because ") + lc.errorMessage()));
   }
   if (!allNear(pixel4, pixel, 1e-5)) {
	   throw(AipsError("Coordinate conversion reflection 3 failed"));
   }
// relative/absolute world

   {
      Vector<double> refVal = lc.referenceValue().copy();
      Vector<double> world4 = refVal;
      lc.makeWorldRelative(world4);
      if (!allNearAbs(world4, 0.0, 1e-6)) {
            throw(AipsError("Coordinate makeWorldRelative 1 gave wrong results"));
      }
//
      Vector<double> incr = 100.0*lc.increment().copy();
      world4 += incr;
      Vector<double> tmp = world4.copy();
      lc.makeWorldRelative(world4);
      Vector<double> world4b(world4.copy());
      lc.makeWorldAbsolute (world4);
      if (!allNearAbs(world4, tmp, 1e-6)) {
            throw(AipsError("Coordinate makeWorldAbsolute/Relative reflection 1 gave wrong results"));
      }
//
      world4 = lc.referenceValue() - 100.0*incr;
      tmp = world4;
      lc.makeWorldRelative(world4);
      lc.makeWorldAbsolute (world4);
      if (!allNearAbs(world4, tmp, 1e-6)) {
            throw(AipsError("Coordinate makeWorldAbsolute 2 gave wrong results"));
      }
   }
//
   {
      MDirection coord;
      lc.toWorld(coord, lc.referencePixel() + 10.0);
//
      MVDirection mv1 = coord.getValue();
      lc.makeWorldRelative(coord);
      MVDirection mv2 = coord.getValue();
      lc.makeWorldAbsolute(coord);
      MVDirection mv3 = coord.getValue();
      if (!allNearAbs(mv1.get(),mv3.get(), 1e-6)) {
         throw(AipsError("Coordinate makeWorldAbsolute/Relative reflection 2 failed"));
      }
   }
//
// Formatting.  
//
   int32_t prec;
   Coordinate::formatType fType = Coordinate::SCIENTIFIC;
   lc.getPrecision(prec, fType, true, 6, 4, 2);
   if (prec != 6) {
      throw(AipsError("Failed getPrecision test 1"));
   }
   fType = Coordinate::FIXED;
   lc.getPrecision(prec, fType, true, 6, 4, 2);
   if (prec != 4) {
      throw(AipsError("Failed getPrecision test 2"));
   }
//
   String unit("rad");
   double val = 0.12343;
   Quantum<double> valq(0.12343, "rad");
   valq.convert(Unit("deg"));
//
   String str = lc.format(unit, Coordinate::FIXED, val, 0,
                          true, true, 4);
   if (unit!="rad" || str != "0.1234") {
      throw(AipsError("Failed format test 1a"));
   }
   uint32_t axis = 0;
   int32_t prec2 = 4;
   str = lc.formatQuantity(unit, Coordinate::FIXED, valq, axis,
                           true, true, prec2);
   if (unit!="rad" || str != "0.1234") {
      throw(AipsError("Failed format test 1b"));
   }
//
   str = lc.format(unit, Coordinate::SCIENTIFIC, val, 0,
                   true, true, 4);
   if (unit!="rad" || str != "1.2343e-01") {
      throw(AipsError("Failed format test 2a"));
   }
   str = lc.formatQuantity(unit, Coordinate::SCIENTIFIC, valq, 0,
                           true, true, 4);
   if (unit!="rad" || str != "1.2343e-01") {
      throw(AipsError("Failed format test 2b"));
   }
//
   str = lc.format(unit, Coordinate::FIXED, val, 1,
                   true, true, 4);
   if (unit!="rad" || str != "0.1234") {
      throw(AipsError("Failed format test 3a"));
   }
   str = lc.formatQuantity(unit, Coordinate::FIXED, valq, 1,
                           true, true, 4);
   if (str != "0.1234") {
      throw(AipsError("Failed format test 3b"));
   }
//
   str = lc.format(unit, Coordinate::SCIENTIFIC, val, 1,
                   true, true, 4);
   if (unit!="rad" || str != "1.2343e-01") {
      throw(AipsError("Failed format test 4a"));
   }
   str = lc.formatQuantity(unit, Coordinate::SCIENTIFIC, valq, 1,
                           true, true, 4);
   if (unit!="rad" || str != "1.2343e-01") {
      throw(AipsError("Failed format test 4b"));
   }
//
// Fairly ugly way to work out what kind of MDirection
// we have in the DC.  Need to know this to figure out what
// the formatting is going to do !
//
   str = lc.format(unit, Coordinate::TIME, val, 0, true,
                   true, 4);
   String str2 = lc.formatQuantity(unit, Coordinate::TIME, valq, 0,
                                   true, true, 4);
   MDirection::GlobalTypes globalType = dir.globalType(lc.directionType());
   if (globalType==MDirection::GRADEC) {
      if (str != "00:28:17.2843" || str2 != "00:28:17.2843") {
         throw(AipsError("Failed format test 5a"));
      }
   } else if (globalType==MDirection::GLONGLAT) {
      if (str != "+007.04.19.2650" || str2 != "+007.04.19.2650") {
         throw(AipsError("Failed format test 5b"));
      }
   } else {
      throw(AipsError("Internal error"));
   }
//
   str = lc.format(unit, Coordinate::TIME, val, 1, true, true, 4);
   str2 = lc.formatQuantity(unit, Coordinate::TIME, valq, 1, true,
                            true, 4);
   if (globalType==MDirection::GRADEC) {
      if (str != "+07.04.19.2650" || str2 != "+07.04.19.2650") {
         throw(AipsError("Failed format test 6a"));
      }
   } else if (globalType==MDirection::GLONGLAT) {
      if (str != "+07.04.19.2650" || str2 != "+07.04.19.2650") {
         throw(AipsError("Failed format test 6b"));
      }
   } else {
      throw(AipsError("Internal error"));
   }
}   


void doit4 (DirectionCoordinate& dC)
//
// Mixed conversions 
//
{
   Vector<double> pixelIn(2), worldIn(2), pixelOut, worldOut;
   Vector<bool> pixelAxes(2), worldAxes(2);
   Vector<String> units = dC.worldAxisUnits().copy();
   Vector<double> refVal = dC.referenceValue().copy();
   Vector<double> refPix = dC.referencePixel().copy();
//
   IPosition shape(2, 512);
   if (!dC.setWorldMixRanges(shape)) {
      throw(AipsError(String("setMixRanges failed because ") + dC.errorMessage()));
   }
//
// Forced errors
//
   pixelAxes.set(false);
   worldAxes.set(false);
   Vector<double> minWorld = dC.worldMixMin().copy();
   Vector<double> maxWorld = dC.worldMixMax().copy();
   if (dC.toMix(worldOut, pixelOut, worldIn, pixelIn, worldAxes, pixelAxes, minWorld, maxWorld)) {
      throw(AipsError(String("Forced fail 1 of toMix did not occur")));
   }
   pixelAxes(0) = true;
   if (dC.toMix(worldOut, pixelOut, worldIn, pixelIn, worldAxes, pixelAxes, minWorld, maxWorld)) {
      throw(AipsError(String("Forced fail 2 of toMix did not occur")));
   }
//
// pixel,pixel->world,world
//
   pixelAxes.set(true);
   worldAxes.set(false);
   pixelIn = dC.referencePixel().copy();
//
   if (!dC.toMix(worldOut, pixelOut, worldIn, pixelIn, worldAxes, pixelAxes, minWorld, maxWorld)){ 
      throw(AipsError(String("Failed pixel->world conversion failed because ")
                  + dC.errorMessage()));
   }
   if (!allNear(pixelOut, dC.referencePixel(), 1e-8)) {
      throw(AipsError(String("Failed pixel->world consistency test 1a")));
   }
   if (!allNear(worldOut, dC.referenceValue(), 1e-8)) {
      throw(AipsError(String("Failed pixel->world consistency test 1b")));
  }
//
// world,world->pixel,pixel
//
   pixelAxes.set(false);
   worldAxes.set(true);
   worldIn = dC.referenceValue().copy();
   if (!dC.toMix(worldOut, pixelOut, worldIn, pixelIn, worldAxes, pixelAxes, minWorld, maxWorld)) {
      throw(AipsError(String("Failed world->pixel conversion failed because ")
                  + dC.errorMessage()));
   }
   if (!allNear(pixelOut, dC.referencePixel(), 1e-8)) {
      throw(AipsError(String("Failed world->pixel consistency test 1a")));
  }
   if (!allNear(worldOut, dC.referenceValue(), 1e-8)) {
      throw(AipsError(String("Failed world->pixel consistency test 1b")));
   }
//
// world,pixel -> pixel,world
//
   worldIn(0) = dC.referenceValue()(0);
   pixelIn(1) = dC.referencePixel()(1);
   pixelAxes.set(false);
   worldAxes.set(false);
   worldAxes(0) = true;
   pixelAxes(1) = true;
   if (!dC.toMix(worldOut, pixelOut, worldIn, pixelIn, worldAxes, pixelAxes, minWorld, maxWorld)) {
      throw(AipsError(String("Conversion failed because ")
                  + dC.errorMessage()));
   }
   if (!allNear(pixelOut, dC.referencePixel(), 1e-6)) {
      throw(AipsError(String("Failed world->pixel consistency test 2a")));
   }
   if (!allNear(worldOut, dC.referenceValue(), 1.0e-6)) {
      throw(AipsError(String("Failed world->pixel consistency test 2b")));
   }
//
// pixel, world -> world, pixel
//
   pixelIn(0) = dC.referencePixel()(0);
   worldIn(1) = dC.referenceValue()(1);
   pixelAxes.set(false);
   worldAxes.set(false);
   worldAxes(1) = true;
   pixelAxes(0) = true;
   if (!dC.toMix(worldOut, pixelOut, worldIn, pixelIn, worldAxes, pixelAxes, minWorld, maxWorld)) {
      throw(AipsError(String("Conversion failed because ")
                  + dC.errorMessage()));
   }
   if (!allNear(pixelOut, dC.referencePixel(), 1e-6)) {
      throw(AipsError(String("Failed world->pixel consistency test 3a")));
   }
   if (!allNear(worldOut, dC.referenceValue(), 1e-6)) {
      throw(AipsError(String("Failed world->pixel consistency test 3b")));
   }
//
// world,pixel -> pixel,world -> world,pixel
// Do it off the reference values
//
   worldIn(0) = dC.referenceValue()(0) + 5*dC.increment()(0);
   pixelIn(1) = 2.32;
   pixelAxes.set(false); pixelAxes(1) = true;
   worldAxes.set(false); worldAxes(0) = true;
   Vector<double> saveWorldIn(worldIn.copy());
   Vector<double> savePixelIn(pixelIn.copy());
   if (!dC.toMix(worldOut, pixelOut, worldIn, pixelIn, worldAxes, pixelAxes, minWorld, maxWorld)) {
      throw(AipsError(String("Conversion failed because ")
                  + dC.errorMessage()));
   }
   pixelIn(0) = pixelOut(0);
   worldIn(1) = worldOut(1);
   pixelAxes.set(false); pixelAxes(0) = true;
   worldAxes.set(false); worldAxes(1) = true;
   if (!dC.toMix(worldOut, pixelOut, worldIn, pixelIn, worldAxes, pixelAxes, minWorld, maxWorld)) {
      throw(AipsError(String("Conversion failed because ")
                  + dC.errorMessage()));
   }
   if (!near(worldOut(0), saveWorldIn(0), 1e-6)) {
      throw(AipsError("Failed mixed conversion reflection test 1a"));
   }
   if (!near(pixelOut(1), savePixelIn(1), 1e-6)) {
      throw(AipsError("Failed mixed conversion reflection test 1a"));
   }
//
// pixel,world -> world,pixel -> pixel, world
// Do it off the reference values
//
   worldIn(1) = dC.referenceValue()(1) + 5*dC.increment()(1);
   pixelIn(0) = 2.32;
   pixelAxes.set(false); pixelAxes(0) = true;
   worldAxes.set(false); worldAxes(1) = true;
   saveWorldIn = worldIn.copy();
   savePixelIn = pixelIn.copy();
   if (!dC.toMix(worldOut, pixelOut, worldIn, pixelIn, worldAxes, pixelAxes, minWorld, maxWorld)) {
      throw(AipsError(String("Conversion failed because ")
                  + dC.errorMessage()));
   }
   pixelIn(1) = pixelOut(1);
   worldIn(0) = worldOut(0);
   pixelAxes.set(false); pixelAxes(1) = true;
   worldAxes.set(false); worldAxes(0) = true;
   if (!dC.toMix(worldOut, pixelOut, worldIn, pixelIn, worldAxes, pixelAxes, minWorld, maxWorld)) {
      throw(AipsError(String("Conversion failed because ")
                  + dC.errorMessage()));
   }
   if (!near(worldOut(1), saveWorldIn(1), 1e-6)) {
      throw(AipsError("Failed mixed conversion reflection test 2a"));
   }
   if (!near(pixelOut(0), savePixelIn(0), 1e-6)) {
      throw(AipsError("Failed mixed conversion reflection test 2a"));
   }
}

void doit5 (DirectionCoordinate& dC)
//
// Fourier coordinate. LinearXform has been tested
// pretty hard (it does the work for DirectionCoordinate) so don't fuss about with 
// the values much
//
{
   Vector<bool> axes(2, true);
   Vector<int32_t> shape(2);
   shape(0) = 128;
   shape(1) = 256;

// All axes

   {
      dC.setWorldAxisUnits(Vector<String>(2, "arcmin"));
      Vector<double> inc(2, 1);
      inc[0] = -1;
      dC.setIncrement(inc);
      std::unique_ptr<Coordinate> pC(dC.makeFourierCoordinate (axes, shape));
//
      Vector<String> units2 = pC->worldAxisUnits();
      Vector<String> names2 = pC->worldAxisNames();
      Vector<double> crval2 = pC->referenceValue();
      Vector<double> crpix2 = pC->referencePixel();
      Vector<double> inc2 = pC->increment();
      if (units2(0)!=String("lambda") || units2(1)!=String("lambda")) {
         throw(AipsError("makeFourierCoordinate (1) failed units test"));
      }
      if (names2(0)!=String("UU") || names2(1)!=String("VV")) {
         throw(AipsError("makeFourierCoordinate (1) failed names test"));
      }
      if (!allNear(crval2,0.0,1e-13)) {
         throw(AipsError("makeFourierCoordinate (1) failed crval test"));
      }
      // inc tests verity CAS-13629
      if (! near(inc2[0], -2*13.4287, 1e-5)) {
         throw(AipsError("makeFourierCoordinate (1) failed cdelt[0] test"));
      }
      if (! near(inc2[1], 13.4287, 1e-5)) {
         throw(AipsError("makeFourierCoordinate (1) failed cdelt[1] test"));
      }
      for (uint32_t i=0; i<pC->nPixelAxes(); i++) { 
         if (!near(double(int32_t(shape(i)/2)), crpix2(i))) {
            throw(AipsError("makeFourierCoordinate (1) failed crpix test"));
         }
      }
   }

// Not all axes 

   {
      axes.set(true);
      axes(1) = false;
      Coordinate* pC = dC.makeFourierCoordinate (axes, shape);
      if (pC) {
         delete pC;
         throw(AipsError("Failed to induce forced error in makeFourierCoordinate (2)"));
      }
      delete pC;
   }
}

void doit6 () 
{
   Vector<double> crval(2);
   Vector<double> cdelt(2);
   Vector<double> crpix(2);
   crval(0) = 10.0; 
   crval(1) = -50.0;
   crpix(0) = 100.0; 
   crpix(1) = 120.0;
   cdelt(0) = -10.0 / 3600.0; 
   cdelt(1) = 20.0 / 3600;
   Matrix<double> xform(2,2);
   xform = 0.0;
   xform.diagonal() = 1.0;
   Projection proj(Projection::CAR);
   MDirection::Types type = MDirection::J2000;
   double longPole = 175;
   double latPole = -90;
//
   DirectionCoordinate dc(type, proj, 
                          crval(0)*C::pi/180.0, 
                          crval(1)*C::pi/180.0, 
                          cdelt(0)*C::pi/180.0, 
                          cdelt(1)*C::pi/180.0, 
                          xform, crpix(0), crpix(1), 
                          longPole*C::pi/180.0, 
                          latPole*C::pi/180.0);
//
   Vector<double> poles = dc.longLatPoles();
   bool ok = (near(poles(0), crval(0)) &&
              near(poles(1), crval(1)) &&
              near(poles(2), longPole));

// The lat pole get recomputed so don't test it

   if (!ok) {
      throw(AipsError("Failed longLatPoles 1 extraction test"));  
   }
//
   Quantum<double> refLong(10.0, Unit("deg"));
   Quantum<double> refLat(-50.0, Unit("deg"));
   Quantum<double> incLong(-10.0, Unit("arcsec"));
   Quantum<double> incLat(20.0, Unit("arcsec"));
   Quantum<double> longPole2(175.0, Unit("deg"));
   Quantum<double> latPole2(-90.0, Unit("deg"));
//
   DirectionCoordinate dc2(type, proj, refLong, refLat,
                           incLong, incLat,
                           xform, crpix(0), crpix(1), 
                           longPole2, latPole2);
//
   Vector<double> poles2 = dc2.longLatPoles();
   ok = (near(poles2(0), refLong.getValue(Unit("deg"))) &&
         near(poles2(1), refLat.getValue(Unit("deg"))) &&
         near(poles2(2), longPole2.getValue("deg")));

// The lat pole get recomputed so don't test it

   if (!ok) {
      throw(AipsError("Failed longLatPoles 2 extraction test"));  
   }
}

void doit7 ()
{
//
// Test conversion with reference change
//
   Projection proj;
   Vector<double> crval, crpix, cdelt;
   Matrix<double> xform;
//
   DirectionCoordinate lc = makeCoordinate(MDirection::J2000,
                                           proj, crval, crpix,
                                           cdelt, xform);
//
   Vector<String> units = lc.worldAxisUnits().copy();
   units = String("deg");
   lc.setWorldAxisUnits(units);
   Vector<double> rv(2);
   rv(0) = 120.0;
   rv(1) = -35.0;
   lc.setReferenceValue(rv);
//
   lc.setReferenceConversion(MDirection::GALACTIC);
   MDirection::Types type2;
   lc.getReferenceConversion (type2);
   if (type2!=MDirection::GALACTIC) {
      throw(AipsError(String("did not recover referenceConversion type")));
   }
//
   Vector<double> pixel = lc.referencePixel().copy() + 10.0;
   Vector<double> world;
   if (!lc.toWorld(world, pixel)) {
      throw(AipsError(String("toWorld + type change conversion (1) failed because ") + lc.errorMessage()));
   }
//
   Vector<double> pixel2;
   if (!lc.toPixel(pixel2, world)) {
      throw(AipsError(String("toPixel + type change conversion (1) failed because ") + lc.errorMessage()));
   }
   if (!allNear(pixel, pixel2, 1e-5)) {
         throw(AipsError("Reference change conversion reflection 1 failed"));
   }    
}
void doit8 ()
{
   Projection proj;
   Vector<double> crval, crpix, cdelt;
   Matrix<double> xform;

// No frame conversion

   {
      DirectionCoordinate lc = makeCoordinate(MDirection::J2000,
                                           proj, crval, crpix,
                                           cdelt, xform);
//
      Vector<bool> failures, failures2;
      const int32_t nCoord = 1000;
      Matrix<double> pixel(2, nCoord), pixel2;
      Matrix<double> world(2, nCoord);
//
      double incr = 1001.0 / nCoord;             // Make sure pixel!=0 so dont have problems with 'near' function
      double pix = -500.0;
      for (int32_t i=0; i<nCoord; i++) {   
        pixel.column(i) = pix;
        pix += incr;
      }
//
      if (!lc.toWorldMany(world, pixel, failures)) {
         throw(AipsError(String("toWorldMany conversion failed because ") + lc.errorMessage())); 
      }
      if (!lc.toPixelMany(pixel2, world, failures2)) {
         throw(AipsError(String("toPixelMany conversion failed because ") + lc.errorMessage())); 
      }
//
      if (!allNear(pixel, pixel2, 0.1)) {
         throw(AipsError("to{World,Pixel}Many reflection failed"));
      }
//
      Vector<double> world2;
      for (int32_t i=0; i<nCoord; i++) {
         if (!lc.toWorld(world2, pixel.column(i))) {
            throw(AipsError(String("toWorld failed because ") + lc.errorMessage())); 
         }
         if (!allNear(world2, world.column(i), 1e-5)) {
            throw(AipsError("World conversions gave wrong results in toWorldMany"));
         }
      }    
   }

// With frame conversion

   {
      DirectionCoordinate lc = makeCoordinate(MDirection::J2000,
                                           proj, crval, crpix,
                                           cdelt, xform);
      lc.setReferenceConversion (MDirection::GALACTIC);
//
      Vector<bool> failures, failures2;
      const int32_t nCoord = 1000;
      Matrix<double> pixel(2, nCoord), pixel2;
      Matrix<double> world(2, nCoord);
//
      double incr = 1001.0 / nCoord;             // Make sure pixel!=0 so dont have problems with 'near' function
      double pix = -500.0;
      for (int32_t i=0; i<nCoord; i++) {   
        pixel.column(i) = pix;
        pix += incr;
      }
//
      if (!lc.toWorldMany(world, pixel, failures)) {
         throw(AipsError(String("toWorldMany conversion failed because ") + lc.errorMessage())); 
      }
      if (!lc.toPixelMany(pixel2, world, failures2)) {
         throw(AipsError(String("toPixelMany conversion failed because ") + lc.errorMessage())); 
      }
//
      if (!allNear(pixel, pixel2, 0.1)) {
         throw(AipsError("to{World,Pixel}Many reflection failed"));
      }
//
      Vector<double> world2;
      for (int32_t i=0; i<nCoord; i++) {
         if (!lc.toWorld(world2, pixel.column(i))) {
            throw(AipsError(String("toWorld failed because ") + lc.errorMessage())); 
         }
         if (!allNear(world2, world.column(i), 1e-5)) {
            throw(AipsError("World conversions gave wrong results in toWorldMany"));
         }
      }    
   }


}

void doit9 ()
{
   Matrix<double> xform(2,2);
   xform = 0.0;
   xform.diagonal() = 1.0;
   DirectionCoordinate dc(MDirection::SUN, Projection::SIN, 0.0, 0.0,
                          -1.0e-6, 1.0e-6, xform,
                           0.0, 0.0, 999.0, 999.0);
   Vector<double> pixel(2), world;
   pixel = 0.0;   
   bool ok = dc.toWorld(world, pixel);
   AlwaysAssert(ok, AipsError);
   cerr << "pixel, world = " << pixel << world << endl;
}

void doit10 ()
{
   Matrix<double> xform(2,2);
   xform = 0.0;
   xform.diagonal() = 1.0;
   DirectionCoordinate dc(MDirection::GALACTIC, Projection::SIN, 0.0, 0.0,
                          -1.0e-6, 1.0e-6, xform,
                           0.0, 0.0, 999.0, 999.0);
//
   Vector<String> units = dc.worldAxisUnits().copy();
   units(0) = String("deg");
   units(1) = String("deg");
   if (!dc.setWorldAxisUnits(units)) {
     throw(AipsError(String("failed to set Units because ") + dc.errorMessage())); 
   }
//
   Vector<double> incr  = dc.increment().copy();
   incr(0) = 1.0;
   if (!dc.setIncrement(incr)) {
     throw(AipsError(String("failed to set increment because ") + dc.errorMessage())); 
   }
//
/*
   Vector<double> refVal = dc.referenceValue().copy();
   refVal(0) = 200;
   if (!dc.setReferenceValue(refVal)) {
     throw(AipsError(String("failed to set refVal because ") + dc.errorMessage())); 
   }
*/
//
   Vector<double> refPix = dc.referencePixel().copy();
   refPix(0) = 200;
   if (!dc.setReferencePixel(refPix)) {
     throw(AipsError(String("failed to set refPix because ") + dc.errorMessage())); 
   }
//
   if (!dc.cylindricalFix (180, 180)) {
     throw(AipsError(String("failed to make cylindrical fix because") + dc.errorMessage())); 
   }
}

