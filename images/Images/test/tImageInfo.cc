 //# tImageInfo.cc: Miscellaneous information related to an image
//# Copyright (C) 1998,1999,2000,2002
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
//#
//# $Id$

#include <casacore/images/Images/ImageInfo.h>

#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Containers/RecordField.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Quanta/QLogical.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/iostream.h>
#include <casacore/coordinates/Coordinates/DirectionCoordinate.h>

#include <casacore/casa/namespace.h>

void equal (const ImageInfo& ii1, const ImageInfo& ii2)
{
    const GaussianBeam& b1 = ii1.restoringBeam();
    const GaussianBeam& b2 = ii2.restoringBeam();
    AlwaysAssert(b1 == b2, AipsError);
    AlwaysAssertExit(ii1.imageType()==ii2.imageType());    
    AlwaysAssertExit(ii1.objectName()==ii2.objectName());  
}

int main()
{

try {


// Default constructor ;

    ImageInfo mii;

//
// Restoring beam
//
    AlwaysAssert(mii.restoringBeam() == mii.defaultRestoringBeam(), AipsError);
    GaussianBeam beam;
    AlwaysAssert(mii.defaultRestoringBeam() == beam,  AipsError);
//
    beam = GaussianBeam(
    	Quantum<Double>(45.0, "arcsec"),
    	Quantum<Double>(45.0, "arcsec"),
    	Quantum<Double>(-45.0, "deg")
    );
    mii.setRestoringBeam(beam);
    AlwaysAssert(mii.restoringBeam() == beam, AipsError);
    mii.setRestoringBeam(beam);

    AlwaysAssert(mii.restoringBeam() == beam, AipsError);
    beam.setMajorMinor(Quantum<Double>(1.0, "deg"), beam.getMinor());
    mii.setRestoringBeam(beam);
    AlwaysAssert(mii.restoringBeam() == beam, AipsError);
    mii.removeRestoringBeam();
    AlwaysAssertExit(mii.restoringBeam().isNull());
//
// ImageType
//
    for (uInt i=0; i<ImageInfo::nTypes; i++) {
       ImageInfo::ImageTypes type = static_cast<ImageInfo::ImageTypes>(i);
       {
          mii.setImageType(type);
          AlwaysAssertExit(type==mii.imageType());
       }
       {
          String typeS = ImageInfo::imageType(type);
          ImageInfo::ImageTypes type2 = ImageInfo::imageType(typeS);
          AlwaysAssertExit(type==type2);
       }
    }
//
// ObjectName
//
    {
      String objectName("PKS133-33");
      mii.setObjectName(objectName);
      AlwaysAssertExit(objectName==mii.objectName());
   }
//
// Copy constructor and assignemnt
//
    mii.setRestoringBeam(beam);
    mii.setImageType(ImageInfo::SpectralIndex);
    mii.setObjectName(String("IC4296"));
    ImageInfo mii2(mii);
    equal(mii2, mii);
//
    GaussianBeam beam2(
    	Quantum<Double>(7.2, "arcsec"),
    	Quantum<Double>(3.6, "arcsec"),
    	Quantum<Double>(-90.0, "deg")
    );
    mii2.setRestoringBeam(beam2);
    mii2.setImageType(ImageInfo::Intensity);
    mii.setObjectName(String("NGC1399"));
    mii = mii2;
    equal(mii2, mii);
//
// Record conversion
//
    Record rec;
    String error;
    AlwaysAssertExit(mii.toRecord(error, rec));
    ImageInfo mii3;
    Bool ok = mii3.fromRecord(error, rec);
    if (!ok) cout << "Error = " << error << endl;
    equal(mii3, mii);
//
// FITS
//
    Record header;
    AlwaysAssertExit(mii3.toFITS(error, header));
    // the header delivered by toFITS contains fields as fields,
    // the header accepted by fromFITS contains fields as subrecords
    //  -- a round trip is therefore not possible directly.
    // need to construct input record
    RecordDesc keywordNumRec;
    keywordNumRec.addField("value", TpDouble);
    RecordDesc keywordStrRec;
    keywordStrRec.addField("value", TpString);
    Record headerb;
    Record rbmaj(keywordNumRec);
    Record rbmin(keywordNumRec);
    Record rbpa(keywordNumRec);
    Record robject(keywordStrRec);
    RecordFieldPtr<Double> bmajval(rbmaj, 0);
    RecordFieldPtr<Double> bminval(rbmin, 0);
    RecordFieldPtr<Double> bpaval(rbpa, 0);
    RecordFieldPtr<String> objectval(robject, 0);
    bmajval.define(0.002);
    bminval.define(0.001);
    bpaval.define(-90.);
    objectval.define("IC4296");
    headerb.defineRecord("bmaj", rbmaj);
    headerb.defineRecord("bmin", rbmin);
    headerb.defineRecord("bpa", rbpa);
    headerb.defineRecord("object", robject);
    // now try to import it
    ImageInfo mii4;
    Vector<String> error2;
    AlwaysAssertExit(mii4.fromFITS(error2, headerb));
    equal(mii4, mii3);
// output stream
//
    cout << mii3 << endl;
    cout << mii4 << endl;
    {
    	// per plane beam tests
    	ImageInfo myinfo;
    	Quantity majAx(5, "arcsec");
    	Quantity minAx(3, "arcsec");
    	Quantity pa(60, "deg");
    	Bool ok = True;
    	try {
    		// no hyper plane beam shape throws exception
    		myinfo.setBeam(1, 1, majAx, minAx, pa);
    		ok = False;
    	}
    	catch (const AipsError& x) {
    		cout << x.getMesg() << endl;
    	}
    	AlwaysAssert(ok, AipsError);
    	myinfo = ImageInfo();
    	myinfo.setAllBeams(2, 1, GaussianBeam());
    	try {
    		// inconsistent plane throws exception
    		myinfo.setBeam(2, 1, majAx, minAx, pa);
    		ok = False;
    	}
    	catch (AipsError x) {
    		cout << "Exception thrown as expected: "
    			<< x.getMesg() << endl;
    	}
    	AlwaysAssert(ok, AipsError);
    	myinfo = ImageInfo();
    	myinfo.setAllBeams(2, 1, GaussianBeam());
    	try {
    		// incorrect beam spec throws error
    		myinfo.setBeam(0, 0, minAx, majAx, pa);
    		ok = False;
    	}
    	catch (AipsError x) {
    		cout << "Exception thrown as expected: "
    			<< x.getMesg() << endl;
    	}
    	AlwaysAssert(ok, AipsError);
    	myinfo = ImageInfo();
    	myinfo.setAllBeams(2, 1, GaussianBeam());
    	myinfo.setBeam(0, 0, majAx, minAx, pa);
    	GaussianBeam beam = myinfo.restoringBeam(0, 0);
    	AlwaysAssert(beam.getMajor() == majAx, AipsError);
    	AlwaysAssert(beam.getMinor() == minAx, AipsError);
    	AlwaysAssert(beam.getPA() == pa, AipsError);
    	Record rec;
    	String err;
    	// not all beams have been set
    	AlwaysAssert(! myinfo.toRecord(error, rec), AipsError);
    	myinfo.setBeam(1, 0, majAx, minAx, pa);
    	AlwaysAssert(myinfo.toRecord(error, rec), AipsError);
    	ImageInfo myinfo2;
    	myinfo2.fromRecord(err, rec);
    	AlwaysAssert(myinfo2.fromRecord(err, rec), AipsError);
    	beam = myinfo2.restoringBeam(1, 0);
    	AlwaysAssert(beam.getMajor() == majAx, AipsError);
    	AlwaysAssert(beam.getMinor() == minAx, AipsError);
    	AlwaysAssert(beam.getPA() == pa, AipsError);
    	cout << "myinfo2 " << myinfo2 << endl;
    }
    {
    	ImageBeamSet bset(IPosition(2, 10, 4));
    	ImageInfo myinfo = ImageInfo();
    	myinfo.setBeams(bset);
    	ImageBeamSet bset2(IPosition(2, 10, 4));
    	myinfo.setBeams(bset2);
    	AlwaysAssert(myinfo.getBeamSet() == bset2, AipsError);
    	// check that we can set a different size beam set
    	ImageBeamSet bset3(IPosition(2, 11, 4));
		myinfo.setBeams(bset3);
    	AlwaysAssert(myinfo.getBeamSet() == bset3, AipsError);

    }
    {
    	cout << "*** Test getBeamAreaInPixels" << endl;
    	ImageInfo myinfo;
    	GaussianBeam beam(
    		Quantity(4, "arcsec"), Quantity(2, "arcsec"), Quantity(30, "deg")
    	);
    	ImageBeamSet bset(10, 4, beam);
    	bset.setBeam(2, 2,
    		GaussianBeam(
    			Quantity(5, "arcsec"), Quantity(3, "arcsec"),
    			Quantity(30, "deg")
    		)
    	);
    	myinfo.setBeams(bset);
    	DirectionCoordinate dc;
    	dc.setWorldAxisUnits(Vector<String>(2, "arcsec"));
    	dc.setIncrement(Vector<Double>(2, 0.7));
    	for (uInt i=0; i<10; i++) {
    		for (uInt j=0; j<4; j++) {
    			Double expec = (i == 2 && j == 2)
    				? 34.686429656840772 : 18.499429150315081;
    	    	AlwaysAssert(
    	    		near(myinfo.getBeamAreaInPixels(i, j, dc), expec),
    	    		AipsError
    	    	);

    		}
    	}
    }
} catch (const AipsError& x) {
  cout << "Caught error " << x.getMesg() << endl;
  return 1;
} 
  
    cout << "OK" << endl;
    return 0;
}
