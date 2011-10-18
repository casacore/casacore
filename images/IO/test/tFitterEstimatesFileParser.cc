//# tImageFitter.cc:  test the PagedImage class
//# Copyright (C) 1994,1995,1998,1999,2000,2001,2002
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

#include <images/IO/FitterEstimatesFileParser.h>
#include <casa/Utilities/Assert.h>
#include <casa/OS/File.h>
#include <components/ComponentModels/GaussianShape.h>
#include <images/Images/FITSImage.h>
#include <images/Images/ImageMetaData.h>
#include <coordinates/Coordinates/DirectionCoordinate.h>

#include <casa/namespace.h>

void writeTestString(const String& test) {
    cout << "\n" << "*** " << test << " ***" << endl;
}


int main() {
	FITSImage fitsImage("gaussian_model_with_noise.fits");
    try {
        {
            writeTestString(
                "test constructor throws error in case where file does not exist"
            );
            Bool thrown = False;
            try {
            	FitterEstimatesFileParser parser("bogusfile.txt", fitsImage);
            }
            catch (AipsError x) {
            	thrown = True;
            }
            if (! thrown) {
            	// if we get here an exception wasn't thrown so the test failed
            	throw AipsError("constructor should have thrown an expeption but did not");
            }
        }
        {
        	writeTestString(
        		"test constructor throws error in case where file is formatted incorrectly"
        	);
        	Bool thrown = False;
            try {
            	FitterEstimatesFileParser parser("./badEstimatesFormat.txt", fitsImage);
            }
            catch (AipsError x) {
            	thrown = True;
            }
            if (! thrown) {
            	// if we get here an exception wasn't thrown so the test failed
            	throw AipsError("constructor should have thrown an exception but did not");
            }
        }
        {
        	writeTestString(
        		"test constructor throws error in case where fixed parameters contain unrecognized flag"
        	);
        	Bool thrown = False;
            try {
            	FitterEstimatesFileParser parser(
            		"./badFixedFormat.txt", fitsImage
            	);
            }
            catch (AipsError x) {
            	thrown = True;
            }
            if (! thrown) {
            	// if we get here an exception wasn't thrown so the test failed
            	throw AipsError("constructor should have thrown an exception but did not");
            }
        }
        {
        	writeTestString(
        		"test constructor parses correctly formatted file"
        	);
            FitterEstimatesFileParser parser("./goodEstimatesFormat.txt", fitsImage);
            ComponentList compList = parser.getEstimates();
            AlwaysAssert(compList.nelements() == 2, AipsError);
            Vector<Double> expectedFlux(2);
            expectedFlux[0] = 67.985402127407909;
            expectedFlux[1] = 135.88582250215657;
            Vector<Quantity> flux;
            Vector<Double> dirTuple;
            Vector<Vector<Double> > expectedDir(2);
            expectedDir[0].resize(2);
            expectedDir[1].resize(2);
            expectedDir[0][0] = 0.00167746;
            expectedDir[0][1] = 0.000121203;
            expectedDir[1][0] = 0.00190532;
            expectedDir[1][1] = 0.000610865;
            Vector<Double> expectedMaj(2);
            expectedMaj[0] = 4;
            expectedMaj[1] = 6.5;
            Vector<Double> expectedMin(2);
            expectedMin[0] = 3.0;
            expectedMin[1] = 4.1;
            Vector<Double> expectedPA(2);
            expectedPA[0] = 60.0;
            expectedPA[1] = 95.11;
            Vector<String> expectedFixed(2);
            expectedFixed[0] = "";
            expectedFixed[1] = "a";
            for (uInt i=0; i<compList.nelements(); i++) {
            	compList.getFlux(flux,i);
                AlwaysAssert(near(flux[0].getValue(), expectedFlux[i], 1e-7), AipsError);
            	dirTuple = compList.getRefDirection(i).getAngle().getValue();
            	AlwaysAssert(dirTuple.size() == 2, AipsError);
            	for (uInt j=0; j<dirTuple.size(); j++) {
            		AlwaysAssert(near(dirTuple[j], expectedDir[i][j], 1e-5), AipsError);
            	}
            	const GaussianShape *gaussShape = static_cast<const GaussianShape * >(
            		compList.getShape(i)
            	);
            	AlwaysAssert(
            		near(gaussShape->majorAxis().getValue(), expectedMaj[i], 1e-8),
            		AipsError
            	);
               	AlwaysAssert(
                	near(gaussShape->minorAxis().getValue(), expectedMin[i], 1e-8),
                	AipsError
                );
               	AlwaysAssert(
                	near(gaussShape->positionAngle().getValue(), expectedPA[i], 1e-8),
                	AipsError
                );
            }
           	Vector<String> fixed = parser.getFixed();
           	for(uInt i=0; i<fixed.size(); i++) {
           		AlwaysAssert(fixed[i] == expectedFixed[i], AipsError);
           	}
        }
        {
            FitterEstimatesFileParser parser("./goodEstimatesFormat.txt", FITSImage("jyperbeammap.fits"));
            ComponentList compList = parser.getEstimates();
            Vector<Double> expectedFlux(2);
            expectedFlux[0] = 20.0;
            expectedFlux[1] = 39.975;
            Vector<Quantity> flux;
            for (uInt i=0; i<compList.nelements(); i++) {
            	compList.getFlux(flux,i);
                AlwaysAssert(near(flux[0].getValue(), expectedFlux[i], 1e-7), AipsError);
            }
        }
    }
    catch (AipsError x) {
        cerr << "Exception caught: " << x.getMesg() << endl;
        return 1;
    }
    cout << "ok" << endl;
    return 0;
}

