//# tImageMetaData.cc:  test the ImageMetaData class
//# Copyright (C) 2009
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

#include <images/Images/ImageMetaData.h>
#include <images/Images/FITSImage.h>
#include <casa/OS/Path.h>
#include <casa/namespace.h>
#include <casa/OS/File.h>

int main() {
    try {
        FITSImage fourAxesImage("ngc5921.clean.fits");
        FITSImage twoAxesImage("ngc5921.clean.no_freq.no_stokes.fits");
   
        ImageMetaData fourAxesImageMetaData(fourAxesImage);
        ImageMetaData twoAxesImageMetaData(twoAxesImage);
        {
            // 3 in the casa format image, but 2 in the FITS image
            AlwaysAssert(fourAxesImageMetaData.spectralAxisNumber() == 2, AipsError);
            AlwaysAssert(twoAxesImageMetaData.spectralAxisNumber() == -1, AipsError);
        }

        {
            AlwaysAssert(fourAxesImageMetaData.nChannels() == 8, AipsError);
            AlwaysAssert(twoAxesImageMetaData.nChannels() == 0, AipsError);
        }

        {
            AlwaysAssert(fourAxesImageMetaData.isChannelNumberValid(1), AipsError);
            AlwaysAssert(! fourAxesImageMetaData.isChannelNumberValid(10), AipsError);
            AlwaysAssert(! twoAxesImageMetaData.isChannelNumberValid(0), AipsError);
        }

        {
            AlwaysAssert(fourAxesImageMetaData.spectralCoordinateNumber() == 2, AipsError);
            AlwaysAssert(twoAxesImageMetaData.spectralCoordinateNumber() == -1, AipsError);
        }

        {
            AlwaysAssert(fourAxesImageMetaData.hasSpectralAxis(), AipsError);
            AlwaysAssert(! twoAxesImageMetaData.hasSpectralAxis(), AipsError);
        }

        {
            AlwaysAssert(fourAxesImageMetaData.polarizationCoordinateNumber() == 1, AipsError);
            AlwaysAssert(twoAxesImageMetaData.polarizationCoordinateNumber() == -1, AipsError);
        }

        {
            AlwaysAssert(fourAxesImageMetaData.hasPolarizationAxis(), AipsError);
            AlwaysAssert(! twoAxesImageMetaData.hasPolarizationAxis(), AipsError);
        }

        {
            AlwaysAssert(fourAxesImageMetaData.stokesPixelNumber("I") == 0, AipsError);
            AlwaysAssert(fourAxesImageMetaData.stokesPixelNumber("Q") == -1, AipsError);
            AlwaysAssert(twoAxesImageMetaData.stokesPixelNumber("I") == -1, AipsError);
        }

        {
            AlwaysAssert(fourAxesImageMetaData.nStokes() == 1, AipsError);
            AlwaysAssert(twoAxesImageMetaData.nStokes() == 0, AipsError);
        }
    
        {
            AlwaysAssert(fourAxesImageMetaData.isStokesValid("I"), AipsError);
            AlwaysAssert(! fourAxesImageMetaData.isStokesValid("Q"), AipsError);
            AlwaysAssert(! twoAxesImageMetaData.isStokesValid("I"), AipsError);
        }

        {
        	// stokesAtPixel
        	AlwaysAssert(twoAxesImageMetaData.stokesAtPixel(0).empty(), AipsError);
        	AlwaysAssert(fourAxesImageMetaData.stokesAtPixel(0) == "I", AipsError);
        	AlwaysAssert(fourAxesImageMetaData.stokesAtPixel(1).empty(), AipsError);

        }
        {
            // TODO test image without a direction coordinate
            AlwaysAssert(fourAxesImageMetaData.directionCoordinateNumber() == 0, AipsError);
            AlwaysAssert(twoAxesImageMetaData.directionCoordinateNumber() == 0, AipsError);
        }         

        {
            // TODO test image without a direction coordinate
            AlwaysAssert(fourAxesImageMetaData.hasDirectionCoordinate(), AipsError);
            AlwaysAssert(twoAxesImageMetaData.hasDirectionCoordinate(), AipsError);
        }         

        {
            // TODO test image without a direction coordinate
            Vector<Int> directionAxesNums = fourAxesImageMetaData.directionAxesNumbers();
            AlwaysAssert(directionAxesNums[0] == 0, AipsError);
            AlwaysAssert(directionAxesNums[1] == 1, AipsError);

            directionAxesNums = twoAxesImageMetaData.directionAxesNumbers();
            AlwaysAssert(directionAxesNums[0] == 0, AipsError);
            AlwaysAssert(directionAxesNums[1] == 1, AipsError);
        }         

        {
            // TODO test image without a direction coordinate
            Vector<Int> directionShape = fourAxesImageMetaData.directionShape();
            AlwaysAssert(directionShape[0] == 6, AipsError);
            AlwaysAssert(directionShape[1] == 11, AipsError);

            directionShape = twoAxesImageMetaData.directionShape();
            AlwaysAssert(directionShape[0] == 6, AipsError);
            AlwaysAssert(directionShape[1] == 11, AipsError);
        } 

        {
            String message;
            AlwaysAssert(fourAxesImageMetaData.areChannelAndStokesValid(message, 1, "I"), AipsError);
            AlwaysAssert(! fourAxesImageMetaData.areChannelAndStokesValid(message, 15, "I"), AipsError);
            AlwaysAssert(! fourAxesImageMetaData.areChannelAndStokesValid(message, 1, "Q"), AipsError);
            AlwaysAssert(! twoAxesImageMetaData.areChannelAndStokesValid(message, 0, "I"), AipsError);
        }
        {
        	// getBeamArea
        	Quantity beamArea;
        	AlwaysAssert( fourAxesImageMetaData.getBeamArea(beamArea), AipsError);
        	beamArea.convert(Unit("arcsec.arcsec"));
        	Double expectedArea = 2769.2432412865101;
        	AlwaysAssert(near(beamArea.getValue(), expectedArea, 1e-8), AipsError);
        	FITSImage noBeam("jyperpixelimage.fits");
        	ImageMetaData noBeamMD = ImageMetaData(noBeam);
        	AlwaysAssert(! noBeamMD.getBeamArea(beamArea), AipsError);
        }
        {
        	// getPixelArea
        	Quantity pixelArea;
        	AlwaysAssert(fourAxesImageMetaData.getDirectionPixelArea(pixelArea), AipsError);
        	pixelArea.convert(Unit("arcsec.arcsec"));
        	Double expectedValue = 225.0;
        	AlwaysAssert(near(pixelArea.getValue(), expectedValue, 1e-8), AipsError);
        }

        cout<< "ok"<< endl;
    }
    catch (AipsError x) {
        cerr << "Exception caught: " << x.getMesg() << endl;
        return 1;
    } 
    return 0;
}

