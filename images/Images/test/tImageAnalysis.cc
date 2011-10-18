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


#include <images/Images/ImageAnalysis.h>
#include <images/Images/FITSImage.h>
#include <casa/namespace.h>


void writeTestString(const String& test) {
    cout << "\n" << "*** " << test << " ***" << endl;
}


int main() {
	Bool ok = True;
	try {
		writeTestString("Verify fix for CAS-2195: error if image has no direction coordinate but does have linear coordiante");
		FITSImage fits("linearCoords.fits");
		ImageAnalysis ia(&fits);
		Record statsOut, region;
		Vector<Int> axes(2);
		axes[0] = 0;
		axes[1] = 1;
		ia.statistics(statsOut, axes, region, "", Vector<String>(0), Vector<Float>(0), Vector<Float>(0));
		Vector<Int> got;
		Vector<Int> expected(2);
		expected[0] = 4;
		expected[1] = 0;
		statsOut.get(RecordFieldId("minpos"), got);
		AlwaysAssert(got.size() == expected.size(), AipsError);
		for (uInt i = 0; i<expected.size(); i++) {
			AlwaysAssert(got[i] == expected[i], AipsError);
		}
		expected[0] = 3;
		expected[1] = 10;
		statsOut.get(RecordFieldId("maxpos"), got);
		AlwaysAssert(got.size() == expected.size(), AipsError);
		for (uInt i = 0; i<expected.size(); i++) {
			AlwaysAssert(got[i] == expected[i], AipsError);
		}
        {
            // CAS-2533
            PagedImage<Float> img("CAS-2533.im");
            ImageAnalysis analysis(&img);

            Vector<casa::Double> wxv(2);
            Vector<casa::Double> wyv(2);
            Vector<casa::Float> z_xval;
            Vector<casa::Float> z_yval;

            wxv[0] = 4.63641;
            wxv[1] = 4.63639;
            wyv[0] = -0.506297;
            wyv[1] = -0.506279;

            bool ok = analysis.getFreqProfile(
                wxv, wyv, z_xval, z_yval,
                "world", "radio velocity",
                0, 0, 0, "", "LSRK"
            );
            AlwaysAssert(ok, AipsError);
            AlwaysAssert(fabs(1-z_xval[0]/137.805) < 1e-5, AipsError); 
            AlwaysAssert(fabs(1-z_xval[1]/133.631) < 1e-5, AipsError); 
            AlwaysAssert(fabs(1-z_xval[2]/129.457) < 1e-5, AipsError); 
            AlwaysAssert(fabs(1-z_xval[3]/125.283) < 1e-5, AipsError); 
            AlwaysAssert(fabs(1-z_xval[4]/121.109) < 1e-5, AipsError); 
            AlwaysAssert(fabs(1-z_xval[5]/116.935) < 1e-5, AipsError); 
            AlwaysAssert(fabs(1-z_xval[6]/112.761) < 1e-5, AipsError); 
            AlwaysAssert(fabs(1-z_xval[7]/108.587) < 1e-5, AipsError); 

            AlwaysAssert(fabs(1-z_yval[0]/-0.146577) < 1e-5, AipsError); 
            AlwaysAssert(fabs(1-z_yval[1]/-0.244666) < 1e-5, AipsError); 
            AlwaysAssert(fabs(1-z_yval[2]/-0.184397) < 1e-5, AipsError); 
            AlwaysAssert(fabs(1-z_yval[3]/0.0869152) < 1e-5, AipsError); 
            AlwaysAssert(fabs(1-z_yval[4]/-0.43336) < 1e-5, AipsError); 
            AlwaysAssert(fabs(1-z_yval[5]/-0.145391) < 1e-5, AipsError); 
            AlwaysAssert(fabs(1-z_yval[6]/-0.0924785) < 1e-5, AipsError); 
            AlwaysAssert(fabs(1-z_yval[7]/-0.131597) < 1e-5, AipsError); 
        }
        {
            // wavelength output
            PagedImage<Float> img("CAS-2533.im");
            ImageAnalysis analysis(&img);

            Vector<casa::Double> wxv(2);
            Vector<casa::Double> wyv(2);
            Vector<casa::Float> z_xval;
            Vector<casa::Float> z_yval;

            wxv[0] = 4.63641;
            wxv[1] = 4.63639;
            wyv[0] = -0.506297;
            wyv[1] = -0.506279;

            bool ok = analysis.getFreqProfile(
                wxv, wyv, z_xval, z_yval,
                "world", "wavelength",
                0, 0, 0, "", "LSRK"
            );

            AlwaysAssert(ok, AipsError);
            AlwaysAssert(fabs(1-z_xval[0]/212.115) < 1e-5, AipsError); 
            AlwaysAssert(fabs(1-z_xval[1]/212.112) < 1e-5, AipsError); 
            AlwaysAssert(fabs(1-z_xval[2]/212.109) < 1e-5, AipsError); 
            AlwaysAssert(fabs(1-z_xval[3]/212.106) < 1e-5, AipsError); 
            AlwaysAssert(fabs(1-z_xval[4]/212.103) < 1e-5, AipsError); 
            AlwaysAssert(fabs(1-z_xval[5]/212.100) < 1e-5, AipsError); 
            AlwaysAssert(fabs(1-z_xval[6]/212.097) < 1e-5, AipsError); 
            AlwaysAssert(fabs(1-z_xval[7]/212.094) < 1e-5, AipsError); 

            AlwaysAssert(fabs(1-z_yval[0]/-0.146577) < 1e-5, AipsError); 
            AlwaysAssert(fabs(1-z_yval[1]/-0.244666) < 1e-5, AipsError); 
            AlwaysAssert(fabs(1-z_yval[2]/-0.184397) < 1e-5, AipsError); 
            AlwaysAssert(fabs(1-z_yval[3]/0.0869152) < 1e-5, AipsError); 
            AlwaysAssert(fabs(1-z_yval[4]/-0.43336) < 1e-5, AipsError); 
            AlwaysAssert(fabs(1-z_yval[5]/-0.145391) < 1e-5, AipsError); 
            AlwaysAssert(fabs(1-z_yval[6]/-0.0924785) < 1e-5, AipsError); 
            AlwaysAssert(fabs(1-z_yval[7]/-0.131597) < 1e-5, AipsError); 
        }
        cout << "ok" << endl;
	}
    catch (AipsError x) {
    	ok = False;
        cerr << "Exception caught: " << x.getMesg() << endl;
    }
	return ok ? 0 : 1;
}

