//# tUVWMachine.cc: This program test the UVWMachine class
//# Copyright (C) 1998
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
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

//# Includes
#include <aips/aips.h>
#include <aips/Exceptions/Error.h>
#include <trial/Measures/UVWMachine.h>
#include <aips/Arrays/Vector.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MEpoch.h>

Int main() {
  try {
    cout << "Test UVWMachine class" << endl;
    cout << "--------------------------------------" << endl;

    // VLA position from an MS
    MPosition mLocation = MPosition(MVPosition(-1601162,
					       -5042003,
					       3554915), 
				    MPosition::ITRF);
    // A time and position frame
    MeasFrame mFrame(MEpoch(Quantity(4.1216294e+09, "s"),
			    MEpoch::TAI),
		     mLocation);
    // A source position
    Vector<Double> d1(2);
    d1(0) = 3.25745692;	// 3C291
    d1(1) = 0.040643336;
    Quantum<Vector<Double> > dir (d1, "rad");
    MDirection mImage(dir, MDirection::B1950);
    MDirection mIm1;
    MDirection mIm2;
    // A UVW position
    MVPosition UVW(-739.048461, -1939.10604, 1168.62562);
    MVPosition UVW1;

    cout << "Original reference:    " << mImage.getRef() << endl;
    cout << "Original phase centre: " << mImage << endl;
    cout << "                       " << mImage.getAngle("deg") << endl;
    cout << "Input UVW: " << UVW << endl;

    
    { 
      cout << "-------- B1950 -> J2000 --------------" << endl;
      cout << "Input coordinates: " << mImage << endl;
      cout << "                 : " << mImage.getAngle("deg") << endl;
     // A new output reference
      MDirection::Ref oref(MDirection::J2000);
      // A UVW machine
      UVWMachine um(oref, mImage);
      cout << "Conversion to: " << oref << endl;
      cout << "Converted to:  " << um.phaseCenter() << endl;
      cout << "               " << um.phaseCenter().getAngle("deg") << endl;
      cout << "UVW rotation matrix: " << um.rotationUVW() << endl;
      mIm1 = um.phaseCenter();	// save for later
      MVPosition uvw(UVW);	// save UVW
      uvw *= um.rotationUVW();
      cout << "New UVW:        " << uvw << endl;
      cout << "Phase rotation: " << uvw * um.rotationPhase() << endl;
      uvw = UVW;
      Double ph;
      um.convertUVW(ph, uvw);
      cout << "Phase: "  << ph << ", UVW: " << uvw << endl;
    }

    { 
      cout << "-------- B1950 -> B1950+offset ---------" << endl;
      cout << "Input coordinates: " << mImage << endl;
      cout << "                 : " << mImage.getAngle("deg") << endl;
     // A new output reference
      MDirection::Ref oref(MDirection::B1950);
      MDirection odir(MVDirection(-0.990818, -0.125371, 0.0506217), oref);
      // A UVW machine
      UVWMachine um(odir, mImage);
      cout << "Conversion to: " << oref << endl;
      cout << "Converted to:  " << um.phaseCenter() << endl;
      cout << "               " << um.phaseCenter().getAngle("deg") << endl;
      cout << "UVW rotation matrix: " << um.rotationUVW() << endl;
      MVPosition uvw(UVW);	// save UVW
      uvw *= um.rotationUVW();
      cout << "New UVW:        " << uvw << endl;
      cout << "Phase rotation: " << uvw * um.rotationPhase() << endl;
      uvw = UVW;
      Double ph;
      um.convertUVW(ph, uvw);
      cout << "Phase: "  << ph << ", UVW: " << uvw << endl;
    }

    { 
      cout << "-------- B1950 -> J2000+offset ---------" << endl;
     // A new output reference
      cout << "Input coordinates: " << mImage << endl;
      cout << "                 : " << mImage.getAngle("deg") << endl;
      MDirection::Ref oref(MDirection::J2000);
      MVDirection mvo(MVPosition(mIm1.getValue()) +
                      MVPosition(0.01, 0.01, 0.01));
      mvo.adjust();
      MDirection odir(mvo, oref);
      // A UVW machine
      UVWMachine um(odir, mImage);
      cout << "Conversion to: " << oref << endl;
      cout << "Converted to:  " << um.phaseCenter() << endl;
      cout << "               " << um.phaseCenter().getAngle("deg") << endl;
      cout << "UVW rotation matrix: " << um.rotationUVW() << endl;
      mIm2 = um.phaseCenter();	// save for later
      MVPosition uvw(UVW);	// save UVW
      uvw *= um.rotationUVW();
      cout << "New UVW:        " << uvw << endl;
      UVW1 = uvw;	// save
      cout << "Phase rotation: " << uvw * um.rotationPhase() << endl;
      uvw = UVW;
      Double ph;
      um.convertUVW(ph, uvw);
      cout << "Phase: "  << ph << ", UVW: " << uvw << endl;
    }

     { 
      cout << "-------- J2000+offset -> B1950 ---------" << endl;
      // A new output reference
      cout << "Input coordinates: " << mIm2 << endl;
      cout << "                 : " << mIm2.getAngle("deg") << endl;
      MDirection::Ref oref(MDirection::B1950);
      MDirection odir(mImage.getValue(), oref);
      // A UVW machine
      UVWMachine um(odir, mIm2);
      cout << "Conversion to: " << oref << endl;
      cout << "Converted to:  " << um.phaseCenter() << endl;
      cout << "               " << um.phaseCenter().getAngle("deg") << endl;
      cout << "UVW rotation matrix: " << um.rotationUVW() << endl;
      MVPosition uvw(UVW1);	// save UVW
      uvw *= um.rotationUVW();
      cout << "New UVW:        " << uvw << endl;
      cout << "Phase rotation: " << uvw * um.rotationPhase() << endl;
      uvw = UVW1;
      Double ph;
      um.convertUVW(ph, uvw);
      cout << "Phase: "  << ph << ", UVW: " << uvw << endl;
    }

  } catch (AipsError x) {
    cout << x.getMesg() << endl;
  } end_try;

  try {

    cout << "-------------- deproject -----------------" << endl;
    // Output direction 
    MDirection::Ref oref(MDirection::J2000);
    MDirection odir(MVDirection(Quantity(30, "deg"),
				Quantity(37, "deg")), oref);
    // Main direction
    MDirection indir(MVDirection(Quantity(26, "deg"),
				 Quantity(34, "deg")),
		     MDirection::Ref(MDirection::J2000));
    // A UVW machine without projection
    UVWMachine um(odir, indir, False, False);
    // A UVW machine with projection
    UVWMachine ump(odir, indir, False, True);
    cout << "Input coordinates:    " << indir << endl;
    cout << "                 :    " << indir.getAngle("deg") << endl;
    cout << "Output coordinates:   " << odir << endl;  
    cout << "                  :   " << odir.getAngle("deg") << endl;
    MVPosition uvw(10, 100, 200);
    cout << "Input uvw:            " << uvw << endl;
    cout << "UVW rotation:         " << um.rotationUVW() << endl;
    // Save UVW
    MVPosition u1(uvw);
    Double ph;
    um.convertUVW(ph, u1);
    cout << "Phase correction:     " << ph << endl;
    cout << "Corrected UVW:        " << u1 << endl;
    u1 = uvw;
    um.convertUVW(u1);
    cout << "Corrected UVW:        " << u1 << endl;
    // Save UVW
    u1 = uvw;
    ump.convertUVW(ph, u1);
    cout << "-------------- Projected: ------------" << endl;
    cout << "UVW rotation:         " << ump.rotationUVW() << endl;
    cout << "Phase correction:     " << ph << endl;
    cout << "Corrected UVW:        " << u1 << endl;
    u1 = uvw;
    ump.convertUVW(u1);
    cout << "Corrected UVW:        " << u1 << endl;
    u1 = uvw;
    cout << "Phase correction:     " << ump.getPhase(u1) << endl;
    cout << "Corrected UVW:        " << u1 << endl;
    Vector<Double> vd;
    vd = uvw.getValue();
    cout << "Phase correction:     " << ump.getPhase(vd) << endl;
    cout << "Corrected UVW:        " << MVPosition(vd) << endl;
    vd = uvw.getValue();
    ump.convertUVW(vd);
    cout << "Corrected UVW:        " << MVPosition(vd) << endl;
    u1 = uvw;
    cout << "Corrected UVW:        " << ump(u1) << endl;
    vd = uvw.getValue();
    cout << "Corrected UVW:        " << MVPosition(ump(vd)) << endl;

  } catch (AipsError x) {
    cout << x.getMesg() << endl;
  } end_try;

  exit(0);
}
