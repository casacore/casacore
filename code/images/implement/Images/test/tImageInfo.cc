 //# tImageInfo.cc: Miscellaneous information related to an image
//# Copyright (C) 1998,1999,2000
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

#include <trial/Images/ImageInfo.h>

#include <aips/Utilities/Assert.h>
#include <aips/Exceptions/Error.h>
#include <aips/Mathematics/Math.h>
#include <aips/Containers/Record.h>
#include <aips/Quanta/Quantum.h>
#include <aips/Quanta/QLogical.h>
#include <aips/Arrays/Vector.h>

void equalBeams(const Vector<Quantum<Double> >& b1, const Vector<Quantum<Double> >& b2)
{
    AlwaysAssertExit(b1.nelements()==b2.nelements());
    AlwaysAssertExit(b1.nelements()==0 || b1.nelements()==3);
    if (b1.nelements()==0) return;
//
    for (uInt i=0; i<b1.nelements(); i++) {
       AlwaysAssertExit(b1(i) == b2(i));
    }
}


int main()
{

try {


// Default constructor ;

    ImageInfo mii;

//
// Restoring beam
//
    equalBeams(mii.restoringBeam(), mii.defaultRestoringBeam());
    Vector<Quantum<Double> > beam;
    equalBeams(mii.defaultRestoringBeam(), beam);
//
    beam.resize(3);
    beam(0) = Quantum<Double>(45.0, "arcsec");
    beam(1) = Quantum<Double>(45.0, "arcsec");
    beam(2) = Quantum<Double>(-45.0, "deg");
    mii.setRestoringBeam(beam);
    equalBeams(mii.restoringBeam(), beam);
    mii.setRestoringBeam(beam(0), beam(1), beam(2));
    equalBeams(mii.restoringBeam(), beam);
    beam(0) = Quantum<Double>(1.0, "deg");
    mii.setRestoringBeam(beam);
    equalBeams(mii.restoringBeam(), beam);
//
    mii.removeRestoringBeam();
    AlwaysAssertExit(mii.restoringBeam().nelements()==0);
//
// Copy constructor and assignemnt
//
    mii.setRestoringBeam(beam(0), beam(1), beam(2));
    ImageInfo mii2(mii);
    equalBeams(mii2.restoringBeam(), beam);
    Vector<Quantum<Double> > beam2(3);
    beam2(0) = Quantum<Double>(50.0, "arcsec");
    beam2(1) = Quantum<Double>(0.0001, "rad");
    beam2(2) = Quantum<Double>(-90.0, "deg");
    mii2.setRestoringBeam(beam2);
    mii = mii2;
    equalBeams(mii.restoringBeam(), beam2);
//
// Record conversion
//
    Record rec;
    String error;
    AlwaysAssertExit(mii.toRecord(error, rec));
//
    ImageInfo mii3;
    Bool ok = mii3.fromRecord(error, rec);
    if (!ok) cout << "Error = " << error << endl;
    equalBeams(mii3.restoringBeam(), beam2);
//
// output stream
//
    cout << mii3 << endl;

} catch (AipsError x) {
  cout << "Caught error " << x.getMesg() << endl;
  return 1;
} 
  
    cout << "OK" << endl;
    return 0;
}
