//# dM1950_2000.cc: This program demonstrates  B1950<->J2000 conversion
//# Copyright (C) 2007
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
//# $Id: dM1950_2000.cc,v 1.2 2007/08/30 22:56:31 wyoung Exp $

//# Includes
#include <casa/aips.h>
#include <casa/Exceptions/Error.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayIO.h>
#include <casa/Arrays/Vector.h>
#include <casa/BasicSL/Constants.h>
#include <casa/Quanta/MVAngle.h>
#include <casa/Quanta/MVPosition.h>
#include <measures/Measures.h>
#include <measures/Measures/MEpoch.h>
#include <measures/Measures/MCDirection.h>
#include <measures/Measures/MDirection.h>
#include <measures/Measures/MeasData.h>
#include <casa/iostream.h>

#include <casa/namespace.h>

namespace casa {
  void fk45(Double R1950, Double D1950, Double BEPOCH, Double &R2000,
	    Double &D2000, Double X2000[], Vector<Double> V2000[]);

  void conB1950(const MVDirection &b1950, const MVDirection &j2000,
		Double epo=2000.0,
		MeasFrame *frame=0);

  void fk45(Double R1950, Double D1950, Double BEPOCH, Double &R2000,
	    Double &D2000, Double X2000[3], Vector<Double> V2000[]) {
    Double W;
    //  Position and position+velocity vectors
    Vector<Double> R0(3),A1(3),V1(3),V2(6),V3(3);

    //  Radians per year to arcsec per century
    const Double PMF=100e0*60e0*60e0*360e0/C::_2pi;

    //  Functions
    //  Double sla_EPJ,sla_EPB2D,sla_DRANRM

    //  Vectors A and Adot, and matrix M (only half of which is needed here)
    Double A[3] = { -1.62557e-6,  -0.31919e-6, -0.13843e-6};
    Double AD[3] = { +1.245e-3,    -1.580e-3,   -0.659e-3};

    Double EM[3][6] = 
      { { +0.9999256782,   +0.0111820610, +0.0048579479,
	  -0.000551, +0.238514, -0.435623 },
	{ -0.0111820611,
	  +0.9999374784,
	  -0.0000271474,
	  -0.238565,
	  -0.002667,
	  +0.012254 },
	{ -0.0048579477,
	  -0.0000271765,
	  +0.9999881997,
	  +0.435739,
	  -0.008541,
	  +0.002117 }
      };

    // Spherical to Cartesian
  
    R0 = MVPosition(Quantity(1.0,"m"),R1950,D1950).getValue();
    V2000[0] = R0; //v

    //  Adjust vector A to give zero proper motion in FK5
    W=(BEPOCH-1950)/PMF;
    for (uInt I=0; I<3; ++I) A1[I]=A[I]+W*AD[I];
    V2000[1] = A1; //v

    //  Remove e-terms
    W=R0[1]*A1[1]+R0[2]*A1[2]+R0[0]*A1[0];
    for (uInt I=0; I<3; ++I) V1[I]=R0[I]-A1[I]+W*R0[I];
    V2000[2] = V1; //v

    //  Convert position vector to Fricke system
    for (uInt I=0; I<6; ++I) {
      W=0;
      for (uInt J=0; J<3; ++J) W=W+EM[J][I]*V1[J];
      V2[I]=W;
    };
    for (uInt i=0; i<3; ++i) V2000[3][i] = V2[i]; //v
    for (uInt i=0; i<3; ++i) V2000[4][i] = V2[i+3]; //v

    //  Allow for fictitious proper motion in FK4
    W=(BEPOCH-2000)/PMF; /// needs b->D
    for (uInt I=0; I<3; ++I) V3[I]=V2[I]+W*V2[I+3];
    V2000[5] = V3;

    //  Revert to spherical coordinates
    for (uInt i=0; i<3;++i) X2000[i] = V3[i];
    R2000 = MVPosition(V3).getLong(); 
    D2000 = MVPosition(V3).getLat(); 
  }

  void conB1950(const MVDirection &b1950, const MVDirection &j2000,
		Double epo,
		MeasFrame *frame) { 	
    // References
    MDirection::Ref j2000ref(MDirection::J2000);
    MDirection::Ref b1950ref(MDirection::B1950);
    if (frame) {
      MVEpoch time(((epo-2000.0)*MeasData::JDCEN/100.0+MeasData::MJD2000));
      MEpoch tim(time);
      frame->set(tim);
      b1950ref.set(*frame);
    };
    // Coordinates and conversion
    MDirection j2000m(j2000, j2000ref);
    MDirection b1950m(b1950, b1950ref);
    MDirection::Convert toj2000(b1950m, j2000ref);
    MDirection::Convert fromj2000(j2000m, b1950ref);
    MDirection jcoord(toj2000());
    MDirection bcoord(fromj2000());
    // Act
    cout << "----------------------------------------------------" << endl;
    if (frame) cout << "Conversion for epoch " << epo << endl;
    else cout << "Conversion for default epoch 2000.0" << endl;
    cout << "B1950:       " << MVAngle::Format(MVAngle::TIME, 12u) <<
      MVAngle(b1950.getLong()) << "  " <<
      MVAngle::Format(MVAngle::ANGLE, 11u) << MVAngle(b1950.getLat()) << endl;
    cout << "J2000:       " << MVAngle::Format(MVAngle::TIME, 12u) <<
      MVAngle(j2000.getLong()) << "  " <<
      MVAngle::Format(MVAngle::ANGLE, 11u) << MVAngle(j2000.getLat()) << endl;
    cout << "----------------------------------------------------" << endl;
    cout.precision(12);
    cout << "J2000in:     " << j2000m.getAngle("deg") << endl;
    cout << "             " << j2000m.getValue().getValue() << endl;
    cout << "B1950in:     " << b1950m.getAngle("deg") << endl;
    cout << "             " << b1950m.getValue().getValue() << endl;
    cout << "to J2000:    " << toj2000().getAngle("deg") << endl;
    cout << "             " << toj2000().getValue().getValue() << endl;
    cout << "J2000out-in: " << toj2000().getAngle("deg")
      -j2000m.getAngle("deg") << endl;
    cout << "             " << toj2000().getValue().getValue()
      -j2000m.getValue().getValue() << endl;
    MDirection newcoord = fromj2000(toj2000());
    cout << "back2 B1950: "<< newcoord.getAngle("deg") << endl;
    cout << "             "<< newcoord.getValue().getValue() << endl;
    cout << "B1950out-in: " << newcoord.getAngle("deg")
      -b1950m.getAngle("deg") << endl;
    cout << "             " << newcoord.getValue().getValue()
      -b1950m.getValue().getValue() << endl;
    // fk45
    Vector<Double> xyz(3);
    Double R2000, D2000, X2000[3];
    Vector<Double> V2000[20];
    for (uInt i=0; i<20; ++i) V2000[i].resize(3, 0.0);
    fk45(b1950.getLong(), b1950.getLat(), epo , R2000, D2000, X2000, V2000);
    for (uInt i=0; i<3; ++i) xyz[i] = X2000[i];
    cout << "fk45out:     " << MVDirection(R2000, D2000).getAngle("deg") <<
      endl;
    cout << "             " << 
      MVDirection(R2000, D2000).getValue() << endl;
    cout << "out-J2000in: " <<  MVDirection(R2000, D2000).getAngle("deg")
      - j2000m.getAngle("deg") << endl;
    cout << "             " <<  MVDirection(R2000, D2000).getValue()
      - j2000m.getValue().getValue() << endl;
    cout << "out-J2000out:" <<  MVDirection(R2000, D2000).getValue()
      - toj2000().getValue().getValue() << endl;
    cout << "             " <<  MVDirection(R2000, D2000).getAngle("deg")
      - toj2000().getAngle("deg") << endl;

    cout << "----------------------------------------------------" << endl;
  }

} //# NAMESPACE CASA - END

int main()
{
  try {
    cout << "Demonstrate B1950<-> J2000" << endl;
    cout << "----------------------------------------------------" << endl;

    String epoch;
    //	while (epoch != "B1950" && epoch != "J2000") {
    //  cout << "Specify the base epoch (B1950, J2000) [B1950]: ";
    // The following and other flush() are necessary for cfront (although
    // theoretically cin should auto flush)
    //  cout.flush();
    //  if (cin.peek() == '\n') {
    //cin.get();
    //epoch = "";
    //  } else {
    //cin >> epoch;
    //  };
    //  epoch.capitalize();
    if (epoch.empty()) epoch = "B1950";
    //};
    Quantity ra;
    Quantity dec;
    MeasFrame frame;

    Quantity::read(ra, "13h28m49.657756");
    Quantity::read(dec,"30d45m58.64060");
    MVDirection b1950(ra, dec);
    Quantity::read(ra, "13h31m08.288048");
    Quantity::read(dec,"30d30m32.95924");
    MVDirection j2000(ra, dec);
    conB1950(b1950, j2000);
    conB1950(b1950, j2000, 2000.0, &frame);
    conB1950(b1950, j2000, 1979.9, &frame);

    Quantity::read(ra, "03h16m29.567289");
    Quantity::read(dec,"41d19m51.91677");
    b1950 = MVDirection(ra, dec);
    Quantity::read(ra, "03h19m48.160119");
    Quantity::read(dec,"41d30m42.10389");
    j2000 = MVDirection(ra, dec);
    conB1950(b1950, j2000);
    conB1950(b1950, j2000, 1979.9, &frame);
    conB1950(b1950, j2000, 2000.0, &frame);

    Quantity::read(ra, "13h28m49.657700");
    Quantity::read(dec,"30d45m58.640000");
    b1950 = MVDirection(ra, dec);
    Quantity::read(ra, "13h31m08.287984");
    Quantity::read(dec,"30d30m32.958850");
    j2000 = MVDirection(ra, dec);
    conB1950(b1950, j2000);
    conB1950(b1950, j2000, 1979.9, &frame);
    conB1950(b1950, j2000, 2000.0, &frame);

    Quantity::read(ra, "13h28m49.659");
    Quantity::read(dec,"30d45m58.660");
    b1950 = MVDirection(ra, dec);
    Quantity::read(ra, "13h31m08.288");
    Quantity::read(dec,"30d30m32.959");
    j2000 = MVDirection(ra, dec);
    conB1950(b1950, j2000);
    conB1950(b1950, j2000, 1979.9, &frame);
    conB1950(b1950, j2000, 2000.0, &frame);

  } catch (AipsError x) {
    cout << x.getMesg() << endl;
  } 
    
  return(0);
}
