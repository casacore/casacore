//# MeasData.cc: MeasData provides Measure computing data
//# Copyright (C) 1995,1996,1997,1998,2003
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

//# Includes
#include <casacore/casa/Quanta/RotMatrix.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/measures/Measures/MeasData.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Constants
const Double MeasData::MJD2000 =   51544.5;
const Double MeasData::MJDB1950 =  33281.92345905;
const Double MeasData::MJDB1900 =  15019.5;
const Double MeasData::MJDB1850 = -3242.29642;
const Double MeasData::TROPCEN =   36524.2198782;
const Double MeasData::JDCEN =     36525.0;
const Double MeasData::SECinDAY =  (3600.*24.);

//# Member functions

// Galactic coordinates
const RotMatrix &MeasData::GALtoB1950() {
  static Bool needInit = True;
  static RotMatrix rot;
  static const Double data[3][3] = {
    { -0.0669887394,	+0.4927284661,	-0.8676008112},
    { -0.8727557659,	-0.4503469580,	-0.1883746017},
    { -0.4835389146,	+0.7445846333,	+0.4601997848}
  };
  if (needInit) {
    // Multiple threads could execute this, but that is harmless.
    Int i,j;
    for (i=0; i<3; i++) {
      for (j=0; j<3; j++) {
	rot(i,j) = data[i][j];
      }
    }
    needInit = False;
  }
  return rot;
}

const RotMatrix &MeasData::B1950toGAL() {
  static Bool needInit = True;
  static RotMatrix rot;
  static const Double data[3][3] = {
    { -0.0669887394,	+0.4927284661,	-0.8676008112},
    { -0.8727557659,	-0.4503469580,	-0.1883746017},
    { -0.4835389146,	+0.7445846333,	+0.4601997848}
  };
  if (needInit) {
    // Multiple threads could execute this, but that is harmless.
    Int i,j;
    for (i=0; i<3; i++) {
      for (j=0; j<3; j++) {
	rot(i,j) = data[j][i];
      }
    }
    needInit = False;
  }
  return rot;
}

const RotMatrix &MeasData::GALtoJ2000() {
  static Bool needInit = True;
  static RotMatrix rot;
  static const Double data[3][3] = {
    ///    { -0.0548755397,	+0.4941094533,	-0.8676661359},
    ///    { -0.8734371080,	-0.4448295894,	-0.1980763861},
    ///    { -0.483834985,	+0.7469822518,	+0.4559837957}
    { -0.0548777621, +0.4941083214, -0.8676666398},
    { -0.8734369591, -0.4448308610, -0.1980741871},
    { -0.4838350026, +0.7469822433, +0.4559837919}
  };
  if (needInit) {
    // Multiple threads could execute this, but that is harmless.
    Int i,j;
    for (i=0; i<3; i++) {
      for (j=0; j<3; j++) {
	rot(i,j) = data[i][j];
      }
    }
    needInit = False;
  }
  return rot;
}

const RotMatrix &MeasData::J2000toGAL() {
  static Bool needInit = True;
  static RotMatrix rot;
  static const Double data[3][3] = {
    ///    { -0.0548755397,	+0.4941094533,	-0.8676661359},
    ///    { -0.8734371080,	-0.4448295894,	-0.1980763861},
    ///    { -0.483834985,	+0.7469822518,	+0.4559837957}
    { -0.0548777621, +0.4941083214, -0.8676666398},
    { -0.8734369591, -0.4448308610, -0.1980741871},
    { -0.4838350026, +0.7469822433, +0.4559837919}
  };
  if (needInit) {
    // Multiple threads could execute this, but that is harmless.
    Int i,j;
    for (i=0; i<3; i++) {
      for (j=0; j<3; j++) {
	rot(i,j) = data[j][i];
      }
    }
    needInit = False;
  }
  return rot;
}

// B1950-J2000 conversions
const RotMatrix &MeasData::MToB1950(uInt which) {
  static Bool needInit = True;
  static RotMatrix rot[5];
  static const Double data[5][3][3] = {
    {
      {+0.9999256795,	+0.0111814828,	+0.0048590039},
      {-0.0111814828,	+0.9999374849,	-0.0000271771},
      {-0.0048590040,	-0.0000271557,	+0.9999881946}},
    {
      {-0.00000242389840,	-0.00000002710544,	-0.00000001177742},
      {+0.00000002710544,	-0.00000242392702,	+0.00000000006585},
      {+0.00000001177742,	+0.00000000006585,	-0.00000242404995}},
    {
      {-0.000551,	+0.238509,	-0.435614},
      {-0.238560,	-0.002667,	+0.012254},
      {+0.435730,	-0.008541,	+0.002117}},
    {
      {+0.99990432,	+0.01118145,	+0.00485852},
      {-0.01118145,	+0.99991613,	-0.00002717},
      {-0.00485852,	-0.00002716,	+0.99996684}},
    {
      {+0.9999256781,	+0.0111820610,	+0.0048579479},
      {-0.0111820610,	+0.9999374785,	-0.0000271765},
      {-0.0048579477,	-0.0000271765,	+0.9999881998}}
  };
  if (needInit) {
    // Multiple threads could execute this, but that is harmless.
    Int i,j,k;
    for (i=0; i<5; i++) {
      for (j=0; j<3; j++) {
	for (k=0; k<3; k++) {
	  rot[i](j,k) = data[i][k][j];
	}
      }
    }
    needInit = False;
  }
  DebugAssert(which < 5, AipsError);
  return rot[which];
}

const RotMatrix &MeasData::MToJ2000(uInt which) {
  static Bool needInit = True;
  static RotMatrix rot[4];
  static const Double data[4][3][3] = {
    {
      {+0.9999256782,	-0.0111820611,	-0.0048579477},
      {+0.0111820610,	+0.9999374784,	-0.0000271765},
      {+0.0048579479,	-0.0000271474,	+0.9999881997}},
    {
      {+0.00000242395018,	-0.00000002710663,	-0.00000001177656},
      {+0.00000002710663,	+0.00000242397878,	-0.00000000006587},
      {+0.00000001177656,	-0.00000000006582,	+0.00000242410173}},
    {
      {-0.000551,	-0.238565,	+0.435739},
      {+0.238514,	-0.002667,	-0.008541},
      {-0.435623,	+0.012254,	+0.002117}},
    {
      {+0.99994704,	-0.01118251,	-0.00485767},
      {+0.01118251,	+0.99995883,	-0.00002718},
      {+0.00485767,	-0.00002714,	+1.00000956}}
  };
  if (needInit) {
    // Multiple threads could execute this, but that is harmless.
    Int i,j,k;
    for (i=0; i<4; i++) {
      for (j=0; j<3; j++) {
	for (k=0; k<3; k++) {
	  rot[i](j,k) = data[i][k][j];
	}
      }
    }
    needInit = False;
  }
  
  DebugAssert(which < 4, AipsError);
  return rot[which];
}

// Solar semi diameter
Double MeasData::SunSemiDiameter() {
    static const Double data = .004652472638;
    return data;
}

// J2000 obliquity
Double MeasData::eps0J2000() {
  static const Double data = 84381.448*C::arcsec;
  return data;
}

} //# NAMESPACE CASACORE - END

