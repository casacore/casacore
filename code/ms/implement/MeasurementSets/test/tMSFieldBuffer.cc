//# tMSFieldBuffer.cc:
//# Copyright (C) 2000
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

#include <aips/aips.h>
#include <aips/Exceptions/Error.h>
#include <aips/Exceptions/Excp.h>
#include <aips/Utilities/Assert.h>
#include <aips/Utilities/String.h>
#include <iostream.h>

#include <trial/MeasurementSets/MSFieldBuffer.h>
#include <aips/Measures/MEpoch.h>
#include <aips/Measures/MDirection.h>
#include <aips/Arrays/ArrayLogical.h>
//#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/IPosition.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Mathematics/Math.h>
#include <aips/Mathematics/Constants.h>

int main() {
  try {
    // Check the default constructor
    MSFieldBuffer newBuffer;
    AlwaysAssert(newBuffer.ok(), AipsError);
    AlwaysAssert(newBuffer.nrow() == 0, AipsError);
    newBuffer.addRow(2);
    AlwaysAssert(newBuffer.ok(), AipsError);
    AlwaysAssert(newBuffer.nrow() == 2, AipsError);
    {
      MSFieldBuffer fieldBuffer(1);
      { // test the addRow & nrow functions.
	AlwaysAssert(fieldBuffer.nrow() == 1, AipsError);
	fieldBuffer.addRow(4);
	AlwaysAssert(fieldBuffer.nrow() == 5, AipsError);
      }
      { // test the name functions.
	AlwaysAssert(fieldBuffer.name(0) == String(""), AipsError);
	AlwaysAssert(fieldBuffer.name(4) == String(""), AipsError);
	fieldBuffer.name(0, "row 0");
	fieldBuffer.name(4, "row 4");
      }
      { // test the code functions.
	AlwaysAssert(fieldBuffer.code(0) == String(""), AipsError);
	AlwaysAssert(fieldBuffer.code(4) == String(""), AipsError);
	fieldBuffer.code(0, "code 0");
	AlwaysAssert(fieldBuffer.code(0) == String("code 0"), AipsError);
	fieldBuffer.code(4, "code 4");
	AlwaysAssert(fieldBuffer.code(0) == String("code 0"), AipsError);
      }
      { // test the numPoly functions.
	AlwaysAssert(fieldBuffer.numPoly(0) == 0, AipsError);
	AlwaysAssert(fieldBuffer.numPoly(4) == 0, AipsError);
	fieldBuffer.numPoly(0, 1);
	fieldBuffer.numPoly(4, 2);
      }
      { // test the time and timeFrame functions.
	AlwaysAssert(fieldBuffer.time(0) == 0, AipsError);
	AlwaysAssert(fieldBuffer.time(4) == 0, AipsError);
	AlwaysAssert(fieldBuffer.timeFrame() == MEpoch::UTC, AipsError);
	fieldBuffer.time(0, 1.0);
	fieldBuffer.time(4, 2.0);
	fieldBuffer.timeFrame(MEpoch::TAI);
      }
      { // test the delayDir & delayFrame functions.
	AlwaysAssert(fieldBuffer.delayDir(0).shape() == IPosition(2, 2, 2),
		     AipsError);
	AlwaysAssert(allNear(fieldBuffer.delayDir(0), 0.0, C::dbl_epsilon),
		     AipsError);
	AlwaysAssert(fieldBuffer.delayDir(1).shape() == IPosition(2, 2, 1),
		     AipsError);
	AlwaysAssert(allNear(fieldBuffer.delayDir(1), 0.0, C::dbl_epsilon),
		     AipsError);
	AlwaysAssert(fieldBuffer.delayDir(4).shape() == IPosition(2, 2, 3),
		     AipsError);
	AlwaysAssert(allNear(fieldBuffer.delayDir(4), 0.0, C::dbl_epsilon),
		     AipsError);
	AlwaysAssert(fieldBuffer.delayFrame() == MDirection::J2000, AipsError);
	fieldBuffer.delayDir(0, Matrix<Double>(2, 2, 2.0));
	fieldBuffer.delayDir(4, Matrix<Double>(2, 3, 3.0));
	fieldBuffer.delayFrame(MDirection::B1950);
	try { 
	  fieldBuffer.delayDir(4, Matrix<Double>(2, 2, 3.0));
	  throw(AipsError("Exception not thrown"));
	}
	catch (AipsError x) {
	  AlwaysAssert(x.getMesg().contains("direction.ncolumn()"), AipsError);
	}
      }
      { // test the phaseDir & phaseFrame functions.
	AlwaysAssert(fieldBuffer.phaseDir(0).shape() == IPosition(2, 2, 2),
		     AipsError);
	AlwaysAssert(allNear(fieldBuffer.phaseDir(0), 0.0, C::dbl_epsilon),
		     AipsError);
	AlwaysAssert(fieldBuffer.phaseDir(1).shape() == IPosition(2, 2, 1),
		     AipsError);
	AlwaysAssert(allNear(fieldBuffer.phaseDir(1), 0.0, C::dbl_epsilon),
		     AipsError);
	AlwaysAssert(fieldBuffer.phaseDir(4).shape() == IPosition(2, 2, 3),
		     AipsError);
	AlwaysAssert(allNear(fieldBuffer.phaseDir(4), 0.0, C::dbl_epsilon),
		     AipsError);
	AlwaysAssert(fieldBuffer.phaseFrame() == MDirection::J2000, AipsError);
	fieldBuffer.phaseDir(0, Matrix<Double>(2, 2, 20.0));
	fieldBuffer.phaseDir(4, Matrix<Double>(2, 3, 30.0));
	fieldBuffer.phaseFrame(MDirection::AZEL);
	try { 
	  fieldBuffer.phaseDir(4, Matrix<Double>(2, 2, 3.0));
	  throw(AipsError("Exception not thrown"));
	}
	catch (AipsError x) {
	  AlwaysAssert(x.getMesg().contains("direction.ncolumn()"), AipsError);
	}
      }
      { // test the referenceDir & referenceFrame functions.
	AlwaysAssert(fieldBuffer.referenceDir(0).shape() == IPosition(2, 2, 2),
		     AipsError);
	AlwaysAssert(allNear(fieldBuffer.referenceDir(0), 0.0, C::dbl_epsilon),
		     AipsError);
	AlwaysAssert(fieldBuffer.referenceDir(1).shape() == IPosition(2, 2, 1),
		     AipsError);
	AlwaysAssert(allNear(fieldBuffer.referenceDir(1), 0.0, C::dbl_epsilon),
		     AipsError);
	AlwaysAssert(fieldBuffer.referenceDir(4).shape() == IPosition(2, 2, 3),
		     AipsError);
	AlwaysAssert(allNear(fieldBuffer.referenceDir(4), 0.0, C::dbl_epsilon),
		     AipsError);
	AlwaysAssert(fieldBuffer.referenceFrame() == MDirection::J2000,
		     AipsError);
	fieldBuffer.referenceDir(0, Matrix<Double>(2, 2, 10.0));
	fieldBuffer.referenceDir(4, Matrix<Double>(2, 3, 15.0));
	fieldBuffer.referenceFrame(MDirection::GALACTIC);
	try { 
	  fieldBuffer.referenceDir(4, Matrix<Double>(2, 2, 3.0));
	  throw(AipsError("Exception not thrown"));
	}
	catch (AipsError x) {
	  AlwaysAssert(x.getMesg().contains("direction.ncolumn()"), AipsError);
	}
      }
      { // test the sourceID functions.
	AlwaysAssert(fieldBuffer.sourceId(0) == -1, AipsError);
	AlwaysAssert(fieldBuffer.sourceId(4) == -1, AipsError);
	fieldBuffer.sourceId(0, 10);
	fieldBuffer.sourceId(4, 20);
      }
      { // test the flagRow functions.
	AlwaysAssert(fieldBuffer.flagRow(0) == False, AipsError);
	AlwaysAssert(fieldBuffer.flagRow(4) == False, AipsError);
	fieldBuffer.flagRow(3, True);
      }
      { // Check the assignment operator & copy constructor
	MSFieldBuffer otherBuffer(fieldBuffer);
	AlwaysAssert(otherBuffer.ok(), AipsError);
	AlwaysAssert(otherBuffer.nrow() == 5, AipsError);
	newBuffer = otherBuffer;
	AlwaysAssert(newBuffer.ok(), AipsError);
      }
    }
    { // check the data has not been lost.
      AlwaysAssert(newBuffer.nrow() == 5, AipsError);
      AlwaysAssert(newBuffer.name(0) == String("row 0"), AipsError);
      AlwaysAssert(newBuffer.name(1) == String(""), AipsError);
      AlwaysAssert(newBuffer.name(4) == String("row 4"), AipsError);
      AlwaysAssert(newBuffer.code(0) == String("code 0"), AipsError);
      AlwaysAssert(newBuffer.code(1) == String(""), AipsError);
      AlwaysAssert(newBuffer.code(4) == String("code 4"), AipsError);
      AlwaysAssert(newBuffer.numPoly(0) == 1, AipsError);
      AlwaysAssert(newBuffer.numPoly(1) == 0, AipsError);
      AlwaysAssert(newBuffer.numPoly(4) == 2, AipsError);
      AlwaysAssert(near(newBuffer.time(0), 1.0), AipsError);
      AlwaysAssert(near(newBuffer.time(1), 0.0), AipsError);
      AlwaysAssert(near(newBuffer.time(4), 2.0), AipsError);
      AlwaysAssert(newBuffer.timeFrame() == MEpoch::TAI, AipsError);
      AlwaysAssert(newBuffer.delayDir(0).shape() == IPosition(2, 2, 2),
		   AipsError);
      AlwaysAssert(allNear(newBuffer.delayDir(0), 2.0, C::dbl_epsilon),
		   AipsError);
      AlwaysAssert(newBuffer.delayDir(1).shape() == IPosition(2, 2, 1),
		   AipsError);
      AlwaysAssert(allNear(newBuffer.delayDir(1), 0.0, C::dbl_epsilon),
		   AipsError);
      AlwaysAssert(newBuffer.delayDir(4).shape() == IPosition(2, 2, 3),
		   AipsError);
      AlwaysAssert(allNear(newBuffer.delayDir(4), 3.0, C::dbl_epsilon),
		   AipsError);
      AlwaysAssert(newBuffer.delayFrame() == MDirection::B1950, AipsError);
      AlwaysAssert(newBuffer.phaseDir(0).shape() == IPosition(2, 2, 2),
		   AipsError);
      AlwaysAssert(allNear(newBuffer.phaseDir(0), 20.0, C::dbl_epsilon),
		   AipsError);
      AlwaysAssert(newBuffer.phaseDir(1).shape() == IPosition(2, 2, 1),
		   AipsError);
      AlwaysAssert(allNear(newBuffer.phaseDir(1), 0.0, C::dbl_epsilon),
		   AipsError);
      AlwaysAssert(newBuffer.phaseDir(4).shape() == IPosition(2, 2, 3),
		   AipsError);
      AlwaysAssert(allNear(newBuffer.phaseDir(4), 30.0, C::dbl_epsilon),
		   AipsError);
      AlwaysAssert(newBuffer.phaseFrame() == MDirection::AZEL, AipsError);
      AlwaysAssert(newBuffer.referenceDir(0).shape() == IPosition(2, 2, 2),
		   AipsError);
      AlwaysAssert(allNear(newBuffer.referenceDir(0), 10.0, C::dbl_epsilon),
		   AipsError);
      AlwaysAssert(newBuffer.referenceDir(1).shape() == IPosition(2, 2, 1),
		   AipsError);
      AlwaysAssert(allNear(newBuffer.referenceDir(1), 0.0, C::dbl_epsilon),
		   AipsError);
      AlwaysAssert(newBuffer.referenceDir(4).shape() == IPosition(2, 2, 3),
		   AipsError);
      AlwaysAssert(allNear(newBuffer.referenceDir(4), 15.0, C::dbl_epsilon),
		   AipsError);
      AlwaysAssert(newBuffer.referenceFrame() == MDirection::GALACTIC,
		   AipsError);
      AlwaysAssert(newBuffer.sourceId(0) == 10, AipsError);
      AlwaysAssert(newBuffer.sourceId(1) == -1, AipsError);
      AlwaysAssert(newBuffer.sourceId(4) == 20, AipsError);
      AlwaysAssert(newBuffer.flagRow(0) == False, AipsError);
      AlwaysAssert(newBuffer.flagRow(3) == True, AipsError);
      AlwaysAssert(newBuffer.flagRow(4) == False, AipsError);
    }
    { // Check the isValid functions
      AlwaysAssert(newBuffer.isValid(True) == True, AipsError);
      AlwaysAssert(newBuffer.isValid(3u) == True, AipsError);
      AlwaysAssert(newBuffer.isValid() == True, AipsError);
    }
    { // Check the match functions
      AlwaysAssert(newBuffer.matchRefDir(Matrix<Double>(2, 2, 0.0)) == -1,
		   AipsError);
      AlwaysAssert(newBuffer.matchRefDir(Matrix<Double>(2, 2, 10.0), False)==0,
		   AipsError);
      AlwaysAssert(newBuffer.matchRefDir(Matrix<Double>(2, 1, 0.0)) == 2,
		   AipsError);
      AlwaysAssert(newBuffer.matchRefDir(Matrix<Double>(2, 1, 0.0), False)==3,
		   AipsError);
    }
  }
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  }
  cout << "OK" << endl;
}

// Local Variables: 
// compile-command: "gmake OPTLIB=1 XLIBLIST=0 tMSFieldBuffer"
// End: 
