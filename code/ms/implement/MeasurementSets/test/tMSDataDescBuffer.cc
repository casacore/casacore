//# tMSDataDescBuffer.cc:
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

#include <trial/MeasurementSets/MSDataDescBuffer.h>

int main() {
  try {
    // Check the default constructor
    MSDataDescriptionBuffer newBuffer;
    // test the ok function.
    AlwaysAssert(newBuffer.ok(), AipsError);
    // test the addRow & nrow functions.
    AlwaysAssert(newBuffer.nrow() == 0, AipsError);
    newBuffer.addRow(20);
    AlwaysAssert(newBuffer.ok(), AipsError);
    AlwaysAssert(newBuffer.nrow() == 20, AipsError);
    {
      MSDataDescriptionBuffer fieldBuffer(1);
      { // test the addRow & nrow functions.
 	AlwaysAssert(fieldBuffer.nrow() == 1, AipsError);
 	fieldBuffer.addRow(4);
 	AlwaysAssert(fieldBuffer.nrow() == 5, AipsError);
      }
      { // test the spectralWindowId functions.
 	AlwaysAssert(fieldBuffer.spectralWindowId(0) == -1, AipsError);
 	AlwaysAssert(fieldBuffer.spectralWindowId(4) == -1, AipsError);
 	fieldBuffer.spectralWindowId(0, 0);
 	fieldBuffer.spectralWindowId(4, 1);
      }
      { // test the polarizationId functions.
 	AlwaysAssert(fieldBuffer.polarizationId(0) == -1, AipsError);
 	AlwaysAssert(fieldBuffer.polarizationId(4) == -1, AipsError);
 	fieldBuffer.polarizationId(0, 1);
 	fieldBuffer.polarizationId(4, 3);
      }
      { // test the flagRow functions.
 	AlwaysAssert(fieldBuffer.flagRow(0) == False, AipsError);
 	AlwaysAssert(fieldBuffer.flagRow(4) == False, AipsError);
 	fieldBuffer.flagRow(4, True);
      }
      { // Check the assignment operator & copy constructor
 	MSDataDescriptionBuffer otherBuffer(fieldBuffer);
 	AlwaysAssert(otherBuffer.ok(), AipsError);
 	AlwaysAssert(otherBuffer.nrow() == 5, AipsError);
 	newBuffer = otherBuffer;
 	AlwaysAssert(newBuffer.ok(), AipsError);
      }
    }
    { // check the data has not been lost.
      AlwaysAssert(newBuffer.nrow() == 5, AipsError);
      AlwaysAssert(newBuffer.spectralWindowId(0) ==  0, AipsError);
      AlwaysAssert(newBuffer.spectralWindowId(4) ==  1, AipsError);
      AlwaysAssert(newBuffer.polarizationId(0) ==  1, AipsError);
      AlwaysAssert(newBuffer.polarizationId(4) ==  3, AipsError);
      AlwaysAssert(newBuffer.flagRow(0) == False, AipsError);
      AlwaysAssert(newBuffer.flagRow(4) == True, AipsError);
    }
    { // Check the isValid functions
      AlwaysAssert(newBuffer.isValid(True) == False, AipsError);
      AlwaysAssert(newBuffer.isValid(3u) == False, AipsError);
      AlwaysAssert(newBuffer.isValid(4u) == True, AipsError);
      AlwaysAssert(newBuffer.isValid() == False, AipsError);
    }
    { // Check the match functions
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
// compile-command: "gmake OPTLIB=1 XLIBLIST=0 tMSDataDescBuffer"
// End: 
