//# tMSCalEnums.cc: Test program for the MSCalEnums class
//# Copyright (C) 2001,2002
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


#include <casa/aips.h>
#include <casa/iostream.h>
#include <casa/Containers/Block.h>
#include <casa/Arrays/Vector.h>
#include <casa/Utilities/Assert.h>
#include <msvis/MSVis/MSCalEnums.h>

#include <casa/namespace.h>
// <summary>
// Test program for class MSCalEnums.
// </summary>

Bool foundError = False;


void checkField (const Int& calEnum, const String& name, const DataType& type)
{
  if (MSC::fieldName(calEnum) != name || MSC::basicType(calEnum) != type) {
    cout << "Mismatch for field name: " << name << endl;
    foundError = True;
  };
  return;
};

void doTest()
{
  // Check the field names and types
  checkField (MSC::ANTENNA1, "ANTENNA1", TpInt);
  checkField (MSC::ANTENNA2, "ANTENNA2", TpInt);
  checkField (MSC::FEED1, "FEED1", TpInt);
  checkField (MSC::FEED2, "FEED2", TpInt);
  checkField (MSC::FREQ_GROUP_NAME, "FREQ_GROUP_NAME", TpString);
  checkField (MSC::GAIN, "GAIN", TpComplex);
  checkField (MSC::CAL_HISTORY_ID, "CAL_HISTORY_ID", TpInt);
  checkField (MSC::TOTAL_SOLUTION_OK, "TOTAL_SOLUTION_OK", TpBool);
  checkField (MSC::FIT, "FIT", TpFloat);
  checkField (MSC::NUM_SPW, "NUM_SPW", TpInt);
  checkField (MSC::CAL_DESC, "CAL_DESC", TpTable);
  checkField (MSC::POLY_TYPE, "POLY_TYPE", TpString);
  checkField (MSC::SIDEBAND_REF, "SIDEBAND_REF", TpComplex);

  // Check the vector field names method
  Vector<Int> colEnums(5);
  colEnums(0) = MSC::N_JONES;
  colEnums(1) = MSC::CAL_SELECT;
  colEnums(2) = MSC::ROT_MEASURE;
  colEnums(3) = MSC::PHASE_OFFSET;
  colEnums(4) = MSC::POLY_COEFF_AMP;
  Block<String> vecNames = MSC::fieldNames (colEnums);
  Bool vecError = (vecNames[0] != "N_JONES" || 
		   vecNames[1] != "CAL_SELECT" || 
		   vecNames[2] != "ROT_MEASURE" ||
		   vecNames[3] != "PHASE_OFFSET" || 
		   vecNames[4] != "POLY_COEFF_AMP");
  if (vecError) {
    foundError = (foundError || vecError);
    cout << "Error in method MSCalEnums::fieldNames()" << endl;
  };
}


int main()
{
  try {
    doTest();
  } catch (AipsError x) {
    cout << "Unexpected exception: " << x.getMesg() << endl;
    exit(1);
  } catch (...) {
    cout << "Unexpected unknown exception" << endl;
    exit(1);
  }
  if (foundError) {
    exit(1);
  }
  cout << "OK" << endl;
  exit(0);
}
