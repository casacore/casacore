//# tMSSummary.cc: This program tests that VPSkyJones works
//# Copyright (C) 1998,1999,2000
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
#include <casa/aips.h>
#include <casa/Exceptions/Error.h>
#include <casa/Logging/LogIO.h>
#include <ms/MeasurementSets/MeasurementSet.h>
#include <ms/MeasurementSets/MSSummary.h>

int main()
{
  //using namespace std;
  using namespace casa;
  try {
    cout << "MSSummary" << endl;
    cout << "--------------------------------------" << endl;


    // Open the test MS.
    // !!! Make sure that MSName points to a local MeasurementSet!!!
    String MSName("/home/rrusk/testing/3C273XC1.ms");
    MeasurementSet ms(MSName, Table::Old);

    LogIO os(LogOrigin("tMSSummary", "main()"));

    MSSummary mss(ms);
    Bool verbose=True;
    //mss.list(os, verbose);
    mss.listHistory(os, verbose);
  } catch (AipsError x) {
    cout << x.getMesg() << endl;
  } 
  
  exit(0);
}

