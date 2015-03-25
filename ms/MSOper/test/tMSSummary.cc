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
#include <casacore/casa/aips.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/ms/MeasurementSets/MeasurementSet.h>
#include <casacore/ms/MSOper/MSSummary.h>

int main(int argc, const char* argv[])
{
  using namespace std;
  using namespace casacore;

  if (argc != 2) {
  	cout << "Usage: "<< argv[0] << " MS_filename" << endl;
	return 0;
  }

  try {
    cout << "MSSummary" << endl;
    cout << "--------------------------------------" << endl;

    LogIO os(LogOrigin("tMSSummary", "main()"));

    String MSName(argv[1]);
    MeasurementSet ms(MSName, Table::Old);
    MSSummary mss(ms);
    //mss.list(os, verbose);
    mss.listHistory(os);
  } catch (AipsError x) {
    cout << x.getMesg() << endl;
  } 
  
  return 0;
}

