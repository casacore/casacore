//# tMeasIERS.cc: This program tests the MeasIERS functions
//# Copyright (C) 2014
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
//#        Internet email: casa-feedback@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/measures/Measures/MeasIERS.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>
#include <casacore/casa/iomanip.h>

#include <casacore/casa/namespace.h>

void getX (double date)
{
  double result;
  bool sts = MeasIERS::get(result, MeasIERS::MEASURED, MeasIERS::X, date);
  cout <<"MEASURED  "<<date<< ' '<< "X: " << result << ' '<<sts<<endl;
  sts = MeasIERS::get(result, MeasIERS::PREDICTED, MeasIERS::X, date);
  cout <<"PREDICTED "<<date<< ' '<< "X: " << result << ' '<<sts<<endl;
}

int main()
{
  try {

    cout << "Test measure class MeasIERS" << endl;
    cout << "---------------------------" << endl;

    // Get a value for various epochs.

    cout << setprecision(9);
    Vector<Double> val(6);

    double date= 51116;
    for (int i=0; i<3; ++i) {
      getX (date);
      date += 0.5;
    }
    getX (37660);
    getX (37665);
    getX (55000);
    getX (55809);
    getX (600000);


    // Test for handling of leap seconds (CAS-7984)

    Double startMJD = 57202; // 2015-06-29T00:00:00
    Double oneHour = 1.0/24.;
    for (Int i = 0; i < 72; i++) {
      const Double mjd = startMJD + i * oneHour;
      const MVTime now(mjd);
      Double dUT;
      Bool rval = MeasIERS::get(dUT, MeasIERS::PREDICTED, MeasIERS::dUT1, mjd);
      if(!rval){
	cout << "MeasIERS::get returned False for PREDICTED, dUT1, mjd " << mjd << endl;
	return 2;
      }
      cout << now.string(casacore::MVTime::YMD) 
	   << " " << setprecision(3) << fixed << mjd
	   << " " << dUT << endl;
    } 
    

  } catch (const std::exception& x) {
    cout << x.what() << endl;
    return 1;
  } 
  
  return 0;
}
