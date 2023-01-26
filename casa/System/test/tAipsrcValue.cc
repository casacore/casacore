//# tAipsrcValue.cc: This program tests the Aipsrc value interface
//# Copyright (C) 1996,1997,1998,1999,2002
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

#include <casacore/casa/aips.h>
#include <casacore/casa/System/AipsrcValue.h>
#include <casacore/casa/System/AipsrcVector.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/IO/ArrayIO.h>
#include <casacore/casa/Quanta/Unit.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main(){
  String aipsrcKeyword("my.double.test");
  String aipsrcKeyword1("mine.double.test");
  String aipsrcKeyword2("mine.bool.test");
  String aipsrcValue;
  double aVal;
  bool bVal;
  Vector<double> vVal;

  Aipsrc::find(aipsrcValue, aipsrcKeyword);
  cout << aipsrcKeyword << " " << aipsrcValue << endl;

  Aipsrc::find(aipsrcValue, aipsrcKeyword1);
  cout << aipsrcKeyword1 << " " << aipsrcValue << endl;

  AipsrcValue<double>::find(aVal, aipsrcKeyword, 10.5);
  cout << aipsrcKeyword << " (D): " << aVal << endl;

  AipsrcValue<double>::find(aVal, aipsrcKeyword1, 22.9);
  cout << aipsrcKeyword1 << " (D): " << aVal << endl;

  AipsrcValue<bool>::find(bVal, aipsrcKeyword2, true);
  cout << aipsrcKeyword2 << " (B): " << bVal << endl;

  AipsrcVector<double>::find(vVal, aipsrcKeyword1);
  cout << aipsrcKeyword1 << " (V): " << vVal << endl;

  AipsrcValue<double>::find(aVal, aipsrcKeyword1, "m/s", "km/s");
  cout << aipsrcKeyword1 << " (Q): " << aVal << endl;

  cout << "AIPSROOT: " << Aipsrc::aipsRoot() << endl;
  cout << "AIPSARCH: " << Aipsrc::aipsArch() << endl;
  cout << "AIPSSITE: " << Aipsrc::aipsSite() << endl;
  cout << "AIPSHOST: " << Aipsrc::aipsHost() << endl;
  cout << "AIPSHOME: " << Aipsrc::aipsHome() << endl;

  {
    uint32_t n = AipsrcValue<double>::registerRC(aipsrcKeyword, 100.05);
    uint32_t n1= AipsrcValue<double>::registerRC(aipsrcKeyword1, 220.09);
    uint32_t n2= AipsrcValue<bool>::registerRC(aipsrcKeyword2, false);
    cout << "Registrations: " << n << ", " << n1 << ", " << n2 << endl;
    double aVal1;
    aVal = AipsrcValue<double>::get(n);
    aVal1 = AipsrcValue<double>::get(n1);
    bVal = AipsrcValue<bool>::get(n2);
    cout << "Values: " << aVal << ", " << aVal1 << ", " << bVal << endl;
    n = AipsrcValue<double>::registerRC(aipsrcKeyword, 2345);
    AipsrcValue<double>::set(n1, 9876);
    cout << "Registrations: " << n << ", " << n1 << endl;
    aVal = AipsrcValue<double>::get(n);
    aVal1 = AipsrcValue<double>::get(n1);
    cout << "Values: " << aVal << ", " << aVal1 << endl;
    AipsrcValue<double>::save(n);
    AipsrcValue<double>::save(n1);
    AipsrcValue<bool>::save(n2);
  }

  return 0; 
}


