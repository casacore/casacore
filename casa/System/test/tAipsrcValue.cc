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
//#
//# $Id$

#include <casacore/casa/aips.h>
#include <casacore/casa/System/AipsrcValue.h>
#include <casacore/casa/System/AipsrcVector.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/Arrays/ArrayIO.h>
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
  Double aVal;
  Bool bVal;
  Vector<Double> vVal;

  Aipsrc::find(aipsrcValue, aipsrcKeyword);
  cout << aipsrcKeyword << " " << aipsrcValue << endl;

  Aipsrc::find(aipsrcValue, aipsrcKeyword1);
  cout << aipsrcKeyword1 << " " << aipsrcValue << endl;

  AipsrcValue<Double>::find(aVal, aipsrcKeyword, 10.5);
  cout << aipsrcKeyword << " (D): " << aVal << endl;

  AipsrcValue<Double>::find(aVal, aipsrcKeyword1, 22.9);
  cout << aipsrcKeyword1 << " (D): " << aVal << endl;

  AipsrcValue<Bool>::find(bVal, aipsrcKeyword2, True);
  cout << aipsrcKeyword2 << " (B): " << bVal << endl;

  AipsrcVector<Double>::find(vVal, aipsrcKeyword1);
  cout << aipsrcKeyword1 << " (V): " << vVal << endl;

  AipsrcValue<Double>::find(aVal, aipsrcKeyword1, "m/s", "km/s");
  cout << aipsrcKeyword1 << " (Q): " << aVal << endl;

  cout << "AIPSROOT: " << Aipsrc::aipsRoot() << endl;
  cout << "AIPSARCH: " << Aipsrc::aipsArch() << endl;
  cout << "AIPSSITE: " << Aipsrc::aipsSite() << endl;
  cout << "AIPSHOST: " << Aipsrc::aipsHost() << endl;
  cout << "AIPSHOME: " << Aipsrc::aipsHome() << endl;

  {
    uInt n = AipsrcValue<Double>::registerRC(aipsrcKeyword, 100.05);
    uInt n1= AipsrcValue<Double>::registerRC(aipsrcKeyword1, 220.09);
    uInt n2= AipsrcValue<Bool>::registerRC(aipsrcKeyword2, False);
    cout << "Registrations: " << n << ", " << n1 << ", " << n2 << endl;
    Double aVal1;
    aVal = AipsrcValue<Double>::get(n);
    aVal1 = AipsrcValue<Double>::get(n1);
    bVal = AipsrcValue<Bool>::get(n2);
    cout << "Values: " << aVal << ", " << aVal1 << ", " << bVal << endl;
    n = AipsrcValue<Double>::registerRC(aipsrcKeyword, 2345);
    AipsrcValue<Double>::set(n1, 9876);
    cout << "Registrations: " << n << ", " << n1 << endl;
    aVal = AipsrcValue<Double>::get(n);
    aVal1 = AipsrcValue<Double>::get(n1);
    cout << "Values: " << aVal << ", " << aVal1 << endl;
    AipsrcValue<Double>::save(n);
    AipsrcValue<Double>::save(n1);
    AipsrcValue<Bool>::save(n2);
  }

  return 0; 
}


