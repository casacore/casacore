//# tQuantumHolder.cc: This program tests QuantumHolder
//# Copyright (C) 1998
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

#include <aips/aips.h>
#include <aips/Exceptions.h>
#include <aips/Measures/Quantum.h>
#include <trial/Measures/QuantumHolder.h>
#include <aips/Containers/Record.h>
#include <aips/Glish/GlishRecord.h>

main() {
  try {
    cout << "Test QuantumHolder  " << endl;
    cout << "----------------------------------------------------" << endl;

    String error;
    QuantumHolder q00;
    Quantity x00(12.5, "km/s");
    Quantum<Float> x01(30.3, "Jy/a");
    Quantum<Int> x02(2, "pc3/d");
    Quantity r00;
    Record y00;
    GlishRecord z00;
    
    cout << "Input quantity:              " << x00 << endl;
    if (QuantumHolder(x00).toRecord(error, y00)) {
      if (q00.fromRecord(error, y00)) {
	cout <<"Record output quantity:      " << q00() << endl;
      } else {
	cout << "From error: " << error << endl;
      };
    } else {
      cout << "To error: " << error << endl;
    };
    if (QuantumHolder(x00).toRecord(error, z00)) {
      if (q00.fromRecord(error, z00)) {
	cout <<"Glish output quantity:       " << q00() << endl;
      } else {
	cout << "From error: " << error << endl;
      };
    } else {
      cout << "To error: " << error << endl;
    };

    cout << "Is quantity:                 " << q00.isQuantity() << endl;
    cout << "Is empty:                    " << q00.isEmpty() << endl;
    cout << "As quantity:                 " << q00.asQuantity() << endl;

    cout << "Input quantity:              " << x01 << endl;
    if (QuantumHolder(x01).toRecord(error, y00)) {
      if (q00.fromRecord(error, y00)) {
	cout <<"Record output quantity:      " << q00() << endl;
      } else {
	cout << "From error: " << error << endl;
      };
    } else {
      cout << "To error: " << error << endl;
    };
    cout << "Input quantity:              " << x02 << endl;
    if (QuantumHolder(x02).toRecord(error, y00)) {
      if (q00.fromRecord(error, y00)) {
	cout <<"Record output quantity:      " << q00() << endl;
      } else {
	cout << "From error: " << error << endl;
      };
    } else {
      cout << "To error: " << error << endl;
    };
    cout << "Error expected:" << endl;
    cout << "Input quantity:              " << x00 << endl;
    if (QuantumHolder(x00).toRecord(error, y00)) {
      y00.renameField("units", RecordFieldId("unit"));
      if (q00.fromRecord(error, y00)) {
	cout <<"Record output quantity:      " << q00() << endl;
      } else {
	cout << "From error: " << error << endl;
      };
    } else {
      cout << "To error: " << error << endl;
    };
  } catch (AipsError x) {
    cout << x.getMesg() << endl;
  } end_try;

  exit(0);
}
