//# tQuantumHolder.cc: This program tests QuantumHolder
//# Copyright (C) 1998,2000,2002,2003
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

//# Includes

#include <casacore/casa/aips.h>
#include <casacore/casa/Exceptions.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Quanta/QLogical.h>
#include <casacore/casa/Quanta/QuantumHolder.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/iostream.h>

#include <casacore/casa/namespace.h>
int main() {
  try {
    cout << "Test QuantumHolder  " << endl;
    cout << "----------------------------------------------------" << endl;

    String error;
    QuantumHolder q00, q01;
    Quantity x00(12.5, "km/s");
    Quantum<float> x01(30.3, "Jy/a");
    Quantum<int32_t> x02(2, "pc3/d");
    String s00("12:30:00");
    String s01("-97.8 Mpc/a");
    String s02("12.5JY");
    Record y00;
    
    cout << "Input quantity:              " << (QBase &)x00 << endl;
    if (QuantumHolder(x00).toRecord(error, y00)) {
      if (q00.fromRecord(error, y00)) {
	cout <<"Record output quantity:      " << q00.asQuantum() << endl;
      } else {
	cout << "From error: " << error << endl;
      }
    } else {
      cout << "To error: " << error << endl;
    }

    QuantumHolder q02(q00);
    if (q00.asQuantity() != q02.asQuantity()) {
      cout << "Error in copy constructor" << endl;
    }

    cout << "Is quantum:                  " << q00.isQuantum() << endl;
    cout << "Is quantity:                 " << q00.isQuantity() << endl;
    cout << "Is empty:                    " << q00.isEmpty() << endl;
    cout << "Is scalar:                   " << q00.isScalar() << endl;
    cout << "Is array:                    " << q00.isArray() << endl;
    cout << "Is real:                     " << q00.isReal() << endl;
    cout << "Is complex:                  " << q00.isComplex() << endl;
    cout << "Is double:                   " << q00.isQuantumDouble() << endl;
    cout << "Is float:                    " << q00.isQuantumFloat() << endl;
    cout << "Is int32_t:                      " << q00.isQuantumInt() << endl;
    cout << "Is Complex:                  " << q00.isQuantumComplex() << endl;
    cout << "Is DComplex:                 " << q00.isQuantumDComplex() << endl;
    cout << "Is Vector double:            " <<
      q00.isQuantumVectorDouble() << endl;
    cout << "Is Vector float:             " <<
      q00.isQuantumVectorFloat() << endl;
    cout << "Is Vector int32_t:               " <<
      q00.isQuantumVectorInt() << endl;
    cout << "Is Vector Complex:           " <<
      q00.isQuantumVectorComplex() << endl;
    cout << "Is Vector DComplex:          " <<
      q00.isQuantumVectorDComplex() << endl;
    q01 = q00;
    cout << "As quantity:                 " << q00.asQuantity() << endl;
    cout << "As double:                   " << q00.asQuantumDouble() << endl;
    cout << "As Vector double:            " <<
      q00.asQuantumVectorDouble() << endl;
    cout << "As float:                    " << q00.asQuantumFloat() << endl;
    cout << "As Vector float:             " <<
      q00.asQuantumVectorFloat() << endl;
    cout << "As int32_t:                      " << q00.asQuantumInt() << endl;
    cout << "As Vector int32_t:               " <<
      q00.asQuantumVectorInt() << endl;
    q00 = q01;
    cout << "As Complex:                  " << q00.asQuantumComplex() << endl;
    cout << "As Vector Complex:           " <<
      q00.asQuantumVectorComplex() << endl;
    cout << "As DComplex:                 " << q00.asQuantumDComplex() << endl;
    cout << "As Vector DComplex:          " <<
      q00.asQuantumVectorDComplex() << endl;

    cout << "Input quantity:              " << (QBase &)x01 << endl;
    if (QuantumHolder(x01).toRecord(error, y00)) {
      if (q00.fromRecord(error, y00)) {
	cout <<"Record output quantity:      " << q00.asQuantum() << endl;
      } else {
	cout << "From error: " << error << endl;
      }
    } else {
      cout << "To error: " << error << endl;
    }
    cout << "Input quantity:              " << (QBase &)x02 << endl;
    if (QuantumHolder(x02).toRecord(error, y00)) {
      if (q00.fromRecord(error, y00)) {
	cout <<"Record output quantity:      " << q00.asQuantity() << endl;
      } else {
	cout << "From error: " << error << endl;
      }
    } else {
      cout << "To error: " << error << endl;
    }
    cout << "Error expected:" << endl;
    cout << "Input quantity:              " << (QBase &)x00 << endl;
    if (QuantumHolder(x00).toRecord(error, y00)) {
      y00.renameField("units", RecordFieldId("unit"));
      if (q00.fromRecord(error, y00)) {
	cout <<"Record output quantity:      " << q00.asQuantity() << endl;
      } else {
	cout << "From error: " << error << endl;
      }
    } else {
      cout << "To error: " << error << endl;
    }
    cout << "Input String:                " << s00 << endl;
    if (q00.fromString(error, s00)) {
      cout << "As quantity:                 " << q00.asQuantum() << endl;
    } else {
      cout << "Unexpected error for String " << s00 << endl;
    }
    cout << "Input String:                " << s01 << endl;
    if (q00.fromString(error, s01)) {
      cout << "As quantity:                 " << q00.asQuantum() << endl;
    } else {
      cout << "Unexpected error for String " << s01 << endl;
    }
    cout << "Input String:                " << s02 << endl;
    if (q00.fromString(error, s02)) {
      cout << "As quantity:                 " << q00.asQuantum() << endl;
    } else {
      cout << "Expected error for String " << s02 <<
	" (value still " << q00.asQuantum() << ")" << endl;
      cout << "Error message now: " << error << endl;
    }
    cout << "----------------------------------------------------" << endl;

  } catch (std::exception& x) {
    cout << x.what() << endl;
    return 1;
  } 

  return 0;
}
