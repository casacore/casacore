//# tRNG.cc: A test program for the RNG, ACG & MLCG classes
//# Copyright (C) 2000,2001
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

#include <casa/aips.h>
#include <casa/Exceptions/Error.h>
#include <casa/BasicSL/String.h>
#include <casa/iostream.h>
#include <casa/iomanip.h>

#include <casa/BasicMath/Random.h>

#include <casa/namespace.h>
int main() {
  try {
    uInt i;
    Float f;
    Double d;
    cout << "testing the MLCG generator" << endl;
    {
      cout << "random integers, floats & doubles" << endl; 
      MLCG g;
      for (uInt k = 0; k < 4; k++) {
	// Note the values are calculated here rather than in the print
	// statemement because of the problem discussed in
	// http://aips2.nrao.edu/mail/aips2-lib/1391
 	i = g.asuInt(); f = g.asFloat(); d = g.asDouble();
	cout << k << ": " << setbase(16) << i
 	     << ": " << setprecision(6) << f
 	     << ": " << setprecision(12) << d << endl;
      }
      cout << "resetting the generator. Should get the same numbers" << endl; 
      g.reset();
      for (uInt k = 0; k < 4; k++) {
 	i = g.asuInt(); f = g.asFloat(); d = g.asDouble();
 	cout << k << ": " << setbase(16) << i
 	     << ": " << setprecision(6) << f
 	     << ": " << setprecision(12) << d << endl;
      }
    }
    {
      cout << "Using a different seed" << endl; 
      MLCG g(1, 0);
      for (uInt k = 0; k < 4; k++) {
	i = g.asuInt(); f = g.asFloat(); d = g.asDouble();
	cout << k << ": " << setbase(16) << i 
	     << ": " << setprecision(6) << f 
	     << ": " << setprecision(12) << d << endl;
      }
      cout << "resetting the generator. Should get the same numbers" << endl; 
      g.reset();
      for (uInt k = 0; k < 4; k++) {
	i = g.asuInt(); f = g.asFloat(); d = g.asDouble();
	cout << k << ": " << setbase(16) << i 
	     << ": " << setprecision(6) << f 
	     << ": " << setprecision(12) << d << endl;
      }
    }
    cout << "testing the ACG generator" << endl;
    {
      cout << "random integers, floats & doubles" << endl; 
      ACG g;
      for (uInt k = 0; k < 4; k++) {
	i = g.asuInt(); f = g.asFloat(); d = g.asDouble();
	cout << k << ": " << setbase(16) << i 
	     << ": " << setprecision(6) << f 
	     << ": " << setprecision(12) << d << endl;
      }
      cout << "resetting the generator. Should get the same numbers" << endl; 
      g.reset();
      for (uInt k = 0; k < 4; k++) {
	i = g.asuInt(); f = g.asFloat(); d = g.asDouble();
	cout << k << ": " << setbase(16) << i 
	     << ": " << setprecision(6) << f 
	     << ": " << setprecision(12) << d << endl;
      }
    }
    {
      cout << "Using a different seed" << endl; 
      ACG g(7326458, 98);
      for (uInt k = 0; k < 4; k++) {
	i = g.asuInt(); f = g.asFloat(); d = g.asDouble();
	cout << k << ": " << setbase(16) << i 
	     << ": " << setprecision(6) << f 
	     << ": " << setprecision(12) << d << endl;
      }
      cout << "resetting the generator. Should get the same numbers" << endl; 
      g.reset();
      for (uInt k = 0; k < 4; k++) {
	i = g.asuInt(); f = g.asFloat(); d = g.asDouble();
	cout << k << ": " << setbase(16) << i 
	     << ": " << setprecision(6) << f 
	     << ": " << setprecision(12) << d << endl;
      }
    }
  }
  catch (AipsError x) {
    cerr << x.getMesg() << endl;
    cout << "FAIL" << endl;
    return 1;
  }
  catch (...) {
    cerr << "Exception not derived from AipsError" << endl;
    cout << "FAIL" << endl;
    return 1;
  }
  cout << "OK" << endl;
  return 0;
}
// Local Variables: 
// compile-command: "gmake tRNG"
// End: 
