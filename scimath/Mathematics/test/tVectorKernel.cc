//# Copyright (C) 2010 by ESO (in the framework of the ALMA collaboration)
//# Copyright (C) 1996,1997,1998,1999,2002
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
#include <casa/Arrays/Array.h>
#include <casa/Arrays/Vector.h>
#include <scimath/Mathematics/VectorKernel.h>
#include <casa/iostream.h>

#include <casa/namespace.h>

void writeResult( bool failed ){
	if ( failed ){
		cout << "Failed"<<endl;
	}
	else {
		cout << "Passed"<<endl;
	}
}
int main() {
  Bool anyFailures = False;
  {
    Bool failed = False;
    // Test HANNING with shape = 3, peakUnity = true;
    const int SHAPE = 3;
    cout << "Test: method=HANNING, shape=3, peakUnity=true"<<endl;
    Vector<Double> result = VectorKernel::make(VectorKernel::HANNING, 0.0,
                                      SHAPE, true, true);
    if ( static_cast<int> (result.size()) != SHAPE ){
    	failed = true;
    	cout << "Unexpected result size="<<result.size()<<" did not match expected size of "<<SHAPE<<endl;
    }
    else {
    	Vector<Double> expectedResult(SHAPE);
    	expectedResult[0] = 0.5;
    	expectedResult[1] = 1;
    	expectedResult[2] = 0.5;
    	for ( int i = 0; i < SHAPE; i++ ){
    		if ( abs( result[i] - expectedResult[i]) > .0000001 ){
    			cout <<"Result "<<i<<" of "<<result[i]<<
    					" did not match expected result "<<expectedResult[i]<<endl;
    			failed = true;
    		}
    	}
    }
    
    writeResult( failed );
    if ( failed ){
    	anyFailures = true;
    }
  }

  {
     Bool failed = False;
     // Test HANNING with shape = 3, peakUnity = false;
     const int SHAPE = 3;
     cout << "Test: method=HANNING, shape=3, peakUnity=false"<<endl;
     Vector<Double> result = VectorKernel::make(VectorKernel::HANNING, 0.0,
                                       SHAPE, true, false);
     if ( static_cast<int> (result.size()) != SHAPE ){
     	failed = true;
     	cout << "Unexpected result size="<<result.size()<<
     			" did not match expected size of "<<SHAPE<<endl;
     }
     else {
     	Vector<Double> expectedResult(SHAPE);
     	expectedResult[0] = 0.25;
     	expectedResult[1] = 0.5;
     	expectedResult[2] = 0.25;
     	for ( int i = 0; i < SHAPE; i++ ){
     		if ( abs( result[i] - expectedResult[i]) > .0000001 ){
     			cout <<"Result "<<i<<" of "<<result[i]<<
     					" did not match expected result "<<expectedResult[i]<<endl;
     			failed = true;
     		}
     	}
     }

     writeResult( failed );
     if ( failed ){
         anyFailures = true;
     }
   }

  {
     Bool failed = False;
     // Test HANNING with shape = 5, peakUnity = true;
     const int SHAPE = 5;
     cout << "Test: method=HANNING, shape=5, peakUnity=true"<<endl;
     Vector<Double> result = VectorKernel::make(VectorKernel::HANNING, 0.0,
                                       SHAPE, true, true);
     if ( static_cast<int> (result.size()) != SHAPE ){
     	failed = true;
     	cout << "Unexpected result size="<<result.size()<<
     			" did not match expected size of "<<SHAPE<<endl;
     }
     else {
     	Vector<Double> expectedResult(SHAPE);
     	expectedResult[0] = 0.25;
     	expectedResult[1] = 0.75;
     	expectedResult[2] = 1;
     	expectedResult[3] = 0.75;
     	expectedResult[4] = 0.25;
     	for ( int i = 0; i < SHAPE; i++ ){
     		if ( abs( result[i] - expectedResult[i]) > .0000001 ){
     			cout <<"Result "<<i<<" of "<<result[i]<<
     					" did not match expected result "<<expectedResult[i]<<endl;
     			failed = true;
     		}
     	}
     }

     writeResult( failed );
     if ( failed ){
     	anyFailures = true;
     }
   }

  {
      Bool failed = False;
      // Test HANNING with shape = 5, peakUnity = false;
      const int SHAPE = 5;
      cout << "Test: method=HANNING, shape=5, peakUnity=false"<<endl;
      Vector<Double> result = VectorKernel::make(VectorKernel::HANNING, 0.0,
                                        SHAPE, true, false);
      if ( static_cast<int> (result.size()) != SHAPE ){
      	failed = true;
      	cout << "Unexpected result size="<<result.size()<<
      			" did not match expected size of "<<SHAPE<<endl;
      }
      else {
      	Vector<Double> expectedResult(SHAPE);
      	expectedResult[0] = 0.08333333;
      	expectedResult[1] = 0.25;
      	expectedResult[2] = 0.33333333;
      	expectedResult[3] = 0.25;
      	expectedResult[4] = 0.08333333;
      	for ( int i = 0; i < SHAPE; i++ ){
      		if ( abs( result[i] - expectedResult[i]) > .0000001 ){
      			cout <<"Result "<<i<<" of "<<result[i]<<
      					" did not match expected result "<<expectedResult[i]<<endl;
      			failed = true;
      		}
      	}
      }

      writeResult( failed );
      if ( failed ){
          anyFailures = true;
      }
    }

  //////////////////////////////////////

  if (anyFailures) {
    cout << "FAIL" << endl;
    return 1;
  }
  else {
    cout << "OK" << endl;
    return 0;
  }

}

// End:
