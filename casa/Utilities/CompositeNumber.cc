//# CompositeNumber.h: generate a composite number
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
//# $Id$

#include <casacore/casa/Utilities/CompositeNumber.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/BasicMath/Math.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

CompositeNumber::CompositeNumber(const uInt maxval) {

  itsMaxComplete = maxval;
  if (itsMaxComplete < 2) { itsMaxComplete = 2; }
  generate(itsMaxComplete);

}

void CompositeNumber::generate(const uInt maxval) {
  
  itsMaxComplete = maxval;

  uInt n2 = (uInt)(log((Float)maxval)/log(2.0) + 1) +1;
  uInt n3 = (uInt)(log((Float)maxval)/log(3.0) + 1) +1;
  uInt n5 = (uInt)(log((Float)maxval)/log(5.0) + 1) +1;
  
  itsNumbers.resize(n2*n3*n5);
  uInt n = 0;
  for (uInt i2=0; i2<n2; i2++) {
    for (uInt i3=0; i3<n3; i3++) {
      for (uInt i5=0; i5<n5; i5++) {
	itsNumbers[n] = (uInt)(pow(2.0, (Float)i2) * pow(3.0, (Float)i3) * 
			       pow(5.0, (Float)i5) );
	n++;
      }
    }
  }
  GenSort<uInt>::sort(itsNumbers, n2*n3*n5);
}


CompositeNumber::~CompositeNumber() {}




uInt CompositeNumber::nextLarger(const uInt testValue) {

  if (testValue >  itsMaxComplete) {
    generate(testValue);
  }
  for (uInt i=0;i< itsNumbers.nelements(); i++) {
    if (itsNumbers[i] > testValue) {
      return itsNumbers[i];
    }
  }
  return  itsNumbers[0];
}
    


uInt CompositeNumber::nextSmaller(const uInt testValue) {
  if (testValue >  itsMaxComplete) {
    generate(testValue);
  }

  for (Int i=itsNumbers.nelements()-1; i>=0; i--) {
    if (itsNumbers[i] < testValue) {
      return itsNumbers[i];
    }
  }
  return itsNumbers[0];
}
    



uInt CompositeNumber::nearest(const uInt testValue) {
  if (testValue >  itsMaxComplete) {
    generate(testValue);
  }
  for (uInt i=0;i< itsNumbers.nelements(); i++) {
    if (itsNumbers[i] > testValue) {
      if (i==0) {
	return itsNumbers[0];
      } else if (abs((Int)(itsNumbers[i]-testValue)) < abs((Int)(itsNumbers[(i-1)]-testValue)) ) {
	return itsNumbers[i];
      } else {
	return itsNumbers[(i-1)];
      }
    }
  }
  // Should never make it here!
  return itsNumbers[0];
} 





uInt CompositeNumber::nextLargerEven(const uInt testValue) {

  if (testValue >  itsMaxComplete) {
    generate(testValue);
  }
  for (uInt i=0;i< itsNumbers.nelements(); i++) {
    if (itsNumbers[i] > testValue && (itsNumbers[i]%2==0)) {
      return itsNumbers[i];
    }
  }
  return  itsNumbers[0];
}
    

uInt CompositeNumber::nextSmallerEven(const uInt testValue) {
  if (testValue >  itsMaxComplete) {
    generate(testValue);
  }

  for (Int i=itsNumbers.nelements()-1; i>=0; i--) {
    if (itsNumbers[i] < testValue && (itsNumbers[i]%2==0)) {
      return itsNumbers[i];
    }
  }
  return itsNumbers[0];
}
    

uInt CompositeNumber::nearestEven(const uInt testValue) {

  uInt up = nextLargerEven( testValue );
  uInt down = nextSmallerEven( testValue );
  if (abs((Int)(up-testValue)) < abs((Int)(down-testValue)) ) {
    return up;
  } else {
    return down;
  }
}
    

Bool CompositeNumber::isComposite(const uInt testValue) {
  if (testValue >  itsMaxComplete) {
    generate(testValue);
  }
  for (uInt i=0;i< itsNumbers.nelements(); i++) {
    if (itsNumbers[i] == testValue) {
      return True;
    }
  }
  return False;
} 


} //# NAMESPACE CASACORE - END

