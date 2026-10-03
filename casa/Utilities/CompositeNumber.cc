// # CompositeNumber.h: generate a composite number
// # Copyright (C) 2000,2001
// # Associated Universities, Inc. Washington DC, USA.
// #
// # This library is free software; you can redistribute it and/or modify it
// # under the terms of the GNU Library General Public License as published by
// # the Free Software Foundation; either version 2 of the License, or (at your
// # option) any later version.
// #
// # This library is distributed in the hope that it will be useful, but WITHOUT
// # ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// # FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
// # License for more details.
// #
// # You should have received a copy of the GNU Library General Public License
// # along with this library; if not, write to the Free Software Foundation,
// # Inc., 675 Massachusetts Ave, Cambridge, MA 02139, USA.
// #
// # Correspondence concerning AIPS++ should be addressed as follows:
// #        Internet email: casa-feedback@nrao.edu.
// #        Postal address: AIPS++ Project Office
// #                        National Radio Astronomy Observatory
// #                        520 Edgemont Road
// #                        Charlottesville, VA 22903-2475 USA

#include <casacore/casa/Utilities/CompositeNumber.h>
#include <casacore/casa/Utilities/GenSort.h>
#include <casacore/casa/BasicMath/Math.h>

namespace casacore {  // # NAMESPACE CASACORE - BEGIN

CompositeNumber::CompositeNumber(const unsigned int maxval) {
  itsMaxComplete = maxval;
  if (itsMaxComplete < 2) {
    itsMaxComplete = 2;
  }
  generate(itsMaxComplete);
}

void CompositeNumber::generate(const unsigned int maxval) {
  itsMaxComplete = maxval;

  unsigned int n2 = (unsigned int)(log((float)maxval) / log(2.0) + 1) + 1;
  unsigned int n3 = (unsigned int)(log((float)maxval) / log(3.0) + 1) + 1;
  unsigned int n5 = (unsigned int)(log((float)maxval) / log(5.0) + 1) + 1;

  itsNumbers.resize(n2 * n3 * n5);
  unsigned int n = 0;
  for (unsigned int i2 = 0; i2 < n2; i2++) {
    for (unsigned int i3 = 0; i3 < n3; i3++) {
      for (unsigned int i5 = 0; i5 < n5; i5++) {
        itsNumbers[n] = (unsigned int)(pow(2.0, (float)i2) * pow(3.0, (float)i3) * pow(5.0, (float)i5));
        n++;
      }
    }
  }
  GenSort<unsigned int>::sort(itsNumbers, n2 * n3 * n5);
}

CompositeNumber::~CompositeNumber() {}

unsigned int CompositeNumber::nextLarger(const unsigned int testValue) {
  if (testValue > itsMaxComplete) {
    generate(testValue);
  }
  for (unsigned int i = 0; i < itsNumbers.nelements(); i++) {
    if (itsNumbers[i] > testValue) {
      return itsNumbers[i];
    }
  }
  return itsNumbers[0];
}

unsigned int CompositeNumber::nextSmaller(const unsigned int testValue) {
  if (testValue > itsMaxComplete) {
    generate(testValue);
  }

  for (int i = itsNumbers.nelements() - 1; i >= 0; i--) {
    if (itsNumbers[i] < testValue) {
      return itsNumbers[i];
    }
  }
  return itsNumbers[0];
}

unsigned int CompositeNumber::nearest(const unsigned int testValue) {
  if (testValue > itsMaxComplete) {
    generate(testValue);
  }
  for (unsigned int i = 0; i < itsNumbers.nelements(); i++) {
    if (itsNumbers[i] > testValue) {
      if (i == 0) {
        return itsNumbers[0];
      } else if (abs((int)(itsNumbers[i] - testValue)) <
                 abs((int)(itsNumbers[(i - 1)] - testValue))) {
        return itsNumbers[i];
      } else {
        return itsNumbers[(i - 1)];
      }
    }
  }
  // Should never make it here!
  return itsNumbers[0];
}

unsigned int CompositeNumber::nextLargerEven(const unsigned int testValue) {
  if (testValue > itsMaxComplete) {
    generate(testValue);
  }
  for (unsigned int i = 0; i < itsNumbers.nelements(); i++) {
    if (itsNumbers[i] > testValue && (itsNumbers[i] % 2 == 0)) {
      return itsNumbers[i];
    }
  }
  return itsNumbers[0];
}

unsigned int CompositeNumber::nextSmallerEven(const unsigned int testValue) {
  if (testValue > itsMaxComplete) {
    generate(testValue);
  }

  for (int i = itsNumbers.nelements() - 1; i >= 0; i--) {
    if (itsNumbers[i] < testValue && (itsNumbers[i] % 2 == 0)) {
      return itsNumbers[i];
    }
  }
  return itsNumbers[0];
}

unsigned int CompositeNumber::nearestEven(const unsigned int testValue) {
  unsigned int up = nextLargerEven(testValue);
  unsigned int down = nextSmallerEven(testValue);
  if (abs((int)(up - testValue)) < abs((int)(down - testValue))) {
    return up;
  } else {
    return down;
  }
}

bool CompositeNumber::isComposite(const unsigned int testValue) {
  if (testValue > itsMaxComplete) {
    generate(testValue);
  }
  for (unsigned int i = 0; i < itsNumbers.nelements(); i++) {
    if (itsNumbers[i] == testValue) {
      return true;
    }
  }
  return false;
}

}  // namespace casacore
