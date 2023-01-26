//# Math.cc: Implementation of miscellaneous functions in Math.h
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002
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

#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Utilities/Assert.h>

// Changes for SUN CC port - abs changed to fabs for double and float args.

// the following is needed to get the finite function used in isInf
#if defined (AIPS_SOLARIS) || defined(AIPS_IRIX)
#include <ieeefp.h>
#endif

namespace casacore { //# NAMESPACE CASACORE - BEGIN

bool near(uint32_t val1, uint32_t val2, double tol) {
  if (tol <= 0) {
    return (val1 == val2);
  }
  if (val1 == val2) {
    return true;
  } else if (val1 > val2) {
    return (double(val1-val2) <= tol*max(val1,val2));
  } else {
    return (double(val2-val1) <= tol*max(val1,val2));
  }
}

bool near(int32_t val1, int32_t val2, double tol) {
  if (tol <=0) {
    return (val1 == val2);
  }
  if (val1 == val2) {
    return true;
  }
  if ((0<val1) != (0<val2)) {
    return false;
  }
  const int32_t aval1 = std::abs(val1);
  const int32_t aval2 = std::abs(val2);
  return (double(aval1-aval2) <= tol*double(max(aval1,aval2)));
}

bool near(float val1, float val2, double tol) {
  if (tol <=0) {
    return (val1 == val2);
  }
  if (val1 == val2) {
    return true;
  }
  if (val1 == 0) {
    return (fabs(val2) <= (1+tol)*C::flt_min);
  }
  else if (val2 == 0) {
    return (fabs(val1) <= (1+tol)*C::flt_min);
  }
  if ((0<val1) != (0<val2)) {
    return false;
  }
  return (fabs(val1-val2) <= tol*max(fabs(val1),fabs(val2)));
}

bool near(float val1, double val2, double tol) {
   return near(double(val1), val2, tol);
}

bool near(double val1, float val2, double tol) {
   return near(val1, double(val2), tol);
}

bool near(double val1, double val2, double tol) {
  if (tol <=0) {
    return (val1 == val2);
  }
  if (val1 == val2) {
    return true;
  }
  if (val1 == 0) {
    return (fabs(val2) <= (1+tol)*C::dbl_min);
  }
  else if (val2 == 0) {
    return (fabs(val1) <= (1+tol)*C::dbl_min);
  }
  if ((0<val1) != (0<val2)) {
    return false;
  }
  return (fabs(val1-val2) <= tol*max(fabs(val1),fabs(val2)));
}

bool nearAbs(uint32_t val1, uint32_t val2, double tol) {
  if (val1 == val2) {
    return true;
  } else if (val1 > val2) {
    return (tol >= double(val1 - val2));
  } else {
    return (tol >= double(val2 - val1));
  }
}

bool nearAbs(int32_t val1, int32_t val2, double tol) {
  return (tol >= double(std::abs(val2 - val1)));
}

bool nearAbs(float val1, float val2, double tol) {
  return (tol >= double(fabs(val2 - val1)));
}

bool nearAbs(double val1, double val2, double tol) {
  return (tol >= fabs(val2 - val1));
}


float floatNaN() {
  static float nanval;
  static bool init = false;
  if (!init) {
    init = true;
    // All bits on is a NaN
    unsigned char *uptr = (unsigned char *)&nanval;
    for (uint32_t i=0; i<sizeof(nanval); i++) {
      uptr[i] = 255;
    }
    AlwaysAssert(isNaN(nanval), AipsError);
  }
  return nanval;
}

double doubleNaN() {
  static double nanval;
  static bool init = false;
  if (!init) {
    init = true;
    // All bits on is a NaN
    unsigned char *uptr = (unsigned char *)&nanval;
    for (uint32_t i=0; i<sizeof(nanval); i++) {
      uptr[i] = 255;
    }
    AlwaysAssert(isNaN(nanval), AipsError);
  }
  return nanval;
}

void setNaN(float& val) {
  val = floatNaN();
}

void setNaN(double& val) {
  val = doubleNaN();
}

bool isInf(float val) {
  // first see if the OS has a function for determining if the number is
  // infinite. I can only have access to Solaris, Linux and SGI machines to
  // determine this.
#if defined(AIPS_LINUX)
  return (std::isinf(double(val)));
#elif defined(AIPS_DARWIN)
  return (std::isinf(double(val)));
#elif defined(AIPS_SOLARIS) || defined(AIPS_IRIX)
  return (!finite(double(val)) && !isnanf(val));
#else // Otherwise this is a default implementation.
  const unsigned char* uptr = (const unsigned char*) &val;
  uint32_t start, stop;
#if defined(AIPS_LITTLE_ENDIAN)
    if (((uptr[sizeof(val)-1] & 0x7f) != 0x7f) || 
        (uptr[sizeof(val)-2] != 0x80) ) {
      return false;
    }
    start = 0; 
    stop = sizeof(val)-2;
#else
    if (((uptr[0] & 0x7f) != 0x7f) || (uptr[1] != 0x80) ) {
      return false;
    }
    start = 2; 
    stop = sizeof(val);
#endif
  for (uint32_t i = start; i < stop; i++) {
    if (uptr[i] != 0x00) return false;
  }
  return true;
#endif
}

bool isInf(double val) {
  // first see if the OS has a function for determining if the number is
  // infinite. I can only have access to Solaris, Linux and SGI machines to
  // determine this.
#if defined(AIPS_LINUX)
  return (std::isinf(double(val)));
#elif defined(AIPS_DARWIN)
  return (std::isinf(double(val)));
#elif defined(AIPS_SOLARIS) || defined(AIPS_IRIX)
  return (!finite(double(val)) && !isnanf(val));
#else // Otherwise this is a default implementation.
  const unsigned char* uptr = (const unsigned char*) &val;
  uint32_t start, stop;
#if defined(AIPS_LITTLE_ENDIAN)
    if (((uptr[sizeof(val)-1] & 0x7f) != 0x7f) || 
        (uptr[sizeof(val)-2] != 0xf0) ) {
      return false;
    }
    start = 0; 
    stop = sizeof(val)-2;
#else
    if (((uptr[0] & 0x7f) != 0x7f) || (uptr[1] != 0xf0) ) {
      return false;
    }
    start = 2; 
    stop = sizeof(val);
#endif
  for (uint32_t i = start; i < stop; i++) {
    if (uptr[i] != 0x00) return false;
  }
  return true;
#endif
}

float floatInf() {
  static float infval;
  static bool init = false;
  if (!init) {
    init = true;
    unsigned char *uptr = (unsigned char*) &infval;

    for (uint32_t i=0; i<sizeof(infval); i++) {
      uptr[i] = 0x00;
    }
#if defined(AIPS_LITTLE_ENDIAN)
    uptr[sizeof(infval)-1] = 0x7f;
    uptr[sizeof(infval)-2] = 0x80;
#else
    uptr[0] = 0x7f;
    uptr[1] = 0x80;
#endif
  AlwaysAssert(isInf(infval), AipsError);
  }
  return infval;
}

double doubleInf() {
  static double infval;
  static bool init = false;
  if (!init) {
    init = true;
    unsigned char *uptr = (unsigned char*) &infval;
    for (uint32_t i=0; i<sizeof(infval); i++) {
      uptr[i] = 0x00;
    }
#if defined(AIPS_LITTLE_ENDIAN)
    uptr[sizeof(infval)-1] = 0x7f;
    uptr[sizeof(infval)-2] = 0xf0;
#else
    uptr[0] = 0x7f;
    uptr[1] = 0xf0;
#endif
    AlwaysAssert(isInf(infval), AipsError);
  }
  return infval;
}

void setInf(float& val) {
  val = floatInf();
}

void setInf(double& val) {
  val = doubleInf();
}

double roundDouble(double val, double ndigit) {
  double sign = 1;
  if (val == 0) {
    return 0;
  } else if (val < 0) {
    sign = -1;
  }
  val *= sign;
  double lgr = log10(val) - ndigit;
  // E.g. log10(0.1) gives -0.9999999, so add little number when truncating.
  int32_t i = int32_t(lgr >= 0  ?  lgr + 1.000001 : lgr - 0.000001);
  double temp = val * pow(10.0, -i);
  using std::round;
  return sign*round(temp)*pow(10.0, i);
}

} //# NAMESPACE CASACORE - END

