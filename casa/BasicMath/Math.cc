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
//#
//# $Id$

#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/BasicSL/Constants.h>
#include <casacore/casa/Utilities/Assert.h>

// Changes for SUN CC port - abs changed to fabs for double and float args.

// the following is needed to get the finite function used in isInf
#if defined (AIPS_SOLARIS) || defined(AIPS_IRIX)
#include <ieeefp.h>
#endif

namespace casacore { //# NAMESPACE CASACORE - BEGIN

Bool near(uInt val1, uInt val2, Double tol) {
  if (tol <= 0) {
    return (val1 == val2);
  }
  if (val1 == val2) {
    return True;
  } else if (val1 > val2) {
    return (Double(val1-val2) <= tol*max(val1,val2));
  } else {
    return (Double(val2-val1) <= tol*max(val1,val2));
  }
}

Bool near(Int val1, Int val2, Double tol) {
  if (tol <=0) {
    return (val1 == val2);
  }
  if (val1 == val2) {
    return True;
  }
  if ((0<val1) != (0<val2)) {
    return False;
  }
  const Int aval1 = std::abs(val1);
  const Int aval2 = std::abs(val2);
  return (Double(aval1-aval2) <= tol*Double(max(aval1,aval2)));
}

Bool near(Float val1, Float val2, Double tol) {
  if (tol <=0) {
    return (val1 == val2);
  }
  if (val1 == val2) {
    return True;
  }
  if (val1 == 0) {
    return (fabs(val2) <= (1+tol)*C::flt_min);
  }
  else if (val2 == 0) {
    return (fabs(val1) <= (1+tol)*C::flt_min);
  }
  if ((0<val1) != (0<val2)) {
    return False;
  }
  return (fabs(val1-val2) <= tol*max(fabs(val1),fabs(val2)));
}

Bool near(Float val1, Double val2, Double tol) {
   return near(Double(val1), val2, tol);
}

Bool near(Double val1, Float val2, Double tol) {
   return near(val1, Double(val2), tol);
}

Bool near(Double val1, Double val2, Double tol) {
  if (tol <=0) {
    return (val1 == val2);
  }
  if (val1 == val2) {
    return True;
  }
  if (val1 == 0) {
    return (fabs(val2) <= (1+tol)*C::dbl_min);
  }
  else if (val2 == 0) {
    return (fabs(val1) <= (1+tol)*C::dbl_min);
  }
  if ((0<val1) != (0<val2)) {
    return False;
  }
  return (fabs(val1-val2) <= tol*max(fabs(val1),fabs(val2)));
}

Bool nearAbs(uInt val1, uInt val2, Double tol) {
  if (val1 == val2) {
    return True;
  } else if (val1 > val2) {
    return (tol >= Double(val1 - val2));
  } else {
    return (tol >= Double(val2 - val1));
  }
}

Bool nearAbs(Int val1, Int val2, Double tol) {
  return (tol >= Double(std::abs(val2 - val1)));
}

Bool nearAbs(Float val1, Float val2, Double tol) {
  return (tol >= Double(fabs(val2 - val1)));
}

Bool nearAbs(Double val1, Double val2, Double tol) {
  return (tol >= fabs(val2 - val1));
}


Float floatNaN() {
  static Float nanval;
  static Bool init = False;
  if (!init) {
    init = True;
    // All bits on is a NaN
    uChar *uptr = (uChar *)&nanval;
    for (uInt i=0; i<sizeof(nanval); i++) {
      uptr[i] = 255;
    }
    AlwaysAssert(isNaN(nanval), AipsError);
  }
  return nanval;
}

Double doubleNaN() {
  static Double nanval;
  static Bool init = False;
  if (!init) {
    init = True;
    // All bits on is a NaN
    uChar *uptr = (uChar *)&nanval;
    for (uInt i=0; i<sizeof(nanval); i++) {
      uptr[i] = 255;
    }
    AlwaysAssert(isNaN(nanval), AipsError);
  }
  return nanval;
}

void setNaN(Float& val) {
  val = floatNaN();
}

void setNaN(Double& val) {
  val = doubleNaN();
}

Bool isInf(Float val) {
  // first see if the OS has a function for determining if the number is
  // infinite. I can only have access to Solaris, Linux and SGI machines to
  // determine this.
#if defined(AIPS_LINUX)
  return (isinf(Double(val)));
#elif defined(AIPS_DARWIN)
  return (std::isinf(Double(val)));
#elif defined(AIPS_SOLARIS) || defined(AIPS_IRIX)
  return (!finite(Double(val)) && !isnanf(val));
#else // Otherwise this is a default implementation.
  const uChar* uptr = (const uChar*) &val;
  uInt start, stop;
#if defined(AIPS_LITTLE_ENDIAN)
    if (((uptr[sizeof(val)-1] & 0x7f) != 0x7f) || 
        (uptr[sizeof(val)-2] != 0x80) ) {
      return False;
    }
    start = 0; 
    stop = sizeof(val)-2;
#else
    if (((uptr[0] & 0x7f) != 0x7f) || (uptr[1] != 0x80) ) {
      return False;
    }
    start = 2; 
    stop = sizeof(val);
#endif
  for (uInt i = start; i < stop; i++) {
    if (uptr[i] != 0x00) return False;
  }
  return True;
#endif
}

Bool isInf(Double val) {
  // first see if the OS has a function for determining if the number is
  // infinite. I can only have access to Solaris, Linux and SGI machines to
  // determine this.
#if defined(AIPS_LINUX)
  return (isinf(Double(val)));
#elif defined(AIPS_DARWIN)
  return (std::isinf(Double(val)));
#elif defined(AIPS_SOLARIS) || defined(AIPS_IRIX)
  return (!finite(Double(val)) && !isnanf(val));
#else // Otherwise this is a default implementation.
  const uChar* uptr = (const uChar*) &val;
  uInt start, stop;
#if defined(AIPS_LITTLE_ENDIAN)
    if (((uptr[sizeof(val)-1] & 0x7f) != 0x7f) || 
        (uptr[sizeof(val)-2] != 0xf0) ) {
      return False;
    }
    start = 0; 
    stop = sizeof(val)-2;
#else
    if (((uptr[0] & 0x7f) != 0x7f) || (uptr[1] != 0xf0) ) {
      return False;
    }
    start = 2; 
    stop = sizeof(val);
#endif
  for (uInt i = start; i < stop; i++) {
    if (uptr[i] != 0x00) return False;
  }
  return True;
#endif
}

Float floatInf() {
  static Float infval;
  static Bool init = False;
  if (!init) {
    init = True;
    uChar *uptr = (uChar*) &infval;

    for (uInt i=0; i<sizeof(infval); i++) {
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

Double doubleInf() {
  static Double infval;
  static Bool init = False;
  if (!init) {
    init = True;
    uChar *uptr = (uChar*) &infval;
    for (uInt i=0; i<sizeof(infval); i++) {
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

void setInf(Float& val) {
  val = floatInf();
}

void setInf(Double& val) {
  val = doubleInf();
}

Double roundDouble(Double val, Double ndigit) {
  Double sign = 1;
  if (val == 0) {
    return 0;
  } else if (val < 0) {
    sign = -1;
  }
  val *= sign;
  Double lgr = log10(val) - ndigit;
  // E.g. log10(0.1) gives -0.9999999, so add little number when truncating.
  Int i = Int(lgr >= 0  ?  lgr + 1.000001 : lgr - 0.000001);
  Double temp = val * pow(10.0, -i);
  return sign*round(temp)*pow(10.0, i);
}

} //# NAMESPACE CASACORE - END

