//# Math.cc: Implementation of miscellaneous functions in Math.h
//# Copyright (C) 1995,1996
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

#include <aips/Mathematics/Math.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Utilities/Assert.h>


Bool near(uInt val1, uInt val2, Double tol)
{
    if (tol <= 0) {
        return ToBool(val1 == val2);
    }

    if (val1 == val2) return True;
    if ((0<val1) != (0<val2)) return False;

    if (val1 == 0) {
	return ToBool(val2 <= FLT_MIN + FLT_MIN*tol);
    } else if (val2 == 0) {
	return ToBool(val1 <= FLT_MIN + FLT_MIN*tol);
    }
    return ToBool(abs((Int) (val1-val2)) <= tol*max(val1,val2));
}

Bool near(Int val1, Int val2, Double tol)
{
    if (tol <=0) {
        return ToBool(val1 == val2);
    }

    if (val1 == val2) return True;
    if ((0<val1) != (0<val2)) return False;

    if (val1 == 0) {
	return ToBool(abs(val2) <= FLT_MIN + FLT_MIN*tol);
    } else if (val2 == 0) {
	return ToBool(abs(val1) <= FLT_MIN + FLT_MIN*tol);
    }
    return ToBool(abs(val1-val2) <= tol*max(abs(val1),abs(val2)));
}

Bool near(Float val1, Float val2, Double tol)
{
    if (tol <=0) {
        return ToBool(val1 == val2);
    }

    if (val1 == val2) return True;
    if ((0<val1) != (0<val2)) return False;

    if (val1 == 0) {
	return ToBool(abs(val2) <= FLT_MIN + FLT_MIN*tol);
    } else if (val2 == 0) {
	return ToBool(abs(val1) <= FLT_MIN + FLT_MIN*tol);
    }
    return ToBool(abs(val1-val2) <= tol*max(abs(val1),abs(val2)));
}

Bool near(Double val1, Double val2, Double tol)
{
    if (tol <=0) {
        return ToBool(val1 == val2);
    }

    if (val1 == val2) return True;
    if ((0<val1) != (0<val2)) return False;

    if (val1 == 0) {
	return ToBool(abs(val2) <= DBL_MIN + DBL_MIN*tol);
    } else if (val2 == 0) {
	return ToBool(abs(val1) <= DBL_MIN + DBL_MIN*tol);
    }
    return ToBool(abs(val1-val2) <= tol*max(abs(val1),abs(val2)));
}

Bool nearAbs(uInt val1, uInt val2, uInt tol)
{
    return ToBool(tol > abs((Int) (val2 - val1)));
}
Bool nearAbs(Int val1, Int val2, Int tol)
{
    DebugAssert(tol >= 0, AipsError);
    return ToBool(tol > abs(val2 - val1));
}
Bool nearAbs(Float val1, Float val2, Double tol)
{
    DebugAssert(tol >= 0, AipsError);
    return ToBool(tol > fabs(val2 - val1));
}

Bool nearAbs(Double val1, Double val2, Double tol)
{
    DebugAssert(tol >= 0, AipsError);
    return ToBool(tol > fabs(val2 - val1));
}

Bool isNaN(const float &val)
{
    return ToBool(isnan(double(val)));
}

Bool isNaN(const double &val)
{
    return ToBool(isnan(val));
}

void setNaN(float &val)
{
    static float nanval;
    static Bool init = False;
    if (!init) {
        init = True;
	// All bits on is a NaN
	uChar *uptr = (uChar *)&nanval;
	for (Int i=0; i<sizeof(nanval); i++) {
	    uptr[i] = 255;
	}
	AlwaysAssert(isNaN(nanval), AipsError);
    }
    val = nanval;
}

void setNaN(double &val)
{
    static double nanval;
    static Bool init = False;
    if (!init) {
        init = True;
	// All bits on is a NaN
	uChar *uptr = (uChar *)&nanval;
	for (Int i=0; i<sizeof(nanval); i++) {
	    uptr[i] = 255;
	}
	AlwaysAssert(isNaN(nanval), AipsError);
    }
    val = nanval;
}
