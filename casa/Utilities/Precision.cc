//# DataType.h: data types (primarily) in the table system
//# Copyright (C) 1993,1994,1995,1996,1999,2001
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

#include <casacore/casa/Utilities/Precision.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

uInt precisionForValueErrorPairs (const Vector<Double>& pair1,
                                  const Vector<Double>& pair2)
{
  Double val1 = fabs(pair1[0]);
  Double err1 = pair1[1];
  Double value = val1;
  Double error = fabs(err1);

  if (pair2.size() == 2){
    Double val2 = fabs(pair2[0]);
    Double err2 = pair2[1];
    value = max(val1, val2);
    error = (err1 == 0 || err2 == 0)
      ? max(fabs(err1), fabs(err2))
      : min(fabs(err1), fabs(err2));
  }

  // Here are a few general safeguards
  // If we are dealing with a value smaller than the estimated error
  // (e.g., 0.6 +/- 12) , the roles in formatting need to be
  // reversed.
  if ( value < error ) {
    value = max(value,0.1*error);
    // TODO be cool and figure out a way to swap without using a temporary variable
    Double tmp = value;
    value = error;
    error = tmp;
  }

  // A value of precisely 0 formats as if it were 1.  If the error is
  // precisely 0, we print to 3 significant digits
  if ( value == 0 ) {
    value = 1;
  }
  if ( error == 0 ) {
    error = 0.1*value;
  }

  // Arithmetically we have to draw the limit somewhere.  It is
  // unlikely that numbers (and errors) < 1e-10 are reasonably
  // printed using this limited technique.
  value = max(value, 1e-10);
  error = max(error, 1e-8);

  // Print only to two significant digits in the error
  error = 0.1*error;

  // Generate format
  // Add little value for possible round-off error
  uInt after = 0;
  if ( log10(error) < 0 ) {
    after = int(fabs(log10(error)) + 1e-8) + 1;
  }
  return after;
}

} //# NAMESPACE CASACORE - END

