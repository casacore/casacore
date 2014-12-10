//# Quantum2.cc: class to manipulate phsical, dimensioned quantities
//# Copyright (C) 1996,1998,1999,2002
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

#include <casacore/casa/Quanta/Quantum.h>
#include <casacore/casa/Quanta/MVAngle.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/casa/Utilities/MUString.h>
#include <casacore/casa/Utilities/Regex.h>
#include <casacore/casa/iostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

istream &operator>> (istream &is, Quantity &ku)
{
  String str;
  is >> str;
  if (ios::failbit & is.rdstate()) return is;
  Quantity t;
  if (Quantity::read(t, str)) {
    ku = t;
  } else {
    is.clear(ios::failbit | is.rdstate());
  }
  return is;
}

Bool readQuantity(Quantity &res, MUString &in)
{
  Double val0 = 0.0;
  String unit = "";
  res = Quantity();
  UnitVal uv;
  in.push();
  if (!in.eos()) {
    if (MVAngle::read(res, in) || MVTime::read(res, in)) {
      val0 = res.getValue();
      unit = res.getUnit();
    } else {
      val0 = in.getDouble();
      unit = in.get();
      // Check if valid unit specified
      if (!UnitVal::check(unit, uv)) {
	in.pop(); return False;
      }
    }
  }
  //
  // The next statement is necessary once the read return arg is templated
  //  Qtype tmp = (Qtype)((res.getValue()) + val0)
  res.setValue(val0);
  res.setUnit(unit);
  in.unpush();
  return True; 
}

Bool readQuantity(Quantity &res, const String &in)
{
  static Regex ex("^[[:space:][:punct:]]*[[:digit:]]");
  static Regex ex2("[tT][oO][dD][aA][yY]");
  static Regex ex3("[nN][oO][wW]");
  MUString tmp(in);
  // The next construct is to cater for an unexplained error in
  // the Linux egcs stream input library; and an even more funny one in sgi
  ///  if (!in.empty() && (in[0] == 'n' || in[0] == 'N' || in[0] == 'y' ||
  ///		      in[0] == 'Y')) {
  if (!in.empty() && !in.contains(ex2) && !in.contains(ex3) &&
      !in.contains(ex)) {
    tmp = MUString(String("0.0") + in);		// Pointed non-const String
  }
  return readQuantity(res, tmp);
}

} //# NAMESPACE CASACORE - END

