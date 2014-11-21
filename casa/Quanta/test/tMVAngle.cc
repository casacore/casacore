//# tMVAngle.cc: test program for MVAngle class
//# Copyright (C) 1994,1995,1996,1998,1999,2000,2002
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
//# $Id: tMVAngle.cc 21335 2013-03-28 14:20:18Z gervandiepen $

#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Quanta/MVAngle.h>

#include <casacore/casa/namespace.h>

void showTime (MVAngle time, uInt format, uInt prec)
{
  cout << MVAngle::Format(format,prec) << time << endl;
}

Bool testMUS (Quantity& q, const String& str, double exp=0, Bool chk=True)
{
  MUString mus(str);
  Bool res = MVAngle::read (q, mus, chk);
  if (res) {
    if (!near(q.getValue(), exp)) {
      cout << "Incorrect value for " << str << endl;
      AlwaysAssertExit(near(q.getValue(), exp));
    }
  }
  return res;
}

int main ()
{
  try {
    Quantity q;
    AlwaysAssertExit (! testMUS (q, "5"));
    AlwaysAssertExit (! testMUS (q, "5h"));
    AlwaysAssertExit (! testMUS (q, "5h20"));
    AlwaysAssertExit (testMUS (q, "5h20m", 15*(5+20./60)));
    AlwaysAssertExit (! testMUS (q, "5.h20m"));
    AlwaysAssertExit (! testMUS (q, "5h20.m"));
    AlwaysAssertExit (! testMUS (q, "5hm"));
    AlwaysAssertExit (testMUS (q, "5h20ms", 15*(5+20./60)));
    AlwaysAssertExit (testMUS (q, "5h20m12.3", 15*(5+20./60+12.3/3600)));
    AlwaysAssertExit (testMUS (q, "5h20m12.3s", 15*(5+20./60+12.3/3600)));
    AlwaysAssertExit (testMUS (q, "5:", 15*5));
    AlwaysAssertExit (testMUS (q, "5:20", 15*(5+20./60)));
    AlwaysAssertExit (testMUS (q, "5:20:", 15*(5+20./60)));
    AlwaysAssertExit (testMUS (q, "5:20:12.3", 15*(5+20./60+12.3/3600)));
    AlwaysAssertExit (! testMUS (q, "5:20:12.3s"));
    AlwaysAssertExit (testMUS (q, "12:00:00", 15*12));
    AlwaysAssertExit (! testMUS (q, "5h20:"));
    AlwaysAssertExit (! testMUS (q, "5:20m"));
    AlwaysAssertExit (! testMUS (q, "5:20,"));
    AlwaysAssertExit (! testMUS (q, "5:20,", False));
    AlwaysAssertExit (! testMUS (q, "20:19.378"));
    AlwaysAssertExit (testMUS (q, "13:45:32.8187", 15*(13+45./60+32.8187/3600)));
    AlwaysAssertExit (! testMUS (q, "13:45:32.8187Z"));
    AlwaysAssertExit (! testMUS (q, "13:45:32.8187+1:0"));
    AlwaysAssertExit (testMUS (q, "13:45:32.8187Z",
                               15*(13+45./60+32.8187/3600), False));
    AlwaysAssertExit (testMUS (q, "13:45:32.8187+1:0",
                               15*(13+45./60+32.8187/3600), False));
    AlwaysAssertExit (! testMUS (q, "13:45Z"));
    AlwaysAssertExit (! testMUS (q, "13:45+1:0"));
    AlwaysAssertExit (testMUS (q, "13:45Z", 15*(13+45./60), False));
    AlwaysAssertExit (testMUS (q, "13:45+1:0", 15*(13+45./60), False));
    AlwaysAssertExit (testMUS (q, ".20.", 20./60));
    AlwaysAssertExit (! testMUS (q, ".20"));
    AlwaysAssertExit (testMUS (q, "..20.", 20./3600));
    AlwaysAssertExit (! testMUS (q, "1:.", False));
    AlwaysAssertExit (testMUS (q, "1:[", 15*1, False));
    AlwaysAssertExit (testMUS (q, "1d", 1));
    AlwaysAssertExit (testMUS (q, "1d0", 1));
    AlwaysAssertExit (testMUS (q, "1d0[", 1, False));
    AlwaysAssertExit (! testMUS (q, "1h0[", 1, False));
    AlwaysAssertExit (! testMUS (q, "1dm", 0, False));
    AlwaysAssertExit (! testMUS (q, "1d1", 0, False));
    AlwaysAssertExit (! testMUS (q, "1d0m["));
    AlwaysAssertExit (testMUS (q, "1d0m[", 1, False));
    AlwaysAssertExit (testMUS (q, "1d1m5.7[", 1+1./60+5.7/3600, False));
  } catch (AipsError& x) {
    cout << "Unexpected exception: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;
}
