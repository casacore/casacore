//# tMVTime.cc: test program for MVTime class
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
//# $Id$

#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Quanta/MVTime.h>
#include <casacore/casa/BasicMath/Math.h>

#include <casacore/casa/namespace.h>

void showTime (MVTime time, uInt format, uInt prec)
{
  cout << MVTime::Format(format,prec) << time << endl;
}

void checkTime (const String& str, uInt yy, uInt mm, uInt dd,
                uInt h, uInt m, uInt s, double ss, Bool chk=True)
{
  Quantity q;
  AlwaysAssertExit (MVTime::read(q, str, chk));
  MVTime mvtm(q);
  Time tm = mvtm.getTime();
  AlwaysAssertExit (mvtm.year() == Int(yy));
  AlwaysAssertExit (mvtm.month() == mm);
  AlwaysAssertExit (mvtm.monthday() == dd);
  AlwaysAssertExit (tm.year() == yy);
  AlwaysAssertExit (tm.month() == mm);
  AlwaysAssertExit (tm.dayOfMonth() == dd);
  AlwaysAssertExit (tm.hours() == h);
  AlwaysAssertExit (tm.minutes() == m);
  AlwaysAssertExit (tm.seconds() == s);
  AlwaysAssertExit (nearAbs(tm.dseconds(), s+ss, 1e-4));
}

void checkExcp (const String& str)
{
  Quantity q;
  Bool ok = False;
  try {
    MVTime::read(q, str, True, True);
  } catch (const AipsError& x) {
    cout << "Expected exception: " << x.what() << endl;
    ok = True;
  }
  AlwaysAssertExit (ok);
}

int main ()
{
  try {
    Quantity q;
    AlwaysAssertExit (MVTime::read (q, ""));
    AlwaysAssertExit (! MVTime::read (q, "20Nov96-5h20"));
    AlwaysAssertExit (! MVTime::read (q, "20Nov96-5hm"));
    AlwaysAssertExit (! MVTime::read (q, "1996-11-20T5.20"));
    AlwaysAssertExit (! MVTime::read (q, "1996-11-20T5:20,", True));
    AlwaysAssertExit (MVTime::read (q, "today"));
    AlwaysAssertExit (MVTime::read (q, "today/12:00:00"));
    AlwaysAssertExit (MVTime::read (q, "today 12:00:00"));
    AlwaysAssertExit (MVTime::read (q, "today-12:00:00"));
    AlwaysAssertExit (MVTime::read (q, "ToDay"));
    AlwaysAssertExit (! MVTime::read (q, "1996-11-20T5:20,"));
    AlwaysAssertExit (MVTime::read (q, "1996-11-20T5:20,", False));

    checkTime (" 1996-11-20T5:20 ", 1996, 11, 20, 5, 20, 0, 0);
    checkTime ("1996-11-20T..20.", 1996, 11, 20, 0, 0, 1, 5./15);  // time in deg!!
    checkTime ("12-Mar-2011 12:00:00", 2011, 3, 12, 12, 0, 0, 0);
    checkTime ("12-Mar-2011    12:00:00", 2011, 3, 12, 12, 0, 0, 0);
    checkTime ("1996/11/20", 1996, 11, 20 , 0, 0, 0, 0);
    checkTime ("1996/11/20/5:20", 1996, 11, 20, 5, 20, 0, 0);
    checkTime ("20Nov96-5h20m", 1996, 11, 20, 5, 20, 0, 0);
    checkTime ("1996-11-20T5:20", 1996, 11, 20, 5, 20, 0, 0);
    checkTime ("1996-11-20T5:20:", 1996, 11, 20, 5, 20, 0, 0);
    checkTime ("1996-11-20T5:20:19.378Z", 1996, 11, 20, 5, 20, 19, 0.378);
    checkTime ("1996-11-20T5:20Z", 1996, 11, 20, 5, 20, 0, 0);
    checkTime ("2017-Jul-1/11:57:0.0", 2017, 7, 1, 11, 57, 0, 0);
    AlwaysAssertExit (!MVTime::read (q, "2017-Jul-1x/11:57:0.0"));
    checkTime ("2017-Jul-1x/11:57:0.0", 2017, 7, 1, 0, 0, 0, 0, False);
    checkExcp ("2017-Jul-1x/11:57:0.0");
    checkExcp ("x");
    checkExcp ("2017-Jul-1/11:57:0.0  x");

    AlwaysAssertExit (MVTime::read (q, "25-Jan-2012/13:45:32.8187"));
    MVTime time(q);
    showTime (q, MVTime::ANGLE, 9);
    showTime (q, MVTime::ANGLE+MVTime::NO_D, 9);
    showTime (q, MVTime::ANGLE+MVTime::NO_DM, 9);
    showTime (q, MVTime::ANGLE+MVTime::ALPHA, 9);
    showTime (q, MVTime::ANGLE+MVTime::NO_D+MVTime::ALPHA, 9);
    showTime (q, MVTime::ANGLE+MVTime::NO_DM+MVTime::ALPHA, 9);
    showTime (q, MVTime::ANGLE+MVTime::CLEAN, 9);
    showTime (q, MVTime::ANGLE+MVTime::NO_D+MVTime::CLEAN, 9);
    showTime (q, MVTime::ANGLE+MVTime::NO_DM+MVTime::CLEAN, 9);
    showTime (q, MVTime::ANGLE+MVTime::ALPHA+MVTime::CLEAN, 9);
    showTime (q, MVTime::ANGLE+MVTime::NO_D+MVTime::ALPHA+MVTime::CLEAN, 9);
    showTime (q, MVTime::ANGLE+MVTime::NO_DM+MVTime::ALPHA+MVTime::CLEAN, 9);
    showTime (q, MVTime::TIME, 9);
    showTime (q, MVTime::TIME+MVTime::NO_D, 9);
    showTime (q, MVTime::TIME+MVTime::NO_DM, 9);
    showTime (q, MVTime::TIME+MVTime::ALPHA, 9);
    showTime (q, MVTime::TIME+MVTime::NO_D+MVTime::ALPHA, 9);
    showTime (q, MVTime::TIME+MVTime::NO_DM+MVTime::ALPHA, 9);
    showTime (q, MVTime::TIME+MVTime::CLEAN, 9);
    showTime (q, MVTime::TIME+MVTime::NO_D+MVTime::CLEAN, 9);
    showTime (q, MVTime::TIME+MVTime::NO_DM+MVTime::CLEAN, 9);
    showTime (q, MVTime::TIME+MVTime::ALPHA+MVTime::CLEAN, 9);
    showTime (q, MVTime::TIME+MVTime::NO_D+MVTime::ALPHA+MVTime::CLEAN, 9);
    showTime (q, MVTime::TIME+MVTime::NO_DM+MVTime::ALPHA+MVTime::CLEAN, 9);
    showTime (q, MVTime::BOOST, 0);
    showTime (q, MVTime::FITS, 10);
    showTime (q, MVTime::ISO, 10);
    showTime (q, MVTime::YMD, 5);
    showTime (q, MVTime::DMY, 4);
    showTime (q, MVTime::DMY+MVTime::CLEAN, 4);
    showTime (q, MVTime::DAY, 9);
    showTime (q, MVTime::DAY+MVTime::NO_TIME, 9);
    showTime (q, MVTime::DAY+MVTime::DMY+MVTime::USE_SPACE, 9);
    showTime (q, MVTime::MJD, 9);
    showTime (q, MVTime::NO_TIME+MVTime::MJD, 9);
  } catch (std::exception& x) {
    cout << "Unexpected exception: " << x.what() << endl;
    return 1;
  } 
  return 0;
}
