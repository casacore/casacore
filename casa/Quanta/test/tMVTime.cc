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

#include <casa/aips.h>
#include <casa/Utilities/Assert.h>
#include <casa/Exceptions/Error.h>
#include <casa/Quanta/MVTime.h>

#include <casa/namespace.h>

int main ()
{
  try {
    Quantity q;
    AlwaysAssertExit (MVTime::read (q, "today"));
    AlwaysAssertExit (MVTime::read (q, "today/12:00:00"));
    AlwaysAssertExit (MVTime::read (q, "today 12:00:00"));
    AlwaysAssertExit (MVTime::read (q, "today-12:00:00"));
    AlwaysAssertExit (MVTime::read (q, "12-Mar-2011 12:00:00"));
    AlwaysAssertExit (MVTime::read (q, "12-Mar-2011    12:00:00"));
    AlwaysAssertExit (MVTime::read (q, "ToDay"));
    AlwaysAssertExit (MVTime::read (q, "1996/11/20"));
    AlwaysAssertExit (MVTime::read (q, "1996/11/20/5:20"));
    AlwaysAssertExit (MVTime::read (q, "20Nov96-5h20m"));
    AlwaysAssertExit (MVTime::read (q, "1996-11-20T5:20"));
  } catch (AipsError& x) {
    cout << "Unexpected exception: " << x.getMesg() << endl;
    return 1;
  } 
  return 0;
}
