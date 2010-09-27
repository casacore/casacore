//# TaQLStyle.cc: Class with static members defining the TaQL style
//# Copyright (C) 2006
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

#include <tables/Tables/TaQLStyle.h>
#include <tables/Tables/TableError.h>

namespace casa { //# NAMESPACE CASA - BEGIN

void TaQLStyle::set (const String& value)
{
  String val = upcase(value);
  if (val == "GLISH") {
    itsOrigin  = 1;
    itsEndExcl = False;
    itsCOrder  = False;
  } else if (val == "PYTHON") {
    itsOrigin  = 0;
    itsEndExcl = True;
    itsCOrder  = True;
  } else if (val == "BASE1") {
    itsOrigin  = 1;
  } else if (val == "BASE0") {
    itsOrigin  = 0;
  } else if (val == "FORTRANORDER") {
    itsCOrder  = False;
  } else if (val == "CORDER") {
    itsCOrder  = True;
  } else if (val == "ENDINCL") {
    itsEndExcl = False;
  } else if (val == "ENDEXCL") {
    itsEndExcl = True;
  } else {
    throw TableError(value + " is an invalid TaQL STYLE value");
  }
}

void TaQLStyle::reset()
{
  set ("GLISH"); 
  itsDoTiming = False;
}

} //# NAMESPACE CASA - END
