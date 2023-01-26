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

#include <casacore/tables/TaQL/TaQLStyle.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/Utilities/Assert.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

TaQLStyle::TaQLStyle (uint32_t origin)
  : itsOrigin    (origin),
    itsEndExcl   (false),
    itsCOrder    (false),
    itsDoTiming  (false),
    itsDoTracing (false)
{
  // Define mscal as a synonym for derivedmscal.
  defineSynonym ("mscal", "derivedmscal");
  defineSynonym ("py", "pytaql");
}

void TaQLStyle::set (const String& value)
{
  String val = upcase(value);
  if (val == "GLISH") {
    itsOrigin  = 1;
    itsEndExcl = false;
    itsCOrder  = false;
  } else if (val == "PYTHON") {
    itsOrigin  = 0;
    itsEndExcl = true;
    itsCOrder  = true;
  } else if (val == "BASE1") {
    itsOrigin  = 1;
  } else if (val == "BASE0") {
    itsOrigin  = 0;
  } else if (val == "FORTRANORDER") {
    itsCOrder  = false;
  } else if (val == "CORDER") {
    itsCOrder  = true;
  } else if (val == "ENDINCL") {
    itsEndExcl = false;
  } else if (val == "ENDEXCL") {
    itsEndExcl = true;
  } else if (val == "TIME") {
    itsDoTiming = true;
  } else if (val == "NOTIME") {
    itsDoTiming = false;
  } else if (val == "TRACE") {
    itsDoTracing = true;
  } else if (val == "NOTRACE") {
    itsDoTracing = false;
  } else {
    throw TableError(value + " is an invalid TaQL STYLE value");
  }
}

void TaQLStyle::reset()
{
  set ("GLISH");
  itsDoTiming  = false;
  itsDoTracing = false;
}

void TaQLStyle::defineSynonym (const String& synonym, const String& udfLibName)
{
  itsUDFLibNameMap[downcase(synonym)] = udfLibName;
}

void TaQLStyle::defineSynonym (const String& command)
{
  String cmd(command);  // to make it non-const
  String::size_type pos = cmd.find ('=');
  AlwaysAssert (pos != String::npos, AipsError);
  defineSynonym (trim(String(cmd.before(pos))),
                 trim(String(cmd.after(pos))));
}

String TaQLStyle::findSynonym (const String& synonym) const
{
  map<String,String>::const_iterator it = itsUDFLibNameMap.find (synonym);
  if (it == itsUDFLibNameMap.end()) {
    return synonym;
  }
  return it->second;
}


} //# NAMESPACE CASACORE - END
