//# TaQLShow.h: Class to show various TaQL-related info
//# Copyright (C) 2016
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

#ifndef TABLES_TAQLSHOW_H
#define TABLES_TAQLSHOW_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/TaQL/TaQLNodeRep.h>
#include <casacore/casa/Quanta/UnitName.h>
#include <casacore/casa/Arrays/Vector.h>
#include <map>
#include <ostream>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Class to show various TaQL-related info
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="tTaQLNode">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> <linkto group=TableGram.h#TableGramFunctions>TableGram</linkto>
//   <li> Note 199 describing
//        <a href="../notes/199.html">
//        TaQL</a>
// </prerequisite>

// <synopsis>
// TaQLShow::getInfo is called by the TaQL help (or show) command.
// It returns a string (with newlines) containing the requested help info.
//
// Note that a command like 'help func mscal' command calls the getInfo function in
// the dynamically loaded mscal UDF library.
// </synopsis> 

// <motivation>
// It is nice if the user can get online TaQL help.
// </motivation>

class TaQLShow
{
public:
  static String getInfo (const Vector<String>& parts,
                         const TaQLStyle& style);
  static String showTable (const Vector<String>& parts,
                           const TaQLStyle& style);
  static String showCommand (const String& cmd);
  static String showFuncs (const String& type,
                         const Vector<String>& parts,
                         const TaQLStyle& style);
  static void showUnitKind (std::ostream& os, const UnitVal& kind,
                            const std::map<String, UnitName>& units);
  static String showUnits (const String& type);
  static String showMeasTypes (const String& type);
};


} //# NAMESPACE CASACORE - END

#endif
