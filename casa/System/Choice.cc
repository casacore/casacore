//# Choice.cc: Ask a choice to the user
//# Copyright (C) 2004
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
//#
//# $Id$

#include <casacore/casa/System/Choice.h>
#include <casacore/casa/Arrays/Vector.h>
#include <iostream>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Default is no choice function, thus return first choice.
Choice::ChoiceFunc* Choice::theirChoiceFunc = 0;


String Choice::choice (const String& descriptiveText,
		       const Vector<String>& choices)
{
  if (choices.nelements() == 0) {
    return "";
  }
  if (theirChoiceFunc == 0) {
    return choices[0];
  }
  return theirChoiceFunc (descriptiveText, choices);
}

Choice::ChoiceFunc* Choice::setChoiceFunc (Choice::ChoiceFunc* func)
{
  Choice::ChoiceFunc* tmp = theirChoiceFunc;
  theirChoiceFunc = func;
  return tmp;
}

String Choice::ostreamChoice (std::ostream& os,
			      const String& descriptiveText,
			      const Vector<String>& choices)
{
  if (choices.nelements() == 0) {
    return "";
  }
  char answer[256];
  while (True) {
    os << descriptiveText << " ([" << choices(0) << ']';
    for (uInt i=1; i<choices.nelements(); i++) {
      os << ',' << choices[i];
    }
    os << "): ";
    cin.getline (answer, sizeof(answer));
    String str(answer);
    if (str.size() == 0) {
      return choices[0];
    }
    for (uInt i=0; i<choices.nelements(); i++) {
      if (str == choices[i]) {
	return choices[i];
      }
    }
    os << "'" << str << "' is an invalid answer; retry" << std::endl;
  }
  return "";
}

} //# NAMESPACE CASACORE - END

