//# RegexBase.cc: Abstract interface class to regular expressions for String
//# Copyright (C) 2001
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This library is free software; you can redistribute it and/or modify it
//# under the terms of the GNU Library General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or (at your
//# option) any later version.
//#
//# This library is distributed in the hope that it will be useful, but WITHOU$
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

//# Includes
#include <casacore/casa/BasicSL/RegexBase.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Destructor
RegexBase::~RegexBase() {}

//# Member functions
String::size_type RegexBase::rfind(const Char *s, String::size_type len,
				   Int &matchlen,
				   String::size_type pos) const {
  if (len == 0) return String::npos;
  String::size_type xpos = len-1;
  if (xpos > pos) xpos = pos;
  for (++xpos; xpos-- > 0; ) {
    if (find(s, len, matchlen, xpos)) return xpos;
  }
  return String::npos;
}

String::size_type RegexBase::search(const Char *s, String::size_type len,
				    Int &matchlen, Int pos) const {
  if (pos < 0) return rfind(s, len, matchlen, Int(len) + pos);
  return find(s, len, matchlen, pos);
}

String::size_type RegexBase::match(const Char *s,
				   String::size_type len,
				   String::size_type pos) const {
  Int matchlen;
  if (find(s, len, matchlen, pos) == pos && matchlen == Int(len-pos)) {
    return len-pos;
  }
  return String::npos;
}

} //# NAMESPACE CASACORE - END

