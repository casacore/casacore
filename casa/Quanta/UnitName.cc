//# UnitName.cc: defines a tagged unit definition
//# Copyright (C) 1994,1995,1996,1997,1998,2001
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

//# Includes

#include <casacore/casa/Quanta/UnitName.h>
#include <casacore/casa/iomanip.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

UnitName::UnitName() :
    basicKind(),
    basicTag(""),
    basicName("") {}

UnitName::UnitName(const UnitName &other) :
    basicKind(other.basicKind),
    basicTag(other.basicTag),
    basicName(other.basicName) {}

UnitName::UnitName(const String &tag, const UnitVal &kind,
		     const String &name) :
    basicKind(kind),
    basicTag(tag),
    basicName(name) {}

UnitName::UnitName(const Unit &tag, const String &name) 
: basicKind(),
  basicTag(),
  basicName(name) {
      basicKind = tag.getValue();
      basicTag = tag.getName();
}

UnitName::~UnitName() {}

UnitName &UnitName::operator=(const UnitName &other) {
    if (this != &other) {
	basicKind=other.basicKind;
	basicTag=other.basicTag;
	basicName=other.basicName;
    }
    return *this;
}

const UnitVal &UnitName::getVal() const {
    return(basicKind);
}

const String &UnitName::getName() const {
    return (basicTag);
}

ostream& operator<< (ostream &os, const UnitName &name) {
    static String FillString("                                ");
    Int i=os.precision();
    os << name.basicTag <<
	(FillString)(0,10 - name.basicTag.length()) <<
	    "(" <<
		name.basicName << ")" <<
		    (FillString)(0,27 - name.basicName.length()) <<
			setprecision(12) <<
			    name.basicKind  <<
				setprecision(i);
    return os;
}

} //# NAMESPACE CASACORE - END

