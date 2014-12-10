//# Quality.cc: Quality parameter definitions for interface to table data
//# Copyright (C) 1994,1995,1997,2000,2001
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
#include <casacore/measures/Measures/Quality.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

Quality::QualityTypes Quality::type(Int qualityNumber)
{
	QualityTypes val = Undefined;
   if (qualityNumber > Undefined && qualityNumber < NumberOfTypes) {
      val = QualityTypes(qualityNumber);
   }
   return val;
}

Quality::QualityTypes Quality::type(const String &qualityName)
{
   QualityTypes val = Undefined;
   String name = qualityName;
   name.upcase();
   if (name == "DATA")    val = DATA;
   else if (name == "ERROR")   val = ERROR;
   return val;
}

String Quality::name(QualityTypes qualityType)
{
   String qualityName;
   switch (qualityType) {
   case Undefined: qualityName="??"; break;
   case DATA:      qualityName="DATA"; break;
   case ERROR:     qualityName="ERROR"; break;
   }
   return qualityName;
}

Vector<String> Quality::allNames(Bool includeUndefined) {
	uInt size = includeUndefined ? NumberOfTypes : NumberOfTypes - 1;
	Vector<String> names(size);
	uInt idx = 0;
	for (uInt i=0; i<NumberOfTypes; i++) {
		if (includeUndefined || (QualityTypes)i != Undefined) {
			names[idx] = name((QualityTypes)i);
			idx++;
		}
	}
	return names;
}
} //# NAMESPACE CASACORE - END

