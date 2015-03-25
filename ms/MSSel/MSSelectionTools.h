//# MSSelectionTools.h: Classes to hold results from antenna grammar parser
//# Copyright (C) 1994,1995,1997,1998,1999,2000,2001,2003
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

#ifndef MS_MSSELECTIONTOOLS_H
#define MS_MSSELECTIONTOOLS_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/ms/MSSel/MSSelection.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  Vector<Int> set_intersection(const Vector<Int>& v1, const Vector<Int>& v2);
  Vector<Int> set_union(const Vector<Int>& v1, const Vector<Int>& v2);

  // Collective selection returning a selected MS.
  Bool mssSetData(const MeasurementSet& ms, 
		  MeasurementSet& selectedMS,
		  const String& outMSName="",
		  const String& timeExpr="",
		  const String& antennaExpr="",
		  const String& fieldExpr="",
		  const String& spwExpr="",
		  const String& uvDistExpr="",
		  const String& taQLExpr="",
		  const String& polnExpr="",
		  const String& scanExpr="",
		  const String& arrayExpr="",
		  const String& stateExpr="",
		  const String& obsExpr="",
		  MSSelection *mss=NULL
		  );
  // Collective selection also returning in-row (corr/chan) slices
  Bool mssSetData(const MeasurementSet& ms, 
		  MeasurementSet& selectedMS,
		  Vector<Vector<Slice> >& chanSlices,
		  Vector<Vector<Slice> >& corrSlices,
		  const String& outMSName="",
		  const String& timeExpr="",
		  const String& antennaExpr="",
		  const String& fieldExpr="",
		  const String& spwExpr="",
		  const String& uvDistExpr="",
		  const String& taQLExpr="",
		  const String& polnExpr="",
		  const String& scanExpr="",
		  const String& arrayExpr="",
		  const String& stateExpr="",
		  const String& obsExpr="",
		  const Int defaultChanStep=1,
		  MSSelection *mss=NULL
		  );
  
  Bool getSelectedTable(Table& selectedTab,     const Table& baseTab,
			TableExprNode& fullTEN,	const String& outName);

  Record mssSelectedIndices(MSSelection& mss, const MeasurementSet *ms);

  String stripWhite(const String& str, Bool onlyends=True);
  int tokenize(const String& str, const String& sep, Vector<String>& tokens,Bool upCase=False);
  Vector<String> &split(const String &s, char delim, Vector<String> &elems);
}

#endif
