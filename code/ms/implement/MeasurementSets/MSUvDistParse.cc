//# MSUvDistParse.cc: Classes to hold results from UV dist grammar parser
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

#include <ms/MeasurementSets/MSUvDistParse.h>
#include <ms/MeasurementSets/MSColumns.h>

namespace casa { //# NAMESPACE CASA - BEGIN

TableExprNode* MSUvDistParse::node_p = 0x0;

//# Constructor
MSUvDistParse::MSUvDistParse ()
: MSParse()
{
}

//# Constructor with given ms name.
MSUvDistParse::MSUvDistParse (const MeasurementSet* ms)
: MSParse(ms, "UvDist")
{
    if(node_p) delete node_p;
    node_p = new TableExprNode();
}

const TableExprNode *MSUvDistParse::selectUVRange(const Double& startUV,
				    const Double& endUV, const String& unit)
{
  Bool distanceUnit = False;
  Bool wavelenthUnit = False;
  Double startPoint;
  Double endPoint;
  // Column accessors
  ROMSMainColumns msMainCol(*ms());
  ROMSSpWindowColumns msSpwCol(ms()->spectralWindow());
  ROMSDataDescColumns msDataDescCol(ms()->dataDescription());
  
  if(unit.contains("km") || unit.contains("KM") ){
    startPoint = startUV * 1000;
    endPoint = endUV * 1000;
    distanceUnit = True;
  } else if(unit.contains("m") || unit.contains("M") ){
    startPoint = startUV;
    endPoint = endUV;
    distanceUnit = True;
  } else if(unit.contains("Ml") || unit.contains("ML") ){
    startPoint = startUV * 1000000;
    endPoint = endUV * 1000000;
    wavelenthUnit = True;
  } else if(unit.contains("kl") || unit.contains("KL")){
    startPoint = startUV * 1000;
    endPoint = endUV * 1000;
    wavelenthUnit = True;
  } else if(unit.contains("l") || unit.contains("L") ){
    startPoint = startUV;
    endPoint = endUV;
    wavelenthUnit = True;
  } else {
    cout << "please input correct units (km or m) for distance, (ml, kl, or l) for wave length " << endl;
  }
  
  // Loop over all rows in the MS
  Vector<Int> rowsel;
  Int nRowSel = 0;
  if(wavelenthUnit) {
    for (uInt row=0; row<ms()->nrow(); row++) {
      Int ddid = msMainCol.dataDescId()(row);
      Int spwid = msDataDescCol.spectralWindowId()(ddid);
      Double refFreq = msSpwCol.refFrequency()(spwid);
      Vector<Double> uvw = msMainCol.uvw()(row);
      Double uvDist = sqrt(uvw(0)*uvw(0) + uvw(1)*uvw(1)) * refFreq / C::c;
      if ((startPoint <= uvDist) && (uvDist <= endPoint)) {
	nRowSel++;
	rowsel.resize(nRowSel, True);
	rowsel(nRowSel-1) = row;
      };
    };
    if(nRowSel == 0)
      rowsel.resize(nRowSel, True);
  }

  if(distanceUnit) {
    for (uInt row=0; row<ms()->nrow(); row++) {
      Vector<Double> uvw = msMainCol.uvw()(row);
      Double uvDist = sqrt(uvw(0)*uvw(0) + uvw(1)*uvw(1));
      if ((startPoint <= uvDist) && (uvDist <= endPoint)) {
	nRowSel++;
	rowsel.resize(nRowSel, True);
	rowsel(nRowSel-1) = row;
      };
    };
    if(nRowSel == 0)
      rowsel.resize(nRowSel, True);
  }

  TableExprNode condition = (ms()->nodeRownr().in(rowsel));

  if(node_p->isNull())
    *node_p = condition;
  else
    *node_p = *node_p || condition;
  
  return node_p;
}

const TableExprNode* MSUvDistParse::node()
{
    return node_p;
}

} //# NAMESPACE CASA - END
