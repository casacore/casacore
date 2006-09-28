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
#include <ms/MeasurementSets/MSSelectionError.h>

namespace casa { //# NAMESPACE CASA - BEGIN

  MSUvDistParse* MSUvDistParse::thisMSUParser = 0x0; // Global pointer to the parser object
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
						  const Double& endUV, 
						  const String& unit,
						  Bool doSlow)
{
  Bool distanceUnit = False;
  Bool wavelenthUnit = False;
  Double startPoint;
  Double endPoint;
  // Column accessors
  ROMSMainColumns msMainCol(*ms());
  ROMSSpWindowColumns msSpwCol(ms()->spectralWindow());
  ROMSDataDescColumns msDataDescSubTable(ms()->dataDescription());
  String units(unit);
  units.downcase();

  if(units == "km") // Kilo meter
    {
      startPoint = startUV * 1000;
      endPoint = endUV * 1000;
      distanceUnit = True;
    } 
  else if((units == "m")) // Meter
    {
      startPoint = startUV;
      endPoint = endUV;
      distanceUnit = True;
    } 
  else if(units == "ml") // Mega Lambda
    {
      startPoint = startUV * 1000000;
      endPoint = endUV * 1000000;
      wavelenthUnit = True;
    } 
  else if(units == "kl") // Kilo lambda
    {
      startPoint = startUV * 1000;
      endPoint = endUV * 1000;
      wavelenthUnit = True;
    } 
  else if(units == "l") // Lambda
    {
      startPoint = startUV;
      endPoint = endUV;
      wavelenthUnit = True;
    } 
  else 
    {
      String Mesg="Unrecognized units " + 
	units + 
	" found.  Possible units are [Kk][Ll] for wavelengths or [Kk][Mm] for distance.";
      throw(MSSelectionUvDistParseError(Mesg));
    }

  TableExprNode condition;
  if (!doSlow)
    {
      //
      // This version of TEN based query is about 60X faster than the
      // slower code below
      //
      Int nSpwRows, nDDIDRows;
      //
      // In the following, sumsquare TaQL function should be used.  But I
      // (SB) could not figure out how to use it!
      //
      TableExprNode uvwDist = sumSquare(ms()->col(MS::columnName(MS::UVW)));
      //
      // Build a map from DDID (which is a MainTable column) to SpwID
      // (which then indexes into the SpectralWindow sub-table from
      // where the ref. freq. info. is picked up.
      //
      Vector<Int> mapDDID2SpwID;
      nSpwRows = msSpwCol.nrow();
      nDDIDRows = msDataDescSubTable.nrow();
      mapDDID2SpwID.resize(nDDIDRows);

      for(Int i=0;i<nDDIDRows;i++)
	mapDDID2SpwID(i) = msDataDescSubTable.spectralWindowId()(i);
      //
      // If the limits were supplied in wavelength units, convert the
      // limits to meters for all available spectral window(s) and an
      // OR'ed TEN for each Spw.
      //
      if (wavelenthUnit)
	{
	  Float scaledStartPoint, scaledEndPoint;
	  const String DATA_DESC_ID = MS::columnName(MS::DATA_DESC_ID);
	  for(Int i=0;i<nDDIDRows;i++)
	    {
	      Int SpwID=mapDDID2SpwID(i);
	      scaledStartPoint = startPoint * msSpwCol.refFrequency()(SpwID)/C::c;
	      scaledEndPoint = endPoint * msSpwCol.refFrequency()(SpwID)/C::c;
	      
	      if (condition.isNull())
		condition = ((ms()->col(DATA_DESC_ID)==i)  && 
			     (uvwDist >= scaledStartPoint) && 
			     (uvwDist <= scaledEndPoint));
	      else
		condition = condition || ((ms()->col(DATA_DESC_ID)==i)  && 
					  (uvwDist >= scaledStartPoint) && 
					  (uvwDist <= scaledEndPoint));
	    }
	}
      else
	condition = ((uvwDist >= startPoint) && (uvwDist <= endPoint));
    }
  else
    {
      //
      // The earlier, (much) less efficient code.  Its here for
      // testing - should ultimately be removed.
      //
      // Loop over all rows in the MS
      //
      Vector<Int> rowsel;

      Int nRowSel = 0;
      if(wavelenthUnit) {
	for (uInt row=0; row<ms()->nrow(); row++) {
	  Int ddid = msMainCol.dataDescId()(row);
	  Int spwid = msDataDescSubTable.spectralWindowId()(ddid);
	  Double refFreq = msSpwCol.refFrequency()(spwid);
	  Vector<Double> uvw = msMainCol.uvw()(row);
	  Double uvDist = sqrt(uvw(0)*uvw(0) + uvw(1)*uvw(1) + uvw(2)*uvw(2))*refFreq/C::c;
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
	  Double uvDist = sqrt(uvw(0)*uvw(0) + uvw(1)*uvw(1) + uvw(2)*uvw(2));
	  if ((startPoint <= uvDist) && (uvDist <= endPoint)) {
	    nRowSel++;
	    rowsel.resize(nRowSel, True);
	    rowsel(nRowSel-1) = row;
	  };
	};
	if(nRowSel == 0)
	  rowsel.resize(nRowSel, True);
      }
      condition = (ms()->nodeRownr().in(rowsel));
    }

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
