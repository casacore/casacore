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

#include <casacore/ms/MSSel/MSUvDistParse.h>
#include <casacore/ms/MeasurementSets/MSColumns.h>
#include <casacore/ms/MSSel/MSSelectionError.h>
#include <casacore/tables/TaQL/RecordGram.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  MSUvDistParse* MSUvDistParse::thisMSUParser = 0x0; // Global pointer to the parser object
  TableExprNode* MSUvDistParse::node_p = 0x0;
  Matrix<double> MSUvDistParse::selectedUV_p(2,0);
  Vector<bool> MSUvDistParse::meterUnits_p(0,false);

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

const TableExprNode *MSUvDistParse::selectUVRange(const double& startUV,
						  const double& endUV, 
						  const String& unit,
						  bool doSlow)
{
  bool wavelengthUnit=false, distanceUnit=false;
  double startPoint;
  double endPoint;
  // Column accessors
  MSMainColumns msMainCol(*ms());
  MSSpWindowColumns msSpwCol(ms()->spectralWindow());
  MSDataDescColumns msDataDescSubTable(ms()->dataDescription());
  String units(unit);
  units.downcase();

  if(units == "km") // Kilo meter
    {
      startPoint = startUV * 1000;
      endPoint = endUV * 1000;
      distanceUnit = true;
    } 
  else if((units == "m")) // Meter
    {
      startPoint = startUV;
      endPoint = endUV;
      distanceUnit = true;
    } 
  else if(units == "mlambda") // Mega Lambda
    {
      startPoint = startUV * 1000000;
      endPoint = endUV * 1000000;
      wavelengthUnit = true;
    } 
  else if(units == "klambda") // Kilo lambda
    {
      startPoint = startUV * 1000;
      endPoint = endUV * 1000;
      wavelengthUnit = true;
    } 
  else if(units == "lambda") // Lambda
    {
      startPoint = startUV;
      endPoint = endUV;
      wavelengthUnit = true;
    } 
  else 
    {
      String Mesg="Unrecognized units " + 
	units + 
	" found.  Possible (case insensitive) units are [KM]LAMBDA for wavelengths or [K]M for distance.";
      throw(MSSelectionUvDistParseError(Mesg));
    }
  
  accumulateUVList(startPoint, endPoint, wavelengthUnit, distanceUnit);

  TableExprNode condition;
  if (!doSlow)
    {
      //
      // This version of TEN based query is about 60X faster than the
      // slower code below
      //
      int32_t nDDIDRows;
      //      TableExprNode uvwDist = sqrt(sumSquare(ms()->col(MS::columnName(MS::UVW))));

      //
      // This computes only the 2D uv-distance (projection of the
      // baseline on the uv-plane).  It's certainly more expensive to
      // compute in TaQL and probably scientifically incorrect too.
      //
      // String colName = MS::columnName(MS::UVW);
      // TableExprNode uvw = ms()->col(colName);
      // TableExprNodeSet su = TableExprNodeSet(IPosition(1,0));
      // TableExprNodeSet sv = TableExprNodeSet(IPosition(1,1));
      // TableExprNode uvwDist = (uvw(su)*uvw(su) + uvw(sv)*uvw(sv));

      //
      // Turns out that the above style of building the TEN is about
      // 2x slower than the one below.  It is also simpler to build a
      // const TEN via the parser below.
      //
      TableExprNode uvwDist = RecordGram::parse(*ms_p,"SQUARE(UVW[1]) + SQUARE(UVW[2])");
      //
      // Build a map from DDID (which is a MainTable column) to SpwID
      // (which then indexes into the SpectralWindow sub-table from
      // where the ref. freq. info. is picked up.
      //
      Vector<int32_t> mapDDID2SpwID;
      nDDIDRows = msDataDescSubTable.nrow();
      mapDDID2SpwID.resize(nDDIDRows);

      for(int32_t i=0;i<nDDIDRows;i++)
	mapDDID2SpwID(i) = msDataDescSubTable.spectralWindowId()(i);
      //
      // If the limits were supplied in wavelength units, convert the
      // limits to meters for all available spectral window(s) and an
      // OR'ed TEN for each Spw.
      //
      if (wavelengthUnit)
	{
	  float scaledStartPoint, scaledEndPoint;
	  const String DATA_DESC_ID = MS::columnName(MS::DATA_DESC_ID);
	  for(int32_t i=0;i<nDDIDRows;i++)
	    {
	      int32_t SpwID=mapDDID2SpwID(i);
	      double Lambda = C::c/msSpwCol.refFrequency()(SpwID);
	      scaledStartPoint = startPoint * Lambda;
	      scaledEndPoint = endPoint * Lambda;

	      scaledStartPoint *= scaledStartPoint;
	      scaledEndPoint *= scaledEndPoint;
	      TableExprNode pickUVWDist= ((ms()->col(DATA_DESC_ID)==i)  && 
					  ((uvwDist >= scaledStartPoint) && 
					   (uvwDist <= scaledEndPoint)));
	      if (condition.isNull()) condition = pickUVWDist;
	      else                    condition = condition || pickUVWDist;
	    }
	}
      else
	{
	  startPoint *= startPoint;
	  endPoint *= endPoint;
	  if (condition.isNull())
	    condition = ((uvwDist >= startPoint) && (uvwDist <= endPoint));
	  else 
	    condition = condition || ((uvwDist >= startPoint) && (uvwDist <= endPoint));
	}
    }
  else
    {
      //
      // The earlier, (much) less efficient code.  Its here for
      // testing - should ultimately be removed.
      //
      // Loop over all rows in the MS
      //
      Vector<int32_t> rowsel;

      int32_t nRowSel = 0;
      if(wavelengthUnit) {
	for (uint32_t row=0; row<ms()->nrow(); row++) {
	  int32_t ddid = msMainCol.dataDescId()(row);
	  int32_t spwid = msDataDescSubTable.spectralWindowId()(ddid);
	  double refFreq = msSpwCol.refFrequency()(spwid);
	  Vector<double> uvw = msMainCol.uvw()(row);
	  double uvDist = sqrt(uvw(0)*uvw(0) + uvw(1)*uvw(1) + uvw(2)*uvw(2))*refFreq/C::c;
	  if ((startPoint <= uvDist) && (uvDist <= endPoint)) {
	    nRowSel++;
	    rowsel.resize(nRowSel, true);
	    rowsel(nRowSel-1) = row;
	  }
	}
	if(nRowSel == 0)
	  rowsel.resize(nRowSel, true);
      }
      
      if(distanceUnit) {
	for (uint32_t row=0; row<ms()->nrow(); row++) {
	  Vector<double> uvw = msMainCol.uvw()(row);
	  double uvDist = sqrt(uvw(0)*uvw(0) + uvw(1)*uvw(1) + uvw(2)*uvw(2));
	  if ((startPoint <= uvDist) && (uvDist <= endPoint)) {
	    nRowSel++;
	    rowsel.resize(nRowSel, true);
	    rowsel(nRowSel-1) = row;
	  }
	}
	if(nRowSel == 0)
	  rowsel.resize(nRowSel, true);
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

void MSUvDistParse::accumulateUVList(const double r0, const double r1,
				     const bool wavelengthUnit, 
				     const bool)
{
  int32_t n0=selectedUV_p.shape()(1);
  IPosition newShape(selectedUV_p.shape());
  newShape(1)++;
  selectedUV_p.resize(newShape,true);
  meterUnits_p.resize(newShape(1),true);
  selectedUV_p(0,n0) = r0;
  selectedUV_p(1,n0) = r1;
  meterUnits_p(n0)=true;
  if (wavelengthUnit) meterUnits_p(n0)=false;
}
} //# NAMESPACE CASACORE - END
