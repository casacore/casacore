//# MSDopplerUtil.cc: Implementation of MSDopplerUtil.h
//# Copyright (C) 1996,1997,1998,1999,2000,2003
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
//----------------------------------------------------------------------------

#include <casacore/ms/MeasurementSets/MSDopplerUtil.h>
#include <casacore/ms/MeasurementSets/MSColumns.h>
#include <casacore/ms/MSSel/MSSourceIndex.h>
#include <casacore/casa/Exceptions/Error.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//----------------------------------------------------------------------------

MSDopplerUtil::MSDopplerUtil(const MeasurementSet& ms)
  : ms_p(ms)
{
// Construct from an existing MS
// Input:
//    ms                   const MeasurementSet&       Input MS
// Output to private data:
//    ms_p                 MeasurementSet              Private MS copy
//
}

//----------------------------------------------------------------------------

MSDopplerUtil::~MSDopplerUtil()
{
// Null default destructor
//
}

//----------------------------------------------------------------------------

bool MSDopplerUtil::dopplerInfo (Vector<double>& restFrequency,
				 int32_t spwId, int32_t fieldId)
{
// Retrieve a list of all rest frequencies used in Doppler
// tracking of the specified spectral window id.
// Output:
//    restFrequency    Vector<double>     List of rest frequencies
//    dopplerInfo      bool               true if Doppler info. found
//
  // Initialization
  restFrequency.resize();
  int32_t nRestFreq = 0;
  bool found = false;

  // Accessor for the MS columns and sub-tables
  MSColumns msc (ms_p);
  // Retrieve the doppler id & source id
  int32_t dopId = (msc.spectralWindow().dopplerId().isNull() ? 
               -1 : msc.spectralWindow().dopplerId()(spwId));
  int32_t srcId = msc.field().sourceId()(fieldId);
    // Use the doppler table if specified and it exists
  if (dopId >= 0 && (!ms_p.doppler().isNull())) {
    // Find the matching DOPPLER sub-table rows for this DOPPLER_ID
    for (uint32_t idoprow=0; idoprow<msc.doppler().nrow(); idoprow++) {
      if (msc.doppler().dopplerId()(idoprow) == dopId &&
          msc.doppler().sourceId()(idoprow)== srcId) {
        // Find the rest frequency information in the SOURCE subtable
        int32_t transId = msc.doppler().transitionId()(idoprow);

	// When loading g192_a.ms (from regression) into plotxy (probably a
	// wrong thing to do), transId is -1, which causes a segv further down
	// when transId is used as an index. Returning false here causes things
	// to die with an allocation error later on...
	if ( transId < 0 ) {
	  throw( AipsError("MSDopplerUtil::dopplerInfo(): invalid transition id") );
	}

        if (!ms_p.source().isNull()) {
	    // Use indexed access to the SOURCE sub-table
	    MSSourceIndex sourceIndex (ms_p.source());
	    sourceIndex.sourceId() = srcId;
	    sourceIndex.spectralWindowId() = spwId;
	    Vector<rownr_t> rows = sourceIndex.getRowNumbers();
	    for (rownr_t irow=0; irow<rows.nelements(); irow++) {
	      Vector<double> restFrq = msc.source().restFrequency()(irow);
	      if(restFrq.nelements() >0){
		// Does this already exist in the output rest frequency array ?
		bool exists = false;
		for (uint32_t k=0; k<restFrequency.nelements(); k++) {
		  if (restFrq(transId)==restFrequency(k)) {
		    exists = true;
		  }
		}
		if (!exists) {
		  restFrequency.resize(restFrequency.nelements()+1, true);
		  restFrequency(nRestFreq) = restFrq(transId);
		  nRestFreq++;
		  found = true;
		}
	      }
	    } // for (int32_t irow=0..)
        } // if (!ms_p.source().isNull())
      } // if (msc.doppler().dopplerId()..)
    } // for (int32_t idoprow=0;..)
  } else if (!ms_p.source().isNull()) {
    if((ms_p.source().nrow() > 0)){
      // use just the source table if it exists
      MSSourceIndex sourceIndex(ms_p.source());
      sourceIndex.sourceId()= msc.field().sourceId()(fieldId);
      sourceIndex.spectralWindowId()=spwId;
      Vector<rownr_t> rows = sourceIndex.getRowNumbers();
      if (!msc.source().restFrequency().isNull()){
	for (rownr_t irow=0; irow<rows.nelements(); irow++) {
	  if ( msc.source().restFrequency().isDefined(rows(irow))) {
	    Vector<double> restFrq = msc.source().restFrequency()(rows(irow));
	    // Does this already exist in the output rest frequency array ?
	    for (uint32_t transId=0; transId<restFrq.nelements(); transId++) {
	      bool exists = false;
	      for (uint32_t k=0; k<restFrequency.nelements(); k++) {
		if (restFrq(transId)==restFrequency(k)) {
		  exists = true;
		}
	      }
	      if (!exists) {
		restFrequency.resize(restFrequency.nelements()+1, true);
		restFrequency(nRestFreq) = restFrq(transId);
		nRestFreq++;
		found = true;
	      }
	    }
	  } 
	} // for (int32_t irow=0..)
      }   
    }
  }
  return found;
}

//----------------------------------------------------------------------------


} //# NAMESPACE CASACORE - END

