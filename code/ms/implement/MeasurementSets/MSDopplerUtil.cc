//# MSDopplerUtil.cc: Implementation of MSDopplerUtil.h
//# Copyright (C) 1996,1997,1998,1999,2000
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
//# $Id: 
//----------------------------------------------------------------------------

#include <trial/MeasurementSets/MSDopplerUtil.h>
#include <aips/MeasurementSets/MSColumns.h>
#include <trial/MeasurementSets/MSSourceIndex.h>
#include <aips/Exceptions/Error.h>

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
};

//----------------------------------------------------------------------------

MSDopplerUtil::~MSDopplerUtil()
{
// Null default destructor
//
};

//----------------------------------------------------------------------------

Bool MSDopplerUtil::dopplerInfo (Vector<Double>& restFrequency,
				 const Int& spwid)
{
// Retrieve a list of all rest frequencies used in Doppler
// tracking of the specified spectral window id.
// Output:
//    restFrequency    Vector<Double>     List of rest frequencies
//    dopplerInfo      Bool               True if Doppler info. found
//
  // Initialization
  restFrequency.resize();
  Int nRestFreq = 0;
  Bool found = False;

  // Accessor for the MS columns and sub-tables
  MSColumns msc (ms_p);
  // Retrieve the doppler id.
  if (!ms_p.doppler().isNull()) {
    Int dopId = msc.spectralWindow().dopplerId()(spwid);
    if (dopId >= 0) {
      // Find all matching DOPPLER sub-table rows for this DOPPLER_ID
      for (uInt idoprow=0; idoprow<msc.doppler().nrow(); idoprow++) {
	if (msc.doppler().dopplerId()(idoprow) == dopId) {
	  // Find the rest frequency information in the SOURCE subtable
	  Int srcId = msc.doppler().sourceId()(idoprow);
	  Int transId = msc.doppler().transitionId()(idoprow);
	  if (!ms_p.source().isNull()) {
	    // Use indexed access to the SOURCE sub-table
	    MSSourceIndex sourceIndex (ms_p.source());
	    sourceIndex.sourceId() = srcId;
	    sourceIndex.spectralWindowId() = spwid;
	    Vector<uInt> rows = sourceIndex.getRowNumbers();
            for (uInt irow=0; irow<rows.nelements(); irow++) {
	      Vector<Double> restFrq = msc.source().restFrequency()(irow);
	      // Does this already exist in the output rest frequency array ?
	      Bool exists = False;
	      for (uInt k=0; k<restFrequency.nelements(); k++) {
		if (restFrq(transId)==restFrequency(k)) {
		  exists = True;
		};
	      };
	      if (!exists) {
		restFrequency.resize(restFrequency.nelements()+1, True);
		restFrequency(nRestFreq) = restFrq(transId);
		nRestFreq++;
		found = True;
	      };
	    }; // for (Int irow=0..)
	  }; // if (!ms_p.source().isNull())
	}; // if (msc.doppler().dopplerId()..)
      }; // for (Int idoprow=0;..)
    }; // if (dopId >=0)
  }; // if (!ms_p.doppler()..)

  return found;
};

//----------------------------------------------------------------------------

