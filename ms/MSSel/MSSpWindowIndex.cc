//# MSSpWindowIndex.cc: implementation of MSSpWindowIndex.h
//# Copyright (C) 2000,2001,2002
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

#include <casacore/ms/MSSel/MSSpWindowIndex.h>
#include <casacore/casa/Arrays/MaskedArray.h>
#include <casacore/casa/Arrays/ArrayMath.h>
#include <casacore/casa/Arrays/ArrayLogical.h>
#include <casacore/casa/Arrays/ArrayUtil.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//-------------------------------------------------------------------------

MSSpWindowIndex::MSSpWindowIndex(const MSSpectralWindow& spectralWindow)
  : msSpWindowCols_p(spectralWindow)
{ 
// Construct from an MS DATA_DESC subtable
// Input:
//    spectralWindow     const MSSpectralWindow&    Input MSSpectralWindow
//                                                  sub-table
// Output to private data:
//    msSpWindowCols_p   ROMSSpWindowColumns        MSSpWindow columns accessor
//    spWindowIds_p      Vector<Int>                Data desc id's
//    nrows_p            Int                        Number of rows
//
  // Generate an array of data desc id's, used in later queries
  nrows_p = msSpWindowCols_p.nrow();
  spWindowIds_p.resize(nrows_p);
  indgen(spWindowIds_p);
}

//-------------------------------------------------------------------------

Vector<Int> MSSpWindowIndex::matchFreqGrp(const Int& freqGrp)
{
// Match a frequency goup to a set of spectral window id's
// Input:
//    freqGrp             const Int&               Freq group to match
// Output:
//    matchFreqGrp        Vector<Int>              Matching spw. id.'s
//
  LogicalArray maskArray = 
    (msSpWindowCols_p.freqGroup().getColumn()==freqGrp &&
     !msSpWindowCols_p.flagRow().getColumn());
  MaskedArray<Int> maskSpWindowId(spWindowIds_p, maskArray);
  return maskSpWindowId.getCompressedArray();
} 

//-------------------------------------------------------------------------

Vector<Int> MSSpWindowIndex::matchFreqGrp(const Vector<Int>& freqGrps)
{
// Match a set of frequency groups to a set of spectral window id's
// Input:
//    freqGrps            const Vector<Int>&       Freq groups to match
// Output:
//    matchFreqGrp        Vector<Int>              Matching spw. id.'s
//
  Vector<Int> matchedSpWindowIds;
  // Match each spw id individually
  for (uInt freqgrp=0; freqgrp<freqGrps.nelements(); freqgrp++) {
    // Add to list of SpWindow id's
    Vector<Int> currentMatch = matchFreqGrp(freqGrps(freqgrp));
    if (currentMatch.nelements() > 0) {
      Vector<Int> temp(matchedSpWindowIds);
      matchedSpWindowIds.resize(matchedSpWindowIds.nelements() +
				currentMatch.nelements(), True);
      matchedSpWindowIds = concatenateArray(temp, currentMatch);
    }
  }
  return matchedSpWindowIds;
}

//-------------------------------------------------------------------------

Vector<Int> MSSpWindowIndex::matchFreqGrpName(const String& freqGrpName)
{
// Match a frequency goup name to a set of spectral window id's
// Input:
//    freqGrpName         const String&            Freq group name to match
// Output:
//    matchFreqGrpName    Vector<Int>              Matching spw. id.'s
//
  LogicalArray maskArray = 
    (msSpWindowCols_p.freqGroupName().getColumn()==freqGrpName &&
     !msSpWindowCols_p.flagRow().getColumn());
  MaskedArray<Int> maskSpWindowId(spWindowIds_p, maskArray);
  return maskSpWindowId.getCompressedArray();
} 

//-------------------------------------------------------------------------

Vector<Int> MSSpWindowIndex::matchFreq(const Vector<MFrequency>& chanFreq,
				       const Vector<MVFrequency>& chanWidth,
				       const Double& tol)
{
// Match a frequency axis sampling to a set of spectral window id's
// Input:
//    chanFreq        const Vector<MFrequency>&    Channel frequencies
//    chanWidth       const Vector<MVFrequency>&   Channel freq. width
//    tol             const Double&                Tolerance for frequency
//                                                 comparisons
// Output:
//    matchFreq       Vector<Int>                  Matching spw id.'s
//

  // Do the match per frequency channel on each row
  uInt nChan = std::min(chanFreq.nelements(), chanWidth.nelements());
  uInt nrows = msSpWindowCols_p.nrow();
  Vector<Bool> freqMatch(nrows, False);
  for (uInt row=0; row<nrows; row++) {
    Vector<MFrequency> rowChanFreq;
    msSpWindowCols_p.chanFreqMeas().get(row, rowChanFreq);
    Vector<Quantity> rowChanWidth;
    msSpWindowCols_p.chanWidthQuant().get(row, rowChanWidth);
    freqMatch(row) = (rowChanFreq.nelements() == nChan &&
		      rowChanWidth.nelements() == nChan);
    if (freqMatch(row)) {
      for (uInt chan=0; chan<nChan; chan++) {
	freqMatch(row) = (freqMatch(row) &&
			  chanFreq(chan).type() == rowChanFreq(chan).type() &&
			  chanFreq(chan).getValue().
			  nearAbs(rowChanFreq(chan).getValue(), tol) &&
			  chanWidth(chan).nearAbs(rowChanWidth(chan), tol));
      }
    }
  }

  // Return matching row numbers
  LogicalArray maskArray(freqMatch);
  MaskedArray<Int> maskSpwId(spWindowIds_p, maskArray);
  return maskSpwId.getCompressedArray();
}

//-------------------------------------------------------------------------

} //# NAMESPACE CASACORE - END
