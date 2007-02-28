//# MSFitsOutput.h:  Write a MeasurementSet to a random group uvfits file
//# Copyright (C) 1996,1997,1998,1999,2000,2001,2003
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify
//# it under the terms of the GNU General Public License as published by
//# the Free Software Foundation; either version 2 of the License, or
//# (at your option) any later version.
//#
//# This program is distributed in the hope that it will be useful,
//# but WITHOUT ANY WARRANTY; without even the implied warranty of
//# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//# GNU General Public License for more details.
//# 
//# You should have received a copy of the GNU General Public License
//# along with this program; if not, write to the Free Software
//# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA
//#
//# $Id$

#ifndef MS_MSFITSOUTPUT_H
#define MS_MSFITSOUTPUT_H

//# Includes
#include <casa/aips.h>

namespace casa { //# NAMESPACE CASA - BEGIN

//# Forward Declarations
class String;
class FitsOutput;
class MeasurementSet;
class Table;
template<class T> class Block;
template<class T> class Vector;


// <summary>
// Write a MeasurementSet to a random group uvfits file.
// </summary>

class MSFitsOutput
{
public:
  // Convert a MeasurementSet to random group UVFITS,
  // specifying the column to write ("observed", "calibrated", "model") and
  // whether to write the system calibration table.
  // <br>If asMultiSource=True a multi-source UVFits file is written.
  // <br>If combineSpw=True, all spectral-windows of a frequency group
  // are combined.
  static Bool writeFitsFile(const String& fitsfile, const MeasurementSet& ms,
			    const String& column, Int startchan=-1, 
			    Int nchan=-1, Int stepchan=-1, 
			    Bool writeSysCal = False,
			    Bool asMultiSource = False, Bool combineSpw=False,
			    Bool writeStation=False, Double sensitivity = 1.0);


private:
  // Write the main table.
  static FitsOutput *writeMain(Int& refPixelFreq, Double& refFreq,
			       Double& chanbw,
			       const String& outFITSFile,
			       const MeasurementSet& rawms,
			       const String& column,
			       const Block<Int>& spwidMap,
			       Int nrspw,
			       Int startchan, Int nchan, Int stepchan,
			       const Block<Int>& fieldidMap,
			       Bool asMultiSource,
			       Bool combineSpw);

  // Write the FQ table.
  // If combineSpw is True, all spectral-windows are written in one
  // row of the FITS table.
  static Bool writeFQ(FitsOutput *output, const MeasurementSet& ms, 
		      const Block<Int>& spwidMap, Int nrspw,
		      Double refFreq, Int refPixelFreq, 
		      Double chanbw, Bool combineSpw);

  // Write the AN table.
  static Bool writeAN(FitsOutput *output, const MeasurementSet& ms,
		      Double refFreq, Bool writeStation);

  // Write the SU table.
  static Bool writeSU(FitsOutput *output, const MeasurementSet& ms,
		      const Block<Int>& fieldidMap, Int nrfield,
		      const Block<Int>& spwidMap, Int nrspw);

  // Write the TY table.
  static Bool writeTY(FitsOutput *output, const MeasurementSet& ms,
		      const Table& syscal, const Block<Int>& spwidMap,
		      uInt nrif, Bool combineSpw);

  // Write the GC table.
  static Bool writeGC(FitsOutput *output, const MeasurementSet& ms,
		      const Table& syscal, const Block<Int>& spwidMap,
		      uInt nrif, Bool combineSpw, Double sensitivity,
		      Int refPixelFreq, Double refFreq, Double chanbw);

  // Convert time to day and fraction.
  static void timeToDay(Int& day, Double& dayFraction, Double time);

  // Get the time and hourangle from the MS at the given row.
  // It uses the field-id and observation-id to calculate the hourangle.
  static void getStartHA (Double& startTime, Double& startHA,
			  const MeasurementSet& ms, uInt rownr);

  // Handle the SYSCAL table.
  // It skips the entries not needed and sorts it in the correct order.
  static Table handleSysCal (const MeasurementSet& ms,
			     const Vector<Int>& spwids, Bool isSubset);

  // Determine which ids are selected in the main table
  // (used for fields and spectral-window).
  // It fills a block for all possible ids, where -1 tells that the
  // id is not selected. Furthermore it fills a vector with the
  // selected id numbers.
  // The input is a vector containing all ids in the main table.
  // If isSubset is False the main table is not a selection, but
  // represents an entire MS. In that case the map and selids are
  // simply filled with values 0-nrid.
  static Int makeIdMap (Block<Int>& map, Vector<Int>& selids,
			const Vector<Int>& allids, Bool isSubset);
};



} //# NAMESPACE CASA - END

#endif
