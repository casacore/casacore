//# MSFitsOutputAstron.h:  Write a MeasurementSet to a random group uvfits file
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

#ifndef MS_MSFITSOUTPUTASTRON_H
#define MS_MSFITSOUTPUTASTRON_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/ArrayFwd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations
class String;
class FitsOutput;
class MeasurementSet;
class Table;
template<class T> class Block;


// <summary>
// Write a MeasurementSet to a random group uvfits file.
// </summary>

class MSFitsOutputAstron
{
public:
  // Convert a MeasurementSet to random group UVFITS,
  // specifying the column to write ("observed", "calibrated", "model") and
  // whether to write the system calibration table.
  // <br>If asMultiSource=true a multi-source UVFits file is written.
  // <br>If combineSpw=true, all spectral-windows of a frequency group
  // are combined.
  static bool writeFitsFile(const String& fitsfile, const MeasurementSet& ms,
			    const String& column, int32_t startchan=-1, 
			    int32_t nchan=-1, int32_t stepchan=-1, 
			    bool writeSysCal = false,
			    bool asMultiSource = false, bool combineSpw=false,
			    bool writeStation=false, double sensitivity = 1.0);


private:
  // Write the main table.
  static FitsOutput *writeMain(int32_t& refPixelFreq, double& refFreq,
			       double& refFreq1, double& chanbw,
			       const String& outFITSFile,
			       const MeasurementSet& rawms,
			       const String& column,
			       const Block<int32_t>& spwidMap,
			       int32_t nrspw,
			       int32_t startchan, int32_t nchan, int32_t stepchan,
			       const Block<int32_t>& fieldidMap,
			       bool asMultiSource,
			       bool combineSpw);

  // Write the FQ table.
  // If combineSpw is true, all spectral-windows are written in one
  // row of the FITS table.
  static bool writeFQ(FitsOutput *output, const MeasurementSet& ms, 
		      const Block<int32_t>& spwidMap, int32_t nrspw,
		      double refFreq, int32_t refPixelFreq, 
		      double chanbw, bool combineSpw);

  // Write the AN table.
  static bool writeAN(FitsOutput *output, const MeasurementSet& ms,
		      double refFreq, bool writeStation);

  // Write the SU table.
  static bool writeSU(FitsOutput *output, const MeasurementSet& ms,
		      const Block<int32_t>& fieldidMap, int32_t nrfield,
		      const Block<int32_t>& spwidMap, int32_t nrspw);

  // Write the TY table.
  static bool writeTY(FitsOutput *output, const MeasurementSet& ms,
		      const Table& syscal, const Block<int32_t>& spwidMap,
		      uint32_t nrif, bool combineSpw);

  // Write the GC table.
  static bool writeGC(FitsOutput *output, const MeasurementSet& ms,
		      const Table& syscal, const Block<int32_t>& spwidMap,
		      uint32_t nrif, bool combineSpw, double sensitivity,
		      int32_t refPixelFreq, double refFreq, double chanbw);

  // Convert time to day and fraction.
  static void timeToDay(int32_t& day, double& dayFraction, double time);

  // Get the time and hourangle from the MS at the given row.
  // It uses the field-id and observation-id to calculate the hourangle.
  static void getStartHA (double& startTime, double& startHA,
			  const MeasurementSet& ms, uint32_t rownr);

  // Handle the SYSCAL table.
  // It skips the entries not needed and sorts it in the correct order.
  static Table handleSysCal (const MeasurementSet& ms,
			     const Vector<int32_t>& spwids, bool isSubset);

  // Determine which ids are selected in the main table
  // (used for fields and spectral-window).
  // It fills a block for all possible ids, where -1 tells that the
  // id is not selected. Furthermore it fills a vector with the
  // selected id numbers.
  // The input is a vector containing all ids in the main table.
  // If isSubset is false the main table is not a selection, but
  // represents an entire MS. In that case the map and selids are
  // simply filled with values 0-nrid.
  static int32_t makeIdMap (Block<int32_t>& map, Vector<int32_t>& selids,
			const Vector<int32_t>& allids, bool isSubset);
};



} //# NAMESPACE CASACORE - END

#endif
