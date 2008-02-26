//# MSContinuumSubtractor.h: Fit & subtract continuum from spectral line data
//# Copyright (C) 2004
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
//#
#ifndef MS_MSCONTINUUMSUBTRACTOR_H
#define MS_MSCONTINUUMSUBTRACTOR_H

#include <casa/aips.h>
#include <casa/BasicSL/String.h>
#include <ms/MeasurementSets/MSColumns.h>
namespace casa { //# NAMESPACE CASA - BEGIN

class MeasurementSet;
class LogIO;

// <summary>Fits and subtracts or models the continuum in spectra</summary>
// <use visibility=export>
// 
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>
// 
// <prerequisite>
//   <li> <linkto class=MeasurementSet>MeasurementSet</linkto>
// </prerequisite>
//
// <etymology>
// This class's main aim is to subtract the continuum from spectral line data.
// </etymology>
//
// <synopsis>
// Spectral line observations often contain continuum emission which is
// present in all channels (often with small slope across band). This
// class fits this continuum and subtracts it, replacing either the
// corrected data or the model data column.
// </synopsis>
//
// <example>
// <srcBlock>
//     MS inMS(fileName);
//     MSContinuumSubtractor mssub(inMS);
//     mssub.setDataDescriptionIds(ddIds);
//     mssub.setFields(fieldIds);
//     mssub.setSolutionInterval(10.0);
//     mssub.setMode("subtract");
//     mssub.subtract();
// </srcBlock>
// A <src>MSContinuumSubtractor</src> object is constructed 
// and the continuum is subtracted with a 10.0s averaging time for the fit.
// </example>
//
// <motivation>
// This class replaces existing functionality at the glish level, in an
// attempt to speed up the continuum subtraction process.
// </motivation>
//
// <todo asof="">
// </todo>
 

class MSContinuumSubtractor
{
public:
// Constructor
   MSContinuumSubtractor (MeasurementSet& ms);

// Assignment (only copies reference to MS, need to reset selection etc)
  MSContinuumSubtractor& operator=(MSContinuumSubtractor& other);

// Destructor
  ~MSContinuumSubtractor();

// Set the required spws (ddids)
   void setSpw(const String& spw);
   void setDataDescriptionIds(const Vector<Int>& ddIds);
 
// Set the required field Ids
   void setField(const String& field);
   void setFields(const Vector<Int>& fieldIds);

// Set the channels to use in the fit
   void setChannels(const Vector<Int>& channels);

// Set the solution interval in seconds, the value zero implies scan averaging
   void setSolutionInterval(Float solInt);

// Set the order of the fit (1=linear)
   void setOrder(Int order);

// Set the processing mode: subtract, model or replace
   void setMode(const String& mode);

// Do the subtraction (or save the model)
   void subtract();

private:
// Pointer to MS
   MeasurementSet* ms_p;
// DataDescription Ids to process
   Vector<Int> itsDDIds;
// Field Ids to process
   Vector<Int> itsFieldIds;
// Channels to use in fit
   Vector<Int> itsChannels;
// Solution interval for fit
   Float itsSolInt;
// Order of the fit
   Int itsOrder;
// Processing mode
   String itsMode;
// Number of spws
   Int nSpw_;


};


} //# NAMESPACE CASA - END

#endif
