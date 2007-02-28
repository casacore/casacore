//# EPVisTimeAverager.h: class to average VisBuffers in time
//# Copyright (C) 2000,2002
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

#ifndef MSVIS_EPVISTIMEAVERAGER_H
#define MSVIS_EPVISTIMEAVERAGER_H

#include <casa/aips.h>
#include <msvis/MSVis/VisBuffer.h>
#include <casa/Arrays/Cube.h>

namespace casa { //# NAMESPACE CASA - BEGIN

// <summary>
// A class to average VisBuffers in time
// </summary>
//
// <use visibility=export>
//
// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> VisBuffer
// </prerequisite>
//
// <etymology>
// From "visibility", "time" and "averaging".
// </etymology>
//
// <synopsis>
// This class averages VisBuffers in time.
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// Collect all time averaging capabilities for VisBuffer averaging.
// </motivation>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//
// <todo asof="2000/09/01">
//   <li> averaging over other indices.
// </todo>

class EPVisTimeAverager
{
public:
  enum VBDataStructure 
    {
      VISIBILITY,
      MODELVISIBILITY,
      CORRECTEDVISIBILITY,
      VISCUBE,
      MODELVISCUBE,
      CORRECTEDVISCUBE
    };

  // Construct from the number of antennas, the averaging interval and
  // the pre-normalization flag
  EPVisTimeAverager (const Int& nAnt, const Double& interval, 
		   const Bool& prenorm,
		   Bool doUVW=False, 
		   VBDataStructure useVisCubes=VISIBILITY);

  EPVisTimeAverager (const Int& nAnt, const Double& interval,
		   const Bool& prenorm);
  // Null destructor
  ~EPVisTimeAverager();

  // Set internal switches
  Bool doUVW(Bool val) {Bool t=handleUVW; handleUVW=val; return t;}
  VBDataStructure setDataStructure(VBDataStructure val) 
  {VBDataStructure t=whichVBDataStructure; whichVBDataStructure=val; return t;}

  // Reset the averager
  void reset();

  // Accumulate a VisBuffer
  //
  // Extract the appropriate buffer from the given VisBuffer and then
  // call the appropriate accumulater using the function pointer.
  //
  void accumulate (const VisBuffer& vb)
  { 
    getOutVisCube(vb,whichVBDataStructure);
    (this->*Accumulater)(vb);
  };

  void accumulateVisibility (const VisBuffer& vb);
  void accumulateVisCubes (const VisBuffer& vb);

  // Finalize averaging, and return the result
  void finalizeAverage (VisBuffer& avBuf);

private:
  // Prohibit null constructor, copy constructor and assignment for now
  EPVisTimeAverager();
  EPVisTimeAverager& operator= (const EPVisTimeAverager&);
  EPVisTimeAverager (const EPVisTimeAverager&);

  //
  // Initialize the next accumulation interval
  // Call the appropriate initializer using the function pointer.
  //
  void initialize() {(this->*Initializer)();};
  void initializeVisibility();
  void initializeVisCubes();

  //
  // Normalize the current accumulation
  // Call the appropriate Normalizer using the funtion pointer.
  //
  void normalize() {(this->*Normalizer)();};
  void normalizeVisibility();
  void normalizeVisCubes();

  //
  // Extract the appropriate buffer from the supplied VisBuffer
  //
  void getOutVisCube(const VisBuffer& vb, VBDataStructure which=MODELVISCUBE);
  //
  // Extract the appropriate buffer from the internal VisBuffer for
  // accumulation
  //
  void getInVisCube(VBDataStructure which=MODELVISCUBE);
  //
  // Copy the accumulated buffer into the internal buffer of the
  // supplied VisBuffer.
  //
  void copyData(VisBuffer& vb);

  // Hash function to return the row offset for an interferometer (ant1, ant2)
  Int hashFunction (const Int& ant1, const Int& ant2);

  // Number of antennas and channels
  Int nAnt_p, nChan_p;

  // Averaging interval
  Double interval_p;

  // Pre-normalization flag
  Bool prenorm_p;

  // Start time and row of current accumulation
  Double tStart_p;
  Int avrow_p;

  // Flag to mark the first accumulation interval
  Bool firstInterval_p;

  // Averaging buffer
  VisBuffer avBuf_p;

  //Internal switches and related data structures/function pointers
  Bool handleUVW;
  VBDataStructure whichVBDataStructure;

  Cube<Complex> outVisCube,inVisCube;
  Matrix<CStokesVector> outVisibility,inVisibility;

  //
  // Pointers to member function - these are set to the appropriate
  // values in the constructor.
  //
  void (EPVisTimeAverager::*Initializer)();
  void (EPVisTimeAverager::*Normalizer)();
  void (EPVisTimeAverager::*Accumulater)(const VisBuffer &);
};


} //# NAMESPACE CASA - END

#endif


