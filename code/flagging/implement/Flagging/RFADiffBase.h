//# RFADiffBase.h: this defines RFADiffBase and RFADiffMapbase
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
#ifndef FLAGGING_RFADIFFBASE_H
#define FLAGGING_RFADIFFBASE_H

#include <casa/Arrays/LogiVector.h>
#include <casa/Containers/RecordInterface.h>
#include <flagging/Flagging/RFAFlagCubeBase.h> 
#include <flagging/Flagging/RFDataMapper.h> 
#include <flagging/Flagging/RFCubeLattice.h>
#include <flagging/Flagging/RFRowClipper.h>
#include <flagging/Flagging/RFDebugPlot.h>
#include <scimath/Mathematics/MedianSlider.h> 


namespace casa { //# NAMESPACE CASA - BEGIN

// min number of deviations for which average is considered valid
const int   RFA_MIN_NAD = 20;
// significant change in accumulated average
const Float RFA_AAD_CHANGE = 0.05;

class PGPlotterInterface;

// <summary>
// RFADiffBase: abstract class for deviation-based flagging
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> RFCubeLattice
//   <li> RFRowClipper
// </prerequisite>
//
// <etymology>
// Diff = Deviation. Well, almost...
// </etymology>
//
// <synopsis>
// Several flagging algorithms flag by analyzing the deviation w/respect
// to something at each point. RFADiffBase provides common functions for
// these classes. It will maintain a lattice of deviations, compute the 
// noise level estimates, and flag points. It will also flag rows with 
// excessive noise level (using RFRowClipper). Derived classes are
// responsible for computing the deviation.
// </synopsis>
//
// <todo asof="2001/04/16">
//   <li> add this feature
//   <li> fix this bug
//   <li> start discussion of this possible extension
// </todo>

class RFADiffBase : public RFAFlagCubeBase, public PGPlotEnums
{
public:
  RFADiffBase  ( RFChunkStats &ch,const RecordInterface &parm );
  virtual ~RFADiffBase ();
  
  virtual uInt estimateMemoryUse ();  
  virtual Bool newChunk ( Int &maxmem );
  virtual void endChunk ();
  virtual void startData ();
  virtual void startDry ();
  virtual IterMode iterTime (uInt it);
  virtual IterMode iterDry  (uInt it);
  virtual IterMode endData  ();
  virtual IterMode endDry   ();

  virtual String getDesc ();
  static const RecordInterface & getDefaults ();

// sets up a debugging plot object
  void setDebug( const RFDebugPlot &dbg );
  
protected:
  static Bool dummy_Bool;

// prepares for a pass over one data row
  void startDataRow (uInt ifr);
// updates the diff lattice with a value, and performs clipping
  Float setDiff (uInt ich,uInt ifr,Float d,Bool &flagged = dummy_Bool );
// ends pass over single data row  
  void endDataRow   (uInt ifr);
  
// updates noise estimates (sih0), returns the max change
  Float updateSigma ();     
  
// computes a correlations mask. Called once for each chunk (since correlations
// can change from chunk to chunk)
  virtual RFlagWord newCorrMask () =0;

  Double clip_level;      // clipping level, in AADs
  Double row_clip_level;  // clipping level for rows (based on noise estimates), <0 for disable
  Bool   disable_row_clip; // flag: row clipping _disabled_ globally
  Bool   clipping_rows;    // flag: row clipping active for this chunk
  
  RFCubeLattice<Float>   diff;   // (Nchan,Nifr,Nt) lattice of deviations
  FlagCubeIterator *     pflagiter; // flag iterator used by setDiff()
  RFRowClipper          rowclipper;
  
  Vector<Float> diffrow;   // one row of deviations, for noise computations
  int idiffrow;

  Matrix<Float> sig;       // current noise estimate for (it,ifr)
  Matrix<Float> sig0;      // reference estimate (boxcar average from previous pass)
  LogicalVector sigupdated;
  
// methods and members for making debugging plots
// Resets vectors (for start of new plot)
  void resetPlot ();
// Makes a plot using the filled-in vectors (below)  
  void makePlot ();

  RFDebugPlot debug;
  
  Vector<Float> dbg_val,  // vector of data values
               dbg_med,  // vector of corresponding medians (or whatever)
               dbg_thr;  // vector of currently used thresholds
  Vector<Int>   dbg_sym;  // vector of data point symbols
// index into which point in the plot is being filled, for setDiff.
// set up by child classes before calling setDiff
  Int dbg_i;    
};

// <summary>
// Abstract base class for deviation-based flagging with a data mapper.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> RFDataMapper
// </prerequisite>
//
// <synopsis>
// This is another abstract class on top of DiffBase. It is also inherited from
// RFDataMapper, so it includes functions for mapping visibilities to a single
// Float value.
// </synopsis>
//
// <todo asof="2001/04/16">
//   <li> add this feature
//   <li> fix this bug
//   <li> start discussion of this possible extension
// </todo>

class RFADiffMapBase : public RFADiffBase, protected RFDataMapper
{
public:
  RFADiffMapBase  ( RFChunkStats &ch,const RecordInterface &parm );
  virtual ~RFADiffMapBase ();

  virtual IterMode iterTime (uInt it);
  
  virtual String getDesc ();
  
// returns a Record of all available parameters and their default values
  static const RecordInterface & getDefaults ();
  
protected:
  virtual RFlagWord newCorrMask () 
    { return RFDataMapper::corrMask(chunk.visIter()); } 
  
  void setupMapper () 
    { RFDataMapper::setVisBuffer(chunk.visBuf()); }
};

} //# NAMESPACE CASA - END

#endif
