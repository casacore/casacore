//# RFANewMedianClip.h: this defines RFANewMedianClip
//# Copyright (C) 2000,2001
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
#ifndef FLAGGING_RFANEWMEDIANCLIP_H
#define FLAGGING_RFANEWMEDIANCLIP_H

#include <flagging/Flagging/RFAFlagCubeBase.h> 
#include <flagging/Flagging/RFDataMapper.h> 
#include <flagging/Flagging/RFFlagCube.h> 
#include <flagging/Flagging/RFRowClipper.h> 
#include <flagging/Flagging/RFDebugPlot.h> 
#include <casa/System/PGPlotter.h>
#include <scimath/Mathematics/MedianSlider.h> 

namespace casa { //# NAMESPACE CASA - BEGIN

// <summary>
// RFANewMedianClip:RedFlagger Agent;clips relative to median over time slots
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> MedianSlider
//   <li> RFAFlagCubeBase
// </prerequisite>
//
// <synopsis>
// RFANewMedianClip computes a median of some quantity over time slots, 
// per each channel. Deviation w/respect to the median is computed for 
// the actual flagging.
// </synopsis>
//
// <todo asof="2004/04/21">
//   <li> add this feature
//   <li> fix this bug
//   <li> start discussion of this possible extension
// </todo>

class RFANewMedianClip : public RFAFlagCubeBase, public RFDataMapper, public PGPlotEnums
{
public:
  RFANewMedianClip  ( RFChunkStats &ch, const RecordInterface &parm );
  virtual ~RFANewMedianClip ();

  virtual uInt estimateMemoryUse ();
  virtual Bool newChunk (Int &maxmem);
  virtual void endChunk ();
  virtual void startData ();
  virtual void startDry (); // add
  virtual IterMode iterTime (uInt itime);
  virtual IterMode iterRow  (uInt irow);
  virtual IterMode iterDry  (uInt it);
  virtual IterMode endData  ();
  virtual IterMode endDry  ();
 
  virtual String getDesc ();
  static const RecordInterface & getDefaults ();

protected:
  MedianSlider & slider (uInt ich,uInt ifr);
  MedianSlider globalmed;
  void makePlot ( PGPlotterInterface &pgp,uInt ich );

  FlagCubeIterator * pflagiter; 
  FlagCubeIterator flag_iter;
  Double  threshold;  

  MedianSlider * msl;

  // lattice of evaluated values [NCH,NIFR,NTIME]
  RFCubeLattice<Float> evalue;
  // matrix of standard deviation [NCH,NIFR]
  Matrix<Float> stdev;
  Bool stdeved;
  Double globalsigma;
};


inline MedianSlider & RFANewMedianClip::slider (uInt ich,uInt ifr)
{
  return msl[ ifr*num(CHAN) + ich ];
}


} //# NAMESPACE CASA - END

#endif
