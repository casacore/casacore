//# RFAUVBinner.h: this defines RFAUVBinner
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
#ifndef FLAGGING_RFAUVBINNER_H
#define FLAGGING_RFAUVBINNER_H

#include <flagging/Flagging/RFAFlagCubeBase.h> 
#include <flagging/Flagging/RFDataMapper.h> 
#include <flagging/Flagging/RFFlagCube.h> 
#include <flagging/Flagging/RFRowClipper.h> 
#include <flagging/Flagging/RFDebugPlot.h> 
#include <scimath/Mathematics/RigidVector.h>
#include <casa/System/PGPlotter.h>
    
namespace casa { //# NAMESPACE CASA - BEGIN

// <summary>
// RFAUVBinner: flagging via UV binning
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> RFDataMapper
//   <li> RFFlagCubeBase
//   <li> RFCubeLattice
// </prerequisite>
//
// <synopsis>
// </synopsis>
//
// <todo asof="2001/04/16">
//   <li> make UV-distance matrix static, to share between multiple instances
//   <li> collect population statistics across all channels
//   <li> 3D bins (ampl-UVdist-channel)?
//   <li> think how to solve "encroaching" problem to achieve a better
//        probability distribution. Perhaps two sets of staggered bins,
//        and for each point use the count of the bigger bin?
// </todo>

class RFAUVBinner : public RFAFlagCubeBase, public RFDataMapper, public PGPlotEnums
{
public:
  RFAUVBinner  ( RFChunkStats &ch,const RecordInterface &parm ); 
  virtual ~RFAUVBinner () {};
  
  virtual uInt estimateMemoryUse ();
  virtual Bool newChunk (Int &maxmem);
  virtual void endChunk ();
  virtual void startData ();
  virtual void startDry ();
  virtual IterMode iterTime (uInt it);
  virtual IterMode iterRow  (uInt ir);
  virtual IterMode iterDry  (uInt it);
  virtual IterMode endData  ();
  virtual IterMode endDry  ();

  virtual String getDesc ();
  static const RecordInterface & getDefaults ();
  
protected:
  IPosition computeBin( Float uv,Float y,uInt ich );
  void makePlot ( PGPlotterInterface &pgp,uInt ich );

  Double  threshold;
  uInt    min_population;
  uInt    nbin_y,nbin_uv;
  Bool    binned;
  
// stuff for a separate flagging report
  Bool  plot_report,econoplot;
  Int   plot_chan,report_chan,econo_density;
  Int   plot_thr_count;
  uInt  plot_np;
  Vector<Float> plot_px,plot_py,plot_prob;
  
// current UVW column
  Vector< RigidVector<Double,3> > *puvw;

// lattice of yvalues [NCH,NIFR,NTIME]
  RFCubeLattice<Float> yvalue;
// matrix of UV distances [NIFR,NTIME]
  Matrix<Float> uvdist;

// ranges and bin sizes
  Vector<Float> ymin,ymax,ybinsize,
// for UV, we have individual ranges/bins per each channel
               uvmin,uvmax,uvbinsize;
// bin counts
  Cube<Int> bincounts;
  Vector<Int> totcounts;

  RFDebugPlot   debug;
};


} //# NAMESPACE CASA - END

#endif
