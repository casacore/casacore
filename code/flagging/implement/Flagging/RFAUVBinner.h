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
#if !defined(AIPS_RFA_UVBINNER_H)
#define AIPS_RFA_UVBINNER_H

#include <trial/RedFlagger/RFAFlagCubeBase.h> 
#include <trial/RedFlagger/RFDataMapper.h> 
#include <trial/RedFlagger/RFFlagCube.h> 
#include <trial/RedFlagger/RFRowClipper.h> 
#include <trial/RedFlagger/RFDebugPlot.h> 
#include <trial/Arrays/RigidVector.h>
#include <trial/Tasking/PGPlotter.h>
    
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
//   <li> add this feature
//   <li> fix this bug
//   <li> start discussion of this possible extension
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
    
  Double  threshold;
  Bool    binned;
  uInt    nbin_y,nbin_uv;
  
// for flagging report
  Bool  plot_report;
  Int   plot_chan,report_chan;
  PGPlotter pgp;
  
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

#endif
