//# RFAFlagCubeBase.h: this defines RFAFlagCubeBase
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
#ifndef FLAGGING_RFAFLAGCUBEBASE_H
#define FLAGGING_RFAFLAGCUBEBASE_H

#include <casa/Containers/RecordInterface.h>
#include <flagging/Flagging/RFABase.h> 
#include <flagging/Flagging/RFFlagCube.h>

// <summary>
// Abstract RedFlagger Agent class with a flag cube
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> RFFlagCube
// </prerequisite>
//
// <synopsis>
// RFAFlagCubeBase is derived from RFABase. It includes an RFFlagCube
// object, which is essentially an [NCHAN,NIFR,NTIME] lattice of flags.
// Approrpiate vritual methods for managing the cube are defined.
// </synopsis>
//
// <todo asof="2001/04/16">
//   <li> add this feature
//   <li> fix this bug
//   <li> start discussion of this possible extension
// </todo>

class RFAFlagCubeBase : public RFABase
{
public:
  RFAFlagCubeBase  ( RFChunkStats &ch,const RecordInterface &parm );
  virtual ~RFAFlagCubeBase ();
  
  virtual uInt estimateMemoryUse ();  
  virtual Bool newChunk ( Int &maxmem );
  virtual void endChunk ();
  virtual void startData ();
  virtual void startDry ();
  virtual void startFlag ();
  virtual IterMode iterTime (uInt it);
  virtual IterMode iterDry  (uInt it);
  virtual void iterFlag     (uInt it);
  virtual IterMode endData  ();
  virtual IterMode endDry   ();

  virtual String getDesc ();
  virtual String getStats ();
  static const RecordInterface & getDefaults ();

  virtual void plotFlaggingReport ( PGPlotterInterface &pgp );
  
protected:
// mask of active correlations. Must be setup somewhere before calling
// newChunk()
  RFlagWord   corrmask; 
// flag cube lattice
  RFFlagCube  flag;   // flag cube lattice  
};

#endif
