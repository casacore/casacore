//# RFABase.h: this defines RFABase
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
#ifndef FLAGGING_RFABASE_H
#define FLAGGING_RFABASE_H

#include <casa/Arrays/Cube.h> 
#include <lattices/Lattices/TempLattice.h> 
#include <lattices/Lattices/LatticeIterator.h> 
#include <flagging/Flagging/RFChunkStats.h>
#include <casa/Logging/LogIO.h>
#include <casa/Containers/Record.h>

// <summary>
// Abstract RedFlagger Agent base class
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="" demos="">
// </reviewed>

// <synopsis>
// RFABase defines the interface for a flagging agent
// </synopsis>
//
// <motivation>
// RedFlagger works with objetcs called flagging agents. This class
// defines the complete interface between RedFlagger and agents.
// </motivation>
//
// <todo asof="2001/04/16">
//   <li> add this feature
// </todo>

class RFABase : public FlaggerEnums
{
public:
// iteration modes
  enum IterMode { 
      STOP = 0,
      DRY  = 1,
      DATA = 2,
          
      CONT = 3
  };
  
// An agent is always constructed from a chunk stats accessor, and a 
// record of parameters. No other constructors are defined, and no others
// may be used.
  RFABase ( RFChunkStats &ch,const RecordInterface &parm );
// Destructor 
  virtual ~RFABase () {};

// This method is called after constructing the agent.
  virtual void init ();

// This method is called before iterating over a chunk, to inquire the 
// expected memory use. Should return the max desired memory footprint, in MB.
// Available physical memory is divided between agents in proportion to their 
// requests.
  virtual uInt estimateMemoryUse () { return 1; }  

// Called before iterating over a chunk. Returns True if agent will
// process this chunk, or False if this the agent is unable to process it.
// (this can happen if, e.g., the requisite correlations are not present).
// The Int & maxmem argument is the number of MB memory which is still 
// available in the memory pool. The agent class should plan its memory 
// use accordingly, and subtract its expected memory use from maxmem. In effect, 
// the agent "reserves" some amount of memory. This is used by RedFlagger to 
// contain the total memory footprint. Note that only a rough reckoning
// is sufficient, so only bother estimating the biggest data structures.
// See implementations in RFADiffBase and RFATimeMedian for good examples.
  virtual Bool newChunk (Int &) 
         { return active=False; };
// Called once finished with a chunk
  virtual void endChunk () {}
  
// Called before starting a data pass on a chunk. 
  virtual void startData () {};
// Called before starting a dry pass on a chunk. 
  virtual void startDry  () {};
// Called before starting the fetch-flags pass.
  virtual void startFlag () {};
// Called after a pass is completed successfully (i.e., not stopped
// by start or iter methods). Return value: STOP to stop, DATA for 
// another data pass, DRY for another dry pass.
  virtual IterMode endData   () { return STOP; };
// Called after a dry pass is complete
  virtual IterMode endDry    () { return STOP; };
// Called after a flag pass is complete
  virtual void endFlag        () {};
  
// Iteration methods for a data pass. Either or both may be implemented.
// iterTime() is called once for each new VisBuffer (= new time slot)
// Return value: STOP to finish iterating, CONT/DATA to continue, or DRY
// to cancel the data pass and request a dry pass.
  virtual IterMode iterTime ( uInt itime ) { return CONT; };
// iterRow() is called once per each row in the VisBuffer.
// Iterating over rows is perhaps preferrable in terms of performance,
// at least for data iterations.
  virtual IterMode iterRow  ( uInt irow ) { return CONT; };
// Iteration method for a dry pass. Called once per each time slot.
// Return value: STOP to finish iterating, CONT/DRY to continue, or DATA
// to cancel the dry pass and request another data pass.
  virtual IterMode iterDry  ( uInt itime ) { return CONT; };
// Iteration method for a flag pass. Called once per each VisBuffer.
  virtual void iterFlag ( uInt itime ) {}
// called to obtain a short description of this RFA
  virtual String getDesc () { return ""; }
// called (before endChunk()) to obtain a statistics report 
  virtual String getStats () { return ""; }
// called (before endChunk()) to plots a graphical flagging report  
  virtual void plotFlaggingReport ( PGPlotterInterface & ) {};
  virtual void printFlaggingReport ( ) {};
  
// returns the name of this RFA (set in myname)
  const String & name ();
// returns the active status
  Bool isActive ()   { return active; }
// accessor to a LogIO for this agent
  LogIO & logSink ();

// static method for setting the indexing base for agent arguments
  static void setIndexingBase ( uInt base );
  
protected:
  RFChunkStats &chunk;
  Record params;
  String myname;
  
  uInt num (StatEnums which) { return chunk.num(which); };

// Bit mask of correlations which are used & flagged by this RFA. This mask is
// used to (a) interpret the pre-flags of the FLAG column, and (b) apply the
// resulting flags to the FLAG column
  RFlagWord corrmask;
  RFlagWord corrMask()  { return corrmask; }

// flag: agent is active for this chunk (set in newChunk)  
  Bool active;

  LogIO os;
  
// global flag indicates that Glish (1-based) indexing is in use
// for agent arguments
  static uInt indexingBase (); 
  
private:
    
  static uInt indexing_base;
};

inline uInt RFABase::indexingBase () 
{ 
  return indexing_base; 
}

inline void RFABase::setIndexingBase ( uInt base ) 
{ 
  indexing_base = base; 
}
  
inline LogIO & RFABase::logSink ()
{
  return os;
}
inline const String & RFABase::name ()
{
  return myname;
}

#endif
