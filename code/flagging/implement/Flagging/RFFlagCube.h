//# RFFlagCube.h: this defines RFFlagCube
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
#ifndef FLAGGING_RFFLAGCUBE_H
#define FLAGGING_RFFLAGCUBE_H
    
#include <flagging/Flagging/RedFlagger.h>
#include <flagging/Flagging/RFCubeLattice.h>
#include <flagging/Flagging/RFChunkStats.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/LogiMatrix.h>
#include <casa/Arrays/LogiVector.h>
#include <casa/Logging/LogIO.h>

namespace casa { //# NAMESPACE CASA - BEGIN

typedef RFCubeLatticeIterator<RFlagWord> FlagCubeIterator;

class PGPlotterInterface;

// special row flag masks. RowFlagged for flagged rows, 
// RowAbsent for absent rows
const RFlagWord RowFlagged=1,RowAbsent=2;

// Function for working with bitmasks. Does a bitwise-AND
// on every element, returns True if !=0 or False if ==0
template<class T> Array<T> operator & ( const Array<T> &,const T &);
// returns a LogicalArray corresponding to (ARR&MASK)!=0
template<class T> LogicalArray  maskBits  ( const Array<T> &,const T &);

// <summary>
// RFFlagCube: a cube of flags
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> RFCubeLattice
// </prerequisite>
//
// <synopsis>
// RFFlagCube implements an [NCHAN,NIFR,NTIME] cube of flags, stored in
// a TempLattice that is iterated alog the TIME axis.  One static
// (i.e. global) cube is used to hold the actual flags. Individual
// instances (instantiated by flagging agents) have individual unique
// bitmasks and, possibly, individual iterators.
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// </motivation>
//
// <todo asof="2001/04/16">
//   <li> add this feature
//   <li> fix this bug
//   <li> start discussion of this possible extension
// </todo>

class RFFlagCube : public FlaggerEnums
{
public:
// default log sink
  static LogIO default_sink;
    
// constructor
  RFFlagCube ( RFChunkStats &ch,Bool ignore=False,Bool reset=False,LogIO &os=default_sink );
  ~RFFlagCube ();

// returns reference to logsink
  LogIO & logSink ();
// returns estimated size of flag cube for a given chunk.
  static uInt estimateMemoryUse ( const RFChunkStats &ch );
// creates flag cube for current chunk. name is name of agent.
  void init ( RFlagWord polmsk,const String &name = "" );
// cleans up at end of chunk
  void cleanup ();

// returns summary of stats in text form
  String getSummary ();
// prints flagging stats to stderr
  void printStats ();
// produces a plot of the flagging stats
  void plotStats (PGPlotterInterface &pgp);
// returns number of stat plots which will be done 
  static Int numStatPlots (const RFChunkStats &chunk);

// resets at start of pass
  FlagMatrix * reset ();

// advances global flag iterator to time slot it (if required), sets
// the flag cursor from the iterator (see below). If getflags is true,
// also calls getDataFlags().
  FlagMatrix * advance   ( uInt it,Bool getFlags=False );
// fills global flag lattice with apriori flags from a VisBuffer (if required)
  void getMSFlags  ();
// transfers all flags from lattice into VisBuffer
  void setMSFlags  ();

// creates a custom iterator
  FlagCubeIterator newCustomIter ();

// Returns full flag matrix (i.e. cursor of global iterator)
  const FlagMatrix & flagMatrix ();
  
// Returns matrix of row flags
  const FlagMatrix & rowFlagMatrix ();
  
// Returns boolean NIFRxNT map of row flags
  const LogicalMatrix rowFlagMap () 
        { return maskBits(rowFlagMatrix(),RowFlagged); }
// Returns boolean NIFRxNT map of rows actually present in the data
  const LogicalMatrix rowAvailabilityMap () 
        { return !maskBits(rowFlagMatrix(),RowAbsent); }

// sets or clears a flag at the given flag cursor
  Bool setFlag      ( uInt ich,uInt ifr,FlagCubeIterator &iter );
  Bool clearFlag    ( uInt ich,uInt ifr,FlagCubeIterator &iter );
// Gets full flag word at the given flag cursor.
  RFlagWord getFlag ( uInt ich,uInt ifr,FlagCubeIterator &iter );

// Versions of above that use global flag cursor
  Bool setFlag      ( uInt ich,uInt ifr );
  Bool clearFlag    ( uInt ich,uInt ifr );
  RFlagWord getFlag ( uInt ich,uInt ifr );
  
// the preFlagged() function uses the corr-mask to tell if any of this
// agent's correlations are pre-flagged. Uses internal cursor.
  Bool preFlagged   ( uInt ich,uInt ifr );
// The agentFlagged() uses the corr-flagmask to tell if any of my
// correlations are flagged by any agent (including myself). 
// (i.e. has anyone flagged my correlations?). Uses internal cursor.
  Bool agentFlagged ( uInt ich,uInt ifr );
// This corresponds to preFlagged OR agentFlagged. Uses internal cursor.
  Bool anyFlagged   ( uInt ich,uInt ifr );

// Sets or clears a row flag
  Bool setRowFlag      ( uInt ifr,uInt itime );
  Bool clearRowFlag    ( uInt ifr,uInt itime );
// Gets full row flag word
  RFlagWord getRowFlag ( uInt ifr,uInt itime );
  
// tells if a row is pre-flagged in the MS (or does not exist)
  Bool rowPreFlagged   ( uInt ifr,uInt itime );  
// tells if a row is flagged by any agent
  Bool rowAgentFlagged ( uInt ifr,uInt itime );  
// preFlagged OR agentFlagged  
  Bool rowFlagged      ( uInt ifr,uInt itime );
  
// returns reference to internal iterator
  FlagCubeIterator &  iterator ();
  
// returns flag mask for this agent
  RFlagWord flagMask ();      
// returns correlations mask for this agent
  RFlagWord corrMask ();
// returns the checked-correlations mask for this agent
// (=0 for RESET/IGNORE policies, or =corrMask() for HONOR policy).
  RFlagWord checkCorrMask ();
// returns mask of all correlations
  static RFlagWord fullCorrMask ();
// returns combination of flag masks corresponding to agents that operate
// on correlations in this agent's corrmask (including myself).
// (in essence, flag&corrFlagMask() == has anyone flagged my correlations)
// Invalidated by init(), and only becomes valid at next reset()!
  RFlagWord corrFlagMask ();
// for the given corr-mask, returns the corrFlagMask
  static RFlagWord corrFlagMask ( RFlagWord cmask );
  
// returns the number of instances of the flag cube
  static Int numInstances ();

// sets the maximum memory usage for the flag cube  
  static void setMaxMem ( Int maxmem );
// returns the current maximum memory usage
  static int  getMaxMem ();
      
protected:
// helper methods for generating plots
  void plotIfrMap  ( PGPlotterInterface &pgp,const Matrix<Float> &img,const LogicalVector &ifrvalid);
  void plotAntAxis ( PGPlotterInterface &pgp,const Vector<uInt> &antnums,Bool yaxis );
  void plotImage   ( PGPlotterInterface &pgp,const Matrix<Float> &img,
                     const char *labelx,const char *labely,const char *labeltop,
                     Bool wedge = True,Float xbox=0,Float ybox=0,Bool xfreq=False);
    
    
  RFChunkStats &chunk;                  // chunk
  // shortcut to RFChunkStats::num
  uInt num ( StatEnums which ) { return chunk.num(which); }
      
  static RFCubeLattice<RFlagWord> flag; // global flag lattice
  static FlagMatrix flagrow;             // (nIfr,nTime) matrix of row flags
  static Int pos_get_flag,pos_set_flag; 

  static Bool reset_preflags; // flag: RESET policy specified for at least one instance
  
  static uInt npol,nchan;
  
// Flag mask used by this instance. Each instance has a unique 1-bit mask.
// This is assigned automatically in the constructor, by updating the 
// instance count and the nextmask member.
// Note that the low N bits of a mask are assigned to pre-flags (one per
// each correlation in the MS); so the agents start at bit N+1.
  RFlagWord flagmask,       // flagmask of this instance
            corrmask,        // corrmask of this instance (corrs used/flagged by it)
            check_corrmask,  // mask checked by preFlagged() & co. Set to 0 for
                             // RESET or IGNORE policy, or to corrmask for HONOR
            check_rowmask,   // same for row flags: 0 or RowFlagged
            my_corrflagmask; // my corrFlagMask(), see above
  static Int agent_count;    // # of agents instantiated
  static RFlagWord base_flagmask, // flagmask of first agent instance
      full_corrmask;          // bitmask for all correlations in MS (low N bits)
// agent_corrmasks is an array of correlation masks for every object instance
  static RFlagWord agent_corrmasks[sizeof(RFlagWord)*8];
// corr_flagmask is a mapping from corrmasks into masks of agents that flag the
// given corrmask
  static Vector<RFlagWord> corr_flagmask;
  
// log sink
  LogIO os;

// pre-flag policy (can be set on a per-instance basis)
  PreFlagPolicy pfpolicy;
  
// flagging stats for this instance
  uInt tot_fl_raised,fl_raised,fl_cleared,
      tot_row_fl_raised,row_fl_raised,row_fl_cleared;
    
// local flag cursor used by this instance (setFlag and clearFlag). 
// Normally, set to flag.cursor() in advance(), but can be overridden
// by setFlagCursor();
  FlagMatrix * flag_curs;
  uInt flag_itime;
  
// members for managing flagging report plots
  static PGPlotterInterface * report_plotted;
  static String agent_names;
  
// number of instances in use
  static Int num_inst;
// maximum memory size to use for the flag cube
  static Int maxmemuse; 
};

inline RFlagWord RFFlagCube::flagMask ()
{ return flagmask; }

inline RFlagWord RFFlagCube::corrMask ()
{ return corrmask; }

inline RFlagWord RFFlagCube::checkCorrMask ()
{ return check_corrmask; }

inline RFlagWord RFFlagCube::fullCorrMask ()
{ return full_corrmask; }

inline RFlagWord RFFlagCube::corrFlagMask ()
{ return my_corrflagmask; }

inline RFlagWord RFFlagCube::corrFlagMask ( RFlagWord cmask )
{ return corr_flagmask((uInt)cmask); }

inline RFlagWord RFFlagCube::getFlag ( uInt ich,uInt ifr,FlagCubeIterator &iter )
{ return (*iter.cursor())(ich,ifr); }

inline Bool RFFlagCube::setFlag ( uInt ich,uInt ifr ) 
{ return setFlag(ich,ifr,flag.iterator()); } 
inline Bool RFFlagCube::clearFlag ( uInt ich,uInt ifr ) 
{ return clearFlag(ich,ifr,flag.iterator()); } 
inline RFlagWord RFFlagCube::getFlag ( uInt ich,uInt ifr ) 
{ return getFlag(ich,ifr,flag.iterator()); } 

inline FlagCubeIterator RFFlagCube::newCustomIter ()
{ return flag.newIter(); }

inline const FlagMatrix & RFFlagCube::flagMatrix ()
{ return *flag_curs; }

inline const FlagMatrix & RFFlagCube::rowFlagMatrix ()
{ return flagrow; }

inline Bool RFFlagCube::preFlagged ( uInt ich,uInt ifr )
{ return getFlag(ich,ifr)&check_corrmask != 0; }    

inline Bool RFFlagCube::agentFlagged ( uInt ich,uInt ifr )
{ return getFlag(ich,ifr)&my_corrflagmask != 0; }    

inline Bool RFFlagCube::anyFlagged ( uInt ich,uInt ifr )
{ return getFlag(ich,ifr)&(check_corrmask|my_corrflagmask) != 0; }

// Gets full row flag word
inline RFlagWord RFFlagCube::getRowFlag ( uInt ifr,uInt itime )
{ return flagrow(ifr,itime); }
// tells if a row is pre-flagged in the MS (or does not exist)
inline Bool RFFlagCube::rowPreFlagged   ( uInt ifr,uInt itime )
{ return getRowFlag(ifr,itime)&check_rowmask; }
// tells if a row is flagged by any agent
inline Bool RFFlagCube::rowAgentFlagged ( uInt ifr,uInt itime )
{ return getRowFlag(ifr,itime)&~(RowFlagged|RowAbsent); }
// preFlagged OR agentFlagged  
inline Bool RFFlagCube::rowFlagged      ( uInt ifr,uInt itime )
{ return getRowFlag(ifr,itime)&(check_rowmask?~0:~RowFlagged); }

inline FlagCubeIterator & RFFlagCube::iterator ()
{ return flag.iterator(); }

inline int RFFlagCube::numInstances ()
{ return num_inst; }

inline void RFFlagCube::setMaxMem ( Int maxmem )
{ maxmemuse=maxmem; }

inline Int RFFlagCube::getMaxMem ()
{ return maxmemuse; }

inline LogIO & RFFlagCube::logSink ()
{ return os; }


} //# NAMESPACE CASA - END

#endif
