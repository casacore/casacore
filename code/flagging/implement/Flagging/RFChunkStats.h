//# RFChunkStats.h: this defines RFChunkStats
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
#ifndef FLAGGING_RFCHUNKSTATS_H
#define FLAGGING_RFCHUNKSTATS_H

#include <measures/Measures/Stokes.h> 
#include <casa/Arrays/Vector.h> 
#include <casa/Arrays/Matrix.h> 
#include <casa/Arrays/Cube.h> 
#include <casa/Containers/Block.h> 
#include <lattices/Lattices/LatticeIterator.h> 
#include <flagging/Flagging/RFCommon.h>

namespace casa { //# NAMESPACE CASA - BEGIN

class RedFlagger;
class MeasurementSet;
class VisibilityIterator;
class VisBuffer;
class PGPlotterInterface;

class RFABase;
typedef RFABase RFA;

// <summary>
// RFChunkStats: vital information and flagging stats for a visibility chunk
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> VisibilityIterator
//   <li> VisBuffer
//   <li> MeasurementSet
//   <li> RedFlagger
// </prerequisite>
//
// <etymology>
// </etymology>
//
// <synopsis>
// RFChunkStats maintains various stats, derived values, and flagging
// counts for a single visibility chunk. Also serves as an interface to
// VisIter and VisBuffer.
// </synopsis>
//
// <motivation>
// Vital information about an MS or a visibility chunk is spread all over 
// three classes (MS, VisIter, VisBuffer). RFChunkStats provides a central
// point for flagging agents to look this info up. 
// </motivation>
//
// <todo asof="2001/04/16">
//   <li> add this feature
//   <li> fix this bug
//   <li> start discussion of this possible extension
// </todo>

class RFChunkStats : public FlaggerEnums
{
protected:
  VisibilityIterator &visiter;
  VisBuffer          &visbuf;
  RedFlagger         &flagger;
  
  IPosition visshape;  
  uInt counts[Num_StatEnums];
  Matrix<uInt> nf_ifr_time,nf_chan_ifr;
  Vector<uInt> rows_per_ifr;
  Vector<uInt> nrf_ifr,nrf_time;
  Vector<Int>  ifr_nums;
  Vector<Int>  corrtypes;
  Vector<Double> freq;
  String       corr_string;
  Double       start_time,end_time,current_time;
  uInt chunk_no,pass_no;
  Int itime;
  
  PGPlotterInterface *pgp_screen,*pgp_report;
  
public:
// constructor
  RFChunkStats( VisibilityIterator &vi,VisBuffer &vb,RedFlagger &rf,
         PGPlotterInterface *pgp_scr=NULL,PGPlotterInterface *pgp_rep=NULL );

// accessors to VisIter
  const VisibilityIterator & visIter () const { return visiter; }
  VisibilityIterator & visIter () { return visiter; }
// accessor to VisBuffer
  VisBuffer &visBuf () { return visbuf; }
// accessor to MS
  const MeasurementSet & measSet () const;
// accessor to MS
  const String msName () const;
// returns antenna names
  const Vector<String>  & antNames () const;

// accessors to plotters
  PGPlotterInterface & pgpscr() const;
  PGPlotterInterface & pgprep() const;
// divides the pgp_report view into subpanels
  void setReportPanels (Int nx,Int ny) const;
  
// loads data for new chunk, resets all flag stat counters
  void newChunk ();
// loads data for new pass
  void newPass (uInt npass);
// loads data for new iteration
  void newTime ();

// returns current chunk number
  uInt nchunk() const { return chunk_no; };
// returns current pass number
  uInt npass()  const { return pass_no; };
// returns current time slot
  Int  iTime()  const { return itime; };

// returns a data dimension (POL, CHAN, IFR, etc.)
  uInt num ( StatEnums which ) const { return counts[which]; }
// returns vector of frequencies (one per channel)
  const Vector<Double> & frequency ();
  
// returns time of currently iterated time slot
  Double currentMJD () const     
    { return current_time; }
// return first time slot in chunk
  Double startMJD () const     
    { return start_time; }
// return last time slot in chunk
  Double endMJD () const       
    { return end_time; }

// returns corr mask corresponding to specified Stokes types
// (templated, but only String and Int will actually work)
  template<class T> RFlagWord getCorrMask ( const Vector<T> &corrspec );
  
// returns mask with all correlations
  RFlagWord fullCorrMask () { return (1<<num(CORR))-1; };
// returns string of correlations
  const String & getCorrString () { return corr_string; }

// returns IFR index corresponding to current VisBuffer rows
  uInt ifrNum( uInt nr )  { return ifr_nums(nr); };
// returns IFR index corresponding to current VisBuffer rows
  const Vector<Int> & ifrNums ()  { return ifr_nums; };

// converts antenna indices into IFR index
  uInt antToIfr ( uInt ant1,uInt ant2 );
// converts IFR index back to antenna numbers
  void ifrToAnt ( uInt &ant1,uInt &ant2,uInt ifr );
// converts IFR index to standard ID string 
  String ifrString ( uInt ifr ); 
  
// data availability stats, per IFR
  uInt nrowPerIfr ( uInt ifr )            { return rows_per_ifr(ifr); }
  const Vector<uInt> & nrowPerIfr () const { return rows_per_ifr; }

// accessors to various flagging stats
  uInt & nfIfrTime ( uInt ifr,uInt itime )  
                                          { return nf_ifr_time(ifr,itime); }
  const Matrix<uInt> & nfIfrTime () const    
                                          { return nf_ifr_time; }
  uInt & nfChanIfr ( uInt ich,uInt ifr   )  // flags per channel and ifr
                                          { return nf_chan_ifr(ich,ifr); }
  const Matrix<uInt> & nfChanIfr () const   
                                          { return nf_chan_ifr; }        
  uInt & nrfIfr  (uInt i)                  
                                          { return nrf_ifr(i); }
  uInt & nrfTime (uInt i)                  
                                          { return nrf_time(i); }
  const Vector<uInt> & nrfIfr  () const     
                                          { return nrf_ifr; };
  const Vector<uInt> & nrfTime () const     
                                          { return nrf_time; };

// prints stats to stderr
  void printStats ();
};

// enums for which stats are actually collected
const RFChunkStats::StatEnums active_stats[] = { RFChunkStats::CHAN,RFChunkStats::IFR,RFChunkStats::TIME };
const uInt num_active_stats = 3;

// global function for finding polarization by index
Int findCorrType( Stokes::StokesTypes type,const Vector<Int> &corr );


} //# NAMESPACE CASA - END

#ifndef AIPS_NO_TEMPLATE_SRC
#include <flagging/Flagging/RFChunkStats.cc>
#endif //# AIPS_NO_TEMPLATE_SRC
#endif
