//# RFASelector.h: this defines RFASelector
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
#ifndef FLAGGING_RFASELECTOR_H
#define FLAGGING_RFASELECTOR_H

#include <flagging/Flagging/RFAFlagCubeBase.h> 
#include <flagging/Flagging/RFDataMapper.h>
#include <casa/Arrays/LogiVector.h>
    
// <summary>
// RFASelector: flags pixels/rows based on a specified selection
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> RFAFlagCubebase
// </prerequisite>
//
// <etymology>
// RedFlaggerAgent Selector
// </etymology>
//
// <synopsis>
// RFASelector accepts a whole bunch of options to select a subset of the
// MS (by time, antenna, baseline, channel/frequency, etc.), and to flag/unflag 
// the whole selection, or specific parts of it (autocorrelations, specific 
// time slots, VLA quacks, etc.)
// </synopsis>
//
// <todo asof="2001/04/16">
//   <li> add this feature
//   <li> fix this bug
//   <li> start discussion of this possible extension
// </todo>

class RFASelector : public RFAFlagCubeBase
{
public:
// constructor. 
  RFASelector ( RFChunkStats &ch,const RecordInterface &parm ); 
  virtual ~RFASelector ();
  
  virtual uInt estimateMemoryUse () { return RFAFlagCubeBase::estimateMemoryUse()+2; }
  virtual Bool newChunk ( Int &maxmem );
  virtual IterMode iterTime ( uInt it );
  virtual IterMode iterRow  ( uInt ir );

  virtual String getDesc ();
  static const RecordInterface & getDefaults ();

protected:
  typedef struct ClipInfo {
      RFDataMapper *mapper; 
      Float vmin,vmax; 
      Bool clip;          // flag outside range if True (otherwise flag inside)
      Float offset;       // offset added to value (used for angles, etc.)
  } ClipInfo;
    
  template<class T> Bool reformRange( Matrix<T> &rng,const Array<T> &arr );
  template<class T> Bool parseRange( Matrix<T> &rng,const RecordInterface &parm,const String &id );
  template<class T> Bool find( uInt &index,const T &obj,const Vector<T> &arr );
  
  Bool parseTimes  ( Array<Double> &times,const RecordInterface &parm,const String &id,Bool secs=False );
  void addString   ( String &str,const String &s1,const char *sep=" " );
  void processRow  ( uInt ifr,uInt it );
  Bool parseMinMax ( Float &vmin,Float &vmax,const RecordInterface &spec,uInt f0 );
  void addClipInfo ( const Vector<String> &expr,Float vmin,Float vmax,Bool clip );
  void parseClipField  ( const RecordInterface &spec,Bool clip );
  void addClipInfoDesc ( const Block<ClipInfo> &clip );

// description of agent
  String desc_str;
// selection arguments
  Matrix<Double> sel_freq,sel_time,sel_timerng;
  Matrix<Int>    sel_chan;
  Vector<Int>    sel_corr,sel_spwid,sel_fieldid;
  Vector<String>  sel_fieldnames;
  LogicalVector  sel_ifr,flagchan;
  Bool          sel_autocorr,unflag;
  Block<ClipInfo> sel_clip,sel_clip_row;
  LogicalVector  sel_clip_active;
  Bool            sum_sel_clip_active;
  Double        quack_si,quack_dt,scan_start,scan_end;
  
  Bool select_fullrow,flag_everything;
  
};

    
    
#endif
