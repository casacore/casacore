//# RedFlagger.h: this defines RedFlagger
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
#ifndef FLAGGING_REDFLAGGER_H
#define FLAGGING_REDFLAGGER_H

#include <flagging/Flagging/RFChunkStats.h>
#include <casa/System/PGPlotter.h>
#include <casa/Arrays/Vector.h>
#include <casa/Containers/Record.h>
#include <ms/MeasurementSets/MeasurementSet.h>
#include <casa/Logging/LogIO.h>

#include <casa/Quanta/Quantum.h>
#include <measures/Measures/MDirection.h>
#include <measures/Measures/MPosition.h>
#include <measures/Measures/MRadialVelocity.h>

namespace casa { //# NAMESPACE CASA - BEGIN

class VisSet;
class RFABase;
class PGPlotter;
class PGPlotterInterface;
        
// <summary>
// RedFlagger: high-performance automated flagging
// </summary>

// <use visibility=global>

// <reviewed reviewer="" date="" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> implement/RedFlagger
// </prerequisite>
//
// <etymology>
// If it flags, why not red? Plus MSFlagger and plain flagger were already
// taken.
// </etymology>
//
// <synopsis>
// RedFlagger performs automated flagging operations on a measurement set.
// The class is constructed from an MS. After that, the run method may be used
// to run any number of flagging agents.
// </synopsis>
//
// <example>
//        // construct MS and flagger
//        MeasurementSet ms("test.MS2",Table::Update);
//        RedFlagger flagger(ms);
//        // build record of global flagging options
//        Record opt(Record::Variable);
//        // build record of flagging agents to be run
//        Record selopt( flagger.defaultAgents().asRecord("select") );
//        selopt.define(RF_POLICY,"RESET");
//        selopt.define(RF_AUTOCORR,True);
//        Record agents(Record::Variable);
//        agents.defineRecord("select",selopt);
//        // perform the flagging
//        flagger.run(agents,opt);
// </example>
//
// <motivation>
// We need an automated flagging tool. Existing tools (MSFlagger and flagger.g)
// were too slow. Hence, RedFlagger was developed.
// </motivation>
//
// <todo asof="2001/04/16">
//   <li> add this feature
//   <li> fix this bug
//   <li> start discussion of this possible extension
// </todo>


class RedFlagger : public FlaggerEnums
{
protected:
// creates an agent by name
  RFABase * createAgent ( const String &name,RFChunkStats &chunk,const RecordInterface &parms );
// sets up record of agents and default parameters
  const RecordInterface & setupAgentDefaults ();
// sets up debug and report plotting objects
  void setupPlotters     ( const RecordInterface &opt );
// detaches from all active plotters
  void cleanupPlotters   ();
// plots flagging reports from individual agents
  void plotAgentReports  ( PGPlotterInterface &pgp);
  void printAgentReports  ( );
// plots a flagging summary
  void plotSummaryReport ( PGPlotterInterface &pgp,RFChunkStats &chunk,const RecordInterface &opt );
  void printSummaryReport ( RFChunkStats &chunk,const RecordInterface &opt );
 
  MeasurementSet   ms;
  Block<RFABase*> acc;
  //new added
  MeasurementSet *mssel_p;
  VisSet *vs_p;
  String msname_p;
  Bool nullSelect_p;
  Bool setdata_p;
  String dataMode_p;  
  Vector<Int> dataNchan_p;
  Vector<Int> dataStart_p, dataStep_p;
  Vector<Int> dataspectralwindowids_p;
  Vector<Int> datafieldids_p;
  Vector<Int> datadescids_p;
  MRadialVelocity mDataStart_p;
  MRadialVelocity mDataStep_p;
  //
  uInt nant,nifr,ntime;
  Vector<Int> ifr2ant1,ifr2ant2;
  Vector<String> antnames;
  
  PGPlotter pgp_report,pgp_screen;
  Int pgprep_nx,pgprep_ny;
  
  Record agent_defaults;

  static LogIO os;

public:  
// default constructor 
  RedFlagger  ();
// construct and attach to a measurement set
  RedFlagger  ( const MeasurementSet &ms );
  
  ~RedFlagger ();
  
// Change or set the MS this RedFlagger refers to.
  void attach( const MeasurementSet &ms, Bool setupAgentDefaults=True );

  // Set the data selection parameters
  Bool setdata(const String& mode, const Vector<Int>& nchan, 
	       const Vector<Int>& start,
	       const Vector<Int>& step, 
	       const MRadialVelocity& mStart,
	       const MRadialVelocity& mStep,	      
	       const Vector<Int>& spectralwindowids,
	       const Vector<Int>& fieldid,
	       const String& msSelect="");
  
  // Detaches from the MS  
  void detach();
  
// Runs the flagger. agent is a record of agents (name+options). opt is a
// record of additional options.
// Set indexing_base to 1 if agent options use 1-based indexing.  usually,
// this is only necessary if options are being passed from Glish.
  void run ( const RecordInterface &agents,const RecordInterface &opt,uInt indexing_base=0 );    
  void summary ( const RecordInterface &agents,const RecordInterface &opt,uInt indexing_base=0 );    
  
// returns current MS
  const MeasurementSet & measSet() const { return ms; }
    
// number of antennas in MS
  uInt numAnt    () const 
      { return nant; };
// number of IFRs in MS
  uInt numIfr    () const 
      { return nifr; };
// number of time slots in MS
  uInt numTime   () const 
      { return ntime; };
// names of antennas
  const Vector<String> & antNames() const 
      { return antnames; };
  
// derives a flat IFR index from two antenna indices
  uInt ifrNumber ( Int ant1,Int ant2 ) const;
// vector version of above
  Vector<Int> ifrNumbers ( Vector<Int> ant1,Vector<Int> ant2 ) const;
// derives antenna indices from a flat IFR index
  void ifrToAnt ( uInt &ant1,uInt &ant2,uInt ifr ) const;
// returns a record with all available agents and their default options
  const RecordInterface & defaultAgents () const 
      { return agent_defaults; }
// returns a record of available options
  static const RecordInterface & defaultOptions ();
// returns the log sink 
  static LogIO & logSink ()       { return os; }
// returns the flag report plotter
  PGPlotterInterface & pgprep ()   { return pgp_report; }
// returns the screen ("debug") plotter
  PGPlotterInterface & pgpscr ()   { return pgp_screen; }
// Uses SUBP to divide the flag report PGPlotter into subpanes.
// Keeps track of the current setting, so no extra pagebreaks are produced
  void setReportPanels ( Int nx,Int ny );
// Utility function to do channel selection
  Bool selectDataChannel(VisSet& vs, Vector<Int>& spectralwindowids, 
			 String& dataMode, 
			 Vector<Int>& dataNchan, 
			 Vector<Int>& dataStart, Vector<Int>& dataStep,
			 MRadialVelocity& mDataStart, 
			 MRadialVelocity& mDataStep);
  
private:
    
  RedFlagger( const RedFlagger & )          {};

  RedFlagger& operator=(const RedFlagger &)  { return *this; };

  void printAgentRecord(String &, uInt, const RecordInterface &);
};


} //# NAMESPACE CASA - END

#endif
