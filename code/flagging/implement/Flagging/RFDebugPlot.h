//# RFDebugPlot.h: this defines RFDebugPlot
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
#ifndef FLAGGING_RFDEBUGPLOT_H
#define FLAGGING_RFDEBUGPLOT_H
    
#include <flagging/Flagging/RFCommon.h>
    
namespace casa { //# NAMESPACE CASA - BEGIN

class PGPlotter;
class PGPlotterInterface;
    
// <summary>
// RFDebugPlot: holds information relevant to a debug plot
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="" demos="">
// </reviewed>

// <synopsis>
// This is jsut a small helper class for managing debug plots within
// RedFlagger
// </synopsis>
//
// <todo asof="2001/04/16">
//   <li> add this feature
//   <li> fix this bug
//   <li> start discussion of this possible extension
// </todo>

class RFDebugPlot : public FlaggerEnums
{
protected:
  Bool enable;
  PGPlotterInterface *plotter;
  Int dch,difr,dit;

public: 
    
// default constructor creates disabled plot
  RFDebugPlot ();
// constructor to enable a plot
  RFDebugPlot ( PGPlotterInterface &pgp,Int ich,Int ifr,Int it );
// destructor
  ~RFDebugPlot () {};
  
// tells if plotting is enabled or not
  Bool enabled  ()  
      { return enable; }  
  
// returns plotter
  PGPlotterInterface & pgp ()  
      { return *plotter; }

// returns type (i.e. X axis) of plot
  StatEnums type ();
  
// data accessors
  Int ifr   () 
      { return difr; }
  Int chan  () 
      { return dch;  }
  Int time  () 
      { return dit; }

// Returns index of given point along X axis of plot, or -1
// if no plotting
  Int index ( uInt ich,uInt ifr,uInt it );

  // Queries user for new plot limits. Returns True if new limits
  // are entered, or False if user has opted to continue
  Bool queryPlotLimits ( Float &ymin,Float &ymax );
  
};
 

} //# NAMESPACE CASA - END

#endif
