//# RFDebugPlot.cc: this defines RFDebugPlot
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
#include <flagging/Flagging/RFDebugPlot.h>
#include <casa/System/PGPlotter.h>
#include <casa/stdio.h>
        
namespace casa { //# NAMESPACE CASA - BEGIN

RFDebugPlot::RFDebugPlot () :
  enable(False),plotter(NULL),dch(-1),difr(-1),dit(-1)
{}

RFDebugPlot::RFDebugPlot ( PGPlotterInterface &pgp,Int ich,Int ifr,Int it ) :
  enable(True),plotter(&pgp),dch(ich),difr(ifr),dit(it)
{}

RFDebugPlot::StatEnums RFDebugPlot::type ()  
{
  if( !enable ) return NONE; 
  if( dch<0 )   return CHAN;
  if( difr<0 )  return IFR;
  if( dit<0 )   return TIME; 
  return NONE; 
}

Int RFDebugPlot::index ( uInt ich,uInt ifr,uInt it )
{
  if( !enable )
    return -1;
  switch( type() )
  {
    case CHAN:  return (ifr==(uInt)difr && it==(uInt)dit) ? (Int)ich : -1;
    case IFR:   return (ich==(uInt)dch  && it==(uInt)dit) ? (Int)ifr : -1;
    case TIME:  return (ich==(uInt)dch  && ifr==(uInt)difr) ? (Int)it : -1;
    default: return -1;
  }
  return -1;
}


Bool RFDebugPlot::queryPlotLimits ( Float &ymin,Float &ymax )
{
  fprintf(stderr,"Current plot limits: %g %g\nEnter new Ymax,Ymin or hit Enter to continue: ",
      ymax,ymin);
  char s[256]="";
  fgets(s,sizeof(s),stdin);
  if( strlen(s)<2 ) 
    return False;
  Float y0,y1;
  switch( sscanf(s,"%f %f",&y0,&y1) )
  {
    case 0: return False;
    case 1: ymax=y0;
           break;
    case 2: ymax=y0; ymin=y1;
           break;
  }
  return True;
}

} //# NAMESPACE CASA - END

