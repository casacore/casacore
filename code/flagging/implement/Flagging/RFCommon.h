//# RFCommon.h: this defines RFCommon
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
#ifndef FLAGGING_RFCOMMON_H
#define FLAGGING_RFCOMMON_H
    
#include <casa/Arrays/Vector.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Containers/RecordInterface.h>
#include <casa/Containers/RecordInterface.h>
#include <casa/Logging/LogIO.h>

namespace casa { //# NAMESPACE CASA - BEGIN

 // RFAs use bitwise flags
typedef uInt RFlagWord;
typedef Vector<RFlagWord> FlagVector;
typedef Matrix<RFlagWord> FlagMatrix;

// character constants for common agent parameters
const char 
    RF_GLISHINDEX[] = "_glishindex", // this is set automatically to indicate 1-based indexing
    
    RF_NAME[]    = "name",
    
    RF_DEBUG[]   = "debug",
    RF_PLOT[]    = "plot",
    RF_PLOTSCR[] = "plotscr",
    RF_PLOTDEV[] = "plotdev",
    RF_DEVFILE[] = "devfile",
    RF_GLOBAL[]  = "global",
    RF_TRIAL[]   = "trial",

    RF_PLOTCHAN[]= "plotchan",
    RF_ECONOPLOT[]= "econoplot",

    RF_RESET[]   = "reset",
    RF_FIGNORE[] = "fignore",
    RF_UNFLAG[]  = "unflag",
    
    RF_THR[]     = "thr",
    RF_ROW_THR[] = "rowthr",
    
    RF_HW[]      = "hw",
    RF_ROW_HW[]  = "rowhw",
    RF_ROW_DISABLE[] = "norow",

    RF_COLUMN[]  = "column",
    RF_EXPR[]    = "expr",
    
    RF_CLIP[]      = "clip",
    RF_FLAGRANGE[] = "flagrange",
    RF_MIN[]       = "min",
    RF_MAX[]       = "max",
    
    
    RF_NDEG[]    = "ndeg",
    RF_SPWID[]   = "spwid", 
    RF_FREQS[]   = "fq",
    RF_CHANS[]   = "chan",
    RF_REGION[]  = "region",
    
    RF_NBINS[]   = "nbins",
    RF_MINPOP[]  = "minpop",
    
    RF_CORR[]    = "corr",
    RF_ANT[]     = "ant",
    RF_FIELD[]   = "field",
    RF_BASELINE[] = "baseline",
    RF_AUTOCORR[] = "autocorr",
    
    RF_TIMERANGE[] = "timerng",
    RF_CENTERTIME[] = "timeslot",
    RF_TIMEDELTA[] = "dtime",
    
    RF_QUACK[] = "quack",

    RF_NCHAN[] = "nchan",
    RF_START[] = "start",
    RF_STEP[] = "step",
    RF_MODE[] = "mode",
    RF_MSSELECT[] = "msselect";

// <summary>
// FlaggerEnums: collection of enums for various flagger classes
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="" demos="">
// </reviewed>
class FlaggerEnums 
{
public:
  typedef enum 
    { POLZN = 0,POL=POLZN,CORR=POLZN,
      CHAN  = 1,
      IFR   = 2,
      TIME  = 3,
      ROW   = 4,
      ANT   = 5,
      NONE  = -1,
      Num_StatEnums=6 
    } StatEnums;

  typedef enum 
  {
    FL_HONOR  = 0,
    FL_IGNORE = 1,
    FL_RESET  = 2
  } PreFlagPolicy;
};

    
// <summary>
// PGPlotEnums: enums for various PGPlot indices
// </summary>
//
// <synopsis>
// This really belongs somewhere else...
// </synopsis>
//
class PGPlotEnums {
  public:
    typedef enum {
      BOX      = 0,
      DOT      = 1,
      PLUS     = 2,
      ASTERIX  = 3,
      CIRCLE   = 4,
      CROSS    = 5,
      BOX2     = 6,
      TRIANGLE   = 7,
      CIRCLEPLUS = 8,
      CIRCLEDOT  = 9,
      PILLOW     = 10,
      DIAMOND    = 11,
      STAR5      = 12,
      FTRIANGLE  = 13,
      FATPLUS    = 14,
      DAVIDSTAR  = 15,
      FBOX       = 16,
      FCIRCLE    = 17,
      FSTAR5     = 18,
      BIGBOX     = 19,
      CIRCLE1    = 20,
      CIRCLE2    = 21,
      CIRCLE3    = 22,
      CIRCLE4    = 23,
      CIRCLE5    = 24,
      CIRCLE6    = 25,
      CIRCLE7    = 26,
      CIRCLE8    = 27,
      LEFTARROW  = 28,
      RIGHTARROW = 29,
      UPARROW    = 30,
      DOWNARROW  = 31,
      DOT1       = -1,
      DOT2       = -2,
      FTRIANGLE1 = -3,
      FDIAMOND   = -4,
      FPENTAGON  = -5,
      FHEXAGON   = -6,
      FHEPTAGON  = -7,
      FOCTAGON   = -8
    } SymbolEnums;
    
    typedef enum {
      WHITE      = 0,
      BLACK      = 1,
      RED        = 2,
      GREEN      = 3,
      BLUE       = 4,
      CYAN       = 5,
      MAGENTA    = 6,
      YELLOW     = 7,
      ORANGE     = 8,
      GREEN1     = 9,
      GREEN2     = 10,
      BLUE1      = 11,
      PURPLE     = 12,
      MAGENTA1   = 13,
      DARKGREY   = 14,
      LIGHTGREY  = 15
    } ColorEnums;
    
    typedef enum {
      LINE_FULL    = 1,
      LINE_DASH    = 2,
      LINE_DOTDASH = 3,
      LINE_DOT     = 4,
      LINE_DASHDDD = 5
    } LineStyleEnums;
};

// short inline function for checking the type of a record field
inline Bool fieldType ( const RecordInterface &parm,const String &id,DataType type,DataType type2 = TpNumberOfTypes )
{
  if( !parm.isDefined(id) || !parm.shape(id).product() )
    return False;
  DataType t = parm.dataType(id);
  return t==type || t==type2;
}
    
// short inline function for checking a field's data type against some function
inline Bool isField ( const RecordInterface &parm,const String &id,Bool (*func)(DataType) )
{
  if( !parm.isDefined(id) )
    return False;
  DataType type = parm.dataType(id);
  return (*func)(type);
}

// short inline function for checking that a field is a non-empty record
inline Bool isValidRecord ( const RecordInterface &parm,const String &id)
{
  if( !parm.isDefined(id) || parm.dataType(id) != TpRecord )
    return False;
  return parm.asRecord(id).nfields() > 0;
}
    
// Short inline function for checking if a record field is "set",
// i.e. exists, and is not an empty array or a boolean False.
inline Bool isFieldSet ( const RecordInterface &parm,const String &id )
{
  return parm.isDefined(id) && parm.shape(id).product() && ( parm.dataType(id) != TpBool || parm.asBool(id) );
}
    
// Short inline function for returning the number of elements in a field
// (0 for no field, 1 for scalar, >1 for arrays)
inline uInt fieldSize ( const RecordInterface &parm,const String &id )
{
  return parm.isDefined(id) ? parm.shape(id).product() : 0;
}

// Basically just upcase(), but in a form suitable for Array::apply
inline String stringUpper ( const String &in )
{
  return upcase(in);
}

// a debug-printf function, for printf-ing debug messages
int dprintf( LogIO &os, const char *format, ... );


} //# NAMESPACE CASA - END

#endif
