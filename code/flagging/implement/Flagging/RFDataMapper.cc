//# RFDataMapper.cc: this defines RFDataMapper
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
#include <trial/Flagging/RFDataMapper.h> 
#include <aips/Arrays/Slice.h>
#include <aips/Exceptions/Error.h>
#include <aips/BasicMath/Math.h>
#include <aips/BasicSL/Constants.h>
#include <trial/MSVis/VisBuffer.h>
#include <trial/MSVis/VisibilityIterator.h>
#include <aips/Measures/MDirection.h>

// Define functions for mapping a VisBuffer to obsevred, model and corrected
// visibility cubes
#define CubeMapper(name,method) \
  static Cube<Complex> * CubeMap##name ( VisBuffer &vb ) \
  { return &vb.method(); }
CubeMapper(Obs,visCube);
CubeMapper(Model,modelVisCube);
CubeMapper(Corrected,correctedVisCube);
#undef CubeMapper

// a dummyRowMapper for uninitialized row mappings
Float RFDataMapper::dummyRowMapper (uInt)
{
  throw( AipsError("DataMapper: call to unititialized RowMapper") );
}

// define a bunch of row mapper for various UVW stuff
#define UVW ((*puvw)(ir))
#define UVRowMapper(name,expr) \
  Float RFDataMapper::name##_RowMapper (uInt ir) \
  { return expr; }
UVRowMapper(U,UVW(0));
UVRowMapper(V,UVW(1));
UVRowMapper(W,UVW(2));
UVRowMapper(AbsU,abs(UVW(0)));
UVRowMapper(AbsV,abs(UVW(1)));
UVRowMapper(AbsW,abs(UVW(2)));
UVRowMapper(UVD,sqrt(UVW(0)*UVW(0)+UVW(1)*UVW(1)));
UVRowMapper(UVA,atan2(UVW(0),UVW(1))/C::pi*180);
UVRowMapper(HA,sin_dec!=0 ? atan2(UVW(1)/sin_dec,UVW(0))/C::pi*180 : 0 );

// these arrays define a mapping between column names and cube mappers
const String 
       COL_ID[] = { "OBS","DATA","MODEL","CORR" };
const RFDataMapper::CubeMapperFunc 
       COL_MAP[] = { &CubeMapObs,&CubeMapObs,&CubeMapModel,&CubeMapCorrected };

// -----------------------------------------------------------------------
// RFDataMapper::getCubeMapper
// Maps column name to a cube mapper function
// -----------------------------------------------------------------------
RFDataMapper::CubeMapperFunc RFDataMapper::getCubeMapper( const String &column,Bool throw_excp )
{ 
// setup cube mapper
  if( !column.length() )
    return COL_MAP[0];
  String col( upcase(column) );
  for( uInt i=0; i<sizeof(COL_ID)/sizeof(COL_ID[0]); i++ )
    if( col.matches(COL_ID[i]) )
      return COL_MAP[i];
  if( throw_excp )
    throw( AipsError("DataMapper: unknown column "+column) );
  return NULL;
}


// -----------------------------------------------------------------------
// Constructor 1
// For visibility expressions
// -----------------------------------------------------------------------
RFDataMapper::RFDataMapper ( const String &colmn,DDMapper *map )
  : desc(""),
    ddm(map),
//    rowmapper(NULL),
    cubemap(getCubeMapper(colmn,True)),
    mytype(MAPCORR)
{ 
  sin_dec = -10;    // need not be computed by default, so set to <-1
  full_cycle = cycle_base = 0;   // by default, mapped values are not cyclic
  rowmapper = &RFDataMapper::dummyRowMapper;
}

// -----------------------------------------------------------------------
// Constructor 2
// For visibility expressions, with explicit column specification
// -----------------------------------------------------------------------
RFDataMapper::RFDataMapper ( const Vector<String> &expr0,const String &defcol )
  : expr_desc(""),
    ddm(NULL),
    cubemap(NULL),
//    rowmapper(dummyRowMapper),
    mytype(MAPCORR)
{
  sin_dec = -10;    // need not be computed by default, so set to <-1
  full_cycle = cycle_base = 0;   // by default, mapped values are not cyclic
  rowmapper = &RFDataMapper::dummyRowMapper;
  Vector<String> expr( splitExpression(expr0) );
// first, check for per-row expressions (i.e., UVW, etc.)
// at this point, assume we have a per-row expression (i.e. UVW, etc.)
  Bool absof=False;
  String el = upcase(expr(0));
  if( el == "ABS" ) 
  {
    absof=True;
    if( expr.nelements()<2 )
      throw(AipsError("DataMapper: illegal expression "+expr(0)));
    el = upcase( expr(1) );
  } 
  else if( el.matches("ABS",0) )
  {
    absof = True;
    el = el.after(3);
  }
  if( el == "U" )
    rowmapper = absof ? &RFDataMapper::AbsU_RowMapper : &RFDataMapper::U_RowMapper;
  else if( el == "V" )
    rowmapper = absof ? &RFDataMapper::AbsV_RowMapper : &RFDataMapper::V_RowMapper;
  else if( el == "W" )
    rowmapper = absof ? &RFDataMapper::AbsW_RowMapper : &RFDataMapper::W_RowMapper;
  else if( el == "UVD" || el == "UVDIST" )
    rowmapper = &RFDataMapper::UVD_RowMapper;
  else if( el == "UVA" || el == "UVANGLE" )
  {
    rowmapper = &RFDataMapper::UVA_RowMapper;
    full_cycle = 360;   // mapping into angles
    cycle_base = -180;
  }
  else if( el == "HA" || el == "HANGLE" )
  {
    sin_dec = 0; // will need to be computed
    rowmapper = &RFDataMapper::HA_RowMapper;
    full_cycle = 360;   // mapping into angles
    cycle_base = -180;
  }
  else
    rowmapper = NULL;
// have we set up a valid row mapper? Return then
  if( rowmapper )
  {
    desc = absof ? "|"+el+"|" : el;
    expr_desc = desc;
    mytype = MAPROW;
    return;
  }
// at this point, it must be a valid correlation expression
  String column(defcol);
// see if expression starts with a non-empty column specification, if so,
// remember the column, and shift it out of the expression vector
  CubeMapperFunc cm = getCubeMapper(expr(0));
  if( cm && expr(0).length() )
  {
    column = expr(0);
    expr = expr(Slice(1,expr.nelements()-1));
  }
// check if it parses to a valid DDMapper expression
  ddm = DDFunc::getMapper(expr_desc,expr);
// valid expression? Set ourselves up as a correlation mapper then
  if( ddm )
  {
    if( !cm ) // set column from defcol if not set above
      cm = getCubeMapper(defcol,True);
    cubemap = cm;
    desc = (column.length() ? "("+upcase(column)+")" : String("") )+expr_desc;
    mytype = MAPCORR;
    return;
  }
// invalid expression, so throw an exception
  String s;
  for( uInt i=0; i<expr.nelements(); i++ )
  {
    if( i )
      s+=" ";
    s+=expr(i);
  }
  throw(AipsError("DataMapper: illegal expression "+s));
}

RFDataMapper::~RFDataMapper () 
{ 
  if( ddm ) 
    delete ddm; 
};

// computes a correlations mask
RFlagWord RFDataMapper::corrMask ( const VisibilityIterator &vi )
{
  Vector<Int> corr;
  vi.corrType(corr);
// for a visibilities mapper, ask the DDMapper
  if( mytype == MAPCORR )
  {
    if( !ddm->reset( corr ) )
      return 0;
    return (RFlagWord) ddm->corrMask();
  }
// for a row mapper, flag all correlations
  else if( mytype == MAPROW )
  {
    return (1<<corr.nelements())-1;
  }
  else // paranoid case
    throw( AipsError( "DataMapper: unknown mytype") );
}

// sets up for a visbuffer
void RFDataMapper::setVisBuffer ( VisBuffer &vb )
{ 
  if( mytype == MAPCORR )
    pviscube = (*cubemap)(vb); 
  else if( mytype == MAPROW )
    puvw = &vb.uvw();
  else
    throw( AipsError( "DataMapper: unknown mytype") );
  // extract sine of declination, if needed
  if( sin_dec>=-1 )
  {
    sin_dec = sin( MDirection::Convert( vb.phaseCenter(),
                                     MDirection::Ref(MDirection::J2000)
                   )().getAngle().getBaseValue()(1) );
  }
}


