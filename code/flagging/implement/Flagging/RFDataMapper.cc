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
#include <aips/Exceptions/Error.h>
#include <trial/MeasurementEquations/VisBuffer.h>
#include <trial/MeasurementEquations/VisibilityIterator.h>

// Define functions for mapping a VisBuffer to obsevred, model and corrected
// visibility cubes
#define CubeMapper(name,method) \
  static Cube<Complex> * CubeMap##name ( VisBuffer &vb ) \
  { return &vb.method(); }
CubeMapper(Obs,visCube);
CubeMapper(Model,modelVisCube);
CubeMapper(Corrected,correctedVisCube);
#undef CubeMapper

    
RFDataMapper::RFDataMapper ( const String &colmn,DDMapper *map )
  : ddm_desc(""),
    ddm(map),
    cubemap(getCubeMapper(colmn))
{ 
}

RFDataMapper::RFDataMapper ( const String &colmn,const Vector<String> &expr )
  : expr_desc(""),
    ddm( DDFunc::getMapper(expr_desc,expr) ),  
    cubemap(getCubeMapper(colmn))
{
  ddm_desc = upcase(colmn)+" column, "+expr_desc;
}

// -----------------------------------------------------------------------
// RFDataMapper::getCubeMapper
// Maps column name to a cube mapper function
// -----------------------------------------------------------------------
RFDataMapper::CubeMapperFunc RFDataMapper::getCubeMapper( const String &column )
{ 
// setup cube mapper
  String col( upcase(column) );
  if( !col.length() || col.matches("OBS") || col.matches("DATA") )
    return &CubeMapObs;
  else if( col.matches("MODEL") )
    return &CubeMapModel;
  else if( col.matches("CORR") )
    return &CubeMapCorrected;
  else
    throw( AipsError( String("DataMapper: unknown column specified: ")+col ) );
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
  if( !ddm->reset( corr ) )
    return 0;
  return (RFlagWord) ddm->corrMask();
}

