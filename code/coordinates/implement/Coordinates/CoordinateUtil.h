//# CoordinateUtils.h: global functions dealing with coordinates
//# Copyright (C) 1997
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
//#
//# $Id$

#if !defined(AIPS_COORDUTIL_H)
#define AIPS_COORDUTIL_H

#if defined(_AIX)
#pragma implementation ("CoordUtil.cc")
#endif

#include <aips/aips.h>

class CoordinateSystem;

// <summary> Create default coordinate systems</summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="CoordinateSystem">CoordinateSystem</linkto>
// </prerequisite>
//
// <etymology> 
// CoordinateUtils follows the AIPS++ naming convention for global functions
// that are associated with a class.
// </etymology>
//
// <synopsis>
// This file contains declarations for global functions that manipulate
// coordinate systems. It currently contains functions for:
// <ul>
// <li> Creating a default CoordinateSystem
// <li> Adding default axes to a CoordinateSystem
// </ul>
// 
// The functions for adding default axes to a CoordinateSystem are can add
// either a RA/DEC pair of axes, a Polarisation Axis, or a Spectral Axis to
// a user supplied coordinate system. The default values for these functions
// are:
// <ul>
// <li> <src>addDirAxes</src> this adds a DirectionCoordinate with a
// reference pixel of (0,0) corresponding to an RA/DEC of (0,0) in a
// J2000 reference frame. The pixel increment is 1 arc-minute.
// <li> <src>addIQUVAxis</src> this adds a polarization axis with four
// elements corresponding to the Stokes (I,Q,U,V) components.
// <li> <src>addIAxis</src> this adds a polarization axis with one
// element corresponding to the Stokes I component only
// <li> <src>addFreqAxis</src> this adds a spectral axis with a reference
// frequency of 1.415GHz on channel 0. The channel bandwidth (pixel
// increment) is 1kHz, and the reference frame is the Local Standard of
// rest (<linkto class="MFrequency">MFrequency</linkto>::LSR). 
// </ul>
//
// The <src>defaultAxes</src> functions, create from scratch a
// CoordinateSystem using the above described <src>addXXXAxis</src>
// functions to add the required number of dimensions to the
// CoordinateSystem. Only 2, 3 or 4 dimensional coordinate systems can be
// constructed using these functions. The coordinate systems always have
// RA/Dec axes. Three dimensional Systems add a spectral axis and
// four-dimensional systems add an IQUV  polarization axis. An exception
// (AipsError) is thrown if <src>defaultAxes(uInt)</src> is called with a
// parameter that is not 2, 3, or 4. 
//
// The <src>defaultAxesXX</src> functions return the coordinate system by
// value (which involves a copy of the CoordinateSystem) and hence are not
// as effecient as the <src>addXXXAxis</src> functions.
//
// If the default axes provided by these functions are not quite what is
// required it is possible to use member functions of the 
// <linkto class="CoordinateSystem">CoordinateSystem</linkto>
// and <linkto class="Coordinate">Coordinate</linkto> classes 
// (<linkto class="DirectionCoordinate">DirectionCoordinate</linkto>,
// <linkto class="StokesCoordinate">StokesCoordinate</linkto>,
// <linkto class="SpectralCoordinate">SpectralCoordinate</linkto> etc.)
// to tweak the appropriate parameters of the specified axis.
// </synopsis>
//
// <example>
// I use these functions when creating images. 
// <srcblock>
// PagedImage(IPosition(4,256,256,4,32), defaultCoords4D(),
//            String("test.image"));
// </srcblock>
// </example>
//
// <motivation>
// I got fed up writing small functions to create coordinates when writing
// test programs involving Images. 
// </motivation>
//
// <thrown>
//    <li> AipsError
// </thrown>
//
// <todo asof="1997/01/23">
//   <li> This code does all I want at the moment
// </todo>

//  <a name=defaultAxes>
//  <linkfrom anchor=defaultAxes classes="CoordinateSystem">
//      Global functions for creating <here>default</here> coordinate systems
//  </linkfrom>

// <group name=defaultAxes>
// Add a RA/DEC pair of direction axes (ie. a DirectionCoordinate) to the
// user supplied CoordinateSystem. See the synopsis above for the current
// default values.
void addDirAxes(CoordinateSystem & coords);
// Add a Stokes I,Q,U,V axis to the user supplied CoordinateSystem.
void addIQUVAxis(CoordinateSystem & coords);
// Add a Stokes I (only) axis to the user supplied CoordinateSystem.
void addIAxis(CoordinateSystem & coords);
// Add a spectral axis to the user supplied CoordinateSystem. See the
// synopsis above for the current default values.
void addFreqAxis(CoordinateSystem & coords);
// Return a 2-dimensional coordinate system with RA/DEC axes only. 
CoordinateSystem defaultCoords2D();
// Return a 3-dimensional coordinate system with RA/DEC axes and a spectral axis.
CoordinateSystem defaultCoords3D();
// Return a 4-dimensional coordinate system with RA/DEC axes, an IQUV
// polarisation axis  and a spectral axis.
CoordinateSystem defaultCoords4D();
// Calls one of the above three functions depending of the arguement. An
// AipsError is thrown if dims is not 2, 3, or 4.
CoordinateSystem defaultCoords(uInt dims);
//  </group>

#endif
