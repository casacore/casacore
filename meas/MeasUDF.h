//# MeasUDF.h: TaQL functions handling measures
//# Copyright (C) 2011
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

#ifndef MEAS_MEASUDF_H
#define MEAS_MEASUDF_H

#include <casacore/casa/aips.h>
#include <casacore/meas/MeasUDF/EpochUDF.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <module>
//
// <summary>
// TaQL user defined functions handling measures
// </summary>

// <prerequisite>
//   <li> UDFBase
//   <li> <linkto module="Measures:description">Measures</linkto> module
// </prerequisite>
//
// <reviewed reviewer="" date="" demos="">
// </reviewed>

// <synopsis>
// This module extends TaQL (the Table Query Language) with functions handling
// measures. Currently it can handle directions, epochs, and positions.
//
// These functions make it possible to convert one or more measures from
// one reference type and frame to another. For example, to convert a
// direction from J2000 to apparent one can specify the direction in J2000
// as well as a time and position to define the measure frame like:
// <srcblock>
//     calc meas.app ([4h23m32.7, 34d11m54.8], "J2000",
//                    datetime(), "UTC", POSITION) from my.ms/ANTENNA
// </srcblock>
// The above example converts the given J2000 direction to apparent coordinates
// for the given time (current time is used) and for all positions in the
// POSITION column in the given ANTENNA table.
//
// As shown in the example an argument of a <src>meas</src> function can be
// a constant, a table column in a table. or any expression.
// If a table column is given, it is recognized if the column has a reference
// type attached to it (using the TableMeasures). In this example it
// would be recognized that the positions in the POSITION column are given
// as, say, WGS84.
// <br>For constants the reference type can be given in case it differs from
// the default type. In the example UTC is specified for the time (was not
// necessary because it is the default).

// <note>
// The meas library will be loaded dynamically by TaQL when such a function
// is used. Therefore it is important that the library and the other casacore
// libraries are built shared.
// <br>It is also important that the library can be found in the
// (DY)LD_LIBRARY_PATH. 
// </note>
// </synopsis> 
//
// <motivation>
// It is very handy to be able to convert emasures in tools that
// deal with various table columns (e.g. TaQL, TablePlot, pyrap).
// </motivation>

//# <todo asof="1997/02/01">
//# </todo>

// </module>


} //# NAMESPACE CASACORE - END

#endif
