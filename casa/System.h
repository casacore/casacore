//# System.h: System related classes.
//# Copyright (C) 2005
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


#ifndef CASA_SYSTEM_H
#define CASA_SYSTEM_H

#include <casacore/casa/aips.h>

#include <casacore/casa/System/Aipsrc.h>
#include <casacore/casa/System/AipsrcValue.h>
#include <casacore/casa/System/AipsrcVector.h>
#include <casacore/casa/System/AppInfo.h>
#include <casacore/casa/System/Choice.h>
#include <casacore/casa/System/ObjectID.h>
#include <casacore/casa/System/PGPlotter.h>
#include <casacore/casa/System/PGPlotterInterface.h>
#include <casacore/casa/System/ProgressMeter.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <module>
//
// <summary> Classes and global functions for system use </summary>

// <reviewed reviewer="" date="" demos="">
// </reviewed>

// <synopsis>
//
// This module is a bag of related systems classes and
// global functions.
//
// The following functionality is available:
// <ul>
//  <li> Class <linkto class=Aipsrc:description>
//       Aipsrc</linkto>
//       to read the aipsrc general resource files.
//  <li> Class <linkto class=AipsrcValue:description>
//       AipsrcValue</linkto>
//       to read values from the Aipssrc general resource files.
//  <li> Class <linkto class=AipsrcVector:description>
//       AipsrcVector</linkto>
//       to read multiple values from the Aipssrc general resource files.
//  <li> Class <linkto class=AppInfo:description>
//       AppInfo</linkto>
//       to hold general information for application.
//  <li> Class <linkto class=Choice:description>
//       Choice</linkto>
//       to ask a user a choice.
//  <li> Class <linkto class=ObjectID:description>
//       ObjectID</linkto>
//       to hold a unique identifier for distributed and other objects.
//  <li> Class <linkto class=PGPlotter:description>
//       PGPlotter</linkto>
//       to offer a standard plotting object for application programmers.
//       using <linkto class=PGPlotterInterface:description>
//       PGPlotterInterface</linkto> as its abstract base class.
//  <li> Class <linkto class=ProgressMeter:description>
//       ProgressMeter</linkto>
//       to offer visual indication of a tasks progress.
// </ul>
//
// <note role=tip> You may want to look at the individual header files
// to see whether you might not prefer to include only the header
// files you really need; it may be more efficient to do so.
// </note>
//
// </synopsis>

//# <todo asof="2005/06/08">
//#   <li>
//# </todo>

// </module>


} //# NAMESPACE CASACORE - END

#endif

