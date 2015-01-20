//# <Containers.h>:  a module for non-mathematical containers
//# Copyright (C) 1995,1999,2001
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

#ifndef CASA_CONTAINERS_H
#define CASA_CONTAINERS_H

#include <casacore/casa/aips.h>

#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/Containers/Link.h>
#include <casacore/casa/Containers/List.h>
#include <casacore/casa/Containers/ListMap.h>
#include <casacore/casa/Containers/Map.h>
#include <casacore/casa/Containers/OrderedPair.h>
#include <casacore/casa/Containers/Record.h>
#include <casacore/casa/Containers/RecordField.h>
#include <casacore/casa/Containers/SimOrdMap.h>
#include <casacore/casa/Containers/OrderedMap.h>
#include <casacore/casa/Containers/Queue.h>
#include <casacore/casa/Containers/Stack.h>

#include <casacore/casa/Containers/BlockIO.h>
#include <casacore/casa/Containers/ListIO.h>
#include <casacore/casa/Containers/ListMapIO.h>
#include <casacore/casa/Containers/OrdPairIO.h>
#include <casacore/casa/Containers/OrdMapIO.h>
#include <casacore/casa/Containers/SimOrdMapIO.h>
#include <casacore/casa/Containers/MapIO.h>

#include <casacore/casa/Containers/StackError.h>
#include <casacore/casa/Containers/IterError.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <module>
//
// <summary>
//     Non-mathematical Containers
// </summary>
//
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" demos="">
// </reviewed>
//
// <synopsis>
//    This module provides non-mathematical containers. These containers are the
//    prototypical computer science types of containers -- <linkto
//    class=Queue>queues</linkto>, <linkto class=Stack>stacks</linkto>, <linkto
//    class=List>lists</linkto>, <linkto class=Map>associative arrays</linkto>,
//    <linkto class="Record">records</linkto> and <linkto class=Block>simple
//    arrays</linkto>. These classes are useful for all of the various types of low
//    level data management. In general, these classes will have familiar semantics
//    and an unsurprising interface.
//
//    Most of the important classes in this module also have IO shift operators,
//    e.g. for <linkto file=BlockIO.h#BlockIO>writing out a Block</linkto> (simple
//    array). These operators typically allow the container (and the objects it
//    contains) to be written out to both <linkto class=AipsIO>AipsIO</linkto> and
//    the standard <em>ostream</em>.
//
//    The class Block has the option to trace (de)allocations for Blocks with
//    a size above a given threshold. It uses class MemoryTrace to log the
//    trace messages. Unlike MemoryTrace, it also works on non-Linux systems.
//    Since class Array uses Block underneath, it makes it possible to trace
//    Array usage.
// </synopsis>
//
// </module>


} //# NAMESPACE CASACORE - END

#endif
