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

#include <casa/Containers/Block.h>
#include <casa/Containers/Link.h>
#include <casa/Containers/List.h>
#include <casa/Containers/ListMap.h>
#include <casa/Containers/Map.h>
#include <casa/Containers/OrderedPair.h>
#include <casa/Containers/Record.h>
#include <casa/Containers/RecordField.h>
#include <casa/Containers/SimOrdMap.h>
#include <casa/Containers/OrderedMap.h>
#include <casa/Containers/Queue.h>
#include <casa/Containers/Stack.h>

#include <casa/Containers/BlockIO.h>
#include <casa/Containers/ListIO.h>
#include <casa/Containers/ListMapIO.h>
#include <casa/Containers/OrdPairIO.h>
#include <casa/Containers/OrdMapIO.h>
#include <casa/Containers/SimOrdMapIO.h>
#include <casa/Containers/MapIO.h>

#include <casa/Containers/StackError.h>
#include <casa/Containers/IterError.h>

namespace casa { //# NAMESPACE CASA - BEGIN

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
// </synopsis>
//
// </module>


} //# NAMESPACE CASA - END

#endif
