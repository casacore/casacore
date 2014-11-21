//# ListIO.h: Singly linked list IO
//# Copyright (C) 1993,1994,1995,1999,2000
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

#ifndef CASA_LISTIO_H
#define CASA_LISTIO_H

#include <casacore/casa/Containers/List.h>


namespace casa { //# NAMESPACE CASA - BEGIN

// <summary>
// Input/output operators for Lists.
// </summary>

// <use visibility=export>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=List>List</linkto>
//   <li> ostream
// </prerequisite>

// <synopsis> 
// These global functions provide easy input and output of List objects.
// </synopsis>
//
// <group name="List IO">

// These functions are used to write <src>List</src>s to an ostream.
// <group>
template<class t> ostream &operator<<(ostream &, const List<t> &);
template<class t> ostream &operator<<(ostream &, const ConstListIter<t> &);
// </group>
// </group>


} //# NAMESPACE CASA - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Containers/ListIO.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
