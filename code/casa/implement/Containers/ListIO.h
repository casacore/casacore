//# ListIO.h: Singly linked list IO
//# Copyright (C) 1993,1994,1995,1999
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

#if !defined(AIPS_LISTIO_H_)
#define AIPS_LISTIO_H_

#include <aips/Containers/List.h>

class AipsIO;
// <summary>List I/O shift operators (AipsIO & ostream)</summary>
//
//
// <linkfrom anchor='List I/O' classes='List ListIter ConstListIter'>
//     I/O <here>shift operators</here> for <src>List</src>s.
// </linkfrom>
// <group name='List I/O'>

// These functions are used to read and write <src>List</src>s from
// <src>AipsIO</src>.
// <group>
template<class t> AipsIO &operator<<(AipsIO &, const List<t> &);
template<class t> AipsIO &operator>>(AipsIO &, List<t> &);
template<class t> AipsIO &operator<<(AipsIO &, const ConstListIter<t> &);
template<class t> AipsIO &operator>>(AipsIO &, ListIter<t> &);
// </group>

// These functions are used to write <src>List</src>s to an ostream.
// <group>
template<class t> ostream &operator<<(ostream &, const List<t> &);
template<class t> ostream &operator<<(ostream &, const ConstListIter<t> &);
// </group>
// </group>

#endif
