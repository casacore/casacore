//# DlistIO.cc: Doubly linked list IO
//# Copyright (C) 1993,1994,1995
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

#include <casa/Containers/DlistIO.h>
#include <casa/Containers/ListIO.h>
#include <casa/IO/AipsIO.h>

//
//  Outputs a doubly linked list to the specified AipsIO stream.
//
template<class t> AipsIO &operator<<(AipsIO &ios, const Dlist<t> &list) {
  return operator<<(ios, (const List<t> &) list);
}

      
//
//  Inputs a doubly linked list to the specified AipsIO stream.
//
template<class t> AipsIO &operator>>(AipsIO &ios, Dlist<t> &list) {
  return operator>>(ios, (List<t> &) list);
}


//
//  Outputs a doubly linked list iterator to the specified AipsIO stream.
//
template<class t> AipsIO &operator<<(AipsIO &ios, const DlistIter<t> &list) {
  return operator<<(ios,(const ListIter<t> &) list);
}

      
//
//  Inputs a doubly linked list iterator to the specified AipsIO stream.
//
template<class t> AipsIO &operator>>(AipsIO &ios, DlistIter<t> &list) {
  return operator>>(ios,(ListIter<t> &) list);
}

//
//  Outputs a doubly linked list to the specified ostream stream.
//
template<class t> ostream &operator<<(ostream &ios, const Dlist<t> &list) {
  return operator<<(ios, (const List<t> &) list);
}

//
//  Outputs a doubly linked list iterator to the specified ostream stream.
//
template<class t> ostream &operator<<(ostream &ios, const DlistIter<t> &list) {
  return operator<<(ios,(const ListIter<t> &) list);
}
