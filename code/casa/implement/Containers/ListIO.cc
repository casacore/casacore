//# ListIO.cc: Doubly linked list IO
//# Copyright (C) 1993,1994,1995,2000
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

#include <aips/Containers/ListIO.h>
#include <aips/IO/AipsIO.h>

//
//  Inputs a doubly linked list to the specified AipsIO stream.
//
template<class t> AipsIO &operator>>(AipsIO &ios, List<t> &list) {
  t val;
  uInt len;
  int vers = ios.getstart(rtti_decode(list.id()), List<t>::ListVersion);
  ListIter<t> listp = list;

  listp.toStart();
  while (listp.atEnd() == False)
    listp.removeRight();
  ios >> len;
  for (int i = 0; i<len; i++) {
    ios >> val;
    listp.addRight(val);
    listp++;
  }
  ios.getend ();
  return(ios);
}


//
//  Outputs a doubly linked list iterator to the specified AipsIO stream.
//
#define AIPS_LIST_AIPSIO_OUT(TYPE) 						\
template<class t> AipsIO &operator<<(AipsIO &ios, const TYPE<t> &list) {	\
  ConstListIter<t> listp = list;						\
										\
  ios.putstart(rtti_decode(list.id()), TYPE<t>::aips_name2(TYPE,Version) );	\
  ios << list.len() << list.pos();						\
  listp.toStart();								\
  while (listp.atEnd() == False) {						\
    ios << listp.getRight();							\
    listp++;									\
  }										\
  ios.putend ();								\
  return(ios);									\
}

AIPS_LIST_AIPSIO_OUT(List)
AIPS_LIST_AIPSIO_OUT(ConstListIter)

//
//  Inputs a doubly linked list iterator to the specified AipsIO stream.
//
template<class t> AipsIO &operator>>(AipsIO &ios, ListIter<t> &list) {
  t val;
  uInt len;
  uInt pos;
  int vers = ios.getstart(rtti_decode(((ConstListIter<t>&)list).id()));

  list.assign(new List<t>(), True);

  list.toStart();
  while (list.atEnd() == False)
    list.removeRight();
  ios >> len >> pos;
  for (int i = 0; i<len; i++) {
    ios >> val;
    list.addRight(val);
    list++;
  }
  ios.getend ();
  list.pos(pos);
  return(ios);
}

//
//  Outputs a doubly linked list to the specified ostream stream.
//
template<class t> ostream &operator<<(ostream &ios, const List<t> &list) {
  ConstListIter<t> listp = list;

  ios << "len=" << list.len();
  listp.toStart();
  while (listp.atEnd() == False) {
    ios << (char) ios.fill() << listp.getRight();
    listp++;
  }
  return(ios);
}

//
//  Outputs a doubly linked list iterator to the specified ostream stream.
//
template<class t> ostream &operator<<(ostream &ios, const ConstListIter<t> &list) {
  ConstListIter<t> listp = list;

  ios << "len=" << list.len() << (char) ios.fill() << "pos=" << list.pos();
  listp.toStart();
  while (listp.atEnd() == False) {
    ios << (char) ios.fill() << listp.getRight();
    listp++;
  }
  return(ios);
}
