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

#ifndef CASA_LISTIO_TCC
#define CASA_LISTIO_TCC

#include <casacore/casa/Containers/ListIO.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

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

} //# NAMESPACE CASACORE - END


#endif
