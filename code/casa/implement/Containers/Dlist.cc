//# Dlist.cc: Doubly linked list
//# Copyright (C) 1993,1994,1995,1996,2000,2001
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

#include <casa/Containers/Dlist.h>
#include <casa/iostream.h>

extern int Dlist_deprecated_flag;

template<class t>
Dlist<t>::Dlist() : List<t>() {
    if ( ! Dlist_deprecated_flag ) {
	cerr << "ERROR: Dlist<t> is deprecated, List<t> should be used instead" << endl;
	Dlist_deprecated_flag++;
    }
}

extern int ConstDlistIter_deprecated_flag;

template<class t>
ConstDlistIter<t>::ConstDlistIter(const ConstDlistIter<t> *other) : ConstListIter<t>(other) {
    if ( ! ConstDlistIter_deprecated_flag ) {
	cerr << "ERROR: ConstDlistIter<t> is deprecated, ConstListIter<t> should be used instead" << endl;
	ConstDlistIter_deprecated_flag++;
    }
}

template<class t>
ConstDlistIter<t>::ConstDlistIter(const ConstDlistIter<t> &other) : ConstListIter<t>(other) {
    if ( ! ConstDlistIter_deprecated_flag ) {
	cerr << "ERROR: ConstDlistIter<t> is deprecated, ConstListIter<t> should be used instead" << endl;
	ConstDlistIter_deprecated_flag++;
    }
}

template<class t>
ConstDlistIter<t>::ConstDlistIter() : ConstListIter<t>() {
    if ( ! ConstDlistIter_deprecated_flag ) {
	cerr << "ERROR: ConstDlistIter<t> is deprecated, ConstListIter<t> should be used instead" << endl;
	ConstDlistIter_deprecated_flag++;
    }
}

template<class t>
ConstDlistIter<t>::ConstDlistIter(const Dlist<t> *st) : ConstListIter<t>(st) {
    if ( ! ConstDlistIter_deprecated_flag ) {
	cerr << "ERROR: ConstDlistIter<t> is deprecated, ConstListIter<t> should be used instead" << endl;
	ConstDlistIter_deprecated_flag++;
    }
}

template<class t>
ConstDlistIter<t>::ConstDlistIter(const Dlist<t> &st) : ConstListIter<t>(st) {
    if ( ! ConstDlistIter_deprecated_flag ) {
	cerr << "ERROR: ConstDlistIter<t> is deprecated, ConstListIter<t> should be used instead" << endl;
	ConstDlistIter_deprecated_flag++;
    }
}

template<class t>
ConstDlistIter<t> &ConstDlistIter<t>::operator=(const Dlist<t> &other) {
  ConstListIter<t>::operator=(&other);
  return *this;
}

template<class t>
ConstDlistIter<t> &ConstDlistIter<t>::operator=(const Dlist<t> *other) {
  ConstListIter<t>::operator=(other);
  return *this;
}

template<class t>
ConstDlistIter<t> &ConstDlistIter<t>::operator=(const ConstDlistIter<t> &other) {
  ConstListIter<t>::operator=(&other);
  return *this;
}

template<class t>
ConstDlistIter<t> &ConstDlistIter<t>::operator=(const ConstDlistIter<t> *other) {
  ConstListIter<t>::operator=(other);
  return *this;
}

extern int DlistIter_deprecated_flag;

template<class t>
DlistIter<t>::DlistIter(Dlist<t> &st) : ListIter<t>(st), ConstDlistIter<t>(st), ConstListIter<t>(st) {
    if ( ! DlistIter_deprecated_flag ) {
	cerr << "ERROR: DlistIter<t> is deprecated, ListIter<t> should be used instead" << endl;
	DlistIter_deprecated_flag++;
    }
}

template<class t>
DlistIter<t>::DlistIter(Dlist<t> *st, Bool OWN) : ListIter<t>(st,OWN), ConstDlistIter<t>(st), ConstListIter<t>(st) {
    if ( ! DlistIter_deprecated_flag ) {
	cerr << "ERROR: DlistIter<t> is deprecated, ListIter<t> should be used instead" << endl;
	DlistIter_deprecated_flag++;
    }
}


template<class t>
DlistIter<t>::DlistIter(const DlistIter<t> *other) : ListIter<t>(other), ConstDlistIter<t>(other),
                                         ConstListIter<t>(other) {
    if ( ! DlistIter_deprecated_flag ) {
	cerr << "ERROR: DlistIter<t> is deprecated, ListIter<t> should be used instead" << endl;
	DlistIter_deprecated_flag++;
    }
}

template<class t>
DlistIter<t>::DlistIter(const DlistIter<t> &other) : ListIter<t>(other), 
                                         ConstDlistIter<t>(other), 
                                         ConstListIter<t>(other) {
    if ( ! DlistIter_deprecated_flag ) {
	cerr << "ERROR: DlistIter<t> is deprecated, ListIter<t> should be used instead" << endl;
	DlistIter_deprecated_flag++;
    }
}

template<class t>
DlistIter<t>::DlistIter() : ListIter<t>(), ConstDlistIter<t>(), ConstListIter<t>() {
    if ( ! DlistIter_deprecated_flag ) {
	cerr << "ERROR: DlistIter<t> is deprecated, ListIter<t> should be used instead" << endl;
	DlistIter_deprecated_flag++;
    }
}

template<class t>
ConstListIter<t> &ConstDlistIter<t>::operator=(const List<t> &) {
  throw_list_init_error();
  return *this;}
template<class t>
ConstListIter<t> &ConstDlistIter<t>::operator=(const List<t> *) {
  throw_list_init_error();
  return *this;}
template<class t>
ConstListIter<t> &ConstDlistIter<t>::operator=(const ConstListIter<t> &) {
  throw_list_init_error();
  return *this;}
template<class t>
ConstListIter<t> &ConstDlistIter<t>::operator=(const ConstListIter<t> *) {
  throw_list_init_error();
  return *this;}

template<class t>
ConstListIter<t> &DlistIter<t>::operator=(const List<t> &) {
  throw_list_init_error();
  return *this;}
template<class t>
ConstListIter<t> &DlistIter<t>::operator=(const List<t> *) {
  throw_list_init_error();
  return *this;}
template<class t>
ConstListIter<t> &DlistIter<t>::operator=(const ConstListIter<t> &) {
  throw_list_init_error();
  return *this;}
template<class t>
ConstListIter<t> &DlistIter<t>::operator=(const ConstListIter<t> *) {
  throw_list_init_error();
  return *this;}

template<class t>
ListIter<t> &DlistIter<t>::operator=(List<t> &) {
  throw_list_init_error();
  return *this;}
template<class t>
ListIter<t> &DlistIter<t>::operator=(List<t> *) {
  throw_list_init_error();
  return *this;}
template<class t>
ListIter<t> &DlistIter<t>::operator=(const ListIter<t> &) {
  throw_list_init_error();
  return *this;}
template<class t>
ListIter<t> &DlistIter<t>::operator=(const ListIter<t> *) {
  throw_list_init_error();
  return *this;}

template<class t>
ConstDlistIter<t> &DlistIter<t>::operator=(const Dlist<t> &) {
  throw_list_init_error();
  return *this;}
template<class t>
ConstDlistIter<t> &DlistIter<t>::operator=(const Dlist<t> *) {
  throw_list_init_error();
  return *this;}
template<class t>
ConstDlistIter<t> &DlistIter<t>::operator=(const ConstDlistIter<t> &) {
  throw_list_init_error();
  return *this;}
template<class t>
ConstDlistIter<t> &DlistIter<t>::operator=(const ConstDlistIter<t> *) {
  throw_list_init_error();
  return *this;}
