//# List.cc: Singly linked list classes
//# Copyright (C) 1993,1994,1995,1997,1998
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

#include <aips/Containers/List.h>
#include <aips/Containers/IterError.h>

template<class t> uInt ListNotice<t>::type() const  {
  return Register(this);
}

template<class t> int ListNotice<t>::operator==(const Notice &op) const {
  if (type() != op.type()) {
    return 0;
  } else {
    const ListNotice<t> &opD = (const ListNotice<t> &) op;
    return (mod == opD.mod && ocur == opD.ocur && oprev == opD.oprev && 
	    off == opD.off && otherOff == opD.otherOff );
  }
}

template<class t> void List<t>::added(Link<t> *prev, Link<t> *cur) {
  length++;
  if (head == 0 || prev == 0) {
    head = cur;
  }
  if (tail == 0 || tail == prev) {
    tail = cur;
  }
}

template<class t> void List<t>::removed(Link<t> *oldl, Link<t> *prev, Link<t> *cur) { 
  length--;
  if (oldl == head) head = cur;    
  if (oldl == tail) tail = prev;
}

template<class t> List<t>::List(const List<t> &other) : head(0), tail(0), length(0) {
    ConstListIter<t> iter(other);
    Link<t> *cur = 0;
    iter.toStart();
    if (! iter.atEnd()) {
	head = cur = new Link<t>(iter.getRight());
	iter++;
    }
    for (; !iter.atEnd(); iter++ )
	cur = new Link<t>(iter.getRight(),cur);
    tail = cur;
}

template<class t> List<t>::List(const List<t> *other) : head(0), tail(0), length(0) {
    ConstListIter<t> iter(other);
    Link<t> *cur = 0;
    iter.toStart();
    if (! iter.atEnd()) {
	head = cur = new Link<t>(iter.getRight());
	iter++;
    }
    for (; !iter.atEnd(); iter++ )
	cur = new Link<t>(iter.getRight(),cur);
    tail = cur;
}

template<class t> List<t> &List<t>::operator=(const List<t> &other) {
    if ( this == &other )
	return *this;
    ConstListIter<t> iter(other);
    Link<t> *cur = 0;
    if ( head ) delete head;
    head = tail = 0;
    length = 0;
    if (! iter.atEnd()) {
	head = cur = new Link<t>(iter.getRight());
	iter++;
    }
    for (; !iter.atEnd(); iter++ )
	cur = new Link<t>(iter.getRight(),cur);
    tail = cur;
    return *this;
}
    
template<class t> List<t> &List<t>::operator=(const List<t> *other) {
    if ( this == other )
	return *this;
    ConstListIter<t> iter(other);
    Link<t> *cur = 0;
    if ( head ) delete head;
    head = tail = 0;
    length = 0;
    if (! iter.atEnd()) {
	head = cur = new Link<t>(iter.getRight());
	iter++;
    }
    for (; !iter.atEnd(); iter++ )
	cur = new Link<t>(iter.getRight(),cur);
    tail = cur;
    return *this;
}

    
template<class t> List<t>::~List() {
  if (head) {
    ListNotice<t> state;
    notify(state);
    delete head;
  }
}    

template<class t>
ConstListIter<t>::ConstListIter(const List<t> *st) :  
                                     NoticeTarget((NoticeSource *)st),
                                     cur((*st).head), prev(0),
                                     curPos(0), container_((List<t> *)st) {}

template<class t>
ConstListIter<t>::ConstListIter(const ConstListIter<t> *other) : NoticeTarget((NoticeTarget *)other) {
  if (other) {
    cur = (*other).cur;
    prev = (*other).prev;
    curPos = (*other).curPos;
    container_ = (*other).container_;
  } else throw_list_init_error();
}

template<class t>
ConstListIter<t>::~ConstListIter() {}

template<class t>
ConstListIter<t> &ConstListIter<t>::operator=(const List<t> &other) {
  cur = other.head;
  prev = 0;
  curPos = 0;
  container_ = (List<t> *) &other;
  attach((List<t> &)other);
  return *this;
}

template<class t>
ConstListIter<t> &ConstListIter<t>::operator=(const List<t> *other) {
  if (other) {
    cur = (*other).head;
    prev = 0;
    curPos = 0;
    container_ = (List<t> *) other;
    attach((List<t> *)other);
  }
  return *this;
}

template<class t>
ConstListIter<t> &ConstListIter<t>::operator=(const ConstListIter<t> &other) {
  if (other.isValid()) {
    cur = other.cur;
    prev = other.prev;
    curPos = other.curPos;
    container_ = other.container_;
    link(other);
  }
  return *this;
}

template<class t>
ConstListIter<t> &ConstListIter<t>::operator=(const ConstListIter<t> *other) {
  if (other && (*other).isValid()) {
    cur = (*other).cur;
    prev = (*other).prev;
    curPos = (*other).curPos;
    container_ = (*other).container_;
    link(other);
  }
  return *this;
}


template<class t> void ConstListIter<t>::notify(const Notice &note) {
  if (Register((ListNotice<t> *) 0) == note.type()) {
    const ListNotice<t> &opD = (const ListNotice<t> &) note;
    if ( opD.mod != ListNotice<t>::DELETE ) {
      if (cur == opD.ocur && prev == opD.oprev) {
	cur = opD.ncur;
	prev = opD.nprev;
      }
      if ( curPos > opD.off ) {
	switch ( opD.mod ) {
	case ListNotice<t>::ADD:
		++curPos; break;
	case ListNotice<t>::REMOVE:
		--curPos; break;
	case ListNotice<t>::SWAP:
		curPos += opD.otherOff - opD.off; break;
	}
      }
    } else {
      invalidate();
      container_ = 0;
    }
  }
}

template<class t> uInt ConstListIter<t>::pos(uInt where) {
  int i;

  AlwaysAssert(isValid(),InvalidIterError);

  if (where > (*container_).length) 
    throw(IterBoundaryError("Iterator advanced past the end of the list."));
  if (where == curPos) 
    return(curPos);
  if (where > curPos ) {
    uInt savedPos = curPos;
    for (i = 0; i < where - savedPos && atEnd() != True; i++) 
      operator++();
  } else {
    toStart();
    for (i = 0; i < where; i++) operator++();
  }
  return(curPos);
}

template<class t>
ListIter<t>::ListIter(List<t> &st) : ConstListIter<t>(st), own(False){}

template<class t>
ListIter<t>::ListIter(const ListIter<t> &other) : ConstListIter<t>((const ConstListIter<t> &) other), own(False){}

template<class t>
ListIter<t>::~ListIter() {
  if (own && container_)
    delete container_;
}

template<class t>
ListIter<t> &ListIter<t>::assign(List<t> *other,Bool OWN) {
  if (other && own && container_) {
    delete container_;
  }
  own = OWN;
  ConstListIter<t>::operator=(other);
  return *this;
}

template<class t>
Link<t> *ListIter<t>::newLink(t &e, Link<t> *p, Link<t> *n) {
  return new Link<t>(e,p,n);
}


template<class t>
ListIter<t> &ListIter<t>::operator=(List<t> &other) {
  if (own && container_)
    delete container_;
  ConstListIter<t>::operator=(other);
  own = False;
  return *this;
}

template<class t>
ListIter<t> &ListIter<t>::operator=(List<t> *other) {
  if (other && own && container_)
    delete container_;
  ConstListIter<t>::operator=(other);
  own = False;
  return *this;
}

template<class t>
ListIter<t> &ListIter<t>::operator=(const ListIter<t> &other) {
  if (other.isValid() && own && container_)
    delete container_;
  ConstListIter<t>::operator=(other);
  own = False;
  return *this;
}

template<class t>
ListIter<t> &ListIter<t>::operator=(const ListIter<t> *other) {
  if (other && (*other).isValid() && own && container_)
    delete container_;
  ConstListIter<t>::operator=(other);
  own = False;
  return *this;
}

template<class t> void ListIter<t>::removeRight() {

  AlwaysAssert(isValid(),InvalidIterError);

  if (cur != 0) {
    Link<t> *c = cur;
    Link<t> *p = prev;
    cur = (*cur).unlink(prev);          // Link<t>::unlink: 
                                        //         * sets links for deletion
                                        //         * returns next
    (*container_).removed(c,prev,cur);   // Allow container to update
    ListNotice<t> state(ListNotice<t>::REMOVE,c,p,cur,prev,curPos);
    (*container_).notify(state);
    delete c;
  } else throw_list_end_error();
}

template<class t> void ListIter<t>::swapRight(ListIter<t> &swapee){
  if (container_ == swapee.container_) throw_list_swapright_same_error();
  Link<t> *mc = cur;
  Link<t> *mp = prev;
  Link<t> *sc = swapee.cur;
  Link<t> *sp = swapee.prev;
  Link<t> *tmpTail = (*container_).tail;
  if (prev != 0) {
    (*prev).next() = swapee.cur;
    if (swapee.cur) {
      (*swapee.cur).prev() = prev;
      (*container_).tail = (*swapee.container_).tail;
    } else {
      (*container_).tail = prev;
    }
  } else {
    (*container_).head = swapee.cur;
    if (swapee.cur) {
      (*swapee.cur).prev() = 0;
      (*container_).tail = (*swapee.container_).tail;
    } else {
      (*container_).tail = prev;
    }
  }
  Link<t> *tmp = cur;
  cur = swapee.cur;
  if (swapee.prev != 0) {
    (*swapee.prev).next() = tmp;
    if (tmp) {
      (*tmp).prev() = swapee.prev;
      (*swapee.container_).tail = tmpTail;
    } else {
      (*swapee.container_).tail = swapee.prev;
    }
  } else {
    (*swapee.container_).head = tmp;
    if (tmp) {
      (*tmp).prev() = 0;
      (*swapee.container_).tail = tmpTail;
    } else {
      (*swapee.container_).tail = swapee.prev;
    }
  }
  swapee.cur = tmp;

  // Adjust lengths
  uInt tmpLen = (*container_).length;
  (*container_).length = (curPos - 1) + swapee.len() - (swapee.pos() - 1);
  (*swapee.container_).length = (swapee.pos() - 1) + tmpLen - (curPos - 1);

  ListNotice<t> mstate(ListNotice<t>::SWAP,mc,mp,cur,prev,curPos,swapee.curPos);
  (*container_).notify(mstate);

  ListNotice<t> sstate(ListNotice<t>::SWAP,sc,sp,swapee.cur,swapee.prev,swapee.curPos,curPos);
  (*swapee.container_).notify(sstate);
}

//#
//# These functions are protected and ONLY throw an exception
//# to prevent improper initialization of a constant OrderedMapIter
//#
template<class t>
ConstListIter<t> &ListIter<t>::operator=(const List<t> &) {
  throw_list_init_error();
  return *this;}
template<class t>
ConstListIter<t> &ListIter<t>::operator=(const List<t> *) {
  throw_list_init_error();
  return *this;}
template<class t>
ConstListIter<t> &ListIter<t>::operator=(const ConstListIter<t> &) {
  throw_list_init_error();
  return *this;}
template<class t>
ConstListIter<t> &ListIter<t>::operator=(const ConstListIter<t> *) {
  throw_list_init_error();
  return *this;}


rtti_imp_init_a1(List)
rtti_imp_mbrf_a1(List)

rtti_imp_init_a1(ListIter)
rtti_imp_mbrf_a1(ListIter)
rtti_imp_init_a1(ConstListIter)
rtti_imp_mbrf_a1(ConstListIter)
