//# ListMap.h: Map with list ordering/operations
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

#if !defined(AIPS_LISTMAP_H_)
#define AIPS_LISTMAP_H_

#include <aips/aips.h>
#include <aips/Containers/Map.h>
#include <aips/Containers/OrderedPair.h>
#include <aips/Containers/List.h>
#include <aips/RTTI/Typeinfo.h>

template<class key, class value> class ListMap;
template<class key, class value> class ListMapIterRep;

// <summary>Representation of a ListMap class</summary>

template<class key, class value> class ListMapRep : public MapRep<key,value> {
friend class ListMap<key,value>;
public:

  enum DefineOrder {Append, Prepend, Undefined};

  //
  //  Creates a map with a particular default value
  //
  //+grp
  ListMapRep(const value &dflt)
    : MapRep<key,value>(dflt),
      list(new List<OrderedPair<key,value> >,True), 
      deforder(Undefined)
    { }

  ListMapRep(const value &dflt, DefineOrder InOrder)
    : MapRep<key,value>(dflt),
      list(new List<OrderedPair<key,value> >,True), 
      deforder(InOrder)
    { }
  //-grp

  //
  // These functions check to see if a mapping is defined between
  // the specified key and some value. If one is, a pointer to
  // the value is returned, otherwise 0 is returned.
  //
  //+grp
  value *isDefined(const key &);
  const value *isDefined(const key &) const;
  //-grp

  //
  // Returns the number of user defined mappings
  //
  uInt ndefined() const;

  //
  //  Defines a mapping (ie. create a key value mapping)
  //
  value &define(const key &k, const value &v);

  //
  //  Undefines a mapping (ie. remove a key value mapping).
  //
  void remove(const key &);

  //
  // Clear all of the mappings.
  //
  void clear();

  MapIterRep<key,value> *getRep(Map<key,value> *) const;

  MapRep<key,value> *Clone() const;

  //
  // Set/Get the insertion order
  //
  //+grp
  DefineOrder getOrder() const { return deforder;}
  void setOrder(DefineOrder or) { deforder = or;}
  //-grp

  enum {ListMapRepVersion = 1};

protected:
  //
  //  The list used to store the key/value maps.
  //
  ListIter<OrderedPair<key,value> > list;

  DefineOrder deforder;

};

//
// <category lib=aips sect="Containers">
// <summary>Map with list ordering/operations</summary>
//
//  This is one possible implementation of the Map class. It uses
//  a linked list to implement an associative array. Other likely 
//  implementations are hashed, and binary tree. It can be used
//  as follows:
//  <code>
//      ListMap<int,int> map(0);
//      map.define(3,4);
//      map.define(5,6);
//      map.define(7,8);
//      cout << map(3) << endl;
//      cout << map(7) << endl;
//      map(7) = 78;
//  </code>
//
template<class key, class value> class ListMap : public Map<key,value> {
friend class ListMapIterRep<key,value>;
protected:

  ListIter<OrderedPair<key,value> > &list() {
    return ((ListMapRep<key,value> *) Rep)->list;
  }

public:

  enum DefineOrder {Append, Prepend, Undefined};

  //
  //  Creates a map with a particular default value
  //
  //+grp
  explicit ListMap(const value &dflt) : Map<key,value>(new ListMapRep<key,value>(dflt)) { }

  ListMap(const value &dflt, DefineOrder InOrder) : 
                          Map<key,value>(new ListMapRep<key,value>(dflt,
							InOrder == Append ? ListMapRep<key,value>::Append :
							InOrder == Prepend ? ListMapRep<key,value>::Prepend :
							ListMapRep<key,value>::Undefined)) { }
  //-grp

  //
  // Creates a ListMap from an existing one (copy semantics).
  //
  ListMap(const ListMap<key,value> &other) : Map<key,value>(other.Rep->Clone()) { }

  //
  // Assigns one list map to another (copy semantics).
  //
  ListMap<key,value> &operator=(const ListMap<key,value>&other) {
    SetRep(other.Rep->Clone());
    return *this;
  }

  //
  // Set/Get the insertion order
  //
  //+grp
  DefineOrder getOrder() const { 
    int v = ((ListMapRep<key,value> *) Rep)->getOrder();
    return v == ListMapRep<key,value>::Append ? Append :
           v == ListMapRep<key,value>::Prepend ? Prepend :
           Undefined;
  }
  void setOrder(DefineOrder or) { 
    ((ListMapRep<key,value> *)Rep)->setOrder(or == Append ? ListMapRep<key,value>::Append :
		  or == Prepend ? ListMapRep<key,value>::Prepend :
		  ListMapRep<key,value>::Undefined);
  }
  //-grp

  enum {ListMapVersion = 1};

};

//
// <category lib=aips sect="Containers">
// <summary>ListMap iterator "letter"</summary>
//
// This is the "letter" which when paired (Const)MapIter "envelope"
// allows traversal of "ListMap"s.
//
template<class key, class value> class ListMapIterRep : virtual public MapIterRep<key,value> {
protected:

  //  The list used to store the key/value maps.
  ListIter<OrderedPair<key,value> > listp;

public:

  //
  // Checks to see if the iterator is in a valid state.
  //
  Bool isValid() const {return(listp.isValid());}

  //
  // Checks to see if the iterator is at one of the
  // map extremes, "atEnd()" or "atStart()".
  //
  //+grp
  Bool atEnd() const {return(listp.atEnd());}
  Bool atStart() const {return(listp.atStart());}
  //-grp

  //
  // Move the iterator to the beginning of the Map.
  //
  void toStart() {listp.toStart();}

  //
  // Advance the iterator to the next key.
  //
  //+grp
  void operator++();
  void operator++(int);
  //-grp

  //
  // Retrieve the key at the current iterator position.
  //
  const key &getKey() const {
    if (listp.atEnd() == True)
      throw(ExcpError(71));
    return(((ConstListIter<OrderedPair<key,value> >) listp).getRight().x());
  }

  //
  // Retrieve the value at the current iterator position.
  //
  //+grp
  const value &getVal() const {
    if (listp.atEnd() == True)
      throw(ExcpError(71));
    return(((ConstListIter<OrderedPair<key,value> >) listp).getRight().y());
  }
  value &getVal() {
    if (listp.atEnd() == True)
      throw(ExcpError(71));
    return(listp.getRight().y());
  }
  //-grp

  MapIterRep<key,value> *Clone() {
    ListMapIterRep<key,value> *ret = new ListMapIterRep<key,value>((ListMap<key,value> *) Container);
    return ret;
  }

  //
  // These constructors allow a ListMapIter to be constructed from a
  // ListMap.
  //
  //+grp
  ListMapIterRep(ListMap<key,value> *st) : MapIterRep<key,value>(st), listp(st->list()) {}
  ListMapIterRep(ListMap<key,value> &st) : MapIterRep<key,value>(st), listp(st.list()) {}
  //-grp

  enum {ListMapIterRepVersion = 1};

};

#endif
