//# OrderedMap.h: Templated associatve array (map) classes with ordered keys
//# Copyright (C) 1993,1994,1995,1996,1999,2000,2001
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

#ifndef CASA_ORDEREDMAP_H
#define CASA_ORDEREDMAP_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Exceptions/Error.h>
#include <casacore/casa/Containers/Block.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Containers/Map.h>
#include <casacore/casa/Containers/OrderedPair.h>
#include <casacore/casa/Utilities/Register.h>
#include <casacore/casa/Utilities/Notice.h>

namespace casacore { //#Begin casa namespace

template<class t, class v> class OrderedMap;
template<class t, class v> class OrderedMapRep;
template<class t, class v> class OrderedMapIterRep;

// <category lib=aips sect="Notice">
// <summary>Message used for OrderedMap notification</summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
// This is the message that flows between the OrderedMap
// and the OrderedMap iterators. It allows OrderedMap
// iterators to react to changes as they occur to the 
// OrderedMap.
//
template<class t,class v> class OrderedMapNotice : public Notice {
friend class OrderedMapRep<t,v>;
friend class OrderedMapIterRep<t,v>;
private:
  enum NoticeType {CLEAR,DEFINE,REMOVE,DELETE} changeType;
  uInt modPos;

  //*display 1
  //
  // This is used to construct a list notice. The parameters are:
  // <list>
  //    <item> the change modification position
  //    <item> the change type
  // </list>
  //
  OrderedMapNotice(uInt pos, NoticeType typ) : changeType(typ), modPos(pos) {}

public:
  //
  // This function returns the "Notice" type, retrieved
  // from the "type registry".
  //
  uInt type() const {return Register(this);}

  //
  // This operator can be used to compare two
  // "OrderedMapNotice"s.
  //
  int operator==(const Notice &op) const {
    if (type() != op.type()) {
      return 0;
    } else {
      const OrderedMapNotice<t,v> &opD = (const OrderedMapNotice<t,v> &) op;
      return (modPos == opD.modPos && changeType == opD.changeType);
    }
  }
};

// <summary> Representation class for an Ordered Map</summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

template<class key, class value> class OrderedMapRep : public NoticeSource, public MapRep<key,value> {
friend class OrderedMap<key,value>;
public:
  //
  //  Creates a map with the specified default value, "value", and the
  //  internal block size.
  //
  OrderedMapRep (const value&, uInt size);

  //
  //  Creates a map with the specified default value, "value".
  //
  OrderedMapRep (const value&);

  //
  // These functions check to see if a mapping is defined between
  // the specified key and some value. If one is, a pointer to
  // the value is returned, otherwise 0 is returned.
  //
  //+grp
  value *isDefined(const key&);
  const value *isDefined(const key&) const;
  //-grp

  //
  // Returns the number of user defined mappings
  //
  uInt ndefined() const;

  //
  //  Defines a mapping (ie. create a key value mapping)
  //
  value &define (const key&, const value&);

  //
  //  Undefines a mapping (ie. remove a key value mapping).
  //
  void remove (const key&);

  //
  //  Clear the entire map (ie. remove all mappings).
  //
  void clear ();

  MapIterRep<key,value> *getRep(Map<key,value> *) const;

  MapRep<key,value> *Clone() const;

  //
  //  Get the number of mappings.
  //
  uInt nused() const { return nrused; }
  uInt ntot() const { return nrtot; }

  //
  //  Get or set the Block allocation increment.
  //
  //+grp
  uInt incr() const { return nrincr; }
  uInt incr(uInt nri) { return (nrincr = nri); }
  //-grp

  //
  //  Removes a map.
  //
  ~OrderedMapRep ();

  enum {OrderedMapRepVersion = 1};

protected:
    //  The blocks to hold the keys and values
    //  and the total, used and increment size of these blocks.
    PtrBlock<OrderedPair<key,value>*> kvblk;
    uInt nrtot;
    uInt nrused;
    uInt nrincr;

    //  The index of the last key used.
    uInt lastRef;

    //  Binary search for the key.
    Int findKey (const key&, Bool&) const;
};

//
// <category lib=aips sect="Containers">
// <summary>Map with keys ordered</summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
// OrderedMap<key,value> is a template class derived from Map.
// It is similar to ListMap, but the keys are kept in order and
// they have to be unique.
//
// It uses a Block to store an array of pointers to the keys and
// the associated values.
// The keys and values themselves are stored on the heap.
// The keys are kept in order to allow a binary search through
// the keys for rapid access.
//
// This is one (simple) implementation of an ordered map.
// It is not suitable for large arrays of keys, since the overhead
// of keeping the keys in order would get too big.
// For large arrays a red-black tree implementation would be better.
//
// Exceptions are raised when new[] is failing, when the next()
// getKey() or getValue() function is failing or when a duplicate key
// is defined.
//
// The AipsIO >> and << operators are defined in <aips/OrdMapIO.h>.
//
template<class key, class value> class OrderedMap : public Map<key,value> {
friend class OrderedMapIterRep<key,value>;
protected:

  void throwgetKey(uInt) const;
  void throwgetValue(uInt) const;

  value &getVal(uInt inx) {
    if (!this->Rep || inx >= nused())
      throwgetValue(inx);
    return (((OrderedMapRep<key,value> *)(this->Rep))->kvblk[inx]->y());
  }

  const value &getVal(uInt inx) const {
    if (!this->Rep || inx >= nused())
      throwgetValue(inx);
    return (((OrderedMapRep<key,value> *)(this->Rep))->kvblk[inx]->y());
  }

  key &getKey (uInt inx) {
    if (!this->Rep || inx >= nused())
	throwgetKey(inx);
    return (((OrderedMapRep<key,value> *)(this->Rep))->kvblk[inx]->x());
  }

  const key &getKey (uInt inx) const {
    if (!this->Rep || inx >= nused())
	throwgetKey(inx);
    return (((OrderedMapRep<key,value> *)(this->Rep))->kvblk[inx]->x());
  }

public:
  //
  //  Creates a map with the specified default value, "value", and the
  //  internal block size.
  //
  OrderedMap (const value& dflt, uInt size) : Map<key,value>(new OrderedMapRep<key,value>(dflt,size)) { }

  //
  //  Creates a map with the specified default value, "value".
  //
  explicit OrderedMap (const value& dflt) : Map<key,value>(new OrderedMapRep<key,value>(dflt)) { }

  //
  //  Creates a map from another one; use copy semantics.
  //
  OrderedMap (const OrderedMap<key,value>& other) : Map<key,value>(other.Rep->Clone()) { }

  //
  // Does nothing, the destruction is taken care of in the base class, i.e. the
  // letter contains the guts.
  //
  ~OrderedMap();

  //
  //  Assigns this OrderedMap to another with copy semantics.
  //
  OrderedMap<key,value>& operator= (const OrderedMap<key,value>& other) {
    this->SetRep(other.Rep->Clone());
    return *this;
  }

  //
  //  Get the number of mappings.
  //
  uInt nused() const { return ((OrderedMapRep<key,value> *)(this->Rep))->nused(); }
  uInt ntot() const { return ((OrderedMapRep<key,value> *)(this->Rep))->ntot(); }

  //
  //  Get or set the Block allocation increment.
  //
  //+grp
  uInt incr() const { return ((OrderedMapRep<key,value> *)(this->Rep))->incr(); }
  uInt incr(uInt nri) { return ((OrderedMapRep<key,value> *)(this->Rep))->incr(nri);}
  //-grp

  enum {OrderedMapVersion = 1};
};


//
// <category lib=aips sect="Containers">
// <summary>OrderedMap iterator "letter"</summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//
// This is the "letter" which when paired (Const)MapIter "envelope"
// allows traversal of "OrderedMap"s.
//
template<class key, class value> class OrderedMapIterRep : virtual public MapIterRep<key,value>, public NoticeTarget {
protected:

  //*display 4
  //
  // Throw exceptions on behalf of inline functions.
  //
  //+grp
  void thrownext() const;
  void throwInvalidIter() const;
  //-grp

  OrderedMap<key,value> *container;

  uInt CurIndex;

public:

  //
  // Checks to see if the iterator is in a valid state.
  //
  Bool isValid() const;

  //
  // Checks to see if the iterator is at one of the
  // map extremes, "atEnd()" or "atStart()".
  //
  //+grp
  Bool atEnd() const;
  Bool atStart() const;
  //-grp

  //
  // Move the iterator to the beginning of the Map.
  //
  void toStart();

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
  //+grp
  const key &getKey () const;
  const key &getKey (uInt inx) const {
    if (!container || !isValid())
      throwInvalidIter();
    return ((*container).getKey(inx));
  }
  //-grp

  //
  // Retrieve the value at the given index in the internal block
  // which stores the representation of the OrderedMap.
  //
  // <note> This should typically not be used.</note>
  //
  //+grp
  value &getVal(uInt inx) {
    if (!container || !isValid())
      throwInvalidIter();
    return ((*container).getVal(inx));
  }
  //-grp

  //
  // Retrieve the value at the current iterator position.
  //
  //+grp
  const value &getVal() const;
  const value &getVal(uInt inx) const {
    if (!container || !isValid())
      throwInvalidIter();
    return ((*container).getVal(inx));
  }

  value &getVal() {return  getVal(CurIndex);}
  //-grp


  MapIterRep<key,value> *Clone() {
    OrderedMapIterRep<key,value> *ret = new OrderedMapIterRep<key,value>(container);
    return ret;
  }

  //*display 4
  //
  // This function is the hook through which OrderedMap
  // iterators are notified of changes to the OrderedMap
  // which they observe, i.e. changes which may cause
  // require iterator update.
  //
  void notify(const Notice &);

  //
  // These constructors allow a ListMapIter to be constructed from a
  // ListMap.
  //
  //+grp
  OrderedMapIterRep(OrderedMap<key,value> *st)
    : MapIterRep<key,value>(st),
      NoticeTarget((NoticeSource *)((OrderedMapRep<key,value> *) st->Rep)),
      container(st),
      CurIndex(0)
    {}

  OrderedMapIterRep(OrderedMap<key,value> &st)
    : MapIterRep<key,value>(st),
      NoticeTarget((NoticeSource *)((OrderedMapRep<key,value> *) st.Rep)),
      container(&st),
      CurIndex(0)
    {}
  //-grp

  enum {OrderedMapIterRepVersion = 1};

};

} //#End casa namespace
#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Containers/OrderedMap.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
