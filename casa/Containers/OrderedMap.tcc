//# OrderedMap.cc: Templated associatve array (map) classes with ordered keys
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

#ifndef CASA_ORDEREDMAP_TCC
#define CASA_ORDEREDMAP_TCC

#include <casacore/casa/Containers/OrderedMap.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//rtti_imp_mbrf_a2(OrderedMap);

template<class key, class value>
OrderedMapRep<key,value>::OrderedMapRep (const value& dflt, uInt incr)
: MapRep<key,value>(dflt),
  kvblk  (incr),
  nrtot  (incr),
  nrused (0),
  nrincr (incr),
  lastRef(0)
{}

template<class key, class value>
OrderedMapRep<key,value>::OrderedMapRep (const value& dflt)
: MapRep<key,value>(dflt),
  kvblk  (10),
  nrtot  (10),
  nrused (0),
  nrincr (10),
  lastRef(0)
{}

template<class key, class value>
MapRep<key,value> *OrderedMapRep<key,value>::Clone () const
{
  OrderedMapRep<key,value> *ret = new OrderedMapRep<key,value>(this->DefaultVal);
//lastRef = 0;
  ret->nrtot   = ret->kvblk.nelements();
  if (ret->nrtot < nrtot) {
    ret->kvblk.resize (nrtot, False, False);      //# don't copy data
    ret->nrtot = nrtot;
  }
  ret->nrused  = nrused;
  ret->nrincr  = nrincr;
  for (uInt i = 0; i<nrused; i++) {
    ret->kvblk[i] = new OrderedPair<key,value>(kvblk[i]->x(), kvblk[i]->y());
  }
  return ret;
}

template<class key, class value>
OrderedMapRep<key,value>::~OrderedMapRep () { 
  OrderedMapNotice<key,value> note(0,OrderedMapNotice<key,value>::DELETE);
  notify(note);
  clear(); 
}

template<class key, class value>
void OrderedMapRep<key,value>::clear () {

    OrderedMapNotice<key,value> note(0,OrderedMapNotice<key,value>::CLEAR);
    notify(note);

    for (uInt i=0; i<nrused; i++) {
        delete kvblk[i];
    }
    nrused = 0;
}

template<class key, class value>
MapIterRep<key,value> *OrderedMapRep<key,value>::getRep(Map<key,value> *container) const {
  OrderedMapIterRep<key,value> *ret = new OrderedMapIterRep<key,value>((OrderedMap<key,value> *)container);
  return(ret);
}

//#  Serial access functions.
template<class key, class value>
void OrderedMapIterRep<key,value>::toStart() {
    CurIndex = 0;
}

template<class key, class value> 
void OrderedMapIterRep<key,value>::operator++() {
  if (container && isValid()) {
    if (CurIndex >= (*container).nused())
      thrownext();
    CurIndex++;
  } else {
    throwInvalidIter();
  }
}

template<class key, class value> 
void OrderedMapIterRep<key,value>::operator++(int) {
  if (container && isValid()) {
    if (CurIndex >= (*container).nused())
      thrownext();
    CurIndex++;
  } else {
    throwInvalidIter();
  }
}

template<class key, class value>
Bool OrderedMapIterRep<key,value>::atEnd() const {
  if (! container || ! isValid())
    throwInvalidIter();
  return (CurIndex<(*container).nused()  ?  False : True);
}

template<class key, class value>
Bool OrderedMapIterRep<key,value>::atStart() const {
  if (! container || ! isValid())
    throwInvalidIter();
  return (CurIndex>0  ?  False : True);
}

template<class key, class value> 
Bool OrderedMapIterRep<key,value>::isValid() const {
  //
  // Made the comparison to "<=" instead of "<" because we want a 
  // iterator which has been advanced past the end of the list to
  // continue to be valid so that "atEnd()" et al. can be called
  // on it.             -- Schiebel
  //
  return(container && CurIndex <= (*container).nused() && NoticeTarget::isValid() ? True : False);
}

//# template<class key, class value>
//# const value& OrderedMap<key,value>::operator()
//#                                 (const key& k, Bool& defined) {
//#     //  Do a binary search for the key.
//#     Int inx = findKey (k, defined);
//#     if (defined) {
//#         lastRef = inx;
//#         return (kvblk[inx]->Val);
//#     }else{
//#         return (DefaultVal);
//#     }
//# }
//# 
//# template<class key, class value>
//# const value& OrderedMap<key,value>::operator() (const key& k)
//# {
//#     Bool b;
//#     return ((*this) (k,b));
//# }

template<class key, class value> const key& OrderedMapIterRep<key,value>::getKey () const { 
  return getKey(CurIndex); 
}

template<class key, class value> const value &OrderedMapIterRep<key,value>::getVal() const { 
  return getVal(CurIndex); 
}

//# Routines to throw errors for inline functions..
//# Throws are too big for inline functions.
template<class key, class value>
void OrderedMapIterRep<key,value>::thrownext () const
    { throw(indexError<Int>(CurIndex, "OrderedMap-next")); }
template<class key, class value>
void OrderedMapIterRep<key,value>::throwInvalidIter() const
    { throw(indexError<Int>(CurIndex, "Invalid Iterator")); }


template<class key, class value>
void OrderedMap<key,value>::throwgetKey (uInt inx) const
    { throw(indexError<Int>(inx, "OrderedMap-getKey")); }
template<class key, class value>
void OrderedMap<key,value>::throwgetValue (uInt inx) const
    { throw(indexError<Int>(inx, "OrderedMap-getValue")); }

template<class key, class value> OrderedMap<key,value>::~OrderedMap()
{
    // Nothing
}

//# template<class key, class value>
//# OrderedMap<key,value>& OrderedMap<key,value>::operator=
//#                                    (const OrderedMap<key,value>& other)
//# {
//#     OrderedMapNotice<key,value> note(0,OrderedMapNotice<key,value>::CLEAR);
//#     notify(note);
//#     
//#     Int i;
//#     if (&other == this) {
//#         return *this;
//#     }
//#     // First delete the current key,value pairs.
//#     // Then resize the block and copy all keys and values.
//#     for (i=0; i<nrused; i++) {
//#         delete kvblk[i];
//#     }
//#     if (nrtot < other.nrused) {
//#         nrtot = other.nrused;
//#         kvblk.resize (nrtot);
//#     }
//#     for (i=0; i<other.nrused; i++) {
//#         kvblk[i] = new OrderedPair<key,value>
//#                                     (other.kvblk[i]->Key, other.kvblk[i]->Val);
//#     }
//#     nrused  = other.nrused;
//#     lastRef = 0;
//#     return *this;
//# }


template<class key, class value>
Int OrderedMapRep<key,value>::findKey (const key& k, Bool& defined) const {
    //  Do a binary search for the key. Return the index of the first
    //  key which is >= the key searched for.
    //  In case of insertion this gives the index where to insert.
    Int st = 0;
    Int ent= nrused-1;
    Int i  = 0;

    defined = False;
    while (st<=ent) {
        i = (st+ent)/2;
        if (k < kvblk[i]->x()) {
            ent = i-1;
        }else{
	    if (k > kvblk[i]->x()) {
                i++;
                st = i;
            }else{
                defined = True;
                ent     = -1;
            }
        }
    }
    return (i);
}


template<class key, class value>
value *OrderedMapRep<key,value>::isDefined (const key& k)
{
    //  Locate the key. Return pointer to the value if the key exists.
    Bool defined;
    Int inx = findKey (k, defined);
    if (defined) {
        return(&(*kvblk[inx]).y());
    } else {
        return 0;
    }
}

template<class key, class value>
const value *OrderedMapRep<key,value>::isDefined (const key& k) const {
    //  Locate the key. Return pointer to the value if the key exists.
    Bool defined;
    Int inx = findKey (k, defined);
    if (defined) {
        return(&(*kvblk[inx]).y());
    } else {
        return 0;
    }
}

template<class key, class value>
uInt OrderedMapRep<key,value>::ndefined () const {
  return nused();
}

template<class key, class value>
value &OrderedMapRep<key,value>::define (const key& k, const value& v)
{
    //  Locate the key.  Replace if the key already exists.
    Int  i;
    Bool defined;
    Int inx = findKey (k, defined);
    if (defined) {
        delete kvblk[inx];
	kvblk[inx] = new OrderedPair<key,value> (k,v);
    }else{
        //  Extend the blocks if full.
        if (nrused==nrtot) {
            nrtot += nrincr;
	    kvblk.resize (nrtot);
        }
	OrderedMapNotice<key,value> note(inx,OrderedMapNotice<key,value>::DEFINE);
	notify(note);
        //  Shift the keys and values to the right and insert the new ones.
        for (i = nrused; i>inx; i--) {
	    kvblk[i] = kvblk[i-1];
        }
        kvblk[inx] = new OrderedPair<key,value> (k,v);
        nrused++;
    }
    return((*kvblk[inx]).y());
}

template<class key, class value>
void OrderedMapRep<key,value>::remove (const key& k)
{
    //  Locate the key. Error if the key does not exist.
    Bool defined;
    Int inx = findKey (k, defined);
    if (!defined) {
	throw(indexError<key>(k, "OrderedMap-remove"));
    }else{
        OrderedMapNotice<key,value> note(inx,OrderedMapNotice<key,value>::REMOVE);
        notify(note);
        //  Remove the key and value and shift the others to the left.
	//  Set lastRef to the previous key to keep the next loop valid.
        delete kvblk[inx];
        nrused--;
        for (uInt i = inx; i<nrused; i++) {
            kvblk[i] = kvblk[i+1];
        }
	lastRef = inx-1;
    }
}


template<class key, class value>
void OrderedMapIterRep<key,value>::notify(const Notice &note) {
  if (Register(static_cast<OrderedMapNotice<key,value> *>(0)) == note.type()) {
    const OrderedMapNotice<key,value> &opD = (const OrderedMapNotice<key,value> 
&) note;
    switch (opD.changeType) {
    case OrderedMapNotice<key,value>::DELETE:
      CurIndex = 0;
      invalidate();
      break;
    case OrderedMapNotice<key,value>::CLEAR:
      CurIndex = 0;
      break;
    case OrderedMapNotice<key,value>::DEFINE:
      if (CurIndex >= opD.modPos)
        CurIndex++;
      break;
    case OrderedMapNotice<key,value>::REMOVE:
      if (CurIndex >= opD.modPos)
        CurIndex--;
      break;
    default:
      ;
    }
  }
}


} //# NAMESPACE CASACORE - END


#endif
