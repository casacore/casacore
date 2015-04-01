//# HashMap.cc: this defines HashMap, which is a hashed associative array
//# Copyright (C) 1995,1996,1998
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

#ifndef CASA_HASHMAPITER_TCC
#define CASA_HASHMAPITER_TCC
#include <casacore/casa/Containers/HashMapIter.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class key, class val> void ConstHashMapIter<key,val>::toStart() {
    if (!isValid())
	throw_invalid_hashmapiter_error();
    uInt i;
    for (i=0; i < Container->availableBuckets() && ! Container->blk[i]; i++) ;

    curBucket = i;
    if ( curBucket < Container->availableBuckets() ) {
	iter = Container->blk[curBucket];
	iter.toStart();
	if ( iter.atEnd() )
	    step();
	atEnd_ = False;
    } else
	atEnd_ = True;
}

template<class key, class val> void ConstHashMapIter<key,val>::step() {
    if ( ! isValid() )
	throw_invalid_hashmapiter_error();
    if ( ! iter.isValid() )
	throw_invalid_hashmapiter_error();

    if ( ! iter.atEnd() )
	++iter;
    if ( iter.atEnd() ) {
        uInt i;
	for (i=curBucket+1; i < Container->availableBuckets() && ! Container->blk[i]; i++) ;
	curBucket = i;
	if ( curBucket < Container->availableBuckets() ) {
	    iter = Container->blk[curBucket];
	    iter.toStart();
	    if ( iter.atEnd() )
		step();
	} else
	    atEnd_ = True;
    }
}

template<class key, class val> const key &ConstHashMapIter<key,val>::getKey() const {

    if ( iter.atEnd() && iter.isValid() )
	((ConstHashMapIter<key,val>*)this)->step();

    if ( ! isValid() || ! iter.isValid() || atEnd() )
	throw_invalid_hashmapiter_error();

    return iter.getRight().x();
}

template<class key, class val> const val &ConstHashMapIter<key,val>::getVal() const {

    if ( iter.atEnd() && iter.isValid() )
	((ConstHashMapIter<key,val>*)this)->step();

    if ( ! isValid() || ! iter.isValid() || atEnd() )
	throw_invalid_hashmapiter_error();

    return iter.getRight().y();
}

template<class key, class val> Bool ConstHashMapIter<key,val>::atStart() const {
    if ( ! isValid() )
	return False;
    if ( ! iter.isValid() )
	return False;
    uInt i;
    for (i=0; i < Container->availableBuckets() && 
		       (! Container->blk[i] || ! Container->blk[i]->len()) ; i++) ;

    if ( i < Container->availableBuckets() )
	return iter.container() == Container->blk[i] && iter.atStart() ? True : False;
    else
	return True;
}

template<class key, class val> ConstHashMapIter<key,val>::ConstHashMapIter(const HashMap<key,val> &st) :
  Container((HashMap<key,val>*)&st),
  curBucket(0),
  atEnd_(False)
{
    toStart();
}
 
template<class key, class val>
ConstHashMapIter<key,val> &ConstHashMapIter<key,val>::operator=(const HashMap<key,val> &other) {
    Container = (HashMap<key,val>*) &other;
    toStart();
    return *this;
}
    
template<class key, class val>
ConstHashMapIter<key,val>::ConstHashMapIter(const ConstHashMapIter<key,val> &st) :
  iter(st.iter),
  Container(st.Container),
  curBucket(st.curBucket),
  atEnd_(st.atEnd_)
{ }

template<class key, class val>
ConstHashMapIter<key,val> &ConstHashMapIter<key,val>::operator=(const ConstHashMapIter<key,val> &other) {
    if ( this != &other ) {
	iter = other.iter;
	curBucket = other.curBucket;
	atEnd_ = other.atEnd_;
	Container = other.Container;
    }
    return *this;
}

template<class key, class val> ConstHashMapIter<key,val>::~ConstHashMapIter() {}

//--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 

template<class key, class val> val &HashMapIter<key,val>::getVal() {

    if ( this->iter.atEnd() && this->iter.isValid() )
	((HashMapIter<key,val>*)this)->step();

    if ( ! this->isValid() || ! this->iter.isValid() || this->atEnd() )
	throw_invalid_hashmapiter_error();

    return this->iter.getRight().y();
}

template<class key, class val> const val &HashMapIter<key,val>::getVal() const {
    return ConstHashMapIter<key,val>::getVal();
}

// template<class key, class val> val &HashMapIter<key,val>::define(const key &k, const val &v) {
//     if (!isValid())
//       throw_invalid_hashmapiter_error();
//     return(Rep->define(k,v));
// }

template<class key, class val> HashMapIter<key,val> &HashMapIter<key,val>::operator=(HashMap<key,val> &other) {
    ConstHashMapIter<key,val>::operator=(other);
    return *this;
}

template<class key, class val>
HashMapIter<key,val> &HashMapIter<key,val>::operator=(const HashMapIter<key,val> &other) {
    if (! other.isValid())
	throw_invalid_hashmapiter_error();
    ConstHashMapIter<key,val>::operator=(other);
    return *this;
}

template<class key, class val> HashMapIter<key,val>::~HashMapIter() {}

} //# NAMESPACE CASACORE - END


#endif
