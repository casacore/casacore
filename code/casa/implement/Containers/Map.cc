//# Map.cc: Associative array classes (templated)
//# Copyright (C) 1994,1995
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

#include <aips/Containers/Map.h>

rtti_imp_init_a2(Map);
rtti_imp_mbrf_a2(Map);
rtti_imp_init_a2(ConstMapIter);
rtti_imp_mbrf_a2(ConstMapIter);
rtti_imp_init_a2(MapIter);
rtti_imp_mbrf_a2(MapIter);


//# template<class key, class value> value &Map<key,value>::operator()(const key &ky) {
//#   value &*vptr;
//#   if (vptr = isDefined(ky))
//#     return *vptr;
//#   else
//#     return define(ky,DefaultVal);
//# }

template<class key, class value>
Map<key,value>::~Map() {
  if (Rep)
    delete Rep;
}
    
template<class key, class value>
value &Map<key,value>::operator()(const key &ky) {
  return Rep->operator()(ky);
}

template<class key, class value>
const value &Map<key,value>::operator()(const key &ky) const {
  return Rep->operator()(ky);
}


template<class key, class value>
Map<key,value> &Map<key,value>::operator=(const Map<key,value> &m) {
  if (m.Rep)
    SetRep(m.Rep->Clone());
  else
    throw_map_init_error();
  return *this;
}

template<class key, class value>
Map<key,value> &Map<key,value>::operator=(const Map<key,value> *m) {
  if (m && m->Rep) 
    SetRep(m->Rep->Clone());
  else
    throw_map_init_error();
  return *this;
}

template<class key, class value>
Map<key,value>::Map(const Map<key,value> &m) {
  if (m.Rep)
    Rep = m.Rep->Clone();
  else
    throw_map_init_error();
}

template<class key, class value>
Map<key,value>::Map(const Map<key,value> *m) {
  if (m && m->Rep) 
    Rep = m->Rep->Clone();
  else
    throw_map_init_error();
}

template<class key, class value>
void Map<key,value>::cleanup() {
  this->Map<key,value>::~Map();
}

template<class key, class value>
value &Map<key,value>::defaultVal() {
  return Rep->defaultVal();
}

template<class key, class value>
const value &Map<key,value>::defaultVal() const {
  return Rep->defaultVal();
}

template<class key, class value>
const value *Map<key,value>::isDefined(const key &k) const {
  return Rep->isDefined(k);
}

template<class key, class value>
value *Map<key,value>::isDefined(const key &k) {
  return Rep->isDefined(k);
}

template<class key, class value>
uInt Map<key,value>::ndefined() const {
  return Rep->ndefined();
}


template<class key, class value>
value &Map<key,value>::define(const key &k, const value &v) {
  return Rep->define(k,v);
}

template<class key, class value>
void Map<key,value>::remove(const key &k) {
  Rep->remove(k);
}

template<class key, class value>
void Map<key,value>::clear() {
  Rep->clear();
}

template<class key, class value>
MapIterRep<key,value> *Map<key,value>::getRep() const {
  return Rep->getRep((Map<key,value> *)this);
}

template<class key, class value> MapRep<key,value>::~MapRep() {}

template<class key, class value> value &MapRep<key,value>::operator()(const key &ky) {
  value *vptr;
  if ((vptr = isDefined(ky)))
    return *vptr;
  else
    return define(ky,DefaultVal);
}

template<class key, class value> const value &MapRep<key,value>::operator()(const key &ky) const {
  const value *vptr = isDefined(ky);
  if (! vptr)
    throw_map_constop_error();
  return *vptr;
}

template<class key, class value>
const value &ConstMapIter<key,value>::getVal() const { 
  if (!isValid())
    throw_invalid_mapiter_error();
  return Rep->getVal();
}

template<class key, class value>
value &MapIter<key,value>::getVal() {
    if (!isValid())
      throw_invalid_mapiter_error();
    return Rep->getVal();}

template<class key, class value>
const value &MapIter<key,value>::getVal() const {
    return ConstMapIter<key,value>::getVal();}

template<class key, class value>
void ConstMapIter<key,value>::toStart() {
  if (!isValid())
    throw_invalid_mapiter_error();
  Rep->toStart();
}

template<class key, class value>
const value &ConstMapIter<key,value>::defaultVal() const {
  if (!isValid())
    throw_invalid_mapiter_error();
  return Rep->defaultVal();
}

template<class key, class value>
void ConstMapIter<key,value>::operator++() {
  if (!isValid())
    throw_invalid_mapiter_error();
  Rep->operator++();
}

template<class key, class value>
void ConstMapIter<key,value>::operator++(int) {
  if (!isValid())
    throw_invalid_mapiter_error();
  Rep->operator++(0);
}

template<class key, class value>
const value &ConstMapIter<key,value>::operator()(const key &ky) const {
  if (!isValid())
    throw_invalid_mapiter_error();
  return(Rep->operator()(ky));
}

template<class key, class value>
const value *ConstMapIter<key,value>::isDefined(const key &ky) const {
  if (!isValid())
    throw_invalid_mapiter_error();
  return(Rep->isDefined(ky));
}

template<class key, class value>
uInt ConstMapIter<key,value>::ndefined() const {
  if (!isValid())
    throw_invalid_mapiter_error();
  return(Rep->ndefined());
}

template<class key, class value>
const Map<key,value> &ConstMapIter<key,value>::container() const {
    return(Rep->container());
  }
template<class key, class value>
const key &ConstMapIter<key,value>::getKey() const { 
  if (!isValid())
    throw_invalid_mapiter_error();
  return Rep->getKey();
}

template<class key, class value>
Bool ConstMapIter<key,value>::atEnd() const {
  if (!isValid())
    throw_invalid_mapiter_error();
  return Rep->atEnd();}

template<class key, class value>
Bool ConstMapIter<key,value>::atStart() const {
  if (!isValid())
    throw_invalid_mapiter_error();
  return Rep->atStart();
}

template<class key, class value>
Bool ConstMapIter<key,value>::isValid() const {
  return Rep && Rep->isValid() ? True : False;
}

template<class key, class value>
ConstMapIter<key,value>::ConstMapIter(const ConstMapIter<key,value> *st) {
  if (st && (*st).isValid())
    Rep = st->Rep->Clone();
  else 
    throw_mapiter_init_error();
}

template<class key, class value>
ConstMapIter<key,value>::ConstMapIter(const ConstMapIter<key,value> &st) {
  if (st.isValid())
    Rep = st.Rep->Clone();
  else
    throw_mapiter_init_error();
}

template<class key,class value>  
ConstMapIter<key,value>::ConstMapIter(const Map<key,value> *st) : Rep(st->getRep()) {}

template<class key,class value>  
ConstMapIter<key,value>::ConstMapIter(const Map<key,value> &st) : Rep(st.getRep()) {}

template<class key, class value>
ConstMapIter<key,value> &ConstMapIter<key,value>::operator=(const ConstMapIter<key,value> &other) {
  if (other.isValid()) 
    SetRep(other.Rep->Clone());
  else
    throw_mapiter_init_error();
  return *this;
}

template<class key, class value>
ConstMapIter<key,value> &ConstMapIter<key,value>::operator=(const ConstMapIter<key,value> *other) {
  if (other && (*other).isValid())
    SetRep(other->Rep->Clone());
  else
    throw_mapiter_init_error();
  return *this;
}

template<class key, class value>
ConstMapIter<key,value> &ConstMapIter<key,value>::operator=(const Map<key,value> &other) {
    SetRep(other.getRep());
    return *this;
  }
template<class key, class value>
ConstMapIter<key,value> &ConstMapIter<key,value>::operator=(const Map<key,value> *other) {
    if (! other)
      throw_mapiter_init_error();
    SetRep(other->getRep());
    return *this;
  }

template<class key, class value>
ConstMapIter<key,value>::~ConstMapIter() {
  if (Rep)
    delete Rep;
}

template<class key, class value>
void ConstMapIter<key,value>::cleanup(){
  this->ConstMapIter<key,value>::~ConstMapIter();
}

template<class key, class value>
MapIter<key,value> &MapIter<key,value>::operator=(Map<key,value> &other) {
  SetRep(other.getRep());
  return *this;
}

template<class key, class value>
MapIter<key,value> &MapIter<key,value>::operator=(Map<key,value> *other) {
  if (! other)
    throw_mapiter_init_error();
  SetRep(other->getRep());
  return *this;
}

template<class key, class value>
MapIter<key,value> &MapIter<key,value>::operator=(const MapIter<key,value> &other) {
    if (other.isValid()) {
      SetRep(other.Rep->Clone());
    } else
      throw_mapiter_init_error();
    return *this;
  }

template<class key, class value>
MapIter<key,value> &MapIter<key,value>::operator=(const MapIter<key,value> *other) {
  if (other && (*other).isValid()) {
    SetRep(other->Rep->Clone());
  } else
    throw_mapiter_init_error();
  return *this;
}

template<class key, class value>
void MapIter<key,value>::cleanup(){
  this->MapIter<key,value>::~MapIter();
}

//#
//#  - - - - - MapIterRep<k,v> - - - - -
template<class key, class value>
value &MapIterRep<key,value>::define(const key &ky, const value &val) {
  if (!Container || ! isValid())
    throw_invalid_mapiter_error();
  return((*Container).define(ky,val));
}

template<class key, class value>
void MapIterRep<key,value>::remove(const key &ky) {
  if (! Container || ! isValid())
    throw_invalid_mapiter_error();
  (*Container).remove(ky);
}

template<class key, class value>
void MapIterRep<key,value>::clear() {
  if (!Container || ! isValid())
    throw_invalid_mapiter_error();
  (*Container).clear();
}

template<class key, class value>
const value &MapIterRep<key,value>::operator()(const key &ky) const {
  if (Container == 0 || ! isValid())
    throw_invalid_mapiter_error();
  return((*Container).operator()(ky));
}

template<class key, class value>
value &MapIterRep<key,value>::operator()(const key &ky) {
  if (!Container || ! isValid())
    throw_invalid_mapiter_error();
  return((*Container).operator()(ky));
}

template<class key, class value>
const value *MapIterRep<key,value>::isDefined(const key &ky) const {
  if (Container == 0 || ! isValid())
    throw_invalid_mapiter_error();
  return((*Container).isDefined(ky));
}

template<class key, class value>
value *MapIterRep<key,value>::isDefined(const key &ky) {
  if (!Container || ! isValid())
    throw_invalid_mapiter_error();
  return((*Container).isDefined(ky));
}

template<class key, class value>
uInt MapIterRep<key,value>::ndefined() const {
  if (!Container || ! isValid())
    throw_invalid_mapiter_error();
  return((*Container).ndefined());
}

template<class key, class value>
Map<key,value> &MapIterRep<key,value>::container() {
  return *Container;
}

template<class key, class value>
const Map<key,value> &MapIterRep<key,value>::container() const {
  return *Container;
}

template<class key, class value>
value &MapIterRep<key,value>::defaultVal() {
  if (!Container || ! isValid())
    throw_invalid_mapiter_error();
  return (*Container).defaultVal();
}

template<class key, class value>
const value &MapIterRep<key,value>::defaultVal() const {
  if (!Container || ! isValid())
    throw_invalid_mapiter_error();
  return (*Container).defaultVal();
}

template<class key, class value>
MapIterRep<key,value>::MapIterRep(Map<key,value> *st)
: Container((Map<key,value> *) st)
{}

template<class key, class value>
MapIterRep<key,value>::MapIterRep(Map<key,value> &st)
: Container((Map<key,value> *) &st)
{}

template<class key, class value>
MapIterRep<key,value>::~MapIterRep() {}


#if defined(AIPS_STUPID_SUN)
template<class key,class value> ConstMapIter<key,value> *Map<key,value>::getIter() const {
  return new ConstMapIter<key,value>(this);
}
#endif

template<class key, class value> Map<key,value>::Map(MapRep<key,value> *nRep)
: Rep(nRep) 
{
    // Nothing
}
