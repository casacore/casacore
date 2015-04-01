//# Map.cc: Associative array classes (templated)
//# Copyright (C) 1994,1995,1999,2000
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

#ifndef CASA_MAP_TCC
#define CASA_MAP_TCC

#include <casacore/casa/Containers/Map.h>


//# template<class key, class value> value &Map<key,value>::operator()(const key &ky) {
//#   value &*vptr;
//#   if (vptr = isDefined(ky))
//#     return *vptr;
//#   else
//#     return define(ky,DefaultVal);
//# }

namespace casacore { //#Begin casa namespace

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
    if (!this->isValid())
      throw_invalid_mapiter_error();
    return this->Rep->getVal();}

template<class key, class value>
const value &MapIter<key,value>::getVal() const {
    return ConstMapIter<key,value>::getVal();}

template<class key, class value>
void ConstMapIter<key,value>::toStart() {
  if (!this->isValid())
    throw_invalid_mapiter_error();
  this->Rep->toStart();
}

template<class key, class value>
const value &ConstMapIter<key,value>::defaultVal() const {
  if (!this->isValid())
    throw_invalid_mapiter_error();
  return this->Rep->defaultVal();
}

template<class key, class value>
void ConstMapIter<key,value>::operator++() {
  if (!this->isValid())
    throw_invalid_mapiter_error();
  this->Rep->operator++();
}

template<class key, class value>
void ConstMapIter<key,value>::operator++(int) {
  if (!this->isValid())
    throw_invalid_mapiter_error();
  this->Rep->operator++(0);
}

template<class key, class value>
const value &ConstMapIter<key,value>::operator()(const key &ky) const {
  if (!this->isValid())
    throw_invalid_mapiter_error();
  return(this->Rep->operator()(ky));
}

template<class key, class value>
const value *ConstMapIter<key,value>::isDefined(const key &ky) const {
  if (!this->isValid())
    throw_invalid_mapiter_error();
  return(this->Rep->isDefined(ky));
}

template<class key, class value>
uInt ConstMapIter<key,value>::ndefined() const {
  if (!this->isValid())
    throw_invalid_mapiter_error();
  return(this->Rep->ndefined());
}

template<class key, class value>
const Map<key,value> &ConstMapIter<key,value>::container() const {
    return(this->Rep->container());
  }
template<class key, class value>
const key &ConstMapIter<key,value>::getKey() const { 
  if (!this->isValid())
    throw_invalid_mapiter_error();
  return this->Rep->getKey();
}

template<class key, class value>
Bool ConstMapIter<key,value>::atEnd() const {
  if (!this->isValid())
    throw_invalid_mapiter_error();
  return this->Rep->atEnd();}

template<class key, class value>
Bool ConstMapIter<key,value>::atStart() const {
  if (!this->isValid())
    throw_invalid_mapiter_error();
  return this->Rep->atStart();
}

template<class key, class value>
Bool ConstMapIter<key,value>::isValid() const {
  return this->Rep && this->Rep->isValid() ? True : False;
}

template<class key, class value>
ConstMapIter<key,value>::ConstMapIter(const ConstMapIter<key,value> *st) {
  if (st && (*st).isValid())
    this->Rep = st->Rep->Clone();
  else 
    throw_mapiter_init_error();
}

template<class key, class value>
ConstMapIter<key,value>::ConstMapIter(const ConstMapIter<key,value> &st) {
  if (st.isValid())
    this->Rep = st.Rep->Clone();
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
  if (this->Rep)
    delete this->Rep;
}

template<class key, class value>
MapIter<key,value> &MapIter<key,value>::operator=(Map<key,value> &other) {
  this->SetRep(other.getRep());
  return *this;
}

template<class key, class value>
MapIter<key,value> &MapIter<key,value>::operator=(Map<key,value> *other) {
  if (! other)
    throw_mapiter_init_error();
  this->SetRep(other->getRep());
  return *this;
}

template<class key, class value>
MapIter<key,value> &MapIter<key,value>::operator=(const MapIter<key,value> &other) {
    if (other.isValid()) {
      this->SetRep(other.Rep->Clone());
    } else
      throw_mapiter_init_error();
    return *this;
  }

template<class key, class value>
MapIter<key,value> &MapIter<key,value>::operator=(const MapIter<key,value> *other) {
  if (other && (*other).isValid()) {
    this->SetRep(other->Rep->Clone());
  } else
    throw_mapiter_init_error();
  return *this;
}

//#
//#  - - - - - MapIterRep<k,v> - - - - -
template<class key, class value>
value &MapIterRep<key,value>::define(const key &ky, const value &val) {
  if (!this->Container || ! this->isValid())
    throw_invalid_mapiter_error();
  return((*this->Container).define(ky,val));
}

template<class key, class value>
void MapIterRep<key,value>::remove(const key &ky) {
  if (! this->Container || ! this->isValid())
    throw_invalid_mapiter_error();
  (*this->Container).remove(ky);
}

template<class key, class value>
void MapIterRep<key,value>::clear() {
  if (!this->Container || ! this->isValid())
    throw_invalid_mapiter_error();
  (*this->Container).clear();
}

template<class key, class value>
const value &MapIterRep<key,value>::operator()(const key &ky) const {
  if (this->Container == 0 || ! this->isValid())
    throw_invalid_mapiter_error();
  return((*this->Container).operator()(ky));
}

template<class key, class value>
value &MapIterRep<key,value>::operator()(const key &ky) {
  if (!this->Container || ! this->isValid())
    throw_invalid_mapiter_error();
  return((*this->Container).operator()(ky));
}

template<class key, class value>
const value *MapIterRep<key,value>::isDefined(const key &ky) const {
  if (this->Container == 0 || ! this->isValid())
    throw_invalid_mapiter_error();
  return((*this->Container).isDefined(ky));
}

template<class key, class value>
value *MapIterRep<key,value>::isDefined(const key &ky) {
  if (!this->Container || ! this->isValid())
    throw_invalid_mapiter_error();
  return((*this->Container).isDefined(ky));
}

template<class key, class value>
uInt MapIterRep<key,value>::ndefined() const {
  if (!this->Container || ! this->isValid())
    throw_invalid_mapiter_error();
  return((*this->Container).ndefined());
}

template<class key, class value>
Map<key,value> &MapIterRep<key,value>::container() {
  return *this->Container;
}

template<class key, class value>
const Map<key,value> &MapIterRep<key,value>::container() const {
  return *this->Container;
}

template<class key, class value>
value &MapIterRep<key,value>::defaultVal() {
  if (!this->Container || ! this->isValid())
    throw_invalid_mapiter_error();
  return (*this->Container).defaultVal();
}

template<class key, class value>
const value &MapIterRep<key,value>::defaultVal() const {
  if (!this->Container || ! this->isValid())
    throw_invalid_mapiter_error();
  return (*this->Container).defaultVal();
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

} //#End casa namespace

#endif
