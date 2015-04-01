//# VectorSTLIterator.h: Random access iterator for Casacore Vectors
//# Copyright (C) 2004
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

#ifndef CASA_VECTORSTLITERATOR_H
#define CASA_VECTORSTLITERATOR_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/Arrays/Vector.h>
#include <iterator>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# Forward Declarations

// <summary> Casacore Vector iterator </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
// <synopsis>
// This class creates a random access STL iterator for an Casacore Vector. All
// the STL functionality is present (or if something missing can be easily
// added). <br>
// The following comments hold:
// <ul>
// <li> A VectorSTLIterator can be created from a <src>Vector</src> (non-STL)
// It is the same as using <src>Vector.begin()</src>
// <li> No contiguous iterator is provided. Its construction is not necessary,
// since <src>Vector.data()</src> is a fully functional STL iterator already.
// <li> This iterator is non-intrusive. Since it needs state (the 'step') 
// nothing substantial is gained by having it included in the Vector class.
// The major gain would be to be able to use the standard nomenclature:
// <src>Vector::iterator()</src> rather than <src>VectorSTLiterator</src>
// <li> It would be advisable, and easy to implement, to add a template argument
// to the <src>Array</src> classes: <src><class T, Bool isCont=True></src>. The
// default is contiguous, since creation is contiguous. In that case correct
// iterators for e.g. contiguous arrays are supplied automatically.
// <li> needs probably some 'const' fine tuning; and the <src>-></src> operator
// </ul>
// </synopsis>

template <class T>
class VectorSTLIterator
: public std::iterator<std::random_access_iterator_tag, T> {
 public:
  typedef T                             value_type;
  typedef value_type*                   pointer;
  typedef const value_type*             const_pointer;
  typedef VectorSTLIterator<T>          iterator;
  typedef const VectorSTLIterator<T>    const_iterator;
  typedef value_type&                   reference;
  typedef const value_type&             const_reference;
  typedef std::size_t                   size_type;
  typedef ptrdiff_t                     difference_type;
  // Constructors. The iterator constructor from a <src>Vector</src> is
  // the same as if created from <src>Vector.begin()</src>. Copy
  // constructor and assignment can be the default ones.
  // <group>
  explicit VectorSTLIterator(const Vector<T> &c)
    : start_p(const_cast<T*>(c.data())), 
      step_p (std::max(ssize_t(1), c.steps()(0))),
      iter_p (const_cast<T*>(c.data()))
  {}
  VectorSTLIterator() : start_p(0), step_p(1), iter_p(0)
  {}
  VectorSTLIterator(const typename Array<T>::IteratorSTL &c)
    : start_p(c.pos()), 
      step_p (std::max(ssize_t(1), c.steps()(0))),
      iter_p (start_p)
  {}
  // Copy constructor.
  //# It is certainly needed for the Intel compiler.
  VectorSTLIterator(const VectorSTLIterator<T>& that)
    : std::iterator<std::random_access_iterator_tag, T>(that), 
      start_p(that.start_p), step_p(that.step_p),
      iter_p(that.iter_p)
  {}
  // Assignment.
  //# It is certainly needed for the Intel compiler.
  VectorSTLIterator<T>& operator=(const VectorSTLIterator<T>& that)
  {
    if (this != &that) {
      std::iterator<std::random_access_iterator_tag, T>::operator=(that);
      start_p = that.start_p;
      step_p  = that.step_p; 
      iter_p  = that.iter_p;
    }
    return *this;
  }
  // </group>
  // Destructor
  ~VectorSTLIterator() {;}
  // Access
  // <group>
  reference operator[](uInt i) { return *(start_p+i*step_p); };
  const_reference operator[](uInt i) const {
    return *(start_p+i*step_p); };
  reference operator*() { return *iter_p; };
  const_reference operator*() const { return *iter_p; };
  pointer pos() const {return iter_p; }
  // </group>
  // Manipulation
  // <group>
  const iterator &operator++() { iter_p+=step_p; return *this; };
  iterator operator++(int) {
    iterator t = *this; iter_p+=step_p; return t; }; 
  iterator &operator--() { iter_p-=step_p; return *this; };
  iterator operator--(int) {
    VectorSTLIterator<T> t = *this;iter_p-=step_p; return t; }; 
  iterator &operator+=(difference_type i) {
    iter_p+=i*step_p; return *this; };
  iterator &operator-=(difference_type i) {
    iter_p-=i*step_p; return *this; };
  iterator operator+(difference_type i) const {
    VectorSTLIterator<T> t = *this; return t+=i; };
  iterator operator-(difference_type i) const {
    VectorSTLIterator<T> t = *this; return t-=i; };
  // </group>
  // Size related
  // <group>
  difference_type operator-(const VectorSTLIterator<T> &x) const {
    return (iter_p-x.iter_p)/step_p; };
  // </group>
  // Comparisons
  // <group>
  Bool operator== (const iterator &other) const {
    return iter_p == other.iter_p; };
  Bool operator!= (const iterator other) const {
    return iter_p != other.iter_p; };
  Bool operator< (const iterator &other) const {
    return iter_p <  other.iter_p; };
  Bool operator== (const_pointer const pos) const {
    return iter_p == pos; };
  Bool operator!= (const_pointer const pos) const {
    return iter_p != pos; };
  Bool operator< (const_pointer const pos) const {
    return iter_p <  pos; };
  // </group>
 protected: 
  // Start (for random indexing)
  pointer const start_p;
  // Distance between elements
  difference_type step_p;
  // Current element pointer
  pointer iter_p;
};


} //# NAMESPACE CASACORE - END

#endif
