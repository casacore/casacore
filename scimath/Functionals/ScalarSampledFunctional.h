//# ScalarSampledFunctional.h:
//# Copyright (C) 1996
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
//#
//# $Id$

#ifndef SCIMATH_SCALARSAMPLEDFUNCTIONAL_H
#define SCIMATH_SCALARSAMPLEDFUNCTIONAL_H

#include <casacore/casa/aips.h>
#include <casacore/scimath/Functionals/SampledFunctional.h>
#include <casacore/casa/Arrays/Vector.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

template<class T> class Block;

// <summary> A unified interface for indexing into Vectors or Blocks </summary>

// <use visibility=export>

// <reviewed reviewer="wyoung" date="1996/10/18" tests="tSampledFunctional.cc">

// <prerequisite>
//   <li> <linkto class="SampledFunctional">SampledFunctional</linkto>
//   <li> <linkto class="Vector">Vector</linkto>
//   <li> <linkto class="Block">Block</linkto>
// </prerequisite>

// <etymology>
// A SampledFunctional is an interface that allows random access to a fixed
// size data set. I originally conceived this class as being used to access
// scalar values (Int's Float's etc.) stored in Vectors, using the
// SampledFunctional interface. It became generalised to incorporate Blocks
// and I now realise that a better name might be MemorySampledFunctional, to
// highlight that the data is stored in memory (and not on disk).
// </etymology>

// <synopsis>
// This derived class allows allows a Block<T> or Vector<T> object to be
// accessed through the SampledFunctional<T> interface. The principle
// advantage of this is that it unifies the different indexing operators
// (ie. [] for Blocks and () for Vectors). The disadvantage is that it hides
// just about all the other functionality of Vectors and Blocks. If all you
// are interested in is random access to various elements of these objects
// then this class is a suitable abstraction. 

// Reference semantics are used (ie. the class does not make a copy of the
// data but refers to the original data) whenever possible. It is not
// possible to use reference semantics (so a physical copy of the data is
// made), in the following cases:
// <ul>
// <li> When constructing the class from a Block<T>
// <li> When constructing the class from a const Vector<T>
// </ul>
// Reference semantics are always used for the copy constructor and
// assignment operators when the ScalarSampledFunctional is
// non-const. Otherwise copy semantics are used.

// When reference semantics are used you need to be aware that modifying the
// contents of the original Vector will modify the data used by this class.

// This class is always more efficient if reference semantics are used, so
// avoid using const arguments unless you really need to.

// </synopsis>

// <example>
// Constructing and using ScalarSampledFunctional's
// <srcblock> 
// Block<Float> b(10); // Create a block of ten elements
// // ... Fill the block any way you like ... 
// ScalarSampledFunctional<Float> fb(b); 
// for(uInt i = 0; i < 10; i++)
//  cout << "f(" << i << ") = " << fb(i) << endl;
// </srcblock>
// </example>

// <motivation>
// The SampledFunctional is a useful interface. But it needs some concrete
// classes to back it up. This is the first one that was written.
// </motivation>

// <templating arg=Range>
// <li> Very few assumptions are made on the templating type. So this class
// should work for a wide variety of templates types. 
// </templating>

// <thrown>
// <li> Exceptions are not thrown directly by this class.
// </thrown>

// <todo asof="1996/10/28">
//   <li> Nothing I can think of
// </todo>

template<class T> class ScalarSampledFunctional
  :public SampledFunctional<T>
{
public:
  // See the description above to determine whether a copy or a reference is
  // made to the original data. 
  // <group>
  ScalarSampledFunctional();
  ScalarSampledFunctional(Vector<T> & data);
  ScalarSampledFunctional(const Vector<T> & data);
  ScalarSampledFunctional(const Block<T> & data);
  // </group>

  // The standard copy constructor and assignment operator. These functions
  // use reference semantics when the ScalarSampledFunctional is
  // non-const, and copy semantics otherwise.
  // <group>
  ScalarSampledFunctional(ScalarSampledFunctional<T> & other);
  ScalarSampledFunctional(const ScalarSampledFunctional<T> & other);
  ScalarSampledFunctional<T> & operator=(ScalarSampledFunctional<T> &other);
  ScalarSampledFunctional<T> & operator=(const ScalarSampledFunctional<T> &other);
  // </group>

  // Define the functions for the SampledFunctional interface
  // <group> 
  virtual T operator()(const uInt &index) const;
  virtual uInt nelements() const;
  virtual ~ScalarSampledFunctional();
  // </group>

private:
  Vector<T> refData;
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/scimath/Functionals/ScalarSampledFunctional.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif


