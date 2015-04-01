//# PtrHolder.h: Hold and delete pointers not deleted by object destructors
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

#ifndef CASA_PTRHOLDER_H
#define CASA_PTRHOLDER_H

//# Includes
#include <casacore/casa/aips.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Hold and delete pointers not deleted by object destructors
// </summary>

// <use visibility=export>
// <reviewed reviewer="troberts" date="1995/07/29" tests="tPtrHolder">
// </reviewed>

// <prerequisite>
//   <li> module <linkto module=Exceptions>Exceptions</linkto>
// </prerequisite>

// <synopsis> 
// <src>PtrHolder</src>s hold allocated pointers which should be
// deleted when an exception is thrown. Exceptions only call destructors
// of objects. Thus, for example, storage allocated in a global function
// (outside of an object)is not deleted. A <src>PtrHolder</src> solves
// this problem: it merely holds the pointer and deletes it when it is
// destroyed itself, e.g. when an exception is thrown or when the
// function exits normally.
// </synopsis> 

// <example>
// <srcblock>
//    void func(Int *ptr); // some other function that takes a pointer
//    // ...
//    // True below means it's an array, False (the default) would mean
//    // a singleton object.
//    PtrHolder<Int> iholder(new Int[10000], True);
//    func(iholder);                           // converts automatically to ptr
//    (iholder.ptr() + 5) = 11;                // use pointer explicitly
//    some_function_that_throws_exception();   // pointer is deleted
// </srcblock>
// </example>

// <motivation>
// Avoid leaks when throwing/catching exceptions.
// </motivation>

// <todo asof="2000/04/11">
//   <li> Use the autoptr class from the Standard Library
// </todo>


template<class T> class PtrHolder
{
public:
    // The default constructor uses a null pointer.
    PtrHolder();

    // Construct a <src>PtrHolder</src> from a pointer which MUST have
    // been allocated from <src>new</src>, since the destructor will
    // call <src>delete</src> on it. If the pointer is to an array,
    // i.e. allocated with operator <src>new[]</src>, then
    // <src>isCarray</src> should be set to True. (This parameter is
    // required because C-arrays need to be deleted with
    // <src>delete[]</src>.)
    //
    // After the pointer is placed into the holder, the user should
    // not manually delete the pointer; the <src>PtrHolder</src>
    // object will do that, unless <src>set()</src> or
    // <src>clear()</src> is called with <src>deleteCurrentPtr</src>
    // set to False. The pointer must also only be put into
    // <em>one</em> holder to avoid double deletion.
    PtrHolder(T *pointer, Bool isCArray = False);

    ~PtrHolder();

    // Set the pointer to a new value. If <src>deleteCurrentPtr </src>is
    // True (the default), then delete the existing pointer first. If
    // <src>isCarray</src> is True, then the new pointer is assumed to
    // have been allocated with <src>new[]</src>.
    void set(T *pointer, Bool isCarray = False, Bool deleteCurrentPtr = True);

    // Set the current pointer to null; if <src>deletePtr</src> is True
    // (the default), then the current pointer is deleted first.
    void clear(Bool deleteCurrentPtr = True);

    // Release the pointer for use.
    // <group>
    T *ptr() { return ptr_p; }
    const T *ptr() const { return ptr_p; }
    // </group>

    // Attempt to automatically release a pointer when required. If the
    // compiler can't figure it out, you can use the <src>ptr()</src>
    // member function directly.
    operator T *() { return ptr_p; }
    operator T *() const { return ptr_p; }
    // </group>

    // Make it possible to use -> on the pointer object.
    T* operator->() const
      { return ptr_p; }

    // See if the pointer points to a C-array.
    Bool isCArray() const {return isCarray_p;}

private:
    //# Undefined and inaccessible
    PtrHolder(const PtrHolder<T> &other);
    PtrHolder<T> &operator=(const PtrHolder<T> &other);

    //# We'd also like the following to be undefined and inaccessible, 
    //# unfortunately CFront doesn't seem to let you do that.
    //# void *operator new(size_t s);

    //# Put functionality in one place
    void delete_pointer_if_necessary();

    T *ptr_p;
    //# If space were critical, we could make isCarray_p a char
    Bool isCarray_p;
};



// <summary>
// Hold and delete pointers not deleted by object destructors
// </summary>

// <use visibility=export>
// <reviewed reviewer="" date="" tests="tPtrHolder">
// </reviewed>

// <prerequisite>
//   <li> module <linkto module=Exceptions>Exceptions</linkto>
// </prerequisite>

// <synopsis> 
// <src>SPtrHolder</src>s hold allocated pointers to non-array objects
// which should be deleted when an exception is thrown.
// SPtrHolder is similar to PtrHolder, but easier to use and only valid
// for pointer to a single object, thus not to a C-array of objects.
// </synopsis> 

// <example>
// <srcblock>
//    void func(Table *ptr); // some other function that takes a pointer
//    // ...
//    // True below means it's an array, False (the default) would mean
//    // a singleton object.
//    SPtrHolder<Int> iholder(new Table(...));
//    func(iholder);                           // converts automatically to ptr
//    Table* tab = iholder.transfer();         // transfer ownership
// </srcblock>
// If an exception is thrown in function <src>func</src>, the Table will be
// deleted automatically. After the function call, the ownership is tranfered
// back to the 'user' 
// </example>

// <motivation>
// <src>std::auto_ptr</src> is harder to use and its future is unclear.
// <br>
// <src>PtrHolder</src> is not fully inlined and has C-array overhead.
// Furthermore the automatic conversion to a T* is dangerous, because the
// programmer may not be aware that the pointer is maybe taken over.
// </motivation>


template<class T> class SPtrHolder
{
public:
  // Construct an <src>SPtrHolder</src> from a pointer which MUST have
  // been allocated from <src>new</src>, since the destructor will
  // After the pointer is placed into the holder, the user should
  // not manually delete the pointer unless the transfer function is called.
  // The pointer must also only be put into
  // <em>one</em> holder to avoid double deletion.
  explicit SPtrHolder (T* ptr = 0)
    : itsPtr(ptr) {}

  ~SPtrHolder()
    { delete itsPtr; }

  // Reset the pointer.
  void reset (T* ptr)
    { if (ptr != itsPtr) { delete itsPtr; itsPtr = ptr; }}

  // Transfer ownership of the pointer.
  // I.e. return the pointer and set it to 0 in the object.
  T* transfer()
    { T* ptr = itsPtr; itsPtr = 0; return ptr; }

  // Release the pointer.
  void release()
    { itsPtr = 0; }

  // Make it possible to dereference the pointer object.
  // <group>
  T& operator*()
    { return *itsPtr; }
  const T& operator*() const
    { return *itsPtr; }
  // </group>

  // Make it possible to use -> on the pointer object.
  T* operator->() const
    { return itsPtr; }

  // Get the pointer for use.
  // <group>
  T* ptr()
    { return itsPtr; }
  const T* ptr() const
    { return itsPtr; }
  // </group>

private:
  // SPrtHolder cannot be copied.
  // <group>
  SPtrHolder(const SPtrHolder<T> &other);
  SPtrHolder<T> &operator=(const SPtrHolder<T> &other);
  // </group>

  //# The pointer itself.
  T* itsPtr;
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Utilities/PtrHolder.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
