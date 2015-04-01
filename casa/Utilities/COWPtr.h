//# COWPtr.h: this defines the Copy-On-Write-Pointer class.
//# Copyright (C) 1996,1997,1999
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

#ifndef CASA_COWPTR_H
#define CASA_COWPTR_H

#include <casacore/casa/aips.h>
#include <casacore/casa/Utilities/CountedPtr.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Copy-On-Write-Pointer class - allows control of copy based on constness.
// </summary>

// <use visibility=export>

// <reviewed reviewer="Ger van Diepen" date="1996/02/21" tests="tCOWPtr.cc" demos="">
// </reviewed>

// <prerequisite>
//   <li> none
// </prerequisite>
//
// <etymology>
// The COWPtr class name is a contraction of Copy-On-Write-Pointer 
// and is a reflection of its role as a carrier of objects which need to 
// minimize their copying and control their destruction.  Such objects only 
// need to copy if written to.
// </etymology>
//
// <synopsis>
// COWPtr can be used by other classes to implement copy-on-write
// semantics. Copy-on-write means that a copy of an object is not
// made until necessary. A well-known example is a String class
// with internally a pointer to a StringRep containing the true string.
// When a copy of a String is made, the StringRep is not copied yet.
// Only when the String gets changed and when more than one String
// points to the same StringRep, a copy of the StringRep is made.
// This technique can prevent a lot of copying when arguments are
// passed by value.
//<br>
// Implementing a String in this way is straightforward when
// String defines the pointer to its StringRep as <src>COWPtr<StringRep></src>
// and uses the appropriate functions (ref() and rwRef()) to execute
// const and non-const StringRep functions.
//<br>
// An example of this (straightforward) usage is class
//    <linkto class=RecordDesc>RecordDesc</linkto>.
//<p>
// COWPtr offers possibilities for more advanced usage:
// <ul>
// <li> Normally a copy (on write) is made when more than one String points to
//      the same StringRep. By constructing the COWPtr object with
//      readOnly=True, it is possible to already do that when only one
//      String points to a StringRep. This can be used when a function
//      returns an object referencing a constant object. For instance,
//      a function can return an Array object referencing another Array
//      which should not be altered.
//      By returning a <src>COWPtr<Array></src> with readOnly=True,
//      it is assured that a copy is made as soon as somebody wants
//      to change the returned Array object. No (expensive) copy is
//      made when only const access is being done.
// <li> Normally the COWPtr object takes over the pointer and deletes
//      the underlying object when it is not used anymore. With the
//      deleteIt flag it is possible to change this behavior.
//</ul>
//<p>
// Apart from the fact that COWPtr handles the copying, it has
// the big advantage that it forces that its access functions (ref and
// rwRef) are used in the correct way (ie. ref() for a const
// function and rwRef() for a non-const function). This ensures that
// copies are made when needed and not made when not needed.
//<p>
// Note that COWPtr uses the default constructor and the assignment
// operator to make a copy (thus not the copy constructor). The
// reason for this is that the copy constructor of some classes
// (e.g. Array) has reference semantics iso. copy semantics.
// </synopsis>
//
// <example>
// <h4>Example 1:</h4>
// <srcblock>
//    class String {
//    public:
//	// The constructor allocates a StringRep and hands to pointer
//	// to COWPtr.
//        String()
//        : itsRep (new StringRep;) {}
//        // This non-const function needs rwRef to make a copy when needed.
//        void set (const char* str) {itsRep.rwRef().set (str);}
//        // This const function can use ref (making a copy is not needed).
//        const char* get const {return itsRep.ref();}
//    private:
//        COWPtr<StringRep> itsRep;
//    };
//    class StringRep {
//    friend class String;
//    private:
//        void set (const char*);
//        const char* get() const;
//	char* itsData;
//    };
//</srcblock>
// <h4>Example 2:</h4>
// This function requires a const Array be passed out from the local scope.  
// The Array is created with non-const functions out of necessity (i.e. no
// const versions of the Array::getSlice() function exist.)  Preventing
// copies of the Array from being made forces us to use a COWPtr.  The COWPtr
// has arguments which allow us to declare the Array as const and not make
// any copies until a write operation is performed.
// <srcblock> 
// void myFunc(COWPtr<Array<Float> > &obj){
// // make a nonconst from some static const Array that exists "out there"
// Array<Float> &nonConstArray = (Array<Float> &)staticConstArray;
// // "fill" the COWPtr and bring back constness without copying. The first
// // "True" argument indicates the caller of this function may take
// // control of the dynamic pointer's destruction.  The second "True"
// // argument indicates the array is read only and should make a copy of 
// // itself if writing is needed.
// obj.set(new Array<Float>(nonConstArray.getSlice(...), True, True));
// }
// </srcblock>
// The caller of the function will get their piece of a const array without
// making a copy until the last possible moment (maybe never.)
// <srcblock>
// #include <casacore/casa/Utilities/COWPtr.h>
// main(){
//   // create a null filled COWPtr
//   COWPtr<Array<Float> > COW;
//   // fill it inside myfunc
//   myFunc(COW);
//   // use a single element - still no copies have been made!
//   Float someVal = COW->operator()(IPosition(2,3,3))
//   // write to the array - now we get a copy!
//   COW.rwRef().set(42.0f);
//   // etc...
// };
// </srcblock>
// </example>
//
// <motivation>
// Three words; efficiency, efficiency, efficiency.  Not everything may be
// passed as a reference.  With COWPtrs we may fake it.
// </motivation>
//
// <templating arg=T>
//    <li> default constructor
//    <li> assignment operator
// </templating>
//
// <thrown>
//    <li> AipsError
// </thrown>
//
// <todo asof="1996/01/16">
// <li> none
// </todo>

template <class T> class COWPtr
{
public:
  
  // The default constructor: used to create a null pointer which is 
  // delete-able by the destructor.  It is not "readOnly" so that it may be
  // changed by the COWPtr<T>::set() function.
  inline COWPtr(); 
  
  // The dynamic "pointer to object" constructor: default behavior is to 
  // delete the allocated memory when this instance's of COWPtr is destructed. 
  // Or the Boolean argument of "deleteIt = False" implies the pointer is 
  // being maintained by an object other than this instance of COWPtr and 
  // will not delete the allocated memory upon this instance's destruction. 
  // Control of copying is provided by the Boolean "readOnly" argument.  The
  // default value of "readOnly = False" forces a copy if the number of
  // references to the dynamic memory is greater than one.  Copying is always
  // done if the constructor is given an argument of "readOnly = True".  
  // <note> The only copying done (if ever) is upon a call to 
  // COWPtr<T>::rwRef().</note>
  explicit COWPtr(T *obj, Bool deleteIt = True, Bool readOnly = False);

  // copy ctor with reference semantics
  inline COWPtr(const COWPtr<T> &other);

  // assignment operator with reference semantics
  inline COWPtr &operator=(const COWPtr<T> &other);

  // return a pointer to a const object.  This prevents "write" operations.
  inline const T *operator->() const;  

  // return a reference to a const object.  This prevents "write" operations.
  inline const T &operator*() const;

  // Function used to change this instance of COWPtr. The pointer must be 
  // dynamically allocated.  Default behavior is to 
  // delete the allocated memory when this instance's of COWPtr is destructed. 
  // Or the Boolean argument of "deleteIt = False" implies the pointer is 
  // being maintained by an object other than this instance of COWPtr and 
  // will not delete the allocated memory upon this instance's destruction. 
  // Control of copying is provided by the Boolean "readOnly" argument.  The
  // default value of "readOnly = False" forces a copy if the number of
  // references to the dynamic memory is greater than one.  Copying is always
  // done if the constructor is given an argument of "readOnly = True".  
  // <note> The only copying done (if ever) is upon a call to 
  // COWPtr<T>::rwRef().
  // </note>
  // The <src>setReadOnly</src> function is the same as <src>set</src>, but
  // forces <src>deleteIt=False</src> and <src>ReadOnly=True</src>. In
  // that way a const object can also be safely referenced by COWPtr.
  // <group>
  void set(T *obj, Bool deleteIt = True, Bool readOnly = False);
  void setReadOnly (const T *obj);
  void setReadOnly ();
  // </group>

  // return a const reference to the object.
  inline const T &ref() const;

  // return a readable and writable reference to this instance.  Instances of
  // COWPtr constructed with argument "readOnly = True" will be made a copy.  
  // Additionally, all instances of COWPtr with more than one reference to 
  // the allocated memory stored within will be copied.
  inline T &rwRef();

  // returns False if this contains a non-null ptr, otherwise, return True.
  inline Bool isNull() const;

  // returns True if the object is const, otherwise, return False.
  inline Bool isReadOnly() const; 

  // returns True if the object is the only instance, otherwise, return False.
  inline Bool isUnique() const; 

  // Return True if copied, otherwise, False.  This function will make this 
  // instance's object a copy if it is constructed with 
  // "readOnly = True."  Additionally, all instances of COWPtr with more 
  // than one reference to the allocated memory stored within will be 
  // copied. 
  Bool makeUnique(); 

protected:
  CountedPtr<T> obj_p;
  Bool const_p;
};



// Make our own default pointer - deleteIt==True by default, const_p==False
template <class T> inline COWPtr<T>::COWPtr()
:obj_p(static_cast<T *>(0), True), const_p(False)
{
  // does nothing
} 

// copy ctor with reference semantics
template <class T> inline COWPtr<T>::COWPtr(const COWPtr<T> &other)
: obj_p(other.obj_p), const_p(other.const_p)
{
  // does nothing
}

//assignment operator with reference semantics
template <class T> 
inline COWPtr<T> &COWPtr<T>::operator=(const COWPtr<T> &other)
{
  if (this != &other){
    obj_p = other.obj_p;
    const_p = other.const_p;
  }
  return *this;
}

template <class T> inline void COWPtr<T>::setReadOnly (const T *obj)
{
  set ((T*)obj, False, True);
}

template <class T> inline void COWPtr<T>::setReadOnly ()
{
  const_p = True;
}

template <class T> inline const T *COWPtr<T>::operator->() const
{
  return obj_p.operator->();
}

template <class T> inline const T &COWPtr<T>::operator*() const
{
  return obj_p.operator*();
}

template <class T> inline const T &COWPtr<T>::ref() const
{
  return *obj_p;
}

template <class T> inline T &COWPtr<T>::rwRef()
{
  makeUnique();
  return *obj_p;
}

template <class T> inline Bool COWPtr<T>::isNull() const
{
  return obj_p.null();
}

template <class T> inline Bool COWPtr<T>::isReadOnly() const
{
  return const_p; 
}

template <class T> inline Bool COWPtr<T>::isUnique() const
{
  return (const_p || obj_p.nrefs()>1) ? False : True;
}



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/casa/Utilities/COWPtr.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
