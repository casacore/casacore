//# ComponentList: this defines ComponentList.h
//# Copyright (C) 1996,1997,1998
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

#if !defined(AIPS_COMPONENTLIST_H)
#define AIPS_COMPONENTLIST_H

#if defined(_AIX)
#pragma implementation ("ComponentList.cc")
#endif

#include <aips/aips.h>
#include <trial/ComponentModels/SkyComponent.h>
#include <aips/Containers/Block.h>
#include <aips/Tables/Table.h>

class String;
class MDirection;
class MVAngle;
template <class T> class Vector;
template <class T> class ImageInterface;

// <summary> A class for manipulating groups of components </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="SkyComponent">SkyComponent</linkto>
// </prerequisite>
//
// <etymology>
//  Because the SkyComponents were originally stored in a linked list
//  ComponentList seemed like a good name for this class. Now the list is gone
//  (in favour of a Block<SkyComponent>) but the name has stuck.
// </etymology>
//
// <synopsis> 

// This class is a container that allows many SkyComponents (or objects
// derived from SkyComponent) to be grouped together and manipulated as one
// large compound component. The major operations of this class are:
// <ul>
// <li> Functions to add and delete components
// <li> Functions to traverse the list and extract individual components
// <li> Functions to designate components as being selected or not
// <li> Functions to sample the flux of the components in any direction and
//      grid them onto an Image
// <li> Functions to save the components to a table and read them back 
//      again
// <\ul>


//#! What does the class do?  How?  For whom?  This should include code
//#! fragments as appropriate to support text.  Code fragments shall be
//#! delimited by <srcblock> </srcblock> tags.  The synopsis section will
//#! usually be dozens of lines long.
// </synopsis>
//
// <example>
// 
//#! One or two concise (~10-20 lines) examples, with a modest amount of
//#! text to support code fragments.  Use <srcblock> and </srcblock> to
//#! delimit example code.
// </example>
//
// <motivation>
// A way was needed to read/write groups of components to disk and
// manipulate them as a whole.
// </motivation>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//
// <todo asof="yyyy/mm/dd">
//   <li> add this feature
//   <li> fix this bug
//   <li> start discussion of this possible extension
// </todo>

class ComponentList {
public:
  // Sorting criteria for components
  enum SortCriteria {
    // Sort the components by ABS(I flux), largest first.
    FLUX = 0,
    // Sort the components by distance from the reference, closest first.
    POSITION,
    // Sort the components by fractional polarisation, biggest first.
    POLARISATION,
    // No sorting is necessary
    UNSORTED,
    // The number of criteria in this enumerator
    NUMBER_CRITERIA
  };
  // Construct a componentList with no members in the list
  ComponentList();

  // Read a componentList from an existing table. By default the Table is
  // opened read-write. It is recommended that you create a const ComponentList
  // if you open the Table readOnly.
  ComponentList(const String & fileName, const Bool readOnly=False);

  // The Copy constructor uses reference semantics
  ComponentList(const ComponentList & other);

  // The destructor saves the list to disk if it has a name (assigned using the
  // setName member function)
  ~ComponentList();
  
  // The assignment operator uses reference semantics
  ComponentList & operator=(const ComponentList & other);

  // Sample all the members of the componentList at the specified
  // direction. The returned Vector containes all the polarisarions. 
  Flux<Double> sample(const MDirection & sampleDir, const
		      MVAngle & pixelSize) const;

  // Project all the members of the componentList onto the image
  //  void project(ImageInterface<Float> & plane) const;

  // Add a SkyComponent to the end of the ComponentList. The list length is
  // increased by one when using this function. By default the newly added
  // component is not selected.
  void add(SkyComponent component);

  // Remove the specified SkyComponent from the ComponentList. After removing a
  // component all the components with an index greater than this one will be
  // reduced by one.
  void remove(const uInt & index);

  // returns how many components are in the list.
  uInt nelements() const;

  // deselect the specified component. Throws an exception (AipsError) if any
  // element in the index is out of range, ie. index >= nelements().
  void deselect(const Vector<Int> & index);

  // select the specified component. Throws an exception (AipsError) if any
  // element in the index is out of range, ie. index >= nelements().
  void select(const Vector<Int> & index);

  // Returns a Vector whose indices indicate which components are selected
  Vector<Int> selected() const;

  // returns a reference to the specified element in the list.
  // <group>
  const SkyComponent & component(const uInt & index) const;
  SkyComponent & component(const uInt & index);
  // </group>

  // Make the ComponentList persistant by supplying a filename. If the
  // ComponentList is already associated with a Table then the Table will be
  // renamed. Hence this function cannot be used with ComponentLists that are
  // constructed with readonly=True.
  void rename(const String & newName, 
	      const Table::TableOption option=Table::New);

  // Make a real copy of this componentList. As the copy constructor and the
  // assignment operator use reference semantics this is the only way to get a
  // distinct version of the componentList.
  ComponentList copy() const;

  // Sort the components in the list using the given criteria.
  void sort(ComponentList::SortCriteria criteria); 

  // Convert the SortCriteria enumerator to a string
  static String name(ComponentList::SortCriteria enumerator);

  // Convert a given String to a Type enumerator
  static ComponentList::SortCriteria type(const String & criteria);

  // Function which checks the internal data of this class for consistant
  // values. Returns True if everything is fine otherwise returns False.
  Bool ok() const;

private:
  void createTable(const String & fileName, const Table::TableOption option);
  void writeTable();
  void readTable(const String & fileName, const Bool readOnly);
  //  Int compareAbsI(const void * comp1Ptr, const void * comp2Ptr);
  Block<SkyComponent> itsList;
  uInt itsNelements;
  Table itsTable;
  Bool itsROFlag;
  Block<Bool> itsSelectedFlags;
  Block<uInt> itsOrder;
};
#endif
