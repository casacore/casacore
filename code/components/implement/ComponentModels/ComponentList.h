//# ComponentList: this defines ComponentList.h
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

#if !defined(AIPS_COMPONENTLIST_H)
#define AIPS_COMPONENTLIST_H

#if defined(_AIX)
#pragma implementation ("ComponentList.cc")
#endif

#include <aips/Utilities/String.h>
#include <aips/Containers/List.h>
#include <aips/Utilities/CountedPtr.h>
#include <trial/ComponentModels/SkyComponent.h>

class StokesVector;

// <summary> A class for manipulating groups of components </summary>

// <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class="SkyComponent"> SkyComponent <linkto>
// </prerequisite>
//
// <etymology>
//  Because the SkyComponents are actually stored in a linked list
//  ComponentList seemed like a good name for this class
// </etymology>
//
// <synopsis> 

// This class is a container that allows many SkyComponents (or objects
// derived from SkyComponent) to be grouped together and manipulated as one
// large compound component. The major operations of this class are:
// <ul>
// <li> Functions to add and delete components
// <li> Functions to traverse the list and extract individual components
// <li> Functions to sample the flux of the components in any direction and
//      grid the components onto an Image
// <li> Functions to save the list to a table  and read them back again
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
// manipulate them as a whole
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
  // Construct a componentList with no members in the list
  ComponentList();
  // Construct a ComponentList with one element 
  // <note role=warning> When using the third of these functions the
  // componentList will "take over" the pointer, and arrange for its
  // destruction once there are no more references to the
  // <src>SkyComponent</src> You should not manipulate the raw pointer once
  // the ComponentList has taken it over. If you need a pointer to the
  // SkyComponent then the <src>getComp()</src> member function can be used
  // to provide a <src>CountedPtr<SkyComponent></src> </note>
  // <group>
  ComponentList(SkyComponent & component);
  ComponentList(CountedPtr<SkyComponent> ptrComponent);
  ComponentList(SkyComponent * ptrComponent);
  // </group>

  // Read a componentList from an existing table
  ComponentList(const String & filename, Bool readOnly=False);

  ~ComponentList();
  
  // Sample all the members of the componentList at the specified direction
  StokesVector operator()(const MDirection & samplePos);

  // Sample all the members of the componentList at a number of Directions
  Vector<StokesVector> operator()(const Vector<MDirection>& samplePos);

  // Project all the members of the componentList onto the image
  void operator()(ImageInterface<Float>& plane);

  // Project all the members of the componentList onto the image and
  // convolve using the specified psf.
  void operator()(ImageInterface<Float>& plane,
 		  const ImageInterface<Float>& psf);

  // Insert a SkyComponent to a ComponentList before the current component. 
  // <note role=warning> When using the third of these functions the
  // componentList will "take over" the pointer, and arrange for its
  // destruction once there are no more references to the
  // <src>SkyComponent</src> You should not manipulate the raw pointer once
  // the ComponentList has taken it over. If you need a pointer to the
  // SkyComponent then the <src>getComp()</src> member function can be used
  // to provide a <src>CountedPtr<SkyComponent></src> </note>
  // <group>
  void insertComp(SkyComponent & component);
  void insertComp(CountedPtr<SkyComponent> & countedPtr);
  void insertComp(SkyComponent * ptrComponent);
  // </group>

  // Add a SkyComponent to the ComponentList after the current component.
  // <note role=warning> When using the third of these functions the
  // componentList will "take over" the pointer, and arrange for its
  // destruction once there are no more references to the
  // <src>SkyComponent</src> You should not manipulate the raw pointer once
  // the ComponentList has taken it over. If you need a pointer to the
  // SkyComponent then the <src>getComp()</src> member function can be used
  // to provide a <src>CountedPtr<SkyComponent></src> </note>
  // <group>
  void addComp(SkyComponent & component);
  void addComp(CountedPtr<SkyComponent> & countedPtr);
  void addComp(SkyComponent * ptrComponent);
  // </group>

  // Remove the current SkyComponent from the ComponentList
  void removeComp();

  // set the cursor to the beginning/end of the list or the specified element
  void gotoStart();
  void gotoEnd();
  void gotoPosition(uInt index);

  // increment/decrement the cursor to point to the next/prev element
  // Does nothing if the cursor is at the beginning/end
  void nextComp();
  void prevComp();

  // returns how many components in the list
  uInt nComponents() const;

  // Returns the position of the current component in the list. The first
  // component is at position 1.
  uInt curPosition() const;

  // returns a pointer to the current element in the list
  CountedPtr<SkyComponent> getComp();

  // make the ComponentList persistant by supplying a filename.
  void setListName(const String& filename);

private:
  String theFileName;
  ListIter<CountedPtr<SkyComponent> > theList;
};
#endif
