//# RegionHandlerHDF5.h: Class for keeping regions in an HDF5 file
//# Copyright (C) 2008
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

#ifndef IMAGES_REGIONHANDLERHDF5_H
#define IMAGES_REGIONHANDLERHDF5_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/images/Regions/RegionHandler.h>
#include <casacore/casa/HDF5/HDF5File.h>
#include <casacore/tables/Tables/TableRecord.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN


// <summary>
// Class for keeping regions in an HDF5 file.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="tHDF5Image2.cc" demos="">
// </reviewed>

// <prerequisite>
//   <li> <linkto class=HDF5Image>HDF5Image</linkto>
//   <li> <linkto class=ImageRegion>ImageRegion</linkto>
// </prerequisite>

// <synopsis> 
// Persistent regions are stored as subrecords of the table keywords
// "regions" and "masks". The user can choose one of both keywords.
// Keyword "masks" is meant for true image masks, i.e. telling for
// each pixel if it is good or bad. Keyword "regions" is meant for
// true regions in an image.
// <p>
// This class handles defining, getting and removing such regions.
// It is used by class <linkto class=PagedImage</linkto>, but it can also
// be used by other code to handle regions in other tables.
// <p>
// Another function performed by this class for PagedImage is the
// definition of the default region to be used with an image.
// </synopsis> 

// <example>
// </example>

// <motivation>
// This class has 2 purposes:
// <ol>
// <li> This untemplated code can be factored out from the templated
//      Image classes.
// <li> The functions can easily be used by other code.
// </ol>
// </motivation>

//# <todo asof="1999/02/16">
//# <li>
//# </todo>


class RegionHandlerHDF5: public RegionHandler
{
public: 
  // The HDF5File object needed for the region operations.
  typedef const CountedPtr<HDF5File>& GetCallback (void* objectPtr);

  RegionHandlerHDF5 (GetCallback* callback, void* objectPtr);

  // Copy constructor (copy semantics).
  RegionHandlerHDF5 (const RegionHandlerHDF5&);

  virtual ~RegionHandlerHDF5();

  // Assignment (copy semantics).
  RegionHandlerHDF5& operator= (const RegionHandlerHDF5&);

  // Make a copy of the object.
  virtual RegionHandlerHDF5* clone() const;

  // Set the object pointer for callback function.
  virtual void setObjectPtr (void* objectPtr);

  // This class can define and handle regions.
  virtual Bool canDefineRegion() const;

  // Set the default mask to the mask with the given name.
  // It constructs a ImageRegion object for the new default mask.
  // If the table is writable, the setting is persistent by writing
  // the name as a keyword.
  // If the given maskName is the empty string, the default mask is unset.
  virtual void setDefaultMask (const String& maskName);

  // Get the name of the default mask.
  // An empty string is returned if no default mask.
  virtual String getDefaultMask() const;

  // Define a region belonging to the table.
  // The group type determines if it stored as a region or mask.
  // If overwrite=False, an exception will be thrown if the region
  // already exists in the "regions" or "masks" keyword.
  // Otherwise the region will be removed first.
  // <br>A False status is returned if the table is not writable
  virtual Bool defineRegion (const String& name,
			     const ImageRegion& region,
			     RegionHandler::GroupType,
			     Bool overwrite = False);

  // Does the table have a region with the given name?
  virtual Bool hasRegion (const String& name,
			  RegionHandler::GroupType = RegionHandler::Any) const;
  
  // Get a region belonging to the table.
  // A zero pointer is returned if the region does not exist.
  // The caller has to delete the <src>ImageRegion</src> object created.
  // <br>No exception is thrown if the region does not exist.
  virtual ImageRegion* getRegion (const String& name,
				  RegionHandler::GroupType = Any,
				  Bool throwIfUnknown = True) const;

  // Rename a region.
  // If a region with the new name already exists, it is deleted or
  // an exception is thrown (depending on <src>overwrite</src>).
  // The region name is looked up in the given group(s).
  // <br>An exception is thrown if the old region name does not exist.
  virtual Bool renameRegion (const String& newName,
			     const String& oldName,
			     RegionHandler::GroupType = Any,
			     Bool overwrite = False);

  // Remove a region belonging to the table.
  // <br>Optionally an exception is thrown if the region does not exist.
  // <br>A False status is returned if the table is not writable
  virtual Bool removeRegion (const String& name,
			     RegionHandler::GroupType = Any,
			     Bool throwIfUnknown = True);

  // Get the names of all regions/masks.
  virtual Vector<String> regionNames (RegionHandler::GroupType = Any) const;

  // Make a mask (an LCPagedMask) for a stored lattice (a PagedImage).
  // It creates it with the shape and tile shape of the lattice.
  virtual ImageRegion makeMask (const LatticeBase& lattice,
				const String& name);

  // Save the record containing the masking info in the HDF5 file.
  // It is only saved if changed or if always is true.
  void save (Bool always=False);

  // Restore the record containing the masking info from the HDF5 file.
  void restore();

private:
  // Get the file object.
  const CountedPtr<HDF5File>& file() const
    { return itsCallback (itsObjectPtr); }

  // Find field number of the region group to which a region belongs
  // (i.e. the field number of the "regions" or "masks" field).
  // <0 is returned if the region does not exist.
  // <br>Optionally an exception is thrown if the region does not exist.
  virtual Int findRegionGroup (const String& regionName,
			       RegionHandler::GroupType = Any,
			       Bool throwIfUnknown = True) const;


  TableRecord  itsRecord;
  Bool         itsChanged;        //# Has the record changed?
  GetCallback* itsCallback;
  void*        itsObjectPtr;
};


} //# NAMESPACE CASACORE - END

#endif
