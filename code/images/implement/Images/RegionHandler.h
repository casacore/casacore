//# RegionHandler.h: Handle regions stored as table keywords
//# Copyright (C) 1999
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

#if !defined(AIPS_REGIONHANDLER_H)
#define AIPS_REGIONHANDLER_H


//# Includes
#include <aips/aips.h>

//# Forward Declarations
class Table;
class ImageRegion;
class LatticeBase;
class LCPagedMask;
class String;

// <summary>
// Handle regions stored as table keywords.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="tPagedImage2.cc" demos="">
// </reviewed>

// <prerequisite>
// <list>
//   <item> <linkto class=PagedImage>PagedImage</linkto>
//   <item> <linkto class=ImageRegion>ImageRegion</linkto>
// </list>
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
// <p>
// The class consists of static functions only.
// </synopsis> 

// <example>
// </example>

// <motivation>
// This class has 2 purposes:
// <ol>
// <li> This untemplated code can be factored out from the templated
// PagedImage class.
// <li> The functions can easily be used by other code.
// </ol>
// </motivation>

//# <todo asof="1999/02/16">
//# <li>
//# </todo>


class RegionHandler
{
public: 

  // Define the possible group types (regions or masks).
    enum GroupType {
        Regions,
	Masks,
	Any      
    };

    // Set the default mask to the mask with the given name.
    // It constructs a ImageRegion object for the new default mask.
    // If the table is writable, the setting is persistent by writing
    // the name as a keyword.
    // If the given maskName is the empty string, the default mask is unset.
    static void setDefaultMask (Table& table, const String& maskName);
    
    // Get the name of the default mask.
    // An empty string is returned if no default mask.
    static String getDefaultMask (const Table& table);
    
    // Define a region belonging to the table.
    // The group type determines if it stored as a region or mask.
    // If overwrite=False, an exception will be thrown if the region
    // already exists in the "regions" or "masks" keyword.
    // Otherwise the region will be removed first.
    // <br>A False status is returned if the table is not writable
    static Bool defineRegion (Table& table, const String& name,
			      const ImageRegion& region,
			      RegionHandler::GroupType,
			      Bool overwrite = False);
    
    // Does the table have a region with the given name?
    static Bool hasRegion (const Table& table,
			   const String& name,
			   RegionHandler::GroupType = RegionHandler::Any);

    // Get a region belonging to the table.
    // A zero pointer is returned if the region does not exist.
    // The caller has to delete the <src>ImageRegion</src> object created.
    // <br>No exception is thrown if the region does not exist.
    static ImageRegion* getRegion (const Table& table, const String& name,
				   RegionHandler::GroupType = Any,
				   Bool throwIfUnknown = True);
    
    // Remove a region belonging to the table.
    // <br>Optionally an exception is thrown if the region does not exist.
    // <br>A False status is returned if the table is not writable
    static Bool removeRegion (Table& table, const String& name,
			      RegionHandler::GroupType = Any,
			      Bool throwIfUnknown = True);

    // Find field number of the region group to which a region belongs
    // (i.e. the field number of the "regions" or "masks" field).
    // <0 is returned if the region does not exist.
    // <br>Optionally an exception is thrown if the region does not exist.
    static Int findRegionGroup (const Table& table, const String& regionName,
				RegionHandler::GroupType = Any,
				Bool throwIfUnknown = True);

    // Make a mask for a stored lattice (e.g. a PagedImage).
    // It creates it as a subtable of the lattice with the same tile shape.
    static LCPagedMask makeMask (const LatticeBase& lattice,
				 const String& name);
    
private:  
    // This class is not meant to be constructed.
    RegionHandler();
};



#endif

