//# TempImage.h: Temporary astronomical images
//# Copyright (C) 1998,1999
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

#if !defined(AIPS_TEMPIMAGE_H)
#define AIPS_TEMPIMAGE_H


//# Includes
#include <trial/Images/ImageInterface.h>
#include <trial/Lattices/TiledShape.h>
#include <trial/Lattices/TempLattice.h>
#include <aips/Quanta/Unit.h>
#include <aips/Tables/TableRecord.h>

// <summary>
// Temporary astronomical images.
// </summary>

// <use visibility=export>

// <reviewed reviewer="" date="" tests="tTempImage.cc" demos="">
// </reviewed>

// <prerequisite>
// <list>
//   <item> <linkto class=CoordinateSystem>CoordinateSystem</linkto>
//   <item> <linkto class=ImageInterface>ImageInterface</linkto>
//   <item> <linkto class=TempLattice>TempLattice</linkto>
// </list>
// </prerequisite>

// <etymology>
// The TempImage name comes from its role as the Image class for temporary
// storage.
// </etymology>

// <synopsis> 
// The class <src>TempImage</src> is useful for storing temporary images
// for which it is not known whether they can be held in memory.
// It uses class <linkto class=TempLattice>TempLattice</linkto> to
// hold the image in memory when it is small enough. Otherwise it is
// held in a temporary file. Similarly to <src>TempLattice</src>
// one can give the maximum memory to use to control when the image
// can be held in memory.
// <br>
// The other Image information like coordinates, units, and miscinfo
// is held in member variables and disappears when the TempImage object
// is destructed.
// <p>
// It is possibly to temporarily close a TempImage, which only takes effect
// when it is created as a PagedArray. In this way it is possible to reduce
// the number of open files in case a lot of TempImage objects are used.
// A temporarily closed TempImage will be reopened automatically when needed.
// It can also be reopened explicitly.
// </synopsis> 

// <example>
// <srcblock>
// </srcblock>
// </example>

// <motivation>
// The size of astronomical data can be very large.  The ability to fit an 
// entire image into random access memory cannot be guaranteed.  Paging from 
// disk pieces of the image appeared to be the way to deal with this problem.
// </motivation>

//# <todo asof="1998/10/27">
//#  <li>
//# </todo>


template<class T> class TempImage: public ImageInterface<T>
{
public: 
    // The default constructor creates an empty image.
    TempImage();

    // Construct a temporary Image from shape and coordinate information.
    // If the image is sufficiently small, it is kept in memory.
    // Otherwise it is kept in a temporary table.
    // The algorithm is the same as in class
    // <linkto class=TempLattice>TempLattice</linkto>.
    TempImage (const TiledShape& mapShape,
	       const CoordinateSystem& coordinateInfo,
	       Int maxMemoryInMB=-1);
  
    TempImage (const TiledShape& mapShape,
	       const CoordinateSystem& coordinateInfo,
	       Double maxMemoryInMB);
  
    // Copy constructor (reference semantics).
    TempImage (const TempImage<T>& other);

    // Destructor
    ~TempImage();
  
    // Assignment operator (reference semantics).
    TempImage<T>& operator= (const TempImage<T>& other);
  
    // Make a copy of the object (reference semantics).
    // <group>
    virtual Lattice<T>* clone() const;
    virtual ImageInterface<T>* cloneII() const;
    // </group>

    // Is the TempImage paged to disk?
    virtual Bool isPaged() const;

    // Is the TempImage writable?
    virtual Bool isWritable() const;

    // Close the TempImage temporarily (if it is paged to disk).
    // It'll be reopened automatically when needed or when
    // <src>reopen</src> is called explicitly.
    void tempClose()
        { mapPtr_p->tempClose(); }

    // If needed, reopen a temporarily closed TempLattice.
    void reopen()
        { mapPtr_p->reopen(); }

    // Function which changes the shape of the image (N.B. the data is thrown 
    // away - the Image will be filled with nonsense afterwards)
    virtual void resize (const TiledShape& newShape);
  
    // Return the name of the current TempImage object.
    // It is always a null string.
    virtual String name (const Bool stripPath=False) const;

    // Return the shape of the image
    virtual IPosition shape() const;

    // Function which get and set the units associated with the image
    // pixels (i.e. the "brightness" unit).
    // <src>setUnits()</src> always returns True.
    // <group>
    virtual Bool setUnits (const Unit& newUnits);
    virtual Unit units() const;
    // </group>

    // Often we have miscellaneous information we want to attach to an image.
    // This is how it is done. Eventually we will want to register that some
    // of the information is to be destroyed if the image changes so that, e.g.
    // data max/min values can be removed if the image changes.
    //
    // Note that setMiscInfo REPLACES the information with the new information.
    // If can fail if, e.g., the underlying table is not writable.
    // <group>
    virtual const RecordInterface& miscInfo() const;
    virtual Bool setMiscInfo (const RecordInterface& newInfo);
    // </group>

    // Function which sets all of the elements in the Lattice to a value.
    virtual void set (const T& value);

    // Replace every element, x, of the lattice with the result of f(x).
    // You must pass in the address of the function -- so the function
    // must be declared and defined in the scope of your program.  
    // Both versions of apply require a function that accepts a single 
    // argument of type T (the Lattice template actual type) and returns
    // a result of the same type.  The first apply expects a function with
    // an argument passed by value; the second expects the argument to
    // be passed by const reference.  The first form ought to run faster
    // for the built-in types, which may be an issue for large images
    // stored in memory, where disk access is not an issue.
    // <group>
    virtual void apply (T (*function)(T));
    virtual void apply (T (*function)(const T&));
    virtual void apply (const Functional<T,T>& function);
    // </group>
    
    // Get or put a single pixel.
    // Note that the function operator () can also be used to get a pixel.
    // <group>
    virtual T getAt (const IPosition& where) const;
    virtual void putAt (const T& value, const IPosition& where);
    // </group>

    // This is the implementations of the letters for the envelope Iterator
    // class <note> Not for public use </note>
    virtual LatticeIterInterface<T>* makeIter
                                 (const LatticeNavigator& navigator) const;

    // Returns the maximum recommended number of pixels for a cursor.
    // This is the number of pixels in a tile. 
    virtual uInt maxPixels() const;

    // Check for symmetry in data members.
    virtual Bool ok() const;

protected:
    // Function which extracts an array from the map.
    virtual Bool doGetSlice (Array<T>& buffer, const Slicer& theSlice);
  
    // Function to replace the values in the map with soureBuffer.
    virtual void doPutSlice (const Array<T>& sourceBuffer,
			     const IPosition& where,
			     const IPosition& stride);

    // Help the user pick a cursor for most efficient access.
    virtual IPosition doNiceCursorShape (uInt maxPixels) const;


private:  
    TempLattice<T>* mapPtr_p;
    Unit            unit_p;
    TableRecord     misc_p;
};


#endif
