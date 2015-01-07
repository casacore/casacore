//# HDF5Lattice.h: Templated paged array in an HDF5 file
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

#ifndef LATTICES_HDF5LATTICE_H
#define LATTICES_HDF5LATTICE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/lattices/Lattices/Lattice.h>
#include <casacore/lattices/Lattices/TiledShape.h>
#include <casacore/casa/HDF5/HDF5File.h>
#include <casacore/casa/HDF5/HDF5Group.h>
#include <casacore/casa/HDF5/HDF5DataSet.h>
#include <casacore/casa/BasicSL/String.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  // <summary>
  // A Lattice that is read from or written to an HDF5 dataset.
  // </summary>

  // <use visibility=export>

  // <reviewed reviewer="" date="" tests="tHDF5Lattice.cc">
  // </reviewed>

  // <prerequisite>
  //   <li> <linkto class="PagedArray">PagedArray</linkto>
  //   <li> <linkto class="TiledShape">TiledShape</linkto>
  //   <li> <linkto class="HDF5File">HDF5File</linkto>
  // </prerequisite>

  // <synopsis> 
  // Astronomical data arrays (like images) have to be persistent.
  // A Lattice is a templated abstract base class to hold any Casacore array.
  // The PagedArray class is a Lattice specialization which stores the data
  // in a Casacore table.
  // <br>
  // HDF5Lattice ia another Lattice specialization making it possible to store
  // an array as a dataset in a group in an HDF5 file.
  // <p>
  // When you construct an HDF5Lattice you do not read any data into
  // memory. Instead an HDF5 disk file is created, in a place you
  // specify, to hold the data. This means you need to have enough disk space
  // to hold the array. Constructing a new HDF5Lattice is equivalent to
  // creating a data set in an HDF5 file. 
  // <p>
  // To access the data in a HDF5Lattice you can (in order of preference):
  // <ol>
  // <li> Use a <linkto class=LatticeIterator>LatticeIterator</linkto>
  // <li> Use the getSlice and putSlice member functions
  // <li> Use the parenthesis operator or getAt and putAt functions
  // </ol>
  // Class PagedArray contains some more info and examples.
  // </synopsis> 

  // <example>
  // Create a HDF5Lattice of Floats of shape [1024,1024,4,256] in a file
  // called "myData_tmp.array" and initialize it to zero.
  // <srcblock>
  // const IPosition arrayShape(4,1024,1024,4,256);
  // const String filename("myData_tmp.array");
  // HDF5Lattice<Float> diskArray(arrayShape, filename);
  // cout << "Created a HDF5Lattice of shape " << diskArray.shape() 
  //   << " (" << diskArray.shape().product()/1024/1024*sizeof(Float) 
  //   << " MBytes)" << endl
  //   << "in the table called " << diskArray.tableName() << endl;
  // diskArray.set(0.0f);
  // // Using the set function is an efficient way to initialize the HDF5Lattice
  // // as it uses a LatticeIterator internally. Note that the set function is
  // // defined in the Lattice class that HDF5Lattice is derived from. 
  // </srcblock>
  // </example>

  // <motivation>
  // There was a need to be able to use HDF5 files to hold image data.
  // </motivation>

  // <templating arg=T>
  // <li> HDF5DataSet supports only a limited amount of types.
  // This restricts the template argument to
  // the types Bool, Int Float, Double, Complex, and DComplex.
  // </templating>

  template<typename T> class HDF5Lattice : public Lattice<T>
  {
    //# Make members of parent class known.
  public:
    using Lattice<T>::ndim;

  public: 
    // The default constructor creates an HDF5Lattice that is useless for just
    // about everything, except that it can be assigned to with the assignment
    // operator.
    HDF5Lattice();

    // Construct a new HDF5Lattice with the specified shape.
    // A new HDF5 file with the specified filename is constructed to hold
    // the array. The file will remain on disk after the HDF5Lattice goes
    // out of scope or is deleted.
    // Optionally the name of an HDF5 group can be given to create the array in.
    // The group is created if not existing yet.
    HDF5Lattice (const TiledShape& shape, const String& filename,
		 const String& arrayName = "array",
		 const String& groupName = String());

    // Construct a temporary HDF5Lattice with the specified shape.
    // A scratch file is created in the current working directory to hold
    // the array. This file will be deleted automatically when the HDF5Lattice
    // goes out of scope or is deleted.
    explicit HDF5Lattice (const TiledShape& shape);

    // Construct a new HDF5Lattice, with the specified shape, in the given
    // HDF5 file. The array gets the given name.
    // Optionally the name of an HDF5 group can be given to create the array in.
    // The group is created if not existing yet.
    HDF5Lattice (const TiledShape& shape, const CountedPtr<HDF5File>& file,
		 const String& arrayName, const String& groupName = String());

    // Reconstruct from a pre-existing HDF5Lattice in the HDF5 file and group
    // with the given names.
    explicit HDF5Lattice (const String& fileName,
			  const String& arrayName = "array",
			  const String& groupName = String());

    // Reconstruct from a pre-existing HDF5Lattice in the HDF5 file and group
    // with the given name.
    explicit HDF5Lattice (const CountedPtr<HDF5File>& file,
			  const String& arrayName,
			  const String& groupName = String());

    // The copy constructor which uses reference semantics. Copying by value
    // doesn't make sense, because it would require the creation of a
    // temporary (but possibly huge) file on disk.
    HDF5Lattice (const HDF5Lattice<T>& other);
  
    // The destructor flushes the HDF5Lattice's contents to disk. 
    ~HDF5Lattice();
  
    // The assignment operator with reference semantics. As with the copy
    // constructor assigning by value does not make sense.
    HDF5Lattice<T>& operator= (const HDF5Lattice<T>& other);
  
    // Make a copy of the object (reference semantics).
    virtual Lattice<T>* clone() const;

    // A HDF5Lattice is always persistent.
    virtual Bool isPersistent() const;

    // A HDF5Lattice is always paged to disk.
    virtual Bool isPaged() const;

    // Is the HDF5Lattice writable?
    virtual Bool isWritable() const;

    // Returns the shape of the HDF5Lattice.
    virtual IPosition shape() const;

    // Return the current HDF5 file name.
    // By default this includes the full path. 
    // The path preceeding the file name can be stripped off on request.
    virtual String name (Bool stripPath=False) const;

    // Return the current HDF5File object.
    const CountedPtr<HDF5File>& file() const
      { return itsFile; }

    // Return the current HDF5Group object.
    const CountedPtr<HDF5Group>& group() const
      { return itsGroup; }

    // Returns the name of this HDF5Lattice.
    const String& arrayName() const
      { return itsDataSet->getName(); }

    // Returns the current tile shape for this HDF5Lattice.
    IPosition tileShape() const;

    // Set the actual cache size for this Array to be big enough for the
    // indicated number of tiles. This cache is not shared with other
    // HDF5Lattices,
    // Tiles are cached using an LRU algorithm.
    virtual void setCacheSizeInTiles (uInt howManyTiles);

    // Set the cache size as to "fit" the indicated access pattern.
    virtual void setCacheSizeFromPath (const IPosition& sliceShape,
                                       const IPosition& windowStart,
                                       const IPosition& windowLength,
                                       const IPosition& axisPath);

    // Return the value of the single element located at the argument
    // IPosition.
    // Note that <src>Lattice::operator()</src> can also be used.
    virtual T getAt (const IPosition& where) const;
  
    // Put the value of a single element.
    virtual void putAt (const T& value, const IPosition& where);

    // A function which checks for internal consistency. Returns False if
    // something nasty has happened to the HDF5Lattice. In that case
    // it also throws an exception.
    virtual Bool ok() const;

    // This function is used by the LatticeIterator class to generate an
    // iterator of the correct type for a specified Lattice. Not recommended
    // for general use. 
    virtual LatticeIterInterface<T>* makeIter (const LatticeNavigator& navigator,
					       Bool useRef) const;

    // Do the actual getting of an array of values.
    virtual Bool doGetSlice (Array<T>& buffer, const Slicer& section);

    // Do the actual getting of an array of values.
    virtual void doPutSlice (const Array<T>& sourceBuffer,
			     const IPosition& where,
			     const IPosition& stride);

    // Returns the maximum recommended number of pixels for a cursor. This is
    // the number of pixels in a tile.
    virtual uInt advisedMaxPixels() const;

    // Get the best cursor shape.
    virtual IPosition doNiceCursorShape (uInt maxPixels) const;

    // Flush the data (but do not unlock).
    virtual void flush();

  private:
    // Make the Array in the HDF5 file and group.
    void makeArray (const TiledShape& shape, const String& arrayName,
		    const String& groupName);
    // Open the Array in the HDF5 file and group.
    void openArray (const String& arrayName, const String& groupName);
    // Check if the file is writable.
    void checkWritable() const;


    CountedPtr<HDF5File>    itsFile;
    CountedPtr<HDF5Group>   itsGroup;
    CountedPtr<HDF5DataSet> itsDataSet;
  };


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/lattices/Lattices/HDF5Lattice.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES

#endif
