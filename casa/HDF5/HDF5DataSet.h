//# HDF5DataSet.h: An class representing an HDF5 data set
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

#ifndef CASA_HDF5DATASET_H
#define CASA_HDF5DATASET_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/casa/HDF5/HDF5Object.h>
#include <casacore/casa/HDF5/HDF5HidMeta.h>
#include <casacore/casa/HDF5/HDF5DataType.h>
#include <casacore/casa/Arrays/Slicer.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/DataType.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

  //# Forward Declarations
  class ArrayBase;
  template<typename T> class Block;

  // <summary>
  // A class representing an HDF5 data set.
  // </summary>

  // <use visibility=export>

  // <reviewed reviewer="" date="" tests="tHDF5DataSet.cc">
  // </reviewed>

  // <prerequisite>
  //   <li> <a href="http://hdf.ncsa.uiuc.edu">HDF5 system</a>
  // </prerequisite>

  // <synopsis>
  // This class wraps the HDF5 functions to create and open a data set.
  // It is meant to be used in class HDF5Lattice, but can be used in itself
  // as well.
  // A dataset of any HDF5DataType (including compound types) can be created.
  // For a limited number of data types special constructors exist which
  // create the appropriate HDF5DataType themselves.
  // <br>
  // A data set can be created extendible by defining the appropriate
  // axis with length 0, whereafter the extend function can be used to
  // extend the data set.
  // The data can be stored in a tiled (chunked) way by specifying the tile
  // shape when creating it.
  // <br>
  // When opening an existing data set, it is checked if the given data type
  // matches the data set's data type. For a compound data type, it only
  // checks if its size matches; it does not check if the fields match.
  // <br>
  // It is possible to read or write a section of the data set by using an
  // appropriate Slicer object. Note that the Slicer object must be fully
  // filled; it does not infer missing info from the array shape.
  // <p>
  // Note that Casacore arrays are in Fortran order, while HDF5 uses C order.
  // Therefore array axes are reversed, thus axes in shapes, slicers, etc.
  // </synopsis> 

  // <motivation>
  // It was overkill to use the HDF5 C++ interface. Instead little wrappers
  // have been written. HDF5DataSet can be embedded in a shared pointer making
  // it possible to share an HDF5 data set amongst various HDF5Lattice objects
  // and close (i.e. destruct) the HDF5 data set object when needed.
  // </motivation>

  class HDF5DataSet : public HDF5Object
  {
  public: 
    // Create an HDF5 data set in the given hid (file or group).
    // It gets the given name, shape (also tile shape), and data type.
    // <group>
    HDF5DataSet (const HDF5Object&, const String&, const IPosition& shape,
		 const IPosition& tileShape, const bool*);
    HDF5DataSet (const HDF5Object&, const String&, const IPosition& shape,
		 const IPosition& tileShape, const unsigned char*);
    HDF5DataSet (const HDF5Object&, const String&, const IPosition& shape,
		 const IPosition& tileShape, const int16_t*);
    HDF5DataSet (const HDF5Object&, const String&, const IPosition& shape,
		 const IPosition& tileShape, const int32_t*);
    HDF5DataSet (const HDF5Object&, const String&, const IPosition& shape,
		 const IPosition& tileShape, const int64_t*);
    HDF5DataSet (const HDF5Object&, const String&, const IPosition& shape,
		 const IPosition& tileShape, const float*);
    HDF5DataSet (const HDF5Object&, const String&, const IPosition& shape,
		 const IPosition& tileShape, const double*);
    HDF5DataSet (const HDF5Object&, const String&, const IPosition& shape,
		 const IPosition& tileShape, const Complex*);
    HDF5DataSet (const HDF5Object&, const String&, const IPosition& shape,
		 const IPosition& tileShape, const DComplex*);
    HDF5DataSet (const HDF5Object&, const String&, const IPosition& shape,
		 const IPosition& tileShape, const HDF5DataType&);
    // </group>

    // Open an existing HDF5 data set in the given hid (file or group).
    // It checks if the internal type matches the given type.
    // <group>
    HDF5DataSet (const HDF5Object&, const String&, const bool*);
    HDF5DataSet (const HDF5Object&, const String&, const unsigned char*);
    HDF5DataSet (const HDF5Object&, const String&, const int16_t*);
    HDF5DataSet (const HDF5Object&, const String&, const int32_t*);
    HDF5DataSet (const HDF5Object&, const String&, const int64_t*);
    HDF5DataSet (const HDF5Object&, const String&, const float*);
    HDF5DataSet (const HDF5Object&, const String&, const double*);
    HDF5DataSet (const HDF5Object&, const String&, const Complex*);
    HDF5DataSet (const HDF5Object&, const String&, const DComplex*);
    HDF5DataSet (const HDF5Object&, const String&, const HDF5DataType&);
    // </group>

    // The destructor closes the HDF5 dataset object.
    virtual ~HDF5DataSet();

    // Close the hid if valid.
    virtual void close();

    // Set the cache size (in chunks) for the data set.
    // It needs to close and reopen the DataSet to take effect.
    void setCacheSize (uint32_t nchunks);

    // Get the data type for the data set with the given name.
    static DataType getDataType (hid_t, const String& name);

    // Get the shape.
    const IPosition& shape() const
      { return itsShape; }

    // Get the tile (chunk) shape.
    const IPosition& tileShape() const
      { return itsTileShape; }

    // Get a section of data into the array.
    // The array is resized if its shape does not match the slicer's shape.
    // This is only possible if the array is empty or if resize=true.
    // It is not checked if the data type of array and HDF5DataSet match.
    void get (const Slicer&, ArrayBase& buf, bool resize=false);
    
    // Get a section of data.
    // The buffer must be large enough to hold the section.
    void get (const Slicer&, void* buf);

    // Put a section of data.
    // The shape of the array and slicer must match.
    // It is not checked if the data type of array and HDF5DataSet match.
    void put (const Slicer&, const ArrayBase& buf);

    // Put a section of data.
    // The buffer must be large enough to hold the section.
    void put (const Slicer&, const void* buf);

    // Extend the dataset if an axis in the new shape is larger.
    void extend (const IPosition& shape);

  protected:
    // Create the data set.
    void create (const HDF5Object&, const String&,
		 const IPosition& shape, const IPosition& tileShape);

    // Open the data set and check if the external data type matches.
    void open (const HDF5Object&, const String&);

    // Close the dataset (but not other hids).
    void closeDataSet();

  private:
    // Copy constructor cannot be used.
    HDF5DataSet (const HDF5DataSet& that);
    // Assignment cannot be used.
    HDF5DataSet& operator= (const HDF5DataSet& that);

    HDF5HidDataSpace   itsDSid;        //# data space id
    HDF5HidProperty    itsPLid;        //# create property list id
    HDF5HidProperty    itsDaplid;      //# access property list id
    IPosition          itsShape;
    IPosition          itsTileShape;
    HDF5DataType       itsDataType;
    const HDF5Object*  itsParent;
  };

}

#endif
