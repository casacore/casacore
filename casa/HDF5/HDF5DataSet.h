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
//#
//# $Id$

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
  // It is meant to be used in class HDF5Array, but can be used in itself
  // as well.
  // Only a limited number of data types are supported.
  // They are: boolean (stored as chars), 4-byte integer, and single and
  // double precision real and complex.
  // <br>
  // The data set has a fixed shape, thus cannot be extended nor resized.
  // It can be stored in a tiled (chunked) way by specifying the tile shape
  // when creating it.
  // <br>
  // It is possible to read or write a section of the data set by using an
  // appropriate Slicer object. Note that the Slicer object must be fully
  // filled; it does not infer missing info from the array shape.
  // <p>
  // Casacore arrays are in Fortran order, while HDF5 uses C order.
  // Therefore array axes are reversed, thus axes in shapes, slicers, etc.
  // </synopsis> 

  // <motivation>
  // It was overkill to use the HDF5 C++ interface. Instead little wrappers
  // have been written. HDF5DataSet can be embedded in a shared pointer making
  // it possible to share an HDF5 data set amongst various HDF5Array objects
  // and close (i.e. destruct) the HDF5 data set object when needed.
  // </motivation>

  class HDF5DataSet : public HDF5Object
  {
  public: 
    // Create an HDF5 data set in the given hid (file or group).
    // It gets the given name, shape (also tile shape), and data type.
    // <group>
    HDF5DataSet (const HDF5Object&, const String&, const IPosition& shape,
		 const IPosition& tileShape, const Bool*);
    HDF5DataSet (const HDF5Object&, const String&, const IPosition& shape,
		 const IPosition& tileShape, const uChar*);
    HDF5DataSet (const HDF5Object&, const String&, const IPosition& shape,
		 const IPosition& tileShape, const Int*);
    HDF5DataSet (const HDF5Object&, const String&, const IPosition& shape,
		 const IPosition& tileShape, const Int64*);
    HDF5DataSet (const HDF5Object&, const String&, const IPosition& shape,
		 const IPosition& tileShape, const Float*);
    HDF5DataSet (const HDF5Object&, const String&, const IPosition& shape,
		 const IPosition& tileShape, const Double*);
    HDF5DataSet (const HDF5Object&, const String&, const IPosition& shape,
		 const IPosition& tileShape, const Complex*);
    HDF5DataSet (const HDF5Object&, const String&, const IPosition& shape,
		 const IPosition& tileShape, const DComplex*);
    // </group>

    // Open an existing HDF5 data set in the given hid (file or group).
    // It checks if the internal type matches the given type.
    // <group>
    HDF5DataSet (const HDF5Object&, const String&, const Bool*);
    HDF5DataSet (const HDF5Object&, const String&, const uChar*);
    HDF5DataSet (const HDF5Object&, const String&, const Int*);
    HDF5DataSet (const HDF5Object&, const String&, const Int64*);
    HDF5DataSet (const HDF5Object&, const String&, const Float*);
    HDF5DataSet (const HDF5Object&, const String&, const Double*);
    HDF5DataSet (const HDF5Object&, const String&, const Complex*);
    HDF5DataSet (const HDF5Object&, const String&, const DComplex*);
    // </group>

    // The destructor closes the HDF5 dataset object.
    ~HDF5DataSet();

    // Close the hid if valid.
    virtual void close();

    // Set the cache size (in chunks) for the data set.
    // It needs to close and reopen the DataSet to take effect.
    void setCacheSize (uInt nchunks);

    // Get the data type for the data set with the given name.
    static DataType getDataType (hid_t, const String& name);

    // Get the shape.
    const IPosition& shape() const
      { return itsShape; }

    // Get the tile (chunk) shape.
    const IPosition& tileShape() const
      { return itsTileShape; }

    // Get a section of data.
    // The buffer must be large enough to hold the section.
    void get (const Slicer&, void* buf);

    // Put a section of data.
    void put (const Slicer&, const void* buf);

    // Extend the dataset if an axis in the new shape is larger.
    void extend (const IPosition& shape);

    // Helper functions to convert shapes.
    // It reverses the axes, because HDF5 uses C-order.
    // <group>
    static Block<hsize_t> fromShape (const IPosition& shape);
    static IPosition toShape (const Block<hsize_t>& b);
    // </group>

  private:
    // Copy constructor cannot be used.
    HDF5DataSet (const HDF5DataSet& that);
    // Assignment cannot be used.
    HDF5DataSet& operator= (const HDF5DataSet& that);

    // Create the data set.
    void create (const HDF5Object&, const String&,
		 const IPosition& shape, const IPosition& tileShape);

    // Open the data set and check if the external data type matches.
    void open (const HDF5Object&, const String&);

    // Close the dataset (but not other hids).
    void closeDataSet();

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
