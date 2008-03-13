//# HDF5DataSet.cc: An class representing an HDF5 data type
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

#ifdef HAVE_HDF5

#include <casa/HDF5/HDF5DataSet.h>
#include <casa/HDF5/HDF5Error.h>
#include <casa/Containers/Block.h>
#include <casa/Containers/BlockIO.h>
#include <casa/Utilities/Assert.h>

namespace casa { //# NAMESPACE CASA - BEGIN

  HDF5DataSet::HDF5DataSet (const HDF5Object& parentHid, const String& name,
			    const IPosition& shape, const IPosition& tileShape,
			    const Bool* type)
    : itsDataType (type)
  {
    create (parentHid, name, shape, tileShape);
  }

  HDF5DataSet::HDF5DataSet (const HDF5Object& parentHid, const String& name,
			    const IPosition& shape, const IPosition& tileShape,
			    const Int* type)
    : itsDataType (type)
  {
    create (parentHid, name, shape, tileShape);
  }

  HDF5DataSet::HDF5DataSet (const HDF5Object& parentHid, const String& name,
			    const IPosition& shape, const IPosition& tileShape,
			    const Float* type)
    : itsDataType (type)
  {
    create (parentHid, name, shape, tileShape);
  }

  HDF5DataSet::HDF5DataSet (const HDF5Object& parentHid, const String& name,
			    const IPosition& shape, const IPosition& tileShape,
			    const Double* type)
    : itsDataType (type)
  {
    create (parentHid, name, shape, tileShape);
  }
 
  HDF5DataSet::HDF5DataSet (const HDF5Object& parentHid, const String& name,
			    const IPosition& shape, const IPosition& tileShape,
			    const Complex* type)
    : itsDataType (type)
  {
    create (parentHid, name, shape, tileShape);
  }

  HDF5DataSet::HDF5DataSet (const HDF5Object& parentHid, const String& name,
			    const IPosition& shape, const IPosition& tileShape,
			    const DComplex* type)
    : itsDataType (type)
  {
    create (parentHid, name, shape, tileShape);
  }

  HDF5DataSet::HDF5DataSet (const HDF5Object& parentHid, const String& name,
			    const Bool* type)
    : itsDataType (type)
  {
    open (parentHid, name);
  }

  HDF5DataSet::HDF5DataSet (const HDF5Object& parentHid, const String& name,
			    const Int* type)
    : itsDataType (type)
  {
    open (parentHid, name);
  }

  HDF5DataSet::HDF5DataSet (const HDF5Object& parentHid, const String& name,
			    const Float* type)
    : itsDataType (type)
  {
    open (parentHid, name);
  }

  HDF5DataSet::HDF5DataSet (const HDF5Object& parentHid, const String& name,
			    const Double* type)
    : itsDataType (type)
  {
    open (parentHid, name);
  }

  HDF5DataSet::HDF5DataSet (const HDF5Object& parentHid, const String& name,
			    const Complex* type)
    : itsDataType (type)
  {
    open (parentHid, name);
  }

  HDF5DataSet::HDF5DataSet (const HDF5Object& parentHid, const String& name,
			    const DComplex* type)
    : itsDataType (type)
  {
    open (parentHid, name);
  }

  HDF5DataSet::~HDF5DataSet()
  {
    close();
  }

  void HDF5DataSet::create (hid_t parentHid, const String& name,
			    const IPosition& shape, const IPosition& tileShape)
  {
    setName (name);
    // Get the array shape and tile shape. Adjust as needed.
    AlwaysAssert (shape.nelements() >= tileShape.nelements(), AipsError);
    itsShape     = shape;
    itsTileShape = IPosition(shape.nelements(), 1);
    // Trailing elements already have value 1; set the first elements.
    for (uInt i=0; i<tileShape.nelements(); ++i) {
      itsTileShape[i] = std::min (tileShape[i], shape[i]);
    }
    // Create the data space for the array.
    int rank = itsShape.nelements();
    Block<hsize_t> ls = fromShape (itsShape);
    itsDSid = H5Screate_simple(rank, ls.storage(), NULL);
    AlwaysAssert (itsDSid.getHid() >= 0, AipsError);
    // Create the properties to hold the tile shape.
    itsPLid = H5Pcreate (H5P_DATASET_CREATE);
    AlwaysAssert (itsPLid.getHid() >= 0, AipsError);
    Block<hsize_t> cs = fromShape (itsTileShape);
    H5Pset_chunk(itsPLid, rank, cs.storage());
    // Create the data set.
    setHid (H5Dcreate(parentHid, name.chars(), itsDataType.getHidFile(),
		      itsDSid, NULL, itsPLid, NULL));
    if (! isValid()) {
      throw HDF5Error("Data set array " + name + " could not be created");
    }
  }

  void HDF5DataSet::open (hid_t parentHid, const String& name)
  {
    setName (name);
    // Open the dataset.
    setHid (H5Dopen(parentHid, name.chars(), NULL));
    if (! isValid()) {
      throw HDF5Error("Data set array " + name + " does not exist");
    }
    // Get the (external) data type and check if it matches.
    HDF5HidDataType dsType (H5Dget_type(getHid()));
    if (H5Tget_class(dsType) != H5Tget_class(itsDataType.getHidFile())) {
      throw HDF5Error("Data set array " + name + " has an incorrect data type");
    }
    // Get the data space (for the shape).
    itsDSid = H5Dget_space(getHid());
    if (itsDSid.getHid() < 0) {
      throw HDF5Error("Data set array " + name + " does not have data space");
    }
    // Get rank and shape.
    int rank = H5Sget_simple_extent_ndims(itsDSid);
    Block<hsize_t> shp(rank);
    if (rank > 0) {
      rank = H5Sget_simple_extent_dims(itsDSid, shp.storage(), NULL);
    }
    if (rank < 0) {
      throw HDF5Error("Data set array " + name + " does not have dims");
    }
    itsShape = toShape(shp);
    // Get the property list (for the tile shape).
    itsPLid = H5Dget_create_plist(getHid());
    if (itsPLid.getHid() < 0) {
      throw HDF5Error("Data set array " + name + " does not have properties");
    }
    // Get tile shape and check if its rank matches.
    if (H5Pget_chunk(itsPLid, rank, shp.storage()) != rank) {
      throw HDF5Error("Data set array " + name + " tile shape error");
    }
    itsTileShape = toShape(shp);
  }

  void HDF5DataSet::close()
  {
    if (isValid()) {
      H5Dclose(getHid());
      clearHid();
    }
    itsDSid = -1;
    itsPLid = -1;
  }

  DataType HDF5DataSet::getDataType (hid_t parentHid, const String& name)
  {
    hid_t id = H5Dopen(parentHid, name.chars(), NULL);
    if (id < 0) {
      throw HDF5Error("Data set array " + name + " does not exist");
    }
    DataType dtype = TpOther;
    // Get the (external) data type and check if it matches.
    HDF5HidDataType dtid (H5Dget_type(id));
    H5Dclose(id);
    return HDF5DataType::getDataType (dtid);
  }

  void HDF5DataSet::get (const Slicer& section, void* buf)
  {
    // Define the data set selection.
    Block<hsize_t> offset = fromShape(section.start());
    Block<hsize_t> count  = fromShape(section.length());
    Block<hsize_t> stride = fromShape(section.stride());
    if (H5Sselect_hyperslab (itsDSid, H5S_SELECT_SET, offset.storage(),
			     stride.storage(), count.storage(), NULL) < 0) {
      throw HDF5Error("invalid data set array selection");
    }
    // Define a data space for the memory buffer.
    HDF5HidDataSpace memspace (H5Screate_simple (count.size(),
						 count.storage(), NULL));
    // Define memory selection.
    offset = 0;
    if (H5Sselect_hyperslab (memspace, H5S_SELECT_SET, offset.storage(),
			     NULL, count.storage(), NULL) < 0) {
      throw HDF5Error("setting slab of memory buffer");
    }
    // Read the data.
    if (H5Dread (getHid(), itsDataType.getHidMem(), memspace, itsDSid,
		 H5P_DEFAULT, buf) < 0) {
      throw HDF5Error("reading slab from data set array");
    }
  }

  void HDF5DataSet::put (const Slicer& section, const void* buf)
  {
    // Define the data set selection.
    Block<hsize_t> offset = fromShape(section.start());
    Block<hsize_t> count  = fromShape(section.length());
    Block<hsize_t> stride = fromShape(section.stride());
    if (H5Sselect_hyperslab (itsDSid, H5S_SELECT_SET, offset.storage(),
			     stride.storage(), count.storage(), NULL) < 0) {
      throw HDF5Error("invalid data set array selection");
    }
    // Define a data space for the memory buffer.
    HDF5HidDataSpace memspace (H5Screate_simple (count.size(),
						 count.storage(), NULL));
    // Define memory selection.
    offset = 0;
    if (H5Sselect_hyperslab (memspace, H5S_SELECT_SET, offset.storage(),
			     NULL, count.storage(), NULL) < 0) {
      throw HDF5Error("setting slab of memory buffer");
    }
    // Read the data.
    if (H5Dwrite (getHid(), itsDataType.getHidMem(), memspace, itsDSid,
		  H5P_DEFAULT, buf) < 0) {
      throw HDF5Error("writing slab into data set array");
    }
  }

  Block<hsize_t> HDF5DataSet::fromShape (const IPosition& shape)
  {
    // Reverse the axes (Fortran to C).
    Block<hsize_t> b(shape.size());
    std::reverse_copy (shape.begin(), shape.end(), b.begin());
    return b;
  }
  IPosition HDF5DataSet::toShape (const Block<hsize_t>& b)
  {
    // Reverse the axes (C to Fortran).
    IPosition shape(b.size());
    std::reverse_copy (b.begin(), b.end(), shape.begin());
    return shape;
  }

}

#endif
