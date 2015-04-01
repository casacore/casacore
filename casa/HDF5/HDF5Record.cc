//# HDF5Record.cc: A class to write/read a record into HDF5
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

#include <casacore/casa/HDF5/HDF5Record.h>
#include <casacore/casa/HDF5/HDF5DataSet.h>
#include <casacore/casa/HDF5/HDF5Group.h>
#include <casacore/casa/HDF5/HDF5HidMeta.h>
#include <casacore/casa/HDF5/HDF5Error.h>
#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Utilities/Assert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

#ifdef HAVE_HDF5

  herr_t readSubRecord (hid_t groupHid, const char* name,
                        const H5L_info_t*, void* voidRec)
  {
    HDF5Group gid(groupHid, name, true);
    Record& rec = *(static_cast<Record*>(voidRec));
    rec.defineRecord (name, HDF5Record::doReadRecord(gid));
    return 0;
  }

  void HDF5Record::remove (const HDF5Object& parentHid,
			   const String& recordName)
  {
    HDF5Group::remove (parentHid, recordName);
  }

  Record HDF5Record::readRecord (const HDF5Object& parentHid,
				 const String& recordName)
  {
    try {
      HDF5Group gid(parentHid, recordName, true);
      return doReadRecord (gid);
    } catch (HDF5Error&) {
      return Record();     // no such group means empty record
    }
  }

  Record HDF5Record::doReadRecord (hid_t groupHid)
  {
    Record rec;
    char cname[512];
    int nfields = H5Aget_num_attrs (groupHid);
    // Iterate through the attributes in order of index, so we're sure
    // they are read back in the same order as written.
    for (int index=0; index<nfields; ++index) {
      HDF5HidAttribute id(H5Aopen_idx(groupHid, index));
      AlwaysAssert (id.getHid()>=0, AipsError);
      unsigned int namsz = H5Aget_name(id, sizeof(cname), cname);
      AlwaysAssert (namsz<sizeof(cname), AipsError);
      String name(cname);
      // Get rank and shape from the dataspace info.
      HDF5HidDataSpace dsid (H5Aget_space(id));
      int rank = H5Sget_simple_extent_ndims(dsid);
      Block<hsize_t> shp(rank);
      if (rank > 0) {
	rank = H5Sget_simple_extent_dims(dsid, shp.storage(), NULL);
      }
      if (rank < 0) {
	throw HDF5Error("Data set array " + name + " does not have dims");
      }
      // Get data type and its size.
      HDF5HidDataType dtid(H5Aget_type(id));
      if (rank == 0) {
	readScalar (id, dtid, name, rec);
      } else {
	readArray (id, dtid, HDF5DataSet::toShape(shp), name, rec);
      }
    }
    // Now read all subrecords.
    //# Use INDEX_NAME (using INDEX_CRT_ORDER results in a return of -1).
    hsize_t idx=0;
    H5Literate (groupHid, H5_INDEX_NAME, H5_ITER_NATIVE, &idx,
		&readSubRecord, &rec);
    return rec;
  }

  void HDF5Record::readScalar (hid_t attrId, hid_t dtid,
			       const String& name, RecordInterface& rec)
  {
    // Handle a scalar field.
    Int sz = H5Tget_size(dtid);
    switch (H5Tget_class(dtid)) {
    case H5T_INTEGER:
      {
	int sgn = H5Tget_sign(dtid);
	if (sgn == H5T_SGN_2) {
	  if (sz == 1) {
	    readSca<Bool> (attrId, name, rec);
	  } else if (sz == sizeof(Short)) {
	    readSca<Short> (attrId, name, rec);
	  } else if (sz == sizeof(Int)) {
	    readSca<Int> (attrId, name, rec);
	  } else {
	    AlwaysAssert (sz==sizeof(Int64), AipsError);
	    readSca<Int64> (attrId, name, rec);
	  }
	} else {
	  if (sz == 1) {
	    readSca<uChar> (attrId, name, rec);
	  } else {
	    AlwaysAssert (sz==sizeof(uInt), AipsError);
	    readSca<uInt> (attrId, name, rec);
	  }
	}
      }
      break;
    case H5T_FLOAT:
      {
	if (sz == sizeof(Float)) {
	  readSca<Float> (attrId, name, rec);
	} else {
	  AlwaysAssert (sz==sizeof(Double), AipsError);
	  readSca<Double> (attrId, name, rec);
	}
      }
      break;
    case H5T_COMPOUND:
      {
	if (H5Tget_nmembers(dtid) == 2) {
	  if (sz == sizeof(Complex)) {
	    readSca<Complex> (attrId, name, rec);
	  } else {
	    AlwaysAssert (sz==sizeof(DComplex), AipsError);
	    readSca<DComplex> (attrId, name, rec);
	  }
	} else {
	  AlwaysAssert (H5Tget_nmembers(dtid)==3, AipsError);
	  readEmptyArray (attrId, name, rec);
	}
      }
      break;
    case H5T_STRING:
      readScaString (attrId, sz, name, rec);
      break;
    default:
      throw HDF5Error ("Unknown data type of scalar attribute " + name);
    }
  }

  void HDF5Record::readArray (hid_t attrId, hid_t dtid, const IPosition& shape,
			      const String& name, RecordInterface& rec)
  {
    Int sz = H5Tget_size(dtid);
    // Handle an array field.
    switch (H5Tget_class(dtid)) {
    case H5T_INTEGER:
      {
	int sgn = H5Tget_sign(dtid);
	if (sgn == H5T_SGN_2) {
	  if (sz == 1) {
	    readArr<Bool> (attrId, shape, name, rec);
	  } else if (sz == sizeof(Short)) {
	    readArr<Short> (attrId, shape, name, rec);
	  } else if (sz == sizeof(Int)) {
	    readArr<Int> (attrId, shape, name, rec);
	  } else {
	    AlwaysAssert (sz==sizeof(Int64), AipsError);
	    readArr<Int64> (attrId, shape, name, rec);
	  }
	} else {
	  if (sz == 1) {
	    readArr<uChar> (attrId, shape, name, rec);
	  } else {
	    AlwaysAssert (sz==sizeof(uInt), AipsError);
	    readArr<uInt> (attrId, shape, name, rec);
	  }
	}
      }
      break;
    case H5T_FLOAT:
      {
	if (sz == sizeof(Float)) {
	  readArr<Float> (attrId, shape, name, rec);
	} else {
	  AlwaysAssert (sz==sizeof(Double), AipsError);
	  readArr<Double> (attrId, shape, name, rec);
	}
      }
      break;
    case H5T_COMPOUND:
      {
	AlwaysAssert (H5Tget_nmembers(dtid)==2, AipsError);
	if (sz == sizeof(Complex)) {
	  readArr<Complex> (attrId, shape, name, rec);
	} else {
	  AlwaysAssert (sz==sizeof(DComplex), AipsError);
	  readArr<DComplex> (attrId, shape, name, rec);
	}
      }
      break;
    case H5T_STRING:
      readArrString (attrId, shape, name, rec);
      break;
    default:
      throw HDF5Error ("Unknown data type of array attribute " + name);
    }
  }

  void HDF5Record::readEmptyArray (hid_t attrId,
				   const String& name, RecordInterface& rec)
  {
    // Initialize to satisfy compiler; HDF5DataType does not use them.
    Int values[] = {0,0,0};
    HDF5DataType dtype(values[1], values[2]);
    read (attrId, values, dtype);
    Int rank = values[1];
    Int dt   = values[2];
    switch (dt) {
    case TpBool:
      rec.define (name, Array<Bool>(IPosition(rank, 0)));
      break;
    case TpUChar:
      rec.define (name, Array<uChar>(IPosition(rank, 0)));
      break;
    case TpShort:
      rec.define (name, Array<Short>(IPosition(rank, 0)));
      break;
    case TpInt:
      rec.define (name, Array<Int>(IPosition(rank, 0)));
      break;
    case TpUInt:
      rec.define (name, Array<uInt>(IPosition(rank, 0)));
      break;
    case TpInt64:
      rec.define (name, Array<Int64>(IPosition(rank, 0)));
      break;
    case TpFloat:
      rec.define (name, Array<Float>(IPosition(rank, 0)));
      break;
    case TpDouble:
      rec.define (name, Array<Double>(IPosition(rank, 0)));
      break;
    case TpComplex:
      rec.define (name, Array<Complex>(IPosition(rank, 0)));
      break;
    case TpDComplex:
      rec.define (name, Array<DComplex>(IPosition(rank, 0)));
      break;
    case TpString:
      rec.define (name, Array<String>(IPosition(rank, 0)));
      break;
    default:
      throw HDF5Error ("Unknown data type of empty array attribute " + name);
    }
  }

  void HDF5Record::read (hid_t attrId, void* value,
			 const HDF5DataType& dtype)
  {
    AlwaysAssert (H5Aread(attrId, dtype.getHidMem(), value)>=0, AipsError);
  }

  void HDF5Record::readScaString (hid_t attrId, Int sz,
				  const String& name, RecordInterface& rec)
  {
    String value;
    if (sz > 0) {
      value.resize (sz);
      HDF5DataType dtype(value);
      AlwaysAssert (H5Aread(attrId, dtype.getHidMem(), 
			    const_cast<char*>(value.c_str()))>=0,
		    AipsError);
      if (value == "__empty__") {
	value = String();
      }
    }
    rec.define (name, value);
  }

  void HDF5Record::readArrString (hid_t attrId, const IPosition& shape,
				  const String& name, RecordInterface& rec)
  {
    Array<String> value(shape);
    std::vector<char*> ptrs(value.nelements());
    HDF5DataType dtype((String*)0);
    AlwaysAssert (H5Aread(attrId, dtype.getHidMem(), &(ptrs[0])) >= 0,
		  AipsError);
    // Copy the strings to the Array and delete the strings.
    Array<String>::iterator aiter=value.begin();
    for (std::vector<char*>::const_iterator viter = ptrs.begin();
	 viter!=ptrs.end();
	 ++aiter, ++viter) {
      *aiter = String(*viter);
      ::free(*viter);
    }
    rec.define (name, value);
  }

  void HDF5Record::writeRecord (const HDF5Object& parentHid,
				const String& recordName,
				const RecordInterface& rec)
  {
    // First delete the record in case it already exists.
    HDF5Group::remove (parentHid, recordName);
    // Create group.
    HDF5Group gid(parentHid, recordName, false, true);
    doWriteRecord (gid, rec);
  }

  void HDF5Record::doWriteRecord (const HDF5Object& groupHid,
				  const RecordInterface& rec)
  {
    for (uInt i=0; i<rec.nfields(); ++i) {
      String name = rec.name(i);
      switch (rec.dataType(i)) {
      case TpBool:
	writeSca<Bool> (groupHid, name, rec, i);
	break;
      case TpUChar:
	writeSca<uChar> (groupHid, name, rec, i);
	break;
      case TpShort:
	writeSca<Short> (groupHid, name, rec, i);
	break;
      case TpInt:
	writeSca<Int> (groupHid, name, rec, i);
	break;
      case TpUInt:
	writeSca<uInt> (groupHid, name, rec, i);
	break;
      case TpInt64:
	writeSca<Int64> (groupHid, name, rec, i);
	break;
      case TpFloat:
	writeSca<Float> (groupHid, name, rec, i);
	break;
      case TpDouble:
	writeSca<Double> (groupHid, name, rec, i);
	break;
      case TpComplex:
	writeSca<Complex> (groupHid, name, rec, i);
	break;
      case TpDComplex:
	writeSca<DComplex> (groupHid, name, rec, i);
	break;
      case TpString:
	writeScaString (groupHid, name, rec.asString(i));
	break;
      case TpArrayBool:
	writeArr<Bool> (groupHid, name, rec, i);
	break;
      case TpArrayUChar:
	writeArr<uChar> (groupHid, name, rec, i);
	break;
      case TpArrayShort:
	writeArr<Short> (groupHid, name, rec, i);
	break;
      case TpArrayInt:
	writeArr<Int> (groupHid, name, rec, i);
	break;
      case TpArrayUInt:
	writeArr<uInt> (groupHid, name, rec, i);
	break;
      case TpArrayInt64:
	writeArr<Int64> (groupHid, name, rec, i);
	break;
      case TpArrayFloat:
	writeArr<Float> (groupHid, name, rec, i);
	break;
      case TpArrayDouble:
	writeArr<Double> (groupHid, name, rec, i);
	break;
      case TpArrayComplex:
	writeArr<Complex> (groupHid, name, rec, i);
	break;
      case TpArrayDComplex:
	writeArr<DComplex> (groupHid, name, rec, i);
	break;
      case TpArrayString:
	writeArrString (groupHid, name, rec.asArrayString(i));
	break;
      case TpRecord:
	{
	  // Write a record in a new subgroup.
	  HDF5Group gid(groupHid, name, false, true);
	  doWriteRecord (gid, rec.asRecord(i));
	}
	break;
      default:
	throw HDF5Error ("Unknown attribute data type");
      }
    }
  }

  void HDF5Record::writeScalar (hid_t groupHid, const String& name,
				const void* value,
				const HDF5DataType& dtype)
  {
    // Create the data space for the scalar.
    HDF5HidDataSpace dsid (H5Screate_simple(0, NULL, NULL));
    AlwaysAssert (dsid.getHid()>=0, AipsError);
    // Create the attribute.
    HDF5HidAttribute id (H5Acreate2(groupHid, name.c_str(), dtype.getHidFile(),
				    dsid, H5P_DEFAULT, H5P_DEFAULT));
    AlwaysAssert (id.getHid()>=0, AipsError);
    AlwaysAssert (H5Awrite(id, dtype.getHidMem(), value)>=0, AipsError);
  }

  void HDF5Record::writeScaString (hid_t groupHid, const String& name,
				   const String& value)
  {
    // It seems that HDF5 cannot handle empty fixed length strings.
    String val(value);
    if (val.empty()) {
      val = "__empty__";
    }
    // Create the data space for the scalar.
    HDF5HidDataSpace dsid (H5Screate_simple(0, NULL, NULL));
    AlwaysAssert (dsid.getHid()>=0, AipsError);
    // Create the attribute.
    HDF5DataType dtype(val);
    HDF5HidAttribute id (H5Acreate2(groupHid, name.c_str(), dtype.getHidFile(),
				    dsid, H5P_DEFAULT, H5P_DEFAULT));
    AlwaysAssert (id.getHid()>=0, AipsError);
    AlwaysAssert (H5Awrite(id, dtype.getHidMem(), val.c_str())>=0, AipsError);
  }

  void HDF5Record::writeArray (hid_t groupHid, const String& name,
			       const void* value, const IPosition& shape,
			       const HDF5DataType& dtype)
  {
    int rank = shape.nelements();
    if (shape.product() == 0) {
      writeEmptyArray (groupHid, name, rank,
		       HDF5DataType::getDataType(dtype.getHidFile()));
      return;
    }
    // Create the data space for the array.
    Block<hsize_t> ls = HDF5DataSet::fromShape (shape);
    HDF5HidDataSpace dsid (H5Screate_simple(rank, ls.storage(), NULL));
    AlwaysAssert (dsid.getHid()>=0, AipsError);
    // Create the attribute.
    HDF5HidAttribute id (H5Acreate2(groupHid, name.c_str(), dtype.getHidFile(),
				    dsid, H5P_DEFAULT, H5P_DEFAULT));
    AlwaysAssert (id.getHid()>=0, AipsError);
    AlwaysAssert (H5Awrite(id, dtype.getHidMem(), value)>=0, AipsError);
  }

  void HDF5Record::writeEmptyArray (hid_t groupHid, const String& name,
				    Int rank, DataType dtype)
  {
    Int values[3];
    values[0] = 1;
    values[1] = rank;
    values[2] = dtype;
    HDF5DataType dtid(values[1], values[2]);
    writeScalar (groupHid, name, values, dtid);
  }

  void HDF5Record::writeArrString (hid_t groupHid, const String& name,
				   const Array<String>& value)
  {
    if (value.nelements() == 0) {
      writeEmptyArray (groupHid, name, value.ndim(), TpString);
      return;
    }
    // Copy the string pointers to a vector.
    std::vector<char*> ptrs(value.nelements());
    Array<String>::const_iterator aiter=value.begin();
    for (std::vector<char*>::iterator viter = ptrs.begin();
	 viter!=ptrs.end();
	 ++aiter, ++viter) {
      *viter = (char*)(aiter->c_str());
    }
    // It appears that HDF5 cannot write an attribute with more than 4000 values.
    // So cut off if needed. 
    IPosition shape = value.shape();
    if (shape[0] > 4000) {
      LogIO os;
      os << "HDF5Record: Cut off size of attribute " + name + " from " +
        String::toString(shape[0]) + " to 4000 values"
         << LogIO::NORMAL << LogIO::POST;
      shape[0] = 4000;
    }
    // Create the data space for the array.
    int rank = shape.nelements();
    Block<hsize_t> ls = HDF5DataSet::fromShape (shape);
    HDF5HidDataSpace dsid (H5Screate_simple(rank, ls.storage(), NULL));
    AlwaysAssert (dsid.getHid()>=0, AipsError);
    // Create the attribute.
    HDF5DataType dtype((String*)0);
    HDF5HidAttribute id (H5Acreate2(groupHid, name.c_str(), dtype.getHidFile(),
				    dsid, H5P_DEFAULT, H5P_DEFAULT));
    AlwaysAssert (id.getHid()>=0, AipsError);
    AlwaysAssert (H5Awrite(id, dtype.getHidMem(), &(ptrs[0])) >= 0,
		  AipsError);
  }

#else

  void HDF5Record::remove (const HDF5Object&,
			   const String&)
  {
    HDF5Object::throwNoHDF5();
  }

  Record HDF5Record::readRecord (const HDF5Object&,
				 const String&)
  {
    HDF5Object::throwNoHDF5();
    return Record();
  }

  Record HDF5Record::doReadRecord (hid_t)
  {
    HDF5Object::throwNoHDF5();
    return Record();
  }

  void HDF5Record::readScalar (hid_t, hid_t,
			       const String&, RecordInterface&)
  {
    HDF5Object::throwNoHDF5();
  }

  void HDF5Record::readArray (hid_t, hid_t, const IPosition&,
			      const String&, RecordInterface&)
  {
    HDF5Object::throwNoHDF5();
  }

  void HDF5Record::readEmptyArray (hid_t,
				   const String&, RecordInterface&)
  {
    HDF5Object::throwNoHDF5();
  }

  void HDF5Record::read (hid_t, void*,
			 const HDF5DataType&)
  {
    HDF5Object::throwNoHDF5();
  }

  void HDF5Record::readScaString (hid_t, Int,
				  const String&, RecordInterface&)
  {
    HDF5Object::throwNoHDF5();
  }

  void HDF5Record::readArrString (hid_t, const IPosition&,
				  const String&, RecordInterface&)
  {
    HDF5Object::throwNoHDF5();
  }

  void HDF5Record::writeRecord (const HDF5Object&,
				const String&,
				const RecordInterface&)
  {
    HDF5Object::throwNoHDF5();
  }

  void HDF5Record::doWriteRecord (const HDF5Object&,
				  const RecordInterface&)
  {
    HDF5Object::throwNoHDF5();
  }

  void HDF5Record::writeScalar (hid_t, const String&,
				const void* ,
				const HDF5DataType&)
  {
    HDF5Object::throwNoHDF5();
  }

  void HDF5Record::writeScaString (hid_t, const String&,
				   const String&)
  {
    HDF5Object::throwNoHDF5();
  }

  void HDF5Record::writeArray (hid_t, const String&,
			       const void*, const IPosition&,
			       const HDF5DataType&)
  {
    HDF5Object::throwNoHDF5();
  }

  void HDF5Record::writeEmptyArray (hid_t, const String&,
				    Int, DataType)
  {
    HDF5Object::throwNoHDF5();
  }

  void HDF5Record::writeArrString (hid_t, const String&,
				   const Array<String>&)
  {
    HDF5Object::throwNoHDF5();
  }

#endif

}
