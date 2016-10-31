//# ImageOpener.cc: A class with static functions to open an image of any type
//# Copyright (C) 2005
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
//

#include <casacore/images/Images/ImageOpener.h>
#include <casacore/tables/Tables/Table.h>
#include <casacore/tables/Tables/TableInfo.h>
#include <casacore/images/Images/PagedImage.h>
#include <casacore/images/Images/HDF5Image.h>
#include <casacore/images/Images/ImageConcat.h>
#include <casacore/images/Images/ImageExpr.h>
#include <casacore/images/Images/ImageExprParse.h>
#include <casacore/lattices/LEL/LatticeExprNode.h>
#include <casacore/casa/HDF5/HDF5File.h>
#include <casacore/casa/Arrays/ArrayIO.h>
#include <casacore/casa/OS/File.h>
#include <casacore/casa/OS/RegularFile.h>
#include <casacore/casa/IO/RegularFileIO.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Regex.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// Initialize registry with the unknown as the default open function.
SimpleOrderedMap<ImageOpener::ImageTypes,ImageOpener::OpenImageFunction*>
     ImageOpener::theirOpenFuncMap(&ImageOpener::unknownImageOpen);

LatticeBase* ImageOpener::unknownImageOpen (const String&,
					    const MaskSpecifier&)
{
  return 0;
}

void ImageOpener::registerOpenImageFunction (ImageTypes type,
					     OpenImageFunction* func)
{
  theirOpenFuncMap.define (type, func);
}

ImageOpener::ImageTypes ImageOpener::imageType (const String& name)
{
  File file(name);
  if (file.isDirectory()) {
    if (Table::isReadable(name)) {
      TableInfo info = Table::tableInfo (name);
      if (info.type() == TableInfo::type(TableInfo::PAGEDIMAGE)) {
	return AIPSPP;
      }
    } else {
      if (File(name + "/header").isRegular()  &&
	  File(name + "/image").isRegular()) {
	return MIRIAD;
      }
    }
  } else if (file.isRegular()) {
    // Find file type.
    String base = file.path().baseName();
    Int i;
    for (i=base.length()-1; i>0; i--) {
      if (base[i] == '.') {
	break;
      }
    }
    if (i > 0  &&  base.after(i) == "image") {
      String descName = file.path().dirName() + '/' +
	                base.before(i) + ".descr";
      if (File(descName).isRegular()) {
	return GIPSY;
      }
    }
    RegularFileIO fio((RegularFile(file)));
    char buf[2880];
    Int nread = fio.read (2880, buf, False);
    if (nread == 2880) {
      String str(buf, 80);
      if (str.matches (Regex("^SIMPLE *= *T.*"))) {
	return FITS;
      }
    }
    // Check if a CompoundImage (ImageConcat or ImageExpr).
    // Skip AipsIO's object length, magicval, and string length.
    if (nread >= 30) {
      String str1(buf+12, 14);
      if (str1 == "CompoundImage-") {
        String str2(buf+26, 4);
        if (str2 == "Conc") {
          return IMAGECONCAT;
        } else if (str2 == "Expr") {
          return IMAGEEXPR;
        }
      }
    }
    if (HDF5File::isHDF5(name)) {
      return HDF5;
    }
  }
  return UNKNOWN;
}

  
LatticeBase* ImageOpener::openPagedImage (const String& fileName,
					  const MaskSpecifier& spec)
{
  Table table(fileName);
  String type = table.tableInfo().type();
  if (type != TableInfo::type(TableInfo::PAGEDIMAGE)) {
    return 0;
  }
  if (table.nrow() != 1) {
    return 0;
  }
  DataType dtype = TpOther;
  String colName;
  ColumnDesc cdesc = table.tableDesc()[0];
  if (cdesc.isArray()) {
    dtype = cdesc.dataType();
    colName = cdesc.name();
  }
  switch (dtype) {
  case TpFloat:
    return new PagedImage<Float> (table, spec);
  case TpDouble:
    return new PagedImage<Double> (table, spec);
  case TpComplex:
    return new PagedImage<Complex> (table, spec);
  case TpDComplex:
    return new PagedImage<DComplex> (table, spec);
  default:
    return 0;
  }
}

LatticeBase* ImageOpener::openHDF5Image (const String& fileName,
					 const MaskSpecifier& spec)
{
  if (! HDF5File::isHDF5(fileName)) {
    return 0;
  }
  // See if it is an image or just an array.
  if (! isHDF5Image(fileName)) {
    return 0;
  }
  DataType dtype = hdf5imagePixelType(fileName);
  switch (dtype) {
  case TpFloat:
    return new HDF5Image<Float> (fileName, spec);
  case TpDouble:
    return new HDF5Image<Double> (fileName, spec);
  case TpComplex:
    return new HDF5Image<Complex> (fileName, spec);
  case TpDComplex:
    return new HDF5Image<DComplex> (fileName, spec);
  default:
    return 0;
  }
}

LatticeBase* ImageOpener::openImageConcat (const String& fileName)
{
  AipsIO aio(fileName, ByteIO::Old);
  AlwaysAssert (aio.getstart("CompoundImage-Conc") == 0, AipsError);
  Int dtype;
  aio >> dtype;
  LatticeBase* img = 0;
  switch (dtype) {
  case TpFloat:
    img = new ImageConcat<Float> (aio, fileName);
    break;
  case TpDouble:
    img = new ImageConcat<Double> (aio, fileName);
    break;
  case TpComplex:
    img = new ImageConcat<Complex> (aio, fileName);
    break;
  case TpDComplex:
    img = new ImageConcat<DComplex> (aio, fileName);
    break;
  default:
    break;
  }
  aio.getend();
  return img;
}

LatticeBase* ImageOpener::openImageExpr (const String& fileName)
{
  AipsIO aio(fileName, ByteIO::Old);
  AlwaysAssert (aio.getstart("CompoundImage-Expr") == 0, AipsError);
  Int dtype;
  aio >> dtype;
  AlwaysAssert (aio.getstart("ImageExpr") == 1, AipsError);
  String expr;
  aio >> expr;
  LatticeBase* img = openExpr (expr, Block<LatticeExprNode>(), fileName);
  aio.getend();
  aio.getend();
  return img;
}

LatticeBase* ImageOpener::openExpr (const String& expr,
                                    const Block<LatticeExprNode>& nodes,
                                    const String& fileName)
{
  LatticeBase* lattice = 0;
  PtrBlock<const ImageRegion*> regions;
  LatticeExprNode node = ImageExprParse::command (expr, nodes, regions);
  switch (node.dataType()) {
  case TpFloat:
    lattice = new ImageExpr<Float> (LatticeExpr<Float>(node), expr, fileName);
    break;
  case TpDouble:
    lattice = new ImageExpr<Double> (LatticeExpr<Double>(node), expr, fileName);
    break;
  case TpComplex:
    lattice = new ImageExpr<Complex> (LatticeExpr<Complex>(node), expr, fileName);
    break;
  case TpDComplex:
    lattice = new ImageExpr<DComplex> (LatticeExpr<DComplex>(node), expr, fileName);
    break;
  default:
    throw AipsError ("invalid data type of image expression " + expr);
  }
  return lattice;
}


LatticeBase* ImageOpener::openImage (const String& fileName,
				     const MaskSpecifier& spec)
{
   if (fileName.empty()) {
     return 0;
   }
   ImageOpener::ImageTypes type = ImageOpener::imageType(fileName);
   // Do not require the registration of a PagedImage or HDF5Image openFunction.
   if (type == AIPSPP) {
     return openPagedImage (fileName, spec);
   } else if (type == HDF5) {
     return openHDF5Image (fileName, spec);
   } else if (type == IMAGECONCAT) {
     return openImageConcat (fileName);
   } else if (type == IMAGEEXPR) {
     return openImageExpr (fileName);
   }
   // Try to open a foreign image.
   return theirOpenFuncMap(type) (fileName, spec);
}


} //# NAMESPACE CASACORE - END
