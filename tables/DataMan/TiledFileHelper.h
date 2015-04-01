//# TiledFileHelper.h: Helper class for tiled access to an array in a file
//# Copyright (C) 2001,2002
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

#ifndef TABLES_TILEDFILEHELPER_H
#define TABLES_TILEDFILEHELPER_H


//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/TiledStMan.h>
#include <casacore/tables/Tables/TableDesc.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Helper class for tiled access to an array in a file.
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="" tests="tTiledFileAccess.cc">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> Description of Tiled Storage Manager in module file
//        <linkto module=Tables:TiledStMan>Tables.h</linkto>
// </prerequisite>

// <synopsis> 
// TiledFileHelper is a helper class for class
// <linkto class=TiledFileAccess>TiledFileAccess</linkto>.
// It sets up a table description containing one array column
// to make it possible to use the
// <linkto class=TiledStMan>tiled storage manager</linkto>
// to access an array in an arbitrary file.
// </synopsis> 

// <motivation>
// This class was created to be able to read an image in a FITS file.
// </motivation>

//# <todo asof="$DATE:$">
//# A List of bugs, limitations, extensions or planned refinements.
//# </todo>


class TiledFileHelper : public TiledStMan
{
public:
  // Create a TiledFileHelper object.
  // Tell if the data is stored in big or little endian canonical format.
  TiledFileHelper (const String& fileName, const IPosition& shape,
		   DataType dtype, const TSMOption&,
		   Bool writable, Bool bigEndian);

  ~TiledFileHelper();

  virtual const TableDesc& getDesc() const;

  TSMFile* file()
    { return fileSet_p[0]; }

  // Return the class name.
  virtual String dataManagerType() const;

  // These functions are pure virtual, but not needed here.
  // They throw an exception.
  // <group>
  virtual DataManager* clone() const;
  virtual Bool flush (AipsIO&, Bool);
  virtual void create (uInt);
  virtual TSMCube* getHypercube (uInt);
  virtual TSMCube* getHypercube (uInt, IPosition&);
  virtual void readHeader (uInt, Bool);
  // </group>

private:
  // Forbid copy constructor and assignment.
  // <group>
  TiledFileHelper (const TiledFileHelper&);
  TiledFileHelper& operator= (const TiledFileHelper&);
  // </group>


  TableDesc itsDesc;
};



} //# NAMESPACE CASACORE - END

#endif
