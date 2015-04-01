//# DataMan.h: The DataMan module - Casacore table data managers
//# Copyright (C) 1994-2010
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
//# $Id: Tables.h 21434 2014-05-07 13:07:20Z gervandiepen $

#ifndef TABLES_DATAMAN_H
#define TABLES_DATAMAN_H

//# Includes
//#   storage managers
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/StManAipsIO.h>
#include <casacore/tables/DataMan/StandardStMan.h>
#include <casacore/tables/DataMan/StandardStManAccessor.h>
#include <casacore/tables/DataMan/IncrementalStMan.h>
#include <casacore/tables/DataMan/IncrStManAccessor.h>
#include <casacore/tables/DataMan/TiledDataStMan.h>
#include <casacore/tables/DataMan/TiledDataStManAccessor.h>
#include <casacore/tables/DataMan/TiledCellStMan.h>
#include <casacore/tables/DataMan/TiledColumnStMan.h>
#include <casacore/tables/DataMan/TiledShapeStMan.h>
#include <casacore/tables/DataMan/MemoryStMan.h>

//#   virtual column engines
#include <casacore/tables/DataMan/RetypedArrayEngine.h>
#include <casacore/tables/DataMan/RetypedArraySetGet.h>
#include <casacore/tables/DataMan/ScaledArrayEngine.h>
#include <casacore/tables/DataMan/MappedArrayEngine.h>
#include <casacore/tables/DataMan/ForwardCol.h>
#include <casacore/tables/DataMan/ForwardColRow.h>
#include <casacore/tables/DataMan/CompressComplex.h>
#include <casacore/tables/DataMan/CompressFloat.h>
#include <casacore/tables/DataMan/VirtualTaQLColumn.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <module>

// <summary>
// DataManagers are the physical representation of table data.
// </summary>

// <use visibility=export>

// <reviewed reviewer="jhorstko" date="1994/08/30" tests="" demos="">
// </reviewed>

// <prerequisite>
//    <li> <linkto module="Tables:description">Tables</linkto> module
// </prerequisite>

// <etymology>
// DataMan is the abbreviation of data managers.
// </etymology>

// <synopsis> 
// Tables are the fundamental storage mechanism for Casacore.
// Tables themselves are a logical organization of the data.
// Table data are physically stored (or calculated on the fly)
// using data managers.
// <br>Casacore ships with several data managers, but it is possible
// to write a specific data manager that can be loaded dynamically
// from a shared library.
// <br>See the <linkto module="Tables:Data Managers">Tables module</linkto>
// for more information.

// </synopsis>
// </module>



} //# NAMESPACE CASACORE - END

#endif
