//# MappedArrayEngine.h: Templated virtual column engine to map a table array
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

#ifndef TABLES_MAPPEDARRAYENGINE_H
#define TABLES_MAPPEDARRAYENGINE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/BaseMappedArrayEngine.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN


// <summary>
// Templated virtual column engine to map the data type of a table array
// </summary>

// <use visibility=export>

// <reviewed reviewer="Gareth Hunt" date="94Nov17" tests="">
// </reviewed>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> VirtualColumnEngine
//   <li> VirtualArrayColumn
// </prerequisite>

// <synopsis> 
// MappedArrayEngine is a virtual column engine which maps an array
// of one type to another type (without any scaling).
//
// An engine object should be used for one column only, because the stored
// column name is part of the engine. If it would be used for more than
// one column, they would all share the same stored column.
// When the engine is bound to a column, it is checked if the name
// of that column matches the given virtual column name.
//
// The engine can be used for a column containing any kind of array
// (thus direct or indirect, fixed or variable shaped)) as long as the
// virtual array can be stored in the stored array. Thus a fixed shaped
// virtual can use a variable shaped stored, but not vice versa.
// A fixed shape indirect virtual can use a stored with direct arrays.
// </synopsis> 

// <motivation>
// For precision it is sometimes needed to store the visibility data in a
// MeasurementSet in double precision. To be able to use other applications
// on such data, it is needed to map them to single precision.
//
// Because the engine can serve only one column, it was possible to
// combine the engine and the column functionality in one class.
// This has been achieved using multiple inheritance.
// The advantage of this is that only one templated class is used,
// so less template instantiations are needed.
// </motivation>

// <example>
// <srcblock>
// // Create the table description and 2 columns with indirect arrays in it.
// // The Int column will be stored, while the double will be
// // used as virtual.
// TableDesc tableDesc ("", TableDesc::Scratch);
// tableDesc.addColumn (ArrayColumnDesc<Int> ("storedArray"));
// tableDesc.addColumn (ArrayColumnDesc<double> ("virtualArray"));
//
// // Create a new table using the table description.
// SetupNewTable newtab (tableDesc, "tab.data", Table::New);
//
// // Create the array mapping engine to map from double to Int
// // and bind it to the double column.
// // Create the table.
// MappedArrayEngine<double,Int> mappingEngine("virtualArray",
//                                             "storedArray", 10);
// newtab.bindColumn ("virtualArray", mappingEngine);
// Table table (newtab);
//
// // Store a 3-D array (with dim. 2,3,4) into each row of the column.
// // The shape of each array in the column is implicitly set by the put
// // function. This will also set the shape of the underlying Int array.
// ArrayColumn data (table, "virtualArray");
// Array<double> someArray(IPosition(4,2,3,4));
// someArray = 0;
// for (uInt i=0, i<10; i++) {          // table will have 10 rows
//     table.addRow();
//     data.put (i, someArray)
// }
// </srcblock>
// </example>

// <templating arg=VirtualType>
//  <li> only suited for built-in numerics data types
// </templating>
// <templating arg=StoredType>
//  <li> only suited for built-in numerics data types
// </templating>

template<class VirtualType, class StoredType> class MappedArrayEngine : public BaseMappedArrayEngine<VirtualType, StoredType>
{
  //# Make members of parent class known.
public:
  using BaseMappedArrayEngine<VirtualType,StoredType>::virtualName;
protected:
  using BaseMappedArrayEngine<VirtualType,StoredType>::storedName;
  using BaseMappedArrayEngine<VirtualType,StoredType>::table;
  using BaseMappedArrayEngine<VirtualType,StoredType>::column;
  using BaseMappedArrayEngine<VirtualType,StoredType>::setNames;

public:
  // Construct an engine to map all arrays in a column.
  // StoredColumnName is the name of the column where the mapped
  // data will be put and must have data type StoredType.
  // The virtual column using this engine must have data type VirtualType.
  MappedArrayEngine (const String& virtualColumnName,
		     const String& storedColumnName);

  // Construct from a record specification as created by dataManagerSpec().
  MappedArrayEngine (const Record& spec);

  // Destructor is mandatory.
  ~MappedArrayEngine();

  // Return the type name of the engine (i.e. its class name).
  virtual String dataManagerType() const;

  // Get the name given to the engine (is the virtual column name).
  virtual String dataManagerName() const;
  
  // Record a record containing data manager specifications.
  virtual Record dataManagerSpec() const;

  // Return the name of the class.
  // This includes the names of the template arguments.
  static String className();

  // Register the class name and the static makeObject "constructor".
  // This will make the engine known to the table system.
  // The automatically invoked registration function in DataManReg.cc
  // contains MappedArrayEngine<double,Int>.
  // Any other instantiation of this class must be registered "manually"
  // (or added to DataManReg.cc).
  static void registerClass();

private:
  // Copy constructor is only used by clone().
  // (so it is made private).
  MappedArrayEngine (const MappedArrayEngine<VirtualType,StoredType>&);

  // Assignment is not needed and therefore forbidden
  // (so it is made private and not implemented).
  MappedArrayEngine<VirtualType,StoredType>& operator=
                       (const MappedArrayEngine<VirtualType,StoredType>&);

  // Clone the engine object.
  DataManager* clone() const;

  // Copy the stored array to the virtual array.
  virtual void mapOnGet (Array<VirtualType>& array,
                         const Array<StoredType>& stored);

  // Copy the virtual array to the stored array.
  virtual void mapOnPut (const Array<VirtualType>& array,
                         Array<StoredType>& stored);


public:
  // Define the "constructor" to construct this engine when a
  // table is read back.
  // This "constructor" has to be registered by the user of the engine.
  // If the engine is commonly used, its registration can be added
  // to the registerAllCtor function in DataManReg.cc. 
  // That function gets automatically invoked by the table system.
  static DataManager* makeObject (const String& dataManagerType,
				  const Record& spec);
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/tables/DataMan/MappedArrayEngine.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
