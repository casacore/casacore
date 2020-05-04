//# VACEngine.h: Base virtual column for an array column with any type
//# Copyright (C) 1994,1995,1996,1999,2000
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

#ifndef TABLES_VACENGINE_H
#define TABLES_VACENGINE_H

//# Includes
#include <casacore/casa/aips.h>
#include <casacore/tables/DataMan/VirtColEng.h>
#include <casacore/tables/DataMan/VirtArrCol.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Base virtual column for an array column with any type
// </summary>

// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="">
// </reviewed>

// <use visibility=export>

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> VirtualColumnEngine
//   <li> VirtualArrayColumn
// </prerequisite>

// <etymology>
// VACEngine stands for Virtual Array Column Engine, i.e. a class
// handling a virtual table column containing array values.
// </etymology>

// <synopsis> 
// VACEngine is a base virtual column engine to handle a column
// with an arbitrary type.
// Data of columns with standard data types can directly be stored
// in a Table using a storage manager, but data of column with non-standard
// types have to be stored in another way.
// The way to do this is to split the object with the non-standard
// type into its individual elements, which are subsequently put into the
// appropriate columns.
//
// A virtual column engine has to be implemented for each non-standard
// data type, which has to be stored in a table. This engine has to get
// and put the individual parts the object.
// VACEngine is the base class for such engines, so the actual
// engine quite simple to implement. The example shows the implementation
// of an engine AVACEngine handling a data type A.
//
// In principle the name of the engine class is free, but it is strongly
// recommended to use the name <src><dataTypeId>VACEngine</src>, where VAC
// stands for Virtual Array Column (e.g. <src>AVACEngine</src> for class A).
// In this way the default data manager name supplied by the class and by
// class ArrayColumnDesc can be used.
// </synopsis> 

// <example>
// This example shows the implementation of an engine class AVACEngine,
// which stores the data of a class A.
// The data objects A are stored in a column called the source column.
// The user has to associate two target columns with it. The engine stores
// the data parts x and y in the target columns.
// The names of the target columns are stored as keywords in the source
// column. In this way the engine can reconstruct itself when the table
// is read back.
//
// In the example all AVACEngine functions are shown inline, but they
// should be implemented out-of-line in a separate .cc file.
// <srcblock>
//  //# AVACEngine.h: Example virtual column engine to handle data type A
//
//  #if !defined(AIPS_AVACENGINE_H)
//  #define AIPS_AVACENGINE_H
//
//  //# Includes
//  #include <casacore/tables/DataMan/VACEngine.h>
//  #include <casacore/tables/Tables/ArrayColumn.h>
//
//  // Define the class A.
//  class A
//  {
//  public:
//      A(): x_p(0), y_p(0) {}
//      A(Int x, float y) : x_p(x), y_p(y) {}
//      A(const A& that): x_p(that.x_p), y_p(that.y_p) {}
//      static String dataTypeId()
//          { return "A"; }
//      Int x() const
//          { return x_p; }
//      float y() const
//          { return y_p; }
//      Int& x()
//          { return x_p; }
//      float& y()
//          { return y_p; }
//      int operator== (const A& that) const
//          { return x_p==that.x_p && y_p==that.y_p; }
//      int operator< (const A& that) const
//          { return x_p<that.x_p || (x_p==that.x_p && y_p<that.y_p); }
//  private:
//      Int   x_p;
//      float y_p;
//  };
//
//  // Now define the engine to handle objects of type A.
//  class AVACEngine : public VACEngine<A>
//  {
//  public:
//
//      // The default constructor is required for reconstruction of the
//      // engine when a table is read back.
//      AVACEngine()
//      {}
//
//      // Construct the engine for the given source column and storing
//      // the result in the given target columns for the data members
//      // x and y of class A.
//      AVACEngine (const String& sourceColumnName,
//                  const String& xTargetColumnName,
//                  const String& yTargetColumnname)
//      : VACEngine<A>  (sourceColumnName),
//        xTargetName_p (xTargetColumnName),
//        yTargetName_p (yTargetColumnName)
//      {}
//
//      // Destructor is mandatory.
//      virtual ~AVACEngine()
//      {}
//
//      // Clone the object.
//      virtual DataManager* clone() const
//      {
//          DataManager* dmPtr = new AVACEngine (sourceColumnName(),
//					 xTargetName_p, yTargetName_p);
//          return dmPtr;
//      }
//
//      // Store the target column names in the source column keywords.
//      virtual void create (rownr_t)
//      {
//          TableColumn src (table(), sourceColumnName());
//          src.keywordSet().keysString()("_xTargetName") = xTargetName_p;
//          src.keywordSet().keysString()("_yTargetName") = yTargetName_p;
//      }
//
//      // Prepare the engine by allocating column objects
//      // for the target columns.
//      virtual void prepare()
//      {
//          TableColumn src (table(), sourceColumnName());
//          xTargetName_p = src.keywordSet().asString ("_xTargetName");
//          yTargetName_p = src.keywordSet().asString ("_yTargetName");
//          rocolx.attach (table(), xTargetName_p);
//          rocoly.attach (table(), yTargetName_p);
//          if (table().isWritable()) {
//              colx.attach (table(), xTargetName_p);
//              coly.attach (table(), yTargetName_p);
//          }
//      }
//
//      // Get the data from a row.
//      virtual void get (rownr_t rownr, A& value)
//      {
//          rocolx.get (rownr, value.x());
//          rocoly.get (rownr, value.y());
//      }
//
//      // Put the data in a row.
//      virtual void put (rownr_t rownr, const A& value)
//      {
//          colx.put (rownr, value.x());
//          coly.put (rownr, value.y());
//      }
//
//      // Register the class name and the static makeObject "constructor".
//      // This will make the engine known to the table system.
//      static void registerClass()
//      {
//          DataManager::registerCtor ("AVACEngine", makeObject);
//      }
//
//  private:
//      // Copy constructor is only used by clone().
//      // (so it is made private).
//      AVACEngine (const AVACEngine&)
//      : VACEngine<A>  (that),
//        xTargetName_p (that.xTargetName_p),
//        yTargetName_p (that.yTargetName_p)
//      {}
//
//      // Assignment is not needed and therefore forbidden
//      // (so it is made private and is not implemented).
//      AVACEngine& operator= (const AVACEngine&);
//
//
//      // The target column names.
//      String xTargetName_p;
//      String yTargetName_p;
//      // Objects for the target columns.
//      ArrayColumn<Int>     colx;       // used by put
//      ArrayColumn<Int>   rocolx;     // used by get
//      ArrayColumn<float>   coly;       // used by put
//      ArrayColumn<float> rocoly;     // used by get
//
//  public:
//      // Define the "constructor" to construct this engine when a
//      // table is read back.
//      // This "constructor" has to be registered by the user of the engine.
//      // Function registerClass() is doing that.
//      static DataManager* makeObject (const String& dataManagerType)
//      {
//          DataManager* dmPtr = new AVACEngine();
//          return dmPtr;
//      }
//  };
//
//  #endif
// </srcblock>
//
// User code using this engine to create a new table could look like:
// <srcblock>
//   // Register the engine.
//   // This is not needed if the engine is registered as part
//   // of the general DataManager::registerAllCtor function.
//   AVACEngine::registerClass();
//   // Create the table description.
//   TableDesc td;
//   td.addColumn (ArrayColumnDesc<A>("source"));
//   td.addColumn (ArrayColumnDesc<Int>("xTarget"));
//   td.addColumn (ArrayColumnDesc<Int>("yTarget"));
//   SetupNewTable setup ("table.name", td, Table::New);
//   // Define the engine for column "source".
//   AVACEngine engine ("source", "xTarget", "yTarget");
//   Table tab (setup, 10);
//   // Put data into column "source".
//   ArrayColumn<A> col (tab, "source");
//   for (uInt i=0; i<10; i++) {
//       col.put (i, someA);     // writes indirectly xTarget and yTarget
//   }
// </srcblock>
// </example>
//
// <motivation>
// This class makes it easier for the user to implement the engine.
// It supplies several default functions.
// </motivation>

// <templating arg=T>
//  <li> Default constructor    T();
//  <li> Copy constructor       T(const T&);
//  <li> Assignment operator    T& operator= (const T&);
//  <li> comparison operator    int operator== (const T&) const;
//  <li> comparison operator    int operator<  (const T&) const; 
//  <li> identification         <src>static String dataTypeId();</src>
//       This should return the (unique) name of the class, thus
//       when T is templated in its turn, the name should contain the
//       template argument name.
// </templating>


template<class T>
class VACEngine : public VirtualColumnEngine,
                  public VirtualArrayColumn<T>
{
  //# Make members of parent class known.
public:
  using VirtualArrayColumn<T>::dataTypeId;

public:
    // The default constructor is required for reconstruction of the
    // engine when a table is read back.
    // It is also used to construct an engine, which does not check
    // the source column name.
    VACEngine();

    // Construct an engine to handle a column with an arbitrary data type.
    // Later it will check if the source column name is correct.
    VACEngine (const String& sourceColumnName);

    // Destructor is mandatory.
    ~VACEngine();

    // Return the data manager type name.
    // This defaults to the data type ID followed by VACEngine
    // (meaning Virtual Array Column Engine).
    String dataManagerType() const;

    // Get the name of the source column.
    const String& sourceColumnName() const
	{ return sourceName_p; }

protected:

    // Copy constructor is only used by clone().
    // (so it is made protected).
    VACEngine (const VACEngine<T>&);

private:
    // Assignment is not needed and therefore forbidden
    // (so it is made private).
    VACEngine<T>& operator= (const VACEngine<T>&);

    // The column is in principle writable.
    // This does not mean it is actually writable, because that
    // depends on the fact if the table is writable.
    Bool isWritable() const;

    // Create the column object for the array column in this engine.
    // It will check if the given column name matches the source
    // column name. This assures that the engine is bound to the
    // correct column.
    DataManagerColumn* makeDirArrColumn (const String& columnName,
					 int dataType,
					 const String& dataTypeID);
    DataManagerColumn* makeIndArrColumn (const String& columnName,
					 int dataType,
					 const String& dataTypeID);


    //# Now define the data members.
    String         sourceName_p;           //# source column name
};



} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/tables/DataMan/VACEngine.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
#endif
