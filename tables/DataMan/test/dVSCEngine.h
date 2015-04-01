//# dVSCEngine.h: Example virtual column engine to handle data type A
//# Copyright (C) 1994,1995,1996,2001
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

#ifndef TABLES_DVSCENGINE_H
#define TABLES_DVSCENGINE_H


//# Includes
#include <casacore/tables/DataMan/VSCEngine.h>
#include <casacore/tables/Tables/ScalarColumn.h>


#include <casacore/casa/namespace.h>
class VSCExample
{
public:
    VSCExample(): x_p(0), y_p(0) {}
    VSCExample(Int x, float y) : x_p(x), y_p(y) {}
    VSCExample(const VSCExample& that): x_p(that.x_p), y_p(that.y_p) {}
    static String dataTypeId()
	{ return "VSCExample"; }
    Int x() const
	{ return x_p; }
    float y() const
	{ return y_p; }
    Int& x()
	{ return x_p; }
    float& y()
	{ return y_p; }
    int operator== (const VSCExample& that) const
	{ return x_p==that.x_p && y_p==that.y_p; }
    int operator< (const VSCExample& that) const
	{ return x_p<that.x_p || (x_p==that.x_p && y_p<that.y_p); }
private:
    Int   x_p;
    float y_p;
};


// <summary>
// Example virtual column engine to handle data type VSCExample
// </summary>

// <use visibility=export>

// <reviewed reviewer="GvD" date="2004/07/09" tests="">

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> VirtualColumnEngine
//   <li> VirtualScalarColumn
// </prerequisite>

// <synopsis> 
// ExampleVSCEngine is an example showing how to use the templated
// class VSCEngine to handle table data with an arbitrary type.
// Data of columns with the standard data types can directly be
// stored using a storage manager, but data of column with non-standard
// types have to be stored in another way.
//
// The normal way of doing this is to split the object of the non-standard
// type into its elementary types.
// This eample uses the class VSCExample as the data type to be handled.
// It consists of 2 data fields, which will transparently be stored in
// 2 separate columns.
// </synopsis> 


class VSCExampleVSCEngine : public VSCEngine<VSCExample>
{
public:

    // The default constructor is required for reconstruction of the
    // engine when a table is read back.
    // It is also used to construct an engine, which does not check
    // the source column name.
    VSCExampleVSCEngine();

    // Construct the engine for the given source column and storing
    // the result in the given target columns for the data members
    // x and y of class VSCExample.
    VSCExampleVSCEngine (const String& sourceColumnName,
			 const String& xTargetColumnName,
			 const String& yTargetColumnname);

    // Destructor is mandatory.
    ~VSCExampleVSCEngine();

    // Clone the object.
    DataManager* clone() const;

    // Store the target column names in the source column keywords.
    void create (uInt);

    // Prepare the engine by allocating column objects
    // for the target columns.
    void prepare();

    // Get the data from a row.
    void get (uInt rownr, VSCExample& value);

    // Put the data in a row.
    void put (uInt rownr, const VSCExample& value);

    // Register the class name and the static makeObject "constructor".
    // This will make the engine known to the table system.
    static void registerClass();

private:
    // Copy constructor is only used by clone().
    // (so it is made private).
    VSCExampleVSCEngine (const VSCExampleVSCEngine&);

    // Assignment is not needed and therefore forbidden
    // (so it is made private).
    VSCExampleVSCEngine& operator= (const VSCExampleVSCEngine&);


    // The target column names.
    String xTargetName_p;
    String yTargetName_p;
    // Objects for the target columns.
    ScalarColumn<Int>   colx;
    ScalarColumn<float> coly;

public:
    //*display 4
    // Define the "constructor" to construct this engine when a
    // table is read back.
    // This "constructor" has to be registered by the user of the engine.
    static DataManager* makeObject (const String& dataManagerName,
				    const Record& spec);
};


#endif
