//# dVACEngine.h: Example virtual column engine to handle data type A
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

#ifndef TABLES_DVACENGINE_H
#define TABLES_DVACENGINE_H


//# Includes
#include <casacore/tables/DataMan/VACEngine.h>
#include <casacore/tables/Tables/ArrayColumn.h>


#include <casacore/casa/namespace.h>
class VACExample
{
public:
    VACExample(): x_p(0), y_p(0) {}
    VACExample(Int x, float y, const String& z) : x_p(x), y_p(y), z_p(z) {}
    VACExample(const VACExample& that): x_p(that.x_p), y_p(that.y_p), z_p(that.z_p) {}
    static String dataTypeId()
	{ return "VACExample"; }
    Int x() const
	{ return x_p; }
    float y() const
	{ return y_p; }
  
    const String& z() const
	{ return z_p; }
    Int& x()
	{ return x_p; }
    float& y()
	{ return y_p; }
    String& z()
	{ return z_p; }
    int operator== (const VACExample& that) const
	{ return x_p==that.x_p && y_p==that.y_p && z_p==that.z_p; }
    int operator< (const VACExample& that) const
	{ return x_p<that.x_p || (x_p==that.x_p && y_p<that.y_p) ||
            (x_p==that.x_p && y_p==that.y_p && z_p<that.z_p); }
private:
    Int    x_p;
    float  y_p;
    String z_p;
};


// <summary>
// Example virtual column engine to handle data type VACExample
// </summary>

// <use visibility=export>

// <reviewed reviewer="GvD" date="2004/07/09" tests="">

// <prerequisite>
//# Classes you should understand before using this one.
//   <li> VirtualColumnEngine
//   <li> VirtualArrayColumn
// </prerequisite>

// <synopsis> 
// VACExampleVACEngine is an example showing how to use the templated
// class VACEngine to handle table data with an arbitrary type.
// Data of columns with the standard data types can directly be
// stored using a storage manager, but data of column with non-standard
// types have to be stored in another way.
//
// The normal way of doing this is to split the object of the non-standard
// type into its elementary types.
// This eample uses the class VACExample as the data type to be handled.
// It consists of 2 data fields, which will transparently be stored in
// 2 separate columns.
// </synopsis> 


class VACExampleVACEngine : public VACEngine<VACExample>
{
public:

    // The default constructor is required for reconstruction of the
    // engine when a table is read back.
    // It is also used to construct an engine, which does not check
    // the source column name.
    VACExampleVACEngine();

    // Construct the engine for the given source column and storing
    // the result in the given target columns for the data members
    // x and y of class VACExample.
    VACExampleVACEngine (const String& sourceColumnName,
			 const String& xTargetColumnName,
			 const String& yTargetColumnname,
			 const String& zTargetColumnname);

    // Destructor is mandatory.
    virtual ~VACExampleVACEngine();

    // Clone the object.
    DataManager* clone() const;

    // Store the target column names in the source column keywords.
    virtual void create (rownr_t);

    // Prepare the engine by allocating column objects
    // for the target columns.
    virtual void prepare();

    virtual void setShape (rownr_t rownr, const IPosition& shape);
    virtual Bool isShapeDefined (rownr_t rownr);
    virtual IPosition shape (rownr_t rownr);

    // Get the data from a row.
    virtual void getArray (rownr_t rownr, Array<VACExample>& value);

    // Put the data in a row.
    virtual void putArray (rownr_t rownr, const Array<VACExample>& value);

    // Register the class name and the static makeObject "constructor".
    // This will make the engine known to the table system.
    static void registerClass();

private:
    // Copy constructor is only used by clone().
    // (so it is made private).
    VACExampleVACEngine (const VACExampleVACEngine&);

    // Assignment is not needed and therefore forbidden
    // (so it is made private).
    VACExampleVACEngine& operator= (const VACExampleVACEngine&);


    // The target column names.
    String xTargetName_p;
    String yTargetName_p;
    String zTargetName_p;
    // Objects for the target columns.
    ArrayColumn<Int>    colx;
    ArrayColumn<float>  coly;
    ArrayColumn<String> colz;

public:
    //*display 4
    // Define the "constructor" to construct this engine when a
    // table is read back.
    // This "constructor" has to be registered by the user of the engine.
    static DataManager* makeObject (const String& dataManagerName,
				    const Record& spec);
};


#endif
