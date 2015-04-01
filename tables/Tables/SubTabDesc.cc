//# SubTabDesc.cc: Description of columns containing tables
//# Copyright (C) 1994,1995,1996,1997,2001
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

#include <casacore/tables/Tables/SubTabDesc.h>
#include <casacore/tables/Tables/TableDesc.h>
#include <casacore/tables/Tables/TableAttr.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/tables/Tables/TableError.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/iostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

SubTableDesc::SubTableDesc (const String& name, const String& comment,
			    const String& descname, int opt)
: BaseColumnDesc(name, comment, "", "", TpTable, "", opt, 1, IPosition(),
		 False, False, True),
  tabDescPtr_p  (0),
  tabDescTyp_p  (descname),
  byName_p      (True),
  allocSelf_p   (True),
  shallowCopy_p (False)
    { readTableDesc(); }

SubTableDesc::SubTableDesc (const String& name, const String& comment,
			    const TableDesc& desc, int opt)
: BaseColumnDesc(name, comment, "", "", TpTable, "", opt, 1, IPosition(),
		 False, False, True),
  tabDescPtr_p  (new TableDesc(desc, "", "", TableDesc::Scratch)),
  tabDescTyp_p  (desc.getType()),
  byName_p      (False),
  allocSelf_p   (True),
  shallowCopy_p (False)
{}
  
SubTableDesc::SubTableDesc (const String& name, const String& comment,
			    TableDesc* descptr, int opt)
: BaseColumnDesc(name, comment, "", "", TpTable, "", opt, 1, IPosition(),
		 False, False, True),
  tabDescPtr_p  (descptr),
  tabDescTyp_p  (descptr->getType()),
  byName_p      (False),
  allocSelf_p   (False),
  shallowCopy_p (True)
{}
  
SubTableDesc::SubTableDesc (const SubTableDesc& that)
: BaseColumnDesc(that),
  tabDescPtr_p  (0),
  tabDescTyp_p  (""),
  allocSelf_p   (False)
    { operator= (that); }

//# Make a new object.
BaseColumnDesc* SubTableDesc::makeDesc (const String&)
{
    BaseColumnDesc* ptr = new SubTableDesc("", "", TableDesc());
    return ptr;
}

SubTableDesc::~SubTableDesc()
{ 
    if (allocSelf_p) {
	delete tabDescPtr_p;
    }
}


SubTableDesc& SubTableDesc::operator= (const SubTableDesc& that)
{
    BaseColumnDesc::operator= (that);
    if (allocSelf_p) {
	delete tabDescPtr_p;
    }
    tabDescPtr_p  = 0;
    tabDescTyp_p  = that.tabDescTyp_p;
    byName_p      = that.byName_p;
    allocSelf_p   = True;
    shallowCopy_p = that.shallowCopy_p;
    if (shallowCopy_p) {
	tabDescPtr_p  = that.tabDescPtr_p;
	allocSelf_p   = False;
    }else if (byName_p) {
	readTableDesc();
    }else if (that.tabDescPtr_p != 0) {
	tabDescPtr_p = new TableDesc (*that.tabDescPtr_p,
				      "", "", TableDesc::Scratch);
    }
    return *this;
}


//# Clone this column description to another.
BaseColumnDesc* SubTableDesc::clone() const
{
    SubTableDesc* ptr = new SubTableDesc(*this);
    return ptr;
}


//# Return the class name.
String SubTableDesc::className() const
    { return "SubTableDesc"; }


//# Put the object.
//# The data is read by the ctor taking AipsIO.
//# It was felt that putstart takes too much space, so therefore
//# the version is put "manually".
void SubTableDesc::putDesc (AipsIO& ios) const
{
    ios << (uInt)1;                  // class version 1
    ios << tabDescTyp_p;
    ios << byName_p;
    if (!byName_p) {
	tabDescPtr_p->putFile (ios, TableAttr());
    }
}

void SubTableDesc::getDesc (AipsIO& ios)
{
    uInt version;
    ios >> version;
    ios >> tabDescTyp_p;
    ios >> byName_p;
    //# If referenced by name, read the table description.
    //# Otherwise get it from the file itself.
    if (allocSelf_p) {
	delete tabDescPtr_p;
    }
    tabDescPtr_p = 0;
    if (byName_p) {
	readTableDesc();
    }else{
	tabDescPtr_p = new TableDesc;
	tabDescPtr_p->getFile (ios, TableAttr());    // get nested table desc.
    }
}


//# Get the table description.
//# Throw exception if not there.
TableDesc* SubTableDesc::tableDesc()
{
    if (tabDescPtr_p == 0) {
	throw (TableNoFile("desc. " + tabDescTyp_p));
    }
    return tabDescPtr_p;
}

//# Reread the table description if referenced by name.
Bool SubTableDesc::readTableDesc()
{
    Bool success = True;
    if (byName_p) {
	if (allocSelf_p) {
	    delete tabDescPtr_p;
	}
	tabDescPtr_p = 0;
	if (TableDesc::isReadable (tabDescTyp_p)) {
	    tabDescPtr_p = new TableDesc(tabDescTyp_p);
	}else{
	    success = False;
	}
    }
    return success;
}


//# Once the column is added, a deep copy has to be made.
void SubTableDesc::handleAdd (ColumnDescSet&)
    { shallowCopy_p = False; }


//# Show the column.
void SubTableDesc::show (ostream& os) const
{
    os << "   Name=" << name();
    os << "  Subtable type=" << tabDescTyp_p;
    if (byName_p) {
	os << "  (by name)";
    }else{
	if (!allocSelf_p) {
	    os << "  (directly)";
	}
    }
    os << endl;
    os << "   Comment = " << comment() << endl;
}


PlainColumn* SubTableDesc::makeColumn (ColumnSet*) const
    { return 0; }

} //# NAMESPACE CASACORE - END

