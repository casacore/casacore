//# RowCopier.cc:  RowCopier copies part or all of a row from one table to another.
//# Copyright (C) 1995,1996,1999
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

#include <tables/Tables/RowCopier.h>
#include <tables/Tables/TableColumn.h>
#include <tables/Tables/TableDesc.h>
#include <tables/Tables/TableError.h>
#include <tables/Tables/Table.h>

#include <casa/Arrays/IPosition.h>
#include <casa/BasicSL/String.h>
#include <casa/Arrays/Vector.h>


namespace casa { //# NAMESPACE CASA - BEGIN

// this class is used internally by RowCopier and is what really does the work.

class ColumnHolder {
public:
    ColumnHolder(Table &inTab, const Table &outTab);
    ~ColumnHolder();
    void attach(const String &outCol, const String &inCol);
    Bool copy(uInt toRow, uInt fromRow);
private:
    //# The following constructors and operator don't seem to be useful
    ColumnHolder();
    ColumnHolder(const ColumnHolder &other);
    ColumnHolder &operator=(const ColumnHolder &other);

    // The tables involved in the copying
    Table in;
    Table out;

    // Blocks of pointers to the TableColumns that will be involved in copying
    PtrBlock<ROTableColumn *> inTabCol;
    PtrBlock<TableColumn *> outTabCol;

};


ColumnHolder::ColumnHolder(Table &outTab, const Table &inTab)
: in(inTab),
  out(outTab)
{}

ColumnHolder::~ColumnHolder()
{
    for (uInt colNum=0; colNum < inTabCol.nelements(); colNum++) {
	delete inTabCol[colNum];   delete outTabCol[colNum];
	inTabCol[colNum] = 0;      outTabCol[colNum] = 0;
    }
}

void ColumnHolder::attach(const String &outCol, const String &inCol)
{
    if (!out.tableDesc().isColumn(outCol) || !in.tableDesc().isColumn(inCol)) {
	throw(TableError("RowCopier: " + inCol + " or " + outCol + 
			 " is not a column"));
    }

    // out isWritable() but individual columns may not be writable
    // for now, we throw an exception of outCol is not writable
    if (! out.isColumnWritable(outCol)) {
	throw(TableError("RowCopier: output table must be writable"));
    }

    // TBF The block resizes are a quadratic behaviour; we should make them
    // larger in powers of (say) two instead.

    if (out.tableDesc()[outCol].isScalar() ==
           in.tableDesc()[inCol].isScalar() && 
	out.tableDesc()[outCol].dataType() ==
        in.tableDesc()[inCol].dataType()) {

	  inTabCol.resize(inTabCol.nelements() + 1);
  	  outTabCol.resize(outTabCol.nelements() + 1);
	  inTabCol[inTabCol.nelements() - 1] = new ROTableColumn(in,inCol);
	  outTabCol[outTabCol.nelements() - 1] = new TableColumn(out,outCol);
    } else {
	throw(TableError("RowCopier: " + inCol + " and " +
			 outCol + " are not conformant"));
    }
}

Bool ColumnHolder::copy(uInt toRow, uInt fromRow)
{
    uInt i;
    if (fromRow >= in.nrow() || toRow >= out.nrow()) {
	return False;
    }

    // loop over all columns
    for (i=0; i < inTabCol.nelements(); i++) {
	outTabCol[i]->put(toRow, (*inTabCol[i]), (fromRow));
    }
    return True;
}

RowCopier::RowCopier(Table &out, const Table &in)
: columns_p(0)
{
    if (! out.isWritable()) {
	throw(TableError("RowCopier: output table must be writable"));
    }

    columns_p = new ColumnHolder(out,in);
    for (uInt i=0; i < out.tableDesc().ncolumn(); i++) {
	TableColumn outCol(out, i);
	if (in.tableDesc().isColumn(outCol.columnDesc().name())) {
	    ROTableColumn inCol(in, outCol.columnDesc().name());
	    columns_p->attach(outCol.columnDesc().name(),
			      inCol.columnDesc().name());
	}
    }
}

RowCopier::RowCopier(Table &out, const Table &in,
		     const Vector<String>& outNames,
		     const Vector<String>& inNames)
: columns_p(0)
{
    if (! out.isWritable()) {
	throw(TableError("RowCopier: output table must be writable"));
    }

    columns_p = new ColumnHolder(out,in);

    if (inNames.nelements() != outNames.nelements()) {
	throw(TableError("RowCopier: Non-conformant column name vectors"));
    }
    for (uInt i=0; i<inNames.nelements(); i++) {
	columns_p->attach(outNames(i), inNames(i));
    }
}

Bool RowCopier::copy(uInt toRow, uInt fromRow)
{
    return columns_p->copy(toRow, fromRow);
}

RowCopier::~RowCopier()
{
    if (columns_p) {
	delete columns_p;
	columns_p = 0;
    }
}

} //# NAMESPACE CASA - END

