//# RecordField2Writer.cc: non-templated implementation of RecordFieldWriter.h
//# Copyright (C) 1996,2001
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
//#
//# $Id$

#include <casacore/casa/Containers/RecordFieldWriter.h>
#include <casacore/casa/Utilities/Assert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

RecordFieldWriter::~RecordFieldWriter()
{
    // Nothing
}

void MultiRecordFieldWriter::addWriter(RecordFieldWriter *fromNew)
{
    AlwaysAssert(fromNew, AipsError);
    uInt which = writers_p.nelements();
    writers_p.resize(which + 1);
    writers_p[which] = fromNew;
}

void MultiRecordFieldWriter::copy()
{
    uInt n = writers_p.nelements();
    for (uInt i=0; i<n; i++) {
        writers_p[i]->writeField();
    }
}

MultiRecordFieldWriter::~MultiRecordFieldWriter()
{
    for (uInt i=0; i<writers_p.nelements(); i++) {
        delete writers_p[i];
    }
    writers_p.resize(0);
}

} //# NAMESPACE CASACORE - END

