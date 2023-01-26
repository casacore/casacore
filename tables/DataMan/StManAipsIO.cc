//# StManAipsIO.cc: Storage manager for tables using AipsIO
//# Copyright (C) 1994,1995,1996,1997,1998,1999,2000,2001,2002,2003
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

#include <casacore/tables/DataMan/StManAipsIO.h>
#include <casacore/tables/DataMan/StArrAipsIO.h>
#include <casacore/tables/DataMan/StIndArrAIO.h>
#include <casacore/tables/DataMan/StArrayFile.h>
#include <casacore/tables/Tables/RefRows.h>
#include <casacore/casa/Arrays/Vector.h>
#include <casacore/casa/BasicSL/Complex.h>
#include <casacore/casa/BasicMath/Math.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Copy.h>
#include <casacore/casa/Utilities/DataType.h>
#include <casacore/casa/Utilities/ValType.h>
#include <casacore/casa/IO/AipsIO.h>
#include <casacore/casa/OS/DOos.h>
#include <casacore/tables/DataMan/DataManError.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

#define EXTBLSZ 32

StManColumnAipsIO::StManColumnAipsIO (StManAipsIO* smptr,
				      int dataType, bool byPtr)
  : MSMColumn(smptr, dataType, byPtr)
{}

StManColumnAipsIO::~StManColumnAipsIO()
{}

void StManColumnAipsIO::initData (void*, rownr_t)
{}

//# Write all data into AipsIO.
void StManColumnAipsIO::putFile (rownr_t nrval, AipsIO& ios)
{
    ios.putstart ("StManColumnAipsIO", 2);     // class version 2
    ios << uint32_t(nrval);
    uint32_t nr;
    for (uint32_t i=1; i<=nrext_p; i++) {
	nr = ncum_p[i] - ncum_p[i-1];
	if (nr > nrval) {
	    nr = nrval;
	}
	if (nr > 0) {
            ios << nr;
	    putData (data_p[i], nr, ios);
	    nrval -= nr;
	}
    }
    ios.putend();
}
	
void StManColumnAipsIO::putData (void* dp, uint32_t nrval, AipsIO& ios)
{
    switch (dtype()) {
    case TpBool:
	ios.put (nrval, (bool*)dp);
	break;
    case TpUChar:
	ios.put (nrval, (unsigned char*)dp);
	break;
    case TpShort:
	ios.put (nrval, (int16_t*)dp);
	break;
    case TpUShort:
	ios.put (nrval, (uint16_t*)dp);
	break;
    case TpInt:
	ios.put (nrval, (int32_t*)dp);
	break;
    case TpUInt:
	ios.put (nrval, (uint32_t*)dp);
	break;
    case TpInt64:
	ios.put (nrval, (int64_t*)dp);
	break;
    case TpFloat:
	ios.put (nrval, (float*)dp);
	break;
    case TpDouble:
	ios.put (nrval, (double*)dp);
	break;
    case TpComplex:
	ios.put (nrval, (Complex*)dp);
	break;
    case TpDComplex:
	ios.put (nrval, (DComplex*)dp);
	break;
    case TpString:
	ios.put (nrval, (String*)dp);
	break;
    default:
      throw DataManInvDT("StManAipsIO::putData");
    }
}


//# Read all data from AipsIO.
void StManColumnAipsIO::getFile (rownr_t nrval, AipsIO& ios)
{
    uint32_t version = ios.getstart ("StManColumnAipsIO");
    uint32_t nr;
    //# Get and check nr of values.
    ios >> nr;
    if (nr != nrval) {
	throw (DataManInternalError
	                ("StManColumnAipsIO::getFile: mismatch in #values"));
    }
    deleteAll();
    if (nrval > 0) {
	resize (nrval);
	void* datap = data_p[1];
	uint32_t nrd=0;
	while (nrd < nrval) {
	    ios >> nr;
	    if (nr == 0) {
	        nr = nrval - nrd;
	    }
	    if (nr+nrd > nrval) {
		throw (DataManInternalError ("StManColumnAipsIO::getFile"));
	    }
	    getData (datap, nrd, nr, ios, version);
	    nrd += nr;
	}
    }
    ios.getend();
    columnCache().invalidate();
}


void StManColumnAipsIO::getData (void* datap, uint32_t inx, uint32_t nrval,
				 AipsIO& ios, uint32_t)
{
    uint32_t nr;
    ios >> nr;
    switch (dtype()) {
    case TpBool:
	ios.get (nrval, (bool*)datap + inx);
	break;
    case TpUChar:
	ios.get (nrval, (unsigned char*)datap + inx);
	break;
    case TpShort:
	ios.get (nrval, (int16_t*)datap + inx);
	break;
    case TpUShort:
	ios.get (nrval, (uint16_t*)datap + inx);
	break;
    case TpInt:
	ios.get (nrval, (int32_t*)datap + inx);
	break;
    case TpUInt:
	ios.get (nrval, (uint32_t*)datap + inx);
	break;
    case TpInt64:
	ios.get (nrval, (int64_t*)datap + inx);
	break;
    case TpFloat:
	ios.get (nrval, (float*)datap + inx);
	break;
    case TpDouble:
	ios.get (nrval, (double*)datap + inx);
	break;
    case TpComplex:
	ios.get (nrval, (Complex*)datap + inx);
	break;
    case TpDComplex:
	ios.get (nrval, (DComplex*)datap + inx);
	break;
    case TpString:
	ios.get (nrval, (String*)datap + inx);
	break;
    default:
      throw DataManInvDT("StManAipsIO::getData");
    }
}




StManAipsIO::StManAipsIO ()
: MSMBase(),
  uniqnr_p    (0),
  iosfile_p   (0)
{}

StManAipsIO::StManAipsIO (const String& storageManagerName)
: MSMBase (storageManagerName),
  uniqnr_p    (0),
  iosfile_p   (0)
{}

StManAipsIO::StManAipsIO (const String& storageManagerName, const Record& rec)
: MSMBase (storageManagerName, rec),
  uniqnr_p    (0),
  iosfile_p   (0)
{}

StManAipsIO::~StManAipsIO()
{
    delete iosfile_p;
}

DataManager* StManAipsIO::clone() const
{
    StManAipsIO* smp = new StManAipsIO (stmanName_p);
    return smp;
}

DataManager* StManAipsIO::makeObject (const String& storageManagerName,
				      const Record& spec)
{
    StManAipsIO* smp = new StManAipsIO (storageManagerName, spec);
    return smp;
}

String StManAipsIO::dataManagerType() const
    { return "StManAipsIO"; }

DataManagerColumn* StManAipsIO::makeScalarColumn (const String& columnName,
						  int dataType, const String&)
{
    //# Check if data type is not TpOther.
    throwDataTypeOther (columnName, dataType);
    //# Extend colSet_p block if needed.
    if (ncolumn() >= colSet_p.nelements()) {
	colSet_p.resize (colSet_p.nelements() + 32);
    }
    StManColumnAipsIO* colp = new StManColumnAipsIO (this, dataType, false);
    colSet_p[ncolumn()] = colp;
    return colp;
}
DataManagerColumn* StManAipsIO::makeDirArrColumn (const String& columnName,
						  int dataType, const String&)
{
    //# Check if data type is not TpOther.
    throwDataTypeOther (columnName, dataType);
    //# Extend colSet_p block if needed.
    if (ncolumn() >= colSet_p.nelements()) {
	colSet_p.resize (colSet_p.nelements() + 32);
    }
    StManColumnAipsIO* colp = new StManColumnArrayAipsIO (this, dataType);
    colSet_p[ncolumn()] = colp;
    return colp;
}
DataManagerColumn* StManAipsIO::makeIndArrColumn (const String& columnName,
						  int dataType, const String&)
{
    //# Check if data type is not TpOther.
    throwDataTypeOther (columnName, dataType);
    //# Extend colSet_p block if needed.
    if (ncolumn() >= colSet_p.nelements()) {
	colSet_p.resize (colSet_p.nelements() + 32);
    }
    StManColumnAipsIO* colp = new StManColumnIndArrayAipsIO (this, dataType);
    colSet_p[ncolumn()] = colp;
    return colp;
}

bool StManAipsIO::flush (AipsIO&, bool)
{
    //# Do not write if nothing has been put.
    if (! hasPut_p) {
	return false;
    }
    uint32_t i;
    AipsIO ios(fileName(), ByteIO::New);
    ios.putstart ("StManAipsIO", 2);           // version 2
    //# Write the number of rows and columns and the column types.
    //# This is only done to check it when reading back.
    //# Note that an AipsIO object cannot exceed 4 GB, so nrrow_p always
    //# fits in 32 bits.
    ios << stmanName_p;                        // this is added in version 2
    ios << sequenceNr();
    ios << uniqnr_p;
    ios << uint32_t(nrrow_p);
    ios << ncolumn();
    for (i=0; i<ncolumn(); i++) {
	ios << colSet_p[i]->dataType();
    }
    for (i=0; i<ncolumn(); i++) {
	colSet_p[i]->putFile (nrrow_p, ios);
    }
    ios.putend();
    hasPut_p = false;
    return true;
}

void StManAipsIO::create64 (rownr_t nrrow)
{
    nrrow_p = nrrow;
    //# Let the column create something if needed.
    for (uint32_t i=0; i<ncolumn(); i++) {
	colSet_p[i]->doCreate (nrrow);
    }
    setHasPut();
}

rownr_t StManAipsIO::open64 (rownr_t tabNrrow, AipsIO&)
{
    return resync64 (tabNrrow);
}
rownr_t StManAipsIO::resync64 (rownr_t nrrow)
{
    if (iosfile_p != 0) {
        iosfile_p->resync();
    }
    AipsIO ios(fileName());
    uint32_t version = ios.getstart ("StManAipsIO");
    //# Get and check the number of rows and columns and the column types.
    uint32_t i, nrr, nrc, snr;
    int  dt;
    if (version > 1) {
	ios >> stmanName_p;
    }
    ios >> snr;
    ios >> uniqnr_p;
    ios >> nrr;
    ios >> nrc;
    if (snr != sequenceNr()  ||  nrc != ncolumn()) {
	throw (DataManInternalError
	                 ("StManAipsIO::open: mismatch in seqnr,#col"));
    }
    if (nrrow != nrr) {
#if defined(TABLEREPAIR)
        cerr << "StManAipsIO::open: mismatch in #row (expected " << nrrow
	     << ", found " << nrr << ")" << endl;
	cerr << "Remainder will be added or discarded" << endl;
	setHasPut();
#else
	throw (DataManInternalError
	                 ("StManAipsIO::open: mismatch in #row; expected " +
			  String::toString(nrrow) + ", found " +
			  String::toString(nrr)));
#endif
    }
    for (i=0; i<ncolumn(); i++) {
	ios >> dt;
	if (dt != colSet_p[i]->dataType()) {
	    throw (DataManInternalError
		         ("StManAipsIO::open: mismatch in data type"));
	}
    }
    //# Now read in all the columns.
    for (i=0; i<ncolumn(); i++) {
	colSet_p[i]->getFile (nrr, ios);
	//# The following can only be executed in case of TABLEREPAIR.
	//# Add rows if storage manager has fewer rows than table.
	//# Remove rows if storage manager has more rows than table.
	if (nrrow > nrr) {
	    colSet_p[i]->addRow (nrrow, nrr);
	} else if (nrrow < nrr) {
	    for (uint32_t r=nrrow; r<nrr; r++) {
	        colSet_p[i]->remove (nrrow);
	    }
	}
    }
    nrrow_p = nrrow;
    ios.getend();
    return nrrow_p;
}


StManArrayFile* StManAipsIO::openArrayFile (ByteIO::OpenOption opt)
{
    if (iosfile_p == 0) {
	iosfile_p = new StManArrayFile (fileName() + 'i', opt);
    }
    return iosfile_p;
}

void StManAipsIO::reopenRW()
{
    for (uint32_t i=0; i<ncolumn(); i++) {
	colSet_p[i]->reopenRW();
    }
}

void StManAipsIO::deleteManager()
{
    delete iosfile_p;
    iosfile_p = 0;
    DOos::remove (fileName() + 'i', false, false);
    DOos::remove (fileName(), false, false);
}

} //# NAMESPACE CASACORE - END

