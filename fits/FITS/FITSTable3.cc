//# FITSTable.h: Simplified interface to FITS tables with Casacore Look and Feel.
//# Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002,2003
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

#include <casacore/fits/FITS/FITSTable.h>
#include <casacore/fits/FITS/FITSFieldCopier.h>
#include <casacore/casa/Containers/RecordField.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/fits/FITS/fits.h>
#include <casacore/fits/FITS/fitsio.h>
#include <casacore/fits/FITS/hdu.h>
#include <casacore/fits/FITS/FITSKeywordUtil.h>
#include <casacore/casa/OS/Path.h>

#include <casacore/casa/Logging/LogIO.h>
#include <casacore/casa/Arrays/Array.h>

#include <casacore/casa/sstream.h>
#include <casacore/casa/iomanip.h>


namespace casacore { //# NAMESPACE CASACORE - BEGIN

FITSGroupWriter::FITSGroupWriter(const String &fileName,
				 const RecordDesc &description,
				 uInt nrows,
				 const Record &extraKeywords,
				 Bool freeOutput)
    : delete_writer_p(freeOutput), writer_p(0), nrows_written_p(0),
      nrows_total_p(nrows),  group_p(0), error_count_p(0)
{
    LogIO log(LogOrigin("FITSGroupWriter", "FITSGroupWriter", WHERE));

    // Verify the description before doing anything else
    const uInt nfields = description.nfields();

    Int arrayField = -1;
    uInt i;
    for (i=0; i<nfields; i++) {
	if (description.type(i) == TpArrayFloat ) {
	    arrayField = i;
	    if (description.shape(i).product() < 0) {
	    throw(AipsError("FitsGroupWriter::FitsGroupWriter - "
			    "Data array must be constant shape"));
	    }
	} else if (description.type(i) != TpFloat) {
	    throw(AipsError("FitsGroupWriter::FitsGroupWriter - all random"
			    " group columns must be float"));
	}
    }
    if (arrayField < 0) {
	throw(AipsError("FitsGroupWriter::FitsGroupWriter - no array found!"));
    }

    // The description is ok!
    row_p.restructure(description);
    
    // See if we can open the output file
    // use Path so that environment variables and any tilde in fileName are parse
    writer_p = new FitsOutput(Path(fileName).expandedName().chars(), FITS::Disk);
    AlwaysAssert(writer_p, AipsError);
    check_error("creating file");

    FitsKeywordList kw;

    // SIMPLE
    kw.mk(FITS::SIMPLE,True,"Standard FITS format");

    // BITPIX
    kw.mk(FITS::BITPIX,-32,"Floating point values");

    // NAXIS
    IPosition shape = description.shape(arrayField);
    kw.mk(FITS::NAXIS,Int(shape.nelements()+1));

    // NAXIS1
    kw.mk(1, FITS::NAXIS, 0, "Random groups, NOT image");

    // NAXIS*
    for (i=0; i<shape.nelements(); i++) {
	kw.mk(i+2, FITS::NAXIS, shape(i));
    }

    // EXTEND
    kw.mk(FITS::EXTEND, True, "Tables may follow");

    // BLOCKED
    kw.mk(FITS::BLOCKED, True, "File may be blocked");

    // GROUPS
    kw.mk(FITS::GROUPS, True, "Random Group UV data");

    // PCOUNT (-1 because the array takes one slot)
    kw.mk(FITS::PCOUNT, Int(nfields-1), "Number of random parameters");

    // GCOUNT
    kw.mk(FITS::GCOUNT, Int(nrows), "Number of groups (rows) in the file");

    kw.spaces();

    if (!FITSKeywordUtil::addKeywords(kw, extraKeywords)) {
	log << "Error adding extra keywords to FITS header" << LogIO::EXCEPTION;
    }

    kw.spaces();

    // ORIGIN
    ostringstream buffer;
    buffer << setfill('0') << "casacore ";
    String version(buffer);
    kw.mk(FITS::ORIGIN, version.chars());

    kw.spaces();

    // END
    kw.end();

    // Rquires a bit more development if this is ever false
    AlwaysAssert(sizeof(Float) == 4, AipsError);
    group_p = new PrimaryGroup<Float>(kw);

    AlwaysAssert(group_p, AipsError);
    check_error("creating random groups from keywords");

    group_p->write_hdr(*writer_p);
    check_error("writing header");
}

FITSGroupWriter::~FITSGroupWriter()
{
    check_error("closing file");

    if (nrows_written_p < nrows_total_p) {
	LogIO log(LogOrigin("FITSGroupWriter", "~FITSGroupWriter", WHERE));
	log << LogIO::SEVERE <<
	    nrows_total_p << " rows must be written, only " << 
	    nrows_written_p << " have been." << endl <<
	    "Not enough rows were written, repeating the final row" << 
	    LogIO::POST;
	for (uInt i=nrows_written_p; i<nrows_total_p; i++) {
	    write();
	}
    }

    if (delete_writer_p) {
	delete writer_p;
    }
    writer_p = 0;
    delete group_p;
    group_p = 0;
}

void FITSGroupWriter::write()
{
    static Array<Float> tmp;

    if (nrows_written_p >= nrows_total_p) {
	LogIO log(LogOrigin("FITSGroupWriter", "write", WHERE));
	log << LogIO::SEVERE << "You've already written all the rows!!" << 
	    LogIO::POST;
      return;
    }

    uInt nfields = row_p.nfields();
    // This could be sped up by keeping external copies of the field pointers
    Int param = 0;
    for (uInt i=0; i<nfields; i++) {
	if (row_p.type(i) == TpArrayFloat) {
	    // The data array
	    row_p.get(i, tmp);
	    Bool deleteIt;
	    Float *ptr = tmp.getStorage(deleteIt);

	    // It looks to me like store is doing the wrong thing for primary groups
	    group_p->store(ptr);
	    check_error("setting group array");
	    tmp.putStorage(ptr, deleteIt);
	} else {
	    // A random "parameter"
	    Float tmp2;
	    row_p.get(i, tmp2);
	    group_p->rawparm(param) = tmp2;
	    check_error("setting group parameter");
	    param++;
	}
    }

    group_p->write(*writer_p);
    check_error("error writing row");
    nrows_written_p++;
}

void FITSGroupWriter::check_error(const char *extra_info)
{
    static LogOrigin OR("FITSGroupWriter", "");
    static LogMessage msg(OR, LogMessage::SEVERE);

    if (group_p) {
	HeaderDataUnit::HDUErrs err = HeaderDataUnit::HDUErrs(group_p->err());
	if (err != HeaderDataUnit::OK) {
	    ostringstream os;
	    os << "Random Groups error at row " << nrows_written_p << " ";
	    switch (err) {
	    case HeaderDataUnit::NOMEM: os << "(NOMEM)"; break;
	    case HeaderDataUnit::MISSKEY: os << "(MISSKEY)"; break;
	    case HeaderDataUnit::BADBITPIX: os << "(BADBITPIX)"; break;
	    case HeaderDataUnit::NOAXISN: os << "(NOAXISN)"; break;
	    case HeaderDataUnit::NOPCOUNT: os << "(NOPCOUNT)"; break;
	    case HeaderDataUnit::NOGCOUNT: os << "(NOGCOUNT)"; break;
	    case HeaderDataUnit::BADPCOUNT: os << "(BADPCOUNT)"; break;
	    case HeaderDataUnit::BADGCOUNT: os << "(BADGCOUNT)"; break;
	    case HeaderDataUnit::NOGROUPS: os << "(NOGROUPS)"; break;
	    case HeaderDataUnit::BADNAXIS: os << "(BADNAXIS)"; break;
	    case HeaderDataUnit::BADREC: os << "(BADREC)"; break;
	    case HeaderDataUnit::BADTYPE: os << "(BADTYPE)"; break;
	    case HeaderDataUnit::BADRULES: os << "(BADRULES)"; break;
	    case HeaderDataUnit::BADSIZE: os << "(BADSIZE)"; break;
	    case HeaderDataUnit::BADOPER: os << "(BADOPER)"; break;
	    case HeaderDataUnit::BADCONV: os << "(BADCONV)"; break;
	    case HeaderDataUnit::BADIO: os << "(BADIO)"; break;
	    default: os << "(unknown error)";
	    }
	    if (extra_info) {
		os << ". Error occured while " << extra_info << ".\n";
	    }
	    msg.message(os);
	    LogSink::postGlobally(msg);
	    error_count_p++;
	}
    }

    if (writer_p) {
	FitsIO::FitsErrs err = (FitsIO::FitsErrs(writer_p->err()));
	if (err != FitsIO::OK) {
	    ostringstream os;
	    os << "I/O error at row " << nrows_written_p << " ";
	    switch(err) {
	    case FitsIO::IOERR: os << "(IOERR)"; break;
	    case FitsIO::MISSKEY: os << "(MISSKEY)"; break;
	    case FitsIO::BADBEGIN: os << "(BADBEGIN)"; break;
	    case FitsIO::EMPTYFILE: os << "(EMPTYFILE)"; break;
	    case FitsIO::NOPRIMARY: os << "(NOPRIMARY)"; break;
	    case FitsIO::BADOPER: os << "(BADOPER)"; break;
	    case FitsIO::MEMERR: os << "(MEMERR)"; break;
	    case FitsIO::BADBITPIX: os << "(BADBITPIX)"; break;
	    case FitsIO::NOAXISN: os << "(NOAXISN)"; break;
	    case FitsIO::NOPCOUNT: os << "(NOPCOUNT)"; break;
	    case FitsIO::NOGCOUNT: os << "(NOGCOUNT)"; break;
	    case FitsIO::BADPCOUNT: os << "(BADPCOUNT)"; break;
	    case FitsIO::BADGCOUNT: os << "(BADGCOUNT)"; break;
	    case FitsIO::NOGROUPS: os << "(NOGROUPS)"; break;
	    case FitsIO::BADNAXIS: os << "(BADNAXIS)"; break;
	    case FitsIO::BADPRIMARY: os << "(BADPRIMARY)"; break;
	    case FitsIO::BADSIZE: os << "(BADSIZE)"; break;
	    case FitsIO::HDUERR: os << "(HDUERR)"; break;
	    default: os << "(unknown error)";
	    }
	    msg.message(os);
	    LogSink::postGlobally(msg);
	    error_count_p++;
	}
    }

    if (error_count_p > 5) {
	msg.message("More than 5 errors encountered! Throwing an exception");
	LogSink::postGloballyThenThrow(msg);
    }
}

} //# NAMESPACE CASACORE - END

