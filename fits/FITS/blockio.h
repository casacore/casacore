//# blockio.h:
//# Copyright (C) 1993,1994,1995,1996,1999
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
#ifndef FITS_BLOCKIO_H
#define FITS_BLOCKIO_H

//# Include this file first, because it may set LFS variables used by cfitsio.
#include <casacore/casa/aips.h>

//# Make sure that cfitsio does not declare the wcs headers.
extern "C"{
#include <fitsio.h>  //# header file from cfitsio
#include <fitsio2.h> //# using core functions of cfitsio
}

#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>

#include <casacore/fits/FITS/FITSError.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//----------------------------------------------------------------------------
//<category lib=aips module=FITS sect="Blocked I/O">   
//<summary> fixed-length blocked sequentual I/O base class </summary> 
//<synopsis>
// BlockIO is a low level base class that implements fixed-length 
// blocked sequential I/O. Its derived classes, BlockInput and BlockOutput
// are used by the FitsInput and FitsOutput classes. Users will hardly ever
// need to use this class directly.
//</synopsis>
//<todo>
// <li> ifdef kludges until OS dependent flags are developed
//       for the compilation system.
//</todo>

class BlockIO {
    public:
	// error return code
	enum IOErrs { OK, NOSUCHFILE, NOMEM, OPENERR, CLOSEERR, 
		READERR, WRITEERR };
	int err() const { return (int)m_err_status; }

	//  number of physical blocks read/written
	int blockno() const { return m_block_no; }
	
	// reset the m_iosize data member
	void reset_iosize() { m_iosize = 0; }
	
	// get the total bytes of data in m_buffer
	int iosize() const { return m_iosize; }
	
	// get the current read position within m_buffer
	int current() const { return m_current; }
	
	// get m_buffer
	char* buffer() const { return m_buffer; }

	//  number of logical records read/written
	int recno() const { return m_rec_no; }

	// name of file associated with I/O stream, if applicable
	const char *fname() const { return m_filename; }

	// fits_close_file() does not work for reasons that the file pointer does not have the 
	// knowledge of chdu which were written with write_hdr() not write_***_hdr(). So create
	// our own close_file() method.
        int close_file( fitsfile *fptr, int *status);
	// file descriptor associated with I/O stream, if applicable
	int fdes() const { return m_fd; }
	// get the fitsfile pointer
	fitsfile *getfptr() const { return m_fptr; } 
	void setfptr( fitsfile* ffp );
    protected:
	// Construction can be done either from a filename with open options 
	// or from a file descriptor.
	//
	// The remaining arguments are the the logical record size and number
	// of records that make up a physical record followed by the 
	// output stream that is used to write error messages to.
	//<group>
	BlockIO(const char *, int, int, int = 1, 
		FITSErrorHandler errhandler = FITSError::defaultHandler);
	BlockIO(int,               int, int = 1, 
		FITSErrorHandler errhandler = FITSError::defaultHandler);
	virtual ~BlockIO();
	//</group>

	char *m_filename;		     // name of file
	int m_options;		        // options on open statement
	const int m_recsize;	     // size in bytes of a logical record
	const int m_nrec;		     // maximum number of logical records
	const int m_blocksize;	  // size in bytes of physical records
   FITSErrorHandler m_errfn; // FITS error handler function
	IOErrs m_err_status;	// error number
	int m_fd;			   // file descriptor
	char *m_buffer;		// the actual data buffer itself
	int m_block_no;		// number of physical blocks read/written
	int m_rec_no;		   // number of logical records read/written
	int m_current; 		// offset to current logical record
	// size of record in buffer
	int m_iosize;
	// using fitsfile structure from cfitsio of NASA
	fitsfile *m_fptr;  		

	// set the error message and error number for later recovery
	void errmsg(IOErrs, const char *);
};

//<summary> fixed-length blocked sequential input base class</summary>
//<prerequisite>
//   <li> BlockIO
//</prerequisite>

class BlockInput : public BlockIO {
    public:
	// Construction can be done either from a filename or from
	// a file descriptor.
	//
	// The remaining arguments are the the logical record size and number
	// of records that make up a physical record followed by the 
	// output stream that is used to write error messages to.
	//<group>
	BlockInput(const char *, int, int = 1, 
		   FITSErrorHandler errhandler = FITSError::defaultHandler);
	BlockInput(int,          int, int = 1, 
		   FITSErrorHandler errhandler = FITSError::defaultHandler);
	virtual ~BlockInput();
	//</group>

	// read the next logical record or first
	// skip N logical records and then read the next one.
	// (note it is not possible to skip a record without
	// reading a record). 
	//<note role=caution> these functions return a pointer to an
	// internal record. The user must make sure that
	// after destruction of this class no dangling pointers
	// are left.
	//</note>
	//<group>
	virtual char *read();       // read a physical block.  
	virtual char *skip(int);    
	//</group>
};

//<summary> fixed-length blocked sequential output base class</summary>
//<prerequisite>
//   <li> BlockIO
//</prerequisite>             

class BlockOutput : public BlockIO {
    public:
	// Construction can be done either from a filename or from
	// a file descriptor.
	//
	// The remaining arguments are the the logical record size and number
	// of records that make up a physical record followed by the 
	// output stream that is used to write error messages to.
	//<group>
	BlockOutput(const char *, int, int = 1,
		    FITSErrorHandler errhandler = FITSError::defaultHandler);
	BlockOutput(int,          int, int = 1,
		    FITSErrorHandler errhandler = FITSError::defaultHandler);
	virtual ~BlockOutput();
        void flush_buffer();
	//</group>

	// write the next logical record. The input must point
	// to a logical record
	virtual int write(char *);
};

} //# NAMESPACE CASACORE - END

# endif

