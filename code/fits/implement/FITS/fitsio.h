//# fitsio.h:
//# Copyright (C) 1993,1994,1995,1996,1999,2001,2003
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

# if !defined(AIPS_FITS_IO_H)
# define AIPS_FITS_IO_H

# include <aips/FITS/fits.h>
# include <aips/FITS/blockio.h>
# include <aips/FITS/hdu.h>

//<category lib=aips module=FITS sect="FITS I/O">   
//<summary> sequential FITS I/O </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//<synopsis>
// FitsIO is a base class that handles all the sequential blocked
// FITS I/O. Special derived classes do the input and output.
// No interpretation of the data is attempted here, there are 
// special FITS classes that handle syntax and interpretation.
//</synopsis>
//<linkfrom anchor=FitsIO classes="BlockIO">
// <here> FitsIO </here> for some BlockIO derived classes.
//</linkfrom>
//<example>
//<srcblock>
// FitsInput fin("myfile.fits",FITS::Disk);	// open disk file for FITS input
// if (fin.err() == FitsIO::IOERR) {            // check if open succeeded
//     cout << "Could not open FITS input\n";
//     exit(0);
// }
// if (fin.rectype() == FITS::HDURecord &&      // test for primary array
//     fin.hdutype() == FITS::PrimaryArrayHDU) {
// }
//</srcblock>
//</example>

class FitsIO {
    public:
	virtual ~FitsIO();

	// error return code. Should be one of an 
	// enumerated type:
	//
	//#  until cxx2html can handle enum() we duplicate it here
	//
	//<srcblock>
	//  enum FitsErrs { OK, IOERR, MISSKEY, BADBEGIN, EMPTYFILE,
	//	NOPRIMARY, BADOPER, BADEOF, MEMERR, BADBITPIX, NOAXISN,
	//	NOPCOUNT, NOGCOUNT, BADPCOUNT, BADGCOUNT, NOGROUPS,
	//	BADNAXIS, BADPRIMARY, BADSIZE, HDUERR };
	//</srcblock>
	//<group>
	enum FitsErrs { OK, IOERR, MISSKEY, BADBEGIN, EMPTYFILE,
		NOPRIMARY, BADOPER, BADEOF, MEMERR, BADBITPIX, NOAXISN,
		NOPCOUNT, NOGCOUNT, BADPCOUNT, BADGCOUNT, NOGROUPS,
		BADNAXIS, BADPRIMARY, BADSIZE, HDUERR };
	int err() const 			{ return err_status; }
	//</group>
	//
	// record size, in bytes, of a FITS block. 
	// Normally set at 2880, unless some form of blocking was used.
	int fitsrecsize() const 		{ return FitsRecSize; }
	// is it a valid fits file (SIMPLE==T). If not, the only
	// safest operation is to skip the data portion of the
	// current HeaderDataUnit
	Bool isafits() const 			{ return valid_fits; }
	// see if there may be FITS extensions present (EXTENT==T)
	Bool isextend() const 			{ return extend; }
	// test if end of file has been reached
	Bool eof() const 	 { return Bool(rec_type == FITS::EndOfFile); }
	// the FITS record type
	FITS::FitsRecType rectype() const 	{ return rec_type;  }
	// Header Data Unit type (e.g. 
	FITS::HDUType hdutype() const 		{ return hdu_type; }
	FITS::ValueType datatype() const 	{ return data_type; }
	// return the datasize of the current HDU. This excludes
	// the trailing end of the blocked data portion.
	uInt datasize() const 			{ return data_size; }
	// data characteristics
	Int itemsize() const 			{ return item_size; }
	// for input, size of remaining data
	// for output, size of data written
	uInt currsize() const 			{ return curr_size; }

    protected:
	FitsIO(FITSErrorHandler);

	const int FitsRecSize;
	Bool valid_fits;		// True if SIMPLE == T
	Bool extend;			// True if EXTEND == T
	Bool isaprimary;		// True if there is a primary HDU
        Bool header_done;		// True if header has been processed
	FITS::FitsRecType rec_type; 	// always set
	FITS::HDUType hdu_type;		// always set

	FITSErrorHandler errfn;	        // error handler function
	FitsErrs err_status;
	FitsKeyCardTranslator kc;
	FitsKeywordList kw;

	char *curr;			// pointer to current record
	int bytepos;			// current byte position within record
	Int item_size;			// data characteristics
	FITS::ValueType data_type;
	uInt data_size;		
	// for input, size of remaining data
	// for output, size of data written
	uInt curr_size;			

	// set error message that belongs to one of the enumerated types
	virtual void errmsg(FitsErrs, char *) = 0;
					
};

//<summary> fixed-length sequential blocked FITS input </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//<linkfrom anchor=FitsInput classes="BlockInput">
// FitsInput
//</linkfrom>

class FitsInput : public FitsIO {
	friend int HeaderDataUnit::get_hdr(FITS::HDUType, FitsKeywordList &);
	friend int HeaderDataUnit::read_all_data(char *);
	friend int HeaderDataUnit::read_data(char *, int);
	friend int HeaderDataUnit::skip(int);
	friend int HeaderDataUnit::skip();

    public:
	//<group>
	FitsInput(const char *, const FITS::FitsDevice &, int = 10, 
		  FITSErrorHandler errhandler = FITSError::defaultHandler);
	FitsInput(FITSErrorHandler errhandler = FITSError::defaultHandler); 
	~FitsInput();
	//</group>

	int skip_hdu();
	// read special or unrecognizable records
	char *read_sp();

        //  number of physical blocks read/written
        int blockno() const {return fin.blockno();}

        //  number of logical records read/written
        int recno() const {return fin.recno();}

    private:
	BlockInput &fin;
	BlockInput &make_input(const char *, const FITS::FitsDevice &, int, 
			       FITSErrorHandler errhandler = FITSError::defaultHandler);

	// flag used for read control in errors
	Bool got_rec;		

	virtual void errmsg(FitsErrs, char *);
	void init();
	void read_header_rec();

	//# check if this comes out ok in cxx2html
        // Special interface to class HeaderDataUnit
	//<group>
        // special way to process header
	int process_header(FITS::HDUType, FitsKeywordList &);
	// read all data into a given address - all responsibility is given
	// to the user
	uInt read_all(FITS::HDUType, char *);
	// read N bytes into address
	int read(FITS::HDUType, char *, int);
	// skip N bytes
	int skip(FITS::HDUType, int);
	// skip all remaining data
	void skip_all(FITS::HDUType);
        //</group>
};

//<summary> fixed-length sequential blocked FITS output </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

class FitsOutput : public FitsIO {
	friend int HeaderDataUnit::write_hdr(FitsOutput &);
	friend int HeaderDataUnit::write_all_data(FitsOutput &, char *);
	friend int HeaderDataUnit::write_data(FitsOutput &, char *, int);

    public:
	//<group>
	FitsOutput(const char *, const FITS::FitsDevice &, int = 10, 
		   FITSErrorHandler errhandler = FITSError::defaultHandler);
	FitsOutput(FITSErrorHandler errhandler = FITSError::defaultHandler);
	~FitsOutput();
	//</group>

	// write a special record. For this the record type must also
	// be to set to FITS::SpecialRecord
	int write_sp(char *rec);

    private:
	BlockOutput &fout;
	BlockOutput &make_output(const char *, const FITS::FitsDevice &, int, 
				 FITSErrorHandler errhandler = FITSError::defaultHandler);

	virtual void errmsg(FitsErrs, char *);

	int hdu_complete() { 
	    return (rec_type == FITS::HDURecord && data_size == 0); }
	int hdu_inprogress() { return (rec_type == FITS::HDURecord && data_size > 0 
		                       && curr_size < data_size); }

	// Special interface to class HeaderDataUnit
	//<group>
	int write_hdr(FitsKeywordList &, FITS::HDUType, FITS::ValueType, Int, Int);
	// write all data from address
	int write_all(FITS::HDUType, char *, char);
	// write N bytes from address
	int write(FITS::HDUType, char *, int, char); 
	//</group>
};

//<summary> FITS input from disk </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

class FitsDiskInput : public BlockInput {
    public:
	FitsDiskInput(const char *, int, int = 1, 
		      FITSErrorHandler errhandler = FITSError::defaultHandler);
	~FitsDiskInput();
        // implements skip in terms of lseek
	char *skip(int); 
};

//<summary> FITS output to disk </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

class FitsDiskOutput : public BlockOutput {
    public:
	FitsDiskOutput(const char *, int, int = 1, 
		       FITSErrorHandler errhandler = FITSError::defaultHandler);
	~FitsDiskOutput();
};

//<summary> FITS input from standard input </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

class FitsStdInput : public BlockInput {
    public:
	FitsStdInput(int, 
		     FITSErrorHandler errhandler = FITSError::defaultHandler);
	~FitsStdInput();
};

//<summary> FITS output to standard output </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

class FitsStdOutput : public BlockOutput {
    public:
	FitsStdOutput(int, 
		      FITSErrorHandler errhandler = FITSError::defaultHandler);
	~FitsStdOutput();
};

//<summary> FITS input from 9-track tape </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

class FitsTape9Input : public BlockInput {
    public:
	FitsTape9Input(const char *, int, int = 10, 
		       FITSErrorHandler errhandler = FITSError::defaultHandler);
	~FitsTape9Input();
};

//<summary> FITS output to 9-track tape </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

class FitsTape9Output : public BlockOutput {
    public:
	FitsTape9Output(const char *, int, int = 10, 
			FITSErrorHandler errhandler = FITSError::defaultHandler);
	~FitsTape9Output();
};

# endif
