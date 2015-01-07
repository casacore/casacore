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

#ifndef FITS_FITSIO_H
#define FITS_FITSIO_H

#include <casacore/casa/aips.h>
# include <casacore/fits/FITS/fits.h>
# include <casacore/fits/FITS/blockio.h>
# include <casacore/fits/FITS/hdu.h>
//# include <casacore/casa/stdvector.h>
# include <casacore/casa/Arrays/Vector.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//<summary> sequential FITS I/O </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//<synopsis>
// FitsIO is a base class that handles all the sequential blocked
// FITS I/O. Special derived classes do the input and output.
// No interpretation of the data is attempted here, there are 
// special FITS classes that handle syntax and interpretation.
//</synopsis>
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
	int err() const 			{ return m_err_status; }
	//</group>
	//
	// record size, in bytes, of a FITS block. 
	// Normally set at 2880, unless some form of blocking was used.
	int fitsrecsize() const 		{ return m_recsize; }
	// is it a valid fits file (SIMPLE==T). If not, the only
	// safest operation is to skip the data portion of the
	// current HeaderDataUnit
	Bool isafits() const 			{ return m_valid_fits; }
	// see if there may be FITS extensions present (EXTENT==T)
	Bool isextend() const 			{ return m_extend; }
	// test if end of file has been reached
	Bool eof() const 	 { return Bool(m_rec_type == FITS::EndOfFile); }
	// the FITS record type
	FITS::FitsRecType rectype() const 	{ return m_rec_type;  }
	// Header Data Unit type (e.g. 
	FITS::HDUType hdutype() const 		{ return m_hdu_type; }
	FITS::ValueType datatype() const 	{ return m_data_type; }
	// return the datasize of the current HDU. This excludes
	// the trailing end of the blocked data portion.
	OFF_T datasize() const 			{ return m_data_size; }
	// data characteristics
	Int itemsize() const 			{ return m_item_size; }
	// for input, size of remaining data
	// for output, size of data written
	OFF_T currsize() const 			{ return m_curr_size; }
	// get FitsKeyCardTranslator
	FitsKeyCardTranslator& getkc(){  return m_kc;  }
	// get the fitsfile pointer
	fitsfile *getfptr() const { return m_fptr; }

	// get the size of the last skipped HDU
	OFF_T getskipsize() const {return m_skipHDU_size;}


   protected:
	FitsIO(FITSErrorHandler);

	fitsfile *m_fptr;
	const int m_recsize;
	Bool m_valid_fits;		// True if SIMPLE == T
	Bool m_extend;			   // True if EXTEND == T
	Bool m_isaprimary;		// True if there is a primary HDU
   Bool m_header_done;		// True if header has been processed
	FITS::FitsRecType m_rec_type; 	// always set
	FITS::HDUType m_hdu_type;		// always set

	FITSErrorHandler m_errfn;	        // error handler function
	FitsErrs m_err_status;
	FitsKeyCardTranslator m_kc;
	FitsKeywordList m_kw;

	char *m_curr;			// pointer to current record
	int m_bytepos;			// current byte position within record
	Int m_item_size;			// data characteristics
	FITS::ValueType m_data_type;
	//uInt m_data_size;
	OFF_T m_data_size;		
	// for input, size of remaining data
	// for output, size of data written
	//uInt m_curr_size;
	OFF_T m_curr_size;			

	// for size of the last HDU skipped
	OFF_T m_skipHDU_size;

	// set error message that belongs to one of the enumerated types
	virtual void errmsg(FitsErrs, const char *) = 0;
					
};

//<summary> fixed-length sequential blocked FITS input </summary>

class FitsInput : public FitsIO {
	friend int HeaderDataUnit::get_hdr(FITS::HDUType, FitsKeywordList &);
	friend OFF_T HeaderDataUnit::read_all_data(char *);
	friend int HeaderDataUnit::read_data(char *, Int);
	friend int HeaderDataUnit::skip(uInt);
	friend int HeaderDataUnit::skip();

    public:
	//<group>
	FitsInput(const char *, const FITS::FitsDevice &, int = 10, 
		  FITSErrorHandler errhandler = FITSError::defaultHandler);
	FitsInput(FITSErrorHandler errhandler = FITSError::defaultHandler); 
	~FitsInput();
	//</group>

	int skip_hdu();

	// skip all remaining data
	void skip_all(FITS::HDUType);

	//int skip_hdu2();
	// read special or unrecognizable records
	char *read_sp();

   // get hdu header image cards as strings. By default the strings will be of
   // variable length. You can optionally ask for them to be length 80 (padded
   // with spaces).
	Vector<String> kwlist_str(Bool length80=False);

   //  number of physical blocks read/written
   int blockno() const {return m_fin.blockno();}

   //  number of logical records read/written
   int recno() const {return m_fin.recno();}
   BlockInput & getfin(){ return m_fin; } // for test use only

   // the number of hdu in this fits file
   int getnumhdu() const {return m_thdunum;}

    private:
	BlockInput &m_fin;
	BlockInput &make_input(const char *, const FITS::FitsDevice &, int, 
			       FITSErrorHandler errhandler = FITSError::defaultHandler);

	// flag used for read control in errors
	Bool m_got_rec;
	// total number of hdu in this fits file
	int m_thdunum;		

	virtual void errmsg(FitsErrs, const char *);
	void init();
	void read_header_rec();
	bool current_hdu_type( FITS::HDUType &);
	bool get_data_type( FITS::ValueType &);

	//# check if this comes out ok in cxx2html
        // Special interface to class HeaderDataUnit
	//<group>
        // special way to process header
	int process_header(FITS::HDUType, FitsKeywordList &);
	// read all data into a given address - all responsibility is given
	// to the user
	OFF_T read_all(FITS::HDUType, char *);
	// read N bytes into address
	int read(FITS::HDUType, char *, int );
	// skip N bytes
	int skip(FITS::HDUType, OFF_T);
        //</group>
};

//<summary> fixed-length sequential blocked FITS output </summary>

class FitsOutput : public FitsIO {
	friend int HeaderDataUnit::write_hdr(FitsOutput &);
	friend int HeaderDataUnit::write_all_data(FitsOutput &, char *);
	friend int HeaderDataUnit::write_data(FitsOutput &, char *, Int);

    public:
	//<group>
	FitsOutput(const char *, const FITS::FitsDevice &, int = 10, 
		   FITSErrorHandler errhandler = FITSError::defaultHandler);
	FitsOutput(FITSErrorHandler errhandler = FITSError::defaultHandler);
	~FitsOutput();
	//</group>
   // used by PrimaryArray, BinaryTabelExtention etc to work with the constructor without keyword list.
	void set_data_info( FitsKeywordList &kwl, FITS::HDUType t, FITS::ValueType dt, OFF_T ds, Int is);
	// write a special record. For this the record type must also
	// be to set to FITS::SpecialRecord
	int write_sp(char *rec);
	// check if the current hdu is done. It was private.
	int hdu_complete() { 
	    return (m_rec_type == FITS::HDURecord && m_data_size == 0);
   }
   BlockOutput & getfout(){ return m_fout; }
	void setfptr( fitsfile* ffp ); 
	Bool required_keys_only(){ return m_required_keys_only; }

    private:
	BlockOutput &m_fout;
	Bool m_required_keys_only;
	BlockOutput &make_output(const char *, const FITS::FitsDevice &, int, 
				 FITSErrorHandler errhandler = FITSError::defaultHandler);

	virtual void errmsg(FitsErrs, const char *);

	int hdu_inprogress() { 
	    return (m_rec_type == FITS::HDURecord && m_data_size > 0 && m_curr_size < m_data_size);
	 }

	// Special interface to class HeaderDataUnit
	//<group>
	int write_hdr(FitsKeywordList &, FITS::HDUType, FITS::ValueType, OFF_T, Int);
	// write all data from address
	int write_all(FITS::HDUType, char *, char);
	// write N bytes from address
	int write(FITS::HDUType, char *, Int, char); 
	//</group>
};

//<summary> FITS input from disk </summary>

class FitsDiskInput : public BlockInput {
    public:
	FitsDiskInput(const char *, int, int = 1, 
		      FITSErrorHandler errhandler = FITSError::defaultHandler);
	~FitsDiskInput();
        // implements skip in terms of lseek
	char *skip(int); 
};

//<summary> FITS output to disk </summary>

class FitsDiskOutput : public BlockOutput {
    public:
	FitsDiskOutput(const char *, int, int = 1, 
		       FITSErrorHandler errhandler = FITSError::defaultHandler);
	~FitsDiskOutput();
};

//<summary> FITS input from standard input </summary>

class FitsStdInput : public BlockInput {
    public:
	FitsStdInput(int, 
		     FITSErrorHandler errhandler = FITSError::defaultHandler);
	~FitsStdInput();
};

//<summary> FITS output to standard output </summary>

class FitsStdOutput : public BlockOutput {
    public:
	FitsStdOutput(int, 
		      FITSErrorHandler errhandler = FITSError::defaultHandler);
	~FitsStdOutput();
};

//<summary> FITS input from 9-track tape </summary>

class FitsTape9Input : public BlockInput {
    public:
	FitsTape9Input(const char *, int, int = 10, 
		       FITSErrorHandler errhandler = FITSError::defaultHandler);
	~FitsTape9Input();
};

//<summary> FITS output to 9-track tape </summary>

class FitsTape9Output : public BlockOutput {
    public:
	FitsTape9Output(const char *, int, int = 10, 
			FITSErrorHandler errhandler = FITSError::defaultHandler);
	~FitsTape9Output();
};


} //# NAMESPACE CASACORE - END

# endif
