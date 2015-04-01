//# hdu.h:
//# Copyright (C) 1993,1994,1995,1996,1997,1999,2000,2002,2003
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

#ifndef FITS_HDU_H
#define FITS_HDU_H

# include <casacore/casa/aips.h>
# include <casacore/fits/FITS/fits.h>
# include <casacore/fits/FITS/blockio.h>
# include <casacore/casa/BasicSL/String.h>
# include <casacore/casa/Arrays/Vector.h>

//# # include <stdarg.h> // If we ever wan to put varargs support back

namespace casacore { //# NAMESPACE CASACORE - BEGIN

class FitsInput;
class FitsOutput;

//<summary> base class that defines a HDU </summary>
//<synopsis>
// The class HeaderDataUnit contains what is common to all 
// header-data-units, including the collection of keywords.
// From this class a number of FITS header-data-units are 
// derived, each of them with their own rich assortment of 
// functions for accessing and manipulating data of specific types.
//
// The following inheritence hierarchy illustrates the current
// derived classes:
//<srcblock>
//
//				HeaderDataUnit
//				/          | 
//			       /           | 
//		     PrimaryArray	ExtensionHeaderDataUnit
//		      /  |  \			           | 
//		     /   |   \                             | 
//	  PrimaryGroup	 |   ImageExtension                | 
//		         |                                 | 
//	            PrimaryTable			BinaryTableExtension
//							   /
//							  /
//					AsciiTableExtension
//</srcblock>
//</synopsis>

class HeaderDataUnit {
	friend std::ostream & operator << (std::ostream &, HeaderDataUnit &);
    public:
	virtual ~HeaderDataUnit();

	Int dims() const 			{ return no_dims; }
	Int dim(int n) const 			{ return dimn[n]; }
	OFF_T fitsdatasize() const 		{ return fits_data_size; }
	FITS::ValueType datatype() const	{ return data_type; }
	Int fitsitemsize() const 		{ return fits_item_size; }
	Int localitemsize() const		{ return local_item_size; }
	FITS::HDUType hdutype() const 		{ return hdu_type; }

	// error handling and error codes that can be returned
	//<group>
	enum HDUErrs { OK, NOMEM, MISSKEY, BADBITPIX, NOAXISN,
		NOPCOUNT, NOGCOUNT, BADPCOUNT, BADGCOUNT, NOGROUPS,
		BADNAXIS, BADREC, BADTYPE, BADRULES, BADSIZE, BADOPER,
		BADCONV,  BADIO };
	int err() const { return err_status; }
	//</group>

	// skipping one or more HDU's
	//<group>
	int skip(uInt n);
	int skip();
	//</group>

	// write the current header
	int write_hdr(FitsOutput &);

	// Determines the HDU type and the data type 
	// Parameterss: keyword list, hdu type, data type, error handler and 
	// error status.
	// Returns False if a serious error was detected, otherwise True
	static Bool determine_type(FitsKeywordList &, FITS::HDUType &, 
		FITS::ValueType &, FITSErrorHandler, HDUErrs &);

 
	// Compute the total size of the data associated with an HDU.  
	// The number of dimensions is also determined.  This routine 
	// assumes that hdu type has been appropriately set, but it may 
	// be changed in the process.  Data type is also determined.
	// Returns False if a serious error was detected, otherwise True
	static Bool compute_size(FitsKeywordList &, OFF_T &, Int &,
		FITS::HDUType &, FITS::ValueType &, FITSErrorHandler, HDUErrs &);

	// Operations on the HDU's keyword list
	//<group>
	ConstFitsKeywordList &kwlist(){	 return constkwlist_;}
	// return the header of the chdu as a vector of String. You can
        // force the strings to be length 80 (padded with spaces)
	Vector<String> kwlist_str(Bool length80=False);
	void firstkw() { kwlist_.first(); }
	void lastkw() { kwlist_.last(); }
	const FitsKeyword *nextkw() { return kwlist_.next(); }
	const FitsKeyword *prevkw() { return kwlist_.prev(); }
	const FitsKeyword *currkw() { return kwlist_.curr(); }	
	const FitsKeyword *kw(int n) { return kwlist_(n); }
	//# 07/21/98 AKH Added const to quite Apogee warnings:
	const FitsKeyword *kw(const FITS::ReservedName &n) { 
		return kwlist_(n); }
	const FitsKeyword *nextkw(FITS::ReservedName &n) { 
		return kwlist_.next(n); }
	const FitsKeyword *kw(FITS::ReservedName &n, int i) { 
		return kwlist_(n,i); }
	const FitsKeyword *nextkw(FITS::ReservedName &n, int i) { 
		return kwlist_.next(n,i); }
	const FitsKeyword *kw(const char *n) { return kwlist_(n); }
	const FitsKeyword *nextkw(const char *n) { return kwlist_.next(n); }
	void mk(FITS::ReservedName k, Bool v, const char *c = 0);
	void mk(FITS::ReservedName k, const char *v = 0, const char *c = 0);
	void mk(FITS::ReservedName k, Int v, const char *c = 0);
	void mk(FITS::ReservedName k, double v, const char *c = 0);
	void mk(int n, FITS::ReservedName k, Bool v, const char *c = 0);
	void mk(int n, FITS::ReservedName k, const char *v, const char *c = 0);
	void mk(int n, FITS::ReservedName k, Int v, const char *c = 0);
	void mk(int n, FITS::ReservedName k, double v, const char *c = 0);
	void mk(const char *n, Bool v, const char *c = 0);
	void mk(const char *n, const char *v = 0, const char *c = 0);
	void mk(const char *n, Int v, const char *c = 0);
	void mk(const char *n, float v, const char *c = 0);
	void mk(const char *n, double v, const char *c = 0);
	void mk(const char *n, Int r, Int i, const char *c = 0);
	void mk(const char *n, float r, float i, const char *c = 0);
	void mk(const char *n, double r, double i, const char *c = 0);
	void spaces(const char *n = 0, const char *c = 0);
	void comment(const char *n = 0, const char *c = 0);
	void history(const char *c = 0);
	//</group>

	Bool notnull(double x) const { return double_null < x ? True : False; }
	Bool notnull(char *s) const { return ! s ? False : (s[0] != '\0' ? True : False); }
	Bool notnull(Int l) const { return Int_null < l ? True : False; }

    protected:
	//	For input -- ~ should delete the keyword list: kwflag = 1
	HeaderDataUnit(FitsInput &, FITS::HDUType, 
		       FITSErrorHandler errhandler = FITSError::defaultHandler);
	//	For output -- ~ should not delete keyword list: kwflag = 0
	// 07/21/98 AKH Clarification: HeaderDataUnit has a copy of the
	//              FitsKeywordList, and should delete it.  The kwflag
	//              comments above are not important now.
	HeaderDataUnit(FitsKeywordList &, FITS::HDUType, 
		       FITSErrorHandler errhandler = FITSError::defaultHandler,
		       FitsInput * = 0);
	// constructor for objects that write only required keyword to fits file.
	// the write method to call by these object should be those for the specific
	// hdu, such as write_bintbl_hdr().
	HeaderDataUnit(FITS::HDUType, 
		       FITSErrorHandler errhandler = FITSError::defaultHandler,
		       FitsInput * = 0);
	// for write required keywords only to use.
	bool init_data_unit( FITS::HDUType t );

	FitsKeywordList &kwlist_;
	ConstFitsKeywordList constkwlist_;
	void posEnd();

	FitsInput *fin;
	FITSErrorHandler errfn;
	HDUErrs err_status;
	void errmsg(HDUErrs, const char *);

	Int no_dims;		// number of dimensions
	Int *dimn;		// size of dimension N
	//uInt fits_data_size;	// size in bytes of total amount of data
	OFF_T fits_data_size;	// size in bytes of total amount of data
	FITS::ValueType data_type;	// type of data - derived from BITPIX
	Int fits_item_size;	// size in bytes of an item of FITS data
	Int local_item_size;	// size in bytes of an item of local data
	FITS::HDUType hdu_type;	// type of header/data unit
	char pad_char;		// char to pad FITS data block

	//<group>
	char * assign(FITS::ReservedName);
	char * assign(FITS::ReservedName, int);
	double asgdbl(FITS::ReservedName, double);
	double asgdbl(FITS::ReservedName, int, double);
	//</group>
	double double_null;
	char char_null;
	Int Int_null;

    public:
	int get_hdr(FITS::HDUType, FitsKeywordList &);
	int read_data(char *, Int);
	int write_data(FitsOutput &, char *, Int);
	OFF_T read_all_data(char *);
	int write_all_data(FitsOutput &, char *);
};

inline std::ostream & operator << (std::ostream &o, HeaderDataUnit &h) {
	return o << h.kwlist_; }
inline void HeaderDataUnit::mk(FITS::ReservedName k, Bool v, const char *c) {
	posEnd(); kwlist_.mk(k,v,c); }
inline void HeaderDataUnit::mk(FITS::ReservedName k, const char *v, 
	const char *c) { posEnd(); kwlist_.mk(k,v,c); }
inline void HeaderDataUnit::mk(FITS::ReservedName k, Int v, const char *c) {
	posEnd(); kwlist_.mk(k,v,c); }
inline void HeaderDataUnit::mk(FITS::ReservedName k, double v, const char *c) {
	posEnd(); kwlist_.mk(k,v,c); }
inline void HeaderDataUnit::mk(int n, FITS::ReservedName k, Bool v, 
	const char *c) { posEnd(); kwlist_.mk(n,k,v,c); }
inline void HeaderDataUnit::mk(int n, FITS::ReservedName k, const char *v, 
	const char *c) { posEnd(); kwlist_.mk(n,k,v,c); }
inline void HeaderDataUnit::mk(int n, FITS::ReservedName k, Int v, 
	const char *c) { posEnd(); kwlist_.mk(n,k,v,c); }
inline void HeaderDataUnit::mk(int n, FITS::ReservedName k, double v, 
	const char *c) { posEnd(); kwlist_.mk(n,k,v,c); }
inline void HeaderDataUnit::mk(const char *n, Bool v, const char *c) {
	posEnd(); kwlist_.mk(n,v,c); }
inline void HeaderDataUnit::mk(const char *n, const char *v, const char *c) {
	posEnd(); kwlist_.mk(n,v,c); }
inline void HeaderDataUnit::mk(const char *n, Int v, const char *c) {
	posEnd(); kwlist_.mk(n,v,c); }
inline void HeaderDataUnit::mk(const char *n, float v, const char *c) {
	posEnd(); kwlist_.mk(n,v,c); }
inline void HeaderDataUnit::mk(const char *n, double v, const char *c) {
	posEnd(); kwlist_.mk(n,v,c); }
inline void HeaderDataUnit::mk(const char *n, Int r, Int i, const char *c) {
	posEnd(); kwlist_.mk(n,r,i,c); }
inline void HeaderDataUnit::mk(const char *n, float r, float i, const char *c) {
	posEnd(); kwlist_.mk(n,r,i,c); }
inline void HeaderDataUnit::mk(const char *n, double r, double i, 
	const char *c) { posEnd(); kwlist_.mk(n,r,i,c); }
inline void HeaderDataUnit::spaces(const char *n, const char *c) { 
	posEnd(); kwlist_.spaces(n,c); }
inline void HeaderDataUnit::comment(const char *n, const char *c) { 
	posEnd(); kwlist_.comment(n,c); }
inline void HeaderDataUnit::history(const char *c) { 
	posEnd(); kwlist_.history(c); }

//<summary> templated primary array base class of given type </summary>
//<synopsis>
// A Primary Data Array is represented by the following:
//<srcblock>
//	<Type> data_array [NAXIS1][NAXIS2]...[NAXISN]
//</srcblock>
//
// For a PrimaryArray, dims() gives the number of dimensions
// and dim(i) gives the value of the i-th dimension
//
// WARNING!  Multi-dimensional arrays are stored in FORTRAN order, 
// NOT in C order.  Options on the store, copy, and move functions exist 
// to convert from one order to the other, if that is necessary.
//
// 
// It is important to understand the proper sequence of operations with
// respect to I/O and data access.  For input, the `read()' functions
// allocate an internal buffer of the appropriate size, if not already
// allocated, as well as reading and converting data; a `read()' function
// must be performed prior to accessing the data, i. e. before executing
// any `()', `data()', `copy()', or `move()' function.  For output, the
// `store()' function similarly allocates an internal buffer before
// transfering data, and must be executed prior to any data access or
// `write()' function. Note: If you call any version of store(), do not
// call set_next().
// 
// Writing portions of an array at a time, rather than the entire array,
// is a special case.  The `set_next()' function is provided for this
// purpose. It declares the intention to write out the next N elements and
// must be executed prior to any `data()' function.  It allocates a buffer
// of appropriate size, if not already allocated.  Again, via the `data()'
// functions, one accesses the array as if the entire array were in
// memory.  The `write()' function always writes the number of current
// elements in the internal buffer.  The sequence of operations for each
// portion of the array written would be: 
// <ul>
// <li> `set_next(N)', 
// <li> fill the array using `data(N)' or other `data()' functions
// <li> `write(fout)'.  
// </ul>
// The `set_next()' function must NOT be used with
// `read()' or `store()' functions; unpredictable results will occur.  
//<example>
// The following example illustrates the output cases.
// 
// Suppose we have an image array with 512 rows and 1024 columns
// stored in C-order.  The C declaration would be:
//<srcblock>
// 	int source[1024][512];
//</srcblock>
// To write out the entire array:
//<srcblock>
// 	FitsOutput fout; // some properly constructed FitsOutput
// 	PrimaryArray<FitsLong> pa; // some properly constructed PrimaryArray
// 	pa.store(source,CtoF);
// 	pa.write(fout);
//</srcblock>
//
// Suppose we wanted to write out the two-dimensional image array a column
// at a time, rather than write out the entire array.  For FITS, dim(0)
// is 512, dim(1) is 1024.  The following code fragment writes one column 
// at a time in the proper FITS Fortran-order.
//
//<srcblock> 
// 	for (i = 0; i < dim(1); ++i) {
// 		pa.set_next(dim(0));
// 		for (j = 0; j < dim(0); ++j)
// 			data(j,i) = source[i][j];
// 		pa.write(fout);
// 	}
//</srcblock>
//</example>
//
//</synopsis>

template <class TYPE>
class PrimaryArray : public HeaderDataUnit {
    public:
        typedef TYPE ElementType;

	// constructor from a FitsInput
	PrimaryArray(FitsInput &, FITSErrorHandler= FITSError::defaultHandler);
	// constructor from a FitsKeywordList
	PrimaryArray(FitsKeywordList &, 
		     FITSErrorHandler= FITSError::defaultHandler);
	// constructor does not require a FitsKeywordList. call write_priArr_hdr() after construction.
	PrimaryArray(FITSErrorHandler= FITSError::defaultHandler);

	// destructor
	virtual ~PrimaryArray();

	// General access routines for a primary array
	//<group>
	double bscale() const 		{ return bscale_x; }
	double bzero() const 		{ return bzero_x; }
	char *bunit() const 		{ return bunit_x; }
	Bool isablank() const		{ return isablank_x; }
	Int blank() const 		{ return blank_x; }
	char *ctype(int n) const 	{ return ctype_x[n]; }
	double crpix(int n) const 	{ return crpix_x[n]; }
	double crota(int n) const 	{ return crota_x[n]; }
	double crval(int n) const 	{ return crval_x[n]; }
	double cdelt(int n) const 	{ return cdelt_x[n]; }
	double datamax() const 		{ return datamax_x; }
	double datamin() const 		{ return datamin_x; }
	OFF_T nelements() const		{ return totsize; }
	//</group>

	// The overloaded operator functions `()' all return physical data, i. e.,
	// data to which bscale() and bzero() have been applied, via the formula
	//<srcblock>
	// 	physical_data[i] = bscale() * raw_data[i] + bzero().
	//</srcblock>
	//<group>
	double operator () (int, int, int, int, int) const; 
	double operator () (int, int, int, int) const; 
	double operator () (int, int, int) const; 
	double operator () (int, int) const;
	double operator () (int) const;
	//</group>

	// The various `data()' functions allow one to access and set the raw data 
	// itself.  
	//<group>
	TYPE & data(int, int, int, int, int); 
	TYPE & data(int, int, int, int); 
	TYPE & data(int, int, int); 
	TYPE & data(int, int);
	TYPE & data(int);
	//</group>

	// The `store()', `move()' and `copy()' functions allow bulk data
	// transfer between the internal FITS array and an external data
	// storage area.  The external storage must have already been allocated
	// and it is assumed that the entire data array is in memory.
	// `store()' transfers raw data at `source' into the FITS array; an
	// allowable option is CtoF, which specifies to convert the array from
	// C-order to Fortran-order.  `move()' is the opposite of `store()'.
	// `move()' transfers raw data from the FITS array to `target'; an
	// allowable option is FtoC, which specifies to convert the array from
	// Fortran-order to C-order.  `copy()' is similar to `move()' except
	// that what is copied is physical data and not raw data; the physical
	// data can be either double or float. copy() also turns blanks into
        // NaN's.
	//<group>
	int store(const TYPE *source, FITS::FitsArrayOption = FITS::NoOpt);
	void copy(double *target, FITS::FitsArrayOption = FITS::NoOpt) const;
	void copy(float *target, FITS::FitsArrayOption = FITS::NoOpt) const;
	void move(TYPE *target, FITS::FitsArrayOption = FITS::NoOpt) const;
        //     <group>
        // Use these versions if you are reading/writing "chunk by
        // chunk." No FtoC option is available. You are responsible for
        // ensuring that npixels corresponds to he number actually read or
        // written. Note that copy() turns blanks into NaN's.
	int store(const TYPE *source, int npixels);
	void copy(double *target, int npixels) const;
	void copy(float *target, int npixels) const;
	void move(TYPE *target, int npixels) const;
        //     </group>
	// </group>
	//<group>
	int write_priArr_hdr( FitsOutput &fout, int simple, int bitpix,     
			      int naxis, long naxes[], int extend );            
	//</group>
	// The `read()' and `write()' functions control reading and writing data
	// from the external FITS I/O medium into the FITS array.  Appropriate
	// conversions are made between FITS and local data representations.  One
	// can read the entire array into memory, or one can only read portions of
	// the array.  In the latter case, one must specify that the next N
	// elements are to be read or written.  Note that the number of elements
	// must be specified, NOT the number of bytes.  If one reads portions of
	// the array, as opposed to the entire array, only that portion is in
	// memory at a given time.  One can still access the elements of the array
	// via the `()' and `data()' functions, as if the entire array was in
	// memory; obviously care must be taken in this case to access only those
	// portions that are actually in memory.
	//<group>
	virtual int read(); 
	virtual int read( int ); 
	virtual int write(FitsOutput &); 
	virtual OFF_T set_next(OFF_T);
	//</group>
	//### if these, even as interspersed comments, cxx2html repeats the global
	//# group info..
	//# read: read entire array into memory
	//# read() read next N elements into memory
	//# write; write current data
	//# set_next(): prepare to write next N elements

    protected:
	// construct from a FitsInput with given HDU type
	PrimaryArray(FitsInput &, FITS::HDUType, 
		     FITSErrorHandler errhandler = FITSError::defaultHandler);
	// construct from a FitsKeywordList with given HDU type
	PrimaryArray(FitsKeywordList &, FITS::HDUType, 
		     FITSErrorHandler errhandler = FITSError::defaultHandler);
			  
	// construct witout FitsKeywordList for given HDU type( for ImageExtension and PrimaryGroup)
	PrimaryArray(FITS::HDUType, 
		     FITSErrorHandler errhandler = FITSError::defaultHandler);


	double bscale_x;
	double bzero_x;
	char *bunit_x;
	Bool isablank_x;
	Int blank_x;
	char **ctype_x;
	double *crpix_x;
	double *crota_x;
	double *crval_x;
	double *cdelt_x;
	double datamax_x;
	double datamin_x;
	OFF_T totsize;

	int *factor; // factors needed to compute array position offsets

	// compute a linear offset from array indicies
	//<group>
	int offset(int, int) const; 
	int offset(int, int, int) const; 
	int offset(int, int, int, int) const; 
	int offset(int, int, int, int, int) const; 
	//</group>
	OFF_T alloc_elems; // current number of allocated elements
	OFF_T beg_elem; // offset of first element in the buffer
	OFF_T end_elem; // offset of last element in the buffer
	// the allocated array
	TYPE *array;

	void pa_assign();
};

typedef PrimaryArray<unsigned char> BytePrimaryArray;
typedef PrimaryArray<short> ShortPrimaryArray;
typedef PrimaryArray<FitsLong> LongPrimaryArray;
typedef PrimaryArray<float> FloatPrimaryArray;
typedef PrimaryArray<double> DoublePrimaryArray;


//<summary> IMAGE extension of given type </summary>
//<templating>
// <li> typedef ImageExtension<unsigned char> ByteImageExtension;
// <li> typedef ImageExtension<short> ShortImageExtension;
// <li> typedef ImageExtension<FitsLong> LongImageExtension;
// <li> typedef ImageExtension<float> FloatImageExtension;
// <li> typedef ImageExtension<double> DoubleImageExtension;
//</templating>

template <class TYPE>
class ImageExtension : public PrimaryArray<TYPE> {
    public:
        typedef TYPE ElementType;

	ImageExtension(FitsInput &, 
		       FITSErrorHandler errhandler = FITSError::defaultHandler);
	ImageExtension(FitsKeywordList &, 
		       FITSErrorHandler errhandler = FITSError::defaultHandler);
	// constructor for header consisted required keywords only			 
	ImageExtension(FITSErrorHandler errhandler = FITSError::defaultHandler);

	~ImageExtension();
	char *xtension() 	{ return xtension_x; }
	char *extname() 	{ return extname_x; }
	Int extver() 		{ return extver_x; }
	Int extlevel() 	{ return extlevel_x; }
	Int pcount() 		{ return pcount_x; }
	Int gcount() 		{ return gcount_x; }
	// write required keywords for ImageExtension
	int write_imgExt_hdr( FitsOutput &fout,
            int bitpix, int naxis, long *naxes);     
    protected:
	char *xtension_x;
	char *extname_x;
	Int extver_x;
	Int extlevel_x;
	Int pcount_x;
	Int gcount_x;

    private:
	void ie_assign();

	//# Make members in parent known
    protected:
	using PrimaryArray<TYPE>::assign;
	using PrimaryArray<TYPE>::errmsg;
	using PrimaryArray<TYPE>::init_data_unit;
	using PrimaryArray<TYPE>::pa_assign;
	using PrimaryArray<TYPE>::char_null;
	using PrimaryArray<TYPE>::kwlist_;
	using PrimaryArray<TYPE>::errfn;
	using PrimaryArray<TYPE>::hdu_type;
	using PrimaryArray<TYPE>::data_type;
	using PrimaryArray<TYPE>::fits_data_size;
	using PrimaryArray<TYPE>::fits_item_size;
	using PrimaryArray<TYPE>::array;
	using PrimaryArray<TYPE>::BADOPER;
};

typedef ImageExtension<unsigned char> ByteImageExtension;
typedef ImageExtension<short> ShortImageExtension;
typedef ImageExtension<FitsLong> LongImageExtension;
typedef ImageExtension<float> FloatImageExtension;
typedef ImageExtension<double> DoubleImageExtension;

//<summary> Random Group datastructure </summary>
//<synopsis>
// A Random Group Structure is represented by the following:
//<srcblock>
//	struct GroupData {
//		<Type> group_parms [PCOUNT];
//		<Type> data_array [NAXIS2][NAXIS3]...[NAXISN];
//	} group_data[GCOUNT];
//</srcblock>
//</synopsis>
//<templating>
//#until cxx2html can handle this, duplicate
// <li>typedef PrimaryGroup<unsigned char> BytePrimaryGroup;
// <li> typedef PrimaryGroup<short> ShortPrimaryGroup;
// <li> typedef PrimaryGroup<FitsLong> LongPrimaryGroup;
// <li> typedef PrimaryGroup<float> FloatPrimaryGroup;
// <li> typedef PrimaryGroup<double> DoublePrimaryGroup;
//</templating>
//<note role=warning>
// Please note that the NOST has deprecated the Random Group 
// datastructure, it has been replaced by the much more powerfull
// BINTABLE extension.
//</note>
template <class TYPE>
class PrimaryGroup : public PrimaryArray<TYPE> {
    public:
	PrimaryGroup(FitsInput &, 
		     FITSErrorHandler errhandler = FITSError::defaultHandler);
	PrimaryGroup(FitsKeywordList &, 
		     FITSErrorHandler errhandler = FITSError::defaultHandler);
	// constructor for header consisted required keywords only
	PrimaryGroup(FITSErrorHandler errhandler = FITSError::defaultHandler);
			 
	~PrimaryGroup();

	// Return basic parameters of a random group
	//<group>
	Int gcount() const 	   { return gcount_x; }
	Int pcount()  const	   { return pcount_x; }
	char *ptype(int n)  const  { return ptype_x[n]; }
	double pscal(int n)  const { return pscal_x[n]; }
	double pzero(int n)  const { return pzero_x[n]; }
	//</group>

	Int currgroup() const	   { return current_group; }

	double parm(int); // return physical parms
	TYPE & rawparm(int); // access raw parms

	void storeparm(const TYPE *source);
	void copyparm(double *target) const;
	void copyparm(float *target) const;
	void moveparm(TYPE *target) const;

	// read, or write the next group
	//<group>
	int read();
	int write(FitsOutput &);
	//</group>
	// write the required keywords for PrimaryGroup
	//<group>
	int write_priGrp_hdr( FitsOutput &fout, int simple, int bitpix,   
            int naxis, long naxes[], long pcount, long gcount ); 
   //</group>
	
	// disable these functions, since they
	// are inherited from PrimaryArray
	//<group>
	OFF_T set_next(OFF_T) { return 0; }
	int read(int) { return -1; }
	//</group>

    protected:
	Int pcount_x;
	Int gcount_x;
	char **ptype_x;
	double *pscal_x;
	double *pzero_x;
	TYPE *group_parm;
	Int current_group;	

    private:
	void pg_assign();

	//# Make members in parent known
    protected:
	using PrimaryArray<TYPE>::assign;
	using PrimaryArray<TYPE>::errmsg;
	using PrimaryArray<TYPE>::init_data_unit;
	using PrimaryArray<TYPE>::pa_assign;
	using PrimaryArray<TYPE>::asgdbl;
	using PrimaryArray<TYPE>::nelements;
	using PrimaryArray<TYPE>::localitemsize;
	using PrimaryArray<TYPE>::fitsitemsize;
	using PrimaryArray<TYPE>::read_data;
	using PrimaryArray<TYPE>::write_data;
	using PrimaryArray<TYPE>::char_null;
	using PrimaryArray<TYPE>::kwlist_;
	using PrimaryArray<TYPE>::errfn;
	using PrimaryArray<TYPE>::err_status;
	using PrimaryArray<TYPE>::hdu_type;
	using PrimaryArray<TYPE>::data_type;
	using PrimaryArray<TYPE>::fits_data_size;
	using PrimaryArray<TYPE>::fits_item_size;
	using PrimaryArray<TYPE>::array;
	using PrimaryArray<TYPE>::totsize;
	using PrimaryArray<TYPE>::dimn;
	using PrimaryArray<TYPE>::no_dims;
	using PrimaryArray<TYPE>::factor;
	using PrimaryArray<TYPE>::ctype_x;
	using PrimaryArray<TYPE>::crpix_x;
	using PrimaryArray<TYPE>::crota_x;
	using PrimaryArray<TYPE>::crval_x;
	using PrimaryArray<TYPE>::cdelt_x;
	using PrimaryArray<TYPE>::BADOPER;
	using PrimaryArray<TYPE>::OK;
	using PrimaryArray<TYPE>::NOMEM;
	using PrimaryArray<TYPE>::BADIO;
};

typedef PrimaryGroup<unsigned char> BytePrimaryGroup;
typedef PrimaryGroup<short> ShortPrimaryGroup;
typedef PrimaryGroup<FitsLong> LongPrimaryGroup;
typedef PrimaryGroup<float> FloatPrimaryGroup;
typedef PrimaryGroup<double> DoublePrimaryGroup;

//<summary> Primary Table structure </summary>
//<templating>
// <li> typedef PrimaryTable<unsigned char> BytePrimaryTable;
// <li> typedef PrimaryTable<short> ShortPrimaryTable;
// <li> typedef PrimaryTable<FitsLong> LongPrimaryTable;
// <li> typedef PrimaryTable<float> FloatPrimaryTable;
// <li> typedef PrimaryTable<double> DoublePrimaryTable;
//</templating>

template <class TYPE>
class PrimaryTable : public PrimaryArray<TYPE> {
    public:
        typedef TYPE ElementType;

	PrimaryTable(FitsInput &, 
		       FITSErrorHandler errhandler = FITSError::defaultHandler);
	PrimaryTable(FitsKeywordList &, 
		       FITSErrorHandler errhandler = FITSError::defaultHandler);
	// constructor for header consisted required keywords only			 
	PrimaryTable(FITSErrorHandler errhandler = FITSError::defaultHandler);

	~PrimaryTable();
	// write required keywords for PrimaryTable
	int write_priTable_hdr( FitsOutput &fout,
            int bitpix, int naxis, long *naxes);     

	int read();
	int read(int) { return -1; }
	int write(FitsOutput &){ return -1; }

	char* object() const 	   { return object_x; }
	char* telescop()  const	   { return telescop_x; }
	char* instrume()  const	   { return instrume_x; }
	char* dateobs()  const { return dateobs_x; }
	char* datemap()  const { return datemap_x; }
	char* bunit()  const	   { return bunit_x; }
	float bscal()  const { return bscale_x; }
	float bzero()  const { return bzero_x; }
	float equinox()  const { return equinox_x; }
	float altrpix()  const { return altrpix_x; }

    protected:
        char* object_x;      //OBJECT
        char* telescop_x;    //TELESCOP
        char* instrume_x;    //INSTRUME
        char* dateobs_x;    //DATE-OBS
        char* datemap_x;    //DATE-MAP
        Float bscale_x;      //BSCALE
        Float bzero_x;       //BZERO
        char* bunit_x;       //BUNIT
        Float equinox_x;     //EQUINOX
        Float altrpix_x;     //ALTRPIX


    private:
	void pt_assign();

	//# Make members in parent known
    protected:
	using PrimaryArray<TYPE>::assign;
	using PrimaryArray<TYPE>::errmsg;
	using PrimaryArray<TYPE>::init_data_unit;
	using PrimaryArray<TYPE>::pa_assign;
	using PrimaryArray<TYPE>::asgdbl;
	using PrimaryArray<TYPE>::nelements;
	using PrimaryArray<TYPE>::localitemsize;
	using PrimaryArray<TYPE>::fitsitemsize;
	using PrimaryArray<TYPE>::read_data;
	using PrimaryArray<TYPE>::write_data;
	using PrimaryArray<TYPE>::char_null;
	using PrimaryArray<TYPE>::kwlist_;
	using PrimaryArray<TYPE>::errfn;
	using PrimaryArray<TYPE>::err_status;
	using PrimaryArray<TYPE>::hdu_type;
	using PrimaryArray<TYPE>::data_type;
	using PrimaryArray<TYPE>::fits_data_size;
	using PrimaryArray<TYPE>::fits_item_size;
	using PrimaryArray<TYPE>::array;
	using PrimaryArray<TYPE>::totsize;
	using PrimaryArray<TYPE>::dimn;
	using PrimaryArray<TYPE>::no_dims;
	using PrimaryArray<TYPE>::factor;
	using PrimaryArray<TYPE>::ctype_x;
	using PrimaryArray<TYPE>::crpix_x;
	using PrimaryArray<TYPE>::crota_x;
	using PrimaryArray<TYPE>::crval_x;
	using PrimaryArray<TYPE>::cdelt_x;
	using PrimaryArray<TYPE>::BADOPER;
	using PrimaryArray<TYPE>::OK;
	using PrimaryArray<TYPE>::NOMEM;
	using PrimaryArray<TYPE>::BADIO;
};

typedef PrimaryTable<unsigned char> BytePrimaryTable;
typedef PrimaryTable<short> ShortPrimaryTable;
typedef PrimaryTable<FitsLong> LongPrimaryTable;
typedef PrimaryTable<float> FloatPrimaryTable;
typedef PrimaryTable<double> DoublePrimaryTable;

//<summary>  base class for generalized exentensions HDU </summary>

class ExtensionHeaderDataUnit : public HeaderDataUnit {
    public:
	ExtensionHeaderDataUnit(FitsInput &, 
				FITSErrorHandler errhandler = FITSError::defaultHandler);
	ExtensionHeaderDataUnit(FitsKeywordList &, 
				FITSErrorHandler errhandler = FITSError::defaultHandler);
	~ExtensionHeaderDataUnit();
	char *xtension() 	{ return xtension_x; }
	char *extname() 	{ return extname_x; }
	Int extver() 		{ return extver_x; }
	Int extlevel() 	{ return extlevel_x; }
	Int pcount() 		{ return pcount_x; }
	Int gcount() 		{ return gcount_x; }

	// read next N bytes into addr
	int read(char *addr, int nbytes) {
	    return read_data(addr, Int(nbytes)); }
	// write next N bytes from addr to the FITS output fout
	int write(FitsOutput &fout, char *addr, int nbytes) {
	    return write_data(fout,addr,nbytes); }

    protected:
	ExtensionHeaderDataUnit(FitsInput &, FITS::HDUType, 
				FITSErrorHandler errhandler = FITSError::defaultHandler);
	ExtensionHeaderDataUnit(FitsKeywordList &, FITS::HDUType, 
				FITSErrorHandler errhandler = FITSError::defaultHandler);
	// This constructor is used for writing only required keywords.
	ExtensionHeaderDataUnit(FITS::HDUType, 
				FITSErrorHandler errhandler = FITSError::defaultHandler);

	char *xtension_x;
	char *extname_x;
	Int extver_x;
	Int extlevel_x;
	Int pcount_x;
	Int gcount_x;

    private:
	void ex_assign();
};

//<summary> helper class </summary>

class FitsBase {
	friend class BinaryTableExtension;
	friend class AsciiTableExtension;
    public:
	FitsBase(const FITS::ValueType &t, int n) : no_elements(n), 
		data_type(t) { }
	virtual ~FitsBase();

	unsigned int nelements() const 	  { return (unsigned int)no_elements; }
	virtual int fitsfieldsize() const = 0;
	virtual int localfieldsize() const = 0;
	virtual void *data() = 0;
	virtual int dims() const;
	virtual int dim(int n) const;
	virtual int *vdim();
	FITS::ValueType fieldtype() const { return data_type; }

	static FitsBase *make(const FITS::ValueType &, int = 1);
	static FitsBase *make(const FITS::ValueType &, int, int *);
	static FitsBase *make(FitsBase &);

	FitsBase & operator = (FitsBase &);
	virtual void show(std::ostream &) = 0;

    protected:
	int no_elements; // the number of elements in the field
	FITS::ValueType data_type;
	virtual void setaddr(void **) = 0;
};

inline std::ostream & operator << (std::ostream &o, FitsBase &x) {
	x.show(o); return o;
}

//<summary> helper class </summary>
//<note>
// Note that FitsField does not allocate space for the data.
// Space is external to FitsField and its address is set via the
// setaddr function.
//</note>

template <class TYPE>
class FitsField : public FitsBase {
    public:
	FitsField(int n = 1) :
	    FitsBase(FITS::getfitstype(NoConvert<TYPE>()),n), field(0) { }
	~FitsField();

	TYPE & operator () () { return (*field)[0]; }
	TYPE & operator () (int i) { return (*field)[i]; }
	FitsField<TYPE> & operator = (const TYPE &x) {
					(*field)[0] = x; return *this; }

	int fitsfieldsize() const;
	int localfieldsize() const;

	void *data();

	void show(std::ostream &);

    protected:
	TYPE **field;
	void setaddr(void **addr);
};

//<summary> helper class </summary>
//<templating>
//#until cxx2html can handle this, duplicate
// <li> typedef FitsField<FitsLogical> LogicalFitsField;
// <li> typedef FitsField<FitsBit> BitFitsField;
// <li> typedef FitsField<char> CharFitsField;
// <li> typedef FitsField<unsigned char> ByteFitsField;
// <li> typedef FitsField<short> ShortFitsField;
// <li> typedef FitsField<FitsLong> LongFitsField;
// <li> typedef FitsField<float> FloatFitsField;
// <li> typedef FitsField<double> DoubleFitsField;
// <li> typedef FitsField<Complex> ComplexFitsField;
// <li> typedef FitsField<IComplex> IComplexFitsField;
// <li> typedef FitsField<DComplex> DComplexFitsField;
// <li> typedef FitsField<FitsVADesc> VADescFitsField;
//</templating>
//<note role=caution> 
// Bit fields require special treatment
//</note>

template <> class FitsField<FitsBit> : public FitsBase {
    public:
	FitsField(int n = 1);
	~FitsField();

	FitsField<FitsBit> & operator () () { byte_offset = 0; mask = 0200;
		return *this; }

	FitsField<FitsBit> & operator () (unsigned i) {
	    byte_offset = i / 8; mask = 0200 >> (i % 8); return *this; }

	FitsField<FitsBit> & operator = (unsigned i) {
	    (*field)[byte_offset] =
		(i == 0 ? ((*field)[byte_offset] & ~mask) :
			  ((*field)[byte_offset] | mask)); return *this; }

	int fitsfieldsize() const;
	int localfieldsize() const;

	operator int() { return (((*field)[byte_offset] & mask) != 0); }

	void *data();

	void show(std::ostream &);

    protected:
	FitsBit **field;
	unsigned char mask;
	int byte_offset;
	void setaddr(void **addr);
};

typedef FitsField<FitsLogical> LogicalFitsField;
typedef FitsField<FitsBit> BitFitsField;
typedef FitsField<char> CharFitsField;
typedef FitsField<unsigned char> ByteFitsField;
typedef FitsField<short> ShortFitsField;
typedef FitsField<FitsLong> LongFitsField;
typedef FitsField<float> FloatFitsField;
typedef FitsField<double> DoubleFitsField;
typedef FitsField<Complex> ComplexFitsField;
typedef FitsField<IComplex> IComplexFitsField;
typedef FitsField<DComplex> DComplexFitsField;
typedef FitsField<FitsVADesc> VADescFitsField;

//<summary> FITS array of given type </summary>
template <class TYPE>
class FitsArray : public FitsField<TYPE> {
    public:
	FitsArray(int, const int *);
    	~FitsArray();
    	TYPE & operator () (int d0, int d1);
    	TYPE & operator () (int, int, int);
    	TYPE & operator () (int, int, int, int);
    	TYPE & operator () (int, int, int, int, int);
	int dims() const;
	int dim(int n) const;
	int *vdim();
    protected:
	int no_dims;
	int *dimn;
	int *factor;

	//# Make members in parent known
    protected:
	using FitsField<TYPE>::no_elements;
	using FitsField<TYPE>::field;
};

//<summary> FITS array of FitsBit type </summary>

//<note>
// We must specify a FitsArray<FitsBit> as a specialization.
//</note>

template <> class FitsArray<FitsBit> : public FitsField<FitsBit> {
    public:
	FitsArray(int, const int *);
    	~FitsArray();
    	FitsField<FitsBit> & operator () (int d0, int d1);
    	FitsField<FitsBit> & operator () (int, int, int);
    	FitsField<FitsBit> & operator () (int, int, int, int);
    	FitsField<FitsBit> & operator () (int, int, int, int, int);
//# Disabled for now - we might eventually want to put varargs support back
//#    	FitsField<FitsBit> & operator () (int, int, int, int, int, int ...);

	int dims() const;
	int dim(int n) const;
	int *vdim();
    protected:
	int no_dims;
	int *dimn;
	int *factor;
};

typedef FitsArray<FitsLogical> LogicalFitsArray;
typedef FitsArray<FitsBit> BitFitsArray;
typedef FitsArray<char> CharFitsArray;
typedef FitsArray<unsigned char> ByteFitsArray;
typedef FitsArray<short> ShortFitsArray;
typedef FitsArray<FitsLong> LongFitsArray;
typedef FitsArray<float> FloatFitsArray;
typedef FitsArray<double> DoubleFitsArray;
typedef FitsArray<Complex> ComplexFitsArray;
typedef FitsArray<IComplex> IComplexFitsArray;
typedef FitsArray<DComplex> DComplexFitsArray;
typedef FitsArray<FitsVADesc> VADescFitsArray;

//<summary> BINTABLE extension </summary>

class BinaryTableExtension : public ExtensionHeaderDataUnit {
    public:
	BinaryTableExtension(FitsInput &, 
			     FITSErrorHandler errhandler = FITSError::defaultHandler);
				  
	BinaryTableExtension(FitsKeywordList &, 
			     FITSErrorHandler errhandler = FITSError::defaultHandler);
	// constructor to match write_bintbl_hdr()			  
	BinaryTableExtension( FITSErrorHandler errhandler = FITSError::defaultHandler);

	virtual ~BinaryTableExtension();

	// return basic elements of a table
	//<group>
	Int nrows() const		{ return dim(1); }
	Int ncols() const		{ return tfields_x; }
	uInt rowsize() const		{ return fitsrowsize; }
	Int tfields() const 		{ return tfields_x; }
	const char *tform(int n) const 	{ return tform_x[n]; }
	double tscal(int n) const 	{ return tscal_x[n]; }
	double tzero(int n) const 	{ return tzero_x[n]; }
	Bool isatnull(int n) const	{ return isatnull_x[n]; }
	Int  tnull(int n) const 	{ return tnull_x[n]; }
	const char *ttype(int n) const 	{ return ttype_x[n]; }	
	const char *tunit(int n) const 	{ return tunit_x[n]; }
	const char *tdisp(int n) const 	{ return tdisp_x[n]; }
	const char *tdim(int n) const 	{ return tdim_x[n]; }
	const char *ctype(int n) const 	{ return ctype_x[n]; }
	double crpix(int n) const 	{ return crpix_x[n]; }
	double crota(int n) const 	{ return crota_x[n]; }
	double crval(int n) const 	{ return crval_x[n]; }
	double cdelt(int n) const 	{ return cdelt_x[n]; }
	Int theap() const 		{ return theap_x; }
	const char *author() const 	{ return author_x; }
	const char *referenc() const	{ return referenc_x; }
	//</group>

	// binds a FitsField to a column
	int bind(int, FitsBase &); 

	// row selector functions
	//<group>
	BinaryTableExtension & operator ++ ();	
	BinaryTableExtension & operator -- ();
	BinaryTableExtension & operator () (int);
	//</group>

	// read entire table into memory
	int read();
	// read next N rows into memory
	int read(int);
	// prepare to write the next N rows
	int set_next(int); 	 
	// write current rows
	int write(FitsOutput &);
	// create a binary table header without using FitsKeywordList objet.
	int write_binTbl_hdr(FitsOutput &, long, int, const char**,
			     const char**, const char**, const char*, long );

	// select a field
	FitsBase &field(int i) const	{ return *fld[i]; }
	// get current row
	Int currrow() const		{ return curr_row; }
	// sets field addresses in the current row
	//void set_fitsrow(Int);

    protected:
	BinaryTableExtension(FitsInput &, FITS::HDUType, 
			     FITSErrorHandler errhandler = FITSError::defaultHandler);
	BinaryTableExtension(FitsKeywordList &, FITS::HDUType, 
			     FITSErrorHandler errhandler = FITSError::defaultHandler);
	BinaryTableExtension(FITS::HDUType, 
			     FITSErrorHandler errhandler = FITSError::defaultHandler);

	Int tfields_x;
	char **tform_x;
	double *tscal_x;
	double *tzero_x;
	Bool *isatnull_x;
	Int *tnull_x;
	char **ttype_x;	
	char **tunit_x;
	char **tdisp_x;
	char **tdim_x;
	char **ctype_x;
	double *crpix_x;
	double *crota_x;
	double *crval_x;
	double *cdelt_x;
	Int nAxis;
	Int theap_x;
	char *author_x;
	char *referenc_x;

	// read and write the next FITS data row
	//<group>
	virtual int readrow();	
	virtual int writerow(FitsOutput &);
	//</group>
	unsigned char *fitsrow;	// the FITS data row buffer
	uInt *fits_offset;	// Offsets to the fields within a FITS row
	uInt fitsrowsize;	// size in bytes of a FITS data row
	Bool isoptimum;		// tells whether optimum case exists or not

	// sets field addresses in the current row
	void set_fitsrow(Int);

	unsigned char *table;	// the table in local format
	uInt tablerowsize;	// size in bytes of a table row
	uInt alloc_row;		// number of currently allocated rows
	Int beg_row;		// range of rows currently in memory
	Int end_row;
	Int curr_row;
	FitsBase **fld;		// The array of fields
	uInt *table_offset;	// Offsets to the fields within a table row
	// data addresses of fields of current row
	void **data_addr;	

    private:
	void bt_assign();
};


//<summary> (ascii) TABLE extension </summary>

class AsciiTableExtension : public BinaryTableExtension {
    public:
	AsciiTableExtension(FitsInput &, 
			    FITSErrorHandler errhandler = FITSError::defaultHandler);
	AsciiTableExtension(FitsKeywordList &, 
			    FITSErrorHandler errhandler = FITSError::defaultHandler);
	AsciiTableExtension(FITSErrorHandler errhandler = FITSError::defaultHandler);

	~AsciiTableExtension();

	//# special overriden functions for ascii TABLE only
	// position in which column starts
	Int tbcol(int n) 	{ return tbcol_x[n]; }
	// ascii string that represents the NULL value
	char *tnull(int n) 	{ return tnulla_x[n]; }
	// write the required keywords for ASCIITableExtension
	int write_ascTbl_hdr( FitsOutput &, long,
			      long, int, const char **, long *,
			      const char **, const char **, const char *e);   

    protected:
	Int *tbcol_x;
	char **tnulla_x;
	uInt *fits_width;	// widths of the fields within a FITS row
        char **format;		// converted formats of the fields

	// read and write the next FITS data row
	//<group>
	int readrow();		
	int writerow(FitsOutput &);
	//</group>  

    private:
	void at_assign();
};


} //# NAMESPACE CASACORE - END

#ifndef CASACORE_NO_AUTO_TEMPLATES
#include <casacore/fits/FITS/hdu.tcc>
#endif //# CASACORE_NO_AUTO_TEMPLATES
# endif
