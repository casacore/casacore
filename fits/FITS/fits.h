//# fits.h:
//# Copyright (C) 1993,1994,1995,1996,1997,1999,2000,2001,2003,2004
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

# if !defined(AIPS_FITS)
# define AIPS_FITS

//# Note that aips.h has to come first for the correct definition of off_t.
# include <casacore/casa/aips.h>
# include <stdlib.h>
# include <ctype.h>
# include <casacore/casa/iostream.h>
# include <casacore/casa/BasicSL/Complex.h>
# include <casacore/casa/BasicSL/IComplex.h>
# include <casacore/fits/FITS/FITSError.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//# All FITS code seems to assume longs are 4 bytes. To take care of machines 
//# for which this isn't true use FitsLong instead of Long in the FITS code
//# where it matters.
# if (defined(__alpha) || defined(__sgi) || defined(__x86_64__))
    typedef Int FitsLong;
# else
    typedef Long FitsLong;
# endif 
//# recovered by GYL

//# Forward declarations
class ReservedFitsKeywordCollection;
class FitsNameResult;
class FitsValueResult;
class FitsKeyword;
class FitsParse;

//<summary> FITS templated helper class </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//<synopsis>
// NoConvert is a template class that is not intended for
// general use, it is used internally.
//</synopsis>

template <class TYPE>
class NoConvert {
    public:
	NoConvert() { }
	void operator = (int) {; }
};

//<summary> FITS helper class </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//<synopsis>
// FitsLogical is a helper class that is not intended for
// general use. 
//</synopsis>
//<example>
// Here is an example of the FitsLogical class. 
//<srcblock>
//	FitsLogical x;
//	FitsLogical y(True);
//	FitsLogical z = x;
//	...
//	x = y; y = False; x.undefine();
//	Bool b;
//	if (x.isdefined())
//		b = x;
//	b = y;  If y is undefined, b will be false.
//</srcblock>
//</example>
class FitsLogical {
	friend ostream & operator << (ostream &o, const FitsLogical &);
    public:
	FitsLogical() : v('\0') { }
	FitsLogical(Bool x) { v = (x == True ? 'T' : 'F'); }
	FitsLogical(const FitsLogical &x) : v(x.v) { }
	FitsLogical & operator = (const FitsLogical &x) { 
		v = x.v; return *this; }
	FitsLogical & operator = (Bool x) { 
		v = (x == True ? 'T' : 'F'); return *this; }
	Bool isdefined() const { return v == '\0' ? True : False; }
	void undefine() { v = '\0'; }
	operator Bool() { return (v == 'T' ? True : False); }
    protected:
	char v;
};

//<summary> helper class for FITS Binary Tables </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//<synopsis>
// This class is not intended for general use.  It only has meaning
// in the context of FITS Binary tables.  There its use is incorporated
// into the concept of a FitsField, where FitsBit is given a specialized
// interpretation.
//</synopsis>

class FitsBit {
    public:
	FitsBit() : bit_array(0) { }
	FitsBit(unsigned char x) : bit_array(x) { }
	FitsBit(const FitsBit &x) : bit_array(x.bit_array) { }
	FitsBit & operator = (const FitsBit &x) { 
		bit_array = x.bit_array; return *this; }
	FitsBit & operator = (unsigned char x) { bit_array = x; return *this; }
	operator unsigned char() { return bit_array; }
    protected:
	unsigned char bit_array;
};

//<summary>  Variable Length Array Descriptor </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

class FitsVADesc { 
	friend ostream & operator << (ostream &o, const FitsVADesc &);
    public:
	FitsVADesc() : no_elements(0), rel_offset(0) { }
	FitsVADesc(const FitsVADesc &x) :
		no_elements(x.no_elements), rel_offset(x.rel_offset) { }
	FitsVADesc & operator = (const FitsVADesc &x) {
		no_elements= x.no_elements;
		rel_offset = x.rel_offset; return *this; }
	FitsVADesc(int n, int o) : no_elements(n), rel_offset(o) { }
	void set(int n, int o) { no_elements = n; rel_offset = o; }
	int num() const 	{ return no_elements; }
	int offset() const 	{ return rel_offset; }
    protected:
	int no_elements;
	int rel_offset;
};

//<summary> static functions and enumerations </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//<synopsis>
// Many of the static functions are utility functions used internally in the
// implementation of the member functions of the FITS classes. They are placed
// in a single class to encapsulate them and to avoid adding many names to the 
// global name space. More important, from the user's perspective, are the
// enumerations. They form the basic vocabulary of a FITS application. For example,
// instead of referring to the FITS <src>NAXIS</src> keyword, 
// <src>FITS::NAXIS</src> should be used
//</synopsis>

class FITS {
    public:

    // FITS I/O Error message types

	// Basic FITS Data Types for keywords and data
	enum ValueType {
	    NOVALUE = 0, LOGICAL = 1, BIT = 2, CHAR = 3, BYTE = 4, 
	    SHORT = 5, LONG = 6, FLOAT = 7, DOUBLE = 8, COMPLEX = 9, 
	    ICOMPLEX = 10, DCOMPLEX = 11, VADESC = 12,
	    STRING, FSTRING, REAL
	}; // REAL means either FLOAT or DOUBLE
	   // STRING and FSTRING are used internally in parsing keywords

	static  FITS::ValueType getfitstype(NoConvert<FitsLogical> x) {
		x=0; return FITS::LOGICAL; }
	static  FITS::ValueType getfitstype(NoConvert<FitsBit> x) {
		x=0; return FITS::BIT; }
	static  FITS::ValueType getfitstype(NoConvert<char> x) {
		x=0; return FITS::CHAR; }
	static  FITS::ValueType getfitstype(NoConvert<unsigned char> x) {
		x=0; return FITS::BYTE; }
	static  FITS::ValueType getfitstype(NoConvert<short> x) {
		x=0; return FITS::SHORT; }
	static  FITS::ValueType getfitstype(NoConvert<Int> x) {
		x=0; return FITS::LONG; }
	static  FITS::ValueType getfitstype(NoConvert<long> x) {
		x=0; return FITS::LONG; }
	static  FITS::ValueType getfitstype(NoConvert<float> x) {
		x=0; return FITS::FLOAT; }
	static  FITS::ValueType getfitstype(NoConvert<double> x) {
		x=0; return FITS::DOUBLE; }
	static  FITS::ValueType getfitstype(NoConvert<Complex> x) {
		x=0; return FITS::COMPLEX; }
	static  FITS::ValueType getfitstype(NoConvert<IComplex> x) {
		x=0; return FITS::ICOMPLEX; }	
	static  FITS::ValueType getfitstype(NoConvert<DComplex> x) {
		x=0; return FITS::DCOMPLEX; }
	static  FITS::ValueType getfitstype(NoConvert<FitsVADesc> x) {
		x=0; return FITS::VADESC; }

	static  int fitssize(FITS::ValueType t);
	static  int localsize(FITS::ValueType t);

	// data conversion routines: FITS - local
	static void f2l(FitsLogical *,void *,int);
	static void l2f(void *,FitsLogical *,int);
	static void f2l(FitsBit *,void *,int);
	static void l2f(void *,FitsBit *,int);
	static void f2l(char *,void *,int);
	static void l2f(void *,char *,int);
	static void f2l(unsigned char *,void *,int);
	static void l2f(void *,unsigned char *,int);
	static void f2l(short *,void *,int);
	static void l2f(void *,short *,int);
	static void f2l(Int *,void *,int);
	static void l2f(void *,Int *,int);
	static void f2l(long *,void *,int);
	static void l2f(void *,long *,int);
	static void f2l(float *,void *,int);
	static void l2f(void *,float *,int);
	static void f2l(double *,void *,int);
	static void l2f(void *,double *,int);
	static void f2l(Complex *,void *,int);
	static void l2f(void *,Complex *,int);
	static void f2l(IComplex *,void *,int);
	static void l2f(void *,IComplex *,int);
	static void f2l(DComplex *,void *,int);
	static void l2f(void *,DComplex *,int);
	static void f2l(FitsVADesc *,void *,int);
	static void l2f(void *,FitsVADesc *,int);
        static void swap2(void *, void *, int);
        static void swap4(void *, void *, int);
        static void swap8(void *, void *, int);

	// FITS Reserved Names. PZERO is named strangely because it can conflict with
        // a standard #define in sys/param.h.
	enum ReservedName {
	    USER_DEF, AUTHOR,   BITPIX,   BLANK,    BLOCKED,  BSCALE,
	    BUNIT,    BZERO,    CDELT,    COMMENT,  CROTA,    CRPIX,
	    CRVAL,    CTYPE,    DATAMAX,  DATAMIN,  DATE,     DATE_OBS,
	    END,      EPOCH,    EQUINOX,  EXTEND,   EXTLEVEL, EXTNAME,
	    EXTVER,   GCOUNT,   GROUPS,   HISTORY,  INSTRUME, NAXIS,
	    OBJECT,   OBSERVER, ORIGIN,   PCOUNT,   PSCAL,    PTYPE,
	    PZERO_FITS,    REFERENC, SIMPLE,   SPACES,   TBCOL,    TDIM,
	    TDISP,    TELESCOP, TFIELDS,  TFORM,    THEAP,    TNULL,
	    TSCAL,    TTYPE,    TUNIT,    TZERO,    XTENSION, ERRWORD,
            ALTRPIX, DATE_MAP
	};

	// Types of FITS Records
	enum FitsRecType { 
	    InitialState, BadBeginningRecord, HDURecord,
	    UnrecognizableRecord, SpecialRecord, EndOfFile 
	};

	// Supported FITS Physical Devices
	enum FitsDevice { 
	    Disk, Std, Tape9
	};

	// Types of FITS Header-Data Units
	enum HDUType {
	    NotAHDU, PrimaryArrayHDU, PrimaryGroupHDU, AsciiTableHDU,
	    BinaryTableHDU, ImageExtensionHDU, UnknownExtensionHDU,
            PrimaryTableHDU
	};

	// Options on FITS array manipulations
	enum FitsArrayOption { NoOpt = 0, CtoF = 1, FtoC = 2};

	static ReservedFitsKeywordCollection &ResWord;
	static void valstr(ostream &o, const ValueType &ty, const void *val);
	static Bool isa_digit(char c);
	static int digit2bin(char c);
	static Bool isa_text(char c);
	static Bool isa_letter(char);
	static int letter2bin(char);
	static void fstr2str(char *, const char *, int);
	static int str2fstr(char *, const char *, int);
	static void get_name(const char *s, int len, FitsNameResult &result);
	static int get_value_id(const char *s, int l, int &pos);
	static void get_value(const char *s, int len, FitsValueResult &result);
	static int trim_comment(const char *s, int len);
	static int chk_comment(const char *s, int len);
	static int get_comment(const char *s, int len, int &begpos);
	static void get_numeric(const char *s, int len, FitsValueResult &result);
    // utility function to parse the binary table variable array
    // column (i.e. uses the heap) of the form nPt(dddd) where n
    // is either 0 or 1, t is one of the standard FITS binary table
    // column types and dddd is the maximum number of elements used
    // by this column.  If there is a format error in the input
    // string (*s), then valType will have the value NOVALUE and
    // maxelem will be -1.
        static void parse_vatform(const char *s, FITS::ValueType &valType,
				  int &maxelem);
	static const Int minInt;
	static const Int maxInt;
	static const float minfloat;
	static const float maxfloat;
	static const double mindouble;
	static const double maxdouble;

    private:
	FITS();
	static double tenpowerD[309];
	static float tenpowerF[39];
	static const int minfltexp;
	static const int maxfltexp;
	static const int mindblexp;
	static const int maxdblexp;
	static const int maxsigdigits;
	static const int maxdigl; // max digits in a long
	static const int maxexpdig; // max digits in an exponent
	static double tenD(Int, int);
	static float tenF(Int, int);
	static int ckaccum(double &, Int, int);
	static int ckaccum(float &, Int, int);
};

inline FITS::FITS() { } // just a dummy function to prevent instantiation
inline Bool FITS::isa_digit(char c) { return isdigit(c) ? True : False; }
inline int FITS::digit2bin(char c) { return c - '0'; }
inline Bool FITS::isa_text(char c) { return isprint(c) ? True : False; }
inline Bool FITS::isa_letter(char c) { return isupper(c) ? True : False; }
inline int FITS::letter2bin(char c) { return c - 'A'; }

ostream & operator << (ostream &, const FITS::ValueType &);

inline double FITS::tenD(Int numb, int pow) {
	return (pow > 0) ? (((double)numb) * tenpowerD[pow]) :
	    ((pow < 0) ? (((double)numb) / tenpowerD[-pow]) : ((double)numb));
}
inline float FITS::tenF(Int numb, int pow) {
	return (pow > 0) ? (((float)numb) * tenpowerF[pow]) :
	    ((pow < 0) ? (((float)numb) / tenpowerF[-pow]) : ((float)numb));
}

//<summary> reserved FITS keyword </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

class ReservedFitsKeyword {
    public:
	const char *aname() const;
	FITS::ReservedName name() const;
	int namesize() const;
	FITS::ValueType type() const;
	Bool isindexed() const;
	Bool isessential() const;
# if defined(TURBOCPP)
	// It is best for the following to be private, but 
	// C-Front won't allow an initializer list if they are private.
	// This issue isn't that crucial since functions in 
	// ReservedFitsKeywordCollection always return const items.
    private:
# endif
	FITS::ReservedName name_;
	const char *aname_;
	int namesize_;
	FITS::ValueType type_;
	Bool isindexed_; // 0 = NOT INDEXED, 1 = INDEXED
	Bool isessential_; // 0 = NO, 1 = YES
};

inline const char *ReservedFitsKeyword::aname() const { return aname_; }
inline int ReservedFitsKeyword::namesize() const { return namesize_; }
inline FITS::ValueType ReservedFitsKeyword::type() const { return type_; }
inline Bool ReservedFitsKeyword::isindexed() const { return isindexed_; }
inline Bool ReservedFitsKeyword::isessential() const { 
	return isessential_; }

//<summary> collection of reserved FITS keywords </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

class ReservedFitsKeywordCollection {
    public:
	const ReservedFitsKeyword & operator [] (int i) const;
	int no() const;
	const ReservedFitsKeyword &get(FITS::ReservedName, Bool, FITS::ValueType,
		const void *, int, const char *&) const;
	const ReservedFitsKeyword &get(const char *, int, Bool, FITS::ValueType,
		const void *, int, const char *&) const;
	const char *aname(FITS::ReservedName) const;
	int essential_name(const char *, int) const;
	const ReservedFitsKeyword &get_essential(int, Bool, FITS::ValueType,
		const void *, int, const char *&) const;
	int isreserved(const char *, int) const;
	Bool isunique(int) const;
	Bool requires_value(int) const;
	const ReservedFitsKeyword &userdef_item() const;
	const ReservedFitsKeyword &err_item() const;
	const ReservedFitsKeyword &end_item() const;
	const ReservedFitsKeyword &spaces() const;
	const ReservedFitsKeyword &comment() const;
	const ReservedFitsKeyword &history() const;
	int rules(const ReservedFitsKeyword &, const char *, int, Bool,
		FITS::ValueType, const void *, int, const char *&) const;
    private:
	static const int no_items; // number of entries in the table
	static const ReservedFitsKeyword &user_def_item; // user-defined keyword
	static const ReservedFitsKeyword &error_item; // error in keyword
	static const ReservedFitsKeyword &end__item;
	static const ReservedFitsKeyword &spaces_item;
	static const ReservedFitsKeyword &comment_item;
	static const ReservedFitsKeyword &history_item;
	static const ReservedFitsKeyword resword[]; // table of reserved words
	static const int resalpha[26]; // alphabetic index to table
	const ReservedFitsKeyword &match(int, const char *, int, Bool,
		FITS::ValueType, const void *, int, const char *&) const;

};

inline const ReservedFitsKeyword & ReservedFitsKeywordCollection::
	operator [] (int i) const { return resword[i]; }
inline int ReservedFitsKeywordCollection::no() const { return no_items; }
inline Bool ReservedFitsKeywordCollection::isunique(int i) const {
	return (Bool)(resword[i + 1].name() != resword[i].name()); }
inline const ReservedFitsKeyword &ReservedFitsKeywordCollection::userdef_item()
	const { return user_def_item; }
inline const ReservedFitsKeyword &ReservedFitsKeywordCollection::err_item() 
	const { return error_item; }
inline const ReservedFitsKeyword &ReservedFitsKeywordCollection::end_item()
	const { return end__item; }
inline const ReservedFitsKeyword &ReservedFitsKeywordCollection::spaces()
	const { return spaces_item; }
inline const ReservedFitsKeyword &ReservedFitsKeywordCollection::comment()
	const { return comment_item; }
inline const ReservedFitsKeyword &ReservedFitsKeywordCollection::history()
	const { return history_item; }

//<summary> analyse the name of a header card </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//<synopsis>
// Analyse the name of a header card
//</synopsis>

class FitsNameResult {
    public:
	Bool isaname;	// 1 if there is a name present, otherwise 0
	int begpos;	// beginning position of name
	int endpos;	// ending position of name
	Bool isaindex;	// whether an index is present or not
	int index;	// index if present
	int len;	// length of name without index
	enum ErrMsg { OK = 0, NO_0_NDX };
	ErrMsg err;
};

//<summary> analyse the value of a header card </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//<synopsis>
// Analyse the value of a header card
//</synopsis>

class FitsValueResult {
    public:
    	FITS::ValueType type;
	union {
    	    Bool b;
	    int s[2];	// for strings, s[0] is offset, s[1] length
    	    Int l;
    	    float f;
    	    double d;
	};
    	Complex c;
    	IComplex lc;
    	DComplex dc;
    	int begpos;		// beginning position of value
	int endpos;		// ending position of value
	Bool isa_point;		// 1 if a point, otherwise 0
	int pointpos;		// position of point, if any
    	int no_sig;		// number of significant digits
    	const char *errmsg;	// error message, if any
};

//<summary> parse a header card </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//<synopsis> 
//  parse a header card 
//</synopsis>

class FitsParse {
	friend class FitsKeywordList;
    public:
	FitsKeyword &parse(const char *, int); // Parsing one string
	int no_errs() const;
	const char *err(int) const;
    private:
	FitsParse(int = 10);
	~FitsParse();
	int no_errs_;
	const int max_errs;
	const char **err_;
	int seterr(const char *);
	FitsKeyword &mkerr(const char *s, int len);
};

inline FitsParse::~FitsParse() { delete [] err_; }
inline int FitsParse::no_errs() const { return no_errs_; }
inline const char *FitsParse::err(int i) const { return err_[i]; }
inline int FitsParse::seterr(const char *s) {
	return no_errs_ < max_errs ? ( err_[no_errs_++] = s, 0) : -1; }

//<summary> FITS keyword </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//<synopsis>
// A FITS keyword contains a name, a value and a comment.
//</synopsis>
class FitsKeyword {
	friend class FitsKeywordList;
	friend class FitsParse;
	// A word about friends:  FitsKeywordList accesses the next and prev
	// pointers and the FitsKeyword constructors. 
	// FitsParse only accesses the FitsKeyword constructors.

    public:

	FitsKeyword(const FitsKeyword &);
	FitsKeyword & operator = (const FitsKeyword &);
	~FitsKeyword();

	//<group>
	// get info about the name
	const char *name() const;
	int namelen() const;
	Bool isreserved() const;
	Bool isindexed() const;
	const ReservedFitsKeyword &kw() const;
	int index() const;
	//</group>

	//<group>
	// access the keyword comment
	const char *comm() const;
	int commlen() const;
	//</group>

	// access the error status
	int err() const;

	// the datatype of the keyword
	FITS::ValueType type() const;

	// access the value of the keyword
	//<group>
	Bool asBool() const;
        const char *asString() const;
	int valStrlen() const;
	Int asInt() const;
	float asFloat() const;
	double asDouble() const;
	IComplex asIComplex() const;
	Complex asComplex() const;
	DComplex asDComplex() const;
	const void *value() const;
	//</group>

	// change the value of the keyword
	//<group>
	FitsKeyword & operator = (Bool);
	FitsKeyword & operator = (const char *);
	FitsKeyword & operator = (Int);
	FitsKeyword & operator = (float);
	FitsKeyword & operator = (double);
	FitsKeyword & operator = (IComplex);
	FitsKeyword & operator = (Complex);
	FitsKeyword & operator = (DComplex);
	//</group>

	// change the comment of the keyword
	void comm(const char *);

	// change the name of the keyword
        void name(const char *);

    private:
	FitsKeyword *next_;
	FitsKeyword *prev_;

	//<group>
	// the keyword name
	// if name_ is 0, keyword is not a user defined name
	// if ndx is 0, there is no index
	char *name_;
	const ReservedFitsKeyword *kw_;
	int ndx;
	short namelen_;
	//</group>

	//<group>
	// the keyword comment
	// if comm_ is 0, there is no comment
	char *comm_;
	short commlen_;
	//</group>


	//<group>
	// the keyword value
	FITS::ValueType type_;
	union {
	    Bool bval;
	    Int ival;
	    float fval;
	    double dval;
	};
	void *val; // pointer to allocated value, if any
	short vallen; // only used for string data
	void del_val(); // does an appropriate delete based on type
	//</group>

	void init(const FitsKeyword &);
	void setval(const FITS::ValueType &, const void *, int);
        void setcomm(const char *, int);
	static void err(const char *, const FITS::ValueType &, const void *,
		 const char *);
	static void memchk(void *);

	//<group>
	// private constructors for use by friends

	// constructs user-defined keywords
        // parms: name, namelen, type, val, vallen, comm, commlen
	FitsKeyword(const char *, int , 
		FITS::ValueType, const void *, int, const char *, int);
	// constructs reserved keywords
        // parms: resword, index, val, vallen, comm, commlen
	FitsKeyword(const ReservedFitsKeyword *, int,
		FITS::ValueType, const void *, int, const char *, int);
	//</group>


};

ostream & operator << (ostream &, const FitsKeyword &);

inline FitsKeyword::FitsKeyword(const FitsKeyword &k) : next_(0), prev_(0),
	name_(0), kw_(0), comm_(0), val(0)  { init(k); }
inline FitsKeyword & FitsKeyword::operator = (const FitsKeyword &k) { 
    	delete [] name_; delete [] comm_; del_val(); init(k); return *this; }
inline FitsKeyword::~FitsKeyword() { 
	delete [] name_; 
	delete [] comm_; 
        del_val(); 
}

inline const ReservedFitsKeyword &FitsKeyword::kw() const { return *kw_; }
inline Bool FitsKeyword::isreserved() const { return 
	(kw().name() != FITS::ERRWORD && kw().name() != FITS::USER_DEF)
		 ? True : False; }
inline const char *FitsKeyword::name() const {
    	return isreserved() ? kw().aname() : (namelen_ ? name_ : ""); }
inline int FitsKeyword::namelen() const { return namelen_; }
inline Bool FitsKeyword::isindexed() const {return ndx > 0 ? True : False;}
inline int FitsKeyword::index() const { return ndx; }

inline const char *FitsKeyword::comm() const {
    return comm_ ? comm_  : ""; }
inline int FitsKeyword::commlen() const { return commlen_; }
inline int FitsKeyword::err() const { return (kw().name() == FITS::ERRWORD); }
inline FITS::ValueType FitsKeyword::type() const { return type_; }

inline Bool FitsKeyword::asBool() const { return bval; }
inline const char *FitsKeyword::asString() const {
	return vallen ? (const char *)val : ""; }
inline int FitsKeyword::valStrlen() const { return vallen; }
inline Int FitsKeyword::asInt() const { 
	if( type() != FITS::LONG ) {
		cerr << "Unexpected keyword type in FitsKeyword::asInt()\n";
		exit(1);
	}
	return ival;
}
inline float FitsKeyword::asFloat() const { 
	switch( type() ) { 
		case FITS::BYTE:
		case FITS::SHORT:
		case FITS::LONG: return (float)ival;
		case FITS::FLOAT: return fval;
		case FITS::DOUBLE: return (float)dval;
		default:
			cerr << "Unexpected keyword type in asFloat()\n";
			exit(1);
	}
	return 0.0;
}
inline double FitsKeyword::asDouble() const { 
	switch( type() ) { 
		case FITS::BYTE:
		case FITS::SHORT:
		case FITS::LONG: return (double)ival;
		case FITS::FLOAT: return (double)fval;
		case FITS::DOUBLE: return dval;
		default:
			cerr << "Unexpected keyword type in asDouble()\n";
			exit(1);
	}
	return 0.0;
}
inline IComplex FitsKeyword::asIComplex() const {
	return *((IComplex *)val); }
inline Complex FitsKeyword::asComplex() const {
	return *((Complex *)val); }
inline DComplex FitsKeyword::asDComplex() const {
	return *((DComplex *)val); }

inline FitsKeyword & FitsKeyword::operator = (Bool x) {
	bval = x; type_ = FITS::LOGICAL; return *this; }
inline FitsKeyword & FitsKeyword::operator = (Int x) {
	ival = x; type_ = FITS::LONG; return *this; }
inline FitsKeyword & FitsKeyword::operator = (float x) {
	fval = x; type_ = FITS::FLOAT; return *this; }
inline FitsKeyword & FitsKeyword::operator = (double x) {
	dval = x; type_ = FITS::DOUBLE; return *this; }
inline FitsKeyword & FitsKeyword::operator = (IComplex x) {
	*((IComplex *)val) = x; type_ = FITS::ICOMPLEX; return *this; }
inline FitsKeyword & FitsKeyword::operator = (Complex x) {
	*((Complex *)val) = x; type_ = FITS::COMPLEX; return *this; }
inline FitsKeyword & FitsKeyword::operator = (DComplex x) {
	*((DComplex *)val) = x; type_ = FITS::DCOMPLEX; return *this; }

class ConstFitsKeywordList; // forward declaration

//<summary> linked list of FITS keywords </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//<synopsis>
// A linked list of FITS keywords.
//</synopsis>

class FitsKeywordList {
    public:
	FitsKeywordList();
	~FitsKeywordList();
	FitsKeywordList(const FitsKeywordList &);
	FitsKeywordList(ConstFitsKeywordList &);
	FitsKeywordList & operator = (const FitsKeywordList &);

        // delete the current keyword (the thing returned by curr()) from the list
	void del();

	// Add (make) a reserved keyword with the given value and optional comment
        // The comment will be truncated if necessary to fit the available space.
        // String values must be less than 69 characters.  String values longer than
        // that will result in an ERROR keyword instead of the desired keyword.
	// <group>
	void mk(FITS::ReservedName k, Bool v, const char *c = 0);
	void mk(FITS::ReservedName k, const char *v = 0, const char *c = 0);
	void mk(FITS::ReservedName k, Int v, const char *c = 0);
	void mk(FITS::ReservedName k, long v, const char *c = 0);
	void mk(FITS::ReservedName k, double v, const char *c = 0);
        // </group>

        // Add (make) an indexed reserved keyword with the given value and optional comment
        // The comment will be truncated if necessary to fit the available space.
        // String values must be less than 69 characters.  String values longer than
        // that will result in an ERROR keyword instead of the desired keyword.
        // <group>
	void mk(int n, FITS::ReservedName k, Bool v, const char *c = 0);
	void mk(int n, FITS::ReservedName k, const char *v, const char *c = 0);
	void mk(int n, FITS::ReservedName k, Int v, const char *c = 0);
	void mk(int n, FITS::ReservedName k, long v, const char *c = 0);
	void mk(int n, FITS::ReservedName k, double v, const char *c = 0);
        // </group>

        // Add (make) a user defined keyword with the given name, value and optional comment.
        // The comment will be truncated if necessary to fit the available space.
        // The name must be no longer than 8 characters.  Names longer than that will 
        // result in an ERROR keyword instead of the desired keyword.
        // String values must no longer than 69 characters.  String values longer than
        // that will result in an ERROR keyword instead of the desired keyword.
        // <group>
	void mk(const char *n, Bool v, const char *c = 0);
	void mk(const char *n, const char *v = 0, const char *c = 0);
	void mk(const char *n, Int v, const char *c = 0);
	void mk(const char *n, long v, const char *c = 0);
	void mk(const char *n, float v, const char *c = 0);
	void mk(const char *n, double v, const char *c = 0);
	void mk(const char *n, Int r, Int i, const char *c = 0);
	void mk(const char *n, float r, float i, const char *c = 0);
	void mk(const char *n, double r, double i, const char *c = 0);
        // </group>

        // add a spaces line
	void spaces(const char *n = 0, const char *c = 0);

        // add a comment card
	void comment(const char *n = 0, const char *c = 0);

        // add a history card
	void history(const char *c = 0);

        // add the end card.  This must be at the end of the list.
	void end();
	
	// Retrieve specific keywords -- these also set the current mark
	//<group>
	// return the i-th keyword -- keyword numbering starts with 0
	FitsKeyword * operator () (int);
	// return first and next non-indexed reserved keyword
	FitsKeyword * operator () (const FITS::ReservedName &);
	FitsKeyword * next(const FITS::ReservedName &);
	// return first and next indexed reserved keyword
	FitsKeyword * operator () (const FITS::ReservedName &, int);
	FitsKeyword * next(const FITS::ReservedName &, int);
	// return first and next user-defined keyword
	FitsKeyword * operator () (const char *);
	FitsKeyword * next(const char *);
	//</group>

	//<group>
	Bool isempty() const;
	void     first();
	void     last();
	FitsKeyword *next();
	FitsKeyword *prev();
	FitsKeyword *curr();
	//</group>

	//<group>
	void delete_all(); 
	int rules(FitsKeyword &, 
		  FITSErrorHandler errhandler = FITSError::defaultHandler);
	int rules(FITSErrorHandler errhandler = FITSError::defaultHandler);
	Bool basic_rules();
	//</group>

	//<group>
	// For parsing a single string
	void parse(const char *, int); 
	int no_parse_errs() const;
	const char *parse_err(int) const;
	//</group>

	void insert(FitsKeyword &);
    private:
	FitsKeyword *beg_;
	FitsKeyword *end_;
	FitsKeyword *pos;
	int total;
	int cursor;
	FitsKeyword &make(const char *nm,
		FITS::ValueType t, const void *v, const char *c);
	FitsKeyword &make(FITS::ReservedName nm,
		FITS::ValueType t, const void *v, const char *c);
	FitsKeyword &make(int ind, FITS::ReservedName nm,
		FITS::ValueType t, const void *v, const char *c);
        // construct an error keyword - this happens when a name is invalid (NULL
        // or more than 8 characters) or a string value is too long (more than
        // 69 characters).  It is the responsibility of the caller to the 
        // several mk functions to ensure that that doesn't happen.  By the time
        // it gets here, it is assumed that such problems are true errors.
        // This is used by the private make functions.
        FitsKeyword &makeErrKeyword(const char *name, FITS::ValueType type, 
				    const void *val, const char *errmsg);
	FitsParse card;
};

ostream & operator << (ostream &o, FitsKeywordList &); // print the entire list

inline FitsKeywordList::FitsKeywordList() : beg_(0), end_(0), pos(0), 
	total(0), cursor(0) { }
inline FitsKeywordList::~FitsKeywordList() { delete_all(); }
inline Bool FitsKeywordList::isempty() const { return total == 0 ? True : False; }
inline void FitsKeywordList::first() { cursor = 0; pos = beg_; }
inline void FitsKeywordList::last() { cursor = total; pos = end_; }
inline FitsKeyword *FitsKeywordList::curr() { return pos; }
inline FitsKeyword *FitsKeywordList::operator () (const FITS::ReservedName &n) {
	first(); return next(n); }
inline FitsKeyword *FitsKeywordList::operator () (const FITS::ReservedName &n,
	int ndx) { first(); return next(n,ndx); }
inline FitsKeyword *FitsKeywordList::operator () (const char *w) {
	first(); return next(w); }
inline void FitsKeywordList::parse(const char *s, int l) {
	insert(card.parse(s,l)); }
inline int FitsKeywordList::no_parse_errs() const { return card.no_errs();}
inline const char *FitsKeywordList::parse_err(int n) const { 
	return card.err(n); }

// FitsKeyword constructors for non-indexed Reserved keywords
inline void FitsKeywordList::mk(FITS::ReservedName k, Bool v, const char *c) {
	insert(make(k,FITS::LOGICAL,&v,c)); }
inline void FitsKeywordList::mk(FITS::ReservedName k, const char *v, 
	const char *c) { insert(make(k,FITS::STRING,v,c)); }
inline void FitsKeywordList::mk(FITS::ReservedName k, Int v, const char *c) {
	insert(make(k,FITS::LONG,&v,c)); }
inline void FitsKeywordList::mk(FITS::ReservedName k, long v, const char *c) {
	insert(make(k,FITS::LONG,&v,c)); }
inline void FitsKeywordList::mk(FITS::ReservedName k, double v, const char *c) {
	insert(make(k,FITS::DOUBLE,&v,c)); }
// FitsKeyword constructors for indexed Reserved keywords
inline void FitsKeywordList::mk(int n, FITS::ReservedName k, Bool v, 
	const char *c) { 
	Bool tmp; tmp = v; insert(make(n,k,FITS::LOGICAL,&tmp,c)); }
inline void FitsKeywordList::mk(int n, FITS::ReservedName k, const char *v, 
	const char *c) { insert(make(n,k,FITS::STRING,v,c)); }
inline void FitsKeywordList::mk(int n, FITS::ReservedName k, Int v, 
	const char *c) { insert(make(n,k,FITS::LONG,&v,c)); }
inline void FitsKeywordList::mk(int n, FITS::ReservedName k, long v, 
	const char *c) { insert(make(n,k,FITS::LONG,&v,c)); }
inline void FitsKeywordList::mk(int n, FITS::ReservedName k, double v, 
	const char *c) { insert(make(n,k,FITS::DOUBLE,&v,c)); }
// FitsKeyword constructors for User-Defined keywords
inline void FitsKeywordList::mk(const char *n, Bool v, const char *c) {
	Bool tmp; tmp = v; insert(make(n,FITS::LOGICAL,&tmp,c)); }
inline void FitsKeywordList::mk(const char *n, const char *v, const char *c) {
	insert(make(n,FITS::STRING,v,c)); }
inline void FitsKeywordList::mk(const char *n, Int v, const char *c) {
	insert(make(n,FITS::LONG,&v,c)); }
inline void FitsKeywordList::mk(const char *n, long v, const char *c) {
	insert(make(n,FITS::LONG,&v,c)); }
inline void FitsKeywordList::mk(const char *n, float v, const char *c) {
	insert(make(n,FITS::FLOAT,&v,c)); }
inline void FitsKeywordList::mk(const char *n, double v, const char *c) {
	insert(make(n,FITS::DOUBLE,&v,c)); }
inline void FitsKeywordList::mk(const char *n, Int r, Int i, const char *c) {
	IComplex v(r,i);
	insert(make(n,FITS::ICOMPLEX,&v,c)); }
inline void FitsKeywordList::mk(const char *n, float r, float i, const char *c)
	{ Complex v(r,i); insert(make(n,FITS::COMPLEX,&v,c)); }
inline void FitsKeywordList::mk(const char *n, double r, double i, 
	const char *c) { DComplex v(r,i);
	insert(make(n,FITS::DCOMPLEX,&v,c)); }
// Additional keyword constructors for commentary, etc.
inline void FitsKeywordList::spaces(const char *n, const char *c) {
	insert((n == 0 ? make(FITS::SPACES,FITS::NOVALUE,0,c) :
	       (c == 0 ? make(FITS::SPACES,FITS::NOVALUE,0,n) : 
			  make(n,FITS::NOVALUE,0,c)))); }
inline void FitsKeywordList::comment(const char *n, const char *c) {
	insert((n == 0 ? make(FITS::COMMENT,FITS::NOVALUE,0,c) :
	       (c == 0 ? make(FITS::COMMENT,FITS::NOVALUE,0,n) :
			  make(n,FITS::NOVALUE,0,c)))); }
inline void FitsKeywordList::history(const char *c) {
	insert(make(FITS::HISTORY,FITS::NOVALUE,0,c)); }
inline void FitsKeywordList::end() {
	insert(make(FITS::END,FITS::NOVALUE,0,0)); }

//<summary> list of read-only FITS keywords </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>

class ConstFitsKeywordList {
    public:
	ConstFitsKeywordList(FitsKeywordList &x) : kw(x) { }

	const FitsKeyword * operator () (int n) { return kw(n); }
	const FitsKeyword * operator () (const FITS::ReservedName &x) {
        	return kw(x); }
	const FitsKeyword * next(const FITS::ReservedName &x) {
		return kw.next(x); }
	const FitsKeyword * operator () (const FITS::ReservedName &x, int n) {
        	return kw(x,n); }
	const FitsKeyword * next(const FITS::ReservedName &x, int n) {
        	return kw.next(x,n); }
	const FitsKeyword * operator () (const char *x) { return kw(x); }
	const FitsKeyword * next(const char *x) { return kw.next(x); }

	Bool isempty() const 		{ return kw.isempty(); }
	void     first() 		{ kw.first(); }
	void     last() 		{ kw.last(); }
	const FitsKeyword *next() 	{ return kw.next(); }
	const FitsKeyword *prev() 	{ return kw.prev(); }
	const FitsKeyword *curr() 	{ return kw.curr(); }

    private:
	FitsKeywordList &kw;
};

//<summary> translator between Keyword lists and fixed FITS cars </summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
//<synopsis> 
// also contains the parser ???
//</synopsis> 

class FitsKeyCardTranslator {
    public:
	FitsKeyCardTranslator(int = 100);
	~FitsKeyCardTranslator();
	FitsKeywordList & parse(const char *, 
		FitsKeywordList &, int, FITSErrorHandler, Bool);
	int build(char *, FitsKeywordList &);
	int no_errs() const;
	const char *err(int) const;
	int err_cardno(int) const;
    private:
	int cardno;		// the current card number within record
	const int FitsCardSize;
	const int FitsMaxCard;
	const int FitsRecSize;
	int max_errs;
	int no_errs_;
	const char **err_;
	int *err_cardno_;
	void fmtcard(char *, const FitsKeyword &);
	char *blanks;
};

inline FitsKeyCardTranslator::~FitsKeyCardTranslator() {
	delete [] err_; delete [] err_cardno_; delete [] blanks; }
inline int FitsKeyCardTranslator::no_errs() const { return no_errs_; }
inline const char *FitsKeyCardTranslator::err(int i) const { return err_[i]; }
inline int FitsKeyCardTranslator::err_cardno(int i) const {
	return err_cardno_[i]; }

// <summary>Utility functions for floating point values</summary>
// <reviewed reviewer="UNKNOWN" date="before2004/08/25" tests="" demos="">
// </reviewed>
class FitsFPUtil
{
public:
    // These functions are useful to tell if some type is a floating point type.
    // This is useful in a templated function, where the processing can vary
    // depending on whether the type is FP or not (e.g. blank handling).
    // <group>
    static Bool isFP(const float *);
    static Bool isFP(const double *);
    static Bool isFP(const void *);
    // </group>

    // For blanking purposes, we need to be able to get a NaN. The NaN we set
    // is all bits on.
    // <group>
    static void setNaN(double &val);
    static void setNaN(float &val);
    // </group>
};


} //# NAMESPACE CASACORE - END

# endif
