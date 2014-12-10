//# fits.cc:
//# Copyright (C) 1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003
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

//# Partial implementation of little endian code by Kris Huber
//# (kris@helios.ece.usu.edu)

# include <casacore/fits/FITS/fits.h>
# include <string.h>
# include <stdlib.h>
# include <casacore/casa/sstream.h>
# include <casacore/casa/BasicSL/Constants.h>


# include <stdio.h>
#include <assert.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//	Discussion of Reserved FitsKeyword Table
//
//	1. The reserved name itself (name_) is not unique;  there may 
//	   be more than one entry with the same reserved name.
//	2. The combination of reserved name, the data type, and whether
//	   it is indexed or not (name_, type_, isindexed_) is unique
//	   within the table.
//	3. The table is sorted by reserved name + type + isindexed.

// Initialize the static data in ReservedFitsKeywordCollection
const int ReservedFitsKeywordCollection::no_items = 56;
const ReservedFitsKeyword ReservedFitsKeywordCollection::resword[56] = {
//        key            aname      namesize         isindexed           Section
//         |               |           |  type           | isessential   in|NOST
//        \|/             \|/         \|/\|/            \|/     \|/      \|/
//         ------          ------      -  -------        -----  -----    ------------
/*  0 */ { FITS::USER_DEF, "",         0, FITS::NOVALUE, False, False },
/*  1 */ { FITS::AUTHOR,   "AUTHOR",   6, FITS::STRING,  False, False }, // 5.2.2.3
/*  2 */ { FITS::BITPIX,   "BITPIX",   6, FITS::LONG,    False, True },  // 5.2.1.1
/*  3 */ { FITS::BLANK,    "BLANK",    5, FITS::LONG,    False, False }, // 5.2.2.5
/*  4 */ { FITS::BLOCKED,  "BLOCKED",  7, FITS::LOGICAL, False, False }, // 5.2.2.1
/*  5 */ { FITS::BSCALE,   "BSCALE",   6, FITS::REAL,    False, False }, // 5.2.2.5
/*  6 */ { FITS::BUNIT,    "BUNIT",    5, FITS::STRING,  False, False }, // 5.2.2.5
/*  7 */ { FITS::BZERO,    "BZERO",    5, FITS::REAL,    False, False }, // 5.2.2.5
/*  8 */ { FITS::CDELT,    "CDELT",    5, FITS::REAL,    True, False },  // 5.2.2.5
/*  9 */ { FITS::COMMENT,  "COMMENT",  7, FITS::NOVALUE, False, False }, // 5.2.2.4
/* 10 */ { FITS::CROTA,    "CROTA",    5, FITS::REAL,    True, False },  // 5.2.2.5
/* 11 */ { FITS::CRPIX,    "CRPIX",    5, FITS::REAL,    True, False },  // 5.2.2.5
/* 12 */ { FITS::CRVAL,    "CRVAL",    5, FITS::REAL,    True, False },  // 5.2.2.5
/* 13 */ { FITS::CTYPE,    "CTYPE",    5, FITS::STRING,  True, False },  // 5.2.2.5
/* 14 */ { FITS::DATAMAX,  "DATAMAX",  7, FITS::REAL,    False, False }, // 5.2.2.5
/* 15 */ { FITS::DATAMIN,  "DATAMIN",  7, FITS::REAL,    False, False }, // 5.2.2.5
/* 16 */ { FITS::DATE,     "DATE",     4, FITS::STRING,  False, False }, // 5.2.2.1
/* 17 */ { FITS::DATE_OBS, "DATE-OBS", 8, FITS::STRING,  False, False }, // 5.2.2.2
/* 18 */ { FITS::END,      "END",      3, FITS::NOVALUE, False, True },  // 5.2.1.1
/* 19 */ { FITS::EPOCH,    "EPOCH",    5, FITS::REAL,    False, False }, // 5.2.2.2
/* 20 */ { FITS::EQUINOX,  "EQUINOX",  7, FITS::REAL,    False, False }, // 5.2.2.2
/* 21 */ { FITS::EXTEND,   "EXTEND",   6, FITS::LOGICAL, False, True },  // 5.2.1.2
/* 22 */ { FITS::EXTLEVEL, "EXTLEVEL", 8, FITS::LONG,    False, False }, // 5.2.2.6
/* 23 */ { FITS::EXTNAME,  "EXTNAME",  7, FITS::STRING,  False, False }, // 5.2.2.6
/* 24 */ { FITS::EXTVER,   "EXTVER",   6, FITS::LONG,    False, False }, // 5.2.2.6
/* 25 */ { FITS::GCOUNT,   "GCOUNT",   6, FITS::LONG,    False, True },  // 5.2.1.2
/* 26 */ { FITS::GROUPS,   "GROUPS",   6, FITS::LOGICAL, False, True },  // 7.1.1.6
/* 27 */ { FITS::HISTORY,  "HISTORY",  7, FITS::NOVALUE, False, False }, // 5.2.2.4
/* 28 */ { FITS::INSTRUME, "INSTRUME", 8, FITS::STRING,  False, False }, // 5.2.2.2
/* 29 */ { FITS::NAXIS,    "NAXIS",    5, FITS::LONG,    False, True },  // 5.2.1.1
/* 30 */ { FITS::NAXIS,    "NAXIS",    5, FITS::LONG,    True, True },   // 5.2.1.1
/* 31 */ { FITS::OBJECT,   "OBJECT",   6, FITS::STRING,  False, False }, // 5.2.2.2
/* 32 */ { FITS::OBSERVER, "OBSERVER", 8, FITS::STRING,  False, False }, // 5.2.2.2
/* 33 */ { FITS::ORIGIN,   "ORIGIN",   6, FITS::STRING,  False, False }, // 5.2.2.1
/* 34 */ { FITS::PCOUNT,   "PCOUNT",   6, FITS::LONG,    False, True },  // 5.2.1.2
/* 35 */ { FITS::PSCAL,    "PSCAL",    5, FITS::REAL,    True, False },  // 7.1.2.2
/* 36 */ { FITS::PTYPE,    "PTYPE",    5, FITS::STRING,  True, False },  // 7.1.2.1
/* 37 */ { FITS::PZERO_FITS,    "PZERO",    5, FITS::REAL,    True, False },  // 7.1.2.3
/* 38 */ { FITS::REFERENC, "REFERENC", 8, FITS::STRING,  False, False }, // 5.2.2.3
/* 39 */ { FITS::SIMPLE,   "SIMPLE",   6, FITS::LOGICAL, False, True },  // 5.2.1.1
/* 40 */ { FITS::SPACES,   "        ", 8, FITS::NOVALUE, False, False }, // 5.2.2.4
/* 41 */ { FITS::TBCOL,    "TBCOL",    5, FITS::LONG,    True, False },  // 8.1.1
/* 42 */ { FITS::TDIM,     "TDIM",     4, FITS::STRING,  True, False },  // A.4, A.9.1
/* 43 */ { FITS::TDISP,    "TDISP",    5, FITS::STRING,  True, False },  // A.4
/* 44 */ { FITS::TELESCOP, "TELESCOP", 8, FITS::STRING,  False, False }, // 5.2.2.2
/* 45 */ { FITS::TFIELDS,  "TFIELDS",  7, FITS::LONG,    False, False }, // 8.1.1
/* 46 */ { FITS::TFORM,    "TFORM",    5, FITS::STRING,  True, False },  // 8.1.1
/* 47 */ { FITS::THEAP,    "THEAP",    5, FITS::LONG,    False, False }, // A.4, A.9.2
/* 48 */ { FITS::TNULL,    "TNULL",    5, FITS::STRING,  True, False },  // 8.1.2
/* 49 */ { FITS::TNULL,    "TNULL",    5, FITS::LONG  ,  True, False },  // A.4
/* 50 */ { FITS::TSCAL,    "TSCAL",    5, FITS::REAL,    True, False },  // 8.1.2
/* 51 */ { FITS::TTYPE,    "TTYPE",    5, FITS::STRING,  True, False },  // 8.1.2
/* 52 */ { FITS::TUNIT,    "TUNIT",    5, FITS::STRING,  True, False },  // 8.1.2
/* 53 */ { FITS::TZERO,    "TZERO",    5, FITS::REAL,    True, False },  // 8.1.2
/* 54 */ { FITS::XTENSION, "XTENSION", 8, FITS::STRING,  False, True },  // 5.2.1.2
/* 55 */ { FITS::ERRWORD,  "",         0, FITS::NOVALUE, False, False }  // last
};
const ReservedFitsKeyword & ReservedFitsKeywordCollection::user_def_item = 
	resword[0];
const ReservedFitsKeyword & ReservedFitsKeywordCollection::error_item = 
	resword[55];
const ReservedFitsKeyword & ReservedFitsKeywordCollection::end__item = 
	resword[18];
const ReservedFitsKeyword & ReservedFitsKeywordCollection::spaces_item = 
	resword[40];
const ReservedFitsKeyword & ReservedFitsKeywordCollection::comment_item = 
	resword[9];
const ReservedFitsKeyword & ReservedFitsKeywordCollection::history_item = 
	resword[27];
const int ReservedFitsKeywordCollection::resalpha[26] = {
     // A  B  C   D   E  F   G   H   I  J  K  L  M   N   O   P  Q   R
	1, 2, 8, 14, 18, 0, 25, 27, 28, 0, 0, 0, 0, 29, 31, 34, 0, 38,
     //  S   T  U  V  W   X  Y  Z
	39, 41, 0, 0, 0, 54, 0, 0
};

// Initialize the static data in FITS
double FITS::tenpowerD[309] = { 1.0,
	  1.0E1,   1.0E2,   1.0E3,   1.0E4,   1.0E5,   1.0E6,   1.0E7,   1.0E8,
	  1.0E9,  1.0E10,  1.0E11,  1.0E12,  1.0E13,  1.0E14,  1.0E15,  1.0E16,
	 1.0E17,  1.0E18,  1.0E19,  1.0E20,  1.0E21,  1.0E22,  1.0E23,  1.0E24,
	 1.0E25,  1.0E26,  1.0E27,  1.0E28,  1.0E29,  1.0E30,  1.0E31,  1.0E32,
	 1.0E33,  1.0E34,  1.0E35,  1.0E36,  1.0E37,  1.0E38,  1.0E39,  1.0E40,
	 1.0E41,  1.0E42,  1.0E43,  1.0E44,  1.0E45,  1.0E46,  1.0E47,  1.0E48,
	 1.0E49,  1.0E50,  1.0E51,  1.0E52,  1.0E53,  1.0E54,  1.0E55,  1.0E56,
	 1.0E57,  1.0E58,  1.0E59,  1.0E60,  1.0E61,  1.0E62,  1.0E63,  1.0E64,
	 1.0E65,  1.0E66,  1.0E67,  1.0E68,  1.0E69,  1.0E70,  1.0E71,  1.0E72,
	 1.0E73,  1.0E74,  1.0E75,  1.0E76,  1.0E77,  1.0E78,  1.0E79,  1.0E80,
	 1.0E81,  1.0E82,  1.0E83,  1.0E84,  1.0E85,  1.0E86,  1.0E87,  1.0E88,
	 1.0E89,  1.0E90,  1.0E91,  1.0E92,  1.0E93,  1.0E94,  1.0E95,  1.0E96,
	 1.0E97,  1.0E98,  1.0E99, 1.0E100, 1.0E101, 1.0E102, 1.0E103, 1.0E104,
	1.0E105, 1.0E106, 1.0E107, 1.0E108, 1.0E109, 1.0E110, 1.0E111, 1.0E112,
	1.0E113, 1.0E114, 1.0E115, 1.0E116, 1.0E117, 1.0E118, 1.0E119, 1.0E120,
	1.0E121, 1.0E122, 1.0E123, 1.0E124, 1.0E125, 1.0E126, 1.0E127, 1.0E128, 
	1.0E129, 1.0E130, 1.0E131, 1.0E132, 1.0E133, 1.0E134, 1.0E135, 1.0E136,
	1.0E137, 1.0E138, 1.0E139, 1.0E140, 1.0E141, 1.0E142, 1.0E143, 1.0E144,
	1.0E145, 1.0E146, 1.0E147, 1.0E148, 1.0E149, 1.0E150, 1.0E151, 1.0E152, 
	1.0E153, 1.0E154, 1.0E155, 1.0E156, 1.0E157, 1.0E158, 1.0E159, 1.0E160, 
	1.0E161, 1.0E162, 1.0E163, 1.0E164, 1.0E165, 1.0E166, 1.0E167, 1.0E168,
	1.0E169, 1.0E170, 1.0E171, 1.0E172, 1.0E173, 1.0E174, 1.0E175, 1.0E176, 
	1.0E177, 1.0E178, 1.0E179, 1.0E180, 1.0E181, 1.0E182, 1.0E183, 1.0E184, 
	1.0E185, 1.0E186, 1.0E187, 1.0E188, 1.0E189, 1.0E190, 1.0E191, 1.0E192,
	1.0E193, 1.0E194, 1.0E195, 1.0E196, 1.0E197, 1.0E198, 1.0E199, 1.0E200,
	1.0E201, 1.0E202, 1.0E203, 1.0E204, 1.0E205, 1.0E206, 1.0E207, 1.0E208, 
	1.0E209, 1.0E210, 1.0E211, 1.0E212, 1.0E213, 1.0E214, 1.0E215, 1.0E216,
	1.0E217, 1.0E218, 1.0E219, 1.0E220, 1.0E221, 1.0E222, 1.0E223, 1.0E224,
	1.0E225, 1.0E226, 1.0E227, 1.0E228, 1.0E229, 1.0E230, 1.0E231, 1.0E232,
	1.0E233, 1.0E234, 1.0E235, 1.0E236, 1.0E237, 1.0E238, 1.0E239, 1.0E240,
	1.0E241, 1.0E242, 1.0E243, 1.0E244, 1.0E245, 1.0E246, 1.0E247, 1.0E248, 
	1.0E249, 1.0E250, 1.0E251, 1.0E252, 1.0E253, 1.0E254, 1.0E255, 1.0E256, 
	1.0E257, 1.0E258, 1.0E259, 1.0E260, 1.0E261, 1.0E262, 1.0E263, 1.0E264,
	1.0E265, 1.0E266, 1.0E267, 1.0E268, 1.0E269, 1.0E270, 1.0E271, 1.0E272,
	1.0E273, 1.0E274, 1.0E275, 1.0E276, 1.0E277, 1.0E278, 1.0E279, 1.0E280,
	1.0E281, 1.0E282, 1.0E283, 1.0E284, 1.0E285, 1.0E286, 1.0E287, 1.0E288, 
	1.0E289, 1.0E290, 1.0E291, 1.0E292, 1.0E293, 1.0E294, 1.0E295, 1.0E296,
	1.0E297, 1.0E298, 1.0E299, 1.0E300, 1.0E301, 1.0E302, 1.0E303, 1.0E304,
	1.0E305, 1.0E306, 1.0E307, 1.0E308 };
float FITS::tenpowerF[39] = { 1.0F,  
	  1.0E1F,  1.0E2F,  1.0E3F,  1.0E4F,  1.0E5F,  1.0E6F,  1.0E7F,  1.0E8F,
	  1.0E9F, 1.0E10F, 1.0E11F, 1.0E12F, 1.0E13F, 1.0E14F, 1.0E15F, 1.0E16F,
	 1.0E17F, 1.0E18F, 1.0E19F, 1.0E20F, 1.0E21F, 1.0E22F, 1.0E23F, 1.0E24F,
	 1.0E25F, 1.0E26F, 1.0E27F, 1.0E28F, 1.0E29F, 1.0E30F, 1.0E31F, 1.0E32F,
	 1.0E33F, 1.0E34F, 1.0E35F, 1.0E36F, 1.0E37F, 1.0E38F };
const int FITS::minfltexp = -38;
const int FITS::maxfltexp = 38;
const int FITS::mindblexp = -308;
const int FITS::maxdblexp = 308;
const int FITS::maxsigdigits = 17;
const int FITS::maxdigl = 9; // max digits in a long
const int FITS::maxexpdig = 3; // max digits in an exponent
const Int FITS::minInt = INT_MIN;
const Int FITS::maxInt = INT_MAX;
#   if defined(GNU)
const float FITS::minfloat = C::minfloat;
const float FITS::maxfloat = C::maxfloat;
const double FITS::mindouble = C::mindouble;
const double FITS::maxdouble = C::maxdouble;
#   else
const float FITS::minfloat = C::flt_min;
const float FITS::maxfloat = C::flt_max;
const double FITS::mindouble = C::dbl_min;
const double FITS::maxdouble = C::dbl_max;
#   endif

ostream & operator << (ostream &o, const FitsLogical &x) {
	if (x.v == 'T') o << "True"; 
	else if (x.v == 'F') o << "False";
	else o << "Undefined"; 
	return o; 
}

ostream & operator << (ostream &o, const FitsVADesc &x) {
	o << "[" << x.num() << "," << x.offset() << "]"; 
	return o; 
}

int FITS::fitssize(FITS::ValueType t) {
				    // 0 1 2 3 4 5 6 7 8 9 10 11 12
	static int FitsDataSize[13] = {0,1,1,1,1,2,4,4,8,8,8, 16, 8};
	return FitsDataSize[t];
}

int FITS::localsize(FITS::ValueType t) {
	static int LocalDataSize[13] = {
		0,			// 0
		sizeof(FitsLogical), 	// 1
		sizeof(FitsBit), 	// 2
		sizeof(char), 		// 3
		sizeof(uChar),   	// 4
		sizeof(short), 		// 5
		sizeof(FitsLong),	// 6
		sizeof(float), 		// 7
		sizeof(double), 	// 8
		sizeof(Complex), 	// 9
		sizeof(IComplex), 	// 10
		sizeof(DComplex), 	// 11
		sizeof(FitsVADesc) 	// 12
	};
	return LocalDataSize[t];
}

// Data conversion:  FitsLogical
void FITS::f2l(FitsLogical *local_addr, void *fits_addr, int number) {
	memcpy(local_addr,fits_addr,(number * sizeof(FitsLogical))); }
void FITS::l2f(void *fits_addr, FitsLogical *local_addr, int number) {
	memcpy(fits_addr,local_addr,(number * sizeof(FitsLogical))); }

// Data conversion:  FitsBit
void FITS::f2l(FitsBit *local_addr, void *fits_addr, int number) {
	int n = number / 8;
	if (number % 8 != 0)
	    ++n;
	memcpy(local_addr,fits_addr,(n * sizeof(uChar))); }
void FITS::l2f(void *fits_addr, FitsBit *local_addr, int number) {
	int n = number / 8;
	if (number % 8 != 0)
	    ++n;
	memcpy(fits_addr,local_addr,(n * sizeof(uChar))); }

// Data conversion:  char
void FITS::f2l(char *local_addr, void *fits_addr, int number) {
	memcpy(local_addr,fits_addr,(number * sizeof(char))); }
void FITS::l2f(void *fits_addr, char *local_addr, int number) {
	memcpy(fits_addr,local_addr,(number * sizeof(char))); }

// Data conversion:  uChar
void FITS::f2l(uChar *local_addr, void *fits_addr, int number) {
	memcpy(local_addr,fits_addr,(number * sizeof(uChar))); }
void FITS::l2f(void *fits_addr, uChar *local_addr, int number) {
	memcpy(fits_addr,local_addr,(number * sizeof(uChar))); }

// Data conversion:  short
void FITS::f2l(short *local_addr, void *fits_addr, int number) {
# if defined(AIPS_LITTLE_ENDIAN)
        FITS::swap2(local_addr, fits_addr, number);
# else
	memcpy(local_addr,fits_addr,(number * sizeof(short)));
# endif
}
void FITS::l2f(void *fits_addr, short *local_addr,  int number) {
# if defined(AIPS_LITTLE_ENDIAN)
	FITS::swap2(fits_addr, local_addr, number);
# else
	memcpy(fits_addr,local_addr,(number * sizeof(short)));
# endif
}

// Data conversion:  long
void FITS::f2l(long *local_addr, void *fits_addr, int number) {
# if defined(AIPS_LITTLE_ENDIAN)
        switch(sizeof(long)) {
          case 4: FITS::swap4(local_addr, fits_addr, number); break;
          case 8: FITS::swap8(local_addr, fits_addr, number); break;
        }
# else
	memcpy(local_addr,fits_addr,(number * sizeof(long)));
# endif
}
void FITS::l2f(void *fits_addr, long *local_addr, int number) {
	FITS::f2l( (long *)fits_addr, local_addr, number );
}

// Data conversion:  Int
void FITS::f2l(Int *local_addr, void *fits_addr, int number) {
# if defined(AIPS_LITTLE_ENDIAN)
        switch(sizeof(Int)) {
          case 2: FITS::swap2(local_addr, fits_addr, number); break;
          case 4: FITS::swap4(local_addr, fits_addr, number); break;
        }
# else
	memcpy(local_addr,fits_addr,(number * sizeof(Int)));
# endif
}
void FITS::l2f(void *fits_addr, Int *local_addr, int number) {
	FITS::f2l((Int *) fits_addr, local_addr, number);
}

// Data conversion:  float
void FITS::f2l(float *local_addr, void *fits_addr, int number) {
# if defined(AIPS_LITTLE_ENDIAN)
        swap4(local_addr, fits_addr, number);
# else
	memcpy(local_addr,fits_addr,(number * sizeof(float)));
# endif
}
void FITS::l2f(void *fits_addr, float *local_addr, int number) {
# if defined(AIPS_LITTLE_ENDIAN)
        swap4(fits_addr, local_addr, number);
# else
	memcpy(fits_addr,local_addr,(number * sizeof(float)));
# endif
}

// Data conversion:  double
void FITS::f2l(double *local_addr, void *fits_addr, int number) {
# if defined(AIPS_LITTLE_ENDIAN)
        swap8(local_addr, fits_addr, number);
# else
	memcpy(local_addr,fits_addr,(number * sizeof(double)));
# endif
}
void FITS::l2f(void *fits_addr, double *local_addr, int number) {
# if defined(AIPS_LITTLE_ENDIAN)
        swap8(fits_addr, local_addr, number);
# else
	memcpy(fits_addr,local_addr,(number * sizeof(double)));
# endif
}

// Data conversion:  Complex
void FITS::f2l(Complex *local_addr, void *fits_addr, int number) {
# if defined(AIPS_LITTLE_ENDIAN)
	FITS::f2l( (float *)local_addr, fits_addr, 2*number );
# else
	memcpy(local_addr,fits_addr,(number * sizeof(Complex)));
# endif
}
void FITS::l2f(void *fits_addr, Complex *local_addr, int number) {
# if defined(AIPS_LITTLE_ENDIAN)
	FITS::f2l( (float *)fits_addr, local_addr, 2*number );
# else
	memcpy(fits_addr,local_addr,(number * sizeof(Complex)));
# endif
}

// Data conversion:  IComplex
void FITS::f2l(IComplex *local_addr, void *fits_addr, int number) {
# if defined(AIPS_LITTLE_ENDIAN)
	FITS::f2l( (Int *)local_addr, fits_addr, 2*number );
# else
	memcpy(local_addr,fits_addr,(number * sizeof(IComplex)));
# endif
}
void FITS::l2f(void *fits_addr, IComplex *local_addr, int number) {
# if defined(AIPS_LITTLE_ENDIAN)
	FITS::f2l( (Int *)fits_addr, local_addr, 2*number );
# else
	memcpy(fits_addr,local_addr,(number * sizeof(IComplex)));
# endif
}

// Data conversion:  DComplex
void FITS::f2l(DComplex *local_addr, void *fits_addr, int number) {
# if defined(AIPS_LITTLE_ENDIAN)
	FITS::f2l( (double *)local_addr, fits_addr, 2*number );
# else
	memcpy(local_addr,fits_addr,(number * sizeof(DComplex)));
# endif
}
void FITS::l2f(void *fits_addr, DComplex *local_addr, int number) {
# if defined(AIPS_LITTLE_ENDIAN)
	FITS::f2l( (double *)fits_addr, local_addr, 2*number );
# else
	memcpy(fits_addr,local_addr,(number * sizeof(DComplex)));
# endif
}

// Data conversion:  FitsVADesc
void FITS::f2l(FitsVADesc *local_addr, void *fits_addr, int number) {
# if defined(AIPS_LITTLE_ENDIAN)
	FITS::f2l( (Int *)local_addr, fits_addr, 2*number );
# else
	memcpy(local_addr,fits_addr,(number * sizeof(FitsVADesc)));
# endif
}
void FITS::l2f(void *fits_addr, FitsVADesc *local_addr, int number) {
# if defined(AIPS_LITTLE_ENDIAN)
	FITS::f2l( (Int *)fits_addr, local_addr, 2*number );
# else
	memcpy(fits_addr,local_addr,(number * sizeof(FitsVADesc)));
# endif
}

// Swap routines for 2, 4 and 8 byte items
void FITS::swap2(void *dest, void *src, int number) {
  uChar *t = (uChar *)dest;
  uChar *s = (uChar *)src;
  if( t == s )
    for (int i = 0; i < number; ++i, t += 2) {
      uChar tmp;
      tmp = *t; *t = t[1]; t[1] = tmp;
    }
  else
    for (int i = 0; i < number; ++i, t += 2) {
      t[1] = *s++; *t = *s++; }
}

void FITS::swap4(void *dest, void *src, int number) {
  uChar *t = (uChar *)dest;
  uChar *s = (uChar *)src;
  if( t == s ) {
    for (int i = 0; i < number; ++i, t += 4) {
      uChar tmp;
      tmp = *t;     *t = t[3]; t[3] = tmp;
      tmp = t[1]; t[1] = t[2]; t[2] = tmp;
    }
  }
  else
    for (int i = 0; i < number; ++i, t += 4) {
      t[3] = *s++; t[2] = *s++; t[1] = *s++; *t = *s++; }
}

void FITS::swap8(void *dest, void *src, int number) {
  uChar *t = (uChar *)dest;
  uChar *s = (uChar *)src;
  if( t == s ) {
    for (int i = 0; i < number; ++i, t += 8) {
      uChar tmp;
      tmp = *t;     *t = t[7]; t[7] = tmp;
      tmp = t[1]; t[1] = t[6]; t[6] = tmp;
      tmp = t[2]; t[2] = t[5]; t[5] = tmp;
      tmp = t[3]; t[3] = t[4]; t[4] = tmp;
    }
  } else {
    for (int i = 0; i < number; ++i, t += 8) {
      t[7] = *s++; t[6] = *s++; t[5] = *s++; t[4] = *s++;
      t[3] = *s++; t[2] = *s++; t[1] = *s++; *t = *s++; }
  }
}

void FITS::valstr(ostream &o, const ValueType &ty, const void *val) {
	int n;
	Complex x; 
	DComplex y; 
	IComplex z;
	if (!val)
	    return;
	switch (ty) {
	    case FITS::NOVALUE: break;
	    case FITS::LOGICAL: 
		o << ((*((Bool *)val) == True) ? "True" : "False"); break;
	    case FITS::BIT: o << "*****"; break;
	    case FITS::CHAR: o << *((char *)val); break;
	    case FITS::BYTE: n = *((uChar *)val); o << n; break;
	    case FITS::SHORT: o << *((short *)val); break;
	    case FITS::LONG: o << *((FitsLong *)val); break;
	    case FITS::STRING: o << "\'" << (char *)val << "\'"; break;
	    case FITS::FLOAT: o << *((float *)val); break;
	    case FITS::DOUBLE: o << *((double *)val); break;
	    case FITS::COMPLEX: x = *(Complex *)val;
		o << "(" << x.real() << "," << x.imag() << ")"; break;
	    case FITS::DCOMPLEX: y = *(DComplex *)val;
		o << "(" << y.real() << "," << y.imag() << ")"; break;
	    case FITS::ICOMPLEX: z = *(IComplex *)val;
		o << "(" << z.real() << "," << z.imag() << ")"; break;
	    default: o << "*****"; break;
	}
}
ostream & operator << (ostream &o, const FITS::ValueType &ty) {
	switch (ty) {
	    case FITS::NOVALUE: break;
	    case FITS::LOGICAL: o << " LOGICAL "; break;
	    case FITS::BIT: o << " BIT "; break;
	    case FITS::CHAR: o << " CHAR "; break;
	    case FITS::BYTE: o << " BYTE "; break;
	    case FITS::SHORT: o << " SHORT "; break;
	    case FITS::LONG: o << " LONG "; break;
	    case FITS::FLOAT: o << " FLOAT "; break;
	    case FITS::DOUBLE: o << " DOUBLE "; break;
	    case FITS::COMPLEX: o << " COMPLEX "; break;
	    case FITS::ICOMPLEX: o << " ICOMPLEX "; break;
	    case FITS::DCOMPLEX: o << " DCOMPLEX "; break;
	    case FITS::VADESC: o << " VADESC "; break;
	    case FITS::STRING: o << " STRING "; break;
	    case FITS::FSTRING: o << " FSTRING "; break;
	    case FITS::REAL: o << " REAL "; break;
	    default: o << " ILLEGAL "; break;
	}
	return o;
}

const ReservedFitsKeyword & ReservedFitsKeywordCollection::match(int i, 
	const char *s, int s_len, Bool n, FITS::ValueType t, const void *v,
	int v_len, const char *&msg) const {

	if (t == FITS::FLOAT || t == FITS::DOUBLE)
	    t = FITS::REAL;	// change t to REAL to match on types
	if (t == FITS::FSTRING)
	    t = FITS::STRING;   // change t to STRING to match on types
	// match type and isindexed
	if (resword[i].type() != t) {
	    while (resword[i + 1].name() == resword[i].name()) {
		++i;
		if (resword[i].type() == t)
		    break;
	    }
	    if (resword[i].type() != t) {
		msg = "Keyword value has wrong data type.";
		return error_item;
	    }
	}
	if (resword[i].isindexed() != n) {
	    while ((resword[i + 1].name() == resword[i].name()) &&
		   (resword[i + 1].type() == resword[i].type())) {
		    ++i;
		    if (resword[i].isindexed() == n)
			break;
	    }
	    if (resword[i].isindexed() != n) {
	      if (resword[i].isindexed()){
		  msg = "Keyword requires an index.";
		  return error_item;
	      }
	      else {
		  msg = "Keyword should not have an index.";
		  return user_def_item; // treat as non-reserved keyword
	      }
	    }
	}
	return rules(resword[i],s,s_len,n,t,v,v_len,msg) == -1 ?
	    error_item : resword[i];
}

const ReservedFitsKeyword & ReservedFitsKeywordCollection::get(
	FITS::ReservedName nm, Bool n, FITS::ValueType t, const void *v,
	int v_len, const char *&msg) const {
	int i;
	msg = 0;
	for (i = 0; i < no_items; ++i)
	    if (resword[i].name() == nm)
		break;
	return match(i,0,0,n,t,v,v_len,msg);
}

const ReservedFitsKeyword & ReservedFitsKeywordCollection::get(const char *s,
	int s_len, Bool n, FITS::ValueType t, const void *v, int v_len,
	const char *&msg) const {
	msg = 0;
	int i; // The index into the table.
	if (!FITS::isa_letter(*s))
	    return rules(user_def_item,s,s_len,n,t,v,v_len,msg) == -1 ?
	       error_item : user_def_item;
	if ((i = resalpha[FITS::letter2bin(*s)]) == 0)
	    return rules(user_def_item,s,s_len,n,t,v,v_len,msg) == -1 ?
	       error_item : user_def_item;
	for(; *(resword[i].aname()) == *s; ++i) // search for a match
	    if (resword[i].namesize() == s_len &&
		strncmp(s,resword[i].aname(),s_len) == 0)
		    break;
	if (*(resword[i].aname()) != *s || resword[i].namesize() != s_len)
	    return rules(user_def_item,s,s_len,n,t,v,v_len,msg) == -1 ?
	       error_item : user_def_item;
	return match(i,s,s_len,n,t,v,v_len,msg);
}

int ReservedFitsKeywordCollection::isreserved(const char *s, int s_len) const {
	int i; // The index into the table.
	// If this name MIGHT be a reserved name, this routine returns
	// an index into the reserved word table, otherwise 0 is returned.
	if (!FITS::isa_letter(*s))
	    return 0;
	if ((i = resalpha[FITS::letter2bin(*s)]) == 0)
	    return 0;
	for(; *(resword[i].aname()) == *s; ++i) // search for a match
	    if (resword[i].namesize() == s_len &&
		strncmp(s,resword[i].aname(),s_len) == 0)
		    break;
	if (*(resword[i].aname()) != *s || resword[i].namesize() != s_len)
	    return 0;
	return i;
}

Bool ReservedFitsKeywordCollection::requires_value(int n) const {
	if (resword[n].type() == FITS::NOVALUE)
	    return False;
	while (resword[n].name() == resword[n + 1].name()) {
	    ++n;
	    if (resword[n].type() == FITS::NOVALUE)
	        return False;
	}
	return True;
}

int ReservedFitsKeywordCollection::rules(const ReservedFitsKeyword &res,
	const char *s, 	int s_len, Bool n, FITS::ValueType, const void *v,
	int v_len, const char *&msg) const {
	// Return: 0 = no errors, 1 = minor errors, -1 = major errors
	static int month[13] = {0,31,28,31,30,31,30,31,31,30,31,30,31};
	int dd, mm, yy, mmdays;
	const char *p;
	// These are context independent rules.
	if (res.name() == FITS::USER_DEF) {
	    for (int i = 0; i < s_len; ++i, ++s) {
		if (!(FITS::isa_letter(*s) || FITS::isa_digit(*s) ||
			*s == '_' || *s == '-')) {
		    msg = "Illegal characters in keyword name.";
		    return 1;
		}
	    }
	    return 0;
	}

	// The name, isindexed, and type match an entry in the table.
	const Int *l;
	switch (res.name()) {
	    case FITS::BITPIX:
		l = (const Int *)v;
		if (!(*l ==   8 || *l ==  16 || *l == 32 ||
		      *l == -32 || *l == -64)) {
		    msg = "Illegal value for keyword BITPIX.";
		    return -1;
		}
		break;
	    case FITS::NAXIS:
		if (n == False) {
		    l = (const Int *)v;
		    if (*l < 0 || *l > 999) {
			msg = "Illegal value for keyword NAXIS.";
			return -1;
		    }
		}
		break;
	    case FITS::TFIELDS:
		l = (const Int *)v;
		if (*l < 0 || *l > 999) {
		    msg = "Illegal value for keyword TFIELDS.";
		    return -1;
		}
		break;
	    case FITS::DATE: // s must be of the form `DD/MM/YY' or 'YYYY-MM-DD[THH:MM:SS[.sss]]
	    case FITS::DATE_OBS:
		if (v_len < 8) {
		    msg = "Illegal date format.";
		    return 1;
		}
		p = (const char *)v;
		// first, is this the new format
 		if ((v_len >= 10) &&
 		    FITS::isa_digit(p[0]) && FITS::isa_digit(p[1]) &&
 		    FITS::isa_digit(p[2]) && FITS::isa_digit(p[3]) &&
 		    FITS::isa_digit(p[5]) && FITS::isa_digit(p[6]) &&
 		    FITS::isa_digit(p[8]) && FITS::isa_digit(p[9]) &&
 		    (p[4] == '-') && (p[7] == '-')) {
		    // must be an attempt at the new format
		    yy = FITS::digit2bin(p[0]) * 1000 + 
			FITS::digit2bin(p[1]) * 100 +
			FITS::digit2bin(p[2]) * 10 + FITS::digit2bin(p[3]);
		    mm = FITS::digit2bin(p[5]) * 10 + FITS::digit2bin(p[6]);
		    dd = FITS::digit2bin(p[8]) * 10 + FITS::digit2bin(p[9]);
		    if (v_len > 10) {
			// if there are more than 10 character, the time must
			// be fully there up to an optional decimal point
			if (v_len >= 19 &&
			    FITS::isa_digit(p[11]) && FITS::isa_digit(p[12]) &&
			    FITS::isa_digit(p[14]) && FITS::isa_digit(p[15]) &&
			    FITS::isa_digit(p[17]) && FITS::isa_digit(p[18]) &&
			    (p[10] == 'T') && (p[13] == ':') && 
			    (p[16] == ':')) {
			    // 24 is okay, more than that is not in hours field
			    if ((FITS::digit2bin(p[11]) == 2 &&
				 FITS::digit2bin(p[12]) > 4) ||
				FITS::digit2bin(p[11]) > 2) {
				msg = "Illegal time.";
				return 1;
			    }
			    // 60 is okay, more than that is not in minutes field
			    if ((FITS::digit2bin(p[14]) == 6 &&
				 FITS::digit2bin(p[15]) > 0) ||
				FITS::digit2bin(p[14]) > 6) {
				msg = "Illegal time.";
			    }
			    // 60 is okay, more than that is not in secomds field
			    if ((FITS::digit2bin(p[17]) == 6 &&
				 FITS::digit2bin(p[18]) > 0) ||
				FITS::digit2bin(p[17]) > 6) {
				msg = "Illegal time.";
				return 1;
			    }
			    // decimal required at 19th char, if exists.
			    if (v_len > 19 && !(p[19] == '.')) {
				msg = "Illegal date format.";
				return 1;
			    }
			    // digits required after decimal
			    for (Int curr = 20;curr <= (v_len-1); curr++) {
				if (!FITS::isa_digit(p[curr])) {
				    msg = "Illegal date format.";
				    return 1;
				}
			    }
			} else {
			    msg = "Illegal date format.";
			    return 1;
			}
		    }
		} else if (FITS::isa_digit(p[0]) && FITS::isa_digit(p[1]) &&
			   FITS::isa_digit(p[3]) && FITS::isa_digit(p[4]) &&
			   FITS::isa_digit(p[6]) && FITS::isa_digit(p[7]) &&
			   (p[2] == '/') && (p[5] == '/')) {
		    // the old format
		    dd = FITS::digit2bin(p[0]) * 10 + FITS::digit2bin(p[1]);
		    mm = FITS::digit2bin(p[3]) * 10 + FITS::digit2bin(p[4]);
		    yy = FITS::digit2bin(p[6]) * 10 + FITS::digit2bin(p[7]);
		    yy += 1900;
		} else {
		    msg = "Illegal date format.";
		    return 1;
		}
		if (mm == 0 || mm > 12) {
		    msg = "Illegal date.";
		    return 1;
		}
		mmdays = month[mm];
		if ((mm == 2) && ((yy % 4) == 0))
		    mmdays = 29;
		if (dd == 0 || dd > mmdays) {
		    msg = "Illegal date.";
		    return 1;
		}
		break;
	    // The following "default" was added to prevent compilers
	    // such as GNU g++ from giving warnings about enumeration
	    // values not being handled.  This should be cleaned up
	    // some point.
	    //              -OO
	    default:
	      // Nothing - OK
	        break;
	}
	return 0;
}

const char *ReservedFitsKeywordCollection::aname(FITS::ReservedName nm) const {
	int i;
	for (i = 0; i < no_items; ++i)
	    if (resword[i].name() == nm)
		break;
	return i < no_items ? resword[i].aname() : "";
}

int ReservedFitsKeywordCollection::essential_name(const char *s, int s_len)
	const {
	// If this name MIGHT be an essential name, this routine returns
	// an index into the reserved word table, otherwise 0 is returned.
	int i; // The index into the table.
	if (!FITS::isa_letter(*s))
	    return 0;
	if ((i = resalpha[FITS::letter2bin(*s)]) == 0)
	    return 0;
	for(; *(resword[i].aname()) == *s; ++i) // search for a match
	    if (resword[i].namesize() == s_len &&
		strncmp(s,resword[i].aname(),s_len) == 0)
		    break;
	if (*(resword[i].aname()) != *s || resword[i].namesize() != s_len)
	    return 0;
	if (resword[i].isessential())
	    return i;
	while (resword[i + 1].name() == resword[i].name()) {
	    ++i;
	    if (resword[i].isessential())
		return i;
	}
	return 0;
}

const ReservedFitsKeyword & ReservedFitsKeywordCollection::get_essential(int i,
	Bool n, FITS::ValueType t, const void *v, int v_len,
	const char *&msg) const {
	// This index i must be to an essential name in the table.
	msg = 0;
	if (i <= 0 || i >= no_items) {
		msg = "Internal error!  Invalid index into ResWord.";
		return error_item;
	}
	if (!resword[i].isessential()) {
		msg = "Internal error!  Invalid index into ResWord.";
		return error_item;
	}
	// we have a match on name - match type and isindexed
	if (t == FITS::FLOAT || t == FITS::DOUBLE)
	    t = FITS::REAL;	// change t to REAL to match on types
	if (t == FITS::FSTRING) 
	    t = FITS::STRING;   // change to to STRING to match on types
	if (resword[i].type() != t) {
	    while ((resword[i + 1].name() == resword[i].name()) &&
		   resword[i + 1].isessential()) {
		++i;
		if (resword[i].type() == t)
		    break;
	    }
	    if (resword[i].type() != t) {
		msg = "Keyword value has wrong data type.";
		return error_item;
	    }
	}
	if (resword[i].isindexed() != n) {
	    while ((resword[i + 1].name() == resword[i].name()) &&
		   (resword[i + 1].type() == resword[i].type())) {
		    ++i;
		    if (resword[i].isindexed() == n)
			break;
	    }
	    if (resword[i].isindexed() != n) {
		if (resword[i].isindexed())
		    msg = "Keyword requires an index.";
		else
		    msg = "Keyword must not have an index.";
		return error_item;
	    }
	}
	return rules(resword[i],0,0,n,t,v,v_len,msg) == -1 ?
	    error_item : resword[i];
}

// Instantiate the reserved keyword collection
ReservedFitsKeywordCollection ResWord_;
ReservedFitsKeywordCollection &FITS::ResWord = ResWord_;

void FITS::get_name(const char *s, int len, FitsNameResult &result) {
	int i;
	// A name is any sequence of text chars except ' ' and '='
	result.err = FitsNameResult::OK;
	for (i = 0; *s == ' ' && (i < len); ++i, ++s) ; // skip spaces
	if (i == len || (!FITS::isa_text(*s)) || *s == '=') {
	    result.isaname = False; // If there is no name, only
	    result.begpos = i;	// begpos has meaning.
	    return;
	}
	result.isaname = True;
	result.begpos = i;
	for(; *s != ' ' && *s != '=' && (i < len) && FITS::isa_text(*s); 
		++i, ++s) ;
	result.endpos = i - 1;
	result.len = i - result.begpos;
	--s;
	result.isaindex = False;
	result.index = 0;
	if (FITS::isa_digit(*s)) { // get any index
	    result.isaindex = True;
	    result.index = FITS::digit2bin(*s--);
	    --result.len;
	    if (FITS::isa_digit(*s)) {
		i = 1;
		do {
		    i *= 10;
		    result.index += i * FITS::digit2bin(*s--);
		    --result.len;
		} while(FITS::isa_digit(*s));
		if (s[1] == '0') // index cannot have leading zeros
		    result.err = FitsNameResult::NO_0_NDX; 
	    }
	}
}

int FITS::get_value_id(const char *s, int l, int &pos) {
	// A value_id is the first occurance of "=" in the field.
	// If there is a value_id, 1 is returned; otherwise, 0 is returned.
	// If a value_id is present, its position is recorded in pos.
	int i;
	for (i = 0; (i < l) && *s == ' '; ++i, ++s) ; // skip spaces
	if (i == l || *s != '=') {
	    pos = 0;
	    return 0;
	}
	pos = i;
	return 1;
}

/******************************************************************************
	Fortran-77 Definition of integer, real, and double (no embedded blanks)
	--------------------------------------------------
		digit 		:= <0|1|2|3|4|5|6|7|8|9>
		digit_sequence 	:= digit [ digit ... ]
		sign 		:= <+|->
		point 		:= <.>
		integer 	:= [sign] digit_sequence
		efield 		:= E [integer]
		dfield 		:= D [integer]
		basic_real 	:= integer point [ digit_sequence ]
			           [sign] point digit_sequence
		real 		:= basic_real [ efield ]
				   integer efield
		double 		:= basic_real dfield
				   integer dfield

	FITS fixed formats on keyword cards (NOST 5.3.2)
	------------------------------------------------
	string		' in col 11, ' to close (in or before col 80),
			'' represents ' within the string, 
			trailing blanks not significant
	logical		T or F in col 30
	integer		integer right justified in col 11-30
	complex integer	real part col 11-30, img part col 31-50, right just
	real float	real or double right justified in col 11-30, point req
	complex float	real part col 11-30, img part col 31-50, right just
******************************************************************************/

void FITS::get_value(const char *s, int len, FitsValueResult &result) {
	int i, j;
	result.type = FITS::NOVALUE;
	result.s[0] = 0;
	result.s[1] = 0;
	result.begpos = 0;
	result.endpos = 0;
	result.isa_point = False;
	result.pointpos = 0;
	result.no_sig = 0;
	result.errmsg = 0;
	for (i = 0; *s == ' ' && (i < len); ++s, ++i) ; // skip spaces
	if (i == len) // The field is all blanks.
	    return;
	switch (*s) { // the first non-blank
	    case 'T':	// logical
		result.b = True;
		result.type = FITS::LOGICAL;
		result.begpos = i;
		result.endpos = i;
		return;
	    case 'F':	// logical
		result.b = False;
		result.type = FITS::LOGICAL;
		result.begpos = i;
		result.endpos = i;
		return;
	    case '\'':	// string
		result.type = FITS::STRING;
		result.begpos = i++;
		s++;
		if (i == len) {
		    result.errmsg = "Invalid string syntax.";
		    result.endpos = result.begpos;
		    return;
		}
		result.s[0] = i;
		j = 0; // length of string
		for(;;) {
		    if (i == len) {
			result.errmsg = "String has no ending quote mark.";
			i = len - 1;
			break;
		    } else if (*s != '\'') {
			if (!FITS::isa_text(*s)) {
			    if (!result.errmsg)
			        result.errmsg = 
			              "String value contains non-ASCII_text.";
			}
		        ++j;
			++s;
			++i;
		    } else if (i == (len - 1)) {
			++s;
			break;
		    } else if (*(s+1) == '\'') {
			result.type = FITS::FSTRING;
			s += 2;
			i += 2;
			++j;
		    } else {
			++s;
			break;
		    }
		}
		result.endpos = i;
		--s; // chop off any trailing blanks
		if (*s == '\'')
		    --s;
		while (*s == ' ' && j > 8) {
		    --j;
		    --s;
		}
		result.s[1] = j;
		return;
	    case '(':	// F77 list-directed complex
		result.type = FITS::NOVALUE;
		result.errmsg = "F77 list-directed complex not implemented.";
		return;
	    default:
		if (*s == '.' && (i < (len - 1))) {
		    if (s[i + 1] == 'T') { // F77 list-directed logical
			result.b = True;
			result.type = FITS::LOGICAL;
			result.begpos = i;
			i += 2;
			s += 2;
			for (; *s != ' ' && *s != '/' && (i < len); ++i) ;
			if (i == len)
			    --i;
			result.endpos = i;
			return;
		    } else if (s[i + 1] == 'F') { // F77 logical
			result.b = False;
			result.type = FITS::LOGICAL;
			result.begpos = i;
			i += 2;
			s += 2;
			for (; *s != ' ' && *s != '/' && (i < len); ++i) ;
			if (i == len)
			    --i;
			result.endpos = i;
			return;
		    }
		}
		// Must be numeric, it's the only thing left
		get_numeric(s,(len - i),result);
		result.begpos += i; // the beginning of this s is offset by i
		result.endpos += i;
		if (result.isa_point)
		    result.pointpos += i;
		if (result.errmsg) {
		    if (strcmp(result.errmsg,"Not a number") == 0)
		        result.errmsg = "Value field is not a valid data type.";
		}
		break;
	}
}

int FITS::ckaccum(double &d, Int numb, int pow) {
	// compute d += numb * 10**pow checking for over/underflow
	double tmp = (double)numb;
	if (pow > 0) {
	    if (pow > 2 * maxdblexp)
		return 1;
	    if (tmp <= (maxdouble / tenpowerD[pow]))
		tmp *= tenpowerD[pow];
	    else
		return 1;
	} else if (pow < 0) {
	    pow = -pow;
	    if (pow > 2 * maxdblexp)
		return -1;
	    if ((mindouble * tenpowerD[pow]) <= tmp)
		tmp /= tenpowerD[pow];
	    else
		return -1;
	}
	if ((maxdouble - tmp) < d)
		return 1;
	d += tmp;
	return 0;
}

int FITS::ckaccum(float &f, Int numb, int pow) {
	// compute f += numb * 10**pow checking for over/underflow
	float tmp = (float)numb;
	if (pow > 0) {
	    if (pow > 2 * maxfltexp)
		return 1;
	    if (tmp <= (maxfloat / tenpowerF[pow]))
		tmp *= tenpowerF[pow];
	    else
		return 1;
	} else if (pow < 0) {
	    pow = -pow;
	    if (pow > 2 * maxfltexp)
		return -1;
	    if ((minfloat * tenpowerF[pow]) <= tmp)
		tmp /= tenpowerF[pow];
	    else
		return -1;
	}
	if ((maxfloat - tmp) < f)
		return 1;
	f += tmp;
	return 0;
}

void FITS::get_numeric(const char *s, int len, FitsValueResult &result) {
	int n;	// the number of chars processed -- n == len signals `at end'
	int i, j; // counter, confined to local context
	const char *p; // utility valiable, confined to local context

	result.type = NOVALUE;	// Initialize result
	result.errmsg = 0;
	result.l = 0; // It may not be nessary to init the rest of these.
	result.begpos = 0;
	result.endpos = 0;
	result.isa_point = False;
	result.pointpos = 0;
	result.no_sig = 0;

	// 1. Skip any spaces at the beginning
	for (n = 0; *s == ' ' && (n < len); ++n, ++s) ;
	if (n == len) {
	    result.errmsg = "Value field is all blanks";
	    return;
	}
	// 2. Mark position as the start of the field
	result.begpos = n;
	// 3. Check for any sign that may be present
	int negsign = (*s == '-' ? (++s, ++n, 1) :
			(*s == '+' ? (++s, ++n, 0) : 0));
	if (n == len || !(FITS::isa_digit(*s) || *s == '.')) {
	    result.errmsg = "Not a number";
	    return;
	}
	// 4. Skip any leading 0s as insignificant
	for (; *s == '0' && n < len; ++n, ++s) ;
	if (n == len) {
	    result.type = LONG;
	    result.l = 0;
	    result.endpos = n - 1;
	    result.isa_point = False;
	    result.pointpos = 0;
	    result.no_sig = 1;
	    return;
	}
	// 5. Get integer part of number.  Get digits, store in Ints,
	//	and count significant digits
	Int intpart1 = 0;	// part 1 of digits of integer part
	Int intpart2 = 0;	// part 2 of digits of integer part
	int  sigint   = 0;	// number of significant digits
	if (isa_digit(*s)) {
	    intpart1 = digit2bin(*s);
	    ++sigint;
	    for (++n, ++s; isa_digit(*s) && (n < len); ++n, ++s) {
		++sigint;
		intpart1 = intpart1 * 10 + digit2bin(*s);
		if (sigint == maxdigl) // Longs can always hold maxdigl digits
		    break;
	    }
	    if (sigint == maxdigl && n < len) {
		++n;
		++s;
		if (isa_digit(*s) && n < len) {
		    intpart2 = digit2bin(*s);
		    ++sigint;
		    for (++n, ++s; isa_digit(*s) && (n < len); ++n, ++s) {
			++sigint;
			intpart2 = intpart2 * 10 + digit2bin(*s);
			if (sigint == maxsigdigits)
			    break;
		    }
		}
		if (sigint == maxsigdigits && n < len) { // Discard digits
		    for (++n, ++s; isa_digit(*s) && (n < len); ++n, ++s)
			++sigint; // But count them
		}
	    }
	}
	if (n == len || (!(*s == '.' || *s == 'E' || *s == 'D'))) {
	    result.endpos = n - 1;
	    result.isa_point = False;
	    result.pointpos = 0;
	    if (sigint < 10) {
		result.type = LONG;
		result.l = negsign ? (-intpart1) : intpart1;
		result.no_sig = sigint == 0 ? 1 : sigint;
		return;
	    }
	    if (sigint > 10) {
		result.errmsg = negsign ? "Integer underflow" :
					  "Integer overflow";
		return;
	    }
	    if (negsign) {
		intpart1 = -intpart1;
		if (((minInt + intpart2) / 10) <= intpart1) {
		    result.type = LONG;
		    result.l = intpart1 * 10 - intpart2;
		    result.no_sig = sigint;
		    return;
		} else {
		    result.errmsg = "Integer underflow";
		    return;
		}
	    } else {
		if (((maxInt - intpart2) / 10) >= intpart1) {
		    result.type = LONG;
		    result.l = intpart1 * 10 + intpart2;
		    result.no_sig = sigint;
		    return;
		} else {
		    result.errmsg = "Integer overflow";
		    return;
		}
	    }
	}
	// 6. If valid, the number is float or double.  Get the fraction
	//	part, if any.
	Int fracpart1 = 0;	// part 1 of digits of fraction part
	Int fracpart2 = 0;	// part 2 of digits of fraction part
	int  sigfrac   = 0;	// number of significant digits in fraction
	int  fracpos   = 0;	// position of first digit relative to point
	int  exp       = 0;	// exponent
	int  sigexp    = 0;	// number of significant digits in exponent
	char exp_type  = ' ';	// the exponent letter
	if (*s == '.') {
	    result.isa_point = True;
	    result.pointpos = n;
	    // 7. Get the fraction part
	    ++s;
	    ++n;
	    if (n == len)
		goto real_value;
	    if (*s == 'E' || *s == 'D')
		goto get_exp;
	    if (!(isa_digit(*s)))
		goto real_value;
	    if (sigint == 0) { // i. e., there are no sig digs in int part
		for (; *s == '0' && n < len; ++n, ++s) ;
		if (n == len)
		    goto real_value;
		fracpos = n - result.pointpos - 1;
	    }
	    p = s; // save start of fraction part
	    for (; isa_digit(*s) && (n < len); ++s, ++n, ++sigfrac) ;
	    // adjust sigfrac to only process the maximum number of sig digits
	    if (sigint >= maxsigdigits)
		sigfrac = 0;
	    else if ((sigint + sigfrac) > maxsigdigits)
		    sigfrac = maxsigdigits - sigint;
	    // get sigfrac digits starting at p
	    if (sigfrac > 0) {
		fracpart1 = digit2bin(*p);
		for (i = 1, ++p; i < sigfrac && i < maxdigl; ++i, ++p)
		    fracpart1 = fracpart1 * 10 + digit2bin(*p);
		if (i == maxdigl && i < sigfrac) {
		    if (i < sigfrac) {
			fracpart2 = digit2bin(*p);
			for (++i, ++p; i < sigfrac; ++i, ++p)
			    fracpart2 = fracpart2 * 10 + digit2bin(*p);
		    }
		}
	    }
	    // 8. Check next char after fraction part
	    if (!(*s == 'E' || *s == 'D'))
		goto real_value;
	}
	// 9. Get the exponent, if any
    get_exp:
	exp_type = *s++;
	++n;
	if (n == len)
	    goto real_value;
	// 10. Check for any sign that may be present
	i = 0; // i is the sign
	if (*s == '+' || *s == '-') {
	    if (*s == '-')
		i = 1;
	    ++n;
	    ++s;
	    if (n == len) {	   // This is a strange condition, the field
		result.endpos = n; // ends with a sign.  Mark it as an error.
		result.errmsg = "Value field is not a valid number";
		return;
	    }
	}
	// 11. Skip any leading 0s as insignificant
	for (; *s == '0' && n < len; ++n, ++s) ;
	if (n == len)
	    goto real_value;
	// 12. Get, at most, the max digits in an exponent
	if (isa_digit(*s)) {
	    exp = digit2bin(*s);
	    ++sigexp;
	    for (++n, ++s; isa_digit(*s) && (n < len); ++n, ++s) {
		++sigexp;
		exp = exp * 10 + digit2bin(*s);
		if (sigexp == maxexpdig) {
		    ++n;
		    if (n == len)
			break;
		    if (isa_digit(s[1])) {
			result.endpos = n - 1;
			if (i)
			    result.errmsg = "Exponent underflow";
			else
			    result.errmsg = "Exponent overflow";
			return;
		    }
		    break;
		}
	    }
	    if (i)
		exp = -exp;
	}
    real_value: // 13. Compute real value
	// It's real because either result.isa_point == 1, or exp_type != ' '.
	result.endpos = n - 1;
	result.no_sig = sigint + sigfrac;
	if (result.no_sig <= 5 && (exp_type == ' ' || exp_type == 'E')) {
	    if (intpart1 == 0) {
		if (fracpart1 == 0) {
		    result.type = FLOAT;
		    result.f = 0.0F;
		    return;
		} else if ((exp - fracpos - 1) > minfltexp &&
		    (exp - fracpos) < maxfltexp) {
		    result.type = FLOAT;
		    result.f = tenF(fracpart1,(exp - sigfrac - fracpos));
		    if (negsign)
			result.f = -result.f;
		    return;
		}
	    } else if ((sigint - 1 + exp) > minfltexp &&
		    (sigint + exp) < maxfltexp) {
		result.type = FLOAT;
		result.f = (fracpart1 == 0) ? 0.0F :
			   tenF(fracpart1,(exp - sigfrac - fracpos));
		result.f += tenF(intpart1,exp);
		if (negsign)
		    result.f = -result.f;
		return;
	    }
	    result.f = 0.0F;
	    i = ckaccum(result.f,fracpart1,(exp - sigfrac - fracpos));
	    if (i) {
		result.errmsg = (i > 0) ? "Float overflow" : "Float underflow";
		return;
	    }
	    i = ckaccum(result.f,intpart1,exp);
	    if (i) {
		result.errmsg = (i > 0) ? "Float overflow" : "Float underflow";
		return;
	    }
	    result.type = FLOAT;
	    if (negsign)
		result.f = -result.f;
	    return;
	}
	if (result.no_sig > 5 || exp_type == 'D') {
	    if (intpart1 == 0) {
		if (fracpart1 == 0) {
		    result.type = DOUBLE;
		    result.d = 0.0;
		    return;
		} else if ((exp - fracpos - 1) > mindblexp &&
			(exp - fracpos) < maxdblexp) {
		    result.type = DOUBLE;
		    result.d = (fracpart2 == 0) ? 0.0 :
			   tenD(fracpart2,(exp - sigfrac - fracpos));
		    i = (sigfrac < maxdigl) ? sigfrac : maxdigl;
		    i = exp - fracpos - i;
		    result.d += tenD(fracpart1,i);
		    if (negsign)
			  result.d = -result.d;
		    return;
		}
	    } else if ((sigint - 1 + exp) > mindblexp &&
			(sigint + exp) < maxdblexp) {
		result.type = DOUBLE;
		result.d = (fracpart2 == 0) ? 0.0 :
		       tenD(fracpart2,(exp - sigfrac - fracpos));
		if (fracpart1) {
		    i = (sigfrac < maxdigl) ? sigfrac : maxdigl;
		    i = exp - fracpos - i;
		    result.d += tenD(fracpart1,i);
		}
		if (intpart2) {
		    i = sigint - maxsigdigits;
		    if (i < 0)
			i = 0;
		    i += exp;
		    result.d += tenD(intpart2,i);
		}
		i = sigint - maxdigl;
		if ( i < 0)
		    i = 0;
		i += exp;
		result.d += tenD(intpart1,i);
		if (negsign)
		    result.d = -result.d;
		return;
	    }
	}
	// There might be an overflow.
	result.d = 0.0;
	j = ckaccum(result.d,fracpart2,(exp - sigfrac - fracpos));
	if (j) {
	    result.errmsg = (j > 0) ? "Double overflow" : "Double underflow";
	    return;
	}
	if (fracpart1) {
	    i = (sigfrac < maxdigl) ? sigfrac : maxdigl;
	    i = exp - fracpos - i;
	    j = ckaccum(result.d,fracpart1,i);
	    if (j) {
	      result.errmsg = (j > 0) ? "Double overflow" : "Double underflow";
	      return;
	    }
	}
	if (intpart2) {
	    i = sigint - maxsigdigits;
	    if (i < 0)
		i = 0;
	    i += exp;
	    j = ckaccum(result.d,intpart2,i);
	    if (j) {
	      result.errmsg = (j > 0) ? "Double overflow" : "Double underflow";
	      return;
	    }
	}
	i = sigint - maxdigl;
	if ( i < 0)
	    i = 0;
	i += exp;
	j = ckaccum(result.d,intpart1,i);
	if (j) {
	    result.errmsg = (j > 0) ? "Double overflow" : "Double underflow";
	    return;
	}
	result.type = DOUBLE;
	return;
}

int FITS::trim_comment(const char *s, int len) {
	// trim blanks from end of comment and return the length
	for (--len; len >= 0 && s[len] == ' '; --len) ;
	return ++len;
}

int FITS::chk_comment(const char *s, int len) { 
	while (len--) // check comment for invalid characters
	    if (!FITS::isa_text(*s++))
	        return -1;
	return 0;
}

int FITS::get_comment(const char *s, int len, int &begpos) {
	// find the beginning of a comment and return the trimmed length
	int i;
	for (i = 0; i < len && s[i] == ' '; ++i) ;
	if (i < len && s[i] == '/') {
	    ++i;
	    if (i < len) {
		begpos = i;
		return trim_comment(&s[i],(len - i));
	    }
            begpos = 0;
            return 0;
	}
        begpos = 0;
        return trim_comment(s,len);
}

void FITS::fstr2str(char *target, const char *source, int len) {
	// len is the `real' length, i. e. '' counts as one char
	while (len--) {
	    *target++ = *source++;
	    if (len && (*source == '\''))
	        ++source;
	}
}

int FITS::str2fstr(char *target, const char *source, int len) {
	// len is the maximum size of the target
	int i = 0; // the actual number of F77 chars in target
	int j = 0; // the actual number of ascii chars in target
	while (*source && len) {
	    if (len && (*source == '\'')) {
	    	*target++ = '\'';
		++j;
	    	--len;
		if (len ==0)
		    break;
	    }
	    *target++ = *source++;
	    ++i;
	    ++j;
	    --len;
	}
	for (; (i < 8) && len; ++i, --len)
	    *target++ = ' ';	// target must be at least 8 chars long
	return j;
}

void FITS::parse_vatform(const char *s, FITS::ValueType &valType,
			 int &maxelem)
{
    int ok = 1;
    if (s && *s != '\0') {
	// if first character is a digit, must be 0 or 1
	if (FITS::isa_digit(*s)) {
	    if (!(*s == '0' || *s == '1')) ok = 0;
	    s++;
	}
	// this char must be a P
	if (ok && *s != 'P') ok = 0;
	if (ok) s++;
	// this char must be a letter
	if (ok && FITS::isa_letter(*s)) {
	    switch (*s) {
	    case 'L': valType = FITS::LOGICAL; break;
	    case 'X': valType = FITS::BIT; break;
	    case 'B': valType = FITS::BYTE; break;
	    case 'I': valType = FITS::SHORT; break;
	    case 'J': valType = FITS::LONG; break;
	    case 'A': valType = FITS::CHAR; break;
	    case 'E': valType = FITS::FLOAT; break;
	    case 'D': valType = FITS::DOUBLE; break;
	    case 'C': valType = FITS::COMPLEX; break;
	    case 'M': valType = FITS::DCOMPLEX; break;
	    default: valType = FITS::NOVALUE; break;
	    }
	    s++;
	} else {
	    ok = 0;
	}
	// this char must be a '('
	if (ok && *s == '(') {
	    s++;
	    // get all the digits which make up the maxelem
	    // skip leading zeros
	    maxelem = -1;
	    while (FITS::isa_digit(*s) && *s == '0') s++;
	    if (FITS::isa_digit(*s)) maxelem = FITS::digit2bin(*s++);
	    while (FITS::isa_digit(*s)) {
		maxelem = maxelem * 10 + FITS::digit2bin(*s++);
	    }
	    // at the end of all the above, must find a ')'
	    if (*s != ')' || maxelem < 0) ok = 0;
	} else {
	    ok = 0;
	}
    }
    if (!ok) {
	maxelem = -1;
	valType = FITS::NOVALUE;
    }
}

FitsParse::FitsParse(int max) : no_errs_(0), max_errs(max) {
	err_ = new const char * [max_errs];
	// check for storage allocation errors
	if (err_ == 0) {
	    cerr << "FitsParse cannot allocate storage -- exiting\n";
	    exit(1);
	}
}

FitsKeyword & FitsParse::mkerr(const char *s, int len) {
	int comm_len = FITS::trim_comment(s,len);
	if (FITS::chk_comment(s,comm_len))
	    seterr("Comment contains non-ASCII_text.");
	return *new FitsKeyword("ERROR",5,FITS::NOVALUE,0,0,s,comm_len);
}

FitsKeyword & FitsParse::parse(const char *s, int len) {
	FitsNameResult kword;
	FitsValueResult val;
	int isa_value_id;
	int value_id_pos;
	int comm_pos;
	int comm_len;
	int pos;
	int i, n;

	no_errs_ = 0; // reset the number of errors

	FITS::get_name(s,8,kword); // First, get the name in the name field
	if (!kword.isaname) {
	    if (kword.begpos >= 8) {
		comm_len = FITS::trim_comment(&s[8],(len - 8));
		if (FITS::chk_comment(&s[8],comm_len))
		    seterr("Comment contains non-ASCII_text.");
		return *new FitsKeyword(&FITS::ResWord.spaces(),
				0,FITS::NOVALUE,0,0,&s[8],comm_len);
	    } else {
		seterr("Invalid name field.");
		return mkerr(s,len);
	    }
	}
	int namelen = kword.endpos - kword.begpos + 1;
	if (kword.begpos != 0)
	    seterr("Name should be left-justified.");
	else if (namelen < 8) {
	    for (i = kword.endpos + 1; s[i] == ' ' && i < 8; ++i) ;
	    if (i < 8) {
		seterr("Invalid name field.");
		return mkerr(s,len);
	    }
	}
	if (namelen > 8)
		seterr("Name cannot be greater than 8 chars.");
	if (strncmp(s,"COMMENT",namelen) == 0) {
		comm_len = FITS::trim_comment(&s[8],(len - 8));
		if (FITS::chk_comment(&s[8],comm_len))
		    seterr("Comment contains non-ASCII_text.");
		return *new FitsKeyword(&FITS::ResWord.comment(),
				0,FITS::NOVALUE,0,0,&s[8],comm_len);
	}
	if (strncmp(s,"HISTORY",namelen) == 0) {
		comm_len = FITS::trim_comment(&s[8],(len - 8));
		if (FITS::chk_comment(&s[8],comm_len))
		    seterr("Comment contains non-ASCII_text.");
		return *new FitsKeyword(&FITS::ResWord.history(),
				0,FITS::NOVALUE,0,0,&s[8],comm_len);
	}

	// At this point we have eliminated SPACES, HISTORY, and COMMENT
	// cards and have some sort of name, either a user-defined or
	// reserved name.

	// Get the value indicator
	pos = kword.endpos + 1;
	isa_value_id = FITS::get_value_id(&s[pos],(len - pos),value_id_pos);

	if (!isa_value_id) {
	    n = FITS::ResWord.isreserved(&s[kword.begpos],kword.len);
	    if (n != 0 && FITS::ResWord.requires_value(n)) {
	        seterr("No value indicator -- reserved keyword \
must have a value.");
	        return mkerr(s,len);
	    }
	    comm_len = FITS::trim_comment(&s[pos],(len - pos));
	    const char *comerr;
	    const ReservedFitsKeyword *com = &FITS::ResWord.get(&s[kword.begpos],
		namelen,kword.isaindex,FITS::NOVALUE,0,0,comerr);
	    if (comerr)
		seterr(comerr);
	    if (com->name() == FITS::ERRWORD)
		return mkerr(s,len);
	    if (FITS::chk_comment(&s[pos],comm_len))
		seterr("Comment contains non-ASCII_text.");
	    if (com->name() == FITS::USER_DEF)
		return *new FitsKeyword(&s[kword.begpos],namelen,
				FITS::NOVALUE,0,0,&s[pos],comm_len);
	    else
		return *new FitsKeyword(com,0,FITS::NOVALUE,0,0,&s[pos],comm_len);
	};
	if (strncmp(s,"END",namelen) == 0) {
	    seterr("END keyword has a value indicator -- corrected");
	    return *new FitsKeyword(&FITS::ResWord.end_item(),
					0,FITS::NOVALUE,0,0,0,0);
	}
	pos += value_id_pos;
	if ((pos++) != 8)
	    seterr("Value indicator does not conform to FITS standard.");
	if (s[pos] != ' ')
	    seterr("Value indicator must be `= \' -- corrected");
	++pos;
	if (pos == len) {
	    n = FITS::ResWord.isreserved(&s[kword.begpos],kword.len);
	    if (n != 0 && FITS::ResWord.requires_value(n)) {
	        seterr("Reserved keyword must have a value.");
	        return mkerr(s,len);
	    }
	    seterr("Value indicator without a value.");
	    return mkerr(s,len);
	}

	// Get the value
	FITS::get_value(&s[pos],(len - pos),val);
	val.begpos += pos;
	val.endpos += pos;
	if (val.isa_point)
		val.pointpos += pos;
	if (val.errmsg) {
	    seterr(val.errmsg);
	    return mkerr(s,len);
	}
	if (val.type == FITS::NOVALUE) {
	    n = FITS::ResWord.isreserved(&s[kword.begpos],kword.len);
	    if (n != 0 && FITS::ResWord.requires_value(n)) {
	        seterr("Reserved keyword must have a value.");
	        return mkerr(s,len);
	    }
	    seterr("Value indicator without a value.");
	    return mkerr(s,len);
	}
	const void *addrval = &val.l;
	if (val.type == FITS::STRING || val.type == FITS::FSTRING) {
	    val.s[0] += pos;
	    addrval = &s[val.s[0]];
	}

	// Now, see if name, index, type, value matches a reserved word
	const char *reserr = 0;
	const ReservedFitsKeyword *res = &FITS::ResWord.get(&s[kword.begpos],
		kword.len,kword.isaindex,val.type,addrval,val.s[1],reserr);
	if (reserr){
	    seterr(reserr);
	}
	if (res->name() == FITS::ERRWORD) {
	    n = FITS::ResWord.isreserved(&s[kword.begpos],kword.len);
	    if ((n != 0) && FITS::ResWord.isunique(n) &&
		(FITS::ResWord[n].type() != val.type)) {
		// This is a attempt to correct a common error -- integers
		// where there should be reals. Convert these to doubles.
		if (val.type == FITS::LONG && 
		    FITS::ResWord[n].type() == FITS::REAL) {
		    val.type = FITS::DOUBLE;
		    val.d = (double)val.l;
		    seterr("... converted to type double.");
		    reserr = 0;
		    res = &FITS::ResWord.get(&s[kword.begpos],kword.len,
			kword.isaindex,val.type,addrval,val.s[1],reserr);
		    if (reserr)
	    		seterr(reserr);
		}
		else
		    return mkerr(s,len);
	    } else
	        return mkerr(s,len);
	}

	// At this stage we have a legitimate keyword.
	pos = val.endpos + 1;
	comm_len = FITS::get_comment(&s[pos],(len - pos),comm_pos);
	if (FITS::chk_comment(&s[comm_pos + pos],comm_len))
	    seterr("Comment contains non-ASCII_text.");
	if (res->name() == FITS::USER_DEF)
	    return *new FitsKeyword(&s[kword.begpos],namelen,
			val.type,addrval,val.s[1],&s[comm_pos + pos],comm_len);
	else {
	  switch (val.type) { // check for adherence to fixed format
	    case FITS::FSTRING:
	    case FITS::STRING:
              // allow for lenths < 8 characters even though that isn't
              // exactly fixed format.
	      if (!(val.begpos == 10 && val.endpos <= 79))
	        seterr("String value does not conform to FITS fixed format.");
	      break;
	    case FITS::LOGICAL:
	      if (!(val.begpos == 29 && val.endpos == 29))
		seterr("Logical value does not conform to FITS fixed format.");
	      break;
	    default: // is numeric
	      if (!(val.begpos >= 10 && val.endpos == 29))
		seterr("Numeric value does not conform to FITS fixed format.");
	      break;
	    }
	    return *new FitsKeyword(res,kword.index,
			val.type,addrval,val.s[1],&s[comm_pos + pos],comm_len);
	}
}

const void *FitsKeyword::value() const {
	switch (type_) {
	    case FITS::LOGICAL: return &bval;
	    case FITS::LONG: 	return &ival;
	    case FITS::FLOAT: 	return &fval;
	    case FITS::DOUBLE: 	return &dval;
	    default:		return val;
	}
}

FitsKeyword & FitsKeyword::operator = (const char *v) {
	int vlen = strlen(v);
	if (type_ == FITS::STRING && vlen <= vallen) {
	    memcpy(val,v,vlen);
            ((char *)val)[vlen] = '\0';
	    vallen = vlen;
	} else {
	    type_ = FITS::STRING;
	    char *p = new char [vlen + 1]; memchk(p);
	    memcpy(val,v,vlen);
            ((char *)val)[vlen] = '\0';
	    vallen = vlen;
	    del_val();
	    val = p;
	}
	return *this;
}

void FitsKeyword::comm(const char *c) {
	if (c == 0) {
	    delete [] comm_;
	    comm_ = 0;
	    commlen_ = 0;
	    return;
	}
	int clen = strlen(c);
	if (clen <= commlen_) {
	    memcpy(comm_,c,clen);
            comm_[clen] = '\0';
	    commlen_ = clen;
	} else {
	    char *p = new char [clen + 1]; memchk(p);
	    memcpy(p,c,clen);
            p[clen] = '\0';
	    commlen_ = clen;
	    delete [] comm_;
	    comm_ = p;
	}
	return;
}

void FitsKeyword::name(const char *n) {
	if (isreserved()) {
	    err(name(),type(),val,"Cannot change name of reserved word");
	    return;
        }
	if (n == 0) {
            err(name(),type(),val,"User-defined name cannot be null");
	    return;
	}
	int nlen = strlen(n);
	if (nlen <= namelen_) {
	    memcpy(name_,n,nlen);
            name_[nlen] = '\0';
	    namelen_ = nlen;
	} else {
	    char *p = new char [nlen + 1]; memchk(p);
	    memcpy(p,n,nlen);
            p[nlen] = '\0';
	    namelen_ = nlen;
	    delete [] name_;
	    name_ = p;
	}
	return;
}

void FitsKeyword::memchk(void *p) {
	if (p == 0) {
	    cout << "Keyword: could not allocate memory.\n";
	    exit(-1);
	}
}

void FitsKeyword::err(const char *nm, const FITS::ValueType &ty, 
	const void *val, const char *msg) {
	cout << "Keyword Error:  name = " << nm << " of type " << ty 
	     << " and value ";
	FITS::valstr(cout,ty,val);
	cout << "\n\t" << msg << "\n";
}

void FitsKeyword::setval(const FITS::ValueType &ty, const void *v, int vlen) {
	if (ty == FITS::STRING || ty == FITS::FSTRING) {
	    int i, plen = vlen;
	    if (vlen < 8)
	        plen = 8; // FITS strings must be at least 8 chars
	    char *p = new char [plen + 1]; memchk(p);
	    if (ty == FITS::STRING)
		memcpy(p,v,vlen);
	    else
		FITS::fstr2str(p,(const char *)v,vlen);
	    for (i = vlen; i < 8; i++)
		p[i] = ' ';
	    p[i] = '\0';
	    val = p;
	    vallen = i;
            type_ = FITS::STRING;
	} else {
	    type_ = ty;
	    val = 0;
	    vallen = 0;
	    switch(type_) {
		case FITS::LOGICAL: bval = *((Bool *)v); break;
		case FITS::LONG: ival = *((Int *)v); break;
		case FITS::FLOAT: fval = *((float *)v); break;
		case FITS::DOUBLE: dval = *((double *)v); break;
		case FITS::ICOMPLEX:
		    val = new IComplex; memchk(val);
		    *((IComplex *)val) = *((IComplex *)v);
                    break;
		case FITS::COMPLEX:
		    val = new Complex; memchk(val);
		    *((Complex *)val) = *((Complex *)v);
                    break;
		case FITS::DCOMPLEX:
		    val = new DComplex; memchk(val);
		    *((DComplex *)val) = *((DComplex *)v);
                    break;
	    // The following "default" was added to prevent compilers
	    // such as GNU g++ from giving warnings about enumeration
	    // values not being handled.  This should be cleaned up
	    // some point.
	    //              -OO
	        default:
		  break;
	    }
	}
}

void FitsKeyword::setcomm(const char *c, int clen) {
	if (c == 0) {
	    comm_ = 0;
	    commlen_ = 0;
	    return;
	}
	comm_ = new char [clen + 1]; memchk(comm_);
	memcpy(comm_,c,clen);
        comm_[clen] = '\0';
	commlen_ = clen;
}

void FitsKeyword::init(const FitsKeyword &k) {
	next_ = 0;
	prev_ = 0;
	setval(k.type_,k.value(),k.vallen);
	setcomm(k.comm_,k.commlen_);
	kw_ = k.kw_;
	ndx = k.ndx;
	namelen_ = k.namelen_;
	if (k.name_) {
	    name_ = new char [namelen_ + 1]; memchk(name_);
	    memcpy(name_,k.name_,namelen_);
	    name_[namelen_] = '\0';
	}
}

FitsKeyword::FitsKeyword(const char *nm, int nmlen, FITS::ValueType ty,
	const void *v, int vlen, const char *cm, int cmlen) {
	// Construct a user-defined keyword
	next_ = 0;
	prev_ = 0;
	kw_ = &FITS::ResWord.userdef_item();
	ndx = 0;
	namelen_ = nmlen;
	name_ = new char [namelen_ + 1]; memchk(name_);
	memcpy(name_,nm,namelen_);
	name_[namelen_] = '\0';
	setval(ty,v,vlen);
        setcomm(cm,cmlen);
}

FitsKeyword::FitsKeyword(const ReservedFitsKeyword *r, int nd, 
	FITS::ValueType ty, const void *v, int vlen, const char *cm, 
	int cmlen) {
	// Construct a reserved keyword
	next_ = 0;
	prev_ = 0;
	kw_ = r;
	ndx = nd;
	namelen_ = r->namesize();
        name_ = 0;
	setval(ty,v,vlen);
        setcomm(cm,cmlen);
}

void FitsKeyword::del_val() {
	if (val == 0) return;

	switch (type_) {
	case FITS::STRING:
	case FITS::FSTRING: 
	    { char *p = (char *)val;
               delete [] p; }
            break;
        case FITS::ICOMPLEX:
            { IComplex *p = (IComplex *)val;
              delete p; }
            break;
        case FITS::COMPLEX:
            { Complex *p = (Complex *)val;
              delete p; }
            break;
        case FITS::DCOMPLEX:
            { DComplex *p = (DComplex *)val;
              delete p; }
            break;
	default:
	// This is ugly, but this design doesn't allow catching this at
	// compile time - so we throw an error at run time.
        // Each unique type allocated by new should have its own switch above.
            cerr << "FitsKeyword::del_val() internal error - unknown type";
	    cerr << " - exiting." << endl;
	    exit(1);
	}
}


ostream & operator << (ostream &o, const FitsKeyword &x) {
	if (x.kw().name() == FITS::ERRWORD)
	    o << "ERROR: \t ";
	else {
	    o << x.name();
	    if (x.index() == 0) {
	    	if (x.kw().name() == FITS::USER_DEF)
	            o << ":U:";
	    	else
	    	    o << ":  ";
	    } else
	        o << "[" << x.index() << "]:";
	    o << "\t" << x.type();
	}
	FITS::valstr(o,x.type(),x.value());
	if (x.commlen())
	    o << " \"" << x.comm() << "\"";
	o << "\n";
	return o;
}

FitsKeyword &FitsKeywordList::make(const char *nm,
				   FITS::ValueType ty, const void *val, const char *cm) {
    FitsKeyword *kw;
    if (!nm) {
	return makeErrKeyword("", ty, val, "User defined name cannot be NULL.");
    }
    int nmlen = strlen(nm);
    if (nmlen > 8) {
	return makeErrKeyword(nm, ty, val, "User defined name cannot be > 8 characters long.");
    }
    int cmlen = 0;
    if (cm)
	cmlen = strlen(cm);
    int vallen = 0;
    if (ty == FITS::STRING) {
	if (val == 0)
	    ty = FITS::NOVALUE;
	else {
	    vallen = strlen((char *)val);
	    if (vallen > 68) {
		return makeErrKeyword(nm, ty, val, "String values cannot be > 68 characters long.");
	    }
	}
    }
    int valsize = (vallen < 8) ? 8 : vallen;
    const char *errmsg = 0;
    const ReservedFitsKeyword *rw = &FITS::ResWord.get(nm,nmlen, False,
						       ty,val,valsize,errmsg);
    if (errmsg)
	FitsKeyword::err(nm,ty,val,errmsg);
    if (rw->name() == FITS::USER_DEF)
	kw = new FitsKeyword(nm,nmlen,ty,val,vallen,cm,cmlen);
    else
	kw = new FitsKeyword(rw,0,ty,val,vallen,cm,cmlen);
    FitsKeyword::memchk(kw);
    return *kw;
}

FitsKeyword &FitsKeywordList::make(FITS::ReservedName nm,
				   FITS::ValueType ty, const void *val, const char *cm) {
    FitsKeyword *kw;
    int cmlen = 0;
    if (cm)
	cmlen = strlen(cm);
    int vallen = 0;
    if (ty == FITS::STRING) {
	if (val == 0)
	    ty = FITS::NOVALUE;
	else {
	    vallen = strlen((char *)val);
	    if (vallen > 68) {
		return makeErrKeyword(FITS::ResWord.aname(nm), ty, val, "String values cannot be > 68 characters long.");
	    }
	}
    }
    int valsize = (vallen < 8) ? 8 : vallen;
    const char *errmsg = 0;
    const ReservedFitsKeyword *rw = &FITS::ResWord.get(nm,False,
						       ty,val,valsize,errmsg);
    if (errmsg)
	FitsKeyword::err(FITS::ResWord.aname(nm),ty,val,errmsg);
    if (rw->name() == FITS::USER_DEF) {
	return makeErrKeyword(FITS::ResWord.aname(nm), ty, val,
			      "Function cannot be used for user defined keyword.");
    } else {
	kw = new FitsKeyword(rw,0,ty,val,vallen,cm,cmlen);
    }
    FitsKeyword::memchk(kw);
    return *kw;
}

FitsKeyword &FitsKeywordList::make(int ind, FITS::ReservedName nm,
				   FITS::ValueType ty, const void *val, const char *cm) {
    FitsKeyword *kw;
    int cmlen = 0;
    if (cm)
	cmlen = strlen(cm);
    int vallen = 0;
    if (ty == FITS::STRING) {
	if (val == 0)
	    ty = FITS::NOVALUE;
	else {
	    vallen = strlen((char *)val);
	    if (vallen > 68) {
		return makeErrKeyword(FITS::ResWord.aname(nm), ty, val, "String values cannot be > 68 characters long.");
	    }
	}
    }
    int valsize = (vallen < 8) ? 8 : vallen;
    const char *errmsg = 0;
    const ReservedFitsKeyword *rw = &FITS::ResWord.get(nm,True,
						       ty,val,valsize,errmsg);
    if (errmsg)
	FitsKeyword::err(FITS::ResWord.aname(nm),ty,val,errmsg);
    if (rw->name() == FITS::USER_DEF) {
	return makeErrKeyword(FITS::ResWord.aname(nm), ty, val,
			      "Function cannot be used for user defined keyword.");
    } else {
	kw = new FitsKeyword(rw,ind,ty,val,vallen,cm,cmlen);
    }
    FitsKeyword::memchk(kw);
    return *kw;
}

FitsKeyword &FitsKeywordList::makeErrKeyword(const char *name, FITS::ValueType type, 
					     const void *val, const char *errmsg)
{
    FitsKeyword::err(name,type,val,errmsg);
    FitsKeyword *kw = new FitsKeyword(&FITS::ResWord.err_item(),0,FITS::NOVALUE,0,0,0,0);
    FitsKeyword::memchk(kw);
    return *kw;
}

ostream & operator << (ostream &o, FitsKeywordList &w) {
	w.first();
	FitsKeyword *x = w.next();
	for (int i = 1; x != 0; ++i, x = w.next())
		o << i << ". " << *x;
	return o;
}

FitsKeywordList::FitsKeywordList(const FitsKeywordList &w) : beg_(0), end_(0), 
	pos(0), total(0), cursor(0) {
	FitsKeyword *k;
	for (FitsKeyword *x = w.beg_; x != 0; x = x->next_) {
	    k = new FitsKeyword(*x);
	    FitsKeyword::memchk(k);
	    insert(*k);
	}
}

FitsKeywordList::FitsKeywordList(ConstFitsKeywordList &w) : 
	beg_(0), end_(0), pos(0), total(0), cursor(0) {
	FitsKeyword *k;
	w.first();
	for (const FitsKeyword *x = w.next(); x != 0; x = w.next()) {
	    k = new FitsKeyword(*x);
	    FitsKeyword::memchk(k);
	    insert(*k);
	}
}

FitsKeywordList & FitsKeywordList:: operator = (const FitsKeywordList &w) {
	delete_all();
	FitsKeyword *k;
	for (FitsKeyword *x = w.beg_; x != 0; x = x->next_) {
	    k = new FitsKeyword(*x);
	    FitsKeyword::memchk(k);
	    insert(*k);	
	}
	return *this;
}

FitsKeyword *FitsKeywordList::next() {
	if (cursor == total) { return 0; }
	if (cursor == 0) { ++cursor; return pos; }
	pos = pos->next_;
	++cursor;
	return pos;
}

FitsKeyword *FitsKeywordList::prev() {
	if (cursor == 0) { return 0; }
	FitsKeyword *x = pos;
	if (pos->prev_)
	    pos = pos->prev_;
	--cursor;
	return x;
}

// Return the i-th keyword -- keyword numbering starts with 0
FitsKeyword *FitsKeywordList::operator () (int n) {
	if (n < 0 || n >= total)
	    return 0;
	first();
	while(n--)
	    next();
	return curr();
}

void FitsKeywordList::insert(FitsKeyword &k) {
	if (cursor == 0) {
	    k.next_ = beg_;
	    isempty() ? (end_ = &k) : (beg_->prev_ = &k);
	    beg_ = &k;
	} else if (cursor == total) {
	   k.prev_ = end_;
	   isempty() ? (beg_ = &k) : (end_->next_ = &k);
	   end_ = &k;
	} else {
	    k.next_ = pos->next_;
	    k.prev_ = pos;
	    pos->next_->prev_ = &k;
	    pos->next_ = &k;
	}
	pos = &k;
	++cursor;
	++total;
}

void FitsKeywordList::del() {
	if (isempty())
	    return;
	if (cursor == 0) {
	    pos = beg_->next_;
	    pos ? (pos->prev_ = 0) : (end_ = 0);
	    delete beg_;
	    beg_ = pos;
	    --total;
	} else if (cursor == total) {
	    pos = end_->prev_;
	    pos ? (pos->next_ = 0) : (beg_ = 0);
	    delete end_;
	    end_ = pos;
	    --total;
	    cursor = total;
	} else {
	    FitsKeyword *x = pos->prev_;
	    pos->next_->prev_ = x;
	    if (x) {
		x->next_ = pos->next_;
		delete pos;
		pos = x;
	    } else {
		beg_ = pos->next_;
		delete pos;
		pos = beg_;
	    }
	    --total;
	    --cursor;
	}
}

void FitsKeywordList::delete_all() {
	last();
	while (!isempty())
	    del();
}

FitsKeyword *FitsKeywordList::next(const FITS::ReservedName &n) {
	FitsKeyword *x;
	for (x = next(); x != 0; x = next())
		if (x->isreserved() && !(x->isindexed()) &&
		    (n == x->kw().name()))
			break;
	return x ? curr() : 0;
}

FitsKeyword *FitsKeywordList::next(const FITS::ReservedName &n, int ndx) {
	FitsKeyword *x;
	for (x = next(); x != 0; x = next())
		if (x->isreserved() && (x->index() == ndx) &&
		    (n == x->kw().name()))
			break;
	return x ? curr() : 0;
}

FitsKeyword *FitsKeywordList::next(const char *w) {
	FitsKeyword *x;
	for (x = next(); x != 0; x = next())
		if (strcmp(w,x->name()) == 0)
			break;
	return x ? curr() : 0;
}

int FitsKeywordList::rules(FitsKeyword &x, FITSErrorHandler errhandler) {
	// apply rules to x against the keyword list
	// return: 0 = no errors, 1 = minor errors, -1 = major errors

        static char msgstring[180]; // storage for composing error messages

	if (x.kw().name() == FITS::USER_DEF)
	    return 0;
	if (x.kw().name() == FITS::ERRWORD)
	    return -1;
	switch (x.kw().name()) {
	    case FITS::NAXIS:
		if (x.isindexed()) {
		    if ((*this)(FITS::NAXIS) == 0) {
			errhandler("There is no NAXIS keyword",
				   FITSError::SEVERE);
			return -1;
		    } else {
			if (x.index() >= 1 && x.index() <= curr()->asInt()) {
			    if (x.asInt() < 0) {
				ostringstream msgline;
				msgline << "Illegal value for keyword NAXIS"
					<< x.index();
				strncpy(msgstring, msgline.str().c_str(), sizeof(msgstring)-1);
				errhandler(msgstring, FITSError::SEVERE);
				return -1;
			    }
			}
		    }
		}
		break;
	    case FITS::END:	// there must be no comment
		if (x.commlen() != 0) {
		    errhandler("Comments are not allowed on keyword END",
			       FITSError::WARN);
		    return 1;
		}
		break;
	    case FITS::TBCOL:
		// for index between 1 and TFIELDS, value must be >= 0
		if ((*this)(FITS::TFIELDS) == 0) {
		    errhandler("There is no TFIELDS keyword",
			       FITSError::SEVERE);
		    return -1;
		} else {
		    if (x.index() >= 1 && x.index() <= curr()->asInt()) {
			if (x.asInt() < 0) {
			    ostringstream msgline;
			    msgline << "Illegal value for keyword TBCOL"
				    << x.index();
			    strncpy(msgstring, msgline.str().c_str(), sizeof(msgstring)-1);
			    errhandler(msgstring, FITSError::SEVERE);
			    return -1;
			}
		    }
		}
		break;
	    case FITS::BLANK:	// BITPIX must exist and be positive
		if ((*this)(FITS::BITPIX) == 0) {
		    errhandler("There is no BITPIX keyword", FITSError::SEVERE);
		    return -1;
		} else {
		    if (curr()->asInt() < 0) {
		        // Used to be an error. Make it a warning instead.
			errhandler("Keyword BLANK not allowed when BITPIX < 0",
				   FITSError::WARN);
			return 0;
		    }
		}
		break;
	    // The following "default" was added to prevent compilers
	    // such as GNU g++ from giving warnings about enumeration
	    // values not being handled.  This should be cleaned up
	    // some point.
	    //              -OO
	    default:
	      break;
	}
	return 0;
}

int FitsKeywordList::rules(FITSErrorHandler errhandler) {
	int rtn = 0;
	int n;
	FitsKeyword *endkey = 0;
	//first();
	FitsKeyword *x;
	for (x = beg_; x != 0; x = x->next_) {
	    n = rules(*x,errhandler);
	    if (n != 0 && (rtn == 0 || (rtn == 1 && n == -1)))
		    rtn = n;
	    if (x->isreserved() && (x->kw().name() == FITS::END)) {
		endkey = x;
		break;
	    }
	}
	if (!endkey) {
	    errhandler("Keyword list has no END keyword.", FITSError::SEVERE);
	    rtn = -1;
	} else {
	    for (x = x->next_; x != 0; x = x->next_) {
		if (!(x->isreserved() && (x->kw().name() == FITS::SPACES)
			 && x->commlen() == 0)) {
		    errhandler("END keyword is not the last keyword.",
			       FITSError::SEVERE);
		    rtn = -1;
		}
	    }
	}
	return rtn;
}

Bool FitsKeywordList::basic_rules() {
	int rtn = 0;
	const char *msg = 0;
	for (FitsKeyword *x = beg_; x != 0; x = x->next_) {
	   rtn = FITS::ResWord.rules(x->kw(),x->name(),x->namelen(),
		 x->isindexed(),x->type(),x->value(),x->valStrlen(),msg);
	   if (rtn != 0 || msg != 0)
		return False;
	}
	return True;
}

FitsKeyCardTranslator::FitsKeyCardTranslator(int max) : cardno(0),
	FitsCardSize(80), FitsMaxCard(36), FitsRecSize(2880), max_errs(max), 
	no_errs_(0) {
	err_ = new const char * [max_errs];
	err_cardno_ = new int [max_errs];
	blanks = new char [FitsRecSize];
	for (int i = 0; i < FitsRecSize; ++i)
	    blanks[i] = ' ';
	// check for storage allocation errors
}

FitsKeywordList &FitsKeyCardTranslator::parse(const char *buff, 
					      FitsKeywordList &kwlist, 
					      int count, 
					      FITSErrorHandler errhandler, 
					      Bool show_err) {
        char msgstring[180]; // storage for composing error messages
	int i, j;
	cardno = 0;
	int end_found = 0;
	for (i = 0; i < 36; ++i) {
	    ++cardno;
	    kwlist.parse(&buff[i*80],80);
	    if (show_err && (kwlist.no_parse_errs() > 0)) {
		FITSError::ErrorLevel errlev = FITSError::INFO;
		if (strcmp(kwlist.curr()->name(),"ERROR") == 0)
		    errlev = FITSError::WARN;
		ostringstream msgline;
		msgline << "FITS card " << (count * 36 + cardno) << ": ";
		msgline.write(&buff[i*80],80);
		strncpy(msgstring, msgline.str().c_str(), sizeof(msgstring)-1);
		errhandler(msgstring, errlev);
		for (j = 0; j < kwlist.no_parse_errs(); ++j) {
		    errhandler(kwlist.parse_err(j), errlev);
		}
	    }
	    if (end_found) {
		if (kwlist.curr()->isreserved() && 
		    kwlist.curr()->kw().name() == FITS::SPACES &&
		    kwlist.curr()->commlen() == 0)
		    kwlist.del();
		else {
		    if (no_errs_ < max_errs) {
			ostringstream msgline;
			msgline << "FITS card " 
				<< (count * 36 + cardno) << ": ";
			msgline.write(&buff[i*80],80);
			strncpy(msgstring, msgline.str().c_str(), sizeof(msgstring)-1);
			errhandler(msgstring, FITSError::WARN);
		    	errhandler("Invalid card after END keyword.",
				   FITSError::WARN);
		    }
		}
	    }
	    if (kwlist.curr()->isreserved() && 
	        kwlist.curr()->kw().name() == FITS::END)
	    {
		end_found = 1;
                break; // don't attempt to read beyond END card (fails on ASCII null)
	    }
	}
	
	return kwlist;
}

int FitsKeyCardTranslator::build(char *rec, FitsKeywordList &kw) {
	// Beginning at the current location in kw and at the beginning
	// of rec, reformat each keyword in kw for the FITS header output.
	// If the end of the list was encountered, return 0, otherwise 1.
	memcpy(rec,blanks,FitsRecSize);
	char *card = rec;
	FitsKeyword *x = kw.curr();
	for (cardno = 0; cardno < FitsMaxCard && x != 0;
		++cardno, card +=FitsCardSize, x = kw.next())
	    fmtcard(card,*x);
	return ( x ? 1 : 0 );
}

void FitsKeyCardTranslator::fmtcard(char *card, const FitsKeyword &k) {
	int i, n;
	char c[3];
	memcpy(card,k.name(),k.namelen());
	if (k.isreserved() && k.isindexed()) {
	    n = k.index();
	    for (i = 0; n > 0; ++i, n /= 10)
		c[i] = n % 10 + '0';
	    for (--i, n = k.namelen(); i >= 0; ++n, --i)
		card[n] = c[i];
	}
	if (k.type() == FITS::NOVALUE) {
	    if ((n = (k.commlen() <= 72 ? k.commlen() : 72)))
		memcpy(&card[8],k.comm(),n);
	} else if (k.type() == FITS::STRING) {
	    card[8] = '=';
	    card[10] = '\'';
	    n = FITS::str2fstr(&card[11],k.asString(),69);
	    card[11 + n] = '\'';
	    if (k.commlen()) {
	    	i = 14 + n;
	    	if (i <= 30) {
		    card[31] = '/';
		    if ((n = (k.commlen() <= 48 ? k.commlen() : 48)))
			memcpy(&card[32],k.comm(),n);
	        } else {
		    if (i < 80) {
			card[i - 1] = '/';
			if ((n = (k.commlen() <= (80 - i) ?
				k.commlen() : (80 - i))))
			    memcpy(&card[i],k.comm(),n);
		    }
	        }
	    }
 	} else {
	    card[8] = '=';
	    switch (k.type()) {
		case FITS::LOGICAL:
		    card[29] = (k.asBool() == True ? 'T' : 'F');
		    break;
		case FITS::LONG:
		    sprintf(&card[18],"%12d",k.asInt());
		    card[30] = ' ';
		    break;
		case FITS::FLOAT:
		    sprintf(&card[16],"%#14.7E",k.asFloat());
		    card[30] = ' ';
		    break;
		case FITS::DOUBLE:
		    sprintf(&card[10],"%#20.12E",k.asDouble()); // optimum %23.15E
		    for (i = 29; i < 10; --i) // change the E to a D
		        if (card[i] == 'E') { card[i] = 'D'; break; }
		    card[30] = ' ';
		    break;
		case FITS::ICOMPLEX:
		    sprintf(&card[18],"%12d",k.asIComplex().real());
		    card[30] = ' ';
		    sprintf(&card[38],"%12d",k.asIComplex().imag());
		    card[50] = ' ';
		    break;
		case FITS::COMPLEX:
		    sprintf(&card[16],"%#14.6E",k.asComplex().real());
		    sprintf(&card[36],"%#14.6E",k.asComplex().imag());
		    card[50] = ' ';
		    break;
		case FITS::DCOMPLEX:
		    sprintf(&card[10],"%#20.12E",k.asDComplex().real());
		    sprintf(&card[30],"%#20.12E",k.asDComplex().imag());
		    for (i = 29; i < 10; --i) // change E to a D
		        if (card[i] == 'E') { card[i] = 'D'; break; }
		    for (i = 49; i < 30; --i) // change E to a D
		        if (card[i] == 'E') { card[i] = 'D'; break; }
		    card[50] = ' ';
		    break;
		// The following "default" was added to prevent compilers
		// such as GNU g++ from giving warnings about enumeration
		// values not being handled.  This should be cleaned up
		// some point.
		//              -OO
	        default:
	            break;
	    }
	    if (k.commlen()) {
		if (!(k.type() == FITS::ICOMPLEX || 
		      k.type() == FITS::DCOMPLEX)) {
		    card[31] = '/';
		    if ((n = (k.commlen() <= 48 ? k.commlen() : 48)))
			memcpy(&card[32],k.comm(),n);
		} else {
		    card[51] = '/';
		    if ((n = (k.commlen() <= 28 ? k.commlen() : 28)))
			memcpy(&card[52],k.comm(),n);
		}
	    }
	}
}

// For some amazing reason the following wouldn't inline
FITS::ReservedName ReservedFitsKeyword::name() const {  return name_; }

Bool FitsFPUtil::isFP(const float *) {return True;}
Bool FitsFPUtil::isFP(const double *) {return True;}
Bool FitsFPUtil::isFP(const void *) {return False;}

void FitsFPUtil::setNaN(double &val)
{
    unsigned char *cptr = (unsigned char *)(&val);
    for (unsigned int i=0; i<sizeof(double); i++) {
	cptr[i] = 0xff;
    }
}

void FitsFPUtil::setNaN(float &val)
{
    unsigned char *cptr = (unsigned char *)(&val);
    for (unsigned int i=0; i<sizeof(float); i++) {
	cptr[i] = 0xff;
    }
}

} //# NAMESPACE CASACORE - END

