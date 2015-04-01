//# hdu2.cc:
//# Copyright (C) 1993,1994,1995,1996,1997,1999,2001,2002,2003
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

# include <casacore/fits/FITS/fits.h>
# include <casacore/fits/FITS/hdu.h>
# include <casacore/fits/FITS/fitsio.h>
# include <casacore/casa/string.h>
# include <casacore/casa/stdio.h>
# include <assert.h>
# include <casacore/casa/sstream.h>
//# include <casacore/casa/strsteam.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//== FitsBit specializations ==================================================
FitsField<FitsBit>::FitsField(int n) : FitsBase(FITS::BIT,n), field(0) { }
FitsField<FitsBit>::~FitsField() {
}
//=============================================================================
int FitsField<FitsBit>::fitsfieldsize() const {
	int n = no_elements / 8;
	if (no_elements % 8 != 0)
		++n;
	return n;
}
//=============================================================================
int FitsField<FitsBit>::localfieldsize() const {
	int n = no_elements / 8;
	if (no_elements % 8 != 0)
		++n;
	return n * sizeof(FitsBit);
}
//=============================================================================
void * FitsField<FitsBit>::data() {
	return *field;
}
//=============================================================================
void FitsField<FitsBit>::setaddr(void **addr) {
	field = (FitsBit **)addr;
}
//=============================================================================
void FitsField<FitsBit>::show(ostream &o) { 
	for (int i = 0; i < no_elements; ++i)
	    o << (int)((*this)(i));
}
//=================================================================================
FitsField<FitsBit> & FitsArray<FitsBit>::operator () (int d0, int d1) {
	FitsField<FitsBit> *tmp = this;
	return (*tmp)(d0 + d1 * factor[1]);
}
//=================================================================================
FitsField<FitsBit> & FitsArray<FitsBit>::operator () (int d0, int d1, int d2) {
	FitsField<FitsBit> *tmp = this;
	return (*tmp)(d0 + d1 * factor[1] + d2*factor[2]);
}
//=================================================================================
FitsField<FitsBit> & FitsArray<FitsBit>::operator () (int d0, int d1, int d2,
						      int d3) {
	FitsField<FitsBit> *tmp = this;
	return (*tmp)(d0 + d1 * factor[1] + d2*factor[2] + d3*factor[3]);
}
//=================================================================================
FitsField<FitsBit> & FitsArray<FitsBit>::operator () (int d0, int d1, int d2,
						      int d3, int d4) {
	FitsField<FitsBit> *tmp = this;
	return (*tmp)(d0 + d1 * factor[1] + d2*factor[2] + d3*factor[3] +
		      d4*factor[4]);
}
//=================================================================================
#if 0
FitsField<FitsBit> & FitsArray<FitsBit>::operator () (int d0, int d1, int d2,
						      int d3, int d4, int d5 ...) {
	FitsField<FitsBit> *tmp = this;
	if (dims() > 6) {
		int offset, i;
		va_list ap;
		offset = d0 + d1*factor[1] + d2*factor[2] + d3*factor[3] +
              d4*factor[4] + d5*factor[5];
		va_start(ap,d5);
		for (i = 6; i < dims(); ++i)
			offset += va_arg(ap,int) * factor[i];
		va_end(ap);
		return (*tmp)(offset);
	} else
		return (*tmp)(d0 + d1*factor[1] + d2*factor[2] + d3*factor[3] +
              d4*factor[4] + d5*factor[5]);
}
#endif

//== HeaderDataUnit ===========================================================

void HeaderDataUnit::errmsg(HDUErrs e, const char *s) {
    static char msgstring[180]; // storage for composing error messages
    ostringstream msgline;
    msgline << "HDU error:  " << s << endl;
    err_status = e;
    // all of the errors which use this function are SEVERE
    strncpy(msgstring, msgline.str().c_str(), sizeof(msgstring)-1);
    errfn(msgstring, FITSError::SEVERE);
}
//== determine_type of HeaderDataUnit ========================================

// This function determines the HDU type and the data type
Bool HeaderDataUnit::determine_type(FitsKeywordList &kw, FITS::HDUType &htype, 
	FITS::ValueType &dtype, FITSErrorHandler errhandler, HDUErrs &errstat) {
        //cout << "HeaderDataUnit::determine_type        kw=\n" << kw << endl;
	errstat = OK;
	// Get SIMPLE or XTENSION, BITPIX, NAXIS, and NAXIS1
	kw.first();
	FitsKeyword *word1 = kw.next();
        //cout << "word1=" << *word1 << endl;
	if (!word1) {
	    errstat = MISSKEY; 
	    errhandler("There are no keywords", FITSError::SEVERE);
	    htype = FITS::NotAHDU;
	    return False;
	}
	FitsKeyword *p_bitpix = kw.next();
      	FitsKeyword *naxis = kw.next();
	FitsKeyword *naxis1 = kw.next();
	FitsKeyword *naxis2 = kw.next();
	if (!p_bitpix || (p_bitpix->kw().name() != FITS::BITPIX)) {
	    p_bitpix = kw(FITS::BITPIX); // look for BITPIX elsewhere
	    if (!p_bitpix || (p_bitpix->kw().name() != FITS::BITPIX)) {
	        errstat = MISSKEY; 
		errhandler("Missing required BITPIX keyword", FITSError::WARN);
	    } else
		errhandler("Keyword BITPIX is out of order", FITSError::WARN);
	}
	if ((!naxis || 
	     !(naxis->kw().name() == FITS::NAXIS && naxis->index() == 0))) {
	    naxis = kw(FITS::NAXIS); // look for NAXIS elsewhere
	    if ((!naxis || 
	       !(naxis->kw().name() == FITS::NAXIS && naxis->index() == 0))) {
	        errstat = MISSKEY;
		errhandler("Missing required NAXIS keyword.", FITSError::WARN);
	    } else
		errhandler("Keyword NAXIS is out of order.", FITSError::WARN);
	}
        if ((errstat == OK) && (naxis->asInt() != 0)) {
            if (!naxis1 || 
        	!(naxis1->kw().name() == FITS::NAXIS && naxis1->index() == 1)) {
		naxis1 = kw(FITS::NAXIS,1);
		if (!naxis1 || 
        	   !(naxis1->kw().name() == FITS::NAXIS && 
			naxis1->index() == 1)) {
	            errstat = MISSKEY; 
		    errhandler("Missing required NAXIS1 keyword.", FITSError::WARN);
		} else
		    errhandler("Keyword NAXIS1 is out of order.", FITSError::WARN);
	    }
	}
	if (word1->kw().name() != FITS::SIMPLE &&
	    word1->kw().name() != FITS::XTENSION) {
	    word1 = kw(FITS::SIMPLE); // look for SIMPLE
	    if (!word1) {
		word1 = kw(FITS::XTENSION); // look for XTENSION
		if (word1)
		    errhandler("Keyword XTENSION is out of order.", FITSError::WARN);
		else
		    errhandler("Missing keywords SIMPLE and XTENSION.",
			       FITSError::WARN);
	    } else
		errhandler("Keyword SIMPLE is out of order.", FITSError::WARN);
	}
	if (!word1) {
	    errstat = BADREC; 
	    errhandler("Unrecognizeable record.", FITSError::SEVERE);
	}
	if (errstat != OK) {
	    htype = FITS::NotAHDU;
	    return False;
	}

	// OK, got'em
	Int bitpix = p_bitpix->asInt(); // get value of BITPIX
	switch (bitpix) {
	    case 8:   dtype = FITS::BYTE; break;
	    case 16:  dtype = FITS::SHORT; break;
	    case 32:  dtype = FITS::LONG; break;
	    case -32: dtype = FITS::FLOAT; break;
	    case -64: dtype = FITS::DOUBLE; break;
	    default:
		errstat = BADBITPIX; 
		errhandler("Invalid value of BITPIX", FITSError::SEVERE);
		htype = FITS::NotAHDU;
		return False;
	}
	if (word1->kw().name() == FITS::SIMPLE) {
            //cout << "naxis=" << naxis->asInt() << " naxis2=" << naxis2->asInt()
            //     << " naxis1=" << naxis1->asInt() << endl;
	    if (naxis->asInt() > 0) {
	        htype = FITS::PrimaryArrayHDU;
                if (naxis1->asInt() == 0)
	    	   htype = FITS::PrimaryGroupHDU;
	        else if (naxis->asInt() == 2 && 
                         (naxis2->asInt() == 0 && naxis1->asInt() == 777777701))
	    	   htype = FITS::PrimaryTableHDU;
                else
	           htype = FITS::PrimaryArrayHDU;
                //cout << "htype=" << htype << endl;
            } 
	    else 
	    	htype = FITS::PrimaryArrayHDU;
            //cout << "<<HeaderDataUnit::determine_type - simple - htype=" << htype << endl; 
	} else { // word1 is XTENSION
 	    if (strcmp(word1->asString(),"TABLE   ") == 0)
	    	htype = FITS::AsciiTableHDU;
	    else if (strcmp(word1->asString(),"BINTABLE") == 0)
	    	htype = FITS::BinaryTableHDU;
	    // For backward compatibility with many ancient NRAO files
	    // written by classic AIPS.
	    else if (strcmp(word1->asString(),"A3DTABLE") == 0)
	    	htype = FITS::BinaryTableHDU;
	    else if (strcmp(word1->asString(),"IMAGE   ") == 0)
	    	htype = FITS::ImageExtensionHDU;
	    else
	    	htype = FITS::UnknownExtensionHDU;
            //cout << "<<HeaderDataUnit::determine_type - extension- htype=" << htype << endl; 
	}
	return True;
}

//== compute_size of HeaderDataUnit ==========================================

// Compute the total size of the data associated with an HDU.  The number
// of dimensions is also determined.  This routine assumes that hdu type
// has been appropriately set, but it may be changed in the process.  Data
// type is also determined.
Bool HeaderDataUnit::compute_size(FitsKeywordList &kw, OFF_T &datasize,
	Int &dims, FITS::HDUType &htype, FITS::ValueType &dtype,
	FITSErrorHandler errhandler, HDUErrs &st) {

	datasize = 0;
	dims   = 0;
	dtype = FITS::NOVALUE;
	if (htype == FITS::NotAHDU)
		return True;

	Int bitpix = kw(FITS::BITPIX)->asInt(); // get value of BITPIX
	dims = kw(FITS::NAXIS)->asInt(); // get value of NAXIS
	switch (bitpix) {
	    case 8:   dtype = FITS::BYTE; break;
	    case 16:  dtype = FITS::SHORT; break;
	    case 32:  dtype = FITS::LONG; break;
	    case -32: dtype = FITS::FLOAT; break;
	    case -64: dtype = FITS::DOUBLE; break;
	    default:
		st = BADBITPIX; 
		errhandler("Invalid value of BITPIX", FITSError::SEVERE);
		htype = FITS::NotAHDU;
		return False;
	}
	if (dims == 0) // There is no data
	    return True;
	int n;
	FitsKeyword *naxisn;
	// Primary Array HDU
	if (htype == FITS::PrimaryArrayHDU) {
	    datasize = 1;
	    for (n = 1; n <= dims; n++) {
		   naxisn = kw.next();
        	if (!naxisn || !(naxisn->kw().name() == FITS::NAXIS && 
				 naxisn->index() == n)) {
		     naxisn = kw(FITS::NAXIS,n);
		     if (!naxisn || !(naxisn->kw().name() == FITS::NAXIS && 
				  naxisn->index() == n)) {
		    	  st = NOAXISN; 
			     errhandler("Missing required NAXISn keyword",
				                 FITSError::SEVERE);
		        datasize = 0;
		        htype = FITS::NotAHDU;
		        return False;
		      } else{
			     errhandler("NAXISn keyword is out of order.",
				     FITSError::WARN);
			   }
		   } // end of first if(!naxisn || ...).
		 datasize *= naxisn->asInt();
	  }// end of for loop.
	    datasize *= FITS::fitssize(dtype);
	    return True;
	}// end of if( htype == ...).
	// Primary Table HDU
	else if (htype == FITS::PrimaryTableHDU) {
	    //NAXIS1 = 777777701 and NAXIS2 = 0
	    datasize = 0; //by definition
	    return True;
	}
	// Primary Group HDU 
	else if ( htype == FITS::PrimaryGroupHDU) {
	    datasize = 1;
	    kw.next(); // skip NAXIS1;
	    for (n = 2; n <= dims; n++) {
		   naxisn = kw.next();
        	if (!naxisn || !(naxisn->kw().name() == FITS::NAXIS && 
				 naxisn->index() == n)) {
		      naxisn = kw(FITS::NAXIS,n);
		      if (!naxisn || !(naxisn->kw().name() == FITS::NAXIS && 
				   naxisn->index() == n)) {
		    	   st = NOAXISN; 
			      errhandler("Missing required NAXISn keyword",
				   FITSError::SEVERE);
		    	   datasize = 0;
		    	   htype = FITS::NotAHDU;
		    	   return False;
		      } else
			     errhandler("NAXISn keyword is out of order.",
				   FITSError::WARN);
		  }
	      	datasize *= naxisn->asInt();
	    }
	    if (!kw(FITS::PCOUNT)) {
		st = NOPCOUNT; 
		errhandler("Missing required PCOUNT keyword",
			   FITSError::SEVERE);
		datasize = 0;
		htype = FITS::NotAHDU;
		return False;
	    }
	    datasize += kw.curr()->asInt();		 
	    if (!kw(FITS::GCOUNT)) {
		st = NOGCOUNT; 
		errhandler("Missing required GCOUNT keyword",
			   FITSError::SEVERE);
		datasize = 0;
		htype = FITS::NotAHDU;
		return False;
	    }
	    datasize *= kw.curr()->asInt();		 
	    datasize *= FITS::fitssize(dtype);		 

	    if (!kw(FITS::GROUPS))
		{ 
		    st = NOGROUPS; 
		    errhandler("Missing required GROUPS keyword",
			       FITSError::WARN); 
		}
	    return True;
	} 

	// Image Extension HDU
	else if ( htype == FITS::ImageExtensionHDU ) {
	    datasize = 1;
	    for (n = 1; n <= dims; n++) {
		naxisn = kw.next();
        	if (!naxisn || 
        	    !(naxisn->kw().name() == FITS::NAXIS && 
			naxisn->index() == n)) {
		    naxisn = kw(FITS::NAXIS,n);
		    if (!naxisn || !(naxisn->kw().name() == FITS::NAXIS && 
				     naxisn->index() == n)) {
		    	st = NOAXISN; 
			errhandler("Missing required NAXISn keyword",
				   FITSError::SEVERE);
		        datasize = 0;
		        htype = FITS::NotAHDU;
		        return False;
		    } else
			errhandler("NAXISn keyword is out of order.",
				   FITSError::WARN);
		}
		datasize *= naxisn->asInt();
	    }
	    if (!kw(FITS::PCOUNT)) { 
		st = NOPCOUNT; 
		errhandler("Missing required PCOUNT keyword",
			   FITSError::WARN); 
	    } else if (kw.curr()->asInt() != 0) {
		st = BADPCOUNT; 
		errhandler("Invalid value of PCOUNT keyword",
			   FITSError::WARN); 
	    }
	    if (!kw(FITS::GCOUNT)) { 
		st = NOGCOUNT; 
		errhandler("Missing required GCOUNT keyword",
			   FITSError::WARN); 
	    } else if (kw.curr()->asInt() != 1) { 
		st = BADGCOUNT; 
		errhandler("Invalid value of GCOUNT keyword",
			   FITSError::WARN); 
	    }
	    datasize *= FITS::fitssize(dtype);
	    return True;

	} 

	// Conforming Extension HDU of unknown type
	else if ( htype == FITS::UnknownExtensionHDU ) {
	    datasize = 1;
	    for (n = 1; n <= dims; n++) {
		naxisn = kw.next();
        	if (!naxisn || !(naxisn->kw().name() == FITS::NAXIS && 
			         naxisn->index() == n)) {
		    naxisn = kw(FITS::NAXIS,n);
		    if (!naxisn || !(naxisn->kw().name() == FITS::NAXIS && 
				 naxisn->index() == n)) {
		    	st = NOAXISN; 
			errhandler("Missing required NAXISn keyword",
				   FITSError::SEVERE);
		        datasize = 0;
		        htype = FITS::NotAHDU;
		        return False;
		    } else
			errhandler("NAXISn keyword is out of order.",
				   FITSError::SEVERE);
		}
		datasize *= naxisn->asInt();
	    }
	    if (!kw(FITS::PCOUNT)) {
		st = NOPCOUNT; 
		errhandler("Missing required PCOUNT keyword",
			   FITSError::SEVERE);
		datasize = 0;
		htype = FITS::NotAHDU;
		return False;
	    }
	    datasize += kw.curr()->asInt();
	    if (!kw(FITS::GCOUNT)) {
		st = NOGCOUNT; 
		errhandler("Missing required GCOUNT keyword",
			   FITSError::SEVERE);
		datasize = 0;
		htype = FITS::NotAHDU;
		return False;
	    }
	    datasize *= kw.curr()->asInt();
	    datasize *= FITS::fitssize(dtype);
	    return True;

	} 

	// ASCII Table HDU
	else if ( htype == FITS::AsciiTableHDU ) {
	    if (FITS::fitssize(dtype) != 1) {
		st = BADBITPIX; 
		errhandler("BITPIX must be 8",
			   FITSError::SEVERE);
		htype = FITS::NotAHDU;
		return False;
	    }
	    dtype = FITS::CHAR; // This is the proper type
	    if (dims != 2) {
		st = BADNAXIS; 
		errhandler("NAXIS must be 2",
			   FITSError::SEVERE);
		htype = FITS::NotAHDU;
		return False;
	    }
	    datasize = 1;
	    for (n = 1; n <= dims; n++) {
	        naxisn = kw.next();
        	if (!naxisn || 
        	    !(naxisn->kw().name() == FITS::NAXIS && naxisn->index() == n)) {
		    st = NOAXISN; 
		    errhandler("Missing required NAXISn keyword",
			       FITSError::SEVERE);
		    datasize = 0;
		    htype = FITS::NotAHDU;
		    return False;
		}
		datasize *= naxisn->asInt();
	    }
	    FitsKeyword *pcount = kw.next();
	    if (!pcount ||
        	!(pcount->kw().name() == FITS::PCOUNT && pcount->index() == 0)) { 
		st = NOPCOUNT;
		errhandler("Missing required PCOUNT keyword",
			   FITSError::WARN); 
	    } else if (pcount->asInt() != 0) { 
		st = BADPCOUNT; 
		errhandler("PCOUNT must be 0", FITSError::WARN); 
	    }
	    FitsKeyword *gcount = kw.next();
	    if (!gcount ||
        	!(gcount->kw().name() == FITS::GCOUNT && gcount->index() == 0)) { 
		st = NOGCOUNT; 
		errhandler("Missing required GCOUNT keyword",
			   FITSError::WARN); 
	    } else if (gcount->asInt() != 1) { 
		st = BADGCOUNT; 
		errhandler("GCOUNT must be 1", FITSError::WARN); 
	    }
	    return True;

	} 

	// Binary Table HDU
	else if ( htype == FITS::BinaryTableHDU ) {
	    if (FITS::fitssize(dtype) != 1) {
		st = BADBITPIX; 
		errhandler("BITPIX must be 8", FITSError::SEVERE);
		htype = FITS::NotAHDU;
		return False;
	    }
	    if (dims != 2) {
		st = BADNAXIS; 
		errhandler("NAXIS must be 2", FITSError::SEVERE);
		htype = FITS::NotAHDU;
		return False;
	    }
	    datasize = 1;
	    for (n = 1; n <= dims; n++) {
	        naxisn = kw.next();
        	if (!naxisn || 
        	    !(naxisn->kw().name() == FITS::NAXIS && naxisn->index() == n)) {
		    st = NOAXISN; 
		    errhandler("Missing required NAXISn keyword",
			       FITSError::SEVERE);
		    datasize = 0;
		    htype = FITS::NotAHDU;
		    return False;
		}
		datasize *= naxisn->asInt();
	    }
	    FitsKeyword *pcount = kw.next();
	    if (!pcount ||
        	!(pcount->kw().name() == FITS::PCOUNT && pcount->index() == 0)) { 
		st = NOPCOUNT; 
		errhandler("Missing required PCOUNT keyword",
			   FITSError::WARN); 
	    } else {
	        datasize += pcount->asInt(); // The heap convention
	    }
	    FitsKeyword *gcount = kw.next();
	    if (!gcount ||
        	!(gcount->kw().name() == FITS::GCOUNT && gcount->index() == 0)) { 
		st = NOGCOUNT; 
		errhandler("Missing required GCOUNT keyword",
			   FITSError::WARN); 
	    } else if (gcount->asInt() != 1) { 
		st = BADGCOUNT; 
		errhandler("GCOUNT must be 1", FITSError::WARN); 
	    }
	    return True;
	}
	return False;
}
//============================================================================
HeaderDataUnit::~HeaderDataUnit() {
	delete &kwlist_;
	delete [] dimn;
}
//================================================================================
HeaderDataUnit::HeaderDataUnit(FitsInput &f, FITS::HDUType t, 
			       FITSErrorHandler errhandler) : 
	kwlist_(*(new FitsKeywordList)), constkwlist_(kwlist_), fin(&f),
	errfn(errhandler), err_status(OK), no_dims(0), dimn(0), 
	fits_data_size(0), data_type(FITS::NOVALUE), fits_item_size(0), 
	local_item_size(0), hdu_type(FITS::NotAHDU), pad_char('\0'), 
	double_null(FITS::mindouble), char_null('\0'), Int_null(FITS::minInt) {
	
	if (fin->hdutype() != t) {
	    errmsg(BADTYPE,"[HeaderDataUnit::HeaderDataUnit] "
                           "Input does not contain an HDU of this type.");
	    return;
	}
        //cout << ">>HeaderDataUnit::HeaderDataUnit - hdu_type=" << hdu_type 
        //     << " f.hdutype()=" << f.hdutype() 
        //     << " fin->hdutype()=" << fin->hdutype() << endl;
	hdu_type = fin->hdutype();
	data_type = fin->datatype();
	if (get_hdr(t,kwlist_) == -1) { // process the header records
	    hdu_type = fin->hdutype();
	    err_status = BADSIZE;
	    return;
	}
        //cout << "<<HeaderDataUnit::HeaderDataUnit + hdu_type=" << hdu_type 
        //     << " f.hdutype()=" << f.hdutype() 
        //     << " fin->hdutype()=" << fin->hdutype() << endl;
        //cout << "[HeaderDataUnit::HeaderDataUnit] kwlist_:\n" << kwlist_ << endl;
        if (hdu_type==FITS::PrimaryTableHDU) { 
            //cout << "[HeaderDataUnit::HeaderDataUnit] kwlist_:\n" << kwlist_ << endl;
            return;
        }
	fits_data_size = fin->datasize(); // assign values	
	fits_item_size = FITS::fitssize(data_type);
	local_item_size = FITS::localsize(data_type);

        //cout << "fits_data_size=" << fits_data_size 
        //     << "fits_item_size=" << fits_item_size 
        //     << "local_item_size=" << local_item_size 
        //     << endl;
	no_dims = kwlist_(FITS::NAXIS)->asInt();
        //cout << "[HeaderDataUnit::HeaderDataUnit] no_dims=" << no_dims << endl;

	if (no_dims > 0) {
	    if ((dimn = new Int [no_dims]) == 0) {
		errmsg(NOMEM,"[HeaderDataUnit::HeaderDataUnit] Cannot allocate memory.");
		no_dims = 0;
		return;
	    }
	    else {
		for (int i = 0; i < no_dims; i++)
		    dimn[i] = kwlist_(FITS::NAXIS,(i + 1))->asInt();
	    }
	}
        //cout << "<<HeaderDataUnit::HeaderDataUnit ~ hdu_type=" << hdu_type 
        //     << " f.hdutype()=" << f.hdutype() 
        //     << " fin->hdutype()=" << fin->hdutype() << endl;
}
//=================================================================================================
HeaderDataUnit::HeaderDataUnit(FitsKeywordList &k, FITS::HDUType t, 
			       FITSErrorHandler errhandler, FitsInput *f ) 
    : kwlist_(*new FitsKeywordList(k)), constkwlist_(kwlist_), fin(f), 
      errfn(errhandler), err_status(OK), no_dims(0),
      dimn(0), fits_data_size(0), data_type(FITS::NOVALUE),
      fits_item_size(0), local_item_size(0), hdu_type(FITS::NotAHDU), 
      pad_char('\0'), double_null(FITS::mindouble), char_null('\0'),
      Int_null(FITS::minInt) {
		
    if( !init_data_unit( t )){
	    return;
	 }
}
//=================================================================================================
// Use this constructor to construct objects that write only required keywords to fitsfile.
// the write method to call by these object should be those for the specific
// hdu, such as write_binTbl_hdr().
HeaderDataUnit::HeaderDataUnit( FITS::HDUType, 
			       FITSErrorHandler errhandler, FitsInput *f ) 
    : kwlist_(*new FitsKeywordList()), constkwlist_( kwlist_),fin(f),
	   errfn(errhandler), err_status(OK), no_dims(0),
      dimn(0), fits_data_size(0), data_type(FITS::NOVALUE),
      fits_item_size(0), local_item_size(0), hdu_type(FITS::NotAHDU), 
      pad_char('\0'), double_null(FITS::mindouble), char_null('\0'),
      Int_null(FITS::minInt) {
   // do not call init_data_unit() from here, since kwlist_ has no value yet.
	// init_data_unit() in this case will be called from write_XXX_hdr();
}
//================================================================================================
// Call this after write the specific header( and generate the FitsKeywordList
// the header info which has just been written.)
bool HeaderDataUnit::init_data_unit( FITS::HDUType t ){
   // kwlist_ is initialized in the constuctor or methods like write_bintbl_hdr()
	kwlist_.first();
	FitsKeyword *fkw = kwlist_.curr();
	if( fkw == 0 ){ 
		errmsg(BADRULES,"Header is not constructed/written yet![HeaderDataUnit::init_data_unit]");
		return false; 
	}else{ 
	   //constkwlist_= *(new ConstFitsKeywordList(kwlist_)); // This might need to be recovered!
		//ConstFitsKeywordList constfkwl( kwlist_);
		//constkwlist_= constfkwl;
   }

	if ((!kwlist_.basic_rules()) || (kwlist_.rules(errfn) != 0)) {
	    errmsg(BADRULES,"Errors in keyword list[HeaderDataUnit::init_data_unit]");
	    return false;
	}

	if (!determine_type(kwlist_,hdu_type,data_type,errfn,err_status)) {
	    errmsg(BADTYPE,"Could not determine HDU type from keyword list [HeaderDataUnit::init_data_unit]");
	    hdu_type = FITS::NotAHDU;
	    return false;
	}
	if (!compute_size(kwlist_,fits_data_size,no_dims,
		 hdu_type,data_type,errfn,err_status)) {
	    errmsg(BADSIZE,"Could not compute data size from keyword list[HeaderDataUnit::init_data_unit]");
	    hdu_type = FITS::NotAHDU;
	    return false;
	}	
	fits_item_size = FITS::fitssize(data_type);
	local_item_size = FITS::localsize(data_type);
	if (hdu_type != t) {
	    errmsg(BADTYPE,"Improper keyword list for this HDU type[HeaderDataUnit::init_data_unit]");
	    hdu_type = FITS::NotAHDU;
	    return false;
	}
	if (no_dims > 0) {
	   if ((dimn = new Int [no_dims]) == 0) {
		   errmsg(NOMEM,"Cannot allocate memory[HeaderDataUnit::init_data_unit]");
		   no_dims = 0;
		   return false;
	   }else {
	  	    for (int i = 0; i < no_dims; i++)
		    {   dimn[i] = kwlist_(FITS::NAXIS,(i + 1))->asInt();}
			 return true;
	   }
	}
	return true;
}
//===============================================================================================================
void HeaderDataUnit:: posEnd() {
	// Position the kwlist_ cursor before the `END' keyword
	// Assumption:  The `END' keyword is the last keyword.
	kwlist_.last();
	kwlist_.prev();
}
//==============================================================================
char * HeaderDataUnit::assign(FITS::ReservedName nm) {
	char *s;
	if (kwlist_(nm) != 0) {
	    if ((s = new char [kwlist_.curr()->valStrlen() + 1]) == 0)
		errmsg(NOMEM,"Cannot allocate memory");
	    else {
	        memcpy(s,kwlist_.curr()->asString(),kwlist_.curr()->valStrlen());
	        s[kwlist_.curr()->valStrlen()] = '\0';
	    }
	} else
	    s = &char_null;
	return s;
}
//==============================================================================
char * HeaderDataUnit::assign(FITS::ReservedName nm, int ndx) {
	char *s;
	if (kwlist_(nm,ndx) != 0) {
	    if ((s = new char [kwlist_.curr()->valStrlen() + 1]) == 0)
		errmsg(NOMEM,"Cannot allocate memory");
	    else {
	        memcpy(s,kwlist_.curr()->asString(),kwlist_.curr()->valStrlen());
	        s[kwlist_.curr()->valStrlen()] = '\0';
	    }
	} else
	    s = &char_null;
	return s;
}
//=============================================================================
Vector<String> HeaderDataUnit::kwlist_str(Bool length80){ return fin->kwlist_str(length80); }
//=============================================================================
int HeaderDataUnit::read_data(char *addr, Int nb) {
	return (fin ? fin->read(hdu_type,addr,nb) : 0); }
//=============================================================================
int HeaderDataUnit::write_data(FitsOutput &f, char *addr, Int nb) {
	return f.write(hdu_type,addr,nb,pad_char); }
//=============================================================================
OFF_T HeaderDataUnit::read_all_data(char *addr) {
	return (fin ? fin->read_all(hdu_type,addr) : 0); }
//=============================================================================
int HeaderDataUnit::write_all_data(FitsOutput &f, char *addr) {
	return f.write_all(hdu_type,addr,pad_char); }
//=============================================================================
int HeaderDataUnit::skip(uInt n) {
	return (fin ? fin->skip(hdu_type,n) : 0); }
//=============================================================================
int HeaderDataUnit::skip() {
	if (fin) fin->skip_all(hdu_type); return 0; }
//=============================================================================
int HeaderDataUnit::write_hdr(FitsOutput &f) {	
	if(f.write_hdr(kwlist_,hdu_type,data_type,fits_data_size,fits_item_size)){
	   return -1;
	}
	return 0;
}
//=============================================================================
int HeaderDataUnit::get_hdr(FITS::HDUType t, FitsKeywordList &kw) {
	return fin->process_header(t,kw); }
//=============================================================================
double HeaderDataUnit::asgdbl(FITS::ReservedName n, double x) {
    	if (kwlist_(n) == 0)
	    return x;
        else if (kwlist_.curr()->type() == FITS::DOUBLE)
	    return kwlist_.curr()->asDouble();
	else
	    return (double)(kwlist_.curr()->asFloat());
}
//=============================================================================
double HeaderDataUnit::asgdbl(FITS::ReservedName n, int i, double x) {
    	if (kwlist_(n,i) == 0)
	    return x;
        else if (kwlist_.curr()->type() == FITS::DOUBLE)
	    return kwlist_.curr()->asDouble();
	else
	    return (double)(kwlist_.curr()->asFloat());
}

//== ExtensionHeaderDataUnit =================================================

ExtensionHeaderDataUnit::ExtensionHeaderDataUnit(FitsInput &f,
						 FITSErrorHandler errhandler) 
    : HeaderDataUnit(f,FITS::UnknownExtensionHDU,errhandler) {
    ex_assign();
}
//============================================================================
ExtensionHeaderDataUnit::ExtensionHeaderDataUnit(FitsInput &f, 
						 FITS::HDUType t, 
						 FITSErrorHandler errhandler) 
    : HeaderDataUnit(f,t,errhandler) {
    ex_assign();
}
//============================================================================
ExtensionHeaderDataUnit::ExtensionHeaderDataUnit(FitsKeywordList &k, 
						 FITSErrorHandler errhandler) 
    : HeaderDataUnit(k,FITS::UnknownExtensionHDU,errhandler,0) {
    ex_assign();
}
//============================================================================
ExtensionHeaderDataUnit::ExtensionHeaderDataUnit(FitsKeywordList &k, 
						 FITS::HDUType t, 
						 FITSErrorHandler errhandler) 
    : HeaderDataUnit(k,t,errhandler,0) {
    ex_assign();
}
//============================================================================
ExtensionHeaderDataUnit::ExtensionHeaderDataUnit(FITS::HDUType t, 
						 FITSErrorHandler errhandler) 
    : HeaderDataUnit( t,errhandler,0) {
    ex_assign();
}
//============================================================================
ExtensionHeaderDataUnit::~ExtensionHeaderDataUnit() {
	if (xtension_x != &char_null)
	    delete [] xtension_x;
	if (extname_x != &char_null)
	    delete [] extname_x;
}
//====================================================================================
void ExtensionHeaderDataUnit::ex_assign() {
	extver_x = kwlist_(FITS::EXTVER) == 0 ? FITS::minInt : kwlist_.curr()->asInt();
	extlevel_x = kwlist_(FITS::EXTLEVEL) == 0 ? FITS::minInt : kwlist_.curr()->asInt();
	pcount_x = kwlist_(FITS::PCOUNT) == 0 ? FITS::minInt : kwlist_.curr()->asInt();
	gcount_x = kwlist_(FITS::GCOUNT) == 0 ? FITS::minInt : kwlist_.curr()->asInt();
	xtension_x = assign(FITS::XTENSION);
	extname_x = assign(FITS::EXTNAME);
}
//== FitsField and related classes ===================================================
FitsBase::~FitsBase() { 
}
//====================================================================================
int FitsBase::dims() const { 
	return 1; 
}
//====================================================================================
int FitsBase::dim(int n) const { 
	return (n == 0 ? no_elements : 0); 
}
//====================================================================================
int *FitsBase::vdim() { 
	return &no_elements; 
}
//====================================================================================
FitsBase & FitsBase::operator = (FitsBase &x) {
	if (fieldtype() == x.fieldtype() &&
	    nelements() == x.nelements())
	    memcpy(data(),x.data(),localfieldsize());
	return *this;
}
//====================================================================================
FitsBase * FitsBase::make(const FITS::ValueType &type,int n) {
	switch (type) {
	    case FITS::LOGICAL: return (new FitsField<FitsLogical> (n));
	    case FITS::BIT: return (new FitsField<FitsBit> (n));
	    case FITS::CHAR: return (new FitsField<char> (n));
	    case FITS::BYTE: return (new FitsField<unsigned char> (n));
	    case FITS::SHORT: return (new FitsField<short> (n));
	    case FITS::LONG: return (new FitsField<FitsLong> (n));
	    case FITS::FLOAT: return (new FitsField<float> (n));
	    case FITS::DOUBLE: return (new FitsField<double> (n));
	    case FITS::COMPLEX: return (new FitsField<Complex> (n));
	    case FITS::ICOMPLEX: return (new FitsField<IComplex> (n));
	    case FITS::DCOMPLEX: return (new FitsField<DComplex> (n));
	    case FITS::VADESC: return (new FitsField<FitsVADesc> (n));
	    // The following "default" has been added to prevent compilers
	    // such as GNU g++ from complaining about the rest of FITS
	    // enumerations not being handled in the switch statement.
	    //           -OO
	    default:
	        assert(0);
	        break;
	}
	return 0;
}
//========================================================================================
FitsBase * FitsBase::make(const FITS::ValueType &type,int n, int *d) {
	switch (type) {
	    case FITS::LOGICAL: return (new FitsArray<FitsLogical> (n,d));
	    case FITS::BIT: return (new FitsArray<FitsBit> (n,d));
	    case FITS::CHAR: return (new FitsArray<char> (n,d));
	    case FITS::BYTE: return (new FitsArray<unsigned char> (n,d));
	    case FITS::SHORT: return (new FitsArray<short> (n,d));
	    case FITS::LONG: return (new FitsArray<FitsLong> (n,d));
	    case FITS::FLOAT: return (new FitsArray<float> (n,d));
	    case FITS::DOUBLE: return (new FitsArray<double> (n,d));
	    case FITS::COMPLEX: return (new FitsArray<Complex> (n,d));
	    case FITS::ICOMPLEX: return (new FitsArray<IComplex> (n,d));
	    case FITS::DCOMPLEX: return (new FitsArray<DComplex> (n,d));
	    case FITS::VADESC: return (new FitsArray<FitsVADesc> (n,d));
	    // The following "default" has been added to prevent compilers
	    // such as GNU g++ from complaining about the rest of FITS
	    // enumerations not being handled in the switch statement.
	    //           -OO
	    default:
	        assert(0);
	        break;
	}
	return 0;
}
//=======================================================================================
FitsBase * FitsBase::make(FitsBase &x) {
	if (x.dims() == 1)
	    return make(x.fieldtype(),x.nelements());
	return make(x.fieldtype(),x.dims(),x.vdim());
}

//== AsciiTableExtension =====================================================

AsciiTableExtension::AsciiTableExtension(FitsInput &f, 
					 FITSErrorHandler errhandler) : 
	BinaryTableExtension(f,FITS::AsciiTableHDU,errhandler) {
	pad_char = ' ';
	at_assign();	
}
//============================================================================
AsciiTableExtension::AsciiTableExtension(FitsKeywordList &k,
					 FITSErrorHandler errhandler) : 
	BinaryTableExtension(k,FITS::AsciiTableHDU,errhandler) {
	pad_char = ' ';
	at_assign();	
}
//===========================================================================
AsciiTableExtension::AsciiTableExtension(FITSErrorHandler errhandler):
	BinaryTableExtension(FITS::AsciiTableHDU,errhandler) {
	pad_char = ' ';
	//	at_assign(); // this is done within write_asctbl_hdr();	
}
//===========================================================================
AsciiTableExtension::~AsciiTableExtension() {
	int i;
	if (tfields_x > 0) {
	    for (i = 0; i < tfields_x; i++) {
		if (tnulla_x[i] != &char_null)
		    delete tnulla_x[i];
                delete [] format[i];
	    }
	    delete [] tnulla_x;
            delete [] format;
	    delete [] tbcol_x;
	    delete [] fits_width;
	}
}
//===========================================================================
void AsciiTableExtension::at_assign() {
	int i, n, ne;
	size_t row_align;
	const char *s;
	char typecode;

   tfields_x = 0;		// first initialize everything
	tbcol_x = 0;
	tform_x = 0;
	tscal_x = 0;
	tzero_x = 0;
	isatnull_x = 0;
	tnull_x = 0;
	tnulla_x = 0;
	ttype_x = 0;
	tunit_x = 0;
	tdisp_x = 0;
	tdim_x = 0;
	theap_x = 0;
	author_x = 0;
	referenc_x = 0;
	fld = 0;
	fits_offset = 0;
	fits_width = 0;
	format = 0;
	table_offset = 0;
	data_addr = 0;
	alloc_row = 0;
	table = 0;
	fitsrow = 0;
	tablerowsize = 0;
	fitsrowsize = 0;
   isoptimum = False;
	beg_row = 0;
	end_row = 0;
	curr_row = 0;
	if (err_status != OK)
            return;

	// Assign values from keywords
	if (kwlist_(FITS::TFIELDS) == 0) {
	    errmsg(MISSKEY,"Missing required TFIELDS keyword");
	    tfields_x = 0;
	} else
	    tfields_x = kwlist_.curr()->asInt();
	if (tfields_x < 0 || tfields_x > 999) {
	    errmsg(BADSIZE,"Invalid value for TFIELDS keyword");
	    tfields_x = 0;
	}
	theap_x = Int_null;
	author_x = assign(FITS::AUTHOR);
	referenc_x = assign(FITS::REFERENC);
	if (tfields_x == 0)
            return;
	tbcol_x = new Int [tfields_x];
	tform_x = new char * [tfields_x];
	tscal_x = new double [tfields_x];
	tzero_x = new double [tfields_x];
	isatnull_x = new Bool [tfields_x];
	tnull_x = new int [tfields_x];
	tnulla_x = new char * [tfields_x];
	ttype_x = new char * [tfields_x];
	tunit_x = new char * [tfields_x];
	tdisp_x = new char * [tfields_x];
	tdim_x = new char * [tfields_x];
	if (tbcol_x == 0  || tform_x == 0    || tscal_x == 0 || 
	    tzero_x == 0  || isatnull_x == 0 || tnull_x == 0 ||
	    tnulla_x == 0 || ttype_x == 0    || tunit_x == 0 || 
	    tdisp_x == 0  || tdim_x == 0) {
	    errmsg(NOMEM,"Cannot allocate memory");
	    return;
	}
	for (i = 0; i < tfields_x; i++) {
	    tbcol_x[i] = kwlist_(FITS::TBCOL,(i + 1)) == 0 ?
	     			Int_null : kwlist_.curr()->asInt();
	    tform_x[i] = assign(FITS::TFORM,(i + 1));
	    tscal_x[i] = asgdbl(FITS::TSCAL,(i + 1),1.0);
	    tzero_x[i] = asgdbl(FITS::TZERO,(i + 1),0.0);
	    isatnull_x[i] = False;
	    tnull_x[i] = Int_null;
	    if (kwlist_(FITS::TNULL,(i + 1)) != 0) {
	       if (kwlist_.curr()->type() == FITS::STRING) {
	   	   if ((tnulla_x[i] = 
		           new char [kwlist_.curr()->valStrlen() + 1]) == 0)
		       errmsg(NOMEM,"Cannot allocate memory");
	    	   else {
		       memcpy(tnulla_x[i],kwlist_.curr()->asString(),
		           kwlist_.curr()->valStrlen());
	       	       tnulla_x[i][kwlist_.curr()->valStrlen()] = '\0';
		   }
	        } else {
		    errmsg(BADTYPE,"Invalid value for keyword TNULL.");
		    tnulla_x[i] = &char_null;
		}
	    } else
		tnulla_x[i] = &char_null;
	    ttype_x[i] = assign(FITS::TTYPE,(i + 1));
	    tunit_x[i] = assign(FITS::TUNIT,(i + 1));
	    tdisp_x[i] = &char_null; 
	    tdim_x[i] = &char_null;
	}

	// Allocate space for field pointer and create the fields
	fld = new FitsBase * [tfields()];
	fits_offset = new uInt [tfields()];
	fits_width = new uInt [tfields()];
        format = new char * [tfields()];
	table_offset = new uInt [tfields()];
        data_addr = new void * [tfields()];
	if (fld == 0 || fits_offset == 0 || fits_width == 0 || 
	    format == 0 || table_offset == 0 || data_addr == 0) {
	    errmsg(NOMEM,"Cannot allocate memory");
	    return;
	}
	for (i = 0; i < tfields(); ++i) {
	    format[i] = new char [strlen(tform(i)) + 3]; // the new format
	    if (format[i] == 0) {
		errmsg(NOMEM,"Cannot allocate memory");
		return;
	    }
            n = 0;
            format[i][n++] = '%';
	    for (s = tform(i); *s == ' '; ++s) {} // skip leading blanks
	    typecode = *s++; // code indicating kind of field
	    fits_width[i] = 1; // get the fits field width
	    if (FITS::isa_digit(*s)) {
            	format[i][n++] = *s;
		fits_width[i] = FITS::digit2bin(*s++);
		while (FITS::isa_digit(*s)) {
            	    format[i][n++] = *s;
		    fits_width[i] = fits_width[i] * 10 + FITS::digit2bin(*s++);
		}
		if (typecode == 'F' || typecode == 'E' || typecode == 'D') {
		    if (*s == '.') {
			format[i][n++] = '.';
			++s;
			while (FITS::isa_digit(*s))
			    format[i][n++] = *s++;
		    }
                }
	    }
	    switch (typecode) {
		case 'A': fld[i] = new FitsField<char>(fits_width[i]);
			  format[i][n++] = 's'; break;
		case 'I': fld[i] = new FitsField<FitsLong>(1);
                	  format[i][n++] = 'd'; break;
		case 'F': fld[i] = new FitsField<float>(1);
			  format[i][n++] = 'f'; break;
		case 'E': fld[i] = new FitsField<float>(1);
			  format[i][n++] = 'E'; break;
		case 'D': fld[i] = new FitsField<double>(1);
			  format[i][n++] = 'E'; break;
		default:
		    errmsg(BADRULES,"Invalid type code for TFORM");
		    fld[i] = 0;
		    break;
	    }
            format[i][n] = '\0'; // formats are converted

	}
	for (i = 0; i < tfields(); ++i) 
	    if (fld[i] == 0)		// if any fields were not constructed
		return;			// bail out

	for (i = 0; i < tfields(); ++i)
	    fld[i]->setaddr(&data_addr[i]); // set field addresses

	// set FITS rowsize and compute tablerowsize
	fitsrowsize = dim(0);
	tablerowsize = 0;
	for (i = 0; i < tfields(); ++i)
	    tablerowsize += fld[i]->localfieldsize();
	isoptimum = False;

	// Determine field offsets for FITS and table rows
	for (i = 0; i < tfields(); ++i) {
	    if ( tbcol(i) < 1 || tbcol(i) > (int)fitsrowsize) {
		errmsg(BADRULES,"Invalid value for TBCOL keyword");
		return;
	    }
	    fits_offset[i] = tbcol(i) - 1;
	}
	// compute offsets for table rows
	n = 0; // the number of offsets computed
        ne = 0; // the current offset
	row_align = 0; // find the row alignment requirement
	for (i = 0; i < tfields(); ++i)
	    if (fld[i]->fieldtype() == FITS::DOUBLE) {
		table_offset[i] = ne; 
		ne += fld[i]->localfieldsize(); 
		++n;
		if (sizeof(double) > row_align)
		    row_align = sizeof(double); 
	    }
	if (n < tfields())
	    for (i = 0; i < tfields(); ++i)
		if (fld[i]->fieldtype() == FITS::FLOAT) {
		    table_offset[i] = ne; 
		    ne += fld[i]->localfieldsize();
		    ++n;
		    if (sizeof(float) > row_align)
		        row_align = sizeof(float); 
		}
	if (n < tfields())
	    for (i = 0; i < tfields(); ++i)
		if (fld[i]->fieldtype() == FITS::LONG) {
		    table_offset[i] = ne; 
		    ne += fld[i]->localfieldsize();
		    ++n;
		    if (sizeof(FitsLong) > row_align)
		        row_align = sizeof(FitsLong); 
		}
	if (n < tfields())
	    for (i = 0; i < tfields(); ++i)
		if (fld[i]->fieldtype() == FITS::CHAR) {
		    table_offset[i] = ne; 
		    ne += fld[i]->localfieldsize();
		    ++n;
		    if (sizeof(unsigned char) > row_align)
		        row_align = sizeof(unsigned char); 
		}

	// check row alignment
	if ((tablerowsize % row_align) != 0)
	    tablerowsize += row_align - (tablerowsize % row_align);

	// set data buffers and associated bounds markers
	fitsrow = new unsigned char [fitsrowsize];
	if (fitsrow == 0) {
	    errmsg(NOMEM,"Cannot allocate memory");
	    return;
	}
	beg_row = -1;
	end_row = -1;
	curr_row = -1;

}
//=====================================================================================
int AsciiTableExtension::readrow() {
	FitsValueResult res;
	if (read_data((char *)fitsrow,fitsrowsize) !=  (int)fitsrowsize)
	    return -1;

	  // must convert ASCII data to binary
	  for (int i = 0; i < tfields(); ++i) {
	    if (fld[i]->fieldtype() == FITS::CHAR)
	   	memcpy(fld[i]->data(),&fitsrow[fits_offset[i]],fits_width[i]);
	    else {
		   FITS::get_numeric((char *)(&fitsrow[fits_offset[i]]),
				  fits_width[i],res);
		   if (res.errmsg) {
		      errmsg(BADCONV,"Error converting data in current row.");
		      return -1;
		   }
		   if (res.type != fld[i]->fieldtype()) {
		     errmsg(BADCONV,"Error converting data in current row.");
		     return -1;
		   }
		   switch (res.type) {
		      case FITS::LONG:
			     *((FitsLong *)(fld[i]->data())) = res.l;
			     break;
		      case FITS::FLOAT:
			     *((float *)(fld[i]->data())) = res.f;
		   	  break;
		      case FITS::DOUBLE:
			     *((double *)(fld[i]->data())) = res.d;
		   	  break;
		      default:
		        errmsg(BADCONV,"Error converting data in current row.");
			     return -1;
		  }
	   }
	}

	return 0;
}
//======================================================================================
int AsciiTableExtension::writerow(FitsOutput &fout) {
	// must convert binary data row to ASCII
	char tmp[32];
	char *s, *t;
	int i;
	unsigned int n;
	for (i = 0; i < tfields(); ++i) {
	    if (fld[i]->fieldtype() == FITS::CHAR) {
		t = (char *)&fitsrow[fits_offset[i]];
		s = (char *)(fld[i]->data());
		for (n = 0; n < fits_width[i] && *s != '\0'; ++n)
		    *t++ = *s++;
		if (*s ==  '\0')
		    for(; n < fits_width[i]; ++n)
		        *t++ = ' ';    		    
	    } else {
		switch (fld[i]->fieldtype()) {
	    	    case FITS::LONG:
			sprintf(tmp,format[i],*((FitsLong *)(fld[i]->data())));
			if (strlen(tmp) > fits_width[i]) {
	    	            errmsg(BADCONV,
	      "Ascii Table conversion error: numeric value exceeds field size");
			    for (t = (char *)&fitsrow[fits_offset[i]], n = 0;
			    	n < fits_width[i]; ++n, ++t)
			    	*t = ' '; // fill with blanks   		    
			} else
	    		    memcpy(&fitsrow[fits_offset[i]],tmp,fits_width[i]);
			break;
		    case FITS::FLOAT:
			sprintf(tmp,format[i],*((float *)(fld[i]->data())));
			memcpy(&fitsrow[fits_offset[i]],tmp,fits_width[i]);
			break;
		    case FITS::DOUBLE:
			sprintf(tmp,format[i],*((double *)(fld[i]->data())));
			for (t = &tmp[strlen(tmp) - 2]; *t != 'E'; --t) {}
			*t = 'D'; // Change the 'E' to a 'D' in the format
         memcpy(&fitsrow[fits_offset[i]],tmp,fits_width[i]);
			break;
	    // The following "default" has been added to prevent compilers
	    // such as GNU g++ from complaining about the rest of FITS
	    // enumerations not being handled in the switch statement.
	    //           -OO
		    default:
		        assert(0);
		        break;
	        }
	    }
	}

	return write_data(fout,(char *)fitsrow,fitsrowsize);
}
//=========================================================================================================
// Put required Header keywords into the ASCII Table:
/*Write the ASCII table header keywords into the CHU. The optional TUNITn and
  EXTNAME keywords are written only if the input pointers are not null. A null
  pointer may given for the *tbcol parameter in which case a single space will
  be inserted between each column of the table. Similarly, if rowlen is
  given = 0, then CFITSIO will calculate the default rowlength based on the
  tbcol and ttype values.  
*/
int AsciiTableExtension::write_ascTbl_hdr( FitsOutput &fout, // I - FITS output object
           long naxis1,         // I - width of row in the table(number of ascii chars)
           long naxis2,         // I - number of rows in the table       
           int tfields,         // I - number of columns in the table      
           const char **ttype,  // I - name of each column                    
           long *tbcol,         // I - byte offset in row to each column       
           const char **tform,  // I - value of TFORMn keyword for each column 
           const char **tunit,  // I - value of TUNITn keyword for each column 
           const char *extname) // I - value of EXTNAME keyword, if any                                  
                  
{
	// flush m_buffer first
	fout.getfout().flush_buffer();

	if ( fout.rectype() == FITS::InitialState) {
		errmsg(BADOPER,"[AsciiTableExtension::write_ascTbl_hdr()] Primary Header must be written first.");
	   return -1;
	}
	if (!fout.hdu_complete()) {
	   errmsg(BADOPER,"[AsciiTableExtension::write_ascTbl_hdr()] Previous HDU incomplete -- cannot write header.");
	   return -1;
	}
	if (!fout.isextend()) {
		errmsg(BADOPER,"[AsciiTableExtension::write_ascTbl_hdr()] Cannot write extension HDU - EXTEND not True.");
		return -1;
	}
	if( !fout.required_keys_only() ){
	   cerr << "\n[AsciiTableExtension::write_ascTbl_hdr()] write_ascTbl_hdr() works with other write_***_hdr()" << endl;
		cerr << "methods only. It will not work with write_hdr()." << endl;
		errmsg(BADOPER,"Used wrong header-writting function." );
		return -1;
	}
	int l_status = 0; 
	// Since the original file pointer does not have the hdu info about the hdu created by
	// write_hdu() method, we reopen the file to get a new file pointer with all the hdu info.
	// This may cause some loss of efficiency. But so far I have not found a better way.
	char * l_filename = new char[ 80 ];
	if(ffflnm( fout.getfptr(), l_filename, &l_status )){ errmsg(BADOPER,"[AsciiTableExtension::write_ascTbl_hdr()] fflnm() failed!");}
	fitsfile* l_newfptr = 0;
	l_status = 0;
	if (ffopen( &l_newfptr, l_filename, READWRITE, &l_status )){
	   errmsg(BADOPER,"[AsciiTableExtension::write_ascTbl_hdr()] ffreopen() CHDU failed!");
      fits_report_error(stderr, l_status); // print error report
      return -1;
	}

	// Create, initialize, and move the i/o pointer to a new extension appended to the end of the FITS file.
	l_status = 0;
	if(ffcrhd(l_newfptr, &l_status)){
		errmsg(BADOPER,"[AsciiTableExtension::write_ascTbl_hdr() Create new HDU failed!");
	   fits_report_error(stderr, l_status); // print error report
		return -1;
	}
	// write the required keywords for AsciiTableExtension to fitsfile.  
	if(ffphtb( l_newfptr, naxis1, naxis2, tfields,
		   const_cast<char**>(ttype), tbcol,
		   const_cast<char**>(tform),
		   const_cast<char**>(tunit),
		   const_cast<char*>(extname), &l_status)){
	   errmsg(BADOPER,"[AsciiTableExtension::write_ascTbl_hdr()] Write HDU header failed!");
	   fits_report_error(stderr, l_status); // print error report
		return -1;
	}
	OFF_T l_headstart, l_datastart, l_dataend;
	l_status = 0;
   // get size info of the current HDU
   if (ffghof(l_newfptr, &l_headstart, &l_datastart, &l_dataend, &l_status) > 0){
      fits_report_error(stderr, l_status); // print error report
      return -1;
	}
	// move file pointer to the beginning of the new hdu.
	l_status = 0;
	if(ffmbyt(l_newfptr, l_headstart, REPORT_EOF, &l_status)){
	 	fits_report_error(stderr, l_status); // print error report
		return -1;
	}
	// using the cfitsio function to read bytes from the file
  	// pointed to by getfptr() from where the file position indicator currently at.
  	l_status = 0;
	char * l_headerbytes = new char[ l_datastart - l_headstart + 1];
	if(ffgbyt(l_newfptr, l_datastart - l_headstart, l_headerbytes, &l_status)){                   
		fits_report_error(stderr, l_status); // print error report
      return -1;
	}
	// ffgbyt() sometimes does not move bytepos to the new position. So we do it.
	(l_newfptr->Fptr)->bytepos = l_datastart;

	fout.setfptr( l_newfptr );           // update the file pointer in FitsOutput.
	fout.getfout().setfptr( l_newfptr ); // update the file pointer in BlockOutput.

   // now parse the headerbytes into kwlist_. init_data_unit will use kwlist_.
	char* l_header = &l_headerbytes[0];
	OFF_T l_usedbytes = 0;
	while( l_usedbytes < (l_datastart - l_headstart )){
  	   fout.getkc().parse( l_header, kwlist_ ,0, errfn,True);
		l_usedbytes = l_usedbytes + fout.fitsrecsize();
		l_header = &l_headerbytes[ l_usedbytes ];	
	}
	// init the info for the data unit
	init_data_unit( FITS::AsciiTableHDU );
   // assign the asciitable. This is done in constructor for the case when user
	// is required to provide a FitsKeywordList object.
	at_assign();
	// set the info for data unit. init_data_unit() generated the hdu_type, data_type,
	// fits_data_size and fits_item_size
   fout.set_data_info(kwlist_,hdu_type,data_type,fits_data_size,fits_item_size);

   return 0;
}
//=================================================================================================
BinaryTableExtension::BinaryTableExtension(FitsInput &f, 
					   FITSErrorHandler errhandler) 
    : ExtensionHeaderDataUnit(f,FITS::BinaryTableHDU,errhandler) {
	bt_assign();	
}
//=================================================================================================
BinaryTableExtension::BinaryTableExtension(FitsKeywordList &k,
					   FITSErrorHandler errhandler) 
    : ExtensionHeaderDataUnit(k,FITS::BinaryTableHDU,errhandler) {
	bt_assign();
}
//=================================================================================================
// this constructor does not require a FitskeywordList from the user, which is
// used for the situation when to create a header that contains only required
// keywords. The related write method for this constructor is write_binTbl_hdr().
BinaryTableExtension::BinaryTableExtension( FITSErrorHandler errhandler)
    : ExtensionHeaderDataUnit( FITS::BinaryTableHDU,errhandler) {
	//bt_assign(); // this is done within write_bintbl_hdr();
}
//=================================================================================================
// this constructor does not require a FitskeywordList from the user, which is used
// by AsciiTableExtension for the situation when to create a header that contains only 
// required keywords. The related write method for this constructor is write_asctbl_hdr().
BinaryTableExtension::BinaryTableExtension( FITS::HDUType hdutype, FITSErrorHandler errhandler)
    : ExtensionHeaderDataUnit( hdutype,errhandler) {
	 // AsciiTableExtension calls this and then calls at_assign().
}
//=================================================================================================
BinaryTableExtension::BinaryTableExtension(FitsInput &f, FITS::HDUType t,
					   FITSErrorHandler errhandler) 
    : ExtensionHeaderDataUnit(f,t,errhandler) {
	 // AsciiTableExtension calls this and then calls at_assign().
}
//=================================================================================================
BinaryTableExtension::BinaryTableExtension(FitsKeywordList &k, FITS::HDUType t,
					   FITSErrorHandler errhandler) 
    : ExtensionHeaderDataUnit(k,t,errhandler) {
	 // AsciiTableExtension calls this and then calls at_assign().
}
//================================================================================================
BinaryTableExtension::~BinaryTableExtension() {
	int i;
	if (author_x != &char_null)
	    delete [] author_x;
	if (referenc_x != &char_null)
	    delete [] referenc_x;
	if (tfields_x > 0) {
	    delete [] tscal_x;
	    delete [] tzero_x;
	    for (i = 0; i < tfields_x; i++) {
		if (tform_x[i] != &char_null)
	    	    delete [] tform_x[i];
		if (ttype_x[i] != &char_null)
	    	    delete [] ttype_x[i];
		if (tunit_x[i] != &char_null)
	    	    delete [] tunit_x[i];
		if (tdisp_x[i] != &char_null)
	    	    delete [] tdisp_x[i];
		if (tdim_x[i] != &char_null)
	    	    delete [] tdim_x[i];
		delete fld[i];
	    }
	    delete [] tform_x;
	    delete [] isatnull_x;
	    delete [] tnull_x;
	    delete [] ttype_x;
	    delete [] tunit_x;
	    delete [] tdisp_x;
	    delete [] tdim_x;
	}
	delete [] table;
	delete [] fld;
	delete [] table_offset;
	delete [] fits_offset;
        delete [] data_addr;
	if (!isoptimum)
	    delete [] fitsrow;
}
//==========================================================================
void BinaryTableExtension::bt_assign() {
	int i, j, n;
	size_t row_align;
	uInt ne;
	const char *s;
	const char *p;
	int *dd;
	int nd;

	tfields_x = 0;		// first initialize everything
	tform_x = 0;
	tscal_x = 0;
	tzero_x = 0;
	isatnull_x = 0;
	tnull_x = 0;
	ttype_x = 0;
	tunit_x = 0;
	tdisp_x = 0;
	tdim_x = 0;
	theap_x = 0;
	author_x = 0;
	referenc_x = 0;
	fld = 0;
	fits_offset = 0;
	table_offset = 0;
	data_addr = 0;
	alloc_row = 0;
	table = 0;
	fitsrow = 0;
	tablerowsize = 0;
	fitsrowsize = 0;
        isoptimum = False;
	beg_row = 0;
	end_row = 0;
	curr_row = 0;
	if (err_status != OK)
            return;

	if (kwlist_(FITS::TFIELDS) == 0) {
	    errmsg(MISSKEY,"Missing required TFIELDS keyword");
	    tfields_x = 0;
	} else
	    tfields_x = kwlist_.curr()->asInt();
	if (tfields_x < 0 || tfields_x > 999) {
	    errmsg(BADSIZE,"Invalid value for TFIELDS keyword");
	    tfields_x = 0;
	}
	theap_x = kwlist_(FITS::THEAP) == 0 ? Int_null : kwlist_.curr()->asInt();
	author_x = assign(FITS::AUTHOR);
	referenc_x = assign(FITS::REFERENC);
	if (tfields_x == 0)
            return;
	tform_x = new char * [tfields_x];
	tscal_x = new double [tfields_x];
	tzero_x = new double [tfields_x];
	isatnull_x = new Bool [tfields_x];
	tnull_x = new int [tfields_x];
	ttype_x = new char * [tfields_x];
	tunit_x = new char * [tfields_x];
	tdisp_x = new char * [tfields_x];
	tdim_x = new char * [tfields_x];
	if (tform_x == 0    || tscal_x == 0 || tzero_x == 0 || 
	    isatnull_x == 0 || tnull_x == 0 || ttype_x == 0 || 
	    tunit_x == 0    || tdisp_x == 0 || tdim_x == 0) {
	    errmsg(NOMEM,"Cannot allocate memory");
	    return;
	}
	for (i = 0; i < tfields_x; i++) {
	    tform_x[i] = assign(FITS::TFORM,(i + 1));
	    tscal_x[i] = asgdbl(FITS::TSCAL,(i + 1),1.0);
	    tzero_x[i] = asgdbl(FITS::TZERO,(i + 1),0.0);
	    if (kwlist_(FITS::TNULL,(i + 1)) == 0) {
	        isatnull_x[i] = False;
	        tnull_x[i] = Int_null;
	    } else {
	        if (kwlist_.curr()->type() != FITS::LONG) {
		    errmsg(BADTYPE,"Invalid value for keyword TNULL.");
	            isatnull_x[i] = False;
	            tnull_x[i] = Int_null;
	        } else {
	            tnull_x[i] = kwlist_.curr()->asInt();
	            isatnull_x[i] = True;
	        }
	    }
	    ttype_x[i] = assign(FITS::TTYPE,(i + 1));
	    tunit_x[i] = assign(FITS::TUNIT,(i + 1));
	    tdisp_x[i] = assign(FITS::TDISP,(i + 1));
	    tdim_x[i] = assign(FITS::TDIM,(i + 1));
	}

	// Allocate space for field pointer and create the fields
	fld = new FitsBase * [tfields()];
	fits_offset = new uInt [tfields()];
	table_offset = new uInt [tfields()];
        data_addr = new void * [tfields()];
	if (fld == 0 || fits_offset == 0 || table_offset == 0 ||
	    data_addr == 0) {
	    errmsg(NOMEM,"Cannot allocate memory");
	    return;
	}
	for (i = 0; i < tfields(); ++i) {
            for (s = tform(i); *s == ' '; ++s) {} // skip leading blanks
	    ne = 1; // ne is the number of elements in the field
	    if (FITS::isa_digit(*s)) {
		ne = FITS::digit2bin(*s++);
		while (FITS::isa_digit(*s))
		    ne = ne * 10 + FITS::digit2bin(*s++);
	    }
	    p = tdim(i);
	    if (*p != '\0') { // the multidimensional array case
		nd = 0;
                // get the dimensions, and store them in dd
		while (*p == ' ') ++p; // skip spaces
		if (*p != '(') { 
		    errmsg(BADRULES,"Invalid syntax in TDIM keyword"); 
		    return; 
		}
		++p;
		nd = 0; // the number of dimensions
		for (j = 0; p[j] != '\0' && p[j] != ')'; ++j)
		    if (p[j] == ',') ++nd;
		++nd;
		dd = new int [nd]; 
		if (dd == 0) { 
		    errmsg(NOMEM,"Could not allocate memory"); 
		    return; 
		}
		for (j = 0; j < nd; ++j) {
		    while (*p == ' ') ++p; // skip spaces
		    if (!FITS::isa_digit(*p)) { 
			errmsg(BADRULES,"Invalid syntax in TDIM keyword"); 
			return; 
		    }
		    dd[j] = FITS::digit2bin(*p++);
		    while (FITS::isa_digit(*p))
			dd[j] = dd[j] * 10 + FITS::digit2bin(*p++);
		    while (*p == ' ') ++p; // skip spaces
		    if (*p == ',')
			++p;
		    else if (*p == ')')
			break;
		    else { 
			errmsg(BADRULES,"Invalid syntax in TDIM keyword"); 
			return; 
		    }
		}
		switch (*s) {
		  case 'L': fld[i] = new FitsArray<FitsLogical>(nd,dd); break;
		  case 'X': fld[i] = new FitsArray<FitsBit>(nd,dd); break;
		  case 'B': fld[i] = new FitsArray<unsigned char>(nd,dd); break;
		  case 'I': fld[i] = new FitsArray<short>(nd,dd); break;
		  case 'J': fld[i] = new FitsArray<FitsLong>(nd,dd); break;
		  case 'A': fld[i] = new FitsArray<char>(nd,dd); break;
		  case 'E': fld[i] = new FitsArray<float>(nd,dd); break;
		  case 'D': fld[i] = new FitsArray<double>(nd,dd); break;
		  case 'C': fld[i] = new FitsArray<Complex>(nd,dd); break;
		  case 'M': fld[i] = new FitsArray<DComplex>(nd,dd); break;
		  case 'P': fld[i] = new FitsArray<FitsVADesc>(nd,dd); break;
		  default:
		    errmsg(BADRULES,"Invalid type code for TFORM");
		    fld[i] = 0;
		    break;
	        }
		delete [] dd;
	        if (fld[i] == 0) {
		    errmsg(NOMEM,"Cannot allocate memory");
		    return;
		}
		if (fld[i]->nelements() != ne) {
                    errmsg(BADRULES,"Arraysize does not match fieldsize in TFORM");
		    return;
		}
	    } else { // the single array case
	        switch (*s) {
		  case 'L': fld[i] = new FitsField<FitsLogical>(ne); break;
		  case 'X': fld[i] = new FitsField<FitsBit>(ne); break;
		  case 'B': fld[i] = new FitsField<unsigned char>(ne); break;
		  case 'I': fld[i] = new FitsField<short>(ne); break;
		  case 'J': fld[i] = new FitsField<FitsLong>(ne); break;
		  case 'A': fld[i] = new FitsField<char>(ne); break;
		  case 'E': fld[i] = new FitsField<float>(ne); break;
		  case 'D': fld[i] = new FitsField<double>(ne); break;
		  case 'C': fld[i] = new FitsField<Complex>(ne); break;
		  case 'M': fld[i] = new FitsField<DComplex>(ne); break;
		  case 'P': fld[i] = new FitsField<FitsVADesc>(ne); break;
		  default:
		    errmsg(BADRULES,"Invalid type code for TFORM");
		    fld[i] = 0;
		    break;
	        }
	        if (fld[i] == 0) {
		    errmsg(NOMEM,"Cannot allocate memory");
		    return;
	        }
	    }
	}

	for (i = 0; i < tfields(); ++i)
	    fld[i]->setaddr(&data_addr[i]); // set field addresses

	// compute FITS rowsize and tablerowsize
	fitsrowsize = 0;
	tablerowsize = 0;
	isoptimum = True;
	for (i = 0; i < tfields(); ++i) {
	    fitsrowsize += fld[i]->fitsfieldsize();
	    tablerowsize += fld[i]->localfieldsize();
	    if (fld[i]->fitsfieldsize() != fld[i]->localfieldsize())
		isoptimum = False;
	}
	
	// check for consistency
	if ((int)fitsrowsize != dim(0)) {
	    errmsg(BADRULES,"Size of FITS row does not match NAXIS1");
	    return;
	}

	// Check field alignment and compute FITS offsets
	// This criteria for alignment will probably work on most
	// machines, but there may be some weird cases out there.
	fits_offset[0] = 0;
	for (i = 1; i < tfields(); ++i) {
	    fits_offset[i] = fits_offset[i - 1] + fld[i - 1]->fitsfieldsize();
	    n = FITS::fitssize(fld[i]->fieldtype());
	    if (n > (int)sizeof(double))// since DComplex is implemented in 
		n = sizeof(double);     // terms of doubles, this is sufficient
	    if ((fits_offset[i] % n) != 0)
		isoptimum = False;
	}

	if (isoptimum) {
	    for (i = 0; i < tfields(); ++i)
		table_offset[i] = fits_offset[i];
	    // Must find the row alignment requirement
	    n = 0; // the number of fields scanned
	    row_align = 0; // the row alignment requirement
	    for (i = 0; i < tfields(); ++i)
		if (fld[i]->fieldtype() == FITS::DCOMPLEX ||
		    fld[i]->fieldtype() == FITS::DOUBLE) {
		    ++n;
		    if (sizeof(double) > row_align)
		        row_align = sizeof(double); 
		}
	    if (n < tfields())
		for (i = 0; i < tfields(); ++i)
		    if (fld[i]->fieldtype() == FITS::COMPLEX ||
			fld[i]->fieldtype() == FITS::FLOAT) {
			++n;
		        if (sizeof(float) > row_align)
		            row_align = sizeof(float); 
		        }
	    if (n < tfields())
		for (i = 0; i < tfields(); ++i)
		    if (fld[i]->fieldtype() == FITS::VADESC ||
		        fld[i]->fieldtype() == FITS::LONG) {
			++n;
		        if (sizeof(FitsLong) > row_align)
		            row_align = sizeof(FitsLong); 
		        }
	    if (n < tfields())
		for (i = 0; i < tfields(); ++i)
		    if (fld[i]->fieldtype() == FITS::SHORT) {
			++n;
		        if (sizeof(short) > row_align)
		            row_align = sizeof(short); 
		        }
	    if (n < tfields())
		for (i = 0; i < tfields(); ++i)
		    if (fld[i]->fieldtype() == FITS::BYTE ||
		        fld[i]->fieldtype() == FITS::CHAR ||
		        fld[i]->fieldtype() == FITS::LOGICAL ||
		        fld[i]->fieldtype() == FITS::BIT) {
			++n;
		        if (sizeof(unsigned char) > row_align)
			    row_align = sizeof(unsigned char);
		        }
	} else {
	    // Must compute separate offsets for local fields and align them.
	    n = 0; // the number of offsets computed
            ne = 0; // the current offset
	    row_align = 0; // find the row alignment requirement
	    for (i = 0; i < tfields(); ++i)
		if (fld[i]->fieldtype() == FITS::DCOMPLEX ||
		    fld[i]->fieldtype() == FITS::DOUBLE) {
		    table_offset[i] = ne; 
		    ne += fld[i]->localfieldsize(); 
		    ++n;
		    if (sizeof(double) > row_align)
		        row_align = sizeof(double); 
		}
	    if (n < tfields())
		for (i = 0; i < tfields(); ++i)
		    if (fld[i]->fieldtype() == FITS::COMPLEX ||
			fld[i]->fieldtype() == FITS::FLOAT) {
		        table_offset[i] = ne; 
			ne += fld[i]->localfieldsize();
			++n;
		        if (sizeof(float) > row_align)
		            row_align = sizeof(float); 
		        }
	    if (n < tfields())
		for (i = 0; i < tfields(); ++i)
		    if (fld[i]->fieldtype() == FITS::VADESC ||
		        fld[i]->fieldtype() == FITS::LONG) {
		        table_offset[i] = ne; 
			ne += fld[i]->localfieldsize();
			++n;
		        if (sizeof(FitsLong) > row_align)
		            row_align = sizeof(FitsLong); 
		        }
	    if (n < tfields())
		for (i = 0; i < tfields(); ++i)
		    if (fld[i]->fieldtype() == FITS::SHORT) {
		        table_offset[i] = ne; 
			ne += fld[i]->localfieldsize();
			++n;
		        if (sizeof(short) > row_align)
		            row_align = sizeof(short); 
		        }
	    if (n < tfields())
		for (i = 0; i < tfields(); ++i)
		    if (fld[i]->fieldtype() == FITS::BYTE ||
		        fld[i]->fieldtype() == FITS::CHAR ||
		        fld[i]->fieldtype() == FITS::LOGICAL ||
		        fld[i]->fieldtype() == FITS::BIT) {
		        table_offset[i] = ne; 
			ne += fld[i]->localfieldsize();
			++n;
		        if (sizeof(unsigned char) > row_align)
			    row_align = sizeof(unsigned char);
		        }
	}

	// check row alignment
	if ((tablerowsize % row_align) != 0) {
	    isoptimum = False;
	    // must add padding to a table row
            tablerowsize += row_align - (tablerowsize % row_align);
	}

//#     We can never take the 'optimum' route if we have to convert fields
#       if defined(AIPS_LITTLE_ENDIAN)
        isoptimum = False;
#       endif

	// set data buffers and associated bounds markers
	alloc_row = 0;
	table = 0;
	for (i = 0; i < tfields(); ++i)
            data_addr[i] = 0;
	if (isoptimum)
	    fitsrow = table;
	else {
	    fitsrow = new unsigned char [fitsrowsize];
	    if (fitsrow == 0) {
	        errmsg(NOMEM,"Cannot allocate memory");
	        return;
	    }
	}
	beg_row = -1;
	end_row = -1;
	curr_row = -1;
}
//=====================================================================================
int BinaryTableExtension::readrow() {
	int i;
	if (read_data((char *)fitsrow,fitsrowsize) != (int)fitsrowsize)
	    return -1;
	//cout<<"[BinaryTableExtension::readrow()] One row of data read." << endl;
	if (!isoptimum) {
	    for (i = 0; i < tfields(); ++i) {
	      int ne = fld[i]->nelements();
              void *src = &fitsrow[fits_offset[i]];
	      switch(fld[i]->fieldtype()) {
	        case FITS::LOGICAL:
		  FITS::f2l((FitsLogical *)(fld[i]->data()),src,ne); break;
		case FITS::BIT:
		  FITS::f2l((FitsBit *)(fld[i]->data()),src,ne); break;
		case FITS::CHAR:
		  FITS::f2l((char *)(fld[i]->data()),src,ne); break;
		case FITS::BYTE:
		  FITS::f2l((unsigned char *)(fld[i]->data()),src,ne); break;
		case FITS::SHORT:
		  FITS::f2l((short *)(fld[i]->data()),src,ne); break;
		case FITS::LONG:
		  FITS::f2l((FitsLong *)(fld[i]->data()),src,ne); break;
		case FITS::FLOAT:
		  FITS::f2l((float *)(fld[i]->data()),src,ne); break;
		case FITS::DOUBLE:
		  FITS::f2l((double *)(fld[i]->data()),src,ne); break;
		case FITS::COMPLEX:
		  FITS::f2l((Complex *)(fld[i]->data()),src,ne); break;
		case FITS::DCOMPLEX:
		  FITS::f2l((DComplex *)(fld[i]->data()),src,ne); break;
		case FITS::VADESC:
		  FITS::f2l((FitsVADesc *)(fld[i]->data()),src,ne); break;
	    // The following "default" has been added to prevent compilers
	    // such as GNU g++ from complaining about the rest of FITS
	    // enumerations not being handled in the switch statement.
	    //           -OO
	        default:
		  assert(0);
		  break;
	      }
      }	
	}
	return 0;
}
//========================================================================================
int BinaryTableExtension::writerow(FitsOutput &fout) {
	int i;
	if (!isoptimum) {
	    for (i = 0; i < tfields(); ++i) {
	      int ne = fld[i]->nelements();
              void *tg = &fitsrow[fits_offset[i]];
	      switch(fld[i]->fieldtype()) {
	        case FITS::LOGICAL:
		  FITS::l2f(tg,(FitsLogical *)(fld[i]->data()),ne); break;
		case FITS::BIT:
		  FITS::l2f(tg,(FitsBit *)(fld[i]->data()),ne); break;
		case FITS::CHAR:
		  FITS::l2f(tg,(char *)(fld[i]->data()),ne); break;
		case FITS::BYTE:
		  FITS::l2f(tg,(unsigned char *)(fld[i]->data()),ne); break;
		case FITS::SHORT:
		  FITS::l2f(tg,(short *)(fld[i]->data()),ne); break;
		case FITS::LONG:
		  FITS::l2f(tg,(FitsLong *)(fld[i]->data()),ne); break;
		case FITS::FLOAT:
		  FITS::l2f(tg,(float *)(fld[i]->data()),ne); break;
		case FITS::DOUBLE:
		  FITS::l2f(tg,(double *)(fld[i]->data()),ne); break;
		case FITS::COMPLEX:
		  FITS::l2f(tg,(Complex *)(fld[i]->data()),ne); break;
		case FITS::DCOMPLEX:
		  FITS::l2f(tg,(DComplex *)(fld[i]->data()),ne); break;
		case FITS::VADESC:
		  FITS::l2f(tg,(FitsVADesc *)(fld[i]->data()),ne); break;
	    // The following "default" has been added to prevent compilers
	    // such as GNU g++ from complaining about the rest of FITS
	    // enumerations not being handled in the switch statement.
	    //           -OO
	        default:
		  assert(0);
		  break;
	      }
            }
	}
	return write_data(fout,(char *)fitsrow,fitsrowsize);
}
//================================================================================
int BinaryTableExtension::set_next(int n) {
	// check if n rows have been allocated
	if (n > (int)alloc_row) {
	    delete [] table; // must allocate more rows
	    table = new unsigned char [n * tablerowsize];
	    if (table == 0) {
	    	errmsg(NOMEM,"Cannot allocate memory");
	    	return -1;
	    }
	    alloc_row = n;
        }
	// update row markers
	beg_row = end_row + 1;
	end_row = beg_row + n - 1;
	curr_row = beg_row;
	set_fitsrow(beg_row);
        return n;
}
//================================================================================
void BinaryTableExtension::set_fitsrow(Int n) {
	curr_row = n;
	unsigned char *addr = &table[(curr_row - beg_row) * tablerowsize];
	if (isoptimum)
            fitsrow = addr;
	// update field addresses
	for (int i = 0; i < tfields(); ++i)
	    data_addr[i] =  (void *)(addr + table_offset[i]);
}

int BinaryTableExtension::write(FitsOutput &fout) {
	OFF_T n;
	if (isoptimum) {
            n = (end_row - beg_row + 1) * fitsrowsize;
	    //return ((write_data(fout,(char *)table,n) == n) ? 0 : -1);
		 return (write_data(fout,(char *)table,n)); // It was above. GYL
	} else {
	    // write rows from beg_row to end_row
	    for (n = uInt(beg_row); n <= uInt(end_row); ++n) {
	        set_fitsrow(n);
	        if (writerow(fout) == -1)
		    return -1;
	    }        
	}
	return 0;	
}
//================================================================================
/*Put required Header keywords into the Binary Table:
  Write the binary table header keywords into the CHU. The optional
  TUNITn and EXTNAME keywords are written only if the input pointers
  are not null. The pcount parameter, which specifies the size of the
  variable length array heap, should initially = 0; CFITSIO will
  automatically update the PCOUNT keyword value if any variable length
  array data is written to the heap. The TFORM keyword value for variable
  length vector columns should have the form 'Pt(len)' or '1Pt(len)'
  where `t' is the data type code letter (A,I,J,E,D, etc.) and `len' is
  an integer specifying the maximum length of the vectors in that column
  (len must be greater than or equal to the longest vector in the column).
  If `len' is not specified when the table is created (e.g., the input
  TFORMn value is just '1Pt') then CFITSIO will scan the column when the
  table is first closed and will append the maximum length to the TFORM
  keyword value. Note that if the table is subsequently modified to
  increase the maximum length of the vectors then the modifying program
  is responsible for also updating the TFORM keyword value. GYL 
*/
int BinaryTableExtension::write_binTbl_hdr( FitsOutput &fout, // I - FITS output object
           long naxis2,           // I - number of rows in the table             
           int tfields,           // I - number of columns in the table          
           const char **ttype,    // I - name of each column                     
           const char **tform,    // I - value of TFORMn keyword for each column 
           const char **tunit,    // I - value of TUNITn keyword for each column 
           const char *extname,   // I - value of EXTNAME keyword, if any        
           long pcount )          // I - size of the variable length heap area

{
	// flush m_buffer first
	fout.getfout().flush_buffer();

	if ( fout.rectype() == FITS::InitialState) {
		errmsg(BADOPER,"[BinaryTableExtension::write_bintbl_hdr()] Primary Header must be written first.");
	   return -1;
	}
	if (!fout.hdu_complete()) {
	   errmsg(BADOPER,"[BinaryTableExtension::write_bintbl_hdr()] Previous HDU incomplete -- cannot write header.");
	   return -1;
	}
	if (!fout.isextend()) {
		errmsg(BADOPER,"[BinaryTableExtension::write_bintbl_hdr()] Cannot write extension HDU - EXTEND not True.");
		return -1;
	}
	if( !fout.required_keys_only() ){
	   cerr << "\n[BinaryTableExtension::write_binTbl_hdr()] write_binTbl_hdr() works with other write_***_hdr()" << endl;
		cerr << "methods only. It will not work with write_hdr()." << endl;
		errmsg(BADOPER,"Used wrong header-writting function." );
		return -1;
	}
	int l_status = 0; 
	// Since the original file pointer does not have the hdu info about the hdu created by
	// write_hdu() method, we reopen the file to get a new file pointer with all the hdu info.
	// This may cause some loss of efficiency. But so far I have not found a better way.
	char * l_filename = new char[ 80 ];
	if(ffflnm( fout.getfptr(), l_filename, &l_status )){ errmsg(BADOPER,"[BinaryTableExtension::write_bintbl_hdr()] fflnm() failed!");}
	fitsfile* l_newfptr = 0;
	l_status = 0;
	if (ffopen( &l_newfptr, l_filename, READWRITE, &l_status )){
	   errmsg(BADOPER,"[BinaryTableExtension::write_bintbl_hdr()] ffreopen() CHDU failed!");
      fits_report_error(stderr, l_status); // print error report
      return -1;
	}
	// Create, initialize, and move the i/o pointer to a new extension appended to the end of the FITS file.
	l_status = 0;
	if(ffcrhd(l_newfptr, &l_status)){
		errmsg(BADOPER,"[BinaryTableExtension::write_bintbl_hdr()] Create new HDU failed!");
	   fits_report_error(stderr, l_status); // print error report
		return -1;
	}
	// write the required keywords for BinaryTableExtension to fitsfile.  
	if(ffphbn( l_newfptr, naxis2, tfields,
		   const_cast<char**>(ttype),
		   const_cast<char**>(tform),
		   const_cast<char**>(tunit),
		   const_cast<char*>(extname),
		   pcount, &l_status)){
	   errmsg(BADOPER,"[BinaryTableExtension::write_bintbl_hdr()] Write HDU header failed!");
	   fits_report_error(stderr, l_status); // print error report
		return -1;
	}
	// flush the buffer of CFITSIO so that the contents will be actually written to disk.
	// oopes, if we call ffflsh() in this case, the fits file is damaged! why?
   if(ffflsh(l_newfptr, TRUE, &l_status)){ errmsg(BADOPER,"[PrimaryArray::write_priArr_hdr()] Error flushing buffer!"); }
	OFF_T l_headstart, l_datastart, l_dataend;
	l_status = 0;
   // get size info of the current HDU
   if (ffghof(l_newfptr, &l_headstart, &l_datastart, &l_dataend, &l_status) > 0){
      fits_report_error(stderr, l_status); // print error report
      return -1;
	}
	// move file pointer to the beginning of the new hdu.
	l_status = 0;
	if(ffmbyt(l_newfptr, l_headstart, REPORT_EOF, &l_status)){
	   errmsg(BADOPER,"Moving to headstart failed![BinaryTableExtension::write_bintbl_hdr()]");
	 	fits_report_error(stderr, l_status); // print error report
		return -1;
	}
	// using the cfitsio function to read bytes from the file
  	// pointed to by getfptr() from where the file position indicator currently at.
  	l_status = 0;
	char * l_headerbytes = new char[l_datastart - l_headstart + 1];
	if(ffgbyt(l_newfptr, l_datastart - l_headstart, l_headerbytes, &l_status)){
	   errmsg(BADOPER,"ffgbyt() failed![BinaryTableExtension::write_bintbl_hdr()]");                   
		fits_report_error(stderr, l_status); // print error report
      return -1;
	}
	// ffgbyt() sometimes does not move bytepos to the new position. So we do it.
	(l_newfptr->Fptr)->bytepos = l_datastart;
		
	fout.setfptr( l_newfptr );           // update the file pointer in FitsOutput.
	fout.getfout().setfptr( l_newfptr ); // update the file pointer in BlockOutput.
	// now parse the headerbytes into kwlist_. init_data_unit will use kwlist_.
	char* l_header = &l_headerbytes[0];
	OFF_T l_usedbytes = 0;
	while( l_usedbytes < (l_datastart - l_headstart )){
  	   fout.getkc().parse( l_header, kwlist_ ,0, errfn,True);
		l_usedbytes = l_usedbytes + fout.fitsrecsize();
		l_header = &l_headerbytes[ l_usedbytes ];	
	}
	// init the info for the data unit
	init_data_unit( FITS::BinaryTableHDU );
	// assign the binary table. This is done in constructor for the case when user
	// is required to provide a FitsKeywordList object.
	bt_assign();
	// set the info for data unit. init_data_unit() generated the hdu_type, data_type,
	// fits_data_size and fits_item_size
   fout.set_data_info(kwlist_,hdu_type,data_type,fits_data_size,fits_item_size);
   return 0;
}
//=====================================================================================
int BinaryTableExtension::read() {
	// read entire table into memory
	int nr = fitsdatasize() / fitsrowsize;
        return read(nr);
}
//=====================================================================================
int BinaryTableExtension::read(int nr){
	int i;
	if (nr < 1)
            return -1;
	if (set_next(nr) == -1) // check buffer allocation
	    return -1;
	//read next nr rows into memory
	if (isoptimum) {
       i = nr * fitsrowsize;
	    return ((read_data((char *)table,i) == i) ? 0 : -1);
	} else {
	    // read next nr rows
	    for (i = beg_row; i <= end_row; ++i) {
		   if (readrow() == -1)
		     return -1;
		   ++(*this);
	    }
	    // set curr_row to the beginning row and set field addresses
       set_fitsrow(beg_row);
	} 
	return 0;
}
//===================================================================================
BinaryTableExtension & BinaryTableExtension::operator ++ () {
	// increment curr_row and reset field addresses
	++curr_row;
	set_fitsrow(curr_row);
        return *this;
}
//===================================================================================
BinaryTableExtension & BinaryTableExtension::operator -- () {
	// decrement curr_row and reset field addresses
	--curr_row;
	set_fitsrow(curr_row);
        return *this;
}
//===================================================================================
BinaryTableExtension & BinaryTableExtension::operator () (int n) {
	// set curr_row to n and reset field addresses
	curr_row = n;
	set_fitsrow(curr_row);
	return *this;
}
//===================================================================================
int BinaryTableExtension::bind(int no, FitsBase &f) {
	int i;
	// check if f's attributes matches field[no]
	if (f.fieldtype() != fld[no]->fieldtype() ||
	    // The last test is commented out since it seems reasonable
	    // to allow a FitsField to be used where a FitsArray is, e.g.
	    // always allow one-D arrays to be used for N-D.
	    f.nelements() != fld[no]->nelements()  /* ||
	    f.dims()      != fld[no]->dims() */) {
	    errmsg(BADRULES,"Variable type does not match this column.");
	    return -1;
        }
	if (f.dims() > 1) {
	    for (i = 0; i < f.dims(); ++i)
		if (f.dim(i) != fld[no]->dim(i)) {
	    	    errmsg(BADRULES,"Variable type does not match this column.");
	    	    return -1;
        	}
	}
	// set f address to field[no] data's address
	f.setaddr(&data_addr[no]);
	return 0;
}
//===================================================================================
// We must specify FitsArray<Bit> as a specialized class.
FitsArray<FitsBit>::FitsArray(int n, const int *d) : 
	FitsField<FitsBit>(1) {
	int i;
	if (n > 0) {
	    no_dims = n;
	    dimn = new int [no_dims];
	    factor = new int [no_dims];
	    dimn[0] = d[0];
	    no_elements = dimn[0];
	    for (i = 1; i < no_dims; ++i) {
		dimn[i] = d[i]; 
		no_elements *= d[i]; 
	    }
	    factor[0] = 1;
	    for (i = 1; i < no_dims; ++i)
		factor[i] = factor[i - 1] * dimn[i - 1];
	} else {
	    no_dims = 1;
	    dimn = 0;
	    factor = 0;
	    no_elements = 1;
	}	
}
//===================================================================================
FitsArray<FitsBit>::~FitsArray() { 
	delete [] dimn; delete [] factor; 
}
//================================================
int FitsArray<FitsBit>::dims() const { 
	return no_dims; 
}
//================================================
int FitsArray<FitsBit>::dim(int n) const { 
	return dimn[n]; 
}
//================================================
int * FitsArray<FitsBit>::vdim() { 
	return dimn;
}
//================================================

} //# NAMESPACE CASACORE - END

