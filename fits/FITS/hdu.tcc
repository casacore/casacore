//# hdu.cc:
//# Copyright (C) 1993,1994,1995,1996,1997,1999,2003
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

#ifndef FITS_HDU_TCC
#define FITS_HDU_TCC

//# Partial implementation of little endian code by Kris Huber
//# (kris@helios.ece.usu.edu)

#include <casacore/fits/FITS/fits.h>
#include <casacore/fits/FITS/hdu.h>
#include <casacore/fits/FITS/fitsio.h>
#include <string.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//============================================================================

template <class TYPE>
PrimaryArray<TYPE>::PrimaryArray(FitsInput &f, 
				 FITSErrorHandler errhandler) : 
	HeaderDataUnit(f,FITS::PrimaryArrayHDU,errhandler) {
	pa_assign(); // assign values from keyword list
}
//============================================================================
template <class TYPE>
PrimaryArray<TYPE>::PrimaryArray(FitsInput &f, FITS::HDUType t, 
				 FITSErrorHandler errhandler) : 
	HeaderDataUnit(f,t,errhandler) {
	pa_assign(); // assign values from keyword list
}
//============================================================================
template <class TYPE>
PrimaryArray<TYPE>::PrimaryArray(FitsKeywordList &k, 
				 FITSErrorHandler errhandler) :
	HeaderDataUnit(k,FITS::PrimaryArrayHDU,errhandler,0) {
	pa_assign(); // assign values from keyword list
}
//============================================================================
template <class TYPE>
PrimaryArray<TYPE>::PrimaryArray(FitsKeywordList &k, FITS::HDUType t, 
				 FITSErrorHandler errhandler) :
	HeaderDataUnit(k,t,errhandler,0) {
	pa_assign(); // assign values from keyword list
}
//=============================================================================
// constructor does not require a FitsKeywordList. call write_priArr_hdr() after construction.
template <class TYPE>
PrimaryArray<TYPE>::PrimaryArray(FITSErrorHandler errhandler) :
	HeaderDataUnit(FITS::PrimaryArrayHDU,errhandler,0) {
	// pa_assign();
   // in the case when user is not required to provide a kerword list object,
	// pa_assign() must be called from write_priArr_hdr().
}
//===============================================================================
// protected, for ImageExention and PrimaryGroup to use
template <class TYPE>
PrimaryArray<TYPE>::PrimaryArray( FITS::HDUType t, FITSErrorHandler errhandler) :
	HeaderDataUnit(t,errhandler,0) {
}
//=================================================================================
template <class TYPE>
PrimaryArray<TYPE>::~PrimaryArray() {
	if (bunit_x != &char_null)
	    delete [] bunit_x;
	if (no_dims > 0) {
	    delete [] crpix_x;
	    delete [] crota_x;
	    delete [] crval_x;
	    delete [] cdelt_x;
	    for (int i = 0; i < no_dims; i++)
		if (ctype_x[i] != &char_null)
	    	    delete [] ctype_x[i];
	    delete [] ctype_x;
	    delete [] factor;
	}
	if (alloc_elems > 0) {
	    delete [] array;
	}
}
//===================================================================================
template <class TYPE>
void PrimaryArray<TYPE>::pa_assign() {  // assign values from keyword list
	int i;
	bscale_x = 1.0;		       	// first, initialize everything
	bzero_x = 0.0;
	bunit_x = 0;
	isablank_x = False;
	blank_x = FITS::minInt;
	ctype_x = 0;
	crpix_x = 0;
	crota_x = 0;
	crval_x = 0;
	cdelt_x = 0;
	datamax_x = FITS::maxdouble;
	datamin_x = FITS::mindouble;
	totsize = 0;
	factor = 0;
	alloc_elems = 0;
	beg_elem = 0;
   end_elem = 0;
   array = 0;
	if (err_status != OK) // check for error in HDU construction
            return;
	if (FITS::getfitstype(NoConvert<TYPE>()) != datatype()) {
	    errmsg(BADTYPE,"Wrong type! Current HDU is not of this type!");
	    return;
	}

	bscale_x = asgdbl(FITS::BSCALE,1.0);
	bzero_x = asgdbl(FITS::BZERO,0.0);

	if (kwlist_(FITS::BLANK) == 0)
	    blank_x = Int_null;
	else {
	    blank_x = kwlist_.curr()->asInt();
	    isablank_x = True;
	}

	datamax_x = asgdbl(FITS::DATAMAX,double_null);
	datamin_x = asgdbl(FITS::DATAMIN,double_null);
	bunit_x = assign(FITS::BUNIT);
	if (no_dims > 0) {
	    crpix_x = new double [no_dims];
	    crota_x = new double [no_dims];
	    crval_x = new double [no_dims];
	    cdelt_x = new double [no_dims];
	    ctype_x = new char * [no_dims];
	    if (crpix_x == 0 || crota_x == 0 || crval_x == 0 || 
		cdelt_x == 0 || ctype_x == 0) {
		errmsg(NOMEM,"Cannot allocate memory");
		return;
	    }
	    for (i = 0; i < no_dims; i++) {
		crpix_x[i] = asgdbl(FITS::CRPIX,(i + 1),double_null);
		crota_x[i] = asgdbl(FITS::CROTA,(i + 1),double_null);
		crval_x[i] = asgdbl(FITS::CRVAL,(i + 1),double_null);
		cdelt_x[i] = asgdbl(FITS::CDELT,(i + 1),double_null);
		ctype_x[i] = assign(FITS::CTYPE,(i + 1));
	    }
	    // Allocate and initialize factor and totsize.
	    // These are for Fortran order.
	    totsize = dimn[0];
	    for (i = 1; i < no_dims; i++)
		totsize *= dimn[i];
	    factor = new int [3*no_dims];
	    // We need a little extra space for CtoF and FtoC conversions.
	    if (factor == 0) {
		   errmsg(NOMEM,"Cannot allocate memory");
		   return;
	    }
	    factor[0] = 1;
	    for (i = 1; i < no_dims; ++i)
		 { factor[i] = factor[i - 1] * dimn[i - 1]; }
	} else {
	    crpix_x = 0;
	    crota_x = 0;
	    crval_x = 0;
	    cdelt_x = 0;
	    ctype_x = 0;
	    factor = 0;
	    totsize = 0;
	}
	array = 0; // no space allocated for array
	alloc_elems = 0;
	beg_elem = 0;
	end_elem = -1;
}
//======================================================================================
template <class TYPE>
int PrimaryArray<TYPE>::offset(int d0, int d1) const {
		return d0 + factor[1] * d1;
}
//===================================================================================
template <class TYPE>
int PrimaryArray<TYPE>::offset(int d0, int d1, int d2) const {
	OFF_T offset;
	offset = d0 + d1 * factor[1] + d2 * factor[2];
	return offset;
}
//===================================================================================
template <class TYPE>
int PrimaryArray<TYPE>::offset(int d0, int d1, int d2, int d3) const {
	OFF_T offset;
	offset = d0 + d1 * factor[1] + d2 * factor[2] + d3*factor[3];
	return offset;
}
//===================================================================================
template <class TYPE>
int PrimaryArray<TYPE>::offset(int d0, int d1, int d2, int d3, int d4) const {
	OFF_T offset;
	offset = d0 + d1 * factor[1] + d2 * factor[2] + d3*factor[3] + 
	  d4*factor[4];
	return offset;
}
//===================================================================================
template <class TYPE>
double PrimaryArray<TYPE>::operator () (int d0) const {
	//if (d0 < beg_elem || d0 > end_elem) { // out of bounds
	//}
	return bscale() * array[d0 - beg_elem] + bzero();
}
//===================================================================================
template <class TYPE>
double PrimaryArray<TYPE>::operator () (int d0, int d1) const {
	OFF_T n = offset(d0,d1);
	//if (n < beg_elem || n > end_elem) { // out of bounds
	//}
	return bscale() * array[n - beg_elem] + bzero();
}
//===================================================================================
template <class TYPE>
double PrimaryArray<TYPE>::operator () (int d0, int d1, int d2) const {
	OFF_T n = offset(d0,d1,d2);
	//if (n < beg_elem || n > end_elem) { // out of bounds
	//}
	return bscale() * array[n - beg_elem] + bzero();
}
//====================================================================================
template <class TYPE>
double PrimaryArray<TYPE>::operator () (int d0, int d1, int d2, int d3) const {
	OFF_T n = offset(d0,d1,d2,d3);
	//if (n < beg_elem || n > end_elem) { // out of bounds
	//}
	return bscale() * array[n - beg_elem] + bzero();
}
//====================================================================================
template <class TYPE>
double PrimaryArray<TYPE>::operator () (int d0, int d1, int d2, 
					int d3, int d4) const
{
	OFF_T n = offset(d0,d1,d2,d3,d4);
	//if (n < beg_elem || n > end_elem) { // out of bounds
	//}
	return bscale() * array[n - beg_elem] + bzero();
}
//====================================================================================
template <class TYPE>
TYPE & PrimaryArray<TYPE>::data(int d0) {
	//if (d0 < beg_elem || d0 > end_elem) { // out of bounds
	//}
	return array[d0 - beg_elem];
}
//====================================================================================
template <class TYPE>
TYPE & PrimaryArray<TYPE>::data(int d0, int d1) {
   //cout<< "PrimaryArray::data(int, int ) called."<<endl;
	OFF_T n = offset(d0,d1);
	
	//if (n < beg_elem || n > end_elem) { // out of bounds
	//}
	return array[n - beg_elem];
}
//=====================================================================================
template <class TYPE>
TYPE & PrimaryArray<TYPE>::data(int d0, int d1, int d2) {
	OFF_T n = offset(d0,d1,d2);
	//if (n < beg_elem || n > end_elem) { // out of bounds
	//}
	return array[n - beg_elem];
}
//=====================================================================================
template <class TYPE>
TYPE & PrimaryArray<TYPE>::data(int d0, int d1, int d2, int d3) {
	OFF_T n = offset(d0,d1,d2,d3);
	//if (n < beg_elem || n > end_elem) { // out of bounds
	//}
	return array[n - beg_elem];
}
//=====================================================================================
template <class TYPE>
TYPE & PrimaryArray<TYPE>::data(int d0, int d1, int d2, int d3, int d4) {
	OFF_T n = offset(d0,d1,d2,d3,d4);
	//if (n < beg_elem || n > end_elem) { // out of bounds
	//}
	return array[n - beg_elem];
}
//=================================================================================================
template <class TYPE>
int PrimaryArray<TYPE>::read() { // read entire array
	// check if anything has been read
	if (fin->currsize() != fin->datasize()){ // illegal operation
		errmsg(BADOPER,"Illegal operation -- some data already read");
		return -1;
	}
	if (set_next(nelements()) == -1) { // allocate nelements array elements
	    cerr << "Buffer array is too big to fit into memory. You are using PrimaryArray::read()." << endl;
		 cerr << "Please use PrimaryArray::read( int ) to read data by chunk."<< endl;
		 //errmsg(BADOPER,"Error allocating Array.");
	    return -1;
	}

	// read the data (this read number of FITS bytes)
	OFF_T nr = read_all_data((char *)array);
	if (nr != fitsdatasize()) { // check return value for errors
		errmsg(BADIO,"Error reading Array");
		return -1;
	}

	// do the FITS to local conversion, including worrying about
	// the fact that array and FITS size may not be the same
	// ...
	int ne = nr / fitsitemsize(); // the actual number of elements read
	FITS::f2l( (TYPE *)array, array, ne );

	return alloc_elems;
}
//=============================================================================
template <class TYPE>
int PrimaryArray<TYPE>::read(int ne) { // read the next ne data elements
	if (set_next(ne) == -1) { // check buffer allocation
	    return -1;
	}
	// read ne data elements
	int nr = read_data((char *)array,(ne*fitsitemsize()));
	if (nr < 1) { // check return value for errors
		errmsg(BADIO,"Error reading Array");
		return -1;
	}
	nr /= fitsitemsize(); // the actual number of elements returned
	if (nr != ne)
	    end_elem = beg_elem + nr -1;

	// do the FITS to local conversion, including worrying about
	// the fact that array and FITS size may not be the same
	// ...
	FITS::f2l( (TYPE *)array, array, ne );
	return nr;
}
//==============================================================================
template <class TYPE>
int PrimaryArray<TYPE>::write(FitsOutput &fout) {
	// do the local to FITS conversion, including worrying about
	// the fact that array and FITS size may not be the same
	// ...
	int ne = end_elem - beg_elem + 1;
	FITS::l2f( array, (TYPE *)array, ne ); 
	// write the data from beg_elem to end_elem
	OFF_T nb = fitsitemsize() * ne; 
   if (write_data(fout,(char *)array,nb) != 0) {
		errmsg(BADIO,"Error writing Array");
		return -1;
	}
	return ne;
}
//=========================================================================================
// newly wrapped. Use exception to handle the situation that the array is too big.
template <class TYPE>
OFF_T PrimaryArray<TYPE>::set_next(OFF_T ne) {
	// if ne > current buffersize, reallocate
	if (ne > alloc_elems) {
	   delete [] array;
		try{
         array = new TYPE [ne];
      }catch( std::bad_alloc ){ // out of storage exception.
        cerr << "\narray is too big to fit into memory."<< endl;
		  errmsg(BADOPER,"Error allocating Array.");
		  alloc_elems = 0;
		  beg_elem = 0;
		  end_elem = -1;
		  return -1;
      }
	   alloc_elems = ne;
	}
	// set the limits of the array currently in memory
	beg_elem = end_elem + 1;
	end_elem = beg_elem + ne -1;
	return ne;
}
//=========================================================================================
template <class TYPE>
int PrimaryArray<TYPE>::store(const TYPE *source, int npixels) {

    if (npixels < 0) {
        errmsg(BADSIZE, "npixels < 0");
        return -1;
    }
    if ((OFF_T) npixels > nelements()) {
	errmsg(BADSIZE, "npixels > nelements()");
	return -1;
    }
    if (set_next(npixels) == -1) {
	errmsg(BADSIZE, "set_next fails");
	return -1;
    }
    memcpy(array, source, localitemsize()*npixels);
    return 0;
}
//=========================================================================================
template <class TYPE>
int PrimaryArray<TYPE>::store(const TYPE *source, FITS::FitsArrayOption opt) {
   uInt count;
	Int offset, i, *sub;
	if (set_next(nelements()) == -1) { // allocate nelements array elements
	    return -1;
	}

	if (opt == FITS::CtoF) {
	    sub = &factor[dims()];
	    // algorithm for converting C-order to F-order
	    count = 0;
	    for (i = 0; i < dims(); ++i) 
		sub[i] = 0;
	    for(;;) {
		for (i = 1, offset = sub[0]; i < dims(); ++i)
		    offset += sub[i] * factor[i];
		array[offset] = source[count++];
		if (count == nelements()) 
		    break;
		for (i = dims() - 1; i >= 0; --i) {
			++sub[i];
			if (sub[i] < dim(i)) 
			    break;
			sub[i] = 0;
		}
	    }
	} else {
	    memcpy(array,source,(localitemsize() * nelements()));
	}

	return 0;
}
//============================================================================================
template <class TYPE>
void PrimaryArray<TYPE>::copy(double *target, int npixels) const
{
    if (npixels < 0 || npixels > end_elem - beg_elem + 1) {
	PrimaryArray<TYPE> *This = (PrimaryArray<TYPE> *)this;
	This->errmsg(BADSIZE, "npixels<0 or > number of read pixels");
    }

    double scale = bscale();
    double zero = bzero();
    if (!isablank() || FitsFPUtil::isFP((TYPE *)0)) {
	// No blanks or we are FP
	for (int i=0; i<npixels; i++) {
	    target[i] = scale*array[i] + zero;
	}
    } else {
	double nan;
	TYPE blankval = blank();
	FitsFPUtil::setNaN(nan);
	// Blanked integers
	for (int i=0; i<npixels; i++) {
	    target[i] = array[i] != blankval ? scale*array[i] + zero : nan;
	}
    }
}
//============================================================================================
template <class TYPE>
void PrimaryArray<TYPE>::copy(double *target, FITS::FitsArrayOption opt) const {
        uInt count, n;
	Int offset, i, j, *sub, *C_factor;
	double fscale = (double)bscale();
	double fzero = (double)bzero();
	i=0;
	n=0;

	Bool blanked = isablank() && !FitsFPUtil::isFP((TYPE *)0) ? True:False;
	TYPE blankval = TYPE(0);
	if (blanked) {
	    blankval = blank();
	}
	double nan;
	FitsFPUtil::setNaN(nan);

	if (opt == FITS::FtoC) {
	    sub = &factor[dims()];
	    C_factor = &sub[dims()];
	    // compute the C_factors
	    for (i = 0; i < (dims() - 1); ++i)
		for (j = i + 1, C_factor[i] = 1; j < dims(); ++j)
		    C_factor[i] *= dim(j);
	    C_factor[i] = 1;
	    // algorithm for converting F-order to C-order
	    count = 0;
	    for (i = 0; i < dims(); ++i) 
		sub[i] = 0;
	    for(;;) {
		for (i = 0, offset = 0; i < dims(); ++i)
		    offset += sub[i] * C_factor[i];
		target[offset] = (double)(fscale * array[count++] + fzero);
		if (count == nelements()) 
		    break;
		for (i = 0; i < dims(); ++i) {
		    ++sub[i];
		    if (sub[i] == dim(i))
			sub[i] = 0;
		    else
			break;
		}
	    }
	} else {
	    uInt nmax = nelements();
	    if (!blanked) {
		for (n = 0; n < nmax; ++n)
		    target[n] = (double)(fscale * array[n] + fzero);
	    } else {
		for (n=0; i<Int(nmax); ++n) {
		    target[n] = array[n] != blankval ?
			(double)(fscale * array[n] + fzero) : nan;
		}
	    }
	}
}
//===================================================================================
template <class TYPE>
void PrimaryArray<TYPE>::copy(float *target, int npixels) const
{
    if (npixels < 0 || npixels > end_elem - beg_elem + 1) {
	PrimaryArray<TYPE> *This = (PrimaryArray<TYPE> *)this;
	This->errmsg(BADSIZE, "npixels<0 or > number of read pixels");
    }

    float scale = bscale();
    float zero = bzero();
    if (!isablank() || FitsFPUtil::isFP((TYPE *)0)) {
	// No blanks or we are FP
	for (int i=0; i<npixels; i++) {
	    target[i] = scale*array[i] + zero;
	}
    } else {
	float nan;
	TYPE blankval = blank();
	FitsFPUtil::setNaN(nan);
	// Blanked integers
	for (int i=0; i<npixels; i++) {
	    target[i] = array[i] != blankval ? scale*array[i] + zero : nan;
	}
    }
}
//====================================================================================
template <class TYPE>
void PrimaryArray<TYPE>::copy(float *target, FITS::FitsArrayOption opt) const {
        uInt count, n;
	Int offset, i, j, *sub, *C_factor;
	float fscale = (float)bscale();
	float fzero = (float)bzero();
	i=0;
	n=0;

	Bool blanked = isablank() && !FitsFPUtil::isFP((TYPE *)0) ? True:False;
	TYPE blankval = TYPE(0);
	if (blanked) {
	    blankval = blank();
	}
	float nan;
	FitsFPUtil::setNaN(nan);

	if (opt == FITS::FtoC) {
	    sub = &factor[dims()];
	    C_factor = &sub[dims()];
	    // compute the C_factors
	    for (i = 0; i < (dims() - 1); ++i)
		for (j = i + 1, C_factor[i] = 1; j < dims(); ++j)
		    C_factor[i] *= dim(j);
	    C_factor[i] = 1;
	    // algorithm for converting F-order to C-order
	    count = 0;
	    for (i = 0; i < dims(); ++i) 
		sub[i] = 0;
	    for(;;) {
		for (i = 0, offset = 0; i < dims(); ++i)
		    offset += sub[i] * C_factor[i];
		target[offset] = (float)(fscale * array[count++] + fzero);
		if (count == nelements()) 
		    break;
		for (i = 0; i < dims(); ++i) {
		    ++sub[i];
		    if (sub[i] == dim(i))
			sub[i] = 0;
		    else
			break;
		}
	    }
	} else {
	    uInt nmax = nelements();
	    if (!blanked) {
		for (n = 0; n < nmax; ++n)
		    target[n] = (float)(fscale * array[n] + fzero);
	    } else {
		for (n=0; i<Int(nmax); ++n) {
		    target[n] = array[n] != blankval ?
			(float)(fscale * array[n] + fzero) : nan;
		}
	    }
	}
}
//==============================================================================
template <class TYPE>
void PrimaryArray<TYPE>::move(TYPE *target, int npixels) const
{
    if (npixels < 0 || npixels > end_elem - beg_elem + 1) {
	PrimaryArray<TYPE> *This = (PrimaryArray<TYPE> *)this;
	This->errmsg(BADSIZE, "npixels<0 or > number of read pixels");
    }
    memcpy(target, array, npixels*localitemsize());
}


template <class TYPE>
void PrimaryArray<TYPE>::move(TYPE *target, FITS::FitsArrayOption opt) const {
	uInt count, offset, i, j;
	Int *sub, *C_factor;

	if (opt == FITS::FtoC) {
	    sub = &factor[dims()];
	    C_factor = &sub[dims()];
	    // compute the C_factors
	    for (i = 0; i < uInt(dims() - 1); ++i)
		for (j = i + 1, C_factor[i] = 1; j < uInt(dims()); ++j)
		    C_factor[i] *= dim(j);
	    C_factor[i] = 1;
	    // algorithm for converting F-order to C-order
	    count = 0;
	    for (i = 0; i < uInt(dims()); ++i) 
		sub[i] = 0;
	    for(;;) {
		for (i = 0, offset = 0; i < uInt(dims()); ++i)
		    offset += sub[i] * C_factor[i];
		target[offset] = array[count++];
		if (count == uInt(nelements())) 
		    break;
		for (i = 0; i < uInt(dims()); ++i) {
		    ++sub[i];
		    if (sub[i] == dim(i))
			sub[i] = 0;
		    else
			break;
		}
	    }
	} else {
	    memcpy(target,array,(localitemsize() * nelements()));
	}
}
//===========================================================================================
// write required keywords for primary header.
/* Write the primary header keywords into the CHU. The PCOUNT, GCOUNT and
   EXTEND keywords are not required in the primary header and are only
   written if the parameter of ffphpr(), pcount is not equal to zero, gcount
   is not equal to zero or one, and if extend is TRUE, respectively.
*/  
template <class TYPE>
int PrimaryArray<TYPE>::write_priArr_hdr( FitsOutput &fout, // I - FITS output object
            int simple,     // I - does file conform to FITS standard? 1/0 
            int bitpix,     // I - number of bits per data value pixel     
            int naxis,      // I - number of axes in the data array        
            long naxes[],   // I - length of each data axis                 
            int extend )    // I - may FITS file have extensions?                                     
                      
{
   // primary array header is the beginning of FITS file. So no need to call flush_buffer().
	// flush m_buffer first
	//fout.getfout().flush_buffer();
   int  l_status = 0;     // IO - error status 
	if ( fout.rectype() != FITS::InitialState) {
		errmsg(BADOPER,"[PrimaryArray<TYPE>::write_priArr_hdr()] Primary Header must be the beginning of the file.");
	   return -1;
	}
	// write the required keywords for PrimaryArray to fitsfile.
	long pcount = 0;    // number of group parameters (usually 0) 
   long gcount = 1;    // number of random groups (usually 1 or 0)
	// taking pcount = 0, and gcount<1 will make ffpgpr() write the keywords for primary header only.
	// if pcount!=0 or gcount >1 ffphpr will write pcount and gcount(for PrimaryGroup). If extend 
	// has a value of true( or >0), ffphpr will also write EXTEND keyword
	if(ffphpr(fout.getfptr(), simple, bitpix, naxis, naxes, pcount, gcount, extend, &l_status)){
	   errmsg(BADOPER,"Write Primary header failed![PrimaryArray::write_priArr_hdr]");
	   fits_report_error(stderr, l_status); // print error report
		return -1;
	}
	OFF_T l_headstart, l_datastart, l_dataend;
	l_status = 0;
   // get size info of the current HDU
   if (ffghof(fout.getfptr(), &l_headstart, &l_datastart, &l_dataend, &l_status) > 0){
      fits_report_error(stderr, l_status); // print error report
      return -1;
	}
	// flush the buffer of CFITSIO so that the contents will be actually written to disk.
	if(ffflsh(fout.getfptr(), TRUE, &l_status)){ errmsg(BADOPER,"[PrimaryArray::write_priArr_hdr()] Error flushing buffer!"); }
	
	// move file pointer to the beginning of the new hdu.
	l_status = 0;
	if(ffmbyt(fout.getfptr(), l_headstart, REPORT_EOF, &l_status)){
	 	fits_report_error(stderr, l_status); // print error report
		return -1;
	}
   	
	// using the cfitsio function to read bytes from the file
  	// pointed to by getfptr() from where the file position indicator currently at.
  	l_status = 0;
	char * l_headerbytes = new char[l_datastart - l_headstart + 1];
	if(ffgbyt(fout.getfptr(), l_datastart - l_headstart, l_headerbytes, &l_status)){                   
		fits_report_error(stderr, l_status); // print error report
      return -1;
	}
	/*if(ffmbyt(fout.getfptr(), l_datastart, REPORT_EOF, &l_status)){
	 	fits_report_error(stderr, l_status); // print error report
		return -1;
	}*/
	// ffgbyt() sometimes does not move bytepos to the new position. So we do it.
	(fout.getfptr()->Fptr)->bytepos = l_datastart;
   // now parse the headerbytes into kwlist_. init_data_unit will use kwlist_.
	fout.getkc().parse( l_headerbytes, kwlist_ ,0, errfn,True);
	// init the info for the data unit
	init_data_unit( FITS::PrimaryArrayHDU );
	// assign the binary table. This is done in constructor for the case when user
	// is required to provide a FitsKeywordList object.
	pa_assign();
   // set the info for data unit. init_data_unit() generated the hdu_type, data_type,
	// fits_data_size and fits_item_size
   fout.set_data_info(kwlist_,hdu_type,data_type,fits_data_size,fits_item_size);
   return 0;
}

//====================================================================================

template <class TYPE>
ImageExtension<TYPE>::ImageExtension(FitsInput &f, 
				     FITSErrorHandler errhandler) : 
	PrimaryArray<TYPE>(f,FITS::ImageExtensionHDU,errhandler) {
	ie_assign();
}
//====================================================================================
template <class TYPE>
ImageExtension<TYPE>::ImageExtension(FitsKeywordList &k, 
				     FITSErrorHandler errhandler) :
	PrimaryArray<TYPE>(k,FITS::ImageExtensionHDU,errhandler) {
	ie_assign();
}
//====================================================================================
// constructor that does not require a FitsKeywordList object as parameter
template <class TYPE>
ImageExtension<TYPE>::ImageExtension(FITSErrorHandler errhandler) :
	PrimaryArray<TYPE>(FITS::ImageExtensionHDU,errhandler) {
	// ie_assign();
	// in the case when user is not required to provide a kerword list object,
	// it must be called from write_imgext_hdr().
}
//====================================================================================
template <class TYPE>
ImageExtension<TYPE>::~ImageExtension() {
	if (xtension_x != &char_null)
	    delete [] xtension_x;
	if (extname_x != &char_null)
	    delete [] extname_x;
}
//====================================================================================
template <class TYPE>
void ImageExtension<TYPE>::ie_assign() {
	extver_x = kwlist_(FITS::EXTVER) == 0 ? FITS::minInt : kwlist_.curr()->asInt();
	extlevel_x = kwlist_(FITS::EXTLEVEL) == 0 ? FITS::minInt : kwlist_.curr()->asInt();
	pcount_x = kwlist_(FITS::PCOUNT) == 0 ? FITS::minInt : kwlist_.curr()->asInt();
	gcount_x = kwlist_(FITS::GCOUNT) == 0 ? FITS::minInt : kwlist_.curr()->asInt();
	xtension_x = assign(FITS::XTENSION);
	extname_x = assign(FITS::EXTNAME);
}
//====================================================================================================
// write required keywords for ImageExtension
template <class TYPE>
int ImageExtension<TYPE>::write_imgExt_hdr( FitsOutput &fout, // I - FITS output object
            int bitpix,          /* I - bits per pixel              */
            int naxis,           /* I - number of axes in the array */
            long *naxes)         /* I - size of each axis           */
                            
                      
{
	// flush m_buffer first
	fout.getfout().flush_buffer();

   int  l_status = 0;     // IO - error status 
	if ( fout.rectype() == FITS::InitialState) {
		errmsg(BADOPER,"A Primary Header must be written first[ImageExtension::write_imgext_hdr].");
	   return -1;
	}
	if (!fout.hdu_complete()) {
	   errmsg(BADOPER,"Previous HDU incomplete -- cannot write header[ImageExtension::write_imgext_hdr].");
	   return -1;
	}
	if( !fout.isextend() ) {
	   errmsg(BADOPER,"Extensions are not allowed for this FITS file![ImageExtension::write_imgext_hdr].");
	   return -1;
	}
	if( !fout.required_keys_only() ){
	   cerr << "\n[ImageExtension::write_imgTbl_hdr()] write_imgTbl_hdr() works with other write_***_hdr()" << endl;
		cerr << " methods only. It will not work with write_hdr()." << endl;
		errmsg(BADOPER,"Used wrong header-writting function." );
		return -1;
	}
	// Since the original file pointer does not have the hdu info about the hdu created by
	// write_hdu() method, we reopen the file to get a new file pointer with all the hdu info.
	// This may cause some loss of efficiency. But so far I have not found a better way. 
	fitsfile* l_newfptr = 0;
	l_status = 0;
	//(fout.getfout()).close_file( fout.getfptr(), &l_status);
	//file_close( (fout.getfptr()->Fptr)->filehandle);
	
	const char * l_filename = (fout.getfout()).fname();
	if (ffopen( &l_newfptr, l_filename, READWRITE, &l_status )){
	//if(ffreopen( fout.getfptr(), &l_newfptr, &l_status )){
	   errmsg(BADOPER,"[ImageExtension<TYPE>::write_imgExt_hdr()] ffopen() CHDU failed!");
      fits_report_error(stderr, l_status); // print error report
      return -1;
	}
	//ffmahd(l_newfptr, 1, 0, &l_status);
	// write the required keywords for ImageExtension to fitsfile. 
	l_status = 0; 
	if(ffcrim(l_newfptr, bitpix, naxis, naxes, &l_status)){
	   errmsg(BADOPER,"Write Image header failed![ImageExtension::write_imgext_hdr]");
	   fits_report_error(stderr, l_status); // print error report
		return -1;
	}
	// flush the buffer of CFITSIO so that the contents will be actually written to disk.
	// oopes, if we call ffflsh() in this case, the fits file is damaged! why?
	//if(ffflsh(l_newfptr, TRUE, &l_status)){ errmsg(BADOPER,"[PrimaryArray::write_priArr_hdr()] Error flushing buffer!"); }

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
	char * l_headerbytes = new char[l_datastart - l_headstart + 1];
	if(ffgbyt(l_newfptr, l_datastart - l_headstart, l_headerbytes, &l_status)){                   
		fits_report_error(stderr, l_status); // print error report
      return -1;
	}
	// ffgbyt() sometimes does not move bytepos to the new position. So we do it.
	(l_newfptr->Fptr)->bytepos = l_datastart;   
	// update the file pointer in FitsOutput. Always do this before updating the file pointer in BlockOutpu.
	fout.setfptr( l_newfptr );           
	// update the file pointer in BlockOutput.	
	fout.getfout().setfptr( l_newfptr ); 

   // now parse the headerbytes into kwlist_. init_data_unit will use kwlist_.
	fout.getkc().parse( l_headerbytes, kwlist_ ,0, errfn,True);
	// init the info for the data unit
	init_data_unit( FITS::ImageExtensionHDU );
	// Call the parent pa_assign() method to assign the binary table. This is done in the 
	// parent constructor for the case when user is required to provide a FitsKeywordList object.
	pa_assign();

	// assign the binary table. This is done in constructor for the case when user
	// is required to provide a FitsKeywordList object.
	ie_assign();

	// set the info for data unit. init_data_unit() generated the hdu_type, data_type,
	// fits_data_size and fits_item_size
   fout.set_data_info(kwlist_,hdu_type,data_type,fits_data_size,fits_item_size);
   
   return 0;
}
//===================================================================================================
template <class TYPE>
PrimaryGroup<TYPE>::PrimaryGroup(FitsInput &f, FITSErrorHandler errhandler) : 
	PrimaryArray<TYPE>(f,FITS::PrimaryGroupHDU,errhandler) {
	pg_assign();
}
//===================================================================================================
template <class TYPE>
PrimaryGroup<TYPE>::PrimaryGroup(FitsKeywordList &k, 
				 FITSErrorHandler errhandler) :
	PrimaryArray<TYPE>(k,FITS::PrimaryGroupHDU,errhandler) {
	pg_assign();
}
//===================================================================================================
// constructor for the situation when only reqiured keywords for random groups 
// are to be written.
template <class TYPE>
PrimaryGroup<TYPE>::PrimaryGroup(FITSErrorHandler errhandler) :
	PrimaryArray<TYPE>(FITS::PrimaryGroupHDU,errhandler) {
	// pg_assign();
	// in the case when user is not required to provide a kerword list object,
	// pg_assign() must be called from write_priGrp_hdr(). 
}
//===================================================================================================
template <class TYPE>
PrimaryGroup<TYPE>::~PrimaryGroup() {
	if (pcount_x > 0) {
	    for (int i = 0; i < pcount_x; i++)
		if (ptype_x[i] != &char_null)
	    	    delete [] ptype_x[i];
	    delete [] ptype_x;
	    delete [] pzero_x;
	    delete [] pscal_x;
	}
	delete [] group_parm;
	array = 0; // reset array to 0 so PrimaryArray won't delete anything
}
//===================================================================================================
// write the required keyword for promaryGroup( random groups)
/* taking pcount = 0, and gcount<1 will make ffpgpr() write the keywords for primary header only.
   if pcount!=0 or gcount >1 ffphpr() will write pcount and gcount(for PrimaryGroup). If extend 
   has a value of true( or >0), ffphpr() will also write EXTEND keyword
*/
template <class TYPE>
int PrimaryGroup<TYPE>::write_priGrp_hdr( FitsOutput &fout, // I - FITS output object
            int simple,     // I - does file conform to FITS standard? 1/0
            int bitpix,     // I - number of bits per data value pixel     
            int naxis,      // I - number of axes in the data array        
            long naxes[],   // I - length of each data axis(make naxes[0]=0 for Primary Group)                
            long pcount,    // I - number of group parameters ( none 0 for Primary Group)  
            long gcount )   // I - number of random groups ( greater than 1 for PrimaryGroup)                                  
{
	if( naxes[0] != 0 ){
	   errmsg(BADOPER,"[PrimaryGroup<TYPE>::write_priGrp_hdr())] For Primary Group, keyword NAXIS1 must be 0!");
		return -1;
	} 
	if( pcount < 1 && gcount <2 ){
	   errmsg(BADOPER,"[PrimaryGroup<TYPE>::write_priGrp_hdr())] In order to create keyword GROUPS, PCOUNT and GCOUNT, either PCOUNT's value must be >0 or GCOUNT's value must be > 1 !");
		return -1;	
	}
	
	// flush m_buffer first
	fout.getfout().flush_buffer();

	int  l_status = 0;     // IO - error status 
	if ( fout.rectype() != FITS::InitialState) {
		errmsg(BADOPER,"Primary Header must be the begnning of the file[PrimaryGroup::write_priGrp_hdr].");
	   return -1;
	}
	// write the required keywords for PrimaryGroup to fitsfile.
	int extend = 0; // EXTEND is not required by random group. Set it to zero so that it will not be written.
	if(ffphpr(fout.getfptr(), simple, bitpix, naxis, naxes, pcount, gcount, extend, &l_status)){
	   errmsg(BADOPER,"Write PrimaryGroup header failed![PrimaryGroup::write_priGrp_hdr]");
	   fits_report_error(stderr, l_status); // print error report
		return -1;
	}
	OFF_T l_headstart, l_datastart, l_dataend;
	l_status = 0;
   // get size info of the current HDU
   if (ffghof(fout.getfptr(), &l_headstart, &l_datastart, &l_dataend, &l_status) > 0){
      fits_report_error(stderr, l_status); // print error report
      return -1;
	}
	// move file pointer to the beginning of the new hdu.
	l_status = 0;
	if(ffmbyt(fout.getfptr(), l_headstart, REPORT_EOF, &l_status)){
	 	fits_report_error(stderr, l_status); // print error report
		return -1;
	}
	// using the cfitsio function to read bytes from the file
  	// pointed to by getfptr() from where the file position indicator currently at.
  	l_status = 0;
	char * l_headerbytes = new char[l_datastart - l_headstart + 1];
	if(ffgbyt(fout.getfptr(), l_datastart - l_headstart, l_headerbytes, &l_status)){                   
		fits_report_error(stderr, l_status); // print error report
      return -1;
	}
	// ffgbyt() sometimes does not move bytepos to the new position. So we do it.
	(fout.getfptr()->Fptr)->bytepos = l_datastart;

   // now parse the headerbytes into kwlist_. init_data_unit will use kwlist_.
	fout.getkc().parse( l_headerbytes, kwlist_ ,0, errfn,True);
	// init the info for the data unit
	init_data_unit( FITS::PrimaryGroupHDU );
	
	// Call the parent pa_assign() method to assign the binary table. This is done in the 
	// parent constructor for the case when user is required to provide a FitsKeywordList object.
	pa_assign();

	// assign the binary table. This is done in constructor for the case when user
	// is required to provide a FitsKeywordList object.
	pg_assign();

	// set the info for data unit. init_data_unit() generated the hdu_type, data_type,
	// fits_data_size and fits_item_size
   fout.set_data_info(kwlist_,hdu_type,data_type,fits_data_size,fits_item_size);

   return 0;
}
//=====================================================================================
template <class TYPE>
void PrimaryGroup<TYPE>::pg_assign() {
	int i;
	ptype_x = 0;
	pscal_x = 0;
	pzero_x = 0;
	pcount_x = 0;
	gcount_x = 0;
	group_parm = 0;
        current_group = 0;
	if (err_status != OK) // check for previous errors
	    return;
	// get gcount and pcount keywords
	pcount_x = kwlist_(FITS::PCOUNT)->asInt();
	gcount_x = kwlist_(FITS::GCOUNT)->asInt();
	// and assign keyword values
	if (pcount_x > 0) {
	    pscal_x = new double [pcount_x];
	    pzero_x = new double [pcount_x];
	    ptype_x = new char * [pcount_x];
	    if (pscal_x == 0 || pzero_x == 0 || ptype_x == 0) {
		errmsg(NOMEM,"Cannot allocate memory");
		return;
	    }
	    for (i = 0; i < pcount_x; i++) {
		pscal_x[i] = asgdbl(FITS::PSCAL,(i + 1),1.0);
		pzero_x[i] = asgdbl(FITS::PZERO_FITS,(i + 1),0.0);
		ptype_x[i] = assign(FITS::PTYPE,(i + 1));
	    }
	}

	// must recompute the array dimensions and factors
	// and set the group sizes
	totsize = dimn[1];
	for (i = 2; i < no_dims; ++i)
	    totsize *= dimn[i];
	factor[0] = 1;
	for (i = 1; i < (no_dims - 1); ++i)
	    factor[i] = factor[i - 1] * dimn[i];
	for (i = 0; i < (no_dims - 1); ++i)
	    dimn[i] = dimn[i+ 1];
	--no_dims;

	// adjust all axis-relative keyword values
	if (ctype_x[0] != &char_null)
	    delete [] ctype_x[0];
	for (i = 0; i < no_dims; ++i) {
	    crpix_x[i] = crpix_x[i + 1];
	    crota_x[i] = crota_x[i + 1];
	    crval_x[i] = crval_x[i + 1];
	    cdelt_x[i] = cdelt_x[i + 1];
	    ctype_x[i] = ctype_x[i + 1];
	}

	// allocate buffer space for an entire group
	group_parm = new TYPE [pcount() + nelements()];
	if (group_parm == 0) {
	    errmsg(NOMEM,"Cannot allocate memory");
	    return;
	}
	array = &group_parm[pcount()]; // set location of array
}
//====================================================================================
template <class TYPE>
double PrimaryGroup<TYPE>::parm(int n) {
	return pscal(n) * group_parm[n] + pzero(n);
}
//====================================================================================
template <class TYPE>
TYPE & PrimaryGroup<TYPE>::rawparm(int n) {
	return group_parm[n];
}
//====================================================================================
template <class TYPE>
void PrimaryGroup<TYPE>::storeparm(const TYPE *source) {
	memcpy(group_parm,source,(localitemsize() * pcount()));
}
//====================================================================================
template <class TYPE>
void PrimaryGroup<TYPE>::copyparm(double *target) const {
	for (Int i = 0; i < pcount(); ++i)
	    target[i] = pscal(i) * group_parm[i] + pzero(i);
}
//====================================================================================
template <class TYPE>
void PrimaryGroup<TYPE>::copyparm(float *target) const {
	for (Int i = 0; i < pcount(); ++i)
	    target[i] = (float)(pscal(i) * group_parm[i] + pzero(i));
}
//====================================================================================
template <class TYPE>
void PrimaryGroup<TYPE>::moveparm(TYPE *target) const {
	memcpy(target,group_parm,(localitemsize() * pcount()));
}
//====================================================================================
template <class TYPE>
int PrimaryGroup<TYPE>::read() {
	// read the data
	Int nb = fitsitemsize() * (pcount() + nelements());
	if (read_data((char *)group_parm,nb) != nb) {
	    //error message
	    return -1;
	}

	// do the FITS to local conversion, including worrying about
	// the fact that array and FITS size may not be the same
	// ...
	uInt ne = nb / fitsitemsize(); // the actual number of elements read
	FITS::f2l( (TYPE *)group_parm, group_parm, ne );

	++current_group;
	return 0;
}
//===================================================================================
template <class TYPE>
int PrimaryGroup<TYPE>::write(FitsOutput &fout) {
	// do the FITS to local conversion, including worrying about
	// the fact that array and FITS size may not be the same
	// ...
	int ne = pcount() + nelements();
	FITS::l2f( group_parm, (TYPE *)group_parm, ne );
	// write the current group
	OFF_T nb = fitsitemsize() * (pcount() + nelements());

	if (write_data(fout,(char *)group_parm,nb) != 0) {
	    errmsg(BADIO,"Error writing group");
	    return -1;
	}
	++current_group;
	return 0;
}
//====================================================================================

template <class TYPE>
PrimaryTable<TYPE>::PrimaryTable(FitsInput &f, 
				     FITSErrorHandler errhandler) : 
	PrimaryArray<TYPE>(f,FITS::PrimaryTableHDU,errhandler) {
	pt_assign();
}
//====================================================================================
template <class TYPE>
PrimaryTable<TYPE>::PrimaryTable(FitsKeywordList &k, 
				     FITSErrorHandler errhandler) :
	PrimaryArray<TYPE>(k,FITS::PrimaryTableHDU,errhandler) {
	pt_assign();
}
//====================================================================================
// constructor that does not require a FitsKeywordList object as parameter
template <class TYPE>
PrimaryTable<TYPE>::PrimaryTable(FITSErrorHandler errhandler) :
	PrimaryArray<TYPE>(FITS::PrimaryTableHDU,errhandler) {
	// pt_assign();
	// in the case when user is not required to provide a kerword list object,
	// it must be called from write_priTable_hdr().
}
//====================================================================================
template <class TYPE>
PrimaryTable<TYPE>::~PrimaryTable() {
	if (object_x != &char_null)
	    delete [] object_x;
	if (telescop_x != &char_null)
	    delete telescop_x;
}
//====================================================================================
template <class TYPE>
void PrimaryTable<TYPE>::pt_assign() {
        object_x = assign(FITS::OBJECT);
        telescop_x = assign(FITS::TELESCOP);
        instrume_x = assign(FITS::INSTRUME);
        dateobs_x = assign(FITS::DATE_OBS);
        datemap_x = assign(FITS::DATE_MAP);
        //bscale_x = kwlist_(FITS::BSCALE)->asFloat();
        //bzero_x = kwlist_(FITS::BZERO)->asFloat();
        //bunit_x = assign(FITS::BUNIT);
        //equinox_x = kwlist_(FITS::EQUINOX)->asFloat();
        //altrpix_x = kwlist_(FITS::ALTRPIX)->asFloat();
}
template <class TYPE>
int PrimaryTable<TYPE>::read() {
	//return PrimaryArray<TYPE>::read();
        return 0;
}
//====================================================================================================
// write required keywords for PrimaryTable
template <class TYPE>
int PrimaryTable<TYPE>::write_priTable_hdr( FitsOutput &fout, // I - FITS output object
            int bitpix,          /* I - bits per pixel              */
            int naxis,           /* I - number of axes in the array */
            long *naxes)         /* I - size of each axis           */
                            
                      
{
	// flush m_buffer first
	fout.getfout().flush_buffer();

   int  l_status = 0;     // IO - error status 
	if ( fout.rectype() == FITS::InitialState) {
		errmsg(BADOPER,"A Primary Header must be written first[PrimaryTable::write_priTable_hdr].");
	   return -1;
	}
	if (!fout.hdu_complete()) {
	   errmsg(BADOPER,"Previous HDU incomplete -- cannot write header[PrimaryTable::write_priTable_hdr].");
	   return -1;
	}
	if( !fout.isextend() ) {
	   errmsg(BADOPER,"Extensions are not allowed for this FITS file![PrimaryTable::write_priTable_hdr].");
	   return -1;
	}
	if( !fout.required_keys_only() ){
	   cerr << "\n[PrimaryTable::write_priTable_hdr()] write_priTable_hdr() " 
                << "works with other write_***_hdr()" << endl;
	   cerr << " methods only. It will not work with write_hdr()." << endl;
		errmsg(BADOPER,"Used wrong header-writting function." );
		return -1;
	}
	// Since the original file pointer does not have the hdu info about the hdu created by
	// write_hdu() method, we reopen the file to get a new file pointer with all the hdu info.
	// This may cause some loss of efficiency. But so far I have not found a better way. 
	fitsfile* l_newfptr = 0;
	l_status = 0;
	//(fout.getfout()).close_file( fout.getfptr(), &l_status);
	//file_close( (fout.getfptr()->Fptr)->filehandle);
	
	const char * l_filename = (fout.getfout()).fname();
	if (ffopen( &l_newfptr, l_filename, READWRITE, &l_status )){
	//if(ffreopen( fout.getfptr(), &l_newfptr, &l_status )){
	   errmsg(BADOPER,"[PrimaryTable<TYPE>::write_priTable_hdr()] ffopen() CHDU failed!");
           fits_report_error(stderr, l_status); // print error report
           return -1;
	}
	//ffmahd(l_newfptr, 1, 0, &l_status);
	// write the required keywords for PrimaryTable to fitsfile. 
	l_status = 0; 
	if(ffcrim(l_newfptr, bitpix, naxis, naxes, &l_status)){
	   errmsg(BADOPER,"Write Image header failed![PrimaryTable::write_priTable_hdr]");
	   fits_report_error(stderr, l_status); // print error report
		return -1;
	}
	// flush the buffer of CFITSIO so that the contents will be actually written to disk.
	// oopes, if we call ffflsh() in this case, the fits file is damaged! why?
	//if(ffflsh(l_newfptr, TRUE, &l_status)){ errmsg(BADOPER,"[PrimaryArray::write_priArr_hdr()] Error flushing buffer!"); }

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
	char * l_headerbytes = new char[l_datastart - l_headstart + 1];
	if(ffgbyt(l_newfptr, l_datastart - l_headstart, l_headerbytes, &l_status)){                   
		fits_report_error(stderr, l_status); // print error report
           return -1;
	}
	// ffgbyt() sometimes does not move bytepos to the new position. So we do it.
	(l_newfptr->Fptr)->bytepos = l_datastart;   
	// update the file pointer in FitsOutput. Always do this before updating the file pointer in BlockOutpu.
	fout.setfptr( l_newfptr );           
	// update the file pointer in BlockOutput.	
	fout.getfout().setfptr( l_newfptr ); 

        // now parse the headerbytes into kwlist_. init_data_unit will use kwlist_.
	fout.getkc().parse( l_headerbytes, kwlist_ ,0, errfn,True);
	// init the info for the data unit
	init_data_unit( FITS::PrimaryTableHDU );
	// Call the parent pa_assign() method to assign the binary table. This is done in the 
	// parent constructor for the case when user is required to provide a FitsKeywordList object.
	pa_assign();

	// assign the binary table. This is done in constructor for the case when user
	// is required to provide a FitsKeywordList object.
	pt_assign();

	// set the info for data unit. init_data_unit() generated the hdu_type, data_type,
	// fits_data_size and fits_item_size
   fout.set_data_info(kwlist_,hdu_type,data_type,fits_data_size,fits_item_size);
   
   return 0;
}
//==================================================================================
template <class TYPE>
FitsField<TYPE>::~FitsField() { 
}
//==================================================================================
template <class TYPE>
int FitsField<TYPE>::fitsfieldsize() const { 
	return FITS::fitssize(data_type) * no_elements; 
}
//==================================================================================
template <class TYPE>
int FitsField<TYPE>::localfieldsize() const { 
	return FITS::localsize(data_type) * no_elements; 
}
//===================================================================================
template <class TYPE>
void * FitsField<TYPE>::data() { 
	return *field;
}
//===================================================================================
template <class TYPE>
void FitsField<TYPE>::setaddr(void **addr) {
	field = (TYPE **)addr;
}
//===================================================================================
template <class TYPE>
void FitsField<TYPE>::show(std::ostream &o) {
	int i;
	unsigned char *s;
	char *p;
	if (no_elements == 0)
	    return;
	if (fieldtype() == FITS::BYTE) {
	    s = (unsigned char *)(*field);
	    o << (int)s[0];
	    for (i = 1; i < no_elements; ++i)
		o << ", " << (int)s[i];
	} else if (fieldtype() == FITS::CHAR) {
	    p = (char *)(*field);
	    for (i = 0; i < no_elements && *p != '\0'; ++i, ++p)
		o << *p;
	} else {
	    o << (*field)[0];
	    for (i = 1; i < no_elements; ++i)
		o << ", " << (*field)[i];
	}
}
//============================================================================
template <class TYPE>
FitsArray<TYPE>::FitsArray(int n, const int *d) : 
	FitsField<TYPE>(1) {
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
//============================================================================
template <class TYPE>
FitsArray<TYPE>::~FitsArray() { 
	delete [] dimn; delete [] factor; 
}
//============================================================================
template <class TYPE>
int FitsArray<TYPE>::dims() const { 
	return no_dims; 
}
//============================================================================
template <class TYPE>
int FitsArray<TYPE>::dim(int n) const { 
	return dimn[n]; 
}
//============================================================================
template <class TYPE>
int * FitsArray<TYPE>::vdim() { 
	return dimn;
}
//============================================================================
template <class TYPE>
inline TYPE & FitsArray<TYPE>::operator () (int d0, int d1) 
{
        return (*field)[d0 + (factor[1] * d1)]; 
}
//============================================================================
template <class TYPE>
inline TYPE & FitsArray<TYPE>::operator () (int d0, int d1, int d2) 
{
        return (*field)[d0 + (factor[1] * d1) + (factor[2]*d2)]; 
}
//============================================================================
template <class TYPE>
inline TYPE & FitsArray<TYPE>::operator () (int d0, int d1, int d2, int d3) 
{
        return (*field)[d0 + (factor[1] * d1) + (factor[2]*d2) +
		       (factor[3]*d3)]; 
}
//=============================================================================
template <class TYPE>
inline TYPE & FitsArray<TYPE>::operator () (int d0, int d1, int d2, int d3,
					    int d4) 
{
        return (*field)[d0 + (factor[1] * d1) + (factor[2]*d2) + 
		       (factor[3]*d3) + (factor[4]*d4)]; 
}
//=============================================================================

} //# NAMESPACE CASACORE - END


#endif
