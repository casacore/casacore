//# FITSFieldCopier.h: Copy RORecordFields to FitsFields
//# Copyright (C) 1996,1998,1999,2000,2002
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
//#
//# $Id$

#ifndef FITS_FITSFIELDCOPIER_H
#define FITS_FITSFIELDCOPIER_H

#include <casacore/casa/aips.h>
#include <casacore/fits/FITS/hdu.h>
#include <casacore/casa/Containers/RecordField.h>
#include <casacore/casa/Arrays/Array.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/fits/FITS/FITSKeywordUtil.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// <summary>
// Virtual base class for copying RORecordFields to FitsFields
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> RORecordField
//   <li> FitsFields
// </prerequisite>
//
// <etymology>
// </etymology>
//
// <synopsis>
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// </motivation>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//
// <todo asof="yyyy/mm/dd">
//   <li> actually document this
// </todo>


class FITSFieldCopier
{
public:
  // destructor
    virtual ~FITSFieldCopier() {};

  // the things which does the work - to be implemented in each derived class
    virtual void copyToFITS() = 0;
};

// <summary>
// A FITSFieldCopier for copying scalar non-string RecordFields to FitsFields
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> RORecordField
//   <li> FitsFields
// </prerequisite>
//
// <etymology>
// </etymology>
//
// <synopsis>
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// </motivation>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//
// <todo asof="yyyy/mm/dd">
//   <li> actually document this
// </todo>


template<class recordType, class fitsType> class ScalarFITSFieldCopier : 
  public FITSFieldCopier
{
public:
    ScalarFITSFieldCopier(RORecordFieldPtr<recordType> *recptr, 
			  FitsField<fitsType> *fitsptr) 
      : rec_p(recptr), fits_p(fitsptr) {}
    ~ScalarFITSFieldCopier() {delete rec_p; delete fits_p;}

  // Copy the current contents of the input RORecordFieldPtr to the 
  // output FitsField
    virtual void copyToFITS() {(*fits_p)() = *(*rec_p); }
private:
    RORecordFieldPtr<recordType> *rec_p;
    FitsField<fitsType> *fits_p;

    ScalarFITSFieldCopier(const ScalarFITSFieldCopier<recordType,fitsType> &other);
    ScalarFITSFieldCopier &operator=(
			     const ScalarFITSFieldCopier<recordType,fitsType> &other);
};

// <summary>
// A FITSFieldCopier for copying String RecordFields to FitsFields
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> RORecordField
//   <li> FitsFields
// </prerequisite>
//
// <etymology>
// </etymology>
//
// <synopsis>
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// </motivation>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//
// <todo asof="yyyy/mm/dd">
//   <li> actually document this
// </todo>


class StringFITSFieldCopier : public FITSFieldCopier
{
 public:
     StringFITSFieldCopier(RORecordFieldPtr<String> *rptr,
 		 FitsField<char> *fptr) : rec_p(rptr), fits_p(fptr) {}
  // Copy the current contents of the input RORecordFieldPtr to the 
  // output FitsField
     virtual void copyToFITS()
       {
 	  Int fitslength = fits_p->nelements();
 	  Int reclength = (*(*rec_p)).length();
 	  Int minlength = fitslength < reclength ? fitslength : reclength;
 	  const char *chars = (**rec_p).chars();
	  Int i;
 	  for (i=0; i<minlength; i++) {
 	      (*fits_p)(i) = chars[i];
 	  }
 	  if (i < fitslength) {
	      (*fits_p)(i) = '\0'; // null terminate if possible
 	  }
       }
     ~StringFITSFieldCopier() {delete rec_p; delete fits_p;}
private:
     RORecordFieldPtr<String> *rec_p;
     FitsField<char> *fits_p;

    // Undefined and inaccessible.
     StringFITSFieldCopier(const StringFITSFieldCopier &other);
     StringFITSFieldCopier &operator=(const StringFITSFieldCopier &other);
};

// <summary>
// A FITSFieldCopier for copying Array RecordFields to FitsFields
// </summary>

// <use visibility=local>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> RORecordField
//   <li> FitsFields
// </prerequisite>
//
// <etymology>
// </etymology>
//
// <synopsis>
// </synopsis>
//
// <example>
// </example>
//
// <motivation>
// </motivation>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//
// <todo asof="yyyy/mm/dd">
//   <li> actually document this
// </todo>


template<class recordType, class fitsType> class ArrayFITSFieldCopier :
  public FITSFieldCopier
{
public:
    ArrayFITSFieldCopier(RORecordFieldPtr<Array<recordType> > *recptr, 
		 FitsField<fitsType> *fitsptr) : rec_p(recptr), fits_p(fitsptr) {}
    ~ArrayFITSFieldCopier() {delete rec_p; delete fits_p;}
  // Copy the current contents of the input RORecordFieldPtr to the 
  // output FitsField
    virtual void copyToFITS() {
        uInt nfits = fits_p->nelements();
	uInt narray = (**rec_p).nelements();
	uInt nmin = narray < nfits ? narray : nfits;
	Bool deleteIt;
	const recordType *rptr = (**rec_p).getStorage(deleteIt);
	for (uInt i=0; i<nmin; i++) {
	    (*fits_p)(i) = rptr[i];
	}
	// pad with nulls
	for (uInt i=nmin;i<nfits;i++) {
	    (*fits_p)(i) = recordType(0);
	}
	(**rec_p).freeStorage(rptr, deleteIt);
    }
private:
    RORecordFieldPtr<Array<recordType> > *rec_p;
    FitsField<fitsType> *fits_p;

    // Undefined and inaccessible
    ArrayFITSFieldCopier(const ArrayFITSFieldCopier<recordType,fitsType> &other);
    ArrayFITSFieldCopier &operator=(
			    const ArrayFITSFieldCopier<recordType,fitsType> &other);
};

template<class recordType, class fitsType> class VariableArrayFITSFieldCopier :
  public FITSFieldCopier
{
public:
    VariableArrayFITSFieldCopier(RORecordFieldPtr<Array<recordType> > *recptr, 
			 FitsField<fitsType> *fitsptr,
			 FitsField<char> *tdirptr) 
      : rec_p(recptr), fits_p(fitsptr), tdir_p(tdirptr) {}
    ~VariableArrayFITSFieldCopier() {delete rec_p; delete fits_p;}
  // Copy the current contents of the input RORecordFieldPtr to the 
  // output FitsField
    virtual void copyToFITS() {
        uInt nfits = fits_p->nelements();
	uInt narray = (**rec_p).nelements();
	uInt nmin = narray < nfits ? narray : nfits;
	Bool deleteIt;
	const recordType *rptr = (**rec_p).getStorage(deleteIt);
	for (uInt i=0; i<nmin; i++) {
	    (*fits_p)(i) = rptr[i];
	}
	for (uInt i=nmin;i<nfits;i++) {
	    (*fits_p)(i) = recordType(0);
	}
	(**rec_p).freeStorage(rptr, deleteIt);
	// and construct the TDIM value for this array
	String thisTDIR;
	FITSKeywordUtil::toTDIM(thisTDIR, (**rec_p).shape());
	// and store it in the tdir_p FitsField
	Int fitslength = tdir_p->nelements();
	Int reclength = thisTDIR.length();
	Int minlength = fitslength < reclength ? fitslength : reclength;
	const char *chars = thisTDIR.chars();
	Int i;
	for (i=0; i<minlength; i++) {
	    (*tdir_p)(i) = chars[i];
	}
	for (Int i=minlength; i<fitslength; i++) {
	    (*tdir_p)(i) = '\0'; // null terminate if possible
	}
    }
private:
    RORecordFieldPtr<Array<recordType> > *rec_p;
    FitsField<fitsType> *fits_p;
    FitsField<char> *tdir_p;

    // Undefined and inaccessible
    VariableArrayFITSFieldCopier(const VariableArrayFITSFieldCopier<recordType,fitsType> &other);
    VariableArrayFITSFieldCopier &operator=(
			    const VariableArrayFITSFieldCopier<recordType,fitsType> &other);
};


} //# NAMESPACE CASACORE - END

#endif
