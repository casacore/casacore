//# FITSFieldCopier.h: 
//# Copyright (C) 1996,1998
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

#if !defined(AIPS_FITS_FIELD_COPIER_H)
#define AIPS_FITS_FIELD_COPIER_H

#include <aips/aips.h>
#include <aips/FITS/hdu.h>
#include <aips/Containers/RecordField.h>
#include <aips/Arrays/Array.h>

// <summary>
// </summary>

// <use visibility=local>   or   <use visibility=export>

// <reviewed reviewer="" date="yyyy/mm/dd" tests="" demos="">
// </reviewed>

// <prerequisite>
//   <li> SomeClass
//   <li> SomeOtherClass
//   <li> some concept
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
// <templating arg=T>
//    <li>
//    <li>
// </templating>
//
// <thrown>
//    <li>
//    <li>
// </thrown>
//
// <todo asof="yyyy/mm/dd">
//   <li> add this feature
//   <li> fix this bug
//   <li> start discussion of this possible extension
// </todo>


class FITSFieldCopier
{
public:
    virtual ~FITSFieldCopier() {};
    virtual void copyToFITS() = 0;
};

template<class recordType, class fitsType> class ScalarFITSFieldCopier : 
  public FITSFieldCopier
{
public:
    ScalarFITSFieldCopier(RORecordFieldPtr<recordType> *recptr, 
			  FitsField<fitsType> *fitsptr) 
      : rec_p(recptr), fits_p(fitsptr) {}
    ~ScalarFITSFieldCopier() {delete rec_p; delete fits_p;}
    virtual void copyToFITS() {(*fits_p)() = *(*rec_p); }
private:
    RORecordFieldPtr<recordType> *rec_p;
    FitsField<fitsType> *fits_p;

    ScalarFITSFieldCopier(const ScalarFITSFieldCopier<recordType,fitsType> &other);
    ScalarFITSFieldCopier &operator=(
			     const ScalarFITSFieldCopier<recordType,fitsType> &other);
};

class StringFITSFieldCopier : public FITSFieldCopier
{
 public:
     StringFITSFieldCopier(RORecordFieldPtr<String> *rptr,
 		 FitsField<char> *fptr) : rec_p(rptr), fits_p(fptr) {}
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

template<class recordType, class fitsType> class ArrayFITSFieldCopier :
  public FITSFieldCopier
{
public:
    ArrayFITSFieldCopier(RORecordFieldPtr<Array<recordType> > *recptr, 
		 FitsField<fitsType> *fitsptr) : rec_p(recptr), fits_p(fitsptr) {}
    ~ArrayFITSFieldCopier() {delete rec_p; delete fits_p;}
    virtual void copyToFITS() {
        uInt nfits = fits_p->nelements();
	uInt narray = (**rec_p).nelements();
	uInt nmin = narray < nfits ? narray : nfits;
	Bool deleteIt;
	const recordType *rptr = (**rec_p).getStorage(deleteIt);
	for (uInt i=0; i<nmin; i++) {
	    (*fits_p)(i) = rptr[i];
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

#endif
