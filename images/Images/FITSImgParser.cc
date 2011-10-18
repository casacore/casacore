//# FITSImgParser.h: Class for parsing multi-extension FITS images
//# Copyright (C) 2001,2002
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

#include <images/Images/FITSImgParser.h>

#include <fits/FITS/hdu.h>
#include <fits/FITS/fitsio.h>
#include <fits/FITS/FITSKeywordUtil.h>
#include <casa/OS/File.h>
#include <casa/Utilities/ValType.h>
#include <casa/BasicSL/String.h>
#include <casa/Exceptions/Error.h>

#include <casa/iostream.h>

namespace casa { //# NAMESPACE CASA - BEGIN

FITSImgParser::FITSImgParser (const String& name)
: name_p      (name),
  numhdu_p    (0)
{
   setup();
}
 
FITSImgParser::~FITSImgParser()
{
	delete []extensions_p;
}

String FITSImgParser::fitsname (Bool stripPath) const
{
   Path path(name_p);
   if (stripPath) {
      return path.baseName();
   } else {
      return path.absoluteName();
   }
}

Int FITSImgParser::get_index(const FITSExtInfo &extinfo)
{
	// go over all extensions
	for (Int index=0; index < (Int)numhdu_p; index++){
		// return the current index if is was the
		// one you are looking for
		if (extensions_p[index] == extinfo)
			return index;
	}

	// return the sign for 'not found'
	return -1;
}

uInt FITSImgParser::get_firstdata_index(void)
{
	for (uInt index=0; index < numhdu_p; index++){
		if (extensions_p[index].has_data())
			return index;
	}
	return numhdu_p;
}

String FITSImgParser::get_ext_list(const String &delimiter)
{
	String bigString="";

	for (uInt index=0; index < numhdu_p; index++){
		if (extensions_p[index].has_data())
			bigString += extensions_p[index].get_extexpr() + delimiter;
	}
	return bigString;
}

void FITSImgParser::setup(void)
{
	// make sure a proper name is defined
	if (name_p.empty()) {
		throw AipsError("FITSImage: given file name is empty");
	}

	// get the pathname
	Path path(name_p);
	String fullName = path.absoluteName();

	// open the fits image
	FitsInput fin(path.expandedName().chars(),FITS::Disk);
	if (fin.err() == FitsIO::IOERR)
		throw (AipsError(name_p + " Error opening FITS input."));
	else if (fin.err())
		throw (AipsError(name_p + " Error reading initial record -- exiting."));

	// get the number of HDU's
	int num_hdu = fin.getnumhdu();

	// allocate the extensions
	extensions_p = new FITSExtInfo[num_hdu];

	HeaderDataUnit *hdu;
	PrimaryArray<unsigned char> *paB;
	PrimaryArray<short> *paS;
	PrimaryArray<FitsLong> *paL;
	PrimaryArray<float> *paF;
	PrimaryArray<double> *paD;

	uInt extindex = 0;
	Bool isfitsimg=True;
	while (fin.rectype() != FITS::EndOfFile && isfitsimg && !fin.err() && extindex < (uInt)num_hdu){
		extindex++;
		if (fin.rectype() == FITS::HDURecord) {
			switch (fin.hdutype()) {
			case FITS::PrimaryArrayHDU:
				switch (fin.datatype()) {
				case FITS::BYTE:
					paB = new PrimaryArray<unsigned char>(fin);
					process_extension(paB, extindex);
					delete paB;
					break;
				case FITS::SHORT:
					paS = new PrimaryArray<short>(fin);
					process_extension(paS, extindex);
					delete paS;
					break;
				case FITS::LONG:
					paL = new PrimaryArray<FitsLong>(fin);
					process_extension(paL, extindex);
					delete paL;
					break;
				case FITS::FLOAT:
					paF = new PrimaryArray<float>(fin);
					process_extension(paF, extindex);
					delete paF;
					break;
				case FITS::DOUBLE:
					paD = new PrimaryArray<double>(fin);
					process_extension(paD, extindex);
					delete paD;
					break;
				default:
					break;
				}
				break;
			case FITS::ImageExtensionHDU:
				switch (fin.datatype()) {
				case FITS::BYTE:
					paB = new ImageExtension<unsigned char>(fin);
					process_extension(paB, extindex);
					delete paB;
					break;
				case FITS::SHORT:
					paS = new ImageExtension<short>(fin);
					process_extension(paS, extindex);
					delete paS;
					break;
				case FITS::LONG:
					paL = new ImageExtension<FitsLong>(fin);
					process_extension(paL, extindex);
					delete paL;
					break;
				case FITS::FLOAT:
					paF = new ImageExtension<float>(fin);
					process_extension(paF, extindex);
					delete paF;
					break;
				case FITS::DOUBLE:
					paD = new ImageExtension<double>(fin);
					process_extension(paD, extindex);
					delete paD;
					break;
				default:
					break;
				}
				break;
			case FITS::PrimaryGroupHDU:
				isfitsimg = False;
				break;
			case FITS::AsciiTableHDU:
				isfitsimg = False;
				break;
			case FITS::BinaryTableHDU:
				isfitsimg = False;
				break;
			case FITS::UnknownExtensionHDU:
				hdu = new ExtensionHeaderDataUnit(fin);
				hdu->skip();
				delete hdu;
				break;
			default:
				cout << "This isn't supposed to happen\n";
				break;
			}
		}else if (fin.rectype() == FITS::BadBeginningRecord ||
				fin.rectype() == FITS::UnrecognizableRecord) {
			throw (AipsError("Bad Record encountered"));
		}else if (fin.rectype() == FITS::SpecialRecord) {
			throw (AipsError("Special Record encountered"));
		}
	}
}


void FITSImgParser::process_extension(HeaderDataUnit *h,const uInt &extindex)
{
	String extname="";
	Int    extversion=-1;
	Bool   hasdata=False;
	uInt   actindex=extindex-1;

	const FitsKeyword *actkeyw;
	// check whether there is data
	// in the extension;
	// set the flag and skip to
	// the next HDU
	if (h->fitsdatasize())
	{
		hasdata = True;
		h->skip();
	}

	// get the extension name
	actkeyw = h->kw("EXTNAME");
	if (actkeyw){
		extname = actkeyw->asString();
		extname.trim();
	}

	// get the extension version
	actkeyw = h->kw("EXTVER");
	if (actkeyw)
		extversion = actkeyw->asInt();

	// add the extension information to the list;
	// enhance the HDU number
	extensions_p[numhdu_p++] = FITSExtInfo(fitsname(True), actindex, extname, extversion, hasdata);
}

FITSExtInfo::FITSExtInfo(const String &name, const uInt &extindex, const String &extname,
		const Int &extversion, const Bool &hasdata)
: name_p       (name),
  extindex_p   (extindex),
  extname_p    (extname),
  extversion_p (extversion),
  hasdata_p    (hasdata)
{
	// make sure the extension
	// name is upper case
	extname_p.upcase();
}

FITSExtInfo::~FITSExtInfo()
{
}

Bool FITSExtInfo::operator==(const FITSExtInfo &extinfo)
{
	if (name_p != extinfo.name_p)
		return False;

	if (extinfo.extname_p.length() > 0 && extinfo.extversion_p > -1)
	{
		//cout << "Comparing extname and extversion" << endl;
		if (extname_p == extinfo.extname_p && extversion_p == extinfo.extversion_p)
			return True;
	}
	else if (extinfo.extname_p.length() > 0) {
		//cout << "Comparing extname" << endl;
		if (extname_p == extinfo.extname_p)
			return True;
	}
	else {
		//cout << "Comparing index" << endl;
		if (extindex_p == extinfo.extindex_p)
			return True;
	}

	return False;
}

String FITSExtInfo::get_extexpr(void)
{
	String extexpr=name_p + "[";

	if (extname_p.length() > 0){
		extexpr += extname_p;

		if (extversion_p > -1){
		ostringstream os;
		os << extversion_p;
		extexpr += "," + String(os);
		}
	}
	else {
		ostringstream os;
		os << extindex_p;
		extexpr += String(os);
	}

	extexpr += "]";

	return extexpr;
}

} //# NAMESPACE CASA - END

