//# FITSImgParser.cc: Class for parsing multi-extension FITS images
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

#include <casacore/images/Images/FITSImgParser.h>
#include <casacore/fits/FITS/hdu.h>
#include <casacore/fits/FITS/fitsio.h>
#include <casacore/fits/FITS/FITSKeywordUtil.h>
#include <casacore/casa/OS/File.h>
#include <casacore/casa/Utilities/ValType.h>
#include <casacore/casa/BasicSL/String.h>
#include <casacore/casa/Exceptions/Error.h>

#include <casacore/casa/iostream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//const char *FITSImgParser::storeKwords_p[] = {"HDUTYPE", "SCIDATA", "ERRDATA", "QUALDATA",
//			"ERRTYPE", "QUALTYPE", "QUALMASK"};
//const int   FITSImgParser::nKwords_p=7;
const char *FITSImgParser::storeKwords_p[] = {"HDUCLASS", "HDUDOC", "HDUVERS",
		"HDUCLAS1", "HDUCLAS2", "HDUCLAS3","SCIDATA", "ERRDATA", "QUALDATA",	"QUALMASK"};
const int   FITSImgParser::nKwords_p=10;

FITSImgParser::FITSImgParser (const String& name)
: name_p      (name),
  numhdu_p    (0),
  qualimglist_p(0),
  hasmeasurement_p(false)
{
	setup();
	find_qualimgs();
}
 
FITSImgParser::FITSImgParser(const FITSImgParser& other):
  name_p(other.name_p),
  numhdu_p(other.numhdu_p),
  qualimglist_p(other.qualimglist_p),
  hasmeasurement_p(other.hasmeasurement_p)

{
	// allocate the extensions and copy the information
	extensions_p = new FITSExtInfo[other.numhdu_p];
	for (uint32_t index=0; index < numhdu_p; index++){
		extensions_p[index] = other.extensions_p[index];
	}
}

FITSImgParser::~FITSImgParser()
{
	delete []extensions_p;
}

FITSImgParser& FITSImgParser::operator=(const FITSImgParser& other){
   if (this != &other) {
   	name_p           = other.name_p;
   	numhdu_p         = other.numhdu_p;
   	qualimglist_p    = other.qualimglist_p;
   	hasmeasurement_p = other.hasmeasurement_p;

   	// allocate the extensions and copy the information
   	extensions_p = new FITSExtInfo[other.numhdu_p];
   	for (uint32_t index=0; index < numhdu_p; index++){
   		extensions_p[index] = other.extensions_p[index];
   	}
   }
	return *this;
}

String FITSImgParser::fitsname (bool stripPath) const
{
   Path path(name_p);
   if (stripPath) {
      return path.baseName();
   } else {
      return path.absoluteName();
   }
}

int32_t FITSImgParser::get_index(const FITSExtInfo &extinfo)
{
	// go over all extensions
	for (int32_t index=0; index < (int32_t)numhdu_p; index++){
		// return the current index if is was the
		// one you are looking for
		if (extensions_p[index] == extinfo)
			return index;
	}

	// return the sign for 'not found'
	return -1;
}

int32_t FITSImgParser::find_extension(const String &extname, const int32_t &extversion){

	// generate an extinfo object from the input
	FITSExtInfo   fext_info = FITSExtInfo(fitsname(true), 0, extname, extversion, true);

	// return its index
	return get_index(fext_info);
}

uint32_t FITSImgParser::get_firstdata_index(void){
	// go over all extensions
	for (uint32_t index=0; index < numhdu_p; index++){
		// check for data
		if (extensions_p[index].has_data())
			// return the index
			return index;
	}
	// return the number of extension
	// as default
	return numhdu_p;
}

String FITSImgParser::get_extlist_string(const String &delimiter, const String &qualmarker,
		const String &fitsmarker, const bool &listall)
{
	String bigString="";

	// add the quality image sets
	if (listall){
		for (uint32_t index=0; index < qualimglist_p.size(); index++){
			bigString += qualmarker + fitsname(true) + String("[") + qualimglist_p(index) + String("]") + delimiter;
		}
	}

	// add the individual extensions
	for (uint32_t index=0; index < numhdu_p; index++){
		if (extensions_p[index].has_data())
			bigString += fitsmarker + extensions_p[index].get_extexpr() + delimiter;
	}

	// return the String
	return bigString;
}

bool FITSImgParser::is_qualityimg(const String &extexpr){

	bool qualityimg;
	Vector<String> extlist;

	// extract the list of extensions from the
	// extension expression
	// ok = get_extlist(extexpr, extlist);
	get_extlist(extexpr, extlist);

	if (extlist.size()<2){
		//cout << "Only one extension given!" << endl;
		return false;
	}
	else if(extlist.size()>3){
		//cout << "More than three extensions given!" << endl;
		return false;
	}

	// check for integer values in the extension list,
	// which indicates rather an extension version
	// number and not a quality image
	for (uint32_t index=0; index<extlist.size();index++){
		if (String::toInt(extlist(index))){
			//cout << "The extension: " << extlist(index) << " does not exist!" << endl;
			return false;
		}
	}

	// make sure all extensions do exist,
	// store their index
	Vector<int32_t> extindex(extlist.size());
	for (uint32_t index=0; index<extlist.size();index++){
		extindex(index) = find_extension(extlist(index));
		if (extindex(index)<0){
			throw (AipsError("FITSImgParser::is_qualityimg - "
					+ fitsname(true) + " does not have an extension: " + extlist(index)));
		}
	}

	// create list for marking the identified extensions
	Vector<bool> identified(extlist.size(), false);

	// get the data extension
	// return "False" if there is no data extension
	int32_t data_ext = get_dataindex(extindex);
	if (data_ext < 0)
		return false;

	// mark the data extension as identified
	for (uint32_t index=0; index<extindex.size();index++){
		if (data_ext == extindex(index))
			identified(index) = true;
	}

	// search for the error extension and mark
	// it as identified
	String error_ext   = get_errorext(data_ext);
	if (error_ext.size() > 0){
		for (uint32_t index=0; index<extlist.size();index++){
			if (!error_ext.compare(extlist(index)))
				identified(index) = true;
		}
	}

	String mask_ext("");
	/* Loading a mask does not yet work,
	 * hence the identification makes no sense
	// search for the mask extension and mark
	// it as identified
	String mask_ext = get_maskext(data_ext);
	*/
	if (mask_ext.size() > 0){
		for (uint32_t index=0; index<extlist.size();index++){
			if (!mask_ext.compare(extlist(index)))
				identified(index) = true;
		}
	}

	// check whether all given extensions
	// have been identified
	qualityimg=true;
	for (uint32_t index=0; index<identified.size();index++){
		if (!identified(index)){
			qualityimg = false;
		}
	}

	// the final result
	return qualityimg;
}

bool FITSImgParser::get_quality_data(const String &extexpr, int32_t &data_HDU, int32_t &error_HDU,
		                                 String &error_type, int32_t &mask_HDU, String &mask_type, int32_t &mask_value){
	Vector<String> extlist;

	// give some defaults for
	// return values that may
	// not be set
	error_type=String("");
	mask_type =String("");
	mask_value=0;

	// extract the list of extensions from the
	// extension expression
	get_extlist(extexpr, extlist);

	// store the index of each extension
	Vector<int32_t> extindex(extlist.size(), -1);
	for (uint32_t index=0; index<extlist.size();index++)
		extindex(index) = find_extension(extlist(index));

	// identify the data extension
	data_HDU = get_dataindex(extindex);

	if (data_HDU > -1){
		// search for the error extension
		String error_ext   = get_errorext(data_HDU);
		if (error_ext.size() > 0){

			// get the extension  index
			error_HDU = find_extension(error_ext);

			// if the extension exists
			if (error_HDU > -1){
				FitsKeyword *actkeyw;

				// extract the keyword "HDUCLAS3"
				actkeyw = extensions_p[error_HDU].get_keyword(String("HDUCLAS3"));

				// check whether the keyword exists
				if (actkeyw){

					// convert the keyword to string
					String kw_errtype = String(actkeyw->asString());
					kw_errtype.trim();

					// set the value if possible
					if (kw_errtype.size()>0)
						error_type = kw_errtype;
				}
			}
		}
		else{
			// explicitly set the default
			error_HDU =-1;
		}


		String mask_ext("");
		/* Loading a mask does not yet work,
		 * hence the identification makes no sense
		// search for the mask extension
		String mask_ext = get_maskext(data_HDU);
		 */

		if (mask_ext.size() > 0){

			// get the extension  index
			mask_HDU = find_extension(mask_ext);

			// if the extension exists
			if (mask_HDU > -1){
				FitsKeyword *actkeyw;

				// extract the keyword "HDUCLAS3"
				actkeyw = extensions_p[mask_HDU].get_keyword(String("HDUCLAS3"));

				// check whether the keyword exists
				if (actkeyw){

					// convert the keyword to string
					String kw_masktype = String(actkeyw->asString());
					kw_masktype.trim();

					// set the value if possible
					if (kw_masktype.size()>0)
						mask_type = kw_masktype;
				}

				// extract the keyword "QUALMASK"
				actkeyw = extensions_p[mask_HDU].get_keyword(String("QUALMASK"));

				// check whether the keyword exists
				if (actkeyw){

					// convert the keyword to int32_t
					int32_t kw_maskval = actkeyw->asInt();

					// set the value if possible
					if (kw_maskval)
						mask_value = kw_maskval;
				}
			}
		}
		else{
			// explicitly set the default
			mask_HDU =-1;
		}
	}
	else{
		// set defaults
		error_HDU =-1;
		mask_HDU  =-1;
	}
	return true;
}

void FITSImgParser::setup(void)
{
	// make sure a proper name is defined
	if (name_p.empty()) {
		throw AipsError("FITSImgParser::setup - Given file name is empty");
	}

	// get the pathname
	Path path(name_p);
	String fullName = path.absoluteName();

	// open the fits image
	FitsInput fin(path.expandedName().chars(),FITS::Disk);
	if (fin.err() == FitsIO::IOERR)
		throw (AipsError("FITSImgParser::setup - "+name_p+" Error opening FITS input."));
	else if (fin.err())
		throw (AipsError("FITSImgParser::setup - "+name_p+" Error reading initial record -- exiting."));

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

	uint32_t extindex = 0;
	bool isfitsimg=true;
	while (fin.rectype() != FITS::EndOfFile && isfitsimg && !fin.err() && extindex < (uint32_t)num_hdu){
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
				isfitsimg = false;
				break;
			case FITS::AsciiTableHDU:
				isfitsimg = false;
				break;
			case FITS::BinaryTableHDU:
				isfitsimg = false;
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
			throw (AipsError("FITSImgParser::setup - Bad Record encountered"));
		}else if (fin.rectype() == FITS::SpecialRecord) {
			throw (AipsError("FITSImgParser::setup - Special Record encountered"));
		}
	}
}

void FITSImgParser::process_extension(HeaderDataUnit *h,const uint32_t &extindex)
{
	String extname="";
	int32_t    extversion=-1;
	bool   hasdata=false;
	uint32_t   actindex=extindex-1;
	FITSExtInfo fExtInfo;
	const FitsKeyword *actkeyw;
	FitsKeywordList kwlist=FitsKeywordList();

	// check whether there is data
	// in the extension;
	// set the flag and skip to
	// the next HDU
	if (h->fitsdatasize())
	{
		hasdata = true;
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

	// get all relevant keywords
	for (int index=0; index < nKwords_p; index++){
		actkeyw = h->kw((char *)storeKwords_p[index]);
		if (actkeyw){
			FitsKeyword *newkey = new FitsKeyword(*actkeyw);
			kwlist.insert(*newkey);
		}
	}

	// create an Info object and add the keywords
	fExtInfo = FITSExtInfo(fitsname(true), actindex, extname, extversion, hasdata);
	fExtInfo.add_kwlist(kwlist);

	// add the extension information to the list;
	// enhance the HDU number
	extensions_p[numhdu_p++] = fExtInfo;
}

bool FITSImgParser::get_extlist(const String &extexpr, Vector<String> &extlist){

	String extexpr_l = extexpr;
	extexpr_l.trim();

	// there is nothing to do
	if (extexpr_l.size() < 1)
		return true;

	int32_t open_bracepos = 0;
	int32_t close_bracepos= extexpr_l.size();

	// check whether the strings ends with "]"
	if (!extexpr_l.compare(extexpr_l.size()-1, 1, "]", 1)){
		close_bracepos = extexpr_l.size()-1;
	}

	// check whether the strings ends with "]"
	if (!extexpr_l.compare(0, 1, "[", 1)){
		open_bracepos   = 1;
		close_bracepos -= 1;
	}

	String  extexpr_b = String(extexpr_l, open_bracepos, close_bracepos);
	int32_t n_comma = extexpr_b.freq(",");

	int32_t f_start=0;
	for (int32_t index=0; index<n_comma; index++){
		// find the next comma
		int32_t c_pos = extexpr_b.find(",", f_start);

		// create a substring
		String tmp = String(extexpr_b, f_start, c_pos-f_start);
		tmp.trim();

		// extend the extension list and append the substring
		extlist.resize(extlist.size()+1, true);
		extlist(extlist.size()-1)=tmp;
		f_start=c_pos+1;
	}
	String tmp = String(extexpr_b, f_start, extexpr_b.size()-f_start);
	tmp.trim();
	tmp.upcase();

	// extend the list and append the substring
	extlist.resize(extlist.size()+1, true);
	extlist(extlist.size()-1)=tmp;

	return true;
}

int32_t FITSImgParser::get_dataindex(const Vector<int32_t> &extindex){
	// go over all extensions
	for (uint32_t index=0; index<extindex.size();index++){
		// check whether the extension exists
		if (extindex(index) > -1)
			// check whether it is a data extension
			if (index_is_HDUtype(extindex(index), "DATA"))
				// return the index
				return extindex(index);
	}
	// -1 for not found
	return -1;
}

String FITSImgParser::get_errorext(const int32_t &ext_index){
	String error_ext;
	FitsKeyword *actkeyw;

	// make sure the extension index does exist
	if (ext_index < 0 || ext_index > (int32_t)numhdu_p-1){
		ostringstream os;
		os << ext_index;
		throw (AipsError("FITSImgParser::get_errorext - Can not access extension: "+String(os)+" in image: " + fitsname(true)));
	}

	// extract the keyword "ERRDATA"
	actkeyw = extensions_p[ext_index].get_keyword(String("ERRDATA"));

	// check whether the keyword exists
	if (actkeyw){
		// convert the keyword to string
		String kw_error = String(actkeyw->asString());
		kw_error.trim();
		kw_error.upcase();

		// check whether the HDUtype keyword
		// has the correct value
		if (kw_error.size()>0){
			int32_t err_index = find_extension(kw_error);
			if (err_index > -1 && index_is_HDUtype(err_index, "ERROR"))
				error_ext=kw_error;
		}
	}
	return error_ext;
}

String FITSImgParser::get_maskext(const int32_t &ext_index){
	String mask_ext;
	FitsKeyword *actkeyw;

	// make sure the extension index does exist
	if (ext_index < 0 || ext_index > (int32_t)numhdu_p-1){
		ostringstream os;
		os << ext_index;
		throw (AipsError("FITSImgParser::get_maskext - Can not access extension: "+String(os)+" in image: " + fitsname(true)));
	}

	// extract the keyword "QUALDATA"
	actkeyw = extensions_p[ext_index].get_keyword(String("QUALDATA"));

	// check whether the keyword exists
	if (actkeyw){
		// convert the keyword to string
		String kw_mask = String(actkeyw->asString());
		kw_mask.trim();
		kw_mask.upcase();

		// check whether the HDUtype keyword
		// has the correct value
		if (kw_mask.size()>0){
			int32_t mask_index = find_extension(kw_mask);
			if (mask_index > -1 && index_is_HDUtype(mask_index, "QUALITY"))
				mask_ext=kw_mask;
		}
	}
	return mask_ext;
}

bool FITSImgParser::confirm_fix_keywords(const int32_t &ext_index){
	FitsKeyword *actkeyw;

	Vector<String> key_words(3), key_values(3);
	key_words(0) =String("HDUCLASS");  key_words(1)  = String("HDUDOC"); key_words(2)  = String("HDUCLAS1");
	key_values(0)=String("ESO");       key_values(1) = String("DICD");   key_values(2) = String("IMAGE");
	//key_words(3) = String("HDUVERS");  key_values(3) = String("DICD version 6");

	for (uint32_t index=0; index<key_words.size(); index++){
		// extract the keyword
		actkeyw = extensions_p[ext_index].get_keyword(key_words(index));

		// check whether the keyword exists
		if (actkeyw){
			// convert the keyword to string
			String kword_string = String(actkeyw->asString());
			kword_string.trim();

			// compare the keyword value and return true if they are identical
			if (kword_string.size()<1 || kword_string.compare(key_values(index)))
				return false;
		}
		else{
			return false;
		}
	}
	return true;
}

bool FITSImgParser::index_is_HDUtype(const int32_t &ext_index, const String &hdutype){

	FitsKeyword *actkeyw;

	// make sure the extension index does exist
	if (ext_index < 0 || ext_index > (int32_t)numhdu_p-1){
		ostringstream os;
		os << ext_index;
		throw (AipsError("FITSImgParser::index_is_HDUtype - Can not access extension: "+String(os)+" in image: " + fitsname(true)));
	}

	// verify the mandatory, fixed keywords
	if (!confirm_fix_keywords(ext_index))
		return false;

	// extract the keyword "HDUCLAS2"
	actkeyw = extensions_p[ext_index].get_keyword(String("HDUCLAS2"));

	// check whether the keyword exists
	if (actkeyw){
		// convert the keyword to string
		String kw_hdutype = String(actkeyw->asString());
		kw_hdutype.trim();

		// compare the keyword value and return true if they are identical
		if (kw_hdutype.size()>0 && !kw_hdutype.compare(hdutype))
			return true;
	}

	// return false as default
	return false;
}

bool FITSImgParser::find_qualimgs(void)
{
	// go over all extensions
	for (uint32_t index=0; index < numhdu_p; index++){

		// identify the current extension
		// as a data extension
		if (index_is_HDUtype((int32_t)index, "DATA")){
			String errext, maskext;

			// search the corresponding error extension
			errext = get_errorext((int32_t)index);

			// confirm the existence
			// of the error extension
			if (errext.size()>0){
				int32_t err_index = find_extension(errext);
				if (err_index < 0)
					errext = String("");
			}

			maskext = String("");
			/* Loading a mask does not yet work,
			 * hence the identification makes no sense
			// search the corresponding mask extension
			maskext = get_maskext((int32_t)index);

			// confirm the existence of the mask extension
			if (maskext.size()>0){
				int32_t mask_index = find_extension(maskext);
				if (mask_index < 0)
					maskext = String("");
			}
			*/

			// if the data extension has an error or mask extension
			if (errext.size() > 0 || maskext.size()>0){

				// compose the string representation
				String qualimgstr(extensions_p[index].get_extname());
				if (errext.size()>0)
					qualimgstr += String(",") + errext;
				if (maskext.size()>0)
					qualimgstr += String(",") + maskext;

				// extend the list and append the string representation
				qualimglist_p.resize(qualimglist_p.size()+1, true);
				qualimglist_p(qualimglist_p.size()-1)=qualimgstr;
			}
		}
	}
	return true;
}

FITSExtInfo::FITSExtInfo(const String &name, const uint32_t &extindex, const String &extname,
		const int32_t &extversion, const bool &hasdata)
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

// Copy constructor (reference semantics)
FITSExtInfo::FITSExtInfo(const FITSExtInfo& other)
	:name_p(other.name_p),
	 extindex_p(other.extindex_p),
	 extname_p(other.extname_p),
	 extversion_p(other.extversion_p),
	 hasdata_p(other.hasdata_p),
	 kwlist_p(other.kwlist_p)
{
}

// Destructor
FITSExtInfo::~FITSExtInfo()
{
}

// Assignment (reference semantics)
FITSExtInfo& FITSExtInfo::operator=(const FITSExtInfo& other){
   if (this != &other) {
   	name_p       = other.name_p;
   	extindex_p   = other.extindex_p;
   	extname_p    = other.extname_p;
   	extversion_p = other.extversion_p;
   	hasdata_p    = other.hasdata_p;
   	kwlist_p     = FitsKeywordList(other.kwlist_p);
   }
	return *this;
}

bool FITSExtInfo::operator==(const FITSExtInfo &extinfo)
{
	if (name_p != extinfo.name_p)
		return false;

	if (extinfo.extname_p.length() > 0 && extinfo.extversion_p > -1)
	{
		//cout << "Comparing extname and extversion" << endl;
		if (extname_p == extinfo.extname_p && extversion_p == extinfo.extversion_p)
			return true;
	}
	else if (extinfo.extname_p.length() > 0) {
		//cout << "Comparing extname" << endl;
		if (extname_p == extinfo.extname_p)
			return true;
	}
	else {
		//cout << "Comparing index" << endl;
		if (extindex_p == extinfo.extindex_p)
			return true;
	}

	return false;
}

String FITSExtInfo::get_extexpr(void)
{
        String extexpr=name_p + "[" + String::toString(extindex_p);

	if (extname_p.length() > 0){
		extexpr += ':' + extname_p;

		if (extversion_p > -1){
		ostringstream os;
		os << extversion_p;
		extexpr += "," + String(os);
		}
	}

	extexpr += "]";

	return extexpr;
}

void FITSExtInfo::add_kwlist(FitsKeywordList &kwlist){
	FitsKeyword *actkey;

	// if there is at least one keyword
	if (!kwlist.isempty()){

		// iterate over the list
		kwlist.first();
		actkey = kwlist.next();
		while (actkey){

			// copy the current  keyword
			FitsKeyword *newkey = new FitsKeyword(*actkey);
			kwlist_p.insert(*newkey);

			// go to the next keyword
			actkey = kwlist.next();
		}
	}
}

} //# NAMESPACE CASACORE - END

