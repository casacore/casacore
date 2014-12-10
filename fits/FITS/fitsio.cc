//# fitsio.cc:
//# Copyright (C) 1993,1994,1995,1996,1997,1999,2000,2001,2002,2003
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

# include <casacore/fits/FITS/hdu.h>
# include <casacore/fits/FITS/fitsio.h>
# include <casacore/casa/BasicSL/String.h>
# include <casacore/casa/Containers/Block.h>
# include <casacore/casa/string.h>
# include <casacore/casa/sstream.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

// using cfitsio of NASA
// can't use "" to replace <>.
// The following is included through fitsio.h-->blockio.h already! 
//# include <cfitsio/fitsio.h>     
//# include <cfitsio/fitsio2.h>
//
FitsIO::~FitsIO() {
}

FitsInput::~FitsInput() {
    delete &m_fin;
}

FitsDiskInput::~FitsDiskInput() {
}

FitsDiskOutput::~FitsDiskOutput() {
}

FitsStdInput::~FitsStdInput() {
}

FitsStdOutput::~FitsStdOutput() {
}

FitsTape9Input::~FitsTape9Input() {
}

FitsTape9Output::~FitsTape9Output() {
}

//# Cache used to hold errors from read_header_rec, messages and accompanying error levels
Block<String> messages_(32);
Block<Int> errLevels_(32);
uInt nerrs_ = 0;
//============================================================================================
// special error handler function used by read_header_rec
void readHeaderRecErrHandler(const char *errMessage,
        FITSError::ErrorLevel severity) {
    if (nerrs_ >= messages_.nelements()) {
        uInt newSize = messages_.nelements() * 2;
        messages_.resize(newSize, True, True);
        errLevels_.resize(newSize, True, True);
    }
    messages_[nerrs_] = String(errMessage);
    errLevels_[nerrs_] = Int(severity);
    nerrs_++;
}
//=============================================================================================
void FitsInput::errmsg(FitsErrs e, const char *s) {
    //cout<<"[FitsInput::errmsg] called."<<endl;
    static char msgstring[180];
    ostringstream msgline;
    //msgline << "FitsInput Error: ";
    if (m_fin.fname() == 0 || *m_fin.fname() == '\0')
        msgline << "File Descriptor " << m_fin.fdes();
    else
        msgline << "File " << m_fin.fname();
    msgline << " Physical record " << m_fin.blockno() << " logical record "
            << m_fin.recno() << " --\n\t" << s << endl;
    m_err_status = e;
    // all FitsIO input error messages are WARNINGS
    strncpy(msgstring, msgline.str().c_str(), sizeof(msgstring) - 1);
    m_errfn(msgstring, FITSError::WARN);
}
//==========================================================================================
// Implement skip function with cfitsio of NASA. GYL
char *FitsDiskInput::skip(int n) { // skip n logical records and read
    // Is the contents of the m_buffer enough for skip()? if yes,
    // convert n to OFF_T, just in case n*m_recsize is larger than maximum int.
    if ((OFF_T(n) * m_recsize) <= OFF_T(m_iosize - m_current)) {
        m_current += n * m_recsize;
        m_rec_no = m_rec_no + n;
        return read();
    }
    // The situation when m_buffer has less logical record than n:
    // check that we do not exceed number of rows in the file
    int l_endrow = ((m_fptr->Fptr)->bytepos - (m_iosize - m_current))
            / m_recsize + n;
    int l_totalrow = ((m_fptr->Fptr)->filesize) / m_recsize;
    if (l_endrow >= l_totalrow) {
        errmsg(READERR,
                "Attempt to read past end of file [FitsdiskInput::skip()]");
        return (0);
    }

    // move the i/o pointer to the end position of the skipped block.
    // (m_iosize - m_current ) is the bytes of data left within the m_buffer
    // still need to test this part with big fits file.
    OFF_T bytpost = (m_fptr->Fptr)->bytepos + (m_recsize * uInt(n)) - (m_iosize
            - m_current);
    int l_status = 0;
    ffmbyt(m_fptr, bytpost, REPORT_EOF, &l_status);
    if (l_status) {
        fits_report_error(stderr, l_status); /* print error report */
        errmsg(READERR, "bytepos setting error [FitsdiskInput::skip()]");
        return (0);
    } else {
        // (m_iosize-m_current) is in the previous m_block_no already.
        int l_phy_rec = (n - (m_iosize - m_current) / m_recsize) / m_nrec;
        m_block_no += l_phy_rec;
        m_rec_no += n;
        m_iosize = 0; // the m_buffer data is all used.
        m_current = 0; // reset the buffer position to beginning.

    }

    return read();
}
//===============================================================================================
BlockInput &FitsInput::make_input(const char *n, const FITS::FitsDevice &d,
        int b, FITSErrorHandler errhandler) {
    BlockInput *bptr = 0;

    switch (d) {
    case FITS::Disk:
        bptr = new FitsDiskInput(n, m_recsize, b, errhandler);
        break;
    case FITS::Tape9:
        //errmsg(IOERR,"FITS::Tape9 is not supported. Please use FITS::DISK!");
        bptr = new FitsTape9Input(n, m_recsize, b, errhandler);
        break;
    case FITS::Std:
        bptr = new FitsStdInput(m_recsize, errhandler);
        break;
    }
    // Dereferences a null pointer if "d" was not caught in the above switch.
    return *bptr;
}
//=============================================================================================
void FitsOutput::errmsg(FitsErrs e, const char *s) {
    static char msgstring[180];
    ostringstream msgline;
    msgline << "FitsOutput error:  ";
    if (m_fout.fname() == 0 || *m_fout.fname() == '\0')
        msgline << "File Descriptor " << m_fout.fdes();
    else
        msgline << "File " << m_fout.fname();
    msgline << " Physical record " << m_fout.blockno() << " logical record "
            << m_fout.recno() << " --\n\t" << s << endl;
    m_err_status = e;
    // all FitsIO output error messages are SEVERE
    strncpy(msgstring, msgline.str().c_str(), sizeof(msgstring) - 1);
    m_errfn(msgstring, FITSError::SEVERE);
}
//==============================================================================================
BlockOutput &FitsOutput::make_output(const char *n, const FITS::FitsDevice &d,
        int b, FITSErrorHandler errhandler)

{
    BlockOutput *bptr = 0;
    switch (d) {
    case FITS::Disk:
        bptr = new FitsDiskOutput(n, m_recsize, b, errhandler);
        break;
    case FITS::Tape9:
        //errmsg(IOERR,"FITS::Tape9 is not supported. Please use FITS::DISK!");
        bptr = new FitsTape9Output(n, m_recsize, b, errhandler);
        break;
    case FITS::Std:
        bptr = new FitsStdOutput(m_recsize, errhandler);
        break;
    }

    // Dereferences a null pointer if "d" was not caught in the above
    // switch.
    return *bptr;
}
//========================================================================================
FitsOutput::FitsOutput(const char *n, const FITS::FitsDevice &d, int b,
        FITSErrorHandler errhandler) :
    FitsIO(errhandler), m_fout(make_output(n, d, b, errhandler)),
            m_required_keys_only(TRUE) {

    if (m_fout.err()) {
        m_rec_type = FITS::EndOfFile;
        errmsg(IOERR, "Error constructing output");
        return;
    }
    m_curr = new char[m_recsize];
    if (!m_curr) {
        m_rec_type = FITS::EndOfFile;
        errmsg(MEMERR, "Could not allocate storage for output buffer.");
    }
    m_fptr = m_fout.getfptr();
}
//=========================================================================================
FitsOutput::FitsOutput(FITSErrorHandler errhandler) :
    FitsIO(errhandler), m_fout(*(BlockOutput *) (new FitsStdOutput(m_recsize,
            errhandler))), m_required_keys_only(TRUE) {

    if (m_fout.err()) {
        m_rec_type = FITS::EndOfFile;
        errmsg(IOERR, "Error constructing output");
        return;
    }
    m_curr = new char[m_recsize];
    if (!m_curr) {
        m_rec_type = FITS::EndOfFile;
        errmsg(MEMERR, "Could not allocate storage for output buffer.");
    }
}
//==========================================================================================
FitsOutput::~FitsOutput() {

    if (hdu_inprogress()) {
        errmsg(BADOPER, "ERROR! Output closed before HDU was complete.");
    }
    delete &m_fout;
    delete[] m_curr;
}
//========================================================================================
void FitsInput::init() {
    if (m_fin.err())
        errmsg(IOERR, "[FitsInput::init()] Failed to construct input");
    else {
        //cout<<"[FitsInput::init()] First call to BlockInput::read()." << endl;
        m_curr = m_fin.read();
        m_got_rec = True;
        if (!m_curr) {
            errmsg(EMPTYFILE, "[FitsInput::init()] This is an empty file");
            m_rec_type = FITS::EndOfFile;
            return;
        }
        if (m_fin.err()) {
            errmsg(IOERR, "[FitsInput::init()] Failed to read the first record");
            m_rec_type = FITS::BadBeginningRecord;
            return;
        }
        m_kc.parse(m_curr, m_kw, 0, m_errfn, True);

        // get the fitsfile pointer
        m_fptr = m_fin.getfptr();

        HeaderDataUnit::HDUErrs n;
        if (!HeaderDataUnit::determine_type(m_kw, m_hdu_type, m_data_type,
                m_errfn, n)) {
            errmsg(BADBEGIN,
                    "[FitsInput::init()] Unrecognizable record at the beginning");
            m_rec_type = FITS::BadBeginningRecord;
            return;
        }
        if (!(m_hdu_type == FITS::PrimaryArrayHDU || m_hdu_type
                == FITS::PrimaryTableHDU || m_hdu_type == FITS::PrimaryGroupHDU)) {
            errmsg(NOPRIMARY,
                    "[FitsInput::init()] Missing primary header-data unit");
        } else {
            m_isaprimary = True;
            if (m_kw(FITS::SIMPLE)->asBool() == True)
                m_valid_fits = True;
            else
                m_errfn(
                        "Value of keyword SIMPLE is FALSE; this file may not be a "
                            "valid FITS file [FitsInput::init()].",
                        FITSError::WARN);
            if (m_kw(FITS::EXTEND))
                if (m_kw.curr()->asBool() == True)
                    m_extend = True;
        }
        m_rec_type = FITS::HDURecord;
        // Next block of code is to get the total number of hdu in this fits file
        // remember the cfitsio bytepos before calling any cfitsio function
        OFF_T l_bytepos = (m_fptr->Fptr)->bytepos;
        // get the total number of hdu in this fits file
        int l_status = 0;
        if (ffthdu(m_fptr, &m_thdunum, &l_status) > 0) {
            fits_report_error(stderr, l_status); // print error report
            errmsg(IOERR,
                    "[FitsInput::init()] Failed to get total number of HDU.");
            return;
        }
        // set the cfitsio bytepos to what it was at begnning of this method.
        if (l_bytepos < ((m_fptr->Fptr)->filesize)) {
            if (ffmbyt(m_fptr, l_bytepos, REPORT_EOF, &l_status) > 0) {
                fits_report_error(stderr, l_status); // print error report
                errmsg(BADOPER, "[FitsInput::init()] bytepos setting error!");
            }
        } else {
            (m_fptr->Fptr)->bytepos = l_bytepos;
        }
    }
}
//===============================================================================================
// return the header of the chdu as a Vector of Strings.
Vector<String> FitsInput::kwlist_str(Bool length80) {
    Vector<String> cards;
    if (!m_header_done) {
        cout << "[FitsInput::kwlist_str()] If you need call this method, "
            "you should do so before reading any data from CHDU." << endl;
        return cards;
    } else {
        // remember the cfitsio bytepos before calling any cfitsio function
        OFF_T l_bytepos = (m_fptr->Fptr)->bytepos;
        int l_keysexist = 0, l_morekeys = 0, l_status = 0;
        // get the total number of keywords in the chdu
        if (ffghsp(m_fptr, &l_keysexist, &l_morekeys, &l_status)) {
            fits_report_error(stderr, l_status); // print error report
            cout
                    << "[FitsInput::kwlist_str()] Failed to get total number of keywords in CHDU."
                    << endl;
            return cards;
        }
        // get every card image as a char* and store them into cards.
        char cardImg[81];
        ;
        cards.resize(l_keysexist + 1);
        for (int keynum = 1; keynum < l_keysexist + 1; keynum++) {
            if (ffgrec(m_fptr, keynum, cardImg, &l_status)) { // error reading card
                fits_report_error(stderr, l_status); // print error report
                errmsg(BADOPER,
                        "[FitsInput::kwlist_str()] Failed to read the card!");
            } else {
                String onecard(cardImg);
                cards[keynum - 1] = onecard;
            }
        }
        // since keysexist does not count the END keyword, we add it in.
        String endCard("END");
        cards[l_keysexist] = endCard;
        // set the cfitsio bytepos to what it was at begnning of this method.
        if (l_bytepos < ((m_fptr->Fptr)->filesize)) {
            if (ffmbyt(m_fptr, l_bytepos, REPORT_EOF, &l_status) > 0) {
                fits_report_error(stderr, l_status); // print error report
                errmsg(BADOPER,
                        "[FitsInput::kwlist_str()] bytepos setting error!");
            }
        } else {
            (m_fptr->Fptr)->bytepos = l_bytepos;
        }
        // The next block is added by Neil Killeen
        if (length80) {
            String
                    tmp(
                            "                                                                                ");
            //          12345678901234567890123456789012345678901234567890123456789012345678901234567890
            for (uInt i = 0; i < cards.nelements(); i++) {
                String tmp2(tmp);
                tmp2.replace(0, cards(i).length(), cards(i));
                cards(i) = tmp2;
            }
        }
        return cards;
    }
}

//================================================================================================
// read a special record or an unrecognizable record
char * FitsInput::read_sp() {
    m_err_status = OK;
    if (m_rec_type == FITS::BadBeginningRecord) {
        if (m_got_rec) {
            m_got_rec = False;
            return m_curr;
        }
        m_curr = m_fin.read();
        if (!m_curr) {
            m_rec_type = FITS::EndOfFile;
            m_got_rec = True;
            return 0;
        }
        if (m_fin.err()) {
            errmsg(IOERR, "[FitsInput::read_sp()] Failed to read a sp record.");
            return m_curr;
        }
        m_kw.delete_all();
        m_kc.parse(m_curr, m_kw, 0, m_errfn, True);
        HeaderDataUnit::HDUErrs n;
        if (!HeaderDataUnit::determine_type(m_kw, m_hdu_type, m_data_type,
                m_errfn, n)) {
            return m_curr;
        }
        if (!(m_hdu_type == FITS::PrimaryArrayHDU || m_hdu_type
                == FITS::PrimaryGroupHDU || m_hdu_type == FITS::PrimaryTableHDU)) {
            errmsg(NOPRIMARY,
                    "[FitsInput::read_sp()] Missing primary header-data unit.");
        } else {
            if (m_kw(FITS::SIMPLE)->asBool() == True) {
                m_valid_fits = True;
            } else {
                m_errfn(
                        "[FitsInput::read_sp()] Value of keyword SIMPLE is FALSE; this"
                            " file may not be a valid FITS file.",
                        FITSError::WARN);
            }
            if (m_kw(FITS::EXTEND)) {
                if (m_kw.curr()->asBool() == True) {
                    m_extend = True;
                }
            }
        }
        m_rec_type = FITS::HDURecord;
        m_got_rec = True;
        return 0;
    } else if (m_rec_type == FITS::UnrecognizableRecord) {
        if (m_got_rec) {
            m_got_rec = False;
            return m_curr;
        }
        m_curr = m_fin.read();
        if (!m_curr) {
            m_rec_type = FITS::EndOfFile;
            m_got_rec = True;
            return 0;
        }
        if (m_fin.err()) {
            errmsg(IOERR,
                    "[FitsInput::read_sp()] Failed to read a unrecognizable record.");
            return m_curr;
        }
        m_kw.delete_all();
        m_kc.parse(m_curr, m_kw, 0, m_errfn, True);
        HeaderDataUnit::HDUErrs n;
        if (!HeaderDataUnit::determine_type(m_kw, m_hdu_type, m_data_type,
                m_errfn, n)) {
            return m_curr;
        }
        m_rec_type = FITS::HDURecord;
        m_got_rec = True;
        return 0;
    } else if (m_rec_type == FITS::SpecialRecord) {
        if (m_got_rec) {
            m_got_rec = False;
            return m_curr;
        }
        m_curr = m_fin.read();
        if (!m_curr) {
            m_rec_type = FITS::EndOfFile;
            m_got_rec = True;
            m_err_status = OK;
            return 0;
        }
        if (m_fin.err()) {
            errmsg(IOERR, "[FitsInput::read_sp()] Failed to read a sp record.");
            return m_curr;
        }
        m_err_status = OK;
        return m_curr;
    }
    return 0;
}
//========================================================================================================
// implement read_head_rec() with CFITSIO of NASA
void FitsInput::read_header_rec() {
    // make the next hdu be the chdu of cfitsio and set the file position pointer at the begining of the hdu.
    int l_status = 0, l_hdutype = 0, l_chdunum = 0;
    // get the number of the current hdu. This function returns the number of
    // chdu, not an error code. So we do not check the return.
    ffghdn(m_fptr, &l_chdunum);
    // if there is more hdu, make the next hdu be the current hdu
    if (l_chdunum < m_thdunum) {
        if (ffmrhd(m_fptr, 1, &l_hdutype, &l_status) > 0) {
            fits_report_error(stderr, l_status); // print error report
            errmsg(IOERR,
                    "[FitsInput::read_header_rec()] Failed to move to the next hdu");
            return;
        }
    } else { // reach the end of the fits file, end the program gracefully.
        m_curr = m_fin.read();
        m_got_rec = True;
        if (!m_curr) {
            //cout << "[FitsInput::read_header_rec()] Reached the end of the FITS file" << endl;
            m_rec_type = FITS::EndOfFile;
            return;
        }
        if (m_fin.err()) {
            errmsg(IOERR,
                    "[FitsInput::read_header_rec()] Failed to read first record of new header");
            m_rec_type = FITS::UnrecognizableRecord;
            return;
        }
    }
    // since ffmrhd() reads the header, we need to move the file pointer back
    // to the beginning of the hdu before calling m_fin.read()
    OFF_T l_headstart, l_datastart, l_dataend;
    l_status = 0;
    // get size info of the current HDU
    if (ffghof(m_fptr, &l_headstart, &l_datastart, &l_dataend, &l_status) > 0) {
        fits_report_error(stderr, l_status); // print error report
        errmsg(BADSIZE,
                "[FitsInput::read_header_rec()] Failed to get the size of data.");
        return;
    }
    // move file pointer to the beginning of the new hdu.
    l_status = 0;
    if (ffmbyt(m_fptr, l_headstart, REPORT_EOF, &l_status)) {
        fits_report_error(stderr, l_status); // print error report
        errmsg(IOERR,
                "[FitsInput::read_header_rec()] Failed to move the file pointer to beginning.");
    }
    // reset m_iosize so that next m_fin.read() will start from the beginning of next hdu.
    m_fin.reset_iosize();
    // end of the new code
    m_curr = m_fin.read();
    m_got_rec = True;
    if (!m_curr) {
        //cout << "[FitsInput::read_header_rec()] Reached the end of the FITS file" << endl;;
        m_rec_type = FITS::EndOfFile;
        return;
    }
    if (m_fin.err()) {
        errmsg(IOERR,
                "[FitsInput::read_header_rec()] Failed to read the first record of new header");
        m_rec_type = FITS::UnrecognizableRecord;
        return;
    }
    m_kw.delete_all();
    // reset the cache counter nevertheless
    nerrs_ = 0;
    m_kc.parse(m_curr, m_kw, 0, readHeaderRecErrHandler, True);
    //cout << "[ FitsInput::read_header_rec()] Number of errors from parsing: nerrs_ = " << nerrs_ <<endl;
    uInt parseErrs = nerrs_;
    HeaderDataUnit::HDUErrs n;
    //cout << ">>FitsInput::read_header_rec() - hdu_type=" << m_hdu_type << endl;
    if (!HeaderDataUnit::determine_type(m_kw, m_hdu_type, m_data_type,
            readHeaderRecErrHandler, n)) {
        // in this case, the header is completely bogus, the error messages which
        // convey that are the ones returned by determine_type, the ones returned
        // by parse are useless and needlessly confusing, so don't show them
        //cout<< "[ FitsInput::read_header_rec()] Error mesages from determin_type(): " << endl;
        for (uInt i = parseErrs; i < nerrs_; i++) {
            m_errfn(messages_[i].chars(), FITSError::ErrorLevel(errLevels_[i]));
        }
        nerrs_ = 0;
        m_rec_type = FITS::SpecialRecord;
        return;
    }
    //cout << "<<FitsInput::read_header_rec() + hdu_type=" << m_hdu_type << endl;
    // spit out all of the cached error messages
    // cout<< "[ FitsInput::read_header_rec()] Error message from parsing and determin_type():" << endl;
    for (uInt i = 0; i < nerrs_; i++) {
        m_errfn(messages_[i].chars(), FITSError::ErrorLevel(errLevels_[i]));
    }
    nerrs_ = 0;
    if (m_hdu_type == FITS::PrimaryArrayHDU || m_hdu_type
            == FITS::PrimaryGroupHDU || m_hdu_type == FITS::PrimaryTableHDU) {
        errmsg(BADPRIMARY,
                "[FitsInput::read_header_rec()] Misplaced primary header-data unit.");
    }
    m_rec_type = FITS::HDURecord;
    m_header_done = False;
    //cout << "<<FitsInput::read_header_rec() ~ hdu_type=" << m_hdu_type << endl;
}
//========================================================================================
// Implement the skip_hdu with cfitsio of NASA
int FitsInput::skip_hdu() { //Skip an entire header-data unit
    m_err_status = OK;
    if ((m_rec_type != FITS::HDURecord) /*|| m_header_done*/) {
        errmsg(BADOPER, "[FitsInput::skip_hdu()] not a hdu record");
        return (int) m_err_status;
    }
    if (/*(m_rec_type != FITS::HDURecord) || */m_header_done) {
        //errmsg(BADOPER,"[FitsInput::skip_hdu()] hdu already done");
        read_header_rec(); // this will set the current hdu_type etc.
        if (err()) {
            return (int) m_err_status;
        }
        return 0;
    }

    // check if the header of the current HDU is properly ended
    char l_message[FLEN_ERRMSG];
    char l_keyname[FLEN_KEYWORD];
    char l_keyval[FLEN_VALUE];
    char l_card[FLEN_CARD];
    char* l_comm = NULL;

    int l_status = 0;

    OFF_T l_headstart, l_datastart, l_dataend;
    // get size info of the HDU to be skipped
    if (ffghof(m_fptr, &l_headstart, &l_datastart, &l_dataend, &l_status) > 0) {
        fits_report_error(stderr, l_status); // print error report
        errmsg(BADSIZE,
                "[FitsInput::read_header_rec()] Failed to get the size of data.");
        return (int) m_err_status;
    }
    m_skipHDU_size = l_dataend - l_headstart;

    //    --------------------------------------------------------------------------
    int l_found_end = 0, l_namelen = 0;
    for (int nextkey = 1; !l_found_end; nextkey++) {
        // get next keyword
        // don't use ffgkyn here because it trys to parse the card to read
        // the value string, thus failing to read the file just because of
        // minor syntax errors in optional keywords.
        if (ffgrec(m_fptr, nextkey, l_card, &l_status) > 0) // get the 80-byte card
        {
            if (l_status == KEY_OUT_BOUNDS) {
                l_found_end = 1; // simply hit the end of the header
                l_status = 0; // reset error status
                //cout<<"[FitsInput::skip_hdu()] Found END keyword "<<endl;
            } else {
                errmsg(MISSKEY,
                        "[FitsInput::skip_hdu()] Failed to find the END keyword in header.");
                return (int) m_err_status;
            }
        } else { // got the next keyword without error
            ffgknm(l_card, l_keyname, &l_namelen, &l_status); // get the keyword name

            if (fftrec(l_keyname, &l_status) > 0) { // test keyword name; catches no END
                sprintf(
                        l_message,
                        "Name of keyword no. %d contains illegal character(s): %s",
                        nextkey, l_keyname);
                errmsg(MISSKEY, l_message);

                if (nextkey % 36 == 0) { // test if at beginning of 36-card record.
                    errmsg(MISSKEY,
                            "[FitsInput::skip_hdu()] Possible missing END keyword.");
                    return (int) m_err_status;
                }
            }
            if (!strcmp(l_keyname, "END")) {
                l_found_end = 1;
            }
        }
    }

    //    --------------------------------------------------------------------------
    // These functions don't work for END keyword!
    /*if(ffgcrd( m_fptr, l_keyname, l_card, &l_status )){
     //if(ffgkey( m_fptr, l_keyname, l_keyval, l_comm, &l_status ))
     fits_report_error(stderr, l_status); // print error report
     errmsg(BADOPER,"[FitsInput::skip_hdu()] Missing END keyword.");
     return -1;
     }
     */
    // check if the m_extend data member is set
    l_status = 0;
    if (!m_extend) {
        //l_keyname = "EXTEND";
        strcpy(l_keyname, "EXTEND");
        if (!ffgkey(m_fptr, l_keyname, l_keyval, l_comm, &l_status)) {
            if (l_keyval[0] == 'T')
                m_extend = True;
        }
    }
    // reset the m_iosize to 0, so that next m_fin.read() will start from where the file position pointer is;
    // Since read_header_rec() will move the chdu forward for one, no need to call ffmrhd() here. And
    // read_header_rec() will also move the cfitsio bytepos.
    m_fin.reset_iosize();
    // read header to get ready for process_header
    read_header_rec(); // this will set the current hdu_type etc.
    if (err()) {
        return (int) m_err_status;
    }
    return 0;
}
//=================================================================================
int FitsInput::process_header(FITS::HDUType t, FitsKeywordList &uk) {
    //cout << "[FitsInput::process_header] t=" << t << " hdu_type=" << m_hdu_type
    //     << " m_header_done=" << m_header_done << endl;
    m_err_status = OK;
    m_item_size = 0;
    m_data_type = FITS::NOVALUE;
    m_data_size = 0;
    m_bytepos = 0;
    m_curr_size = 0;
    if (m_rec_type != FITS::HDURecord) {
        errmsg(BADOPER, "[FitsInput::process_header()] Not a hdu record");
        return -1;
    }
    if (m_hdu_type != t) {
        errmsg(BADOPER, "[FitsInput::process_header()] mismatch hdu type");
        return -1;
    }
    if (m_header_done) {
        //errmsg(BADOPER,"[FitsInput::process_header()] header already done");
        //return -1;
        read_header_rec();
        return 0;
    }
    uk.delete_all();
    uk = m_kw;
    int cnt = 0;
    int i;
    FitsKeyword *x, *y;
    uk.first();
    y = uk.next(); // set the list pointer
    for (;; m_kc.parse(m_curr, uk, cnt, m_errfn, True)) {
        // The worst error is if there is no END keyword.
        uk.last();
        x = uk.prev(); // do backwards search for END
        if (x->kw().name() == FITS::END)
            break;
        while (x != y) {
            x = uk.prev();
            if (x->kw().name() == FITS::END)
                break;
        }
        if (x->kw().name() == FITS::END)
            break;
        uk.last();
        y = uk.prev(); // reset the list pointer
        uk.last(); // return list iterator to last position
        m_curr = m_fin.read(); // read the next record
        if (!m_curr) {
            errmsg(BADEOF,
                    "[FitsInput::process_header()] Unexpected end of file");
            m_rec_type = FITS::EndOfFile;
            return -1;
        }
        if (m_fin.err()) {
            errmsg(IOERR, "[FitsInput::process_header()] Unrecognizable record");
            m_rec_type = FITS::UnrecognizableRecord;
            return -1;
        }
        ++cnt;
        // This attempts to deal with the problem of no END keyword
        // by searching for non-text data in the first 8 bytes.
        for (i = 0; i < 8; ++i)
            if (!FITS::isa_text(m_curr[i]))
                break;
        if (i < 8) {
            errmsg(MISSKEY,
                    "[FitsInput::process_header()] Missing END keyword.  Non-text data "
                        "found in name field.\n\tEnd of keywords assumed.");
            break;
        }
    }
    //cout << "[ FitsInput::process_header()] keyword list uk:\n" << uk << endl;
    if (!m_extend) {
        if (uk(FITS::EXTEND))
            if (uk.curr()->asBool() == True)
                m_extend = True;
    }
    HeaderDataUnit::HDUErrs n;
    Int nd;
    if (!HeaderDataUnit::compute_size(uk, m_data_size, nd, m_hdu_type,
            m_data_type, m_errfn, n)) {
        errmsg(BADSIZE,
                "[FitsInput::process_header()] Failed to compute size of data.");
        m_rec_type = FITS::UnrecognizableRecord;
        return -1;
    }
    //cout << "t=" << t << " m_curr_size=" << m_curr_size << " m_data_size=" << m_data_size << endl;
    //cout << "m_hdu_type=" << m_hdu_type << " m_header_done=" << m_header_done << endl;
    m_item_size = FITS::fitssize(m_data_type);
    m_curr_size = m_data_size;
    m_header_done = True;

    if (m_data_size > 0) {
        m_curr = m_fin.read();
        m_got_rec = True;
        if (!m_curr) {
            m_hdu_type = FITS::NotAHDU;
            m_item_size = 0;
            m_data_type = FITS::NOVALUE;
            m_data_size = 0;
            m_curr_size = 0;
            errmsg(BADEOF,
                    "[FitsInput::process_header()] Unexpected end of file.");
            m_rec_type = FITS::EndOfFile;
            return -1;
        }
        if (m_fin.err()) {
            m_hdu_type = FITS::NotAHDU;
            m_item_size = 0;
            m_data_type = FITS::NOVALUE;
            m_data_size = 0;
            m_curr_size = 0;
            errmsg(IOERR,
                    "[FitsInput::process_header()] Failed to read first data record.");
            m_rec_type = FITS::UnrecognizableRecord;
            return -1;
        }
    } else {
        //cout << "FitsInput::process_header - t=" << t << " hdu_type=" << m_hdu_type << endl;
        if (t != FITS::PrimaryTableHDU)
            read_header_rec();
    }
    return 0;
}
//===============================================================================================
// Implement the read_all() method with the cfitsio of NASA.
// Read the whole data unit of current HDU from the beginning to end ( the 
// condition m_curr_size = m_data_size guarantees this. The 
// read data is stored into a char* buffer -- addr.
//
// If addr is too big, it cannot be fitted into memory. So if this function is
// called, m_data_size actually cannot be bigger than the machine's memory size.
// we still change the return type to OFF_T though.
OFF_T FitsInput::read_all(FITS::HDUType t, char *addr) {
    if (m_curr_size < 0 || m_curr_size != m_data_size || m_rec_type
            != FITS::HDURecord || t != m_hdu_type || (!m_header_done)) {
        errmsg(BADOPER, "[FitsInput::read_all] Illegal operation on FITS input");
        return 0;
    }
    // get size of the current HDU
    OFF_T l_headstart, l_datastart, l_dataend;
    int l_status = 0;
    if (ffghof(m_fptr, &l_headstart, &l_datastart, &l_dataend, &l_status) > 0) {
        fits_report_error(stderr, l_status); // print error report
        errmsg(BADSIZE,
                "[FitsInput::read_all()] Failed to get the size of current hdu");
        return 0;
    }

    // determine how many byte of data is in the current hdu data unit. This is
    // probably redundant(actually this sometimes cause error) since m_data_size
    // is already determined when read header.
    //m_data_size = l_dataend - l_datastart; // this may not be needed.
    //
    // move file pointer to the beginning of the data unit of the current hsu
    l_status = 0;
    // The following may not be  needed with if the condition m_curr_size = m_data_size
    // is met.
    ffmbyt(m_fptr, l_datastart, REPORT_EOF, &l_status);
    if (l_status) {
        fits_report_error(stderr, l_status); // print error report
        return (0);
    }
    // using the cfitsio function to read m_data_size bytes from the file
    // pointed to by m_fptr from where the file position indicator currently at.
    l_status = 0;
    ffgbyt(m_fptr, m_data_size, addr, &l_status);
    if (l_status) {
        fits_report_error(stderr, l_status); // print error report
        return (0);
    }
    if (l_dataend < ((m_fptr->Fptr)->filesize)) {
        if (ffmbyt(m_fptr, l_dataend, REPORT_EOF, &l_status) > 0) {
            fits_report_error(stderr, l_status); // print error report
            errmsg(BADOPER, "[FitsInput::read_all()] bytepos setting error!");
            return (0);
        }
    } else {
        (m_fptr->Fptr)->bytepos = l_dataend;
    }
    m_curr_size = 0;
    // reset m_iosize so that next m_fin.read() will start from the beginning of next hdu.
    m_fin.reset_iosize();
    read_header_rec();
    return (m_data_size);
}
//=========================================================================================
// Implement read() method with cfitsio of NASA throgh BlockInput::read().
// read next nb bytes into addr within the same hdu. If nb > m_curr_size,
// make nb = m_curr_size.
int FitsInput::read(FITS::HDUType t, char *addr, int nb) {
    // read next nb bytes into addr
    if (m_rec_type != FITS::HDURecord || t != m_hdu_type || (!m_header_done)) {
        errmsg(BADOPER, "[FitsInput::read()] Illegal operation on FITS input");
        return 0;
    }
    if (m_curr_size == 0) {
        read_header_rec();
        return 0;
    }
    if (OFF_T(nb) > m_curr_size)
        nb = m_curr_size;
    int n = nb;
    if (m_bytepos == m_recsize) {
        m_curr = m_fin.read();
        if (!m_curr) {
            errmsg(BADEOF, "[FitsInput::read()] Unexpected end of file");
            m_rec_type = FITS::EndOfFile;
            return -1;
        }
        if (m_fin.err()) {
            errmsg(IOERR,
                    "[FitsInput::read()] Unrecognizable first data record.");
            m_rec_type = FITS::UnrecognizableRecord;
            return -1;
        }
        m_bytepos = 0;
    }
    do {
        if (n <= (m_recsize - m_bytepos)) {
            memcpy(addr, &m_curr[m_bytepos], n);
            m_bytepos += n;
            m_curr_size -= n;
            n = 0;
        } else {
            memcpy(addr, &m_curr[m_bytepos], (m_recsize - m_bytepos));
            m_curr_size -= m_recsize - m_bytepos;
            n -= m_recsize - m_bytepos;
            addr += m_recsize - m_bytepos;
            m_curr = m_fin.read();
            if (!m_curr) {
                errmsg(BADEOF, "[FitsInput::read()] Unexpected end of file");
                m_rec_type = FITS::EndOfFile;
                return -1;
            }
            if (m_fin.err()) {
                errmsg(IOERR, "[FitsInput::read()] Unrecognizable data record.");
                m_rec_type = FITS::UnrecognizableRecord;
                return -1;
            }
            m_bytepos = 0;
        }
    } while (n > 0);
    if (m_curr_size == 0) {
        read_header_rec();
    }
    //cout<<"[FitsInput::read()] byte read, nb = " << nb <<endl;
    return nb;
}
//==========================================================================================
// Implement the skip() method with cfitsio of NASA
// Skip the next nb bytes within the same HDU. If nb is greater than the data size left
// within the current HDU, skip to the end of the HDU. Return(with return statement) the 
// number of bytes that is actually skipped.
int FitsInput::skip(FITS::HDUType t, OFF_T nb) { // skip next nb bytes. Original comment
    if (m_rec_type != FITS::HDURecord || t != m_hdu_type || (!m_header_done)) {
        errmsg(BADOPER, "[FitsInput::skip()] Illegal operation on FITS input");
        return 0;
    }
    // determine how many byte of data left within the current hdu data unit. Keep in mind
    // that there may be still some data in the m_buffer( m_iosize-m_current)
    if (m_curr_size == 0) {
        read_header_rec();
        return 0;
    }
    if (nb > m_curr_size) {
        nb = m_curr_size;
    }
    OFF_T l_n = nb;
    // if m_bytepos = m_recsize, the current data record is used up. So read a new record.
    if (m_bytepos == m_recsize) {
        m_curr = m_fin.read();
        if (!m_curr) {
            errmsg(BADEOF, "[FitsInput::skip()] Unexpected end of file");
            m_rec_type = FITS::EndOfFile;
            return -1;
        }
        if (m_fin.err()) {
            errmsg(IOERR,
                    "[FitsInput::skip()] Failed to read first data record.");
            m_rec_type = FITS::UnrecognizableRecord;
            return -1;
        }
        m_bytepos = 0;
    }

    if (l_n <= OFF_T(m_fin.iosize() - m_fin.current())) {
        // In this case, there is enough data in m_buffer, so no need to get it from disk.
        do {
            // if l_n is smaller than (m_recsize-m_bytepos), simply skip the bytes l_n.
            // this "if block" also takes care of the ending part of the data to read.
            if (l_n <= OFF_T(m_recsize - m_bytepos)) {
                m_bytepos += l_n;
                m_curr_size -= l_n;
                l_n = 0;
            } else {
                m_curr_size -= m_recsize - m_bytepos;
                l_n -= m_recsize - m_bytepos;
                // since this is still within m_buffer, no need to check read errors.
                m_curr = m_fin.read();
                m_bytepos = 0;
            }
        } while (l_n > 0);
    } else {
        // Need to skip more bytes than what m_buffer has. We could still have let the control use above
        // block. But if there are lots of data to read, the following block will be more efficient.
        int l_bb = m_fin.iosize() - m_fin.current(); // bytes in m_buffer
        int l_status = 0;
        int l_res = ((m_fptr->Fptr)->bytepos + l_n - l_bb) % m_recsize;
        // move file position pointer to the end of last complete record to skip.
        OFF_T l_postogo = ((m_fptr->Fptr)->bytepos) + l_n - l_bb - l_res;

        if (l_postogo < ((m_fptr->Fptr)->filesize)) {
            if (ffmbyt(m_fptr, l_postogo, REPORT_EOF, &l_status) > 0) {
                fits_report_error(stderr, l_status); // print error report
                errmsg(BADOPER, "[FitsInput::skip()] bytepos setting error!");
                return -1;
            }
        } else {
            (m_fptr->Fptr)->bytepos = l_postogo;
            m_rec_type = FITS::EndOfFile;
            // do not need a return here. m_fin.read()
            // will handle it if reaches the end of file
        }

        m_fin.reset_iosize(); // this guarantees next m_fin.read() will read from disk.
        // the following lines read whatever is left by ffmbyt. Note that m_fin.read()
        // always read a complete record. That is why we only let ffmbyt() move the file
        // position pointer to the end of the last complete record to skip.
        m_curr = m_fin.read();
        if (!m_curr) {
            //Is this an error?
            errmsg(BADEOF, "[FitsInput::skip()] Reached the end of the file.");
            m_rec_type = FITS::EndOfFile;
            return -1;
        }
        if (m_fin.err()) {
            errmsg(IOERR,
                    "[FitsInput::skip()] Failed to read/skip data record.");
            m_rec_type = FITS::UnrecognizableRecord;
            return -1;
        }
        m_bytepos = l_res;
    }
    // set the current data size(remaining) within the data unit of the current hdu.
    m_curr_size -= l_n;
    if (m_curr_size == 0) {
        read_header_rec();
    }
    return nb;
}
//=====================================================================================
// Implement the skip_all() method with cfitsio of NASA
// Skip the remaining data within the current HDU, and then moving to the 
// beginning of the data unit of the next hdu ( by calling read_header_rec()); 
void FitsInput::skip_all(FITS::HDUType t) {
    if (m_rec_type != FITS::HDURecord || t != m_hdu_type || (!m_header_done)) {
        errmsg(BADOPER,
                "[FitsInput::skip_all()] Illegal operation on FITS input");
        return;
    }
    if (m_curr_size == 0) {
        read_header_rec();
        return;
    }

    /* get size of the current HDU */
    OFF_T l_headstart, l_datastart, l_dataend;
    int l_status = 0;
    if (ffghof(m_fptr, &l_headstart, &l_datastart, &l_dataend, &l_status) > 0) {
        fits_report_error(stderr, l_status); /* print error report */
        errmsg(BADSIZE,
                "[FitsInput::skip_all()] Failed to get the size of current hdu.");
        return;
    }
    // Determine how many byte of data left within the current hdu data unit.
    // Consider the case that the file pointer is still pointing at the header part!
    l_status = 0;
    if (l_dataend < ((m_fptr->Fptr)->filesize)) {
        if (ffmbyt(m_fptr, l_dataend, REPORT_EOF, &l_status) > 0) {
            fits_report_error(stderr, l_status); // print error report
            errmsg(BADOPER, "[FitsInput::skip_all()] bytepos setting error!");
            m_rec_type = FITS::UnrecognizableRecord;
            return;
        }
    } else {
        (m_fptr->Fptr)->bytepos = l_dataend;
        m_rec_type = FITS::EndOfFile;
        // do not need a return here. read_header_rec()
        // will handle it if reaches the end of file
    }

    m_curr_size = 0;
    m_bytepos = m_recsize;
    m_fin.reset_iosize(); // this guarantees next m_fin.read() will read from disk.
    read_header_rec();
    return;
}
//=========================================================================================
// Reset the fitsfile pointer
void FitsOutput::setfptr(fitsfile* ffp) {
    //int l_status = 0;
    //if(m_fout.close_file( m_fptr, &l_status)){
    //   errmsg(IOERR,"[BlockIO::setfptr()] Error closing file");
    //}
    // Cannot do above here. Since then cannot update BlockIO::m_fptr.
    // BlockIO.setfptr() will do above.
    m_fptr = ffp;
}
//=========================================================================================
int FitsOutput::write_hdr(FitsKeywordList &kwl, FITS::HDUType t,
        FITS::ValueType dt, OFF_T ds, Int is) {
    if ((m_rec_type == FITS::EndOfFile) || (m_rec_type == FITS::SpecialRecord)
            || m_header_done || t == FITS::NotAHDU) {
        errmsg(BADOPER, "Illegal operation -- cannot write FITS header.");
        return -1;
    }
    if (t == FITS::PrimaryArrayHDU || t == FITS::PrimaryGroupHDU || t
            == FITS::PrimaryTableHDU) {
        if (m_rec_type != FITS::InitialState) {
            errmsg(BADOPER,
                    "[FitsOutput::write_hdr()] Primary Header must be written first.");
            return -1;
        } else {
            m_isaprimary = True;
            if (kwl(FITS::SIMPLE)->asBool() == True) {
                m_valid_fits = True;
            }
            if (kwl(FITS::EXTEND)) {
                if (kwl.curr()->asBool() == True) {
                    m_extend = True;
                }
            }
        }
    } else if (m_rec_type != FITS::HDURecord) {
        errmsg(BADOPER,
                "[FitsOutput::write_hdr()] Catastrophic error!  Illegal record type.");
        //cout<<"[FitsOutput::write_hdr()] Illeagal record type."<<endl;
        m_rec_type = FITS::EndOfFile;
        return -1;
    } else {
        //cout<<"[FitsOutput::write_hdr()] Non-Primary Header."<<endl;
        if (!hdu_complete()) {
            errmsg(BADOPER,
                    "[FitsOutput::write_hdr()] Previous HDU incomplete -- cannot write header.");
            return -1;
        }
        if (!m_extend) {
            errmsg(BADOPER,
                    "[FitsOutput::write_hdr()] Cannot write extension HDU - EXTEND not True");
            return -1;
        } else {
            if (t == FITS::PrimaryArrayHDU || t == FITS::PrimaryGroupHDU || t
                    == FITS::PrimaryTableHDU) {
                errmsg(BADOPER,
                        "[FitsOutput::write_hdr()] Primary HDU already written.");
                return -1;
            }
        }
    }
    // this boolean variable indicates wheather the user is using only the write_***_hdr() methods, which write
    // only the required key words for the corresponding hdu and they cannot work with write_hdu(). So once
    // write_hdu() is called, we set it to be FALSE here.
    m_required_keys_only = FALSE;
    //-------------------
    // Create, initialize, and move the i/o pointer to a new extension appended to the end of the FITS file.
    /*
     Int l_status = 0;
     if(ffcrhd(m_fptr, &l_status)){
     errmsg(BADOPER,"[FitsOutput::write_hdr() Create new HDU failed!");
     fits_report_error(stderr, l_status); // print error report
     return -1;
     }
     */
    //----------------------
    m_rec_type = FITS::HDURecord;
    m_hdu_type = t;
    m_data_type = dt;
    m_data_size = ds;
    m_item_size = is;
    m_curr_size = 0;
    m_bytepos = 0;

    kwl.first();
    kwl.next();
    while (m_kc.build(m_curr, kwl)) {
        m_fout.write(m_curr);
    }

    m_fout.write(m_curr);
    m_err_status = OK;
    m_header_done = True;
    if (m_data_size == 0) {
        m_header_done = False;
    }

    return 0;
}

// FitsOutput::set_data_into() is used by PrimaryArray::write_priArr_hdr() etc.
void FitsOutput::set_data_info(FitsKeywordList &kwl, FITS::HDUType t,
        FITS::ValueType dt, OFF_T ds, Int is) {
    if (t == FITS::PrimaryArrayHDU || t == FITS::PrimaryGroupHDU || t
            == FITS::PrimaryTableHDU) {
        m_isaprimary = True;
        if (kwl(FITS::SIMPLE)->asBool() == True) {
            m_valid_fits = True;
        }
        if (kwl(FITS::EXTEND)) {
            if (kwl.curr()->asBool() == True) {
                m_extend = True;
            }
        }
    }

    m_rec_type = FITS::HDURecord;
    m_hdu_type = t;
    m_data_type = dt;
    m_data_size = ds;
    m_item_size = is;
    m_curr_size = 0;
    m_bytepos = 0;
    m_err_status = OK;
    m_header_done = True;
    if (m_data_size == 0) {
        m_header_done = False;
    }
}
// write all data from addr
int FitsOutput::write_all(FITS::HDUType t, char *addr, char pad) {
    if (!hdu_inprogress()) {
        errmsg(BADOPER, "Illegal operation -- no HDU in progress");
        return -1;
    }
    if (t != m_hdu_type) {
        errmsg(BADOPER, "Illegal operation -- incorrect HDU type");
        return -1;
    }
    // what if addr is used up first? GYL
    while ((m_data_size - m_curr_size) >= OFF_T(m_recsize)) {
        memcpy(m_curr, addr, m_recsize);
        m_fout.write(m_curr);
        addr += m_recsize;
        m_curr_size += m_recsize;
    }
    // note that  m_curr_size starts from 0. GYL
    m_bytepos = m_data_size - m_curr_size;
    if (m_bytepos) {
        memcpy(m_curr, addr, m_bytepos);
        // pad the last record. GYL
        while (m_bytepos < m_recsize) {
            m_curr[m_bytepos++] = pad;
        }
        m_fout.write(m_curr);
    }
    m_data_size = 0;
    m_curr_size = 0;
    m_err_status = OK;
    m_header_done = False;
    return 0;
}
// BlockOutput::write() is wraped to cfitsio already. So no need
// to directly wrap FitsOuput::write(). GYL
int FitsOutput::write(FITS::HDUType t, char *addr, Int bytes, char pad) {
    int n;
    if (!hdu_inprogress()) {
        errmsg(BADOPER,
                "[FitsOutput::write()] Illegal operation -- no HDU in progress");
        return -1;
    }

    if (t != m_hdu_type) {
        errmsg(BADOPER, "Illegal operation -- incorrect HDU type");
        return -1;
    }

    //cout<<"[FitsOutput::write()] m_hdu_type = "<< m_hdu_type << endl;
    //cout<<"[FitsOutput::write()] is t == m_hdu_type? t =  "<< t << endl;
    if ((bytes + m_curr_size) > m_data_size) {
        errmsg(BADOPER,
                "[FitsOutput::write] Attempt to write too much data -- truncated");
        //cout<<"[FitsOutput::write] Attempt to write too much data -- truncated" << endl;
        bytes = m_data_size - m_curr_size;
    }
    if (bytes <= (m_recsize - m_bytepos)) {
        memcpy(&m_curr[m_bytepos], addr, bytes);
        m_bytepos += bytes;
        m_curr_size += bytes;
    } else {
        n = m_recsize - m_bytepos;
        memcpy(&m_curr[m_bytepos], addr, n);
        m_curr_size += n;
        addr += n;
        bytes -= n;
        m_fout.write(m_curr); // write a record
        while (bytes >= m_recsize) {
            memcpy(m_curr, addr, m_recsize);
            m_fout.write(m_curr); // write a record
            addr += m_recsize;
            m_curr_size += m_recsize;
            bytes -= m_recsize;
        }
        m_bytepos = bytes;
        if (bytes) {
            memcpy(m_curr, addr, bytes);
            m_curr_size += bytes;
        }
    }
    // Fill up and write the last record as long as the data doesn't
    // evenly fill the last record.
    if (m_curr_size == m_data_size) {
        if (bytes) {
            while (m_bytepos < m_recsize) {
                m_curr[m_bytepos++] = pad;
            }
            m_fout.write(m_curr);
        }
        m_data_size = 0;
        m_curr_size = 0;
        m_header_done = False;
    }
    m_err_status = OK;
    //cout<<"[FitsOutput::write()] Ending."<< endl;
    return 0;
}

int FitsOutput::write_sp(char *rec) { // write a special record
    if (m_rec_type == FITS::EndOfFile) {
        errmsg(BADOPER, "Illegal operation -- EOF has been written");
        return -1;
    }
    if (hdu_inprogress()) {
        errmsg(BADOPER, "Illegal operation -- HDU in progress");
        return -1;
    }
    if (m_rec_type != FITS::SpecialRecord) {
        m_rec_type = FITS::SpecialRecord;
    }
    m_fout.write(rec);
    return 0;
}

FitsDiskInput::FitsDiskInput(const char *f, int l, int n,
        FITSErrorHandler errhandler) :
    BlockInput(f, l, n, errhandler) {
}

FitsDiskOutput::FitsDiskOutput(const char *f, int l, int n,
        FITSErrorHandler errhandler) :
    BlockOutput(f, l, n, errhandler) {
}

FitsStdInput::FitsStdInput(int l, FITSErrorHandler errhandler) :
    BlockInput(0, l, 1, errhandler) {
}

FitsStdOutput::FitsStdOutput(int l, FITSErrorHandler errhandler) :

    BlockOutput(1, l, 1, errhandler) {
}

FitsTape9Input::FitsTape9Input(const char *f, int l, int n,
        FITSErrorHandler errhandler) :
    BlockInput(f, l, n, errhandler) {
}

FitsTape9Output::FitsTape9Output(const char *f, int l, int n,
        FITSErrorHandler errhandler) :
    BlockOutput(f, l, n, errhandler) {
}

FitsIO::FitsIO(FITSErrorHandler errhandler) :
    m_recsize(2880), m_valid_fits(False), m_extend(False), m_isaprimary(False),
            m_header_done(False), m_rec_type(FITS::InitialState), m_hdu_type(
                    FITS::NotAHDU), m_errfn(errhandler), m_err_status(OK),
            m_curr(0), m_bytepos(0), m_item_size(0),
            m_data_type(FITS::NOVALUE), m_data_size(0), m_curr_size(0),
            m_skipHDU_size(0) {
}

FitsInput::FitsInput(const char *n, const FITS::FitsDevice &d, int b,
        FITSErrorHandler errhandler) :
    FitsIO(errhandler), m_fin(make_input(n, d, b, errhandler)),
            m_got_rec(False) {
    init();
}

FitsInput::FitsInput(FITSErrorHandler errhandler) :
    FitsIO(errhandler), m_fin(*(BlockInput *) (new FitsStdInput(m_recsize,
            errhandler))) {
    init();
}
} //# NAMESPACE CASACORE - END

