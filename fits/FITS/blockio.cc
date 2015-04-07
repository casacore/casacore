//# blockio.cc:
//# Copyright (C) 1993,1994,1995,1996,1999,2001,2002,2003
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
//
# include <casacore/casa/sstream.h>
# include <casacore/fits/FITS/blockio.h>
# include <casacore/casa/string.h>
#include <unistd.h>

namespace casacore { //# NAMESPACE CASACORE - BEGIN

//======================================================================================
    void BlockIO::errmsg(IOErrs e, const char *s) { 
	static char msgstring[180]; // storage for composing error messages
	ostringstream msgline;
	msgline << "BlockIO:  ";
	if (m_filename == 0 || *m_filename == '\0')
	    msgline << "File Descriptor " << m_fd;
	else
	    msgline << "File " << m_filename;
	msgline << " Physical record " << m_block_no
		<< " logical record " << m_rec_no << " --\n\t" << s << endl;
	m_err_status = e;
	// all BlockIO messages are SEVERE
	strncpy(msgstring, msgline.str().c_str(), sizeof(msgstring)-1);
	m_errfn(msgstring, FITSError::SEVERE);
    }
//========================================================================================
// wrap the constructor with cfitsio of NASA. GYL
    BlockIO::BlockIO(const char *f, int o, int r, int n, 
		     FITSErrorHandler errhandler) :
	m_filename(0), m_options(o), m_recsize(r), m_nrec(n), m_blocksize(r * n),
        m_errfn(errhandler), m_err_status(OK), m_fd(-1), m_buffer(0), m_block_no(0), 
        m_rec_no(0), m_current(0), m_iosize(0) {

	if (f == 0 || (*f == '\0')) {
	    errmsg(NOSUCHFILE,"No filename was specified");
	    return;
	}
	if ((m_filename = new char [strlen(f) + 1]) == 0) {
	    errmsg(NOMEM,"Cannot allocate memory");
	    return;
	}
	if ((m_buffer = new char [m_blocksize]) == 0) {
	    errmsg(NOMEM,"Cannot allocate memory");
	    delete [] m_filename;
	    m_filename = 0;
	    return;
	}
	strcpy(m_filename,f);
	// using cfitsio of NASA to open fits file for writting and reading.
	int l_status, iomode;
	l_status = 0; 
	if (m_options & O_CREAT){
	    if (fits_create_file(&m_fptr, m_filename, &l_status)){ /*create FITS file*/
		fits_report_error(stderr, l_status); /* print error report */
		errmsg(OPENERR,"File exists already!"); 
		delete [] m_filename;
		delete [] m_buffer;
		m_filename = 0;
		m_buffer = 0;
	    }else{
		if( ((m_fptr->Fptr)->io_pos) != 0 ){
		    (m_fptr->Fptr)->io_pos = 0;
		}
	    }
 	}else{
	    iomode = READONLY;
	    // using cfitsio function to open a file for reading.
	    if ( fits_open_file(&m_fptr, m_filename, iomode, &l_status) ){
		fits_report_error(stderr, l_status); /* print error report */
		errmsg(OPENERR,"Open file error!");
		delete [] m_filename;
		delete [] m_buffer;
		m_filename = 0;
		m_buffer = 0;
	    }else{
		// fits_open_file() puts the bytepos at the beginning of the data unit, so move it back to beginning of HDU.
		if( m_fptr == 0 ) cout << "[BlockIO::BlockIO()] m_fptr is null, open file failed." << endl;
		// cout<<"[BlockIO::BlockIO()] filesize = "<< (m_fptr->Fptr)->filesize << endl;
		if( ((m_fptr->Fptr)->bytepos) != 0 ){
		    if(ffmbyt(m_fptr, 0, REPORT_EOF, &l_status)!=0 ){ errmsg(OPENERR,"bytepos setting error!"); }
		}
	    }
	}
	
    }
//======================================================================================
// To see what to do with this constructor. we need a m_fptr for io operation.
// Can we get the file name from the file descriptor fd? No. However, this constructor
// is only used for standard io. So we do not have to worry about it.
    BlockIO::BlockIO(int f, int r, int n, FITSErrorHandler errhandler) :
	m_filename(0), m_options(0), m_recsize(r), m_nrec(n), m_blocksize(n * r), 
	m_errfn(errhandler), m_err_status(OK), m_fd(f), m_block_no(0), m_rec_no(0), 
	m_current(0), m_iosize(0) {
	if ((m_buffer = new char [m_blocksize]) == 0) {
	    errmsg(NOMEM,"Cannot allocate memory");
	    return;
	}
    }
//======================================================================================
// close fits file with cfitsio function
    BlockIO::~BlockIO() {
	if (m_filename != 0 && strlen(m_filename) > 0) {
	    // For writing node, fits_close_file() damages the output file!
	    int l_status = 0;
	    if( m_options == O_RDONLY ){		  
		if( fits_close_file(m_fptr, &l_status)>0 ){
		    errmsg(CLOSEERR,"[~BlockIO()] Error closing file.");
		}
	    }else{ // end of if( m_option ...).	
		   // fits_close_file() does not work in this case. So use our own close_file().	
		if(close_file( m_fptr, &l_status)){
		    errmsg(CLOSEERR,"[~BlockIO()] Error closing file");
		}
	    }
	    delete [] m_filename;
	}// end of if( m_filename ! =0, ...).
	delete [] m_buffer;
    }
//========================================================================================
// Reset the fitsfile pointer
    void BlockIO::setfptr( fitsfile* ffp ){
	int l_status = 0;
	if(close_file( m_fptr, &l_status)){
	    errmsg(CLOSEERR,"[BlockIO::setfptr()] Error closing file");
	}
	m_fptr = ffp;
    }
//========================================================================================
// Close the FITS file by calling the system dependent routine to physically 
// close the FITS file and free the memory.  
    int BlockIO::close_file( fitsfile *fptr, int *status){

	if (!fptr)
	    return(*status = NULL_INPUT_PTR);
	else if ((fptr->Fptr)->validcode != VALIDSTRUC) // check for magic value 
	    return(*status = BAD_FILEPTR);       

	((fptr->Fptr)->open_count)--;                   // decrement usage counter

	if ((fptr->Fptr)->open_count == 0)              // if no other files use structure
	{
	    if( ffflsh(fptr, TRUE, status) ){
		errmsg(CLOSEERR,"[BlockIO::close_file()] Failed to flush the file: (ffflsh)" );
		cout<< (fptr->Fptr)->filename << endl;
	    }

	    if( file_close( (fptr->Fptr)->filehandle) ){ 
		*status = FILE_NOT_CLOSED;       // report if no previous error
		errmsg(CLOSEERR,"[BlockIO::close_file()] Failed to close the file: (ffclos)" );
		cout<< (fptr->Fptr)->filename << endl;
	    }

	    fits_clear_Fptr( fptr->Fptr, status);  // clear Fptr address 
            // iobuffer was added with version 3.181...
            // cfitsio 3.03-3.14 do not have this...
            // However, something like CFITSIO_VERSION 3.03 is greek to CPP.
            // So assume that by 1-Apr-2015 all sites use a sufficiently new cfitsio.
            free((fptr->Fptr)->iobuffer);          // free memory for I/O buffers
	    free((fptr->Fptr)->headstart);         // free memory for headstart array 
	    free((fptr->Fptr)->filename);          // free memory for the filename
	    (fptr->Fptr)->filename = 0;
	    (fptr->Fptr)->validcode = 0;           // magic value to indicate invalid fptr
	    free(fptr->Fptr);                      // free memory for the FITS file structure 
	    free(fptr);                            // free memory for the FITS file structure 
	}else{
	    // just flush the buffers, don't disassociate them
	    int zerostatus = 0;
	    if (*status > 0)
		ffflsh(fptr, FALSE, &zerostatus); 
	    else
		ffflsh(fptr, FALSE, status); 
	    free(fptr);                            // free memory for the FITS file structure 
	}
	return(*status);
    }
//==========================================================================================
    BlockInput::~BlockInput() {
    }
//==========================================================================================
// wrap the read() method with cfitsio of NASA. GYL
    char *BlockInput::read() {
	m_current += m_recsize;
	if (m_current >= m_iosize) {
	    int l_ntoread = m_blocksize;
	    int l_status = 0;
	    m_iosize = 0;
	    // check that we do not exceed the end of the fits file
	    OFF_T l_byte_left_in_file  = (m_fptr->Fptr)->filesize - (m_fptr->Fptr)->bytepos;
	    // if reached the end of file, return to caller with a NULL pointer.
	    if( l_byte_left_in_file == 0 ){ 
		//cout << "No more data in file, return a NULL pointer." << endl;
		return(NULL);
	    }
	
	    if( OFF_T(m_blocksize) > l_byte_left_in_file ){
		l_ntoread = m_recsize; // switch down to reading single records instead of blocks
		if( OFF_T(m_recsize) > l_byte_left_in_file ){
		    // ignore remainder if not multiple of record size
		    cout << "WARNING: fits blockio ignoring last " << l_byte_left_in_file << " bytes." << endl;
		    return(NULL);
		}		
	    }
	    // using the cfitsio function to read m_blocksize bytes from the file
	    // pointed to by m_fptr from where the file position indicator currently at.
	    OFF_T bytepost = (m_fptr->Fptr)->bytepos; 
// 	    cout << "m_iosize, m_blocksize,  m_recsize, m_iosize % m_recsize, bytepost, l_ntoread "
// 		 << m_iosize << " " << m_blocksize << " " << m_recsize << " " << m_iosize % m_recsize 
// 		 << " " << bytepost << " " << l_ntoread << endl; 

	    ffgbyt( m_fptr,       // I - FITS file pointer             
		    l_ntoread,    // I - number of bytes to read       
		    m_buffer,     // O - buffer to read into           
		    &l_status);    // IO - error status               
	    if( l_status ){   
		fits_report_error(stderr, l_status); /* print error report */
		return(0);
	    }
	
	    // try to move on to the next record
	    bytepost = bytepost + OFF_T(l_ntoread);
	    l_byte_left_in_file  = (m_fptr->Fptr)->filesize - bytepost;
	    if( l_byte_left_in_file  >= OFF_T(m_recsize) ){
		if(ffmbyt(m_fptr, bytepost, REPORT_EOF, &l_status)>0 ){ errmsg(READERR,"bytepos setting error!"); }
	    }else{
		(m_fptr->Fptr)->bytepos = bytepost;
	    }
	    m_block_no++;
	    m_iosize = l_ntoread; // always a multiple of the record size
	    m_err_status = OK;

	    m_current = 0;
	}
	m_rec_no++;
	return &m_buffer[m_current];
    }
//================================================================================
// skip the next n logical record and then read the next one
    char *BlockInput::skip(int n) {
	while (n--){
	    read();
	}
	return read();
    }
//=====================================================================================================
// flush both io buffer of CFITSIO and m_buffer
    void BlockOutput::flush_buffer(){
	int l_status = 0;
	// flush the io buffer of CFITSIO.
	// After each call to ffpbyt(), one can call ffflsh() only once. Since we did it after each callto
	// ffpbyt() already, here we cannot call ffflsh() again!
	
	//if(ffflsh(m_fptr, TRUE, &l_status)){ errmsg(WRITEERR,"[flush_buffer()] Error flushing buffer!"); }
	
	// flush the m_buffer
	if (m_current > 0) {	   
	    // see write() for how ffpbyt() works.
	
	    //ffwrite( m_fptr->Fptr, long_current, m_buffer, &l_status); 						
	    if ( ffpbyt( m_fptr, m_current, m_buffer, &l_status) ){
		fits_report_error(stderr, l_status); // print error report
		errmsg(WRITEERR,"[BlockOutput::flush_buffer()] Error writing record");
	    }else{
		//m_iosize = m_current;
		m_err_status = OK;			
	    }
	    // ffpbyt() doesn't write the last record onto disk,instead it puts the last
	    // record into a buffer. So we call ffflsh() to flush everything onto disk. Otherwise
	    // something would go wrong!
	    if(ffflsh(m_fptr, TRUE, &l_status)){ errmsg(WRITEERR,"[flush_buffer()] Error flushing buffer!"); }
	    //if(ffflus(m_fptr, &l_status)){ errmsg(WRITEERR,"[flush_buffer()] Error flushing buffer!"); }
		
	    m_block_no++;
	    m_current = 0;		
	}
    }
//====================================================================================================
    BlockOutput::~BlockOutput() {
	// flush the IO buffer of CFITSIO and m_buffer before destruction
	flush_buffer();
    }
//====================================================================================================
// attach a logical record to the fits file. addr must point to a logical record.
    int BlockOutput::write(char *addr) {
	memcpy(&m_buffer[m_current],addr,m_recsize);
	m_rec_no++;
	m_current += m_recsize;
	// if buffer is full, write it to disk
	if (m_current >= m_blocksize) {	
	    /*
	      put (write) the buffer of bytes to the output FITS file, starting at
	      the current file position.  Write large blocks of data directly to disk;
	      write smaller segments to intermediate IO buffers to improve efficiency.
	    */
	    int l_status = 0;
	    ffpbyt( m_fptr,       /* I - FITS file pointer                    */
		    m_blocksize,  /* I - number of bytes to write             */
		    m_buffer,     /* I - buffer containing the bytes to write */
		    &l_status);   /* IO - error status                        */

	    m_block_no++;
	    if ( l_status  ){
		errmsg(WRITEERR,"[BlockOutput::write()] Error writing record");
	    }else{
		m_iosize = m_blocksize;
		m_err_status = OK;
	    }
		
	    m_current = m_current - m_blocksize; // it should be zero.
	}	
	return (int)m_err_status;
    }
//========================================================================================================
    BlockInput::BlockInput(const char *f, int r, int n, 
			   FITSErrorHandler errhandler) :
	BlockIO(f,O_RDONLY,r,n,errhandler) { }

    BlockInput::BlockInput(int f, int r, int n,
			   FITSErrorHandler errhandler) :
	BlockIO(f,r,n,errhandler) { }

    BlockOutput::BlockOutput(const char *f, int r, int n, 
			     FITSErrorHandler errhandler) :
	BlockIO(f,O_RDWR|O_CREAT|O_TRUNC,r,n,errhandler) { }
// replaced O_WRONLY by O_RDWR in above line.

    BlockOutput::BlockOutput(int f, int r, int n, 
			     FITSErrorHandler errhandler) :
	BlockIO(f,r,n,errhandler) { }

} //# NAMESPACE CASACORE - END

