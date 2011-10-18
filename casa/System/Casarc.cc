//# Casarc.h: Class to read the casa general resource files
//# Copyright (C) 2009
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

#include <casa/System/Casarc.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <time.h>
#include <sys/time.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <cstdio>

#define USE_FLOCK 0
#define CASARC_DEBUG 0

using std::make_pair;
using std::string;
using std::list;
using std::map;


namespace casa {

    bool Casarc::initialized = false;

    map<ino_t,Casarc*> *Casarc::rcfiles = 0;
    map<string,Casarc*> *Casarc::filenames = 0;
    std::list<Casarc*> *Casarc::rclist = 0;

    std::string *Casarc::default_path = 0;

    unsigned int CasarcCleanup::creation_count = 0;

    static off_t find_eol( const char *in, off_t len, off_t offset ) {
	off_t eoloff = offset;
	while ( in[eoloff] != '\n' && in[eoloff] != '\0' && eoloff < len ) ++eoloff;
	return eoloff;
    }

    static void copyline( char *out, int outlen, const char *in, int inlen, off_t offset=0 ) {
	int i = 0;
	for ( ; i < (outlen-1) && (i+offset) < inlen; ++i ) {
	    if ( in[offset+i] != '\n' && in[offset+i] != '\0' )
		out[i] = in[offset+i];
	    else
		break;
	}
	out[i] = '\0';
    }

    void Casarc::setDefaultPath( const std::string &path ) {
	default_path = new std::string(path);
    }

    void Casarc::clearDefaultPath( ) {
	delete default_path;
	default_path = 0;
    }

    Casarc &Casarc::instance( ) {

	if ( initialized == false ) { startup( ); }

	if ( default_path ) {
	    return instance( *default_path );
	} else {
	    const char *home = getenv("HOME");
	    if ( home == 0 ) return instance( "casarc" );
	    struct stat statbuf;
	    char buf[2048];
	    sprintf( buf, "%s/.casa", home );
	    if ( stat( buf, &statbuf ) == 0 ) {
		if ( S_ISDIR(statbuf.st_mode) ) {
		    return instance( std::string(buf) + "/rc" );
		}
	    }
	    return instance( std::string(home) + "/.casarc" );
	}
    }

    Casarc &Casarc::instance( const std::string &path ) {

	if ( initialized == false ) { startup( ); }

	map<string,Casarc*>::iterator f_iter = filenames->find(path);
	if ( f_iter == filenames->end( ) ) {
	    struct stat buf;
	    if ( stat( path.c_str( ), &buf ) >= 0 ) {
		if ( ! S_ISREG(buf.st_mode)) {
		    throw( "Casarc::instance, parameter is not a regular file: " + path );
		}
		map<ino_t,Casarc*>::iterator r_iter = rcfiles->find(buf.st_ino);
		if ( r_iter != rcfiles->end( ) ) {
		    // found by inode, but not by filename (two paths to the same file)
		    filenames->insert(make_pair(path,r_iter->second));
		    return *(r_iter->second);
		} else {
		    Casarc *ret = new Casarc(path);
		    return *ret;
		}
	    } else {
		Casarc *ret = new Casarc(path);
		return *ret;
	    }
	} else {
	    return *(f_iter->second);
	}
    }

    void Casarc::put( const std::string &keyword, const std::string &value ) {

#if CASARC_DEBUG >= 2
	fprintf( stderr, "<<<<<<<<<<<<<<<<<<<<<enter: put(pid:%d)>>>>>>>>>>>>>>>>>>>>>\n", getpid() );
#endif
        int the_lock = lock( READ );
	sync( );

	struct timeval tv = { 0, 0 };
	gettimeofday( &tv, 0 );

	std::map<std::string,std::string>::iterator mapping = rcmap.find(keyword);
	char buf[512];
	if ( mapping == rcmap.end( ) ) {
	    rcmap.insert(make_pair(keyword,value));
	    int fd = lock( APPEND );
	    if ( mapped_file[mapped_file_size-1] != '\n' ) {
		strftime( buf, 512, "\n# added %F %T\n", localtime(&tv.tv_sec) );
	    } else {
		strftime( buf, 512, "# added %F %T\n", localtime(&tv.tv_sec) );
	    }
	    write(fd, buf, strlen(buf) );
	    write(fd, keyword.c_str( ), keyword.length( ));
	    write(fd, ": ", 2);
	    write(fd, value.c_str( ), value.length( ));
	    write(fd, "\n", 1);
	    unlock( fd );
	} else {
	    mapping->second = value;
	    std::map<std::string,meta_entry_>::iterator meta = rcmetamap.find(keyword);
	    if ( meta == rcmetamap.end( ) )
		throw( "Casarc::put, internal inconsistency between data & meta" );

	    strftime( buf, 512, "# modified %F %T\n", localtime(&tv.tv_sec) );

	    off_t start = (meta->second.time_length( ) ? meta->second.time_offset( ) : meta->second.key_offset( ));
	    off_t end = meta->second.value_offset( ) + meta->second.value_length( );

#if CASARC_DEBUG >= 2
	    { fprintf( stderr, " >>old-offset>>>> start: %d, end: %d\n", start, end );
	      char *oldv = (char*) calloc( end - start + 10, sizeof(char) );
	      memcpy( oldv, &mapped_file[start], end-start );
	      fprintf( stderr, " >>old-value>>>>>%s<< [%d]\n", oldv, strlen(oldv) );
	      free( oldv );
	    }

	    { char *outputbuf = (char*) malloc( (start + 1) * sizeof(char) );
	      memcpy(outputbuf, mapped_file, start);
	      outputbuf[start] = '\0';
	      fprintf( stderr, " >>prefix>>>>>>>>%s<< [%d]\n",outputbuf, strlen(outputbuf) );
	      free( outputbuf );
	    }
#endif

	    int buflen = strlen(buf);
	    char *copy = (char*) malloc(sizeof(char) * ( start +
							 (buflen + keyword.length() + value.length() + 5) +
							 (mapped_file_size - end) ));

	    off_t off = 0;
	    memcpy( copy, mapped_file, start );
	    off += start;
	    sprintf( &copy[off], "%s%s: %s", buf, keyword.c_str(), value.c_str() );

#if CASARC_DEBUG >= 2
	    fprintf( stderr, " >>update>>>>>>>>%s<< [%d]\n", &copy[off], strlen(&copy[off]) );
#endif

	    off += strlen(&copy[off]);

#if CASARC_DEBUG >= 2
	    int sufstart = off;
#endif
	    memcpy( &copy[off], &mapped_file[end], mapped_file_size-end );
	    off += mapped_file_size-end;
	    if ( copy[off-1] != '\n' ) {
		copy[off++] = '\n';
	    }
	    copy[off] = '\0';

#if CASARC_DEBUG >= 2
	    fprintf( stderr, " >>suffix>>>>>>>>%s<< [%d]\n", &mapped_file[end], strlen(&mapped_file[end]) );
	    fprintf( stderr, " >>updated-file>>%s<< [%d]\n", copy, strlen(copy) );
#endif


	    munmap( mapped_file, mapped_file_size );
	    mapped_file = 0;
	    mapped_file_size = 0;

	    int fd = lock( WRITE );
	    write( fd, copy, off );
	    free( copy );
	    unlock( fd );
	}

	unlock( the_lock );
#if CASARC_DEBUG >= 2
	fprintf( stderr, "<<<<<<<<<<<<<<<<<<<<<exit: put(pid:%d)>>>>>>>>>>>>>>>>>>>>>\n", getpid() );
#endif
    }

    const std::list<Casarc*> &Casarc::list( ) {
        if ( rclist == 0 ) {
            rclist = new std::list<Casarc*>( );
        }
        return *rclist;
    }

    bool Casarc::get( const std::string &keyword, std::string &value ) {
	sync( );
	std::map<std::string,std::string>::iterator iter = rcmap.find(keyword);
	if ( iter == rcmap.end( ) )
	    return false;
	value = iter->second;
	return true;
    }

    std::string Casarc::get( const std::string &keyword ) {
	sync( );
	std::map<std::string,std::string>::iterator iter = rcmap.find(keyword);
	if ( iter == rcmap.end( ) )
	    return std::string("");
	return iter->second;
    }

    size_t Casarc::size( ) const {
	return rcmap.size( );
    }

    void Casarc::sync( ) {
	struct stat buf;
	if ( stat( filename.c_str( ), &buf ) >= 0 ) {
	    if ( ! S_ISREG(buf.st_mode)) {
		throw( "Casarc::sync, not a regular file: " + filename );
	    }
	}

	if ( mapped_file == 0 ||
	     mapped_file_size != buf.st_size ||
	     current_modification_time(buf) != timestamp ) {
#if CASARC_DEBUG >= 1
	    fprintf( stderr, "casarc update: %ld => ", size() );
	    read_file( );
	    fprintf( stderr, "%ld (pid:%d)\n", size(), getpid() );
#else
	    read_file( );
#endif
	}
    }

    void Casarc::close( int fd ) {
#if USE_FLOCK
	::close( fd );
#else
	if ( have_lock.size( ) == 0 ) {
	    ::close( fd );
	    for ( std::list<int>::iterator iter = stale_fds.begin(); iter != stale_fds.end(); ++iter ) {
		::close( *iter );
	    }
	    stale_fds.clear( );
	} else {
	    stale_fds.push_back(fd);
	}
#endif
    }

    int Casarc::lock( lock_mode mode ) {

#if USE_FLOCK
	int flags = ( mode == READ ? O_RDONLY :
#else
	int flags = ( mode == READ ? O_RDWR :
#endif
		      mode == READ_WRITE ? O_RDWR :
		      mode == WRITE ? O_WRONLY | O_TRUNC :
		      mode == APPEND ? O_APPEND | O_WRONLY : O_RDONLY );

	int fd = open( filename.c_str( ), flags );


	if ( fd < 0 ) {
	    throw( "Casarc::lock: could not open " + filename );
	}

	// (a) if process A acquires the lock and then forks() the child
	// should realize that it does not have the file locked...
	// (b) the same process can lock the file more than once...
	pid_t pid = getpid( );
	if ( have_lock.size( ) > 0 && have_lock.front( ) == pid ) {
	    have_lock.push_front(pid);
	    return fd;
	}
	if ( have_lock.size( ) > 0 ) { have_lock.clear( ); }

#if USE_FLOCK
	if ( flock( fd, LOCK_EX ) < 0 ) { throw( "Casarc::lock, failed to lock: " + filename ); }
#else
	struct flock lock;

	lock.l_type = F_WRLCK;		/** F_RDLCK, F_WRLCK, F_UNLCK         **/
	lock.l_start = 0;		/** byte offset, relative to l_whence **/
        lock.l_whence = SEEK_SET;	/** SEEK_SET, SEEK_CUR, SEEK_END      **/
	lock.l_len = 0;			/** bytes (0 means to EOF)            **/

	if ( fcntl( fd, F_SETLKW, &lock ) < 0 ) { perror("what tha...." ); throw( "Casarc::lock, failed to lock: " + filename ); }
#endif

	have_lock.push_front(pid);
	return fd;
    }

    void Casarc::unlock( int fd ) {
 	if ( have_lock.size( )  > 0 && have_lock.front( ) != getpid( ) ) {
	    have_lock.clear( );
	    return;
	}
	if ( have_lock.size( ) == 0 ) return;

	have_lock.pop_front( );
	if ( have_lock.size( ) == 0 ) {

#if USE_FLOCK
	    if ( flock( fd, LOCK_UN ) < 0 ) { throw( "Casarc::unlock, failed to unlock: " + filename ); }
#else
	    struct flock lock;

	    lock.l_type = F_UNLCK;	/** F_RDLCK, F_WRLCK, F_UNLCK         **/
	    lock.l_start = 0;		/** byte offset, relative to l_whence **/
	    lock.l_whence = SEEK_SET;	/** SEEK_SET, SEEK_CUR, SEEK_END      **/
	    lock.l_len = 0;		/** bytes (0 means to EOF)            **/

	    if ( fcntl( fd, F_SETLK, &lock ) < 0 ) { throw( "Casarc::unlock, failed to lock: " + filename ); }
#endif

	}
	close(fd);
    }

    Casarc::iterator Casarc::begin( ) {
        sync( );
	return rcmap.begin( );
    }
    Casarc::iterator Casarc::end( ) {
        sync( );
        return rcmap.end( );
    }

    Casarc::Casarc( const std::string &path ) : mapped_file(0), mapped_file_size(0),
                                                have_lock(0), filename(path), inode(0) {
	struct stat buf;

	if ( initialized == false ) { startup( ); }

	if ( stat( path.c_str( ), &buf ) < 0 ) {
	    int fd = open( path.c_str( ), O_CREAT | O_WRONLY,
			   S_IRUSR|S_IWUSR|S_IRGRP|S_IROTH );
	    if ( fd < 0 ) {
		throw( "Casarc, could not create: " + path );
	    } else {
		::close(fd);
		if ( stat( path.c_str( ), &buf ) < 0 ) {
		    throw( "Casarc, could not stat: " + path );
		}
	    }
	}

	if ( ! S_ISREG(buf.st_mode)) {
	    throw( "Casarc, parameter is not a regular file: " + path );
	}

	inode = buf.st_ino;
	read_file( );

	rcfiles->insert(make_pair(inode,this));
	filenames->insert(make_pair(path,this));
	rclist->push_back(this);

#if CASARC_DEBUG >= 1
	fprintf( stderr, "casarc start:  %ld rc files managed [", rclist->size() );
	for ( std::list<Casarc*>::iterator iter = rclist->begin( ); iter != rclist->end( ); ++iter ) {
	    fprintf( stderr, iter == rclist->begin() ? "%ld" : ",%ld", (*iter)->size() );
	}
	fprintf( stderr, "] (pid:%d)\n", getpid() );
#endif
    }

    double Casarc::current_modification_time( struct stat &buf ) {
	if ( ! S_ISREG(buf.st_mode)) {
	    throw( "Casarc::current_modification_time: " + filename + " is not a regular file" );
	}
#if defined(__APPLE__)
	// micro seconds: 1000000
	// nano seconds:  1000000000
	//   ...unfortunately, the resolution for modification time seems to be seconds...
	return (double) buf.st_mtimespec.tv_sec + (double) buf.st_mtimespec.tv_nsec / (double) 1000000000;
#else
	//   ...non-apple platforms seem upfront about the 1 sec resolution...
	return (double) buf.st_mtime;
#endif
    }

    #define is_added_tag(PTR,OFF)															\
       (PTR[OFF+0] == '#' && PTR[OFF+1] == ' ' &&                        \
	PTR[OFF+2] == 'a' && PTR[OFF+3] == 'd' && PTR[OFF+4] == 'd' && PTR[OFF+5] == 'e' && PTR[OFF+6] == 'd' && PTR[OFF+7] == ' ' &&			\
	isdigit(PTR[OFF+8]) && isdigit(PTR[OFF+9]) && isdigit(PTR[OFF+10]) && isdigit(PTR[OFF+11]) && PTR[OFF+12] == '-' &&				\
	isdigit(PTR[OFF+13]) && isdigit(PTR[OFF+14]) && PTR[OFF+15] == '-' && isdigit(PTR[OFF+16]) && isdigit(PTR[OFF+17]) && PTR[OFF+18] == ' ' &&	\
	isdigit(PTR[OFF+19]) && isdigit(PTR[OFF+20]) && PTR[OFF+21] == ':' && isdigit(PTR[OFF+22]) && isdigit(PTR[OFF+23]) && PTR[OFF+24] == ':' &&	\
	isdigit(PTR[OFF+25]) && isdigit(PTR[OFF+26]))

    #define is_modified_tag(PTR,OFF)															\
       (PTR[OFF+0] == '#' && PTR[OFF+1] == ' ' &&                        \
	PTR[OFF+2] == 'm' && PTR[OFF+3] == 'o' && PTR[OFF+4] == 'd' && PTR[OFF+5] == 'i' && PTR[OFF+6] == 'f' && PTR[OFF+7] == 'i' && PTR[OFF+8] == 'e' && \
	PTR[OFF+9] == 'd' && PTR[OFF+10] == ' ' &&													\
	isdigit(PTR[OFF+11]) && isdigit(PTR[OFF+12]) && isdigit(PTR[OFF+13]) && isdigit(PTR[OFF+14]) && PTR[OFF+15] == '-' &&				\
	isdigit(PTR[OFF+16]) && isdigit(PTR[OFF+17]) && PTR[OFF+18] == '-' && isdigit(PTR[OFF+19]) && isdigit(PTR[OFF+20]) && PTR[OFF+21] == ' ' &&	\
	isdigit(PTR[OFF+22]) && isdigit(PTR[OFF+23]) && PTR[OFF+24] == ':' && isdigit(PTR[OFF+25]) && isdigit(PTR[OFF+26]) && PTR[OFF+27] == ':' &&	\
	isdigit(PTR[OFF+28]) && isdigit(PTR[OFF+29]))

    void Casarc::read_file( ) {

	int fd = lock( READ );

	//
	// stat( ) file to make sure that it is there... and is a regular file...
	//
	struct stat buf;
	if ( fstat( fd, &buf ) < 0 ) {
	    throw( "Casarc::read_file: could not stat " + filename );
	} else if ( ! S_ISREG(buf.st_mode)) {
	    throw( "Casarc::read_file, parameter is not a regular file: " + filename );
	} else if ( buf.st_size > (1024 * 1024 * 2) ) {
	    throw( "Casarc::read_file, maximum file size (2M) exceeded" );
	}

	//
	// with osx one cannot memory map a zero length file
	// --   Wed Nov 17 21:32:45 UTC 2010
	//
	if ( buf.st_size == 0 ) {
	    write( fd, "\n", 1 );
	    lseek( fd, 0, SEEK_SET );
	    if ( fstat( fd, &buf ) < 0 )
		throw( "Casarc::read_file, internal error" );
	}

	timestamp = current_modification_time( buf );

	rcmap.erase(rcmap.begin( ),rcmap.end( ));
	rcmetamap.erase(rcmetamap.begin( ),rcmetamap.end( ));

	if ( mapped_file != 0 )
	    munmap( mapped_file, mapped_file_size );

	mapped_file_size = buf.st_size;
	if ( (mapped_file = (char*) mmap( 0, mapped_file_size, PROT_READ, MAP_SHARED, fd, 0 )) == (void*) -1 ) {
	    perror( "casarc" );
	    throw( "Casarc::read_file, could not memory map casarc file" );
	}

	int line_count = 0;

	char *keyword, *value;
	int keyword_len = 64;
	int value_len = 64;
	value = (char*) malloc(sizeof(char) * value_len);
	keyword = (char*) malloc(sizeof(char) * keyword_len);

	off_t timestp = 0;
	off_t timestplen = 0;

	for ( off_t off = 0; off < mapped_file_size; ++off ) {
	    if ( isspace(mapped_file[off]) ) {
		if ( mapped_file[off] == '\n' ) ++line_count;
		continue;
	    }
	    if ( mapped_file[off] == '#' ) {
		if ( is_added_tag(mapped_file,off) || is_modified_tag(mapped_file,off) ) {
		    timestp = off;
		    timestplen = find_eol( mapped_file, mapped_file_size, timestp ) - timestp;
		} else {
		    timestp = 0;
		    timestplen = 0;
		}
		off = find_eol(mapped_file, mapped_file_size, off);
		continue;
	    }

	    off_t keystart = off;
	    int keylen = 0;
	    for ( ; ! isspace(mapped_file[keystart+keylen]) && mapped_file[keystart+keylen] != ':' && (keystart+keylen) < mapped_file_size; ++keylen );

	    if ( keylen == 0 || mapped_file[keystart+keylen] == '\n' ) {
		char obuf[21];
		copyline( obuf, 21, mapped_file, mapped_file_size, off );
		fprintf( stderr, "casarc error:  ignoring malformed line %u: %s [file:%s] (pid:%d)\n", line_count, obuf, path().c_str( ), getpid() );
		off = find_eol(mapped_file, mapped_file_size, off);
		continue;
	    }

	    if ( keyword_len < (keylen + 1) ) {
		while ( keyword_len < (keylen + 1) ) keyword_len *= 2;
		keyword = (char *) realloc( keyword, keyword_len );
	    }
	    memcpy(keyword, &mapped_file[keystart], keylen);
	    keyword[keylen] = '\0';

	    off_t valstart = off + keylen;
	    int vallen = 0;
	    while ( isspace(mapped_file[valstart]) && mapped_file[valstart] != '\n' && valstart < mapped_file_size ) ++valstart;
	    if ( mapped_file[valstart] != ':' ) {
		char obuf[21];
		copyline( obuf, 21, mapped_file, mapped_file_size, off );
		fprintf( stderr, "casarc error:  ignoring malformed line %u: %s [file:%s] (pid:%d) \n", line_count, obuf, path().c_str( ), getpid() );
		off = find_eol(mapped_file, mapped_file_size, off);
		continue;
		// raise exception, return an error?
		// ...no, we have to carry on...
	    }

	    ++valstart;			// skip ':'
	    while ( isspace(mapped_file[valstart]) && mapped_file[valstart] != '\n' && valstart < mapped_file_size ) ++valstart;
	    while ( mapped_file[valstart+vallen] != '\n' && (valstart+vallen) < mapped_file_size ) ++vallen;
	    while ( vallen > 0 && isspace(mapped_file[valstart+vallen]) ) --vallen;
	    if ( (valstart+vallen) < mapped_file_size && isspace(mapped_file[valstart+vallen+1]) )
		++vallen;			// move to byte beyond non-space values

	    if ( value_len < (vallen + 1) ) {
		while ( value_len < (vallen + 1) ) value_len *= 2;
		value = (char *) realloc( value, value_len );
	    }
	    memcpy(value, &mapped_file[valstart], vallen);
	    value[vallen] = '\0';

	    off = valstart+vallen;

	    rcmap.insert(make_pair(string((const char*)keyword),string((const char*)value)));
	    rcmetamap.insert(make_pair(string((const char*)keyword),meta_entry_(keystart,keylen,valstart,vallen,timestp,timestplen)));

	    timestp = 0;
	    timestplen = 0;
	}

	free(keyword);
	free(value);
	unlock( fd );
    }

    void Casarc::startup( ) {
	if ( initialized == false ) {
	    initialized = true;
	    rcfiles = new std::map<ino_t,Casarc*>( );
	    filenames = new std::map<std::string,Casarc*>( );
	    rclist = new std::list<Casarc*>( );
	}
    }

    void Casarc::shutdown( ) {
	if ( initialized ) {
#if CASARC_DEBUG >= 1
	    fprintf( stderr, "casarc halt:   %ld rc files managed [", rclist->size() );
	    for ( std::list<Casarc*>::iterator iter = rclist->begin( ); iter != rclist->end( ); ++iter ) {
	      fprintf( stderr, iter == rclist->begin() ? "%ld" : ",%ld", (*iter)->size() );
	    }
	    fprintf( stderr, "] (pid:%d)\n", getpid() );
#endif
	    initialized = false;
	    for ( std::list<Casarc*>::iterator iter = rclist->begin( ); iter != rclist->end( ); ++iter ) {
		delete *iter;
	    }

	    delete rcfiles;
	    rcfiles = 0;
	    delete filenames;
	    filenames = 0;
	    delete rclist;
	    rclist = 0;
	    delete default_path;
	    default_path = 0;
	}
    }

}
