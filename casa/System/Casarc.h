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

#ifndef CASA_CASARC_H
#define CASA_CASARC_H
#include <sys/types.h>
#include <sys/stat.h>
#include <string>
#include <list>
#include <map>

namespace casa { //# NAMESPACE CASA - BEGIN

    class Casarc {

	friend class CasarcCleanup;

	public:

	    typedef std::map<std::string,std::string>::const_iterator iterator;

	    // return default casarc file, e.g. ~/.casarc
	    static Casarc &instance( );

	    // set and clear the default casarc path (to something other than ~/.casarc)
	    static void setDefaultPath( const std::string &path );
	    static void clearDefaultPath( );

	    // return a casarc object attached to a particular file
	    static Casarc &instance( const std::string &path );

	    // return the list of rcfiles that have been loaded
            static const std::list<Casarc*> & list( );

	    // adds the keyword->value mapping
	    void put( const std::string &keyword, const std::string &value );

	    // retrieves the mapping for keyword (or "" if the mapping is not defined)
	    std::string get( const std::string &keyword );

	    // retrieves the mapping for keyword in value (and returns true if the
	    // mapping is defined or false otherwise)
	    bool get( const std::string &keyword, std::string &value );

	    size_t size( ) const;

	    // path to the file that this Casarc mirrors...
	    const std::string &path( ) const { return filename; }

	    iterator begin( );
	    iterator end( );

	private:

	    class meta_entry_ {
	    public:
		meta_entry_( off_t keys_, int keyl_, off_t vals_, int vall_, off_t times_, int timel_ ) : 
					key_offset_(keys_), key_len_(keyl_), 
					val_offset_(vals_), val_len_(vall_),
					time_offset_(times_), time_len_(timel_) { }

		off_t key_offset( ) const { return key_offset_; }
		int key_length( ) const { return key_len_; }

		off_t value_offset( ) const { return val_offset_; }
		int value_length( ) const { return val_len_; }

		off_t time_offset( ) const { return time_offset_; }
		int time_length( ) const { return time_len_; }

	    private:
		off_t key_offset_;
		int key_len_;
		off_t val_offset_;
		int val_len_;
		off_t time_offset_;
		int time_len_;
	    };

	    char *mapped_file;
	    off_t mapped_file_size;

	    std::list<int> stale_fds;
	    void close( int );

	    std::list<pid_t> have_lock;

	    enum lock_mode { READ, READ_WRITE, WRITE, APPEND };
	    // returns a file descriptor or -1 on error...
	    int lock( lock_mode mode );
	    void unlock( int fd );

	    void sync( );
	    double current_modification_time( struct stat &buf );

	    std::string filename;
	    double timestamp;

	    std::map<std::string,std::string> rcmap;
	    // this is broken off separate from the rcmap to allow the
	    // rcmap to be used for iteration by users of this class...
	    std::map<std::string,meta_entry_> rcmetamap;

	    void read_file( );

	    ino_t inode;

	    // singleton Casarcs, (inode->casarc)
	    static std::map<ino_t,Casarc*> *rcfiles;
	    static std::map<std::string,Casarc*> *filenames;
	    static std::list<Casarc*> *rclist;
	    static std::string *default_path;
	    // path is the path to the .casarc (or .aipsrc file)
	    Casarc( const std::string &path );
	    static bool initialized;
	    static void startup( );
	    static void shutdown( );
	    ~Casarc( ) { }
    };

    static class CasarcCleanup {
	public:
	    CasarcCleanup( ) { creation_count += 1; }
	    ~CasarcCleanup( ) { if ( --creation_count == 0 ) { Casarc::shutdown( ); } }
	private:
	    static unsigned int creation_count;
    } local_cleanup_object;
  
} //# NAMESPACE CASA - END

#endif
