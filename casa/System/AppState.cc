//# AppState.cc: casacore library configuration without environment variabes
//# Copyright (C) 2017
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

#include <numeric>
#include <sys/stat.h>
#include <casacore/casa/System/AppState.h>

namespace casacore {

AppState *AppStateSource::user_state = 0;

std::string AppState::resolve(const std::string &subdir) const {
    struct stat statbuf;
    if ( stat( subdir.c_str( ), &statbuf ) == 0 ) return subdir;

    struct {
        std::string operator( )(std::string s, std::string dir) {
            if ( s.size( ) > 0 ) return s;
            struct stat statbuf;
            std::string path = dir + "/" + needle;
            return stat( path.c_str( ), &statbuf ) == 0 ? path : s;
        }
        std::string needle;
    } in_lieu_of_lambda = { subdir };

    const std::list<std::string> &casadata = dataPath( );
    std::string result = std::accumulate( casadata.begin( ), casadata.end( ), std::string( ), in_lieu_of_lambda );
//  std::string result = std::accumulate( casadata.begin( ), casadata.end( ), std::string( ),
//                                        [&](std::string s, std::string dir ) {
//                                            if ( s.size( ) > 0 ) return s;
//                                            std::string path = dir + sep + subdir;
//                                            return stat( path.c_str( ), &statbuf ) == 0 ? path : s; } );
    return result.size( ) > 0 ? result : subdir;
}

}
