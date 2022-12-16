//# tAppState.cc: This program tests the AppState interface
//# Copyright (C) 2017
//# Associated Universities, Inc. Washington DC, USA.
//#
//# This program is free software; you can redistribute it and/or modify it
//# under the terms of the GNU General Public License as published by the Free
//# Software Foundation; either version 2 of the License, or (at your option)
//# any later version.
//#
//# This program is distributed in the hope that it will be useful, but WITHOUT
//# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//# more details.
//#
//# You should have received a copy of the GNU General Public License along
//# with this program; if not, write to the Free Software Foundation, Inc.,
//# 675 Massachusetts Ave, Cambridge, MA 02139, USA.
//#
//# Correspondence concerning AIPS++ should be addressed as follows:
//#        Internet email: aips2-request@nrao.edu.
//#        Postal address: AIPS++ Project Office
//#                        National Radio Astronomy Observatory
//#                        520 Edgemont Road
//#                        Charlottesville, VA 22903-2475 USA

#include <casacore/casa/aips.h>
#include <casacore/casa/System/AppState.h>
#include <casacore/casa/Utilities/Assert.h>
#include <casacore/casa/iostream.h>


#include <casacore/casa/namespace.h>
class MyState: public AppState {

    public:
        MyState( const std::list<std::string> &init ) : my_path(init) { }
        std::list<std::string> dataPath( ) const { return my_path; }
        bool initialized( ) const { return true; }

    private:
        std::list<std::string> my_path;

};

int main( int , char *[] ) {
    AlwaysAssertExit(AppStateSource::fetch( ).initialized( ) == false);

    std::list<std::string> my_path_state;

    // in an actual implementation these would be paths
    // to casacore tables or images... but this is just
    // for demonstration...
    my_path_state.push_back("/usr/bin");
    my_path_state.push_back("/bin");
    my_path_state.push_back("/opt/local/bin");
    my_path_state.push_back("/home/rh/root/usr/bin");

    AppState *original_state = &AppStateSource::fetch( );
    MyState *new_state = new MyState( my_path_state );
    AppStateSource::initialize( new_state );
    AlwaysAssertExit(&AppStateSource::fetch( ) != original_state);
    AlwaysAssertExit(&AppStateSource::fetch( ) == new_state);
    AlwaysAssertExit(AppStateSource::fetch( ).dataPath( ) == my_path_state);

    return 0;
}
