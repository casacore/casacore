/*----------------------------------------------------------------------------
* getuinfo: get the value of the specified AIPS++ resource
*-----------------------------------------------------------------------------
*
*   Copyright (C) 2001
*   Associated Universities, Inc. Washington DC, USA.
*
*   This program is free software; you can redistribute it and/or modify
*   it under the terms of the GNU General Public License as published by
*   the Free Software Foundation; either version 2 of the License, or
*   (at your option) any later version.
*
*   This program is distributed in the hope that it will be useful,
*   but WITHOUT ANY WARRANTY; without even the implied warranty of
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*   GNU General Public License for more details.
*
*   You should have received a copy of the GNU General Public License
*   along with this program; if not, write to the Free Software
*   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*
*   Correspondence concerning AIPS++ should be addressed as follows:
*          Internet email: aips2-request@nrao.edu.
*          Postal address: AIPS++ Project Office
*                          National Radio Astronomy Observatory
*                          520 Edgemont Road
*                          Charlottesville, VA 22903-2475 USA
*
*-----------------------------------------------------------------------------
* getuinfo prints the users gecos field from the password file
*
* Useage: getuinfo $LOGNAME
*
* Options: none
*
* Exit status:
*    0: got the user info and printed it out;
*    1: failed to find the user info
*--------------------------------------------------------------------------*/

#include <pwd.h>
#include <sys/types.h>
int main(int argc, char **argv)
{
   int rstatus = 0;
   struct passwd *me = getpwnam(argv[1]);
   if(me){
      printf("%s\n", me->pw_gecos);
   } else {
      rstatus = 1;
   }
   return rstatus;
}

