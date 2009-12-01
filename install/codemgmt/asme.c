/*----------------------------------------------------------------------------
* asme: Execute a command with the effective uid set to the real uid
*-----------------------------------------------------------------------------
*
*   Copyright (C) 1996
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
* Usage: asme <command>
*-----------------------------------------------------------------------------
* asme executes a command with the effective uid set to the real uid.
*
* Options:
*   none
*
* Exit status:
*   -1:  Fork failed.
*    0:  Success, value returned on stdout.
* else:  Exit status returned by the command.
*
* Original: 1996/03/28 by Mark Calabretta, ATNF
* $Id$
*---------------------------------------------------------------------------*/

#include <unistd.h>
#include <stdlib.h>

main(argc,argv)
 
int   argc;
char  *argv[];

{
   if (argc < 1) exit(0);
   setuid(getuid());
   exit(system(argv[1]));
}
