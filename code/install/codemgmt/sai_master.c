/*----------------------------------------------------------------------------
* sai_master: Setuid wrapper for sai_master.sh, a non-setuid copy of 'ai_master'
*-----------------------------------------------------------------------------
*
*   Copyright (C) 1997,1998
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
* Usage: sai_master <same options as ai_master>
*-----------------------------------------------------------------------------
* sai_master is a setuid wrapper for sai_master.sh.  It simply exec's a
* non-setuid copy of ai_master, sai_master.sh, allowing systems that do
* not support setuid shell scripts (most notably Linux and Digital UNIX)
* to perform ai_master operations.  See the documentation for ai_master
* for further details.
*
* Like ai_master, this program is not indended for direct execution by
* users.  It's only experimental anyway.
*
* Options:
*    See the documentation for ai_master.
*
* Exit status:
*   -1:  exec of sai_master.sh failed
*    1:  initialization error
*        All others returned by sai_master.sh.
*
* Original: 1997/09/08 by Jeff Uphoff, NRAO
* $Id$
*---------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define MASTERSH "/master/etc/sai_master.sh"

int
main (int argc, char *argv[])
{
  char *aipspath;
  char *aipsroot;
  char *toexec;

  argv[0] = "ai_master";

  if ((aipspath = getenv ("AIPSPATH")) == NULL) {
    /* Should not happen; this has been checked already. */ 
    fputs ("ai: AIPSPATH is not defined, abort!\n", stderr);
    return 1;
  }
  aipsroot = strtok (aipspath, " ");
  toexec = (char *)malloc (strlen (aipsroot) + strlen (MASTERSH) + 1);
  sprintf (toexec, "%s%s", aipsroot, MASTERSH);
  printf ("uid: %d, euid: %d\n", getuid(), geteuid());
  return (execv (toexec, argv)); 
}
