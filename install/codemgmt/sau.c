/*----------------------------------------------------------------------------
* sau: Setuid C wrapper for sau.sh, a non-setuid copy of 'au'
*-----------------------------------------------------------------------------
*
*   Copyright (C) 1997
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
* Usage: sau <same options as au>
*-----------------------------------------------------------------------------
* sau is a setuid C wrapper for sau.sh.  It simply exec's a non-setuid
* copy of au, sau.sh, allowing systems that do not support setuid shell
* scripts (most notably Linux and Digital UNIX) to perform au
* operations.  See the documentation for au for further details.
*
* Options:
*    See the documentation for au.
*
* Exit status:
*   -1:  exec of sau.sh failed
*        All others returned by sau.sh.
*
* Original: 1997/09/08 by Jeff Uphoff, NRAO
* $Id$
*---------------------------------------------------------------------------*/

#include <unistd.h>

int
main (int argc, char *argv[])
{
  argv[0] = "au";
  return (execvp ("sau.sh", argv));
}
