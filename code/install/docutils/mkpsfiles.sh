#!/bin/sh
#
#   Copyright (C) 1999,2000
#   Associated Universities, Inc. Washington DC, USA.
#
#   This program is free software; you can redistribute it and/or modify it
#   under the terms of the GNU General Public License as published by the Free
#   Software Foundation; either version 2 of the License, or (at your option)
#   any later version.
#
#   This program is distributed in the hope that it will be useful, but WITHOUT
#   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
#   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
#   more details.
#
#   You should have received a copy of the GNU General Public License along
#   with this program; if not, write to the Free Software Foundation, Inc.,
#   675 Massachusetts Ave, Cambridge, MA 02139, USA.
#
#   Correspondence concerning AIPS++ should be addressed as follows:
#          Internet email: aips2-request@nrao.edu.
#          Postal address: AIPS++ Project Office
#                          National Radio Astronomy Observatory
#                          520 Edgemont Road
#                          Charlottesville, VA 22903-2475 USA
#
#   $Id$
#
# This shell script loops through chapter, section and subsection of a .toc file
# and figures out the start and end page numbers
#
# argument $1 is AIPSCODE from the makedefs
# argument $2 is the basename of the document
#
if [ -x /usr/bin/nawk ] ; then
 AWK=/usr/bin/nawk
elif [ -x /usr/bin/gawk ] ; then
  AWK=/usr/bin/gawk
else
  AWK=awk
fi
for doWhat in ch sec subsec
do
  $AWK -f $1/install/docutils/"$doWhat"pag.awk $2.toc > $doWhat.pagnos
  chmod u+x $doWhat.pagnos
  cat $doWhat.pagnos
done
