#-----------------------------------------------------------------------------
# AIPS++ documentation utilities makefile
#-----------------------------------------------------------------------------
#
#   Copyright (C) 1994-1996,1997,1999,2000
#   Associated Universities, Inc. Washington DC, USA.
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
#   Correspondence concerning AIPS++ should be addressed as follows:
#          Internet email: aips2-request@nrao.edu.
#          Postal address: AIPS++ Project Office
#                          National Radio Astronomy Observatory
#                          520 Edgemont Road
#                          Charlottesville, VA 22903-2475 USA
#
#-----------------------------------------------------------------------------
# GNU makefile used to install AIPS++ documentation utilities.
#
# Original: 1994/11/16 by Mark Calabretta, ATNF.
# $Id$
#=============================================================================

# AIPS++ fundamentals.
#---------------------
AIPSROOT := $(word 1, $(AIPSPATH))
AIPSARCH := $(AIPSROOT)/$(word 2, $(AIPSPATH))
include $(AIPSARCH)/makedefs


# Source lists.
#--------------
SCRIPTS  := ac rac dox redoicons.sh dvips.sh mkpsfiles.sh
PERL5EXE := cxx2html help2tex buildchangelog doxygen_pp
PERL5REQ := Class.pm Html.pm ScanCxx.pm Path.pm


# Pattern rules.
#---------------
$(ARCHBIND)/% : $(CODEDIR)/%
	@ $(RM) $@
	  cp $< $@
	@ chmod 775 $@


# Static and static pattern rules.
#---------------------------------
allsys : $(SCRIPTS) $(CODESUBS)

# Scripts.
$(SCRIPTS) : % : $(ARCHBIND)/% ;

# Perl scripts.
ifneq "$(PERL4)" ""
   allsys : $(PERL4EXE)

   $(PERL4EXE) : % : $(ARCHBIND)/% ;

   $(PERL4EXE:%=$(ARCHBIND)/%) : $(ARCHBIND)/% : $(CODEDIR)/%
	  sed -e '1s#/usr/local/bin/perl4#$(PERL4)#' $< > $@
	@ chmod 775 $@
endif

ifneq "$(PERL5)" ""
   allsys : $(PERL5EXE) $(PERL5REQ)

   $(PERL5EXE) : % : $(ARCHBIND)/% ;

   $(PERL5EXE:%=$(ARCHBIND)/%) : $(ARCHBIND)/% : $(CODEDIR)/%
	  sed -e '1s#/usr/local/bin/perl5#$(PERL5)#' \
	      -e '1s#\$$ARCHBIND#$(ARCHBIND)#' $< > $@
	@ chmod 775 $@

   $(PERL5REQ) : % : $(ARCHBIND)/% ;

   $(PERL5REQ:%=$(ARCHBIND)/%) : $(ARCHBIND)/% : $(CODEDIR)/%
	  cat $< > $@
	@ chmod 664 $@
endif

show_local :
	-@ echo ""
	-@ echo "Variables defined in the install makefile"
	-@ echo "========================================="
	-@ echo ""
	-@ echo "PERL4EXE=$(PERL4EXE)"
	-@ echo "PERL5EXE=$(PERL5EXE)"
	-@ echo "PERL5REQ=$(PERL5REQ)"
	-@ echo ""
	-@ echo "SCRIPTS =$(SCRIPTS)"

help ::
	-@ echo ""
	-@ echo "Targets defined in the install makefile"
	-@ echo "======================================="
	-@ echo ""
	-@ echo "System"
	-@ echo "------"
	-@ echo " (PERL4EXE): install perl-4 documentation utilities."
	-@ echo " (PERL5EXE): install perl-5 documentation utilities."
	-@ echo " (PERL5REQ): install perl-5 required files."
	-@ echo "  (SCRIPTS): install documentation utilities."
