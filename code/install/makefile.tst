#-----------------------------------------------------------------------------
# makefile.tst: Generic AIPS++ test program makefile
#-----------------------------------------------------------------------------
#
#   Copyright (C) 1992-2002
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
# This GNU makefile is included by other AIPS++ makefiles and is not intended
# for stand-alone use.
#
# Original: 1992/06/24 by Mark Calabretta, ATNF
# $Id$
#-----------------------------------------------------------------------------

# AIPS++ fundamentals.
#---------------------
include $(word 1, $(AIPSPATH))/code/install/makefile.tst_c


# Pattern rules.
#---------------
$(BINTESTD)/% : $(CODEDIR)/%.cc $(INSTLIBR:%=$(CODEDIR)/templates) $(AIPSLIBS)
	@ echo ""
	@ $(TIMER)
	@ echo "Remaking $@ ($(TESTOPT)) because of $?"
	@ $(INSTLIBR:%=$(MAKE) %)
	@ cd $(TMPPCKGD) && \
	  $(C++) $(C++OPTS) -I$(CODEDIR) $(AIPSINCL) -o $@ $< $(AIPSVERS) $(INSTLIBR) $(AIPSLIBS) $(XTRNLIBS) $(INSTLIBR)
	@ $(TIMER)
	@ $(RM) $(TMPPCKGD)/$(<F:cc=o)
	@ chmod 775 $@


# Programmer-oriented pattern rules.
ifeq "$(MAKEMODE)" "programmer"
   .PRECIOUS : $(PGMRBIND)/%

   $(PGMRBIND)/% : $(PGMRBIND)/%.o $(PGMRLIBR)
	$(C++) $(CPPFLAGS) $(PRGAPINC) $(PGMRINCL) $(C++FLAGS) $(LDFLAGS) -o $@ $< $(PGMRLIBS);

   ifneq "$(PGMRBIND)" "."
     % : $(PGMRBIND)/% ;
   endif

   check : $(ALLEXES)
	@ for PROG in $(ALLEXES); do \
	    if [ "$(PGMRBIND)" != "." ]; \
	    then \
	      $(RM) $$PROG; \
	      ln -s $(PGMRBIND)/$$PROG; \
	    fi; \
	    assay $$PROG; \
	    if [ "$(PGMRBIND)" != "." ]; \
	    then \
	      $(RM) $$PROG; \
	    fi; \
	  done
endif
