#-----------------------------------------------------------------------------
# AIPS++ top directory makefile.
#-----------------------------------------------------------------------------
#
#   Copyright (C) 1992-1997,1999
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
# GNU makefile used to build all of AIPS++.
#
# Original: 1992/05/01 by Mark Calabretta, ATNF.
# $Id$
#=============================================================================

# AIPS++ fundamentals.
#---------------------
AIPSROOT := $(word 1, $(AIPSPATH))
AIPSARCH := $(AIPSROOT)/$(word 2, $(AIPSPATH))
include $(AIPSARCH)/makedefs


# Directory containing symbolic links.
#-------------------------------------
SYMDIR   := include


# Directory symlinks.
#--------------------
ifdef AIPSRCS
   INCLINKS := $(patsubst $(AIPSRCS)/%/implement,include/%, \
                    $(wildcard $(AIPSRCS)/*/implement))
else
   INCLINKS := $(patsubst $(AIPSCODE)/%/implement,include/%, \
                    $(wildcard $(AIPSCODE)/*/implement))
endif


# Subdirectories in which to invoke "allsys", in order.
#------------------------------------------------------
ALLSYSD  := install aips dish display synthesis vlbi

# Should only exist in consortium installations.
ALLSYSD  += admin

# Add auxiliary and consortium directories, if any.
ALLSYSD  += $(AUXILIARY) $(CONSORTIA)

# Compile documentation?
ALLSYSD  += $(DOCSYS)

# Check for existence and convert to absolute pathnames.
ALLSYSD  := $(addprefix $(CODEDIR)/,$(wildcard $(ALLSYSD)))


# Reorder CODESUBS to match ALLSYSD as far as possible (mainly for "chkout").
#----------------------------------------------------------------------------
CODESUBS := $(filter $(CODESUBS),$(ALLSYSD)) \
            $(filter-out $(ALLSYSD),$(CODESUBS))


# Static and static pattern rules.
#---------------------------------
.PHONY : symlinks

allsys : symlinks sysdirs $(ALLSYSD) $(DOCEXTR)

.cleansys ::
	-$Q cd $(ARCHAUXD) && $(RM) docscan docscan.* docextr.*

symlinks : $(SYMDIR) $(INCLINKS) ;

$(SYMDIR) :
	 mkdir $@
	 chmod ug=rwx,g+s,o=rx $@

$(INCLINKS) :
	-@ rm -f $@
	   ln -s ../$(@F)/implement $@

docsys : $(DOCSYS)

resolvedocs :
	find $(addprefix $(CODEDIR)/, doc) -name "*.latex" -exec touch {} \;
	$(MAKE) -C $(addprefix $(CODEDIR)/, doc)

docscan docextr :
	 @ for i in $(ALLSYSD) ; do \
	     echo "" ; \
	     echo gmake[$(MAKELEVEL)]: $(MAKE) -C $$i $@ ; \
	     $(MAKE) -C $$i $@ ; \
	  done

runtests :
	 @ for i in $(ALLSYSD) ; do \
	     echo "" ; \
	     echo gmake[$(MAKELEVEL)]: $(MAKE) -C $$i $@ ; \
	     $(MAKE) -C $$i $@ ; \
	   done
	-@ if [ -f $(AIPSARCH)/bintest/runtests.report ] ; then \
	     echo "" ; \
	     echo "" ; \
	     echo "Summary of test results" ; \
	     echo "-----------------------" ; \
	     cat $(AIPSARCH)/bintest/runtests.report ; \
	   fi

# Programmer-oriented static and static pattern rules.
ifeq "$(MAKEMODE)" "programmer"
   all : symlinks
endif

show_local :
	-@ echo ""
	-@ echo "Variables defined in the top-level makefile"
	-@ echo "==========================================="
	-@ echo ""
	-@ echo "SYMDIR  =$(SYMDIR)"
	-@ echo "INCLINKS=$(INCLINKS)"
	-@ echo "ALLSYSD =$(ALLSYSD)"

help ::
	-@ echo ""
	-@ echo "Targets defined in the top-level makefile"
	-@ echo "========================================="
	-@ echo ""
	-@ echo "System"
	-@ echo "------"
	-@ echo "   symlinks: create include file symlinks."
	-@ echo "     docsys: compile all documentation."
	-@ echo "    docscan: scan C++ sources for class information."
	-@ echo "    docextr: extract documentation from the C++ sources."
	-@ echo "   runtests: run all test programs."
	-@ echo ""
	-@ echo "Programmer"
	-@ echo "----------"
	-@ echo "   symlinks: create include file symlinks."
