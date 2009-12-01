#-----------------------------------------------------------------------------
# AIPS++ top directory makefile.
#-----------------------------------------------------------------------------
#
#   Copyright (C) 1992-2004
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
# appsglish/apps symlink is needed to compile the megaserver
INCLINKS += include/appsglish


# Subdirectories in which to invoke "allsys", in order.
#------------------------------------------------------
ALLSYSD  := install $(AIPSGLISH) $(PACKAGES)

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

# appsglish/apps symlink is needed to compile the megaserver
$(INCLINKS) :
	-@ rm -f $@
	 if [ -d $(@F)/implement ] ; then \
	   ln -s ../$(@F)/implement $@ ; \
	 else \
	   ln -s ../$(@F)/apps $@ ; \
         fi

docsys : $(DOCSYS)

atoms :
	$(MAKE) -C $(addprefix $(CODEDIR)/, doc/user) atoms

resolvedocs :
	grep externallabels $(addprefix $(CODEDIR)/, doc/*/*.latex)  | awk -F: '{print $$1}' | sort -u> $(ARCHTMPD)/labels.file ; \
	grep externallabels $(addprefix $(CODEDIR)/, doc/*/*/*.latex)  | awk -F: '{print $$1}' | sort -u>> $(ARCHTMPD)/labels.file ; \
	for filename in `cat $(ARCHTMPD)/labels.file | sed -e 's/code.doc/docs/g' | sed -e 's/latex/ps.gz/g' | sed -e 's/\.dir.*gz/.ps.gz/g'` ; do \
	  echo $$filename ; \
          if [ -e $$filename ] ; then \
              rm $$filename ; \
          fi \
	done
	$(MAKE) -C $(addprefix $(CODEDIR)/, doc)

docscan docextr :
	 @ for i in $(ALLSYSD) ; do \
	     echo "" ; \
	     echo gmake[$(MAKELEVEL)]: $(MAKE) -C $$i $@ ; \
	     $(MAKE) -C $$i $@ ; \
	  done

changelogs :
	-@ rm -rf $(AIPSROOT)/docs/project/releasenotes/changelogs
	   mkdir $(AIPSROOT)/docs/project/releasenotes/changelogs
	   buildchangelog -split module -dir $(AIPSROOT)/docs/project/releasenotes/changelogs $(AIPSROOT)/code

docdoxy : $(AIPSDOCS)/doxygen 
	 @ cp $(DOXYGENCFG) $</doxygen.cfg; \
	   if [ "$(DOXYDOT)" != "" ]; then \
	     echo "HAVE_DOT         = YES" >> $</doxygen.cfg; \
	     echo "DOT_PATH         = $(DOXYDOT)" >> $</doxygen.cfg; \
	   fi; \
	   echo "OUTPUT_DIRECTORY = $<" >> $</doxygen.cfg; \
	   echo "STRIP_FROM_PATH  = $(AIPSCODE)" >> $</doxygen.cfg; \
	   echo "INPUT            = $(DOXYPACKAGES)" >> $</doxygen.cfg; \
	   echo "INPUT_FILTER     = $(AIPSARCH)/bin/doxygen_pp" >> $</doxygen.cfg; \
	   echo "GENERATE_TAGFILE = $</aipspp.tag" >> $</doxygen.cfg; \
           $(RM) $</doxygen.log;
	 $(DOXYGEN) $</doxygen.cfg 2>&1 > $</doxygen.log

$(AIPSDOCS)/doxygen :
	-@ echo ""
	 @ amkdir -p ug=rwx,g+s,o=rx -v $@

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

##
## developer's release targets
##
drupdate :
	@ mktree -l -r -s
	@ $(MAKE) 'NODEP=1' cleancode
	@ $(MAKE) 'NODEP=1' chkout

drbuild :
	@ $(MAKE) allsys

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
	-@ echo "    docscan: scan C++ sources for cxx2html class information."
	-@ echo "    docextr: extract cxx2html documentation from C++ headers."
	-@ echo "    docdoxy: extract doxygen documentation from C++ headers."
	-@ echo " changelogs: build the changelog html files."
	-@ echo "   runtests: run all test programs."
	-@ echo ""
	-@ echo "Programmer"
	-@ echo "----------"
	-@ echo "   symlinks: create include file symlinks."
