#-----------------------------------------------------------------------------
# AIPS++ install directory makefile
#-----------------------------------------------------------------------------
#
#   Copyright (C) 1992-1997,1998,1999,2001,2002
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
# GNU makefile used to install AIPS++ system utilities and databases.
#
# Original: 1992/03/16 by Mark Calabretta, ATNF.
# $Id$
#=============================================================================

# AIPS++ fundamentals.
#---------------------
AIPSROOT := $(word 1, $(AIPSPATH))
AIPSARCH := $(AIPSROOT)/$(word 2, $(AIPSPATH))
include $(AIPSARCH)/makedefs


# Source lists.
#--------------
DOTFILES := cshrc exrc login profile prompt glishrc
DOTEXECS := rshexec
AIPSINIT := aipsinit.csh aipsinit.es aipsinit.rc aipsinit.sh
ARCHSCRP := aipsrc makedefs
SCRIPTS  := adate affirm amkdir assay avers depend doover mkinst updatelib floatcheck.sh

AIPSEXES := $(basename $(filter %.c,$(AIPSSRCS)))
PGMREXES := $(basename $(wildcard *.c))
ALLEXES  := $(sort $(AIPSEXES) $(PGMREXES))


# Pattern rules.
#---------------
$(AIPSROOT)/.% : $(CODEDIR)/%
	  sed -e 's#__AIPSROOT#$(AIPSROOT)#g' $< > $@
	@ chmod 664 $@

$(AIPSROOT)/% : $(CODEDIR)/%
	  sed -e 's#__AIPSROOT#$(AIPSROOT)#g' $< > $@
	@ chmod 664 $@

$(ARCHBIND)/% : $(CODEDIR)/%
	@ $(RM) $@
	  cp $< $@
	@ chmod 775 $@

$(AIPSARCH)/% : $(CODEDIR)/%
	@ $(RM) $@
	  cp $< $@
	@ chmod 664 $@

$(ARCHBIND)/% : $(CODEDIR)/%.c
	-@ echo ""
	   cd $(ARCHTMPD) && $(CC) $(COPT) -DTOOLBOX -o $@ $^
	-@ $(RM) $(ARCHTMPD)/$(@F).o
	-@ chmod 775 $@

# Programmer-oriented pattern rules.
ifeq "$(MAKEMODE)" "programmer"
   vpath %.c $(CODEDIR)

   %.o : %.c
	$(CC) $(CFLAGS) -DTOOLBOX -c $<
endif


# Static and static pattern rules.
#---------------------------------
.PHONY : thisys

allsys : thisys $(CODESUBS)

thisys : $(CHKDIR) $(DOTFILES) $(DOTEXECS) $(AIPSINIT) $(ARCHSCRP) $(SCRIPTS) \
    bin

.cleancode ::
	-$Q cd $(CODEDIR) && $(RM) *.i *.o

# Just the root level files.
aipsroot : $(DOTFILES) $(DOTEXECS) $(AIPSINIT)

# Root level "dot" files.
$(DOTFILES) $(DOTEXECS) : % : $(AIPSROOT)/.% ;

$(addprefix $(AIPSROOT)/.,$(DOTEXECS)) : $(AIPSROOT)/.% : $(CODEDIR)/%
	sed -e 's#__AIPSROOT#$(AIPSROOT)#g' $< > $@
	@ chmod 775 $@

# Root level include-scripts and databases.
$(AIPSINIT) : % : $(AIPSROOT)/% ;

# Default aipsrc and makedefs files.
$(ARCHSCRP) : % : $(AIPSARCH)/% ;

# Scripts.
$(SCRIPTS) : % : $(ARCHBIND)/% ;

# Utility programs.
bin    : $(foreach BINARY,$(AIPSEXES),$(ARCHBIND)/$(BINARY)) ;

$(AIPSEXES:%=bin/%) : bin/% : $(ARCHBIND)/% ;

# Programmer-oriented static and static pattern rules.
ifeq "$(MAKEMODE)" "programmer"
   all : $(basename $(wildcard *.c))

   .clean ::
	-$(RM) *.i *.o

   .cleaner ::
	-$(RM) $(ALLEXES)

   $(ALLEXES) : % : %.o
	$(CC) $(CFLAGS) -DTOOLBOX -o $@ $^
endif

show_local :
	-@ echo ""
	-@ echo "Variables defined in the install makefile"
	-@ echo "========================================="
	-@ echo ""
	-@ echo "DOTFILES=$(DOTFILES)"
	-@ echo ""
	-@ echo "DOTEXECS=$(DOTEXECS)"
	-@ echo ""
	-@ echo "AIPSINIT=$(AIPSINIT)"
	-@ echo ""
	-@ echo "ARCHSCRP=$(ARCHSCRP)"
	-@ echo ""
	-@ echo "SCRIPTS =$(SCRIPTS)"
	-@ echo ""
	-@ echo "AIPSEXES=$(AIPSEXES)"
	-@ echo "PGMREXES=$(PGMREXES)"
	-@ echo "ALLEXES =$(ALLEXES)"

help ::
	-@ echo ""
	-@ echo "Targets defined in the install makefile"
	-@ echo "======================================="
	-@ echo ""
	-@ echo "System"
	-@ echo "------"
	-@ echo "     thisys: same as allsys but without recursion."
	-@ echo "   aipsroot: install all root level files."
	-@ echo " (DOTFILES): install root level dot files."
	-@ echo " (DOTEXECS): install root level executables."
	-@ echo " (AIPSINIT): install root level aipsinit scripts."
	-@ echo " (ARCHSCRP): install aipsrc and makedefs."
	-@ echo "  (SCRIPTS): install general purpose scripts."
	-@ echo "      bin/%: compile and install an (AIPSEXES) executable."
	-@ echo ""
	-@ echo "Programmer"
	-@ echo "----------"
	-@ echo "  (ALLEXES): compile an executable."
