#-----------------------------------------------------------------------------
# AIPS++ code management directory makefile
#-----------------------------------------------------------------------------
#
#   Copyright (C) 1992-1997
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
# GNU makefile used to install AIPS++ code management utilities.
#
# Original: 1994/09/05 by Mark Calabretta, ATNF.
# $Id$
#=============================================================================

# AIPS++ fundamentals.
#---------------------
AIPSROOT := $(word 1, $(AIPSPATH))
AIPSARCH := $(AIPSROOT)/$(word 2, $(AIPSPATH))
include $(AIPSARCH)/makedefs


# Source lists.
#--------------
AISCRIPT := ai ae amv am ao au ax rau
DISTSCRP := inhale sneeze
SCRIPTS  := alog av copy_installation ih_log_info.pl inhold mktree rai rao \
            runtests squiz xrcs

AIPSEXES := $(basename $(filter %.c,$(AIPSSRCS)))
PGMREXES := $(basename $(wildcard *.c))
ALLEXES  := $(sort $(AIPSEXES) $(PGMREXES))


# Pattern rules.
#---------------
$(ARCHBIND)/% :: $(CODEDIR)/%
	@ $(RM) $@
	  cp $< $@
	@ chmod 775 $@

$(ARCHBIND)/% : $(CODEDIR)/%.c
	-@ echo ""
	   cd $(ARCHTMPD) && $(CC) $(COPT) -o $@ $^
	-@ $(RM) $(ARCHTMPD)/$(@F).o
	-@ chmod 775 $@

# Programmer-oriented pattern rules.
ifeq "$(MAKEMODE)" "programmer"
   vpath %.c $(CODEDIR)

   %.o : %.c
	$(CC) $(CFLAGS) -c $<
endif


# Static and static pattern rules.
#---------------------------------
allsys : $(AISCRIPT) $(DISTSCRP) $(SCRIPTS) bin $(CODESUBS)

.cleancode ::
	-$Q cd $(CODEDIR) && $(RM) *.i *.o

# Scripts.
$(AISCRIPT) $(DISTSCRP) $(SCRIPTS) : % : $(ARCHBIND)/% ;

# Source code management scripts.
ifdef AIPSRCS
   $(addprefix $(ARCHBIND)/,$(AISCRIPT)) : $(ARCHBIND)/% : $(RCSDIR)/%,v
	@ $(CO) -p $< > $@
	@ chmod ug=rwx,o=r,u+s $@
else
   $(addprefix $(ARCHBIND)/,$(AISCRIPT)) : $(ARCHBIND)/% : $(CODEDIR)/%
	cat $< > $@
	@ chmod ug=rwx,o=r,u+s $@
endif

# Code distribution scripts.
ifdef AIPSRCS
   $(addprefix $(ARCHBIND)/,$(DISTSCRP)) : $(ARCHBIND)/% : $(RCSDIR)/%,v
	@ $(CO) -p $< > $@
	@ chmod 764 $@
else
   $(addprefix $(ARCHBIND)/,$(DISTSCRP)) : $(ARCHBIND)/% : $(CODEDIR)/%
	  cat $< > $@
	@ chmod 764 $@
endif

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
	$(CC) $(CFLAGS) -o $@ $^
endif

show_local :
	-@ echo ""
	-@ echo "Variables defined in the install/codemgmt makefile"
	-@ echo "=================================================="
	-@ echo ""
	-@ echo "AISCRIPT=$(AISCRIPT)"
	-@ echo ""
	-@ echo "DISTSCRP=$(DISTSCRP)"
	-@ echo ""
	-@ echo "SCRIPTS =$(SCRIPTS)"
	-@ echo ""
	-@ echo "AIPSEXES=$(AIPSEXES)"
	-@ echo "PGMREXES=$(PGMREXES)"
	-@ echo "ALLEXES =$(ALLEXES)"

help ::
	-@ echo ""
	-@ echo "Targets defined in the install/codemgmt makefile"
	-@ echo "================================================"
	-@ echo ""
	-@ echo "System"
	-@ echo "------"
	-@ echo " (AISCRIPT): install source code management scripts."
	-@ echo " (DISTSCRP): install code distribution scripts."
	-@ echo "  (SCRIPTS): install general purpose scripts."
	-@ echo "      bin/%: compile and install an (AIPSEXES) executable."
	-@ echo ""
	-@ echo "Programmer"
	-@ echo "----------"
	-@ echo "  (ALLEXES): compile an executable."
