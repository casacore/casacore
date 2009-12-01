#-----------------------------------------------------------------------------
# AIPS++ printer utilities makefile
#-----------------------------------------------------------------------------
#
#   Copyright (C) 1992,1993,1994,1995
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
# GNU makefile used to install printer utilities, including 'lwf_a', the
# text-to-PostScript filter used by AIPS++.
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
LWFCSRCS := $(filter %.c,$(AIPSSRCS))
MANPAGES := lwf_a.1
SCRIPTS  := pra prd pri prm prq


# Pattern rules.
#---------------
$(ARCHMAN1)/% :: $(CODEDIR)/%
	cp $< $@
	@ chmod 664 $@


$(ARCHBIND)/% :: $(CODEDIR)/%
	@ $(RM) $@
	  cp $< $@
	@ chmod 775 $@

# Programmer-oriented pattern rules.
ifeq "$(MAKEMODE)" "programmer"
   vpath %.c $(CODEDIR)

   %.o : %.c
	$(CC) $(CFLAGS) -c $<
endif


# Static and static pattern rules.
#---------------------------------
allsys : $(ARCHBIND)/lwf_a $(MANPAGES) $(SCRIPTS)

.cleancode ::
	-$Q cd $(CODEDIR) && $(RM) *.i *.o lwf_a

# The lwf_a executable.
bin/lwf_a : $(ARCHBIND)/lwf_a ;

$(ARCHBIND)/lwf_a : $(addprefix $(CODEDIR)/,$(LWFCSRCS))
	cd $(ARCHTMPD) ; $(CC) $(COPT) -o $@ $^
	@ rm -f $(ARCHTMPD)/lwf_a.o $(ARCHTMPD)/range.o
	@ chmod 775 $@

# Manual page.
$(MANPAGES) : % : $(ARCHMAN1)/%

# Scripts.
$(SCRIPTS) : % : $(ARCHBIND)/% ;

# Programmer-oriented static and static pattern rules.
ifeq "$(MAKEMODE)" "programmer"
   all : lwf_a

   .clean ::
	-$(RM) *.i *.o lwf_a

   lwf_a : $(LWFCSRCS:.c=.o)
	$(CC) $(CFLAGS) -o $@ $^
endif

show_local :
	-@ echo ""
	-@ echo "Variables defined in the printer utilities makefile"
	-@ echo "==================================================="
	-@ echo ""
	-@ echo "LWFCSRCS=$(LWFCSRCS)"
	-@ echo ""
	-@ echo "MANPAGES=$(MANPAGES)"
	-@ echo ""
	-@ echo "SCRIPTS =$(SCRIPTS)"

help ::
	-@ echo ""
	-@ echo "Targets defined in the printer utilities makefile"
	-@ echo "================================================="
	-@ echo ""
	-@ echo "System"
	-@ echo "------"
	-@ echo "  bin/lwf_a: compile and install lwf_a."
	-@ echo " (MANPAGES): install the manual pages for lwf_a."
	-@ echo "  (SCRIPTS): install general purpose scripts."
	-@ echo ""
	-@ echo "Programmer"
	-@ echo "----------"
	-@ echo "        %.o: compile an object module."
	-@ echo "      lwf_a: compile lwf_a."
