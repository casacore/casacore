#-----------------------------------------------------------------------------
# makefile.app: Generic AIPS++ applications makefile
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
# This GNU makefile is included by other AIPS++ makefiles and is not intended
# for stand-alone use.
#
# Original: 1992/06/24 by Mark Calabretta, ATNF
# $Id$
#=============================================================================

# AIPS++ fundamentals.
#---------------------
include $(word 1, $(AIPSPATH))/code/install/makefile.app_c

bin    : $(TMPPCKGD) $(BINOPTD)/$(THISAPP) pyopyc $(LIBEXECS) $(LIBICONS) $(BINEXECS) ;

bindbg : $(TMPPCKGD) $(BINDBGD)/$(THISAPP) ;

# Find the most appropriate version.o object module.
DBGVERSO := $(firstword $(wildcard $(LIBDBGD)/version.o $(LIBOPTD)/version.o))
OPTVERSO := $(firstword $(wildcard $(LIBOPTD)/version.o $(LIBDBGD)/version.o))

# Programmer-oriented pattern rules.
ifeq "$(MAKEMODE)" "programmer"
   .PRECIOUS : $(PGMRBIND)/% $(PGMRBIND)/%.o $(PGMRBIND)/%.i

   $(PGMRBIND)/% : $(PGMRBIND)/%.o $(PGMRLIBR)
	-@ echo ""
	-@ $(RM) $@
	   $(C++) $(PRGAPINC) $(PGMRINCL) $(C++FLAGS) $(LDFLAGS) -o $@ $< $(PGMRLIBS)
endif

ifneq "$(findstring $(THISAPP),$(MEGASERVE))" ""
   # No need to compile, just link to the megaserver.
   $(BINDBGD)/% : $(CODEDIR)/%.cc FORCE
	-@ echo ""
	-@ echo "$* (dbg) symlinked to the megaserver."
	 @ cd $(BINDBGD) && \
	   if [ ! -h $@ ] ; then \
	     $(RM) $@ ; \
	     ln -s megaserver $* ; \
	   fi

   $(BINOPTD)/% : $(CODEDIR)/%.cc FORCE
	-@ echo ""
	-@ echo "$* (opt) symlinked to the megaserver."
	 @ cd $(BINOPTD) && \
	   if [ ! -h $@ ] ; then \
	     $(RM) $@ ; \
	     ln -s megaserver $* ; \
	   fi

else
   $(BINDBGD)/% : $(CODEDIR)/%.cc $(AIPSINST) \
      $(addprefix $(TMPPCKGD)/, $(addsuffix cc, $(LEXYACC))) \
      $(addprefix $(CODEDIR)/,$(AIPSIMPS)) $(DBGLIBS)
	-@ echo ""
	-@ $(TIMER)
	-@ echo "Remaking $* (dbg) because of $(?F)"
	-@ if [ -h $@ ]; then $(RM) $@; fi
	 @ cd $(TMPPCKGD) && \
	   $(C++) $(CPPDBG) -I$(TMPPCKGD) -I$(CODEDIR) $(AIPSINCL) $(C++DBG) \
	      $(LDDBG) -o $@ $< $(AIPSINST:%=%/*.cc) \
	      $(addprefix $(CODEDIR)/,$(AIPSIMPS)) $(DBGVERSO) \
	      $(patsubst $(LIBDBGD)/lib%.$(SFXSHAR), -l%, $(DBGLIBS)) \
	      $(MODULIBS) $(XTRNLIBS)
	-@ $(TIMER)
	-@ $(RM) $(patsubst %.cc,$(TMPPCKGD)/%.o,$(<F) $(AIPSIMPS)) \
	         $(addprefix $(TMPPCKGD)/, $(addsuffix cc, $(LEXYACC)))
	-@ chmod 775 $@

   $(BINOPTD)/% : $(CODEDIR)/%.cc $(AIPSINST) \
      $(addprefix $(TMPPCKGD)/, $(addsuffix cc, $(LEXYACC))) \
      $(addprefix $(CODEDIR)/,$(AIPSIMPS)) $(OPTLIBS)
	-@ echo ""
	-@ $(TIMER)
	-@ echo "Remaking $* (opt) because of $(?F)"
	-@ if [ -h $@ ]; then $(RM) $@; fi
	 @ cd $(TMPPCKGD) && \
	   $(C++) $(CPPOPT) -I$(TMPPCKGD) -I$(CODEDIR) $(AIPSINCL) $(C++OPT) \
	      $(LDOPT) -o $@ $< $(AIPSINST:%=%/*.cc) \
	      $(addprefix $(CODEDIR)/,$(AIPSIMPS)) $(OPTVERSO) \
	      $(patsubst $(LIBOPTD)/lib%.$(SFXSHAR), -l%, $(OPTLIBS)) \
	      $(MODULIBS) $(XTRNLIBS)
	-@ $(TIMER)
	-@ $(RM) $(patsubst %.cc,$(TMPPCKGD)/%.o,$(<F) $(AIPSIMPS)) \
	         $(addprefix $(TMPPCKGD)/, $(addsuffix cc, $(LEXYACC)))
	-@ chmod 775 $@
endif


$(LIBDBGD)/%.$(SFXSTAT) : ;
$(LIBDBGD)/%.$(SFXSHAR) : ;
$(LIBOPTD)/%.$(SFXSTAT) : ;
$(LIBOPTD)/%.$(SFXSHAR) : ;
