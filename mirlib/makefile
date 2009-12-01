#-----------------------------------------------------------------------------
# GNU makefile for MIRLIB in AIPS++.
#
# $Id$
#-----------------------------------------------------------------------------

# AIPS++ fundamentals.
#---------------------
AIPSROOT := $(word 1, $(AIPSPATH))
AIPSARCH := $(AIPSROOT)/$(word 2, $(AIPSPATH))
include $(AIPSARCH)/makedefs


# Temporary directories.
#-----------------------
MIRTMPD  := $(ARCHTMPD)/mirlib

FIRST := $(shell amkdir -p ug=rwx,g+s,o=rx -v $(MIRTMPD))


# MIRLIB library.
#----------------
MIRLIB := $(LIBOPTD)/libmir.$(SFXSTAT)


# Source lists.
#--------------
MIRIMPS := $(filter %.c,$(AIPSSRCS))
ifdef AIPSRCS
   MIRIMPS += $(patsubst $(RCSDIR)/%$(comma)v,%,$(wildcard $(RCSDIR)/*/*.c,v))
else
   MIRIMPS += $(subst $(CODEDIR)/,,$(wildcard $(CODEDIR)/*/*.c))
endif


# Pattern rules.
#---------------
$(MIRLIB)(%.o) : $(CODEDIR)/%.c
	-@ echo ""
	-@ echo "$%"
	 @ cd $(MIRTMPD) && $(CC) $(MIRDEFS) -I$(CODEDIR) $(COPT) -c $<
	-@ $(TIMER)


# Static and static pattern rules.
#---------------------------------
allsys : mirlib

.cleansys ::
	-$Q $(RM) $(MIRLIB)

mirlib : $(MIRIMPS:%.c=$(MIRLIB)(%.o))
	-@ cd $(MIRTMPD) ; \
	   if [ "`ls *.o 2>/dev/null`" != "" ] ; then \
	      echo "" ; \
	      echo "Updating MIRLIB..." ; \
	      $(AR) $(ARFLAGS) $(MIRLIB) *.o ; \
	      $(RANLIB) $(MIRLIB) ; \
	      $(RM) $(MIRTMPD)/*.o ; \
	   fi

show_local :
	-@ echo ""
	-@ echo "Variables defined in the MIRLIB makefile"
	-@ echo "========================================"
	-@ echo ""
	-@ echo "System"
	-@ echo "------"
	-@ echo "MIRTMPD =$(MIRTMPD)"
	-@ echo "MIRLIB  =$(MIRLIB)"
	-@ echo "MIRIMPS =$(MIRIMPS)"


# Dependency list. (still to do )
#-----------------
